# Cross-cutting tests for the project mutation API. Covers the
# .markModified lifecycle, R6 delegate parity with the standalone
# functions, and the integration loop validate -> mutate -> validate.

# Lifecycle: .markModified clears validatedSinceMutation -------------

test_that("a fresh project starts unmodified and unvalidated", {
  project <- testProject()
  expect_false(project$modified)
  expect_false(project$validatedSinceMutation)
})

test_that("addOutputPath() sets modified and clears validatedSinceMutation", {
  project <- testProject()
  project$.markValidated()
  expect_true(project$validatedSinceMutation)

  addOutputPath(project, "X", "Organism|A|Concentration in container")

  expect_true(project$modified)
  expect_false(project$validatedSinceMutation)
})

test_that("removeOutputPath() warns on missing key and is a no-op", {
  project <- testProject()
  expect_warning(removeOutputPath(project, "Ghost"), "not found")
  expect_false(project$modified)
})

# Standalone vs R6 delegate parity -----------------------------------

test_that("project$addX delegates to the standalone addX", {
  project <- testProject()

  project$addOutputPath("X", "Organism|A|Concentration in container")
  expect_true("X" %in% names(project$outputPaths))

  project$addIndividual("NewI", species = "Human", gender = "MALE")
  expect_true("NewI" %in% names(project$individuals))

  project$addPopulation("NewP", species = "Human", numberOfIndividuals = 50)
  expect_true("NewP" %in% names(project$populations))

  project$addApplication("NewApp")
  expect_true("NewApp" %in% names(project$applications))

  project$addModelParameterEntry("NewSet", "Organism|A", "K", 1.5, "1/h")
  expect_length(project$modelParameterSets$NewSet, 1L)
})

# Add then remove round-trips ----------------------------------------

test_that("addIndividual + removeIndividual round-trip leaves the section unchanged", {
  project <- testProject()
  before <- project$individuals

  addIndividual(project, "NewI", species = "Human", gender = "MALE")
  expect_true("NewI" %in% names(project$individuals))

  removeIndividual(project, "NewI")
  expect_identical(project$individuals, before)
})

test_that("removeModelParameterEntry auto-removes empty parameter sets", {
  project <- testProject()
  addModelParameterEntry(project, "TempSet", "Organism|A", "K", 1.5, "1/h")
  expect_true("TempSet" %in% names(project$modelParameterSets))

  removeModelParameterEntry(project, "TempSet", "Organism|A", "K")
  expect_false("TempSet" %in% names(project$modelParameterSets))
})

test_that("remove*ParameterSetEntry no-op on missing entry does not mark modified", {
  project <- testProject()
  addModelParameterEntry(project, "MSet", "Organism|A", "K", 1.5, "1/h")
  addApplicationParameterSetEntry(
    project,
    "ASet",
    "Organism|B",
    "K",
    2,
    "1/h"
  )
  addIndividualParameterSetEntry(
    project,
    "ISet",
    "Organism|C",
    "K",
    3,
    "1/h"
  )
  project$.markValidated()
  expect_true(project$validatedSinceMutation)

  expect_warning(
    removeModelParameterEntry(project, "MSet", "Organism|A", "Ghost"),
    "not found"
  )
  expect_warning(
    removeApplicationParameterSetEntry(project, "ASet", "Organism|B", "Ghost"),
    "not found"
  )
  expect_warning(
    removeIndividualParameterSetEntry(project, "ISet", "Organism|C", "Ghost"),
    "not found"
  )

  expect_true(project$validatedSinceMutation)
})

# FK validation ------------------------------------------------------

test_that("addScenario aborts when a referenced individualId is unknown", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addScenario(
      project,
      scenarioName = "Bad",
      modelFile = "Aciclovir.pkml",
      individualId = "Ghost"
    )
  )
})

test_that("addIndividual aborts when individualId already exists", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addIndividual(project, "Indiv1", species = "Human", gender = "MALE")
  )
})

test_that("addOutputPath aborts on a duplicate id", {
  project <- testProject()
  existing <- names(project$outputPaths)[[1]]
  expect_snapshot(
    error = TRUE,
    addOutputPath(
      project,
      existing,
      "Organism|other|Concentration in container"
    )
  )
})

test_that("addScenario rejects NA-valued FK args", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addScenario(
      project,
      scenarioName = "S",
      modelFile = "Aciclovir.pkml",
      individualId = NA_character_
    )
  )
  expect_snapshot(
    error = TRUE,
    addScenario(
      project,
      scenarioName = "S",
      modelFile = "Aciclovir.pkml",
      outputPathIds = c("Output1", NA_character_)
    )
  )
})

test_that("addPlotGrid aborts when no plots are defined", {
  project <- testProject()
  project$plots <- list(
    dataCombined = list(),
    plotConfiguration = data.frame(),
    plotGrids = data.frame()
  )
  expect_snapshot(
    error = TRUE,
    addPlotGrid(project, "G1", plotIDs = "MissingPlot")
  )
})

# .warnIfReferenced --------------------------------------------------

test_that("removeOutputPath warns when the id is referenced by a scenario, removes anyway", {
  project <- testProject()
  referenced <- intersect(
    names(project$outputPaths),
    unlist(lapply(project$scenarios, \(sc) names(sc$outputPaths)))
  )[[1]]

  expect_warning(removeOutputPath(project, referenced), "referenced")
  expect_false(referenced %in% names(project$outputPaths))
})

test_that("removeIndividual warns when referenced by a scenario", {
  project <- testProject()
  referenced <- "Indiv1"
  expect_warning(removeIndividual(project, referenced), "referenced")
  expect_false(referenced %in% names(project$individuals))
})

# Integration: validate -> mutate -> .ensureValid re-runs validation -

test_that("a mutation after validateProject() forces .ensureValid to re-validate", {
  project <- testProject()
  # Force the cache flag without having to run a full validation
  # (validateProject() depends on dataFolder existing in the test
  # fixture, which is a separate concern).
  project$.markValidated()
  expect_true(project$validatedSinceMutation)

  # A mutation must clear the cache so .ensureValid re-runs the
  # validators on the new shape — otherwise downstream callers
  # (runScenarios, createPlots) would skip on a now-invalid project.
  addOutputPath(project, "X", "Organism|A|Concentration in container")
  expect_false(project$validatedSinceMutation)

  # .ensureValid short-circuits only when the flag is TRUE; re-mark
  # validated, mutate again, and confirm the flag is cleared a second
  # time (i.e. every successful mutator goes through .markModified).
  project$.markValidated()
  removeOutputPath(project, "X")
  expect_false(project$validatedSinceMutation)
})

# Round-trip through JSON --------------------------------------------

test_that("mutated project survives a saveProject -> loadProject round-trip", {
  project <- testProject()

  addOutputPath(project, "RoundtripX", "Organism|A|Concentration in container")
  addIndividual(
    project,
    "Pediatric_male",
    species = "Human",
    population = "European_ICRP_2002",
    gender = "MALE",
    weight = 25,
    height = 125,
    age = 8
  )

  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)
  reloaded <- loadProject(out)

  expect_identical(
    reloaded$outputPaths$RoundtripX,
    project$outputPaths$RoundtripX
  )
  expect_named(reloaded$individuals, names(project$individuals))
  expect_identical(reloaded$individuals$Pediatric_male$weight, 25)
})
