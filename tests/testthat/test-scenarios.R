# Tests for the Scenario class and the .parseScenarios helper.

test_that("Scenario has the documented field defaults", {
  sc <- Scenario()

  expect_s3_class(sc, "Scenario")
  expect_type(sc, "list")

  # Fields default to NULL except where the spec calls for a typed default.
  expect_null(sc$scenarioName)
  expect_null(sc$modelFile)
  expect_null(sc$applicationProtocol)
  expect_null(sc$individualId)
  expect_null(sc$populationId)
  expect_null(sc$outputPaths)
  expect_identical(sc$simulationType, "Individual")
  expect_false(sc$readPopulationFromCSV)
  expect_false(sc$simulateSteadyState)
  expect_null(sc$simulationTime)
  expect_null(sc$simulationTimeUnit)
  expect_identical(sc$steadyStateTime, 1000)
  expect_null(sc$steadyStateTimeUnit)
  expect_false(sc$overwriteFormulasInSS)
  expect_null(sc$modelParameterSets)
})

test_that("as.list(Scenario()) exposes exactly the v2.0 schema fields", {
  sc <- Scenario()

  expect_named(
    as.list(sc),
    c(
      "scenarioName",
      "modelFile",
      "applicationProtocol",
      "individualId",
      "populationId",
      "outputPaths",
      "simulationType",
      "readPopulationFromCSV",
      "simulateSteadyState",
      "simulationTime",
      "simulationTimeUnit",
      "steadyStateTime",
      "steadyStateTimeUnit",
      "overwriteFormulasInSS",
      "modelParameterSets",
      "initialConditions"
    )
  )
})

test_that(".scenarioFieldNames stays in sync with the Scenario() formals", {
  # Guards against silent drift between the two sources of the field set:
  # a formal added without updating the constant would be silently dropped,
  # and a constant entry without a matching formal errors at construction.
  expect_identical(.scenarioFieldNames, names(formals(Scenario)))
})

test_that("Scenario records have copy semantics", {
  sc <- Scenario(scenarioName = "A", modelFile = "m.pkml")
  copy <- sc
  copy$modelFile <- "other.pkml"

  expect_identical(sc$modelFile, "m.pkml")
})

test_that("Scenario derives simulationType from populationId", {
  expect_identical(Scenario()$simulationType, "Individual")
  expect_identical(
    Scenario(populationId = "Pop")$simulationType,
    "Population"
  )
})

test_that(".parseScenarios returns list() for NULL input", {
  expect_identical(
    .parseScenarios(NULL, list()),
    list()
  )
})

test_that(".parseScenarios copies basic fields for an individual scenario", {
  project <- exampleProject()
  sc <- project$definitions$scenarios[["aciclovir_iv"]]

  expect_s3_class(sc, "Scenario")
  expect_identical(sc$scenarioName, "aciclovir_iv")
  expect_identical(sc$modelFile, "Aciclovir.pkml")
  expect_identical(sc$individualId, "adult_male")
  expect_identical(sc$applicationProtocol, "aciclovir_iv_250mg")
  expect_identical(sc$modelParameterSets, c("global", "aciclovir"))
  expect_null(sc$populationId)
  expect_identical(sc$simulationType, "Individual")
  expect_false(sc$readPopulationFromCSV)
})

test_that(".parseScenarios reads a scenario's initialConditions references", {
  raw <- list(list(
    name = "WithIC",
    modelFile = "m.pkml",
    parameterSets = list("ps1"),
    initialConditions = list("ic1", "ic2")
  ))
  sc <- .parseScenarios(raw, list())[["WithIC"]]
  expect_identical(sc$initialConditions, c("ic1", "ic2"))
})

test_that(".parseScenarios leaves initialConditions NULL when JSON omits it", {
  raw <- list(list(name = "NoIC", modelFile = "m.pkml"))
  sc <- .parseScenarios(raw, list())[["NoIC"]]
  expect_null(sc$initialConditions)
})

test_that("a scenario's initialConditions round-trips through serialize/parse", {
  sc <- Scenario(
    scenarioName = "RT",
    modelFile = "m.pkml",
    initialConditions = c("ic1", "ic2")
  )
  json <- .scenarioToJson(sc)
  expect_identical(json$initialConditions, list("ic1", "ic2"))

  reparsed <- .parseScenarios(
    list(stats::setNames(
      list(json$name, json$modelFile, json$initialConditions),
      c("name", "modelFile", "initialConditions")
    )),
    list()
  )[["RT"]]
  expect_identical(reparsed$initialConditions, c("ic1", "ic2"))
})

test_that(".parseScenarios sets simulationType=Population when populationId present", {
  project <- exampleProject()
  sc <- project$definitions$scenarios[["aciclovir_iv_population"]]

  expect_identical(sc$populationId, "european_adults")
  expect_identical(sc$simulationType, "Population")
})

test_that(".parseScenarios defaults applicationProtocol to NA when JSON has null", {
  raw <- list(
    list(
      name = "X",
      individual = "i",
      modelFile = "m.pkml",
      application = NULL
    )
  )
  result <- .parseScenarios(raw, list())

  expect_length(result, 1L)
  expect_true(is.na(result[["X"]]$applicationProtocol))
})

test_that(".parseScenarios converts steadyStateTime to base units (minutes)", {
  project <- exampleProject()
  sc <- project$definitions$scenarios[["aciclovir_iv_steadystate"]]

  expect_true(sc$simulateSteadyState)
  # 1 hour -> 60 minutes
  expect_equal(sc$steadyStateTime, 60)
  expect_identical(sc$steadyStateTimeUnit, "h")
})

test_that(".parseScenarios coerces a whole-number steadyStateTime to double", {
  # `jsonlite::fromJSON` reads a whole number as integer; a same-unit
  # conversion preserves that type, so without an explicit coercion the parsed
  # value would be integer while a freshly built scenario's default is double,
  # breaking a byte-equivalent round trip.
  raw <- list(
    list(
      name = "WholeSS",
      individual = "i",
      modelFile = "m.pkml",
      steadyStateTime = 1000L,
      steadyStateTimeUnit = "min"
    )
  )
  sc <- .parseScenarios(raw, list())[["WholeSS"]]

  expect_type(sc$steadyStateTime, "double")
  expect_identical(sc$steadyStateTime, 1000)
})

test_that(".parseScenarios leaves simulateSteadyState=FALSE when JSON omits/sets false", {
  project <- exampleProject()
  sc <- project$definitions$scenarios[["aciclovir_iv"]]

  expect_false(sc$simulateSteadyState)
  expect_null(sc$steadyStateTimeUnit)
  # The class default of 1000 stays put when JSON's steadyStateTime is null.
  expect_identical(sc$steadyStateTime, 1000)
})

test_that(".parseScenarios errors when steadyStateTime set without unit", {
  raw <- list(
    list(
      name = "BadSS",
      individual = "i",
      modelFile = "m.pkml",
      steadyStateTime = 5,
      steadyStateTimeUnit = NULL
    )
  )
  expect_error(
    .parseScenarios(raw, list()),
    "BadSS.*steadyStateTime.*steadyStateTimeUnit"
  )
})

test_that(".parseScenarios parses simulationTime to a list of length-3 numerics", {
  project <- exampleProject()
  sc <- project$definitions$scenarios[["aciclovir_iv"]]

  expect_type(sc$simulationTime, "list")
  expect_length(sc$simulationTime, 1L)
  expect_identical(sc$simulationTime[[1L]], c(0, 24, 60))
  expect_identical(sc$simulationTimeUnit, "h")
})

test_that(".parseScenarios resolves outputPaths ids to literal outputPaths in declared order", {
  project <- exampleProject()
  sc <- project$definitions$scenarios[["aciclovir_iv_steadystate"]]

  expect_type(sc$outputPaths, "character")
  expect_length(sc$outputPaths, 2L)
  # Names are the ids, values are the literal paths; order follows JSON declaration.
  expect_named(sc$outputPaths, c("aciclovir_fat_cell", "aciclovir_pvb"))
  expect_identical(
    unname(sc$outputPaths),
    c(
      "Organism|Fat|Intracellular|Aciclovir|Concentration in container",
      "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
    )
  )
})

test_that(".parseScenarios single outputPaths id resolves to a length-1 named character vector", {
  project <- exampleProject()
  sc <- project$definitions$scenarios[["aciclovir_iv"]]

  expect_type(sc$outputPaths, "character")
  expect_length(sc$outputPaths, 1L)
  expect_named(sc$outputPaths, "aciclovir_pvb")
  expect_identical(
    unname(sc$outputPaths),
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  )
})

test_that(".parseScenarios keeps unknown outputPaths ids as dangling refs (lazy)", {
  # Referential integrity is lazy: an unknown output-path id does not abort
  # the parse; it survives as a name with an NA literal path so the
  # cross-reference validator can flag it later.
  raw <- list(
    list(
      name = "BadRefs",
      individual = "i",
      modelFile = "m.pkml",
      outputPaths = list("Aciclovir_PVB", "Nope", "AlsoNope")
    )
  )
  outputPaths <- list(Aciclovir_PVB = "Organism|PVB|...")

  sc <- .parseScenarios(raw, outputPaths)[["BadRefs"]]
  expect_named(sc$outputPaths, c("Aciclovir_PVB", "Nope", "AlsoNope"))
  expect_identical(
    unname(sc$outputPaths[["Aciclovir_PVB"]]),
    "Organism|PVB|..."
  )
  expect_true(is.na(sc$outputPaths[["Nope"]]))
  expect_true(is.na(sc$outputPaths[["AlsoNope"]]))
})

test_that(".parseScenarios collapses duplicate outputPaths ids to one (first-seen order)", {
  # A repeated id is redundant, not an error: it must resolve to a single
  # entry so the path is never run or plotted twice, keeping first-seen order.
  raw <- list(
    list(
      name = "Dups",
      individual = "i",
      modelFile = "m.pkml",
      outputPaths = list("a", "a", "b", "a")
    )
  )
  outputPaths <- list(a = "PATH_A", b = "PATH_B")

  sc <- .parseScenarios(raw, outputPaths)[["Dups"]]
  expect_named(sc$outputPaths, c("a", "b"))
  expect_identical(unname(sc$outputPaths), c("PATH_A", "PATH_B"))
})

test_that(".parseScenarios leaves outputPaths NULL when JSON omits outputPaths", {
  raw <- list(
    list(
      name = "NoOutputs",
      individual = "i",
      modelFile = "m.pkml"
    )
  )
  result <- .parseScenarios(raw, list())

  expect_null(result[["NoOutputs"]]$outputPaths)
})

test_that("addScenario aborts when a referenced individual is unknown", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addScenario(
      project,
      id = "bad",
      modelFile = "Aciclovir.pkml",
      individual = "Ghost"
    )
  )
})

test_that("addScenario suggests the closest existing id for a dangling reference", {
  # Authoring catches a dangling reference eagerly and `validateProject()`
  # catches it later; both name the near miss, so the same typo reads the same
  # way whichever path finds it.
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addScenario(
      project,
      id = "bad",
      modelFile = "Aciclovir.pkml",
      individual = "indiv2"
    )
  )
  expect_snapshot(
    error = TRUE,
    addScenario(
      project,
      id = "bad",
      modelFile = "Aciclovir.pkml",
      outputPaths = "aciclovir_pv"
    )
  )
})

test_that("addScenario leaves the reference error bare when no id is close", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addScenario(
      project,
      id = "bad",
      modelFile = "Aciclovir.pkml",
      outputPaths = "somethingentirelyunrelated"
    )
  )
})

test_that("addScenario accepts a valid initialConditions reference", {
  project <- testProject()
  addInitialConditions(project, "icset")
  addScenario(
    project,
    id = "withic",
    modelFile = "Aciclovir.pkml",
    initialConditions = "icset"
  )
  expect_identical(
    project$definitions$scenarios[["withic"]]$initialConditions,
    "icset"
  )
})

test_that("addScenario aborts eagerly on a dangling initialConditions ref", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addScenario(
      project,
      id = "bad",
      modelFile = "Aciclovir.pkml",
      initialConditions = "ghostset"
    )
  )
})

test_that("addScenario accepts the numeric simulation-time grid", {
  project <- testProject()
  # The numeric triple and the string spell the same grid, so both must land as
  # the parsed list-of-intervals the record stores.
  addScenario(
    project,
    id = "numeric",
    modelFile = "Aciclovir.pkml",
    simulationTime = c(0, 42, 48)
  )
  addScenario(
    project,
    id = "string",
    modelFile = "Aciclovir.pkml",
    simulationTime = "0, 42, 48"
  )
  expect_equal(
    project$definitions$scenarios[["numeric"]]$simulationTime,
    list(c(0, 42, 48))
  )
  expect_identical(
    project$definitions$scenarios[["numeric"]]$simulationTime,
    project$definitions$scenarios[["string"]]$simulationTime
  )
})

test_that("addScenario takes one simulation-time grid per id from a list", {
  project <- testProject()
  addScenario(
    project,
    id = c("first", "second"),
    modelFile = "Aciclovir.pkml",
    simulationTime = list(c(0, 42, 48), "0, 24, 60")
  )
  expect_equal(
    project$definitions$scenarios[["first"]]$simulationTime,
    list(c(0, 42, 48))
  )
  expect_equal(
    project$definitions$scenarios[["second"]]$simulationTime,
    list(c(0, 24, 60))
  )
})

test_that("setScenario accepts the numeric simulation-time grid", {
  project <- testProject()
  setScenario(project, "testscenario", simulationTime = c(0, 48, 120))
  expect_equal(
    project$definitions$scenarios[["testscenario"]]$simulationTime,
    list(c(0, 48, 120))
  )

  # A supplied NULL still clears the field rather than leaving it untouched.
  setScenario(project, "testscenario", simulationTime = NULL)
  expect_null(project$definitions$scenarios[["testscenario"]]$simulationTime)
})

test_that("addScenario names both simulation-time forms on a bad value", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addScenario(
      project,
      id = "bad",
      modelFile = "Aciclovir.pkml",
      simulationTime = c(0, 42)
    )
  )
})

test_that("setScenario updates and clears the initialConditions reference", {
  project <- testProject()
  addInitialConditions(project, "icset")
  addScenario(project, id = "sc", modelFile = "Aciclovir.pkml")

  setScenario(project, "sc", initialConditions = "icset")
  expect_identical(
    project$definitions$scenarios[["sc"]]$initialConditions,
    "icset"
  )

  setScenario(project, "sc", initialConditions = NULL)
  expect_null(project$definitions$scenarios[["sc"]]$initialConditions)
})

test_that("setScenario aborts eagerly on a dangling initialConditions ref", {
  project <- testProject()
  addScenario(project, id = "sc", modelFile = "Aciclovir.pkml")
  expect_snapshot(
    error = TRUE,
    setScenario(project, "sc", initialConditions = "ghostset")
  )
})

test_that("addScenario rejects NA-valued FK args", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addScenario(
      project,
      id = "S",
      modelFile = "Aciclovir.pkml",
      individual = NA_character_
    )
  )
  expect_snapshot(
    error = TRUE,
    addScenario(
      project,
      id = "S",
      modelFile = "Aciclovir.pkml",
      outputPaths = c("Output1", NA_character_)
    )
  )
})

test_that("addScenario treats a zero-length reference vector as none", {
  # A definition file carries `[]` for a scenario that references no output
  # paths, parameter sets, or initial conditions, so `character(0)` means what
  # `NULL` means here: there are none. Rejecting it would make a value the
  # write path produces unusable as authoring input.
  project <- testProject()
  addScenario(
    project,
    id = "nonerefs",
    modelFile = "Aciclovir.pkml",
    outputPaths = character(0),
    parameterSets = character(0),
    initialConditions = character(0)
  )
  sc <- project$definitions$scenarios[["nonerefs"]]
  expect_null(sc$outputPaths)
  expect_null(sc$modelParameterSets)
  expect_null(sc$initialConditions)
})

test_that("addScenario treats the empty list jsonlite yields for [] as none", {
  # Reading a definition file with `jsonlite::fromJSON()` turns `[]` into
  # `list()`, not `character(0)`, so the zero-length rule has to cover both.
  project <- testProject()
  addScenario(
    project,
    id = "emptylist",
    modelFile = "Aciclovir.pkml",
    outputPaths = list(),
    parameterSets = list()
  )
  sc <- project$definitions$scenarios[["emptylist"]]
  expect_null(sc$outputPaths)
  expect_null(sc$modelParameterSets)
})

test_that("setScenario clears a reference field given a zero-length vector", {
  project <- testProject()
  setScenario(project, "testscenario", outputPaths = character(0))
  expect_null(project$definitions$scenarios[["testscenario"]]$outputPaths)
})

test_that("a scenario's own written reference fields are accepted back by addScenario", {
  # The round trip an imported project needs: read a scenario's fields straight
  # out of its definition file and hand them to `addScenario()`. Read the file
  # the way the package reads it (`simplifyVector = FALSE`), so an absent
  # reference list arrives as `list()` and a populated one as a list of strings.
  project <- testProject()
  addScenario(project, id = "written", modelFile = "Aciclovir.pkml")
  addScenario(
    project,
    id = "writtenrefs",
    modelFile = "Aciclovir.pkml",
    outputPaths = c("aciclovir_pvb", "aciclovir_fat_cell")
  )
  saveProject(project)

  readDefinition <- function(id) {
    jsonlite::fromJSON(
      file.path(
        project$info$projectDirPath,
        project$paths$definitionsFolder,
        "scenarios",
        paste0(id, ".json")
      ),
      simplifyVector = FALSE
    )
  }

  bare <- readDefinition("written")
  expect_length(bare$outputPaths, 0L)
  addScenario(
    project,
    id = "rebuiltbare",
    modelFile = bare$modelFile,
    outputPaths = bare$outputPaths,
    parameterSets = bare$parameterSets,
    initialConditions = bare$initialConditions
  )
  expect_null(project$definitions$scenarios[["rebuiltbare"]]$outputPaths)

  withRefs <- readDefinition("writtenrefs")
  expect_type(withRefs$outputPaths, "list")
  expect_length(withRefs$outputPaths, 2L)
  addScenario(
    project,
    id = "rebuiltrefs",
    modelFile = withRefs$modelFile,
    outputPaths = withRefs$outputPaths
  )
  expect_named(
    project$definitions$scenarios[["rebuiltrefs"]]$outputPaths,
    c("aciclovir_pvb", "aciclovir_fat_cell")
  )
})

test_that("addScenario keeps rejecting a reference list holding a non-string", {
  # Only an all-strings list flattens to a reference vector; anything else is
  # still a malformed argument, not a list of ids.
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addScenario(
      project,
      id = "badlist",
      modelFile = "Aciclovir.pkml",
      outputPaths = list("aciclovir_pvb", 1)
    )
  )
})

test_that("addScenario collapses duplicate outputPaths to one (first-seen order)", {
  project <- testProject()
  addScenario(
    project,
    id = "dupout",
    modelFile = "Aciclovir.pkml",
    outputPaths = c("aciclovir_pvb", "aciclovir_pvb", "aciclovir_fat_cell")
  )
  sc <- project$definitions$scenarios[["dupout"]]
  expect_named(sc$outputPaths, c("aciclovir_pvb", "aciclovir_fat_cell"))
})

test_that("setScenario collapses duplicate outputPaths to one (first-seen order)", {
  project <- testProject()
  setScenario(
    project,
    "testscenario",
    outputPaths = c(
      "aciclovir_fat_cell",
      "aciclovir_pvb",
      "aciclovir_fat_cell"
    )
  )
  sc <- project$definitions$scenarios[["testscenario"]]
  expect_named(sc$outputPaths, c("aciclovir_fat_cell", "aciclovir_pvb"))
})

test_that("removeScenario uses the id argument matching addScenario", {
  project <- testProject()
  addScenario(
    project,
    id = "toremove",
    modelFile = "Aciclovir.pkml"
  )
  expect_true("toremove" %in% names(project$definitions$scenarios))
  removeScenario(project, id = "toremove")
  expect_false("toremove" %in% names(project$definitions$scenarios))
})

test_that("addScenario aborts on an existing id, replaces it with overwrite", {
  project <- testProject()
  existing <- names(project$definitions$scenarios)[[1]]
  expect_snapshot(
    error = TRUE,
    addScenario(project, id = existing, modelFile = "Aciclovir.pkml")
  )
  before <- length(project$definitions$scenarios)
  addScenario(
    project,
    id = existing,
    modelFile = "Aciclovir.pkml",
    simulationTimeUnit = "min",
    overwrite = TRUE
  )
  expect_length(project$definitions$scenarios, before)
  expect_identical(
    project$definitions$scenarios[[existing]]$simulationTimeUnit,
    "min"
  )
})

test_that("addScenario and removeScenario clear the validation cache", {
  project <- testProject()
  .markValidated(project)
  expect_true(.isValidated(project))

  addScenario(project, id = "x", modelFile = "Aciclovir.pkml")
  expect_false(.isValidated(project))

  .markValidated(project)
  removeScenario(project, id = "x")
  expect_false(.isValidated(project))
})

test_that("addScenario stores steadyStateTime in base units and round-trips the declared unit", {
  project <- testProject()
  addScenario(
    project,
    id = "ss",
    modelFile = "Aciclovir.pkml",
    individual = "indiv1",
    steadyState = TRUE,
    steadyStateTime = 10,
    steadyStateTimeUnit = "h"
  )

  # Stored value is the base unit (minutes): 10 h -> 600 min.
  expect_equal(project$definitions$scenarios[["ss"]]$steadyStateTime, 600)
  expect_equal(project$definitions$scenarios[["ss"]]$steadyStateTimeUnit, "h")

  # Saved JSON carries the declared 10 / "h" (the serializer converts the
  # base-unit value back to the declared unit).
  out <- withr::local_tempfile(fileext = ".json")
  .saveProjectJson(project, out)
  raw <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  savedSS <- Filter(\(s) identical(s[["name"]], "ss"), raw$scenarios)[[1]]
  expect_equal(savedSS$steadyStateTime, 10)
  expect_equal(savedSS$steadyStateTimeUnit, "h")

  # Reload round-trips back to the base-unit value.
  reloaded <- loadProject(out)
  expect_equal(reloaded$definitions$scenarios[["ss"]]$steadyStateTime, 600)
  expect_equal(reloaded$definitions$scenarios[["ss"]]$steadyStateTimeUnit, "h")
})

# Passing a parsed Scenario record back ----

test_that("a parsed scenario is accepted back by addScenario() unchanged", {
  # The record's field names are the ones the runtime reads, not the authoring
  # argument names, and three of its fields are stored in a different shape from
  # the one authoring takes (resolved output paths, a parsed time grid, a
  # base-unit steady-state time). Handing the record back has to survive all of
  # that: the copy must equal the original in every field but its id.
  project <- testProject()
  sc <- project$definitions$scenarios[["testscenario"]]

  addScenario(project, sc, overwrite = TRUE)
  copy <- project$definitions$scenarios[["testscenario"]]

  # This record declares no `steadyStateTimeUnit`, and a definition file cannot
  # carry a steady-state time without one, so the copy gains the default "min" -
  # the unit the stored value is already in, leaving the duration unchanged. This
  # is what re-adding a scenario from its written definition file does too.
  expect_null(sc$steadyStateTimeUnit)
  expect_equal(copy$steadyStateTimeUnit, "min")
  expect_equal(copy$steadyStateTime, sc$steadyStateTime)
  rest <- setdiff(names(sc), "steadyStateTimeUnit")
  expect_equal(copy[rest], sc[rest])

  # And into a second project, under the record's own id.
  other <- testProject()
  removeScenario(other, "testscenario")
  addScenario(other, sc)
  expect_equal(other$definitions$scenarios[["testscenario"]][rest], sc[rest])
})

test_that("a scenario carrying a multi-interval time grid and a non-minute steady state round-trips", {
  # The two record fields whose stored shape differs most from the authoring
  # argument: a list of intervals, and a steady-state time held in minutes while
  # the scenario declares hours.
  project <- testProject()
  addScenario(
    project,
    "grid",
    modelFile = "Aciclovir.pkml",
    simulationTime = "0, 42, 48; 48, 96, 24",
    steadyState = TRUE,
    steadyStateTime = 5,
    steadyStateTimeUnit = "h"
  )
  sc <- project$definitions$scenarios[["grid"]]
  expect_length(sc$simulationTime, 2L)
  expect_equal(sc$steadyStateTime, 300)

  addScenario(project, sc, overwrite = TRUE)
  expect_equal(project$definitions$scenarios[["grid"]], sc)
})

test_that("setScenario() takes a parsed scenario, so an edited record writes back", {
  project <- testProject()
  sc <- project$definitions$scenarios[["testscenario"]]
  sc$modelFile <- "Aciclovir.pkml"
  sc$simulationTimeUnit <- "min"

  setScenario(project, sc)
  after <- project$definitions$scenarios[["testscenario"]]
  expect_equal(after$modelFile, "Aciclovir.pkml")
  expect_equal(after$simulationTimeUnit, "min")
  rest <- setdiff(names(sc), "steadyStateTimeUnit")
  expect_equal(after[rest], sc[rest])
})

test_that("a scenario record passed with field arguments alongside it aborts", {
  project <- testProject()
  sc <- project$definitions$scenarios[["testscenario"]]
  expect_snapshot(
    error = TRUE,
    addScenario(project, sc, modelFile = "Other.pkml")
  )
  expect_snapshot(error = TRUE, setScenario(project, sc, individual = NULL))
})

test_that("addScenario() reads a list of intervals as one grid for one scenario", {
  # A parsed multi-interval `simulationTime` is a list of length-3 numerics; with
  # one id that is one grid, not one value per id.
  project <- testProject()
  addScenario(
    project,
    "listgrid",
    modelFile = "Aciclovir.pkml",
    simulationTime = list(c(0, 42, 48), c(48, 96, 24))
  )
  expect_equal(
    project$definitions$scenarios[["listgrid"]]$simulationTime,
    list(c(0, 42, 48), c(48, 96, 24))
  )

  # With two ids, a two-element list stays one grid per id.
  addScenario(
    project,
    c("g1", "g2"),
    modelFile = "Aciclovir.pkml",
    simulationTime = list(c(0, 42, 48), c(48, 96, 24))
  )
  expect_equal(
    project$definitions$scenarios[["g1"]]$simulationTime,
    list(c(0, 42, 48))
  )
  expect_equal(
    project$definitions$scenarios[["g2"]]$simulationTime,
    list(c(48, 96, 24))
  )
})

# setScenario ----

test_that("setScenario changes a field in memory and persists on save", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "scenarios")

  setScenario(project, "testscenario", simulationTime = "0, 48, 120")

  expect_equal(
    project$definitions$scenarios[["testscenario"]]$simulationTime,
    list(c(0, 48, 120))
  )
  saveProject(project)
  reloaded <- loadProject(project$info$projectFilePath)
  expect_equal(
    reloaded$definitions$scenarios[["testscenario"]]$simulationTime,
    list(c(0, 48, 120))
  )
  expect_true(file.exists(file.path(dir, "testscenario.json")))
})

test_that("setScenario partial update leaves other fields untouched", {
  project <- testProject()
  before <- project$definitions$scenarios[["testscenario"]]

  setScenario(project, "testscenario", simulationTimeUnit = "min")
  after <- project$definitions$scenarios[["testscenario"]]

  expect_equal(after$simulationTimeUnit, "min")
  # Every other field is unchanged.
  for (f in setdiff(names(before), "simulationTimeUnit")) {
    expect_equal(after[[f]], before[[f]])
  }
})

test_that("setScenario invalidates the validation cache", {
  project <- testProject()
  .markValidated(project)
  expect_true(.isValidated(project))

  setScenario(project, "testscenario", simulationTimeUnit = "min")

  expect_false(.isValidated(project))
})

test_that("setScenario can clear an optional field with NULL", {
  project <- testProject()
  expect_false(is.null(
    project$definitions$scenarios[["populationscenario"]]$individualId
  ))

  setScenario(project, "populationscenario", individual = NULL)

  expect_null(
    project$definitions$scenarios[["populationscenario"]]$individualId
  )
  saveProject(project)
  reloaded <- loadProject(project$info$projectFilePath)
  expect_null(
    reloaded$definitions$scenarios[["populationscenario"]]$individualId
  )
})

test_that("setScenario aborts on a non-existent scenario, no file written", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "scenarios")
  before <- list.files(dir)

  expect_snapshot(
    error = TRUE,
    setScenario(project, "Ghost", simulationTimeUnit = "min")
  )
  expect_setequal(list.files(dir), before)
})

test_that("saveProject() fails fast on a structural violation, disk untouched", {
  project <- testProject()
  saveProject(project)
  dir <- file.path(project$info$projectDirPath, "definitions", "scenarios")
  beforeFile <- readLines(file.path(dir, "testscenario.json"))

  # Clearing modelFile is a structural violation. The edit is accepted in
  # memory; the abort happens at save (the serialize-in-memory-first guarantee),
  # leaving disk unchanged.
  setScenario(project, "testscenario", modelFile = NULL)
  expect_error(
    saveProject(project),
    "modelFile"
  )
  expect_identical(readLines(file.path(dir, "testscenario.json")), beforeFile)
})

test_that("setScenario rejects an unknown foreign key like addScenario", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    setScenario(project, "testscenario", individual = "Ghost")
  )
})

test_that("the write path allows a dangling outputPathId as a lazy referential finding", {
  project <- testProject()
  # The authoring functions check references eagerly, but the lower-level
  # write-through entry point (`.setSection()`) is structural-only: an unknown
  # output-path id is allowed at write time (referential checks are lazy); it
  # is the cross-reference validator that flags it later.
  scenarios <- .getSection(project, "scenarios")
  sc <- scenarios[["testscenario"]]
  sc$outputPaths <- c(sc$outputPaths, Ghost = NA_character_)
  scenarios[["testscenario"]] <- sc
  expect_no_error(.setSection(project, "scenarios", scenarios))
})

test_that("setScenario stays in memory until saveProject()", {
  source <- testProject()
  dir <- file.path(source$info$projectDirPath, "definitions", "scenarios")
  sourceFile <- readLines(file.path(dir, "testscenario.json"))

  setScenario(source, "testscenario", simulationTimeUnit = "min")

  # The edit is in memory only; the on-disk file is untouched before a save.
  expect_equal(
    source$definitions$scenarios[["testscenario"]]$simulationTimeUnit,
    "min"
  )
  expect_identical(readLines(file.path(dir, "testscenario.json")), sourceFile)
})

test_that("setScenario unit-only steadyState change relabels without rescaling", {
  project <- testProject()
  addScenario(
    project,
    id = "ss",
    modelFile = "Aciclovir.pkml",
    individual = "indiv1",
    steadyState = TRUE,
    steadyStateTime = 10,
    steadyStateTimeUnit = "h"
  )
  # Seeded: 10 h -> 600 base-min.
  expect_equal(project$definitions$scenarios[["ss"]]$steadyStateTime, 600)

  setScenario(project, "ss", steadyStateTimeUnit = "min")

  # Pure relabel: the stored base duration is unchanged, only the unit label
  # moves to "min".
  expect_equal(project$definitions$scenarios[["ss"]]$steadyStateTime, 600)
  expect_equal(project$definitions$scenarios[["ss"]]$steadyStateTimeUnit, "min")
})

test_that("setScenario with steadyStateTime still converts under the effective unit", {
  project <- testProject()
  addScenario(
    project,
    id = "ss",
    modelFile = "Aciclovir.pkml",
    individual = "indiv1",
    steadyState = TRUE,
    steadyStateTime = 10,
    steadyStateTimeUnit = "h"
  )

  # Value + unit supplied together: convert under the new unit (5 h -> 300 min).
  setScenario(project, "ss", steadyStateTime = 5, steadyStateTimeUnit = "h")
  expect_equal(project$definitions$scenarios[["ss"]]$steadyStateTime, 300)
  expect_equal(project$definitions$scenarios[["ss"]]$steadyStateTimeUnit, "h")

  # Value supplied, unit inherited from the record (still "h"): 5 h -> 300 min.
  setScenario(project, "ss", steadyStateTime = 5)
  expect_equal(project$definitions$scenarios[["ss"]]$steadyStateTime, 300)
  expect_equal(project$definitions$scenarios[["ss"]]$steadyStateTimeUnit, "h")
})

# renameScenario ----

test_that("renameScenario moves the definition file on save and changes the in-memory key", {
  project <- testProject()
  saveProject(project)
  dir <- file.path(project$info$projectDirPath, "definitions", "scenarios")
  before <- readLines(file.path(dir, "testscenario.json"))

  renameScenario(project, "testscenario", "renamed")

  # In-memory: old key gone, new key present.
  expect_false("testscenario" %in% names(project$definitions$scenarios))
  expect_true("renamed" %in% names(project$definitions$scenarios))
  # On disk after save: old file removed, new file written.
  saveProject(project)
  expect_false(file.exists(file.path(dir, "testscenario.json")))
  expect_true(file.exists(file.path(dir, "renamed.json")))

  # Content is preserved up to the name field: the only difference between the
  # old and new file is the line carrying the scenario name (now the new id).
  after <- readLines(file.path(dir, "renamed.json"))
  changed <- which(before != after)
  expect_length(changed, 1L)
  expect_match(before[changed], "testscenario")
  expect_match(after[changed], "renamed")
})

test_that("renameScenario updates the record's stored name so a reload round-trips", {
  project <- testProject()

  renameScenario(project, "testscenario", "renamed")

  expect_equal(
    project$definitions$scenarios[["renamed"]]$scenarioName,
    "renamed"
  )
  # A reload re-derives scenarios from the tree; the new key must validate and
  # round-trip (name == key invariant holds).
  saveProject(project)
  reloaded <- loadProject(project$info$projectFilePath)
  expect_true("renamed" %in% names(reloaded$definitions$scenarios))
  expect_equal(
    reloaded$definitions$scenarios[["renamed"]]$scenarioName,
    "renamed"
  )
  expect_no_error(validateProject(reloaded))
})

test_that("renameScenario errors clearly on a non-existent id", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    renameScenario(project, "Ghost", "renamed")
  )
})

test_that("renameScenario errors when the target id already exists", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    renameScenario(project, "testscenario", "populationscenario")
  )
})

test_that("renameScenario canonicalizes newId, warning and landing on the canonical form", {
  project <- testProject()
  expect_snapshot(
    renameScenario(project, "testscenario", "New Name")
  )
  expect_true("new_name" %in% names(project$definitions$scenarios))
  expect_false("testscenario" %in% names(project$definitions$scenarios))
})

test_that("renameScenario stays in memory until saveProject()", {
  source <- testProject()
  saveProject(source)
  dir <- file.path(source$info$projectDirPath, "definitions", "scenarios")
  sourceFiles <- list.files(dir)

  renameScenario(source, "testscenario", "renamed")

  # The edit is in memory only; the on-disk tree is untouched before a save.
  expect_true("renamed" %in% names(source$definitions$scenarios))
  expect_setequal(list.files(dir), sourceFiles)
})

test_that("renameScenario warns when a dataCombined still references it", {
  project <- testProject()
  addDataCombined(
    project,
    "dc_ref",
    simulated = list(
      list(
        label = "ref",
        scenario = "testscenario",
        path = "Organism|A|Concentration"
      )
    )
  )
  expect_snapshot(renameScenario(project, "testscenario", "renamed"))
})

# duplicateScenario ----

test_that("duplicateScenario creates an independent copy in memory, persisted on save", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "scenarios")

  duplicateScenario(project, "testscenario", "copy")

  # Both exist in memory; the original is untouched.
  expect_true(all(
    c("testscenario", "copy") %in% names(project$definitions$scenarios)
  ))
  expect_equal(project$definitions$scenarios[["copy"]]$scenarioName, "copy")
  # On save, the copy is a new definition file alongside the original.
  saveProject(project)
  expect_true(file.exists(file.path(dir, "testscenario.json")))
  expect_true(file.exists(file.path(dir, "copy.json")))
})

test_that("duplicateScenario produces an independent copy: mutating it leaves the original", {
  project <- testProject()
  originalBefore <- project$definitions$scenarios[["testscenario"]]

  duplicateScenario(project, "testscenario", "copy")
  setScenario(project, "copy", simulationTimeUnit = "min")

  expect_equal(
    project$definitions$scenarios[["copy"]]$simulationTimeUnit,
    "min"
  )
  # The original record (and its file) is unchanged.
  expect_equal(project$definitions$scenarios[["testscenario"]], originalBefore)
  saveProject(project)
  reloaded <- loadProject(project$info$projectFilePath)
  expect_equal(
    reloaded$definitions$scenarios[["testscenario"]]$simulationTimeUnit,
    originalBefore$simulationTimeUnit
  )
})

test_that("duplicateScenario errors on a non-existent source id", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    duplicateScenario(project, "Ghost", "copy")
  )
})

test_that("duplicateScenario errors when the target id already exists", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    duplicateScenario(project, "testscenario", "populationscenario")
  )
})

test_that("duplicateScenario stays in memory until saveProject()", {
  source <- testProject()
  saveProject(source)
  dir <- file.path(source$info$projectDirPath, "definitions", "scenarios")
  sourceFiles <- list.files(dir)

  duplicateScenario(source, "testscenario", "copy")

  expect_true("copy" %in% names(source$definitions$scenarios))
  # The copy is in memory only; the on-disk tree is untouched before a save.
  expect_setequal(list.files(dir), sourceFiles)
})

# Vectorized authoring ----

test_that("addScenario adds N scenarios in one call equal to N scalar adds", {
  vectorized <- testProject()
  addScenario(
    vectorized,
    c("s1", "s2"),
    modelFile = "Aciclovir.pkml",
    individual = "indiv1",
    outputPaths = "aciclovir_pvb"
  )

  scalar <- testProject()
  addScenario(
    scalar,
    "s1",
    modelFile = "Aciclovir.pkml",
    individual = "indiv1",
    outputPaths = "aciclovir_pvb"
  )
  addScenario(
    scalar,
    "s2",
    modelFile = "Aciclovir.pkml",
    individual = "indiv1",
    outputPaths = "aciclovir_pvb"
  )

  expect_identical(
    vectorized$definitions$scenarios[c("s1", "s2")],
    scalar$definitions$scenarios[c("s1", "s2")]
  )
})

test_that("addScenario aborts on a duplicate id in the batch", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addScenario(
      project,
      c("s1", "s1"),
      modelFile = "Aciclovir.pkml",
      individual = "indiv1",
      outputPaths = "aciclovir_pvb"
    )
  )
})

test_that("removeScenario warns when a dataCombined still references it", {
  project <- testProject()
  addDataCombined(
    project,
    "dc_ref",
    simulated = list(
      list(
        label = "ref",
        scenario = "testscenario",
        path = "Organism|A|Concentration"
      )
    )
  )
  expect_snapshot(removeScenario(project, "testscenario"))
})

test_that("addScenario recycles a scalar field and applies outputPaths whole", {
  project <- testProject()
  addScenario(
    project,
    c("s1", "s2"),
    modelFile = "Aciclovir.pkml",
    individual = c("indiv1", NULL),
    outputPaths = c("aciclovir_pvb", "aciclovir_fat_cell")
  )
  expect_identical(project$definitions$scenarios$s1$modelFile, "Aciclovir.pkml")
  expect_identical(project$definitions$scenarios$s2$modelFile, "Aciclovir.pkml")
  # outputPaths applied whole to both scenarios.
  expect_identical(
    names(project$definitions$scenarios$s1$outputPaths),
    c("aciclovir_pvb", "aciclovir_fat_cell")
  )
  expect_identical(
    names(project$definitions$scenarios$s2$outputPaths),
    c("aciclovir_pvb", "aciclovir_fat_cell")
  )
})

test_that("addScenario persists all N to disk in one saveProject()", {
  project <- testProject()
  addScenario(project, c("s1", "s2"), modelFile = "Aciclovir.pkml")
  saveProject(project)
  reloaded <- loadProject(project$info$projectFilePath)
  expect_true(all(c("s1", "s2") %in% names(reloaded$definitions$scenarios)))
})

test_that("addScenario aborts the whole batch and writes nothing on a bad reference", {
  project <- testProject()
  before <- names(project$definitions$scenarios)
  expect_error(
    addScenario(
      project,
      c("s1", "s2"),
      modelFile = "Aciclovir.pkml",
      individual = c("indiv1", "ghost")
    )
  )
  expect_identical(names(project$definitions$scenarios), before)
  reloaded <- loadProject(project$info$projectFilePath)
  expect_identical(names(reloaded$definitions$scenarios), before)
})

test_that("addScenario aborts on a mismatched scalar field length", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addScenario(
      project,
      c("s1", "s2", "s3"),
      modelFile = c("A.pkml", "B.pkml")
    )
  )
})

test_that("setScenario vectorizes a partial update across N ids", {
  project <- testProject()
  addScenario(project, c("s1", "s2"), modelFile = "Aciclovir.pkml")
  setScenario(project, c("s1", "s2"), simulationTimeUnit = c("min", "s"))
  expect_identical(project$definitions$scenarios$s1$simulationTimeUnit, "min")
  expect_identical(project$definitions$scenarios$s2$simulationTimeUnit, "s")
  expect_identical(project$definitions$scenarios$s1$modelFile, "Aciclovir.pkml")
})

test_that("removeScenario removes a vector of ids in one write-through", {
  project <- testProject()
  addScenario(project, c("s1", "s2"), modelFile = "Aciclovir.pkml")
  removeScenario(project, c("s1", "s2"))
  expect_false(any(c("s1", "s2") %in% names(project$definitions$scenarios)))
  reloaded <- loadProject(project$info$projectFilePath)
  expect_false(any(c("s1", "s2") %in% names(reloaded$definitions$scenarios)))
})

# buildSimulations ----

test_that("buildSimulations returns simulation + population without running", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- testProject()
  built <- buildSimulations(project, scenarios = "testscenario")
  expect_named(built, "testscenario")
  expect_named(built$testscenario, c("simulation", "population"))
  expect_s3_class(built$testscenario$simulation, "Simulation")
  expect_null(built$testscenario$population)
})

test_that("buildSimulations attaches a Population for a population scenario", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- testProject()
  built <- buildSimulations(project, scenarios = "populationscenario")
  expect_s3_class(built$populationscenario$population, "Population")
})

test_that("buildSimulations applies customParams to the built simulation", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- testProject()
  path <- "Organism|Liver|EHC continuous fraction"
  built <- buildSimulations(
    project,
    scenarios = "testscenario",
    customParams = list(paths = path, values = 0.42, units = "")
  )
  applied <- ospsuite::getQuantityValuesByPath(
    quantityPaths = path,
    simulation = built$testscenario$simulation
  )
  expect_equal(applied, 0.42)
})

test_that("buildSimulations errors on an unknown scenario name", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- testProject()
  expect_error(
    buildSimulations(project, scenarios = "NopeNope"),
    regexp = "nopenope"
  )
})

test_that("buildSimulations rejects a non-Project", {
  expect_error(
    buildSimulations("not a project"),
    regexp = "must be a"
  )
})

# Print method ----

test_that("print.Scenario renders the configured fields", {
  project <- testProject()
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$definitions$scenarios[["testscenario"]]))
})
