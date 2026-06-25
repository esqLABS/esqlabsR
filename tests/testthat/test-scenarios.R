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
      "modelParameterSets"
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
    esqlabsR:::.parseScenarios(NULL, list()),
    list()
  )
})

test_that(".parseScenarios copies basic fields for an individual scenario", {
  project <- exampleProject()
  sc <- project$scenarios[["Aciclovir_iv"]]

  expect_s3_class(sc, "Scenario")
  expect_identical(sc$scenarioName, "Aciclovir_iv")
  expect_identical(sc$modelFile, "Aciclovir.pkml")
  expect_identical(sc$individualId, "Adult_male")
  expect_identical(sc$applicationProtocol, "Aciclovir_iv_250mg")
  expect_identical(sc$modelParameterSets, c("Global", "Aciclovir"))
  expect_null(sc$populationId)
  expect_identical(sc$simulationType, "Individual")
  expect_false(sc$readPopulationFromCSV)
})

test_that(".parseScenarios sets simulationType=Population when populationId present", {
  project <- exampleProject()
  sc <- project$scenarios[["Aciclovir_iv_population"]]

  expect_identical(sc$populationId, "European_adults")
  expect_identical(sc$simulationType, "Population")
})

test_that(".parseScenarios defaults applicationProtocol to NA when JSON has null", {
  raw <- list(
    list(
      name = "X",
      individualId = "i",
      modelFile = "m.pkml",
      applicationProtocol = NULL
    )
  )
  result <- esqlabsR:::.parseScenarios(raw, list())

  expect_length(result, 1L)
  expect_true(is.na(result[["X"]]$applicationProtocol))
})

test_that(".parseScenarios converts steadyStateTime to base units (minutes)", {
  project <- exampleProject()
  sc <- project$scenarios[["Aciclovir_iv_steadystate"]]

  expect_true(sc$simulateSteadyState)
  # 1 hour -> 60 minutes
  expect_equal(sc$steadyStateTime, 60)
  expect_identical(sc$steadyStateTimeUnit, "h")
})

test_that(".parseScenarios leaves simulateSteadyState=FALSE when JSON omits/sets false", {
  project <- exampleProject()
  sc <- project$scenarios[["Aciclovir_iv"]]

  expect_false(sc$simulateSteadyState)
  expect_null(sc$steadyStateTimeUnit)
  # The class default of 1000 stays put when JSON's steadyStateTime is null.
  expect_identical(sc$steadyStateTime, 1000)
})

test_that(".parseScenarios errors when steadyStateTime set without unit", {
  raw <- list(
    list(
      name = "BadSS",
      individualId = "i",
      modelFile = "m.pkml",
      steadyStateTime = 5,
      steadyStateTimeUnit = NULL
    )
  )
  expect_error(
    esqlabsR:::.parseScenarios(raw, list()),
    "BadSS.*steadyStateTime.*steadyStateTimeUnit"
  )
})

test_that(".parseScenarios parses simulationTime to a list of length-3 numerics", {
  project <- exampleProject()
  sc <- project$scenarios[["Aciclovir_iv"]]

  expect_type(sc$simulationTime, "list")
  expect_length(sc$simulationTime, 1L)
  expect_identical(sc$simulationTime[[1L]], c(0, 24, 60))
  expect_identical(sc$simulationTimeUnit, "h")
})

test_that(".parseScenarios resolves outputPathIds to literal outputPaths in declared order", {
  project <- exampleProject()
  sc <- project$scenarios[["Aciclovir_iv_steadystate"]]

  expect_type(sc$outputPaths, "character")
  expect_length(sc$outputPaths, 2L)
  # Names are the ids, values are the literal paths; order follows JSON declaration.
  expect_named(sc$outputPaths, c("Aciclovir_fat_cell", "Aciclovir_PVB"))
  expect_identical(
    unname(sc$outputPaths),
    c(
      "Organism|Fat|Intracellular|Aciclovir|Concentration in container",
      "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
    )
  )
})

test_that(".parseScenarios single outputPathId resolves to a length-1 named character vector", {
  project <- exampleProject()
  sc <- project$scenarios[["Aciclovir_iv"]]

  expect_type(sc$outputPaths, "character")
  expect_length(sc$outputPaths, 1L)
  expect_named(sc$outputPaths, "Aciclovir_PVB")
  expect_identical(
    unname(sc$outputPaths),
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  )
})

test_that(".parseScenarios errors on unknown outputPathIds with the scenario name", {
  raw <- list(
    list(
      name = "BadRefs",
      individualId = "i",
      modelFile = "m.pkml",
      outputPathIds = list("Aciclovir_PVB", "Nope", "AlsoNope")
    )
  )
  outputPaths <- list(Aciclovir_PVB = "Organism|PVB|...")

  expect_error(
    esqlabsR:::.parseScenarios(raw, outputPaths),
    "BadRefs.*Nope.*AlsoNope"
  )
})

test_that(".parseScenarios leaves outputPaths NULL when JSON omits outputPathIds", {
  raw <- list(
    list(
      name = "NoOutputs",
      individualId = "i",
      modelFile = "m.pkml"
    )
  )
  result <- esqlabsR:::.parseScenarios(raw, list())

  expect_null(result[["NoOutputs"]]$outputPaths)
})

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

test_that("removeScenario uses scenarioName argument matching addScenario", {
  project <- testProject()
  addScenario(
    project,
    scenarioName = "ToRemove",
    modelFile = "Aciclovir.pkml"
  )
  expect_true("ToRemove" %in% names(project$scenarios))
  removeScenario(project, scenarioName = "ToRemove")
  expect_false("ToRemove" %in% names(project$scenarios))
})

test_that("addScenario stores steadyStateTime in base units and round-trips the declared unit", {
  project <- testProject()
  addScenario(
    project,
    scenarioName = "SS",
    modelFile = "Aciclovir.pkml",
    individualId = "Indiv1",
    steadyState = TRUE,
    steadyStateTime = 10,
    steadyStateTimeUnit = "h"
  )

  # Stored value is the base unit (minutes): 10 h -> 600 min.
  expect_equal(project$scenarios[["SS"]]$steadyStateTime, 600)
  expect_equal(project$scenarios[["SS"]]$steadyStateTimeUnit, "h")

  # Saved JSON carries the declared 10 / "h" (the serializer converts the
  # base-unit value back to the declared unit).
  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)
  raw <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  savedSS <- Filter(\(s) identical(s[["name"]], "SS"), raw$scenarios)[[1]]
  expect_equal(savedSS$steadyStateTime, 10)
  expect_equal(savedSS$steadyStateTimeUnit, "h")

  # Reload round-trips back to the base-unit value.
  reloaded <- loadProject(out)
  expect_equal(reloaded$scenarios[["SS"]]$steadyStateTime, 600)
  expect_equal(reloaded$scenarios[["SS"]]$steadyStateTimeUnit, "h")
})
