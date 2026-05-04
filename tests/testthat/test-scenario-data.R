# Tests for the internal ScenarioData class and the .parseScenarios helper.
# ScenarioData is the transitional internal name for the plain-data scenario
# class that Chapter 3 will rename to `Scenario` and export. Both symbols
# (and the parser) are intentionally unexported, hence `:::`.

example_project_json_path <- function() {
  system.file(
    "extdata",
    "projects",
    "Example",
    "Project.json",
    package = "esqlabsR",
    mustWork = TRUE
  )
}

test_that("ScenarioData has the documented field defaults", {
  sc <- esqlabsR:::ScenarioData$new()

  expect_s3_class(sc, "ScenarioData")
  expect_s3_class(sc, "R6")

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
  expect_null(sc$modelParameters)
})

test_that(".parseScenarios returns list() for NULL input", {
  expect_identical(
    esqlabsR:::.parseScenarios(NULL, list()),
    list()
  )
})

test_that(".parseScenarios copies basic fields for an individual scenario", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())
  sc <- project$scenarios[["Aciclovir_iv"]]

  expect_s3_class(sc, "ScenarioData")
  expect_identical(sc$scenarioName, "Aciclovir_iv")
  expect_identical(sc$modelFile, "Aciclovir.pkml")
  expect_identical(sc$individualId, "Adult_male")
  expect_identical(sc$applicationProtocol, "Aciclovir_iv_250mg")
  expect_identical(sc$modelParameters, c("Global", "Aciclovir"))
  expect_null(sc$populationId)
  expect_identical(sc$simulationType, "Individual")
  expect_false(sc$readPopulationFromCSV)
})

test_that(".parseScenarios sets simulationType=Population when populationId present", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())
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
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())
  sc <- project$scenarios[["Aciclovir_iv_steadystate"]]

  expect_true(sc$simulateSteadyState)
  # 1 hour -> 60 minutes
  expect_equal(sc$steadyStateTime, 60)
  expect_identical(sc$steadyStateTimeUnit, "h")
})

test_that(".parseScenarios leaves simulateSteadyState=FALSE when JSON omits/sets false", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())
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
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())
  sc <- project$scenarios[["Aciclovir_iv"]]

  expect_type(sc$simulationTime, "list")
  expect_length(sc$simulationTime, 1L)
  expect_identical(sc$simulationTime[[1L]], c(0, 24, 60))
  expect_identical(sc$simulationTimeUnit, "h")
})

test_that(".parseScenarios resolves outputPathIds to literal outputPaths in declared order", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())
  sc <- project$scenarios[["Aciclovir_iv_steadystate"]]

  expect_type(sc$outputPaths, "character")
  expect_length(sc$outputPaths, 2L)
  # Order in JSON is fat_cell, PVB; the literal paths must follow that order.
  expect_identical(
    sc$outputPaths,
    c(
      "Organism|Fat|Intracellular|Aciclovir|Concentration in container",
      "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
    )
  )
})

test_that(".parseScenarios single outputPathId resolves to a length-1 character vector", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())
  sc <- project$scenarios[["Aciclovir_iv"]]

  expect_type(sc$outputPaths, "character")
  expect_length(sc$outputPaths, 1L)
  expect_identical(
    sc$outputPaths,
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
