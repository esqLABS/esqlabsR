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
