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
