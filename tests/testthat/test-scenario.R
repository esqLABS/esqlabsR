# Create a project configuration
projectConfiguration <- testProjectConfiguration()

test_that("It throws an error when the application protocol is not found", {
  scenarioNames <- c(
    "TestScenario"
  )
  # Create `ScenarioConfiguration` objects from excel files
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )

  # Set application protocol to a non existent
  scenarioConfigurations$TestScenario$applicationProtocol <- "NonExistentProtocol"

  expect_error(
    Scenario$new(scenarioConfigurations$TestScenario),
    messages$errorApplicationProtocolNotFound(scenarioNames[[1]], "NonExistentProtocol")
  )
})
