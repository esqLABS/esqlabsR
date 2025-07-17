test_that("Scenario can be created from excel file", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

  # Define which scenarios to run
  scenarioNames <- c("TestScenario")
  # Create `ScenarioConfiguration` objects from excel files
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )

  scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations)

  expect_equal(length(scenarios), 1)
  expect_equal(names(scenarios), scenarioNames)
})

test_that("Scenario can be run", {
  # Create a project configuration using temporary project
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

  # Define which scenarios to run
  scenarioNames <- c("TestScenario")
  # Create `ScenarioConfiguration` objects from excel files
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )

  scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations)

  simulatedScenarios <- runScenarios(
    scenarios = scenarios
  )

  expect_equal(names(simulatedScenarios), scenarioNames)
})

test_that("It throws an error when the application protocol is not found", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
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
    messages$errorApplicationProtocolNotFound(
      scenarioNames[[1]],
      "NonExistentProtocol"
    )
  )
})

test_that("The name of a scenario is set as simulation name", {
  # Create a project configuration using temporary project
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

  scenarioNames <- c(
    "TestScenario"
  )
  # Create `ScenarioConfiguration` objects from excel files
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )
  scenarios <- createScenarios(
    scenarioConfigurations = scenarioConfigurations,
    stopIfParameterNotFound = FALSE
  )

  # Check if the name of the simulation is set to the name of the scenario
  expect_equal(scenarios[[1]]$simulation$name, scenarioNames[[1]])
})
