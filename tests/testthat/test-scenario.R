# Create a project configuration
projectConfiguration <- testProjectConfiguration()

test_that("It throws an error when the application protocol is not found", {
  skip_on_os("mac")

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

test_that("Scenario properties are read-only", {
  skip_on_os("mac")

  scenarioNames <- c("TestScenario")
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )

  scenario <- Scenario$new(
    scenarioConfigurations$TestScenario,
    stopIfParameterNotFound = FALSE
  )

  # Test that properties are read-only
  expect_error(
    scenario$scenarioConfiguration <- "new value"
  )
  expect_error(scenario$finalCustomParams <- "new value")
  expect_error(scenario$simulation <- "new value")
  expect_error(scenario$population <- "new value")
  expect_error(scenario$scenarioType <- "new value")
})

test_that("Scenario type is correctly identified", {
  skip_on_os("mac")

  # Create individual scenario
  scenarioNames <- c("TestScenario")
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )

  individualScenario <- Scenario$new(
    scenarioConfigurations$TestScenario,
    stopIfParameterNotFound = FALSE
  )
  expect_equal(individualScenario$scenarioType, "Individual")

  # Create population scenario
  scenarioNames <- c("PopulationScenario")
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )

  populationScenario <- Scenario$new(
    scenarioConfigurations$PopulationScenario,
    stopIfParameterNotFound = FALSE
  )
  expect_equal(populationScenario$scenarioType, "Population")
})

test_that("Custom parameters are correctly applied", {
  skip_on_os("mac")

  scenarioNames <- c("TestScenario")
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )

  # Define custom parameter
  customParams <- list(
    paths = "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
    values = 500,
    units = "mg"
  )

  scenario <- Scenario$new(
    scenarioConfigurations$TestScenario,
    customParams = customParams,
    stopIfParameterNotFound = FALSE
  )

  # Check that the custom parameter was applied
  idx <- which(
    scenario$finalCustomParams$paths ==
      "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose"
  )
  expect_equal(scenario$finalCustomParams$values[[idx]], 500)
})

test_that("Print method works", {
  skip_on_os("mac")

  scenarioNames <- c("TestScenario")
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )

  scenario <- Scenario$new(
    scenarioConfigurations$TestScenario,
    stopIfParameterNotFound = FALSE
  )

  # Test that print method returns the object invisibly
  expect_equal(scenario, scenario$print())
})

test_that("Warning is shown when individual characteristics are not found", {
  skip_on_os("mac")

  scenarioNames <- c("TestScenario")
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )

  # Set individual ID to non-existent individual
  scenarioConfigurations$TestScenario$individualId <- "NonExistentIndividual"

  expect_warning(
    Scenario$new(
      scenarioConfigurations$TestScenario,
      stopIfParameterNotFound = FALSE
    )
  )
})



test_that("Population from CSV is loaded correctly", {
  skip_on_os("mac")

  scenarioNames <- c("PopulationScenarioFromCSV")
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )

  # Ensure readPopulationFromCSV is set to TRUE
  scenarioConfigurations$PopulationScenarioFromCSV$readPopulationFromCSV <- TRUE

  scenario <- Scenario$new(
    scenarioConfigurations$PopulationScenarioFromCSV,
    stopIfParameterNotFound = FALSE
  )

  expect_true(isOfType(scenario$population, "Population"))
  expect_equal(scenario$scenarioType, "Population")
})

test_that("The name of a scenario is set as simulation name", {
  projectConfiguration <- testProjectConfiguration()
  scenarioNames <- c(
    "TestScenario"
  )
  # Create `ScenarioConfiguration` objects from excel files
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )
  scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations, stopIfParameterNotFound = FALSE)

  # Check if the name of the simulation is set to the name of the scenario
  expect_equal(scenarios[[1]]$simulation$name, scenarioNames[[1]])
})
