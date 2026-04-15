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

test_that("Scenario properties are read-only", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

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
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

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
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

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
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

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
  expect_output(result <- scenario$print())
  expect_equal(scenario, result)
})

test_that("Warning is shown when individual characteristics are not found", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

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
    ),
    regexp = messages$warningNoIndividualCharacteristics(
      "TestScenario",
      "NonExistentIndividual"
    )
  )
})

test_that("Individual parameter sets from 'Individual Parameter Sets' column are applied", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

  scenarioNames <- c("TestScenario")
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )

  # TestScenario uses Indiv1, whose "Individual Parameter Sets" column points to
  # the "Indiv1" sheet (GFR = 90 ml/min). Verify this parameter is applied.
  scenario <- Scenario$new(
    scenarioConfigurations$TestScenario,
    stopIfParameterNotFound = FALSE
  )

  gfrPath <- "Organism|Kidney|GFR"
  idx <- which(scenario$finalCustomParams$paths == gfrPath)
  expect_true(length(idx) > 0)
  expect_equal(scenario$finalCustomParams$values[[idx]], 90)
  expect_equal(scenario$finalCustomParams$units[[idx]], "ml/min")
})

test_that("Error is thrown when an individual parameter set is not found", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

  # Add a fake individual with a mapping to a non-existent parameter set
  projectConfiguration$individuals[["TestIndiv_bad_set"]] <-
    ospsuite::createIndividualCharacteristics(
      species = ospsuite::Species$Human,
      population = ospsuite::HumanPopulation$European_ICRP_2002,
      gender = ospsuite::Gender$Male,
      weight = 70,
      height = 170,
      age = 30
    )
  projectConfiguration$individualParameterSetMapping[["TestIndiv_bad_set"]] <-
    "NonExistentSheet"

  scenarioNames <- c("TestScenario")
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )
  scenarioConfigurations$TestScenario$individualId <- "TestIndiv_bad_set"

  expect_error(
    Scenario$new(
      scenarioConfigurations$TestScenario,
      stopIfParameterNotFound = FALSE
    ),
    regexp = messages$errorIndividualParameterSetNotFound(
      "TestScenario",
      "NonExistentSheet"
    )
  )
})


test_that("Population from CSV is loaded correctly", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

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
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

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

# JSON integration ----

test_that("Scenario initializes from JSON-loaded ProjectConfiguration", {
  pc <- testProjectConfigurationJSON()
  sc <- pc$scenarioConfigurations[["TestScenario"]]
  scenario <- Scenario$new(sc)
  expect_s3_class(scenario, "Scenario")
  expect_false(is.null(scenario$simulation))
  expect_equal(scenario$scenarioConfiguration$scenarioName, "TestScenario")
})

test_that("Scenario initializes with steady state and multiple param groups", {
  pc <- testProjectConfigurationJSON()
  sc <- pc$scenarioConfigurations[["TestScenario2"]]
  scenario <- Scenario$new(sc)
  expect_s3_class(scenario, "Scenario")
  expect_equal(length(scenario$scenarioConfiguration$simulationTime), 2)
})
