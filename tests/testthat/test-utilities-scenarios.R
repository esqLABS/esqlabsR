##  context("runScenarios")
# Create a project configuration
projectConfiguration <- createDefaultProjectConfiguration(test_ProjectConfiguration())
defaultOutputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

test_that("It runs one scenario without specifying output paths", {
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
  expect_equal(simulatedScenarios[[scenarioNames[[1]]]]$results$allQuantityPaths, defaultOutputPath)
})

test_that("It runs one scenario with specifying output paths", {
  OutputPaths <- enum(list(
    Aciclovir_PVB = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    Aciclovir_bone_pls = "Organism|Bone|Plasma|Aciclovir|Concentration"
  ))

  # Define which scenarios to run
  scenarioNames <- c("TestScenario")
  # Create `ScenarioConfiguration` objects from excel files
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )

  for (scenarioConfiguration in scenarioConfigurations) {
    scenarioConfiguration$outputPaths <- enumValues(OutputPaths)
  }

  scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations)

  simulatedScenarios <- runScenarios(
    scenarios = scenarios
  )

  expect_equal(names(simulatedScenarios), scenarioNames)
  expect_equal(simulatedScenarios[[scenarioNames[[1]]]]$results$allQuantityPaths, enumValues(OutputPaths))
})

test_that("It runs two scenarios", {
  # Define which scenarios to run
  scenarioNames <- c(
    "TestScenario",
    "TestScenario2"
  )
  # Create `ScenarioConfiguration` objects from excel files
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )
  # Disable steady-state for second config
  scenarioConfigurations[[2]]$simulateSteadyState <- FALSE

  scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations)

  simulatedScenarios <- runScenarios(
    scenarios = scenarios
  )

  expect_equal(names(simulatedScenarios), scenarioNames)
  expect_equal(simulatedScenarios[[scenarioNames[[1]]]]$results$allQuantityPaths, defaultOutputPath)

  expect_equal(simulatedScenarios[[scenarioNames[[2]]]]$results$allQuantityPaths, c(
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    "Organism|Fat|Intracellular|Aciclovir|Concentration in container"
  ))
})

test_that("It runs population and individual scenarios", {
  # Define which scenarios to run
  scenarioNames <- c(
    "TestScenario",
    "PopulationScenario"
  )
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
  # Check that the first scenario is individual simulation
  expect_equal(length(simulatedScenarios[[scenarioNames[[1]]]]$results$allIndividualIds), 1)
  # Check that the second scenario is population simulation
  expect_equal(length(simulatedScenarios[[scenarioNames[[2]]]]$results$allIndividualIds), 2)
})
