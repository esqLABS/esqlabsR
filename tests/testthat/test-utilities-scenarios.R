##  context("runScenarios")
# Create a project configuration
projectConfiguration <- createDefaultProjectConfiguration(path = "../data/ProjectConfiguration_forTests.xlsx")
defaultOutputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

test_that("It runs one scenario without specifying output paths", {
  # Define which scenarios to run
  scenarioNames <- c("TestScenario")
  # Create `ScenarioConfiguration` objects from excel files
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )
  simulatedScenarios <- runScenarios(
    scenarioConfigurations = scenarioConfigurations,
    customParams = NULL, saveSimulationsToPKML = FALSE
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
  simulatedScenarios <- runScenarios(
    scenarioConfigurations = scenarioConfigurations,
    customParams = NULL, saveSimulationsToPKML = FALSE
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
  simulatedScenarios <- runScenarios(
    scenarioConfigurations = scenarioConfigurations,
    customParams = NULL, saveSimulationsToPKML = FALSE
  )

  expect_equal(names(simulatedScenarios), scenarioNames)
  expect_equal(simulatedScenarios[[scenarioNames[[1]]]]$results$allQuantityPaths, defaultOutputPath)
})

# test_that("It saves simulations if saveSimulationsToPKML = TRUE", {
#   #ToDo - have to write to temp dir!
# })
