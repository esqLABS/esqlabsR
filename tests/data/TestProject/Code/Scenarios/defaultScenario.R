defaultScenario <- function(projectConfiguration) {
  ########### Initializing and running scenarios########
  ospsuite.utils::validateIsOfType(projectConfiguration, ProjectConfiguration)

  # Define which scenarios to run
  scenarioNames <- c("TestScenario")

  # Create `ScenarioConfiguration` objects from excel files
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )

  # Adjust scenario configurations, if necessary.
  # E.g., enable setting of test parameters. If set to `TRUE`, parameters
  # defined in "InputCoode/TestParameters.R" will be applied
  scenarioConfigurations[[1]]$setTestParameters <- FALSE
  # Set output paths for each scenario
  for (scenarioConfiguration in scenarioConfigurations) {
    scenarioConfiguration$outputPaths <- enumValues(OutputPaths)
  }

  # Run scenarios
  simulatedScenarios <- runScenarios(
    scenarioConfigurations = scenarioConfigurations,
    customParams = NULL, saveSimulationsToPKML = FALSE
  )

  ########### Load observed data - data template v10########
  # Which sheets to load
  dataSheets <- "Laskin 1982.Group A"
  observedData <- esqlabsR::loadObservedData(
    projectConfiguration = projectConfiguration,
    sheets = dataSheets
  )

  ########## Create figures########

  # Return simulated scenarios
  return(simulatedScenarios)
}
