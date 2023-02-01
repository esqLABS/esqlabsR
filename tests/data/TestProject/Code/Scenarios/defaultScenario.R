defaultScenario <- function(projectConfiguration, loadPreSimulatedResults = FALSE) {
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


  customParams <- NULL
  # Replace by the folder where the resutls are stored, if applicable!
  resultsSubFolder <- "DateAndTimeSuffixForTheSubfolder"
  # Run or load scenarios
  if (loadPreSimulatedResults) {
    simulatedScenarios <- loadScenarioResults(names(scenarioConfigurations), file.path(projectConfiguration$outputFolder, "SimulationResults", resultsSubFolder))
  } else {
    simulatedScenarios <- runScenarios(
      scenarioConfigurations = scenarioConfigurations,
      customParams = customParams, saveSimulationsToPKML = TRUE
    )
    saveScenarioResults(simulatedScenarios, projectConfiguration)
  }

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
