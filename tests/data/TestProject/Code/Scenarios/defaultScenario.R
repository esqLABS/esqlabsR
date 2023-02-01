defaultScenario <- function(projectConfiguration, loadPreSimulatedResults = FALSE) {
  ########### Initializing and running scenarios########
  ospsuite.utils::validateIsOfType(projectConfiguration, ProjectConfiguration)

  # Define which scenarios to run
  scenarioNames <- c("TestScenario")
  # Set scenario names to NULL if you want to simulate all scenarios defined in the
  # excel file
  # scenarioNames <- NULL

  # Create `ScenarioConfiguration` objects from excel files
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )

  # Adjust scenario configurations, if necessary.
  # E.g., enable setting of test parameters. If set to `TRUE`, parameters
  # defined in "InputCoode/TestParameters.R" will be applied
  scenarioConfigurations[[1]]$setTestParameters <- FALSE

  # Disable check for negative values if required
  scenarioConfigurations[[1]]$simulationRunOptions <- SimulationRunOptions$new()
  scenarioConfigurations[[1]]$simulationRunOptions$checkForNegativeValues <- FALSE


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

  ########### Load observed data########
  # Which sheets to load
  dataSheets <- "Laskin 1982.Group A"
  observedData <- esqlabsR::loadObservedData(
    projectConfiguration = projectConfiguration,
    sheets = dataSheets
  )

  ########## Create figures########
  # Output the names of loaded data sets to conveniently transfer them to the excel
  # file for figure specification
  # sort(names(observedData))
  ########## Create figures########
  plots <- createPlotsFromExcel(
    simulatedScenarios = simulatedScenarios,
    observedData = observedData,
    projectConfiguration = projectConfiguration,
    stopIfNotFound = TRUE
  )

  # Return a list with simulated scenarios and created plots
  return(list(simulatedScenarios = simulatedScenarios, plots = plots))
}
