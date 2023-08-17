defaultScenario <- function(projectConfiguration, loadPreSimulatedResults = FALSE, setTestParameters = FALSE) {
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

  # Adjust simulation run options, if necessary.
  # E.g. disable check for negative values if required
  simulationRunOptions <- SimulationRunOptions$new()
  simulationRunOptions$checkForNegativeValues <- FALSE

  customParams <- NULL
  # Apply parameters defined in "InputCoode/TestParameters.R"
  if (setTestParameters) {
    customParams <- getTestParameters(customParams)
  }

  # Replace by the folder where the resutls are stored, if applicable!
  resultsSubFolder <- "DateAndTimeSuffixForTheSubfolder"
  # Run or load scenarios
  if (loadPreSimulatedResults) {
    simulatedScenariosResults <- loadScenarioResults(
      names(scenarioConfigurations),
      file.path(projectConfiguration$outputFolder, "SimulationResults", resultsSubFolder)
    )
  } else {
    # Create scenarios
    scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations, customParams = customParams)

    simulatedScenariosResults <- runScenarios(
      scenarios = scenarios,
      simulationRunOptions = simulationRunOptions
    )
    saveScenarioResults(simulatedScenariosResults, projectConfiguration)
  }

  ########### Load observed data########
  # Which sheets to load

  # For compatibility with projects created with esqlabsR <5.0.1, use old data set
  # naming pattern.
  importerConfiguration <- ospsuite::loadDataImporterConfiguration(
    configurationFilePath = projectConfiguration$dataImporterConfigurationFile
  )
  importerConfiguration$namingPattern <- "{Molecule}_{Study Id}_{Subject Id}_{Species}_{Organ}_{Compartment}_{Dose}_{Route}_{Group Id}"

  dataSheets <- c("Laskin 1982.Group A")
  observedData <- esqlabsR::loadObservedData(
    projectConfiguration = projectConfiguration,
    sheets = dataSheets,
    importerConfiguration = importerConfiguration
  )

  ########## Create figures########
  # Output the names of loaded data sets to conveniently transfer them to the excel
  # file for figure specification
  # sort(names(observedData))
  ########## Create figures########
  plots <- createPlotsFromExcel(
    simulatedScenarios = simulatedScenariosResults,
    observedData = observedData,
    projectConfiguration = projectConfiguration,
    stopIfNotFound = TRUE
  )

  # Return a list with simulated scenarios and created plots
  return(list(simulatedScenariosResults = simulatedScenariosResults, plots = plots))
}
