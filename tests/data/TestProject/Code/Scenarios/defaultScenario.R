defaultScenario <- function(projectConfiguration){
  ###########Initializing and running scenarios########
  ospsuite.utils::validateIsOfType(projectConfiguration, ProjectConfiguration)
  #Create a base scenario configuration based on the current project configuration
  scenarioConfiguration <- esqlabsR::ScenarioConfiguration$new(projectConfiguration)
  # If set to `TRUE`, parameters defined in
  # "InputCoode/TestParameters.R" will be applied
  scenarioConfiguration$setTestParameters <- FALSE

  # Define which scenarios to run
  scenarioNames <- c("TestScenario")
  #Initialize and run scenarios
  simulatedScenarios <- esqlabsR::runScenarios(scenarioNames = scenarioNames, scenarioConfiguration = scenarioConfiguration, customParams = NULL, saveSimulationsToPKML = FALSE)

  ###########Load observed data - data template v10########
  #Which sheets to load
  dataSheets <- "Laskin 1982.Group A"
  observedData <- esqlabsR::loadObservedData(projectConfiguration = projectConfiguration,
                                   sheets = dataSheets)

  ##########Create figures########

  # Return simulated scenarios
  return(simulatedScenarios)
}
