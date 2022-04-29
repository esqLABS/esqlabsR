#' Read scenario definition from excel file
#'
#' @details Reads scenario definition from the excel file defined in
#' `ProjectConfiguration` and updates the `ScenarioConfiguration` with new
#' information.
#'
#' @param scenarioConfiguration A `ScenarioConfiguration` that will be updated.
#'
#' @return Updated `ScenarioConfiguration`
#' @export
#'
#' @examples
#' \dontrun{
#' # Create default ProjectConfiguration
#' projectConfiguration <- createDefaultProjectConfiguration()
#' # Create ScenarioConfiguration from ProjectConfiguration
#' scenarioConfiguration <- ScenarioConfiguration$new(projectConfiguration)
#' # Set scenario name
#' scenarioConfiguration$scenarioName <- "MyScenario"
#' # Read scenario definition from excel
#' scenarioConfiguration <- readScenarioConfigurationFromExcel(scenarioConfiguration)
#' }
readScenarioConfigurationFromExcel <- function(scenarioConfiguration) {
  # Current scenario definition structure:
  # "Scenario_name", "IndividualId", "ModelParameterSheets", "ApplicationProtocol",
  # "SimulationTime", "SimulationTimeUnit", "SteadyState", "ModelFile"
  colTypes <- c("text", "text", "text", "text", "numeric", "text", "logical", "text")
  data <- readExcel(
    path = file.path(
      scenarioConfiguration$projectConfiguration$paramsFolder,
      scenarioConfiguration$projectConfiguration$scenarioDefinitionFile
    ),
    col_types = colTypes
  )
  # Select the scenario
  if (!scenarioConfiguration$scenarioName %in% data$Scenario_name) {
    stop("readScenarioDefinition: Scenario '", scenarioConfiguration$scenarioName, "' is not specified!")
  }
  data <- data[data$Scenario_name == scenarioConfiguration$scenarioName, ]

  # Parameter sheets
  scenarioConfiguration$removeParamSheets()
  paramSheets <- gsub(data$ModelParameterSheets, pattern = " ", replacement = "", fixed = TRUE)
  if (!is.na(paramSheets)) {
    scenarioConfiguration$addParamSheets(strsplit(x = paramSheets, split = ",", fixed = TRUE)[[1]])
  }

  # Simulation time
  simTime <- data$SimulationTime
  simTimeUnit <- data$SimulationTimeUnit
  scenarioConfiguration$simulationTime <- ospsuite::toBaseUnit(ospDimensions$Time, values = simTime, unit = simTimeUnit)

  # Individual id
  scenarioConfiguration$individualId <- data$IndividualId

  # Application protocol
  scenarioConfiguration$applicationProtocol <- data$ApplicationProtocol

  # Simulate steady-state?
  scenarioConfiguration$simulateSteadyState <- data$SteadyState

  # Model file
  scenarioConfiguration$modelFile <- data$ModelFile

  return(scenarioConfiguration)
}

#' Set applications
#'
#' @details Set the parameter values describing the application protocol
#' defined in the scenario configuration. Either calling a function that is stored
#' in the `applicationProotocolsEnum`, or from excel.
#'
#' @param simulation A `Simulation` object that will be modified.
#' @param scenarioConfiguration A `ScenarioConfiguration` object holding the
#' name of the application protocol.
#' @param applicationProtocolsEnum (Optional) A named list with functions that
#' define setting the application. If `NULL` or no entry with the application
#' protocol defined in the `scenarioConfiguiration`, the parameters to set
#' are extracted from excel.
#'
#' @export
setApplications <- function(simulation, scenarioConfiguration, applicationProtocolsEnum = NULL) {
  applicationName <- scenarioConfiguration$applicationProtocol

  # If the application is defined in the enum, call the function
  if (!is.null(applicationProtocolsEnum) &&
    enumHasKey(key = applicationName, enum = applicationProtocolsEnum)) {
    applicationProtocolsEnum[[applicationName]]()
  } else {
    # Otherwise, set from excel
    .setApplicationFromExcel(scenarioConfiguration = scenarioConfiguration, simulation = simulation)
  }
}

#' Set application protocol from excel
#'
#' @param scenarioConfiguration A `ScenarioConfiguration` object holding the
#' name of the application protocol.
#' @param simulation A `Simulation` object that will be modified.
#' @keywords internal
.setApplicationFromExcel <- function(scenarioConfiguration, simulation) {
  excelFilePath <- file.path(
    scenarioConfiguration$projectConfiguration$paramsFolder,
    scenarioConfiguration$projectConfiguration$scenarioApplicationsFile
  )
  # Only try to apply parameters if the sheet exists
  if (scenarioConfiguration$applicationProtocol %in% readxl::excel_sheets(excelFilePath)) {
    params <- readParametersFromXLS(excelFilePath, scenarioConfiguration$applicationProtocol)
    ospsuite::setParameterValuesByPath(
      parameterPaths = params$paths, values = params$values,
      simulation = simulation, units = params$units
    )
  }
}
