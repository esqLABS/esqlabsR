#' Read scenario definition(s) from excel file
#'
#' @param scenarioNames Names of the scenarios that are defined in the excel file
#' @param projectConfiguration A `ProjectConfiguration` object holding base information
#'
#' @details Reads scenario definition from the excel file defined in
#' `ProjectConfiguration` and creates `ScenarioConfiguration` objects with new
#' information.
#' If a scenario that is specified in `scenarioNames` is not found in the excel
#' file, an error is thrown.
#'
#' @return A named list of `ScenarioConfiguration` objects withe the names of the
#' list being the `scenarioNames`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create default ProjectConfiguration
#' projectConfiguration <- createDefaultProjectConfiguration()
#' scenarioName <- "MyScenario"
#' # Read scenario definition from excel
#' scenarioConfiguration <- readScenarioConfigurationFromExcel(scenarioConfiguration)[[scenarioName]]
#' }
readScenarioConfigurationFromExcel <- function(scenarioNames, projectConfiguration) {
  validateIsString(scenarioNames)
  validateIsOfType(projectConfiguration, ProjectConfiguration)

  # Current scenario definition structure:
  # "Scenario_name", "IndividualId", "ModelParameterSheets", "ApplicationProtocol",
  # "SimulationTime", "SimulationTimeUnit", "SteadyState", "ModelFile"
  colTypes <- c("text", "text", "text", "text", "numeric", "text", "logical", "text")
  wholeData <- readExcel(
    path = file.path(
      projectConfiguration$paramsFolder,
      projectConfiguration$scenarioDefinitionFile
    ),
    col_types = colTypes
  )

  # Create a scenario configuration for each name
  scenarioConfigurations <- vector("list", length(scenarioNames))
  for (i in seq_along(scenarioNames)) {
    scenarioName <- scenarioNames[[i]]

    # Select the scenario
    if (!scenarioName %in% wholeData$Scenario_name) {
      stop(messages$scenarioConfigurationNameNotFoundWhenReading(scenarioName))
    }
    data <- wholeData[wholeData$Scenario_name == scenarioName, ]

    # Create a base scenario configuration based on the current project configuration
    scenarioConfiguration <- ScenarioConfiguration$new(projectConfiguration)

    # Scenario name
    scenarioConfiguration$scenarioName <- scenarioName
    # Parameter sheets
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

    # Add the new ScenarioConfiguration to the output list
    scenarioConfigurations[[i]] <- scenarioConfiguration
  }
  names(scenarioConfigurations) <- scenarioNames

  return(scenarioConfigurations)
}

#' Set an application protocol in a `Simulation`
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
