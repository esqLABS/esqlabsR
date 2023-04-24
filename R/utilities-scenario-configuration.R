#' Read scenario definition(s) from excel file
#'
#' @param scenarioNames Names of the scenarios that are defined in the excel file.
#' If `NULL` (default), all scenarios specified in the excel file will be
#' created.
#' @param projectConfiguration A `ProjectConfiguration` object holding base information
#'
#' @details Reads scenario definition from the excel file defined in
#' `ProjectConfiguration` and creates `ScenarioConfiguration` objects with new
#' information.
#' If a scenario that is specified in `scenarioNames` is not found in the excel
#' file, an error is thrown.
#'
#' @return A named list of `ScenarioConfiguration` objects withe the names of the
#' list being scenario names.
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
readScenarioConfigurationFromExcel <- function(scenarioNames = NULL, projectConfiguration) {
  validateIsString(scenarioNames, nullAllowed = TRUE)
  validateIsOfType(projectConfiguration, ProjectConfiguration)

  # Current scenario definition structure:
  expectedColumns <- c(
    "Scenario_name", "IndividualId", "PopulationId", "ReadPopulationFromCSV", "ModelParameterSheets", "ApplicationProtocol",
    "SimulationTime", "SimulationTimeUnit", "SteadyState", "SteadyStateTime", "SteadyStateTimeUnit", "ModelFile",
    "OutputPathsIds"
  )
  # Define the casting functions to cast columns to specific type
  colTypes <- c(
    "text", "text", "text", "logical", "text",
    "text", "numeric", "text", "logical",
    "numeric", "text", "text", "text"
  )

  # Read only the header of the excel file to check structure
  header <- readExcel(
    path = file.path(
      projectConfiguration$paramsFolder,
      projectConfiguration$scenarioDefinitionFile
    ),
    sheet = "Scenarios",
    n_max = 0
  )

  # Check if the structure is correct
  if (!identical(names(header), expectedColumns)) {
    stop(messages$errorWrongXLSStructure(filePath = file.path(
      projectConfiguration$paramsFolder,
      projectConfiguration$scenarioDefinitionFile
    ), expectedColNames = expectedColumns))
  }

  # If no errors were raised before, structure is correct. Whole excel file is
  # read with column types.

  wholeData <- readExcel(
    path = file.path(
      projectConfiguration$paramsFolder,
      projectConfiguration$scenarioDefinitionFile
    ),
    sheet = "Scenarios",
    col_types = colTypes
  )

  # Remove empty rows
  wholeData <- wholeData[rowSums(is.na(wholeData)) != ncol(wholeData), ]

  outputPathsDf <- readExcel(
    path = file.path(
      projectConfiguration$paramsFolder,
      projectConfiguration$scenarioDefinitionFile
    ),
    sheet = "OutputPaths"
  )

  scenarioNames <- scenarioNames %||% wholeData$Scenario_name
  # Create a scenario configuration for each name
  scenarioConfigurations <- vector("list", length(scenarioNames))
  for (i in seq_along(scenarioNames)) {
    scenarioName <- scenarioNames[[i]]

    # Select the scenario
    if (!any(wholeData$Scenario_name == scenarioName)) {
      stop(messages$scenarioConfigurationNameNotFoundWhenReading(scenarioName))
    }
    data <- wholeData[wholeData$Scenario_name == scenarioName, ]
    # If multiple rows with the same scenario name if present, stop with an error
    if (nrow(data) > 1) {
      stop(messages$stopScenarioNameNonUnique(scenarioName))
    }

    # Create a base scenario configuration based on the current project configuration
    scenarioConfiguration <- ScenarioConfiguration$new(projectConfiguration)

    # Scenario name
    scenarioConfiguration$scenarioName <- scenarioName
    # Parameter sheets
    paramSheets <- data$ModelParameterSheets
    if (!is.na(paramSheets)) {
      sheetNames <- strsplit(x = paramSheets, split = ",", fixed = TRUE)[[1]]
      # Remove leading/trailing whitespaces
      sheetNames <- trimws(sheetNames)
      scenarioConfiguration$addParamSheets(sheetNames)
    }

    # Simulation time
    simTime <- data$SimulationTime
    simTimeUnit <- data$SimulationTimeUnit
    # Set the time only if new value is defined
    if (!is.na(simTime)) {
      scenarioConfiguration$simulationTime <-
        ospsuite::toBaseUnit(ospDimensions$Time, values = simTime, unit = simTimeUnit)
    }

    # Individual id
    scenarioConfiguration$individualId <- data$IndividualId

    # Population id
    if (!is.na(data$PopulationId)) {
      scenarioConfiguration$populationId <- data$PopulationId
      scenarioConfiguration$simulationType <- "Population"
    }

    # ReadPopulationFromCSV
    if (!is.na(data$ReadPopulationFromCSV)) {
      scenarioConfiguration$readPopulationFromCSV <- data$ReadPopulationFromCSV
    }

    # Application protocol
    scenarioConfiguration$applicationProtocol <- data$ApplicationProtocol

    # Simulate steady-state?
    if (!is.na(data$SteadyState)) {
      scenarioConfiguration$simulateSteadyState <- data$SteadyState
    }

    # Steady-state time
    ssTime <- data$SteadyStateTime
    ssTimeUnit <- data$SteadyStateTimeUnit

    if (!is.na(ssTime)) {
      scenarioConfiguration$steadyStateTime <- ospsuite::toBaseUnit(
        quantityOrDimension = ospDimensions$Time,
        values = ssTime,
        unit = ssTimeUnit
      )
    }

    # Model file
    scenarioConfiguration$modelFile <- data$ModelFile

    # OutputPaths
    if (!is.na(data$OutputPathsIds)) {
      pathIds <- strsplit(x = data$OutputPathsIds, split = ",", fixed = TRUE)[[1]]
      # Remove leading/trailing whitespaces
      pathIds <- trimws(pathIds)
      # Check if all paths IDs are defined in the OutputPaths sheet
      missingIds <- setdiff(pathIds, outputPathsDf$OutputPathId)
      if (length(missingIds) != 0) {
        stop(messages$invalidOutputPathIdsfunction(outputPathIds = missingIds, scenarioName = scenarioName))
      }
      # Get the paths corresponding to the ids
      outputPaths <- dplyr::filter(outputPathsDf, OutputPathId %in% pathIds)$OutputPath

      scenarioConfiguration$outputPaths <- outputPaths
    }

    # Add the new ScenarioConfiguration to the output list
    scenarioConfigurations[[i]] <- scenarioConfiguration
  }
  names(scenarioConfigurations) <- scenarioNames

  return(scenarioConfigurations)
}

#' Set an application protocol in a `Simulation` from the excel file.
#'
#' @details Set the parameter values describing the application protocol
#' defined in the scenario configuration.
#'
#' @param simulation A `Simulation` object that will be modified.
#' @param scenarioConfiguration A `ScenarioConfiguration` object holding the
#' name of the application protocol.
#'
#' @export
setApplications <- function(simulation, scenarioConfiguration) {
  # Set from excel
  excelFilePath <- file.path(
    scenarioConfiguration$projectConfiguration$paramsFolder,
    scenarioConfiguration$projectConfiguration$scenarioApplicationsFile
  )
  # Only try to apply parameters if the sheet exists
  if (any(readxl::excel_sheets(excelFilePath) == scenarioConfiguration$applicationProtocol)) {
    params <- readParametersFromXLS(excelFilePath, scenarioConfiguration$applicationProtocol)
    ospsuite::setParameterValuesByPath(
      parameterPaths = params$paths, values = params$values,
      simulation = simulation, units = params$units
    )
  }
}

#' Validate `ScenarioConfiguration` objects
#'
#' @param scenarioConfigurations Scenario configurations to validate.
#' @keywords internal
.validateScenarioConfigurations <- function(scenarioConfigurations) {
  validateIsOfType(scenarioConfigurations, "ScenarioConfiguration")

  # Check if population is defined for each population scenario
  for (scenarioConfiguration in scenarioConfigurations) {
    if (scenarioConfiguration$simulationType == "Population" && is.null(scenarioConfiguration$populationId)) {
      stop(messages$noPopulationIdForPopulationScenario(scenarioConfiguration$scenarioName))
    }
  }
}
