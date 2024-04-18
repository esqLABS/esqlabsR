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
#' projectConfiguration <- createProjectConfiguration()
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
    "text", "text", "text", "logical",
    "numeric", "text", "text", "text"
  )

  # Read only the header of the excel file to check structure
  header <- readExcel(
    path = projectConfiguration$scenarioDefinitionFile,
    sheet = "Scenarios",
    n_max = 0
  )

  # Check if the structure is correct
  if (!identical(names(header), expectedColumns)) {
    stop(messages$errorWrongXLSStructure(
      filePath = projectConfiguration$scenarioDefinitionFile,
      expectedColNames = expectedColumns
    ))
  }

  # If no errors were raised before, structure is correct. Whole excel file is
  # read with column types.

  wholeData <- readExcel(
    path = projectConfiguration$scenarioDefinitionFile,
    sheet = "Scenarios",
    col_types = colTypes
  )

  # Remove empty rows
  wholeData <- dplyr::filter(wholeData, !dplyr::if_all(dplyr::everything(), is.na))

  outputPathsDf <- readExcel(
    path = projectConfiguration$scenarioDefinitionFile,
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
    # Set the time only if new value is defined
    if (!is.na(data$SimulationTime)) {
      scenarioConfiguration$simulationTime <- data$SimulationTime

      if (is.na(data$SimulationTimeUnit)) {
        stop(messages$stopScenarioMissingTimeUnit(scenarioName))
      }

      scenarioConfiguration$simulationTimeUnit <- data$SimulationTimeUnit
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
        stop(messages$invalidOutputPathIds(outputPathIds = missingIds, scenarioName = scenarioName))
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
#' This function is deprecated. Use `setParametersFromXLS` instead.
#'
#' @export
setApplications <- function(simulation, scenarioConfiguration) {
  .Deprecated("setApplications", "setParametersFromXLS")
  # Set from excel
  excelFilePath <- scenarioConfiguration$projectConfiguration$scenarioApplicationsFile
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

# Parse simulation time intervals
.parseSimulationTimeIntervals <- function(simulationTimeIntervalsString) {
  # Check if the simulation time intervals are defined
  if (is.null(simulationTimeIntervalsString)) {
    return(NULL)
  }

  # Split the string by ';'
  simulationTimeIntervals <- strsplit(x = simulationTimeIntervalsString, split = ";", fixed = TRUE)[[1]]
  # Split each interval by ','
  simulationTimeIntervals <- strsplit(x = simulationTimeIntervals, split = ",", fixed = TRUE)
  # Convert to numeric
  simulationTimeIntervals <- lapply(simulationTimeIntervals, as.numeric)
  # Validate that all are numeric
  validateIsNumeric(simulationTimeIntervals)
  # Validate that all are positive
  if (any(unlist(simulationTimeIntervals) < 0)) {
    stop(messages$stopWrongTimeIntervalString(simulationTimeIntervalsString))
  }
  # Validate all intervals are of length 3
  if (any(sapply(simulationTimeIntervals, length) != 3)) {
    stop(messages$stopWrongTimeIntervalString(simulationTimeIntervalsString))
  }
  # Validate all resolution entries are greater than 0
  if (any(sapply(simulationTimeIntervals, function(x) x[3] <= 0))) {
    stop(messages$stopWrongTimeIntervalString(simulationTimeIntervalsString))
  }
  # Validate all start values are smaller than end values
  if (any(sapply(simulationTimeIntervals, function(x) x[1] >= x[2]))) {
    stop(messages$stopWrongTimeIntervalString(simulationTimeIntervalsString))
  }

  return(simulationTimeIntervals)
}
