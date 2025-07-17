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
#' @returns A named list of `ScenarioConfiguration` objects withe the names of the
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
    path = projectConfiguration$scenariosFile,
    sheet = "Scenarios",
    n_max = 0
  )

  # Check if the structure is correct
  if (!identical(names(header), expectedColumns)) {
    stop(messages$errorWrongXLSStructure(
      filePath = projectConfiguration$scenariosFile,
      expectedColNames = expectedColumns
    ))
  }

  # If no errors were raised before, structure is correct. Whole excel file is
  # read with column types.

  wholeData <- readExcel(
    path = projectConfiguration$scenariosFile,
    sheet = "Scenarios",
    col_types = colTypes
  )

  # Remove empty rows
  wholeData <- dplyr::filter(wholeData, !dplyr::if_all(dplyr::everything(), is.na))
  # Remove all rows where the name of the scenario is not defined. This might happen when other columns
  # have accidentially some entry, and then the whole row is read with scneario name = NA
  wholeData <- dplyr::filter(wholeData, !is.na(Scenario_name))

  outputPathsDf <- readExcel(
    path = projectConfiguration$scenariosFile,
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
  excelFilePath <- scenarioConfiguration$projectConfiguration$applicationsFile
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

#' Add entries to the scenarios file for given simulation scenarios
#'
#' Existing scenarios will not be overwriten. If the scenario configuration
#' file already contains a scenario with the same name, the resulting file will
#' have a duplicate entry. It is the resposibility of the user to ensure that no duplicate
#' scenarios are present in the excel file.
#'
#' @param scenarioConfigurations A list of `ScenarioConfiguration` objects
#' @param projectConfiguration An object of type `ProjectConfiguration`
#'
#' @export
addScenariosToExcel <- function(scenarioConfigurations, projectConfiguration) {
  validateIsOfType(scenarioConfigurations, ScenarioConfiguration)
  validateIsOfType(projectConfiguration, ProjectConfiguration)

  # Load existing scenarios from Excel to combine them with the new scenarios
  # and write them back to the file
  scenariosFromExcel <- readScenarioConfigurationFromExcel(projectConfiguration = projectConfiguration)
  # Combine them with the passed list of scenario configurations
  scenarioConfigurations <- modifyList(scenariosFromExcel, scenarioConfigurations)

  scenariosDf <- data.frame(list(
    "Scenario_name" = character(),
    "IndividualId" = character(),
    "PopulationId" = character(),
    "ReadPopulationFromCSV" = logical(),
    "ModelParameterSheets" = character(),
    "ApplicationProtocol" = character(),
    "SimulationTime" = character(),
    "SimulationTimeUnit" = numeric(),
    "SteadyState" = logical(),
    "SteadyStateTime" = numeric(),
    "SteadyStateTimeUnit" = character(),
    "ModelFile" = character(),
    "OutputPathsIds" = character()
  ))

  # Get already defined output paths in the scenarios file
  outputPathsDf <- readExcel(
    path = projectConfiguration$scenariosFile,
    sheet = "OutputPaths"
  )
  # Named list of output selections,
  # where names are the output paths, and the values are the aliases
  outputPathsAliases <- as.list(outputPathsDf$OutputPathId)
  names(outputPathsAliases) <- outputPathsDf$OutputPath

  for (scenarioConfiguration in scenarioConfigurations) {
    # Create a string for output intervals
    simulationTimeString <- c()
    for (i in seq_along(scenarioConfiguration$simulationTime)) {
      simulationTimeString <- c(simulationTimeString, paste(scenarioConfiguration$simulationTime[[i]], collapse = ","))
    }
    simulationTimeString <- paste(simulationTimeString, collapse = ";")

    # Multiple output paths can be defined for each scenario.
    # In the "Scenarios" sheet, output aliases are defined, that are mapped to
    # output paths in the sheet "OutputPaths".
    # The following loop iterates through all output paths of the scenario and
    # constructs a list of output aliases.
    outputPaths <- scenarioConfiguration$outputPaths
    outputAliases <- lapply(outputPaths, function(x) {
      # Check if this path has already been added to the "OutputPaths"
      alias <- outputPathsAliases[[x]]
      if (is.null(alias)) {
        alias <- paste0("Output_", length(outputPathsAliases) + 1)
        outputPathsAliases[[x]] <<- alias
      }
      return(alias)
    })

    scenarioList <- list(
      "Scenario_name" = scenarioConfiguration$scenarioName,
      "IndividualId" = scenarioConfiguration$individualId,
      "PopulationId" = scenarioConfiguration$populationId,
      "ReadPopulationFromCSV" = scenarioConfiguration$readPopulationFromCSV,
      "ModelParameterSheets" = paste(names(scenarioConfiguration$paramSheets), collapse = ","),
      "ApplicationProtocol" = scenarioConfiguration$applicationProtocol,
      "SimulationTime" = simulationTimeString,
      "SimulationTimeUnit" = scenarioConfiguration$simulationTimeUnit,
      "SteadyState" = scenarioConfiguration$simulateSteadyState,
      "SteadyStateTime" = scenarioConfiguration$steadyStateTime,
      "SteadyStateTimeUnit" = ospUnits$Time$min,
      "ModelFile" = scenarioConfiguration$modelFile,
      "OutputPathsIds" = paste(outputAliases, collapse = ", ")
    )
    # Replace all NULL by NA in the list
    scenarioList <- lapply(scenarioList, function(x) if (is.null(x)) NA else x)
    # Create a scenario row for this simulation
    scenariosDf <- rbind(scenariosDf, scenarioList)
  }
  writexl::write_xlsx(
    x = list(
      "Scenarios" = scenariosDf,
      "OutputPaths" = data.frame(list(
        "OutputPathId" = unlist(outputPathsAliases, use.names = FALSE),
        "OutputPath" = names(outputPathsAliases)
      ))
    ),
    path = projectConfiguration$scenariosFile
  )
}
