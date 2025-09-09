#' Read scenario definition(s) from Excel file
#'
#' @param scenarioNames Character vector. Names of the scenarios that are defined in the Excel file.
#'   If `NULL` (default), all scenarios specified in the Excel file will be
#'   created.
#' @param projectConfiguration A `ProjectConfiguration` object holding base information.
#'
#' @details Reads scenario definition from the Excel file defined in
#' `ProjectConfiguration` and creates `ScenarioConfiguration` objects with new
#' information.
#' If a scenario that is specified in `scenarioNames` is not found in the Excel
#' file, an error is thrown.
#'
#' The function expects the Excel file to have a "Scenarios" sheet with the following
#' columns: `Scenario_name`, `IndividualId`, `PopulationId`, `ReadPopulationFromCSV`,
#' `ModelParameterSheets`, `ApplicationProtocol`, `SimulationTime`, `SimulationTimeUnit`,
#' `SteadyState`, `SteadyStateTime`, `SteadyStateTimeUnit`, `ModelFile`, `OutputPathsIds`.
#' It also expects an "OutputPaths" sheet with `OutputPathId` and `OutputPath` columns.
#'
#' @returns A named list of `ScenarioConfiguration` objects with the names of the
#' list being scenario names.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create default ProjectConfiguration
#' projectConfiguration <- createProjectConfiguration()
#' scenarioName <- "MyScenario"
#' # Read scenario definition from Excel
#' scenarioConfiguration <- readScenarioConfigurationFromExcel(scenarioNames = scenarioName, projectConfiguration)[[scenarioName]]
#' }
readScenarioConfigurationFromExcel <- function(
  scenarioNames = NULL,
  projectConfiguration
) {
  validateIsString(scenarioNames, nullAllowed = TRUE)
  validateIsOfType(projectConfiguration, ProjectConfiguration)

  # Current scenario definition structure:
  expectedColumns <- c(
    "Scenario_name",
    "IndividualId",
    "PopulationId",
    "ReadPopulationFromCSV",
    "ModelParameterSheets",
    "ApplicationProtocol",
    "SimulationTime",
    "SimulationTimeUnit",
    "SteadyState",
    "SteadyStateTime",
    "SteadyStateTimeUnit",
    "ModelFile",
    "OutputPathsIds"
  )
  # Define the casting functions to cast columns to specific type
  colTypes <- c(
    "text",
    "text",
    "text",
    "logical",
    "text",
    "text",
    "text",
    "text",
    "logical",
    "numeric",
    "text",
    "text",
    "text"
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
  wholeData <- dplyr::filter(
    wholeData,
    !dplyr::if_all(dplyr::everything(), is.na)
  )
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
      # The values can be enclosed in "" in case sheet names contain a ','.
      # Split the input string by ',' but do not split within ""
      paramSheets <- trimws(scan(
        text = as.character(paramSheets),
        what = "character",
        sep = ",",
        quiet = TRUE
      ))
      scenarioConfiguration$addParamSheets(paramSheets)
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
      # Check for steady-state time unit
      if (is.na(ssTimeUnit)) {
        stop(messages$missingSteadyStateTimeUnit(scenarioName))
      }

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
      pathIds <- strsplit(x = data$OutputPathsIds, split = ",", fixed = TRUE)[[
        1
      ]]
      # Remove leading/trailing whitespaces
      pathIds <- trimws(pathIds)
      # Check if all paths IDs are defined in the OutputPaths sheet
      missingIds <- setdiff(pathIds, outputPathsDf$OutputPathId)
      if (length(missingIds) != 0) {
        stop(messages$invalidOutputPathIds(
          outputPathIds = missingIds,
          scenarioName = scenarioName
        ))
      }
      # Get the paths corresponding to the ids
      outputPaths <- dplyr::filter(
        outputPathsDf,
        OutputPathId %in% pathIds
      )$OutputPath

      scenarioConfiguration$outputPaths <- outputPaths
    }

    # Add the new ScenarioConfiguration to the output list
    scenarioConfigurations[[i]] <- scenarioConfiguration
  }
  names(scenarioConfigurations) <- scenarioNames

  return(scenarioConfigurations)
}

#' Set an application protocol in a `Simulation` from the Excel file
#'
#' @param simulation A `Simulation` object that will be modified.
#' @param scenarioConfiguration A `ScenarioConfiguration` object holding the
#'   name of the application protocol.
#'
#' @details Sets the parameter values describing the application protocol
#' defined in the scenario configuration by reading from the Applications.xlsx file.
#' The function looks for a sheet named after the application protocol and applies
#' all parameter values found in that sheet to the simulation.
#'
#' @section Deprecation:
#' This function is deprecated. Use `setParametersFromXLS` instead for better
#' parameter handling and more flexibility.
#'
#' @export
setApplications <- function(simulation, scenarioConfiguration) {
  .Deprecated("setApplications", "setParametersFromXLS")
  # Set from excel
  excelFilePath <- scenarioConfiguration$projectConfiguration$applicationsFile
  # Only try to apply parameters if the sheet exists
  if (
    any(
      readxl::excel_sheets(excelFilePath) ==
        scenarioConfiguration$applicationProtocol
    )
  ) {
    params <- readParametersFromXLS(
      excelFilePath,
      scenarioConfiguration$applicationProtocol
    )
    ospsuite::setParameterValuesByPath(
      parameterPaths = params$paths,
      values = params$values,
      simulation = simulation,
      units = params$units
    )
  }
}

#' Validate `ScenarioConfiguration` objects
#'
#' @param scenarioConfigurations List of `ScenarioConfiguration` objects to validate.
#'
#' @details Validates that all scenario configurations are of the correct type
#' and that population scenarios have a defined population ID. Throws an error
#' if validation fails.
#'
#' @keywords internal
.validateScenarioConfigurations <- function(scenarioConfigurations) {
  validateIsOfType(scenarioConfigurations, "ScenarioConfiguration")

  # Check if population is defined for each population scenario
  for (scenarioConfiguration in scenarioConfigurations) {
    if (
      scenarioConfiguration$simulationType == "Population" &&
        is.null(scenarioConfiguration$populationId)
    ) {
      stop(messages$noPopulationIdForPopulationScenario(
        scenarioConfiguration$scenarioName
      ))
    }
  }
}

#' Parse simulation time intervals from string format
#'
#' @param simulationTimeIntervalsString Character string. A string containing simulation time intervals
#'   in the format "start1,end1,resolution1;start2,end2,resolution2;...".
#'   Each interval consists of start time, end time, and resolution separated by commas,
#'   and multiple intervals are separated by semicolons.
#'
#' @details Parses a string representation of simulation time intervals into a list
#' of numeric vectors. Each vector contains three elements: [start_time, end_time, resolution].
#' The function validates that all values are numeric, positive, and that start times
#' are less than end times.
#'
#' @returns A list of numeric vectors, each containing three elements representing
#' [start_time, end_time, resolution] for each time interval. Returns `NULL` if
#' the input string is `NULL`.
#'
#' @keywords internal
.parseSimulationTimeIntervals <- function(simulationTimeIntervalsString) {
  # Check if the simulation time intervals are defined
  if (is.null(simulationTimeIntervalsString)) {
    return(NULL)
  }

  # Split the string by ';'
  simulationTimeIntervals <- strsplit(
    x = simulationTimeIntervalsString,
    split = ";",
    fixed = TRUE
  )[[1]]
  # Split each interval by ','
  simulationTimeIntervals <- strsplit(
    x = simulationTimeIntervals,
    split = ",",
    fixed = TRUE
  )
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

#' Create scenario configurations from PKML files
#'
#' @description
#' Creates scenario configurations from PKML files by extracting available information
#' such as applications, output paths, and simulation time settings. This function
#' creates scenario configuration objects that can be used with the esqlabsR workflow.
#'
#' @param pkmlFilePaths Character vector of paths to PKML files to create scenarios from.
#' @param projectConfiguration A `ProjectConfiguration` object holding base information.
#' @param scenarioNames Character vector. Optional custom names for the scenarios. If `NULL` (default),
#'   scenario names will be extracted from the simulation names in the PKML files.
#'   If provided, must have the same length as `pkmlFilePaths`.
#' @param individualId Character string. Optional individual ID to use for all scenarios. If `NULL` (default),
#'   no individual will be specified.
#' @param populationId Character string. Optional population ID to use for all scenarios. If `NULL` (default),
#'   no population will be specified. If provided, sets simulation type to "Population".
#' @param applicationProtocols Character vector. Optional application protocol names to use for scenarios.
#'   If `NULL` (default), application protocols will be set to the scenario name.
#'   If provided, can be a single string (applied to all scenarios) or a vector
#'   with the same length as `pkmlFilePaths`.
#' @param paramSheets Character vector. Optional parameter sheet names to apply to scenarios.
#'   If `NULL` (default), no parameter sheets will be applied.
#' @param outputPaths Character vector. Optional output paths to use for scenarios. If `NULL` (default),
#'   output paths will be extracted from the PKML files' output selections.
#' @param simulationTime Character string. Optional simulation time to use for scenarios as character string containing one or
#'   multiple time intervals separated by a ';'. Each time interval is a triplet of values <StartTime, EndTime, Resolution>,
#'   where `Resolution` is the number of simulated points per time unit defined in the `simulationTimeUnit`. If `NULL` (default),
#'   simulation time will be extracted from the PKML files' output schema intervals.
#' @param simulationTimeUnit Character string. Optional simulation time unit. Only used when `simulationTime` is provided.
#'   If `NULL` (default), will be extracted from the PKML file's output schema intervals, or set to "min" (minutes) if not available.
#' @param steadyState Logical. Whether to simulate steady-state. Default is `FALSE`.
#' @param steadyStateTime Numeric. Steady-state time. Only used when `steadyState = TRUE`.
#'   If `NULL` (default), no steady-state time will be set.
#' @param steadyStateTimeUnit Character string. Steady-state time unit. Only used when `steadyState = TRUE` and `steadyStateTime` is provided.
#'   If `NULL` (default), "min" will be used.
#' @param readPopulationFromCSV Logical. Whether to read population from CSV. Default is `FALSE`.
#'
#' @details
#' This function extracts the following information from PKML files:
#' * **Applications**: Application protocol names (defaults to scenario name).
#' * **Output paths**: All selected outputs for the simulation from `outputSelections$allOutputs`.
#' * **Simulation time**: Time intervals with start time, end time, and resolution from `outputSchema$intervals`.
#' * **Simulation time unit**: Time unit from the output schema intervals (e.g., "h" for hours).
#'
#' The function handles duplicate scenario names by appending indices (e.g., "Scenario_1", "Scenario_2").
#' It creates scenario configurations but does not write them to Excel files.
#' Use `addScenarioConfigurationsToExcel()` to add the scenarios to the project's Excel files.
#'
#' @returns A named list of `ScenarioConfiguration` objects with the names being
#'   the scenario names.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create default project configuration
#' projectConfiguration <- createDefaultProjectConfiguration()
#'
#' # Create scenarios from a single PKML file
#' pkmlPath <- "path/to/simulation.pkml"
#' scenarios <- createScenarioConfigurationsFromPKML(
#'   pkmlFilePaths = pkmlPath,
#'   projectConfiguration = projectConfiguration
#' )
#'
#' # Add scenarios to Excel configuration
#' addScenarioConfigurationsToExcel(
#'   scenarioConfigurations = scenarios,
#'   projectConfiguration = projectConfiguration
#' )
#'
#' # Create scenarios from multiple PKML files with custom names
#' pkmlPaths <- c("path/to/sim1.pkml", "path/to/sim2.pkml")
#' scenarios <- createScenarioConfigurationsFromPKML(
#'   pkmlFilePaths = pkmlPaths,
#'   projectConfiguration = projectConfiguration,
#'   scenarioNames = c("Scenario1", "Scenario2")
#' )
#'
#' # Add multiple scenarios to configuration
#' addScenarioConfigurationsToExcel(
#'   scenarioConfigurations = scenarios,
#'   projectConfiguration = projectConfiguration
#' )
#' }
createScenarioConfigurationsFromPKML <- function(
  pkmlFilePaths,
  projectConfiguration,
  scenarioNames = NULL,
  individualId = NULL,
  populationId = NULL,
  applicationProtocols = NULL,
  paramSheets = NULL,
  outputPaths = NULL,
  simulationTime = NULL,
  simulationTimeUnit = NULL,
  steadyState = FALSE,
  steadyStateTime = NULL,
  steadyStateTimeUnit = NULL,
  readPopulationFromCSV = FALSE
) {
  # Validate inputs
  validateIsCharacter(pkmlFilePaths)
  validateIsOfType(projectConfiguration, ProjectConfiguration)
  validateIsString(scenarioNames, nullAllowed = TRUE)
  validateIsString(individualId, nullAllowed = TRUE)
  validateIsString(populationId, nullAllowed = TRUE)
  validateIsCharacter(applicationProtocols, nullAllowed = TRUE)
  validateIsCharacter(paramSheets, nullAllowed = TRUE)
  validateIsCharacter(outputPaths, nullAllowed = TRUE)
  validateIsString(simulationTime, nullAllowed = TRUE)
  validateIsString(simulationTimeUnit, nullAllowed = TRUE)
  validateIsLogical(steadyState)
  validateIsNumeric(steadyStateTime, nullAllowed = TRUE)
  validateIsString(steadyStateTimeUnit, nullAllowed = TRUE)
  validateIsLogical(readPopulationFromCSV)
  # Validate scenario names length
  if (
    !is.null(scenarioNames) && length(scenarioNames) != length(pkmlFilePaths)
  ) {
    cli::cli_abort(c(
      "Invalid argument lengths:",
      "x" = "scenarioNames must have the same length as pkmlFilePaths",
      "i" = "scenarioNames has length {length(scenarioNames)}, pkmlFilePaths has length {length(pkmlFilePaths)}"
    ))
  }

  # Initialize variables
  scenarioConfigurations <- list()
  allScenarioNames <- c()

  for (i in seq_along(pkmlFilePaths)) {
    pkmlPath <- pkmlFilePaths[[i]]
    # Check if PKML files exist
    if (!file.exists(pkmlPath)) {
      cli::cli_abort(c(
        "File not found:",
        "x" = "Cannot find PKML file: {.path {pkmlPath}}"
      ))
    }

    # Load simulation from PKML file
    simulation <- ospsuite::loadSimulation(
      filePath = pkmlPath,
      loadFromCache = FALSE
    )

    # Extract scenario name from simulation if not provided by user
    if (is.null(scenarioNames)) {
      originalScenarioName <- scenarioName <- simulation$name
    } else {
      originalScenarioName <- scenarioName <- scenarioNames[[i]]
    }

    # Look for duplicated scenario names
    if (scenarioName %in% allScenarioNames) {
      # Duplicated scenarioName found
      # count number of existing scenarios with this name
      existingCount <- sum(allScenarioNames == originalScenarioName)
      # Make scenario name unique by appending index
      scenarioName <- paste0(scenarioName, "_", existingCount + 1)

      # Warn the user
      cli::cli_warn(c(
        "Duplicate scenario names found and made unique by adding indices:",
        "i" = "Duplicated names: {.val {originalScenarioName}}, renamed to {.val {scenarioName}}"
      ))
    }

    allScenarioNames <- c(allScenarioNames, originalScenarioName)

    simTree <- ospsuite::getSimulationTree(simulation)

    # Create scenario configuration
    scenarioConfiguration <- ScenarioConfiguration$new(projectConfiguration)
    scenarioConfiguration$scenarioName <- scenarioName
    scenarioConfiguration$modelFile <- basename(pkmlPath)

    # Set individual ID
    if (!is.null(individualId)) {
      scenarioConfiguration$individualId <- individualId
    }

    # Set population ID
    if (!is.null(populationId)) {
      scenarioConfiguration$populationId <- populationId
      scenarioConfiguration$simulationType <- "Population"
    }

    # Set read population from CSV
    scenarioConfiguration$readPopulationFromCSV <- readPopulationFromCSV

    # Set parameter sheets
    if (!is.null(paramSheets)) {
      scenarioConfiguration$addParamSheets(paramSheets)
    }

    # Extract and set application protocol
    if (!is.null(applicationProtocols)) {
      if (length(applicationProtocols) == 1) {
        protocolName <- applicationProtocols
      } else if (length(applicationProtocols) == length(pkmlFilePaths)) {
        protocolName <- applicationProtocols[[i]]
      } else {
        cli::cli_abort(c(
          "Invalid applicationProtocols length:",
          "x" = "applicationProtocols must have length 1 or same length as pkmlFilePaths",
          "i" = "applicationProtocols has length {length(applicationProtocols)}, pkmlFilePaths has length {length(pkmlFilePaths)}"
        ))
      }
    } else {
      # Application protocol name is by default the name of the scenario
      protocolName <- scenarioName
    }

    scenarioConfiguration$applicationProtocol <- protocolName

    # Extract and set output paths
    if (!is.null(outputPaths)) {
      scenarioConfiguration$outputPaths <- outputPaths
    } else {
      # Extract output paths from PKML
      if (!is.null(simulation$outputSelections$allOutputs)) {
        outputPathsFromPKML <- sapply(
          simulation$outputSelections$allOutputs,
          function(x) x$path
        )
        scenarioConfiguration$outputPaths <- outputPathsFromPKML
      }
    }

    # Extract and set simulation time
    if (!is.null(simulationTime)) {
      scenarioConfiguration$simulationTime <- simulationTime
      scenarioConfiguration$simulationTimeUnit <- simulationTimeUnit
    } else {
      # Extract simulation time from PKML
      if (!is.null(simulation$outputSchema$intervals)) {
        intervals <- simulation$outputSchema$intervals
        if (length(intervals) > 0) {
          # Extract time unit from PKML if not provided
          if (is.null(simulationTimeUnit)) {
            # Get the unit from the first interval's start time
            firstInterval <- intervals[[1]]
            if (!is.null(firstInterval$startTime$displayUnit)) {
              simulationTimeUnit <- firstInterval$startTime$displayUnit
            } else {
              # Fallback to minutes if no display unit found
              simulationTimeUnit <- "min"
            }
          }

          # Create time intervals string from all intervals
          timeIntervals <- character()
          for (j in seq_along(intervals)) {
            interval <- intervals[[j]]
            startTime <- interval$startTime$value
            endTime <- interval$endTime$value
            resolution <- interval$resolution$value

            # Get the units from the interval
            startTimeUnit <- interval$startTime$displayUnit
            endTimeUnit <- interval$endTime$displayUnit

            # Convert to target units if different
            if (startTimeUnit != simulationTimeUnit) {
              startTimeDisplay <- ospsuite::toUnit(
                quantityOrDimension = ospsuite::ospDimensions$Time,
                values = startTime,
                targetUnit = simulationTimeUnit
              )
            } else {
              startTimeDisplay <- startTime
            }

            if (endTimeUnit != simulationTimeUnit) {
              endTimeDisplay <- ospsuite::toUnit(
                quantityOrDimension = ospsuite::ospDimensions$Time,
                values = endTime,
                targetUnit = simulationTimeUnit
              )
            } else {
              endTimeDisplay <- endTime
            }

            # Format this interval as "start, end, resolution"
            intervalString <- paste(
              startTimeDisplay,
              endTimeDisplay,
              resolution,
              sep = ", "
            )
            timeIntervals <- c(timeIntervals, intervalString)
          }

          # Join all intervals with semicolons
          if (length(timeIntervals) > 0) {
            scenarioConfiguration$simulationTime <- paste(
              timeIntervals,
              collapse = "; "
            )
            scenarioConfiguration$simulationTimeUnit <- simulationTimeUnit
          }
        }
      }
    }

    # Set steady state configuration
    if (steadyState) {
      scenarioConfiguration$simulateSteadyState <- TRUE
      if (!is.null(steadyStateTime)) {
        # Use default time unit if not provided
        timeUnit <- if (is.null(steadyStateTimeUnit)) {
          "min"
        } else {
          steadyStateTimeUnit
        }
        scenarioConfiguration$steadyStateTime <- ospsuite::toBaseUnit(
          quantityOrDimension = ospsuite::ospDimensions$Time,
          values = steadyStateTime,
          unit = timeUnit
        )
      }
    }

    scenarioConfigurations[[scenarioName]] <- scenarioConfiguration
  }

  return(scenarioConfigurations)
}

#' Add scenario configurations to project Excel files
#'
#' @description
#' Adds scenario configurations to the project's Scenarios.xlsx file and ensures
#' that required application protocol sheets exist in the Applications.xlsx file.
#' This function handles the Excel file operations for adding scenarios to a project.
#'
#' @param scenarioConfigurations Named list of `ScenarioConfiguration` objects to add to the project.
#' @param projectConfiguration A `ProjectConfiguration` object holding base information.
#' @param appendToExisting Logical. Whether to append new scenarios to existing ones in the
#'   scenarios file. If `FALSE`, the ENTIRE scenarios file will be overwritten with
#'   only the new scenarios. If `TRUE` (default), new scenarios will be added to existing ones.
#'
#' @details
#' This function performs the following operations:
#' * Checks for duplicate scenario names if `appendToExisting` is `TRUE`.
#' * Creates missing application protocol sheets in Applications.xlsx by extracting
#'   parameters from PKML files (both Events and Applications parameters).
#' * Writes scenario configurations to the Scenarios.xlsx file with proper structure.
#' * Manages output paths and their IDs in the OutputPaths sheet.
#'
#' The function ensures that the Excel files are properly structured with the following sheets:
#' * **Scenarios sheet**: Contains scenario definitions with columns for scenario name,
#'   individual/population IDs, parameter sheets, application protocol, simulation time,
#'   steady state settings, model file, and output path IDs.
#' * **OutputPaths sheet**: Contains output path IDs and their corresponding paths.
#' * **Applications.xlsx**: Contains application protocol sheets with parameter definitions.
#'
#' The function handles parameter extraction from PKML files, excluding default parameters
#' like "Volume" and "Application rate", and only includes constant parameters.
#'
#' @returns Invisibly returns the names of the added scenarios.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create scenario configurations from PKML
#' scenarios <- createScenarioConfigurationsFromPKML(
#'   pkmlFilePaths = "path/to/simulation.pkml",
#'   projectConfiguration = projectConfiguration
#' )
#'
#' # Add scenarios to project Excel files
#' addScenarioConfigurationsToExcel(
#'   scenarioConfigurations = scenarios,
#'   projectConfiguration = projectConfiguration,
#'   appendToExisting = TRUE
#' )
#' }
addScenarioConfigurationsToExcel <- function(
  scenarioConfigurations,
  projectConfiguration,
  appendToExisting = TRUE
) {
  # Validate inputs
  validateIsOfType(projectConfiguration, ProjectConfiguration)
  validateIsLogical(appendToExisting)

  if (
    !is.list(scenarioConfigurations) || is.null(names(scenarioConfigurations))
  ) {
    cli::cli_abort(c(
      "Invalid scenarioConfigurations:",
      "x" = "scenarioConfigurations must be a named list",
      "i" = "Each scenario configuration must have a unique name"
    ))
  }

  # Validate that all entries are ScenarioConfiguration objects
  for (i in seq_along(scenarioConfigurations)) {
    validateIsOfType(scenarioConfigurations[[i]], ScenarioConfiguration)
  }

  # Check for duplicate scenario names if appending
  if (appendToExisting && file.exists(projectConfiguration$scenariosFile)) {
    # Read existing scenarios to check for duplicates
    existingScenarios <- readScenarioConfigurationFromExcel(
      projectConfiguration = projectConfiguration
    )
    existingNames <- names(existingScenarios)
    newNames <- names(scenarioConfigurations)
    duplicateNames <- intersect(existingNames, newNames)

    if (length(duplicateNames) > 0) {
      cli::cli_abort(c(
        "Duplicate scenario names found:",
        "x" = "Cannot add scenarios with duplicate names to existing configuration",
        "i" = "Duplicated names: {.val {duplicateNames}}"
      ))
    }
  }

  # Ensure protocol sheets exist in Applications.xlsx and extract parameters
  for (scenarioConfig in scenarioConfigurations) {
    protocolName <- scenarioConfig$applicationProtocol
    if (!is.null(protocolName) && !is.na(protocolName)) {
      applicationsFile <- projectConfiguration$applicationsFile

      # Check if sheet already exists
      sheetExists <- FALSE
      if (file.exists(applicationsFile)) {
        sheets <- readxl::excel_sheets(applicationsFile)
        sheetExists <- protocolName %in% sheets
      }

      # Only extract and write parameters if sheet doesn't exist
      if (!sheetExists) {
        # Load the PKML file to extract parameters
        pkmlPath <- file.path(
          projectConfiguration$modelFolder,
          scenarioConfig$modelFile
        )

        if (file.exists(pkmlPath)) {
          simulation <- ospsuite::loadSimulation(
            filePath = pkmlPath,
            loadFromCache = FALSE
          )

          # Extract all events parameters. Starting from PK-Sim v12, applications are located
          # under the `Events` node in the simulation tree.
          eventsParams <- ospsuite::getAllParametersMatching(
            "Events|**",
            simulation
          )

          # Extract all applications parameters. Before v12, applications were located
          # under the `Applications` node in the simulation tree.
          applicationsParams <- ospsuite::getAllParametersMatching(
            "Applications|**",
            simulation
          )

          # Some default parameters should be excluded from the list of parameters.
          defaultParamsToExclude <- c(
            "Volume",
            "Application rate"
          )

          # Iterate through all parameters and only keep those that are defined by a constant
          # value.
          constantApplicationParams <- lapply(
            c(applicationsParams, eventsParams),
            function(param) {
              # Check if the parameter is in the excluded list
              if (param$name %in% defaultParamsToExclude) {
                return(NULL)
              }

              if (param$isConstant) {
                return(param)
              } else {
                return(NULL)
              }
            }
          )
          # Remove all `NULL` from the list
          constantApplicationParams <- constantApplicationParams[
            lengths(constantApplicationParams) != 0
          ]

          # Write parameters to Applications excel if there are parameters to write
          if (length(constantApplicationParams) > 0) {
            exportParametersToXLS(
              parameters = constantApplicationParams,
              paramsXLSpath = applicationsFile,
              sheet = protocolName
            )
          } else {
            # If no parameters, create empty sheet with proper header
            if (file.exists(applicationsFile)) {
              sheets <- readxl::excel_sheets(applicationsFile)
              if (length(sheets) > 0) {
                # Read header from first sheet
                firstSheet <- sheets[1]
                header <- readxl::read_excel(
                  applicationsFile,
                  sheet = firstSheet,
                  n_max = 0
                )
                # Add new sheet with header only
                wb <- openxlsx::loadWorkbook(applicationsFile)
                openxlsx::addWorksheet(wb, protocolName)
                openxlsx::writeData(wb, protocolName, header)
                openxlsx::saveWorkbook(wb, applicationsFile, overwrite = TRUE)
              }
            } else {
              # Create new file with empty parameters sheet
              emptyParams <- data.frame(
                "Container Path" = character(0),
                "Parameter Name" = character(0),
                "Value" = numeric(0),
                "Units" = character(0),
                check.names = FALSE
              )
              wb <- openxlsx::createWorkbook()
              openxlsx::addWorksheet(wb, protocolName)
              openxlsx::writeData(wb, protocolName, emptyParams)
              openxlsx::saveWorkbook(wb, applicationsFile)
            }
          }
        }
      }
    }
  }

  # Write scenarios to Excel
  .writeScenariosToExcel(
    scenarioConfigurations = scenarioConfigurations,
    projectConfiguration = projectConfiguration,
    appendToExisting = appendToExisting
  )

  invisible(names(scenarioConfigurations))
}


#' Write scenarios to Excel file
#'
#' @param scenarioConfigurations List of `ScenarioConfiguration` objects to write.
#' @param projectConfiguration A `ProjectConfiguration` object containing file paths.
#' @param appendToExisting Logical. Whether to append to existing scenarios or overwrite.
#'
#' @details Internal function that handles the actual writing of scenario configurations
#' to Excel files. Creates both the Scenarios and OutputPaths sheets with proper
#' data types and structure. Handles merging with existing data when appending.
#'
#' @keywords internal
.writeScenariosToExcel <- function(
  scenarioConfigurations,
  projectConfiguration,
  appendToExisting
) {
  # Create data frame for scenarios
  scenariosData <- data.frame(
    Scenario_name = character(),
    IndividualId = character(),
    PopulationId = character(),
    ReadPopulationFromCSV = logical(),
    ModelParameterSheets = character(),
    ApplicationProtocol = character(),
    SimulationTime = character(),
    SimulationTimeUnit = character(),
    SteadyState = logical(),
    SteadyStateTime = numeric(),
    SteadyStateTimeUnit = character(),
    ModelFile = character(),
    OutputPathsIds = character(),
    stringsAsFactors = FALSE
  )

  # Create data frame for output paths
  outputPathsData <- data.frame(
    OutputPathId = character(),
    OutputPath = character(),
    stringsAsFactors = FALSE
  )

  # Collect all output paths
  allOutputPaths <- character()
  for (scenarioConfig in scenarioConfigurations) {
    if (!is.null(scenarioConfig$outputPaths)) {
      allOutputPaths <- c(allOutputPaths, scenarioConfig$outputPaths)
    }
  }
  allOutputPaths <- unique(allOutputPaths)

  # Check if there are existing output paths in the Excel file
  existingOutputPaths <- NULL
  if (file.exists(projectConfiguration$scenariosFile)) {
    tryCatch(
      {
        existingOutputPaths <- readExcel(
          path = projectConfiguration$scenariosFile,
          sheet = "OutputPaths"
        )
      },
      error = function(e) {
        # File doesn't exist or can't be read, continue with new IDs
      }
    )
  }

  # Create output paths data frame with appropriate IDs
  outputPathIds <- character(length(allOutputPaths))
  for (i in seq_along(allOutputPaths)) {
    outputPath <- allOutputPaths[i]

    # Check if this output path already exists in the Excel file
    if (!is.null(existingOutputPaths)) {
      existingIdx <- which(existingOutputPaths$OutputPath == outputPath)
      if (length(existingIdx) > 0) {
        # Use existing ID
        outputPathIds[i] <- existingOutputPaths$OutputPathId[existingIdx[1]]
      } else {
        # Generate new sequential ID
        outputPathIds[i] <- paste0("OpP", i)
      }
    } else {
      # No existing file, generate new sequential ID
      outputPathIds[i] <- paste0("OpP", i)
    }
  }

  outputPathsData <- data.frame(
    OutputPathId = outputPathIds,
    OutputPath = allOutputPaths,
    stringsAsFactors = FALSE
  )

  # Create scenarios data frame
  for (scenarioConfig in scenarioConfigurations) {
    # Get output path IDs for this scenario
    scenarioOutputPathIds <- character()
    if (!is.null(scenarioConfig$outputPaths)) {
      for (outputPath in scenarioConfig$outputPaths) {
        idx <- which(outputPathsData$OutputPath == outputPath)
        if (length(idx) > 0) {
          scenarioOutputPathIds <- c(
            scenarioOutputPathIds,
            outputPathsData$OutputPathId[idx]
          )
        }
      }
    }

    # Create scenario row
    scenarioRow <- data.frame(
      Scenario_name = scenarioConfig$scenarioName,
      IndividualId = ifelse(
        is.null(scenarioConfig$individualId),
        "",
        scenarioConfig$individualId
      ),
      PopulationId = ifelse(
        is.null(scenarioConfig$populationId),
        "",
        scenarioConfig$populationId
      ),
      ReadPopulationFromCSV = scenarioConfig$readPopulationFromCSV,
      ModelParameterSheets = paste(
        enumKeys(scenarioConfig$paramSheets),
        collapse = ", "
      ),
      ApplicationProtocol = ifelse(
        is.null(scenarioConfig$applicationProtocol),
        "",
        scenarioConfig$applicationProtocol
      ),
      SimulationTime = ifelse(
        is.null(scenarioConfig$simulationTime),
        "",
        .formatSimulationTimeForExcel(scenarioConfig$simulationTime)
      ),
      SimulationTimeUnit = scenarioConfig$simulationTimeUnit,
      SteadyState = scenarioConfig$simulateSteadyState,
      SteadyStateTime = ifelse(
        scenarioConfig$simulateSteadyState,
        ospsuite::toUnit(
          ospsuite::ospDimensions$Time,
          scenarioConfig$steadyStateTime,
          "min"
        ),
        NA
      ),
      SteadyStateTimeUnit = ifelse(
        scenarioConfig$simulateSteadyState,
        "min",
        ""
      ),
      ModelFile = scenarioConfig$modelFile,
      OutputPathsIds = paste(scenarioOutputPathIds, collapse = ", "),
      stringsAsFactors = FALSE
    )

    scenariosData <- rbind(scenariosData, scenarioRow)
  }

  # Read existing data if appending
  if (appendToExisting && file.exists(projectConfiguration$scenariosFile)) {
    existingScenarios <- tryCatch(
      readExcel(
        path = projectConfiguration$scenariosFile,
        sheet = "Scenarios"
      ),
      error = function(e) {
        warning(
          "Could not read existing scenarios file, creating new one: ",
          e$message
        )
        NULL
      }
    )
    existingOutputPaths <- tryCatch(
      readExcel(
        path = projectConfiguration$scenariosFile,
        sheet = "OutputPaths"
      ),
      error = function(e) {
        warning(
          "Could not read existing scenarios file, creating new one: ",
          e$message
        )
        NULL
      }
    )
    if (!is.null(existingScenarios) && !is.null(existingOutputPaths)) {
      # Ensure columns match and order is correct for scenarios
      allScenarioCols <- colnames(scenariosData)
      for (col in setdiff(allScenarioCols, colnames(existingScenarios))) {
        existingScenarios[[col]] <- NA
      }
      for (col in setdiff(colnames(existingScenarios), allScenarioCols)) {
        scenariosData[[col]] <- NA
      }
      # Reorder columns to match expected structure
      existingScenarios <- existingScenarios[, allScenarioCols, drop = FALSE]
      scenariosData <- scenariosData[, allScenarioCols, drop = FALSE]
      # Check for duplicate scenario names and throw error
      if (nrow(existingScenarios) > 0 && nrow(scenariosData) > 0) {
        duplicateNames <- scenariosData$Scenario_name[
          scenariosData$Scenario_name %in% existingScenarios$Scenario_name
        ]
        if (length(duplicateNames) > 0) {
          stop(messages$errorDuplicateScenarioNames(duplicateNames))
        }
      }
      # Combine scenarios
      scenariosData <- rbind(existingScenarios, scenariosData)

      # Ensure columns match and order is correct for output paths
      allOutputCols <- colnames(outputPathsData)
      for (col in setdiff(allOutputCols, colnames(existingOutputPaths))) {
        existingOutputPaths[[col]] <- NA
      }
      for (col in setdiff(colnames(existingOutputPaths), allOutputCols)) {
        outputPathsData[[col]] <- NA
      }
      existingOutputPaths <- existingOutputPaths[,
        allOutputCols,
        drop = FALSE
      ]
      outputPathsData <- outputPathsData[, allOutputCols, drop = FALSE]

      # Combine output paths (preserve existing IDs)
      for (i in seq_len(nrow(outputPathsData))) {
        existingIdx <- which(
          existingOutputPaths$OutputPath == outputPathsData$OutputPath[i]
        )
        if (length(existingIdx) == 0) {
          # This is a new output path, add it with the new ID
          existingOutputPaths <- rbind(
            existingOutputPaths,
            outputPathsData[i, , drop = FALSE]
          )
        }
        # If output path already exists, we don't need to do anything
        # The ID in outputPathsData is already correct from the earlier logic
      }
      outputPathsData <- existingOutputPaths
    }
  }

  # Ensure all required columns exist before coercion
  requiredScenarioCols <- c(
    "Scenario_name",
    "IndividualId",
    "PopulationId",
    "ReadPopulationFromCSV",
    "ModelParameterSheets",
    "ApplicationProtocol",
    "SimulationTime",
    "SimulationTimeUnit",
    "SteadyState",
    "SteadyStateTime",
    "SteadyStateTimeUnit",
    "ModelFile",
    "OutputPathsIds"
  )
  for (col in requiredScenarioCols) {
    if (!col %in% colnames(scenariosData)) {
      # Use appropriate NA type
      if (col %in% c("ReadPopulationFromCSV", "SteadyState")) {
        scenariosData[[col]] <- as.logical(NA)
      } else if (col == "SteadyStateTime") {
        scenariosData[[col]] <- as.numeric(NA)
      } else {
        scenariosData[[col]] <- as.character(NA)
      }
    }
  }
  scenariosData <- scenariosData[, requiredScenarioCols, drop = FALSE]

  requiredOutputCols <- c("OutputPathId", "OutputPath")
  for (col in requiredOutputCols) {
    if (!col %in% colnames(outputPathsData)) {
      outputPathsData[[col]] <- as.character(NA)
    }
  }
  outputPathsData <- outputPathsData[, requiredOutputCols, drop = FALSE]

  # Coerce all columns to correct types before writing
  scenariosData$Scenario_name <- as.character(scenariosData$Scenario_name)
  scenariosData$IndividualId <- as.character(scenariosData$IndividualId)
  scenariosData$PopulationId <- as.character(scenariosData$PopulationId)
  scenariosData$ReadPopulationFromCSV <- as.logical(
    scenariosData$ReadPopulationFromCSV
  )
  scenariosData$ModelParameterSheets <- as.character(
    scenariosData$ModelParameterSheets
  )
  scenariosData$ApplicationProtocol <- as.character(
    scenariosData$ApplicationProtocol
  )
  scenariosData$SimulationTime <- as.character(scenariosData$SimulationTime)
  scenariosData$SimulationTimeUnit <- as.character(
    scenariosData$SimulationTimeUnit
  )
  scenariosData$SteadyState <- as.logical(scenariosData$SteadyState)
  scenariosData$SteadyStateTime <- as.numeric(scenariosData$SteadyStateTime)
  scenariosData$SteadyStateTimeUnit <- as.character(
    scenariosData$SteadyStateTimeUnit
  )
  scenariosData$ModelFile <- as.character(scenariosData$ModelFile)
  scenariosData$OutputPathsIds <- as.character(scenariosData$OutputPathsIds)
  outputPathsData$OutputPathId <- as.character(outputPathsData$OutputPathId)
  outputPathsData$OutputPath <- as.character(outputPathsData$OutputPath)

  # Write to Excel file
  scenariosList <- list(
    Scenarios = scenariosData,
    OutputPaths = outputPathsData
  )
  .writeExcel(data = scenariosList, path = projectConfiguration$scenariosFile)
}

#' Format simulation time for Excel
#'
#' @param simulationTime List of numeric vectors. Simulation time intervals (as parsed by ScenarioConfiguration)
#'   or character string containing time intervals.
#'
#' @details Converts simulation time intervals from a list format to a string format
#' suitable for Excel storage. Each interval is formatted as "start, end, resolution"
#' and multiple intervals are separated by semicolons.
#'
#' @returns Formatted string for Excel storage, or empty string if input is NULL.
#' @keywords internal
.formatSimulationTimeForExcel <- function(simulationTime) {
  if (is.null(simulationTime)) {
    return("")
  }

  # Convert each interval to "start, end, resolution" format
  intervalStrings <- sapply(simulationTime, function(interval) {
    paste(interval[1], interval[2], interval[3], sep = ", ")
  })

  # Combine all intervals with semicolon separator
  paste(intervalStrings, collapse = "; ")
}
