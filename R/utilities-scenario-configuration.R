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
#' scenarioConfiguration <-
#'   readScenarioConfigurationFromExcel(scenarioNames = scenarioName, projectConfiguration)[[scenarioName]]
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
  # have accidentally some entry, and then the whole row is read with scenario name = NA
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
  protocolName <- .sanitizeExcelSheetName(
    scenarioConfiguration$applicationProtocol,
    warn = FALSE
  )
  if (
    any(
      readxl::excel_sheets(excelFilePath) == protocolName
    )
  ) {
    params <- readParametersFromXLS(
      excelFilePath,
      protocolName
    )
    ospsuite::setParameterValuesByPath(
      parameterPaths = params$paths,
      values = params$values,
      simulation = simulation,
      units = params$units
    )
  }
}


#' Create scenario configurations from PKML files
#'
#' @description
#' Creates scenario configurations from PKML files by extracting available information
#' such as applications, output paths, and simulation time settings. This function
#' creates scenario configuration objects that can be used with the esqlabsR workflow.
#'
#' @param pkmlFilePaths Character vector of paths to PKML files to create scenarios from.
#'   Can be a single string (recycled for all scenarios) or a vector with the same length
#'   as the number of scenarios being created (determined by the longest vector argument).
#' @param projectConfiguration A `ProjectConfiguration` object holding base information.
#' @param scenarioNames Character vector. Optional custom names for the scenarios. If `NULL` (default),
#'   scenario names will be extracted from the simulation names in the PKML files.
#'   If provided, must have the same length as `pkmlFilePaths`.
#' @param individualId Character vector. Optional individual IDs to use for scenarios. If `NULL` (default),
#'   no individual will be specified. Can be a single string (recycled for all scenarios) or a vector
#'   with the same length as `pkmlFilePaths`.
#' @param populationId Character vector. Optional population IDs to use for scenarios. If `NULL` (default),
#'   no population will be specified. If provided, sets simulation type to "Population". Can be a single
#'   string (recycled for all scenarios) or a vector with the same length as `pkmlFilePaths`.
#' @param applicationProtocols Character vector. Optional application protocol names to use for scenarios.
#'   If `NULL` (default), application protocols will be set to the scenario name.
#'   Can be a single string (recycled for all scenarios) or a vector with the same length as `pkmlFilePaths`.
#' @param paramSheets Character vector. Optional parameter sheet names to apply to scenarios.
#'   If `NULL` (default), no parameter sheets will be applied. Can be a single string (recycled for all
#'   scenarios) or a vector with the same length as `pkmlFilePaths`. If providing multiple sheets per
#'   scenario, separate them with commas in the string.
#' @param outputPaths Character vector or named vector. Optional output paths to use for scenarios. If `NULL` (default),
#'   output paths will be extracted from the PKML files' output selections. Can be a single string
#'   (recycled for all scenarios) or a vector with the same length as `pkmlFilePaths`. If providing
#'   multiple paths per scenario, separate them with commas in the string. Named vectors are supported
#'   where names serve as aliases for the paths, e.g., c("plasma" = "Organism|VenousBlood|Plasma|AKB-9090|Concentration in container").
#' @param simulationTime Character vector. Optional simulation time to use for scenarios as character strings containing one or
#'   multiple time intervals separated by a ';'. Each time interval is a triplet of values <StartTime, EndTime, Resolution>,
#'   where `Resolution` is the number of simulated points per time unit defined in the `simulationTimeUnit`. If `NULL` (default),
#'   simulation time will be extracted from the PKML files' output schema intervals. Can be a single string (recycled for all
#'   scenarios) or a vector with the same length as `pkmlFilePaths`.
#' @param simulationTimeUnit Character vector. Optional simulation time units. Only used when `simulationTime` is provided.
#'   If `NULL` (default), will be extracted from the PKML file's output schema intervals, or set to "min" (minutes) if not available.
#'   Can be a single string (recycled for all scenarios) or a vector with the same length as `pkmlFilePaths`.
#' @param steadyState Logical vector. Whether to simulate steady-state for each scenario. Default is `FALSE`.
#'   Can be a single logical value (recycled for all scenarios) or a vector with the same length as `pkmlFilePaths`.
#' @param steadyStateTime Numeric vector. Steady-state times. Only used when corresponding `steadyState` is `TRUE`.
#'   If `NULL` (default), no steady-state time will be set. Can be a single numeric value (recycled for all scenarios)
#'   or a vector with the same length as `pkmlFilePaths`.
#' @param steadyStateTimeUnit Character vector. Steady-state time units. Only used when `steadyState = TRUE` and `steadyStateTime` is provided.
#'   If `NULL` (default), "min" will be used. Can be a single string (recycled for all scenarios) or a vector
#'   with the same length as `pkmlFilePaths`.
#' @param readPopulationFromCSV Logical vector. Whether to read population from CSV for each scenario. Default is `FALSE`.
#'   Can be a single logical value (recycled for all scenarios) or a vector with the same length as `pkmlFilePaths`.
#'
#' @details
#' This function extracts the following information from PKML files:
#' * **Applications**: Application protocol names (defaults to scenario name).
#' * **Output paths**: All selected outputs for the simulation from `outputSelections$allOutputs`.
#' * **Simulation time**: Time intervals with start time, end time, and resolution from `outputSchema$intervals`.
#' * **Simulation time unit**: Time unit from the output schema intervals (e.g., "h" for hours).
#'
#' ## Vector Arguments and Recycling
#'
#' All arguments support vectorization to create scenarios with different parameter values:
#' * **Length 1**: The value is recycled (applied to all scenarios).
#' * **Length > 1**: All vector arguments must have the same length, which determines the number of scenarios.
#' * **Mixed lengths**: An error is thrown if vector arguments have inconsistent lengths.
#'
#' The number of scenarios created is determined by the longest vector argument. All shorter vectors
#' (including `pkmlFilePaths`) are recycled to match this length.
#'
#' This allows you to efficiently create multiple scenarios in several ways:
#' * **Same PKML, different settings**: Use a single PKML file with vectors of different parameter values.
#' * **Different PKMLs, same settings**: Use multiple PKML files with single parameter values.
#' * **Different PKMLs, different settings**: Use vectors of both PKML files and parameter values.
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
#'
#' # Example of vector recycling - single value applied to all scenarios
#' scenarios <- createScenarioConfigurationsFromPKML(
#'   pkmlFilePaths = c("sim1.pkml", "sim2.pkml", "sim3.pkml"),
#'   projectConfiguration = projectConfiguration,
#'   individualId = "Individual_001", # Recycled to all scenarios
#'   steadyState = TRUE, # Recycled to all scenarios
#'   steadyStateTime = 1000 # Recycled to all scenarios
#' )
#'
#' # Example of vector arguments - different values per scenario
#' scenarios <- createScenarioConfigurationsFromPKML(
#'   pkmlFilePaths = c("pediatric.pkml", "adult.pkml", "elderly.pkml"),
#'   projectConfiguration = projectConfiguration,
#'   scenarioNames = c("Pediatric", "Adult", "Elderly"),
#'   individualId = c("Child_001", "Adult_001", "Elderly_001"),
#'   applicationProtocols = c("Pediatric_Dose", "Standard_Dose", "Reduced_Dose"),
#'   steadyState = c(FALSE, TRUE, TRUE),
#'   steadyStateTime = c(NA, 2000, 1500)
#' )
#'
#' # Example of PKML recycling - same model, different settings
#' scenarios <- createScenarioConfigurationsFromPKML(
#'   pkmlFilePaths = "base_model.pkml", # Single PKML recycled
#'   projectConfiguration = projectConfiguration,
#'   scenarioNames = c("LowDose", "MediumDose", "HighDose"),
#'   individualId = c("Patient1", "Patient2", "Patient3"),
#'   applicationProtocols = c("Low_Protocol", "Med_Protocol", "High_Protocol"),
#'   steadyState = c(FALSE, TRUE, TRUE) # Different settings per scenario
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
  if (!is.null(scenarioNames)) {
    validateIsCharacter(scenarioNames)
  }
  if (!is.null(individualId)) {
    validateIsCharacter(individualId)
  }
  if (!is.null(populationId)) {
    validateIsCharacter(populationId)
  }
  if (!is.null(applicationProtocols)) {
    validateIsCharacter(applicationProtocols)
  }
  if (!is.null(paramSheets)) {
    validateIsCharacter(paramSheets)
  }
  if (!is.null(outputPaths)) {
    validateIsCharacter(outputPaths)
  }
  if (!is.null(simulationTime)) {
    validateIsCharacter(simulationTime)
  }
  if (!is.null(simulationTimeUnit)) {
    validateIsCharacter(simulationTimeUnit)
  }
  validateIsLogical(steadyState)
  if (!is.null(steadyStateTime)) {
    validateIsNumeric(steadyStateTime)
  }
  if (!is.null(steadyStateTimeUnit)) {
    validateIsCharacter(steadyStateTimeUnit)
  }
  validateIsLogical(readPopulationFromCSV)

  # Get the number of scenarios to create based on vector arguments
  # Note: projectConfiguration is excluded as it should always be a single object
  nScenarios <- .getScenarioCount(
    pkmlFilePaths,
    scenarioNames,
    individualId,
    populationId,
    applicationProtocols,
    paramSheets,
    outputPaths,
    simulationTime,
    simulationTimeUnit,
    steadyState,
    steadyStateTime,
    steadyStateTimeUnit,
    readPopulationFromCSV
  )

  # Recycle or validate all vector arguments (including pkmlFilePaths)
  pkmlFilePaths <- .recycleOrValidateVector(
    pkmlFilePaths,
    "pkmlFilePaths",
    nScenarios
  )
  scenarioNames <- .recycleOrValidateVector(
    scenarioNames,
    "scenarioNames",
    nScenarios
  )
  individualId <- .recycleOrValidateVector(
    individualId,
    "individualId",
    nScenarios
  )
  populationId <- .recycleOrValidateVector(
    populationId,
    "populationId",
    nScenarios
  )
  applicationProtocols <- .recycleOrValidateVector(
    applicationProtocols,
    "applicationProtocols",
    nScenarios
  )
  paramSheets <- .recycleOrValidateVector(
    paramSheets,
    "paramSheets",
    nScenarios
  )
  # Special handling for outputPaths to preserve named vectors
  if (!is.null(outputPaths)) {
    if (is.list(outputPaths)) {
      # outputPaths is a list - validate length
      if (length(outputPaths) == 1) {
        # Recycle single list element to all scenarios
        outputPaths <- rep(outputPaths, nScenarios)
      } else if (length(outputPaths) != nScenarios) {
        cli::cli_abort(c(
          "Invalid argument length:",
          "x" = "outputPaths must have length 1 or same length as pkmlFilePaths",
          "i" = "outputPaths has length {length(outputPaths)}, pkmlFilePaths has length {nScenarios}"
        ))
      }
    } else {
      # outputPaths is a vector
      if (length(outputPaths) == 1) {
        # Single vector - recycle entire vector to each scenario (preserves named vectors)
        outputPaths <- rep(list(outputPaths), nScenarios)
      } else if (length(outputPaths) == nScenarios) {
        # Vector matches scenarios length - use as is (for comma-separated strings)
        outputPaths <- .recycleOrValidateVector(
          outputPaths,
          "outputPaths",
          nScenarios
        )
      } else {
        cli::cli_abort(c(
          "Invalid argument length:",
          "x" = "outputPaths must have length 1 or same length as pkmlFilePaths",
          "i" = "outputPaths has length {length(outputPaths)}, pkmlFilePaths has length {nScenarios}"
        ))
      }
    }
  }
  simulationTime <- .recycleOrValidateVector(
    simulationTime,
    "simulationTime",
    nScenarios
  )
  simulationTimeUnit <- .recycleOrValidateVector(
    simulationTimeUnit,
    "simulationTimeUnit",
    nScenarios
  )
  steadyState <- .recycleOrValidateVector(
    steadyState,
    "steadyState",
    nScenarios
  )
  steadyStateTime <- .recycleOrValidateVector(
    steadyStateTime,
    "steadyStateTime",
    nScenarios
  )
  steadyStateTimeUnit <- .recycleOrValidateVector(
    steadyStateTimeUnit,
    "steadyStateTimeUnit",
    nScenarios
  )
  readPopulationFromCSV <- .recycleOrValidateVector(
    readPopulationFromCSV,
    "readPopulationFromCSV",
    nScenarios
  )

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
      scenarioName <- glue::glue("{scenarioName}_{existingCount + 1}")

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
    scenarioConfiguration$modelFile <- path_rel(
      pkmlPath,
      start = projectConfiguration$modelFolder
    )

    # Set individual ID
    if (!is.null(individualId)) {
      scenarioConfiguration$individualId <- individualId[[i]]
    }

    # Set population ID
    if (!is.null(populationId)) {
      scenarioConfiguration$populationId <- populationId[[i]]
      scenarioConfiguration$simulationType <- "Population"
    }

    # Set read population from CSV
    scenarioConfiguration$readPopulationFromCSV <- readPopulationFromCSV[[i]]

    # Set parameter sheets
    if (!is.null(paramSheets)) {
      # paramSheets can be a character vector for this scenario
      scenarioParamSheets <- paramSheets[[i]]
      if (
        !is.null(scenarioParamSheets) &&
          !is.na(scenarioParamSheets) &&
          nchar(scenarioParamSheets) > 0
      ) {
        # Split by comma if it's a single string with multiple sheets
        if (
          is.character(scenarioParamSheets) && length(scenarioParamSheets) == 1
        ) {
          scenarioParamSheets <- trimws(strsplit(scenarioParamSheets, ",")[[1]])
        }
        scenarioConfiguration$addParamSheets(scenarioParamSheets)
      }
    }

    # Extract and set application protocol
    if (!is.null(applicationProtocols)) {
      protocolName <- .sanitizeExcelSheetName(
        applicationProtocols[[i]],
        warn = TRUE
      )
    } else {
      # Application protocol name is by default the name of the scenario
      # Sanitize to ensure it's a valid Excel sheet name
      protocolName <- .sanitizeExcelSheetName(scenarioName, warn = TRUE)
    }

    scenarioConfiguration$applicationProtocol <- protocolName

    # Extract and set output paths
    if (!is.null(outputPaths)) {
      # Handle different types of outputPaths structure
      if (is.list(outputPaths)) {
        # outputPaths is a list (named vectors case)
        scenarioOutputPaths <- outputPaths[[i]]
      } else {
        # outputPaths is a regular vector (comma-separated strings case)
        scenarioOutputPaths <- outputPaths[i]
      }
      if (
        !is.null(scenarioOutputPaths) &&
          !any(is.na(scenarioOutputPaths)) &&
          all(nchar(scenarioOutputPaths) > 0)
      ) {
        # Split by comma if it's a single string with multiple paths
        if (
          is.character(scenarioOutputPaths) &&
            length(scenarioOutputPaths) == 1 &&
            is.null(names(scenarioOutputPaths))
        ) {
          scenarioOutputPaths <- trimws(strsplit(scenarioOutputPaths, ",")[[1]])
        }
        # Handle named vectors: preserve names if they exist
        scenarioConfiguration$outputPaths <- scenarioOutputPaths
      }
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
      scenarioConfiguration$simulationTime <- simulationTime[[i]]
      if (!is.null(simulationTimeUnit)) {
        scenarioConfiguration$simulationTimeUnit <- simulationTimeUnit[[i]]
      }
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
              scenarioSimulationTimeUnit <- firstInterval$startTime$displayUnit
            } else {
              # Fallback to minutes if no display unit found
              scenarioSimulationTimeUnit <- "min"
            }
          } else {
            scenarioSimulationTimeUnit <- simulationTimeUnit[[i]]
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
            if (startTimeUnit != scenarioSimulationTimeUnit) {
              startTimeDisplay <- ospsuite::toUnit(
                quantityOrDimension = ospsuite::ospDimensions$Time,
                values = startTime,
                targetUnit = scenarioSimulationTimeUnit
              )
            } else {
              startTimeDisplay <- startTime
            }

            if (endTimeUnit != scenarioSimulationTimeUnit) {
              endTimeDisplay <- ospsuite::toUnit(
                quantityOrDimension = ospsuite::ospDimensions$Time,
                values = endTime,
                targetUnit = scenarioSimulationTimeUnit
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
            scenarioConfiguration$simulationTimeUnit <- scenarioSimulationTimeUnit
          }
        }
      }
    }

    # Set steady state configuration
    if (steadyState[[i]]) {
      scenarioConfiguration$simulateSteadyState <- TRUE
      if (!is.null(steadyStateTime)) {
        scenarioSteadyStateTime <- steadyStateTime[[i]]
        if (
          !is.null(scenarioSteadyStateTime) && !is.na(scenarioSteadyStateTime)
        ) {
          # Use default time unit if not provided
          timeUnit <- if (is.null(steadyStateTimeUnit)) {
            "min"
          } else {
            steadyStateTimeUnit[[i]]
          }
          scenarioConfiguration$steadyStateTime <- ospsuite::toBaseUnit(
            quantityOrDimension = ospsuite::ospDimensions$Time,
            values = scenarioSteadyStateTime,
            unit = timeUnit
          )
        }
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
#'   When named vectors are used for `outputPaths` in scenario configurations, the names
#'   will be used as OutputPathId values.
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
      # Sanitize protocol name to ensure it's a valid Excel sheet name
      protocolName <- .sanitizeExcelSheetName(protocolName)
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
              sheet = protocolName,
              append = TRUE
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
        } else {
          cli::cli_abort("PKML {pkmlPath} file cannot be find.")
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

  # Collect all output paths with their names (if any)
  allOutputPaths <- character()
  allOutputPathNames <- character()
  for (scenarioConfig in scenarioConfigurations) {
    if (!is.null(scenarioConfig$outputPaths)) {
      paths <- scenarioConfig$outputPaths
      names <- names(paths)

      for (j in seq_along(paths)) {
        path <- paths[j]
        if (!path %in% allOutputPaths) {
          allOutputPaths <- c(allOutputPaths, path)
          # Use name as identifier if available, otherwise use the path itself as basis for ID
          if (!is.null(names) && !is.na(names[j]) && nchar(names[j]) > 0) {
            allOutputPathNames <- c(allOutputPathNames, names[j])
          } else {
            allOutputPathNames <- c(allOutputPathNames, NA_character_)
          }
        }
      }
    }
  }

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
    outputPathName <- allOutputPathNames[i]

    # Check if this output path already exists in the Excel file
    if (!is.null(existingOutputPaths)) {
      existingIdx <- which(existingOutputPaths$OutputPath == outputPath)
      if (length(existingIdx) > 0) {
        # Use existing ID
        outputPathIds[i] <- existingOutputPaths$OutputPathId[existingIdx[1]]
      } else {
        # Use name as ID if available, otherwise generate new sequential ID
        if (!is.na(outputPathName)) {
          outputPathIds[i] <- outputPathName
        } else {
          outputPathIds[i] <- glue::glue("OpP{i}")
        }
      }
    } else {
      # Use name as ID if available, otherwise generate new sequential ID
      if (!is.na(outputPathName)) {
        outputPathIds[i] <- outputPathName
      } else {
        outputPathIds[i] <- glue::glue("OpP{i}")
      }
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

#' Sanitize a string to be a valid Excel sheet name
#'
#' @param sheetName Character string. The proposed sheet name to sanitize.
#' @param warn Logical. Whether to warn if the sheet name was modified. Default is TRUE.
#'
#' @details Sanitizes a string to comply with Excel sheet naming rules:
#' * Must be 31 characters or less
#' * Cannot contain any of these characters: / \ * [ ] : ?
#' * Cannot be empty
#' * Leading/trailing spaces are trimmed
#' If the name becomes empty after sanitization, "Sheet" is used as default.
#' If the name is too long, it's truncated and a suffix may be added to ensure uniqueness.
#' If warn=TRUE and the name was modified, a warning is issued.
#'
#' @returns A valid Excel sheet name.
#' @keywords internal
.sanitizeExcelSheetName <- function(sheetName, warn = TRUE) {
  if (is.null(sheetName) || is.na(sheetName)) {
    return("Sheet")
  }

  # Store original name for comparison
  originalName <- as.character(sheetName)

  # Convert to character and trim spaces
  sheetName <- trimws(originalName)

  # If empty after trimming, use default
  if (nchar(sheetName) == 0) {
    if (warn && originalName != "Sheet") {
      cli::cli_warn(c(
        "Excel sheet name was empty or invalid:",
        "i" = "Using default name 'Sheet'"
      ))
    }
    return("Sheet")
  }

  # Remove invalid characters: / \ * [ ] : ?
  sanitizedName <- sheetName
  sanitizedName <- gsub("/", "_", sanitizedName) # Replace /
  sanitizedName <- gsub("\\\\", "_", sanitizedName) # Replace \
  sanitizedName <- gsub("\\*", "_", sanitizedName) # Replace *
  sanitizedName <- gsub("\\[", "_", sanitizedName) # Replace [
  sanitizedName <- gsub("\\]", "_", sanitizedName) # Replace ]
  sanitizedName <- gsub(":", "_", sanitizedName) # Replace :
  sanitizedName <- gsub("\\?", "_", sanitizedName) # Replace ?

  # Trim to 31 characters maximum
  if (nchar(sanitizedName) > 31) {
    sanitizedName <- substr(sanitizedName, 1, 31)
  }

  # Final check - if still empty (unlikely), use default
  if (nchar(trimws(sanitizedName)) == 0) {
    if (warn) {
      cli::cli_warn(c(
        "Excel sheet name became empty after sanitization:",
        "x" = "Original name: '{originalName}'",
        "i" = "Using default name 'Sheet'"
      ))
    }
    return("Sheet")
  }

  # Warn if the name was changed
  if (warn && sanitizedName != originalName) {
    cli::cli_warn(c(
      "Excel sheet name was sanitized to comply with naming rules:",
      "x" = "Original name: '{originalName}'",
      "v" = "Sanitized name: '{sanitizedName}'",
      "i" = "Excel sheet names must be 31 characters or less and cannot contain: / \\\\ * [ ] : ?"
    ))
  }

  return(sanitizedName)
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
#' of numeric vectors. Each vector contains three elements: start_time, end_time, resolution.
#' The function validates that all values are numeric, positive, and that start times
#' are less than end times.
#'
#' @returns A list of numeric vectors, each containing three elements representing
#' start_time, end_time, resolution for each time interval. Returns `NULL` if
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

#' Get the number of scenarios to create based on vector arguments
#'
#' @param pkmlFilePaths Character vector of PKML file paths.
#' @param ... Other vector arguments to check for length consistency.
#'
#' @details Determines the number of scenarios to create based on the length of vector
#' arguments. All vector arguments with length > 1 must have the same length, which
#' determines the final number of scenarios.
#'
#' @returns Integer number of scenarios to create.
#' @keywords internal
.getScenarioCount <- function(pkmlFilePaths, ...) {
  args <- list(...)
  all_args <- c(list(pkmlFilePaths = pkmlFilePaths), args)
  lengths <- sapply(all_args, function(x) if (is.null(x)) 0 else length(x))

  # Filter out NULL arguments (length 0)
  valid_lengths <- lengths[lengths > 0]
  # Get lengths > 1 (vectors that determine scenario count)
  vector_lengths <- valid_lengths[valid_lengths > 1]

  if (length(vector_lengths) == 0) {
    # Case 1 & 2: No vectors with length > 1
    # If pkmlFilePaths exists and has length 1, and all other args are length 1 or NULL
    # Create exactly 1 scenario
    pkml_length <- length(pkmlFilePaths)
    if (pkml_length == 1) {
      return(1) # Cases 1 & 2: Single scenario
    } else {
      # This shouldn't happen since pkmlFilePaths length > 1 would be in vector_lengths
      return(pkml_length)
    }
  } else {
    # Cases 3, 4, 5: At least one vector with length > 1
    if (length(unique(vector_lengths)) == 1) {
      # All vectors with length > 1 have the same length
      return(vector_lengths[1])
    } else {
      # Inconsistent vector lengths
      cli::cli_abort(c(
        "Inconsistent vector argument lengths:",
        "x" = "All vector arguments with length > 1 must have the same length",
        "i" = "Found lengths: {paste(unique(vector_lengths), collapse = ', ')}"
      ))
    }
  }
}

#' Recycle or validate vector arguments for scenario creation
#'
#' @param arg Vector argument to recycle or validate.
#' @param argName Character string name of the argument for error messages.
#' @param nScenarios Integer number of scenarios to create.
#'
#' @details Handles vector recycling for scenario parameters. Single values are
#' recycled to all scenarios, vectors with the correct length are used as-is,
#' and invalid lengths throw an error.
#'
#' @returns Vector with the correct length for all scenarios, or NULL if input was NULL.
#' @keywords internal
.recycleOrValidateVector <- function(arg, argName, nScenarios) {
  if (is.null(arg)) {
    return(NULL)
  }

  if (length(arg) == 1) {
    # Recycle single value to all scenarios
    return(rep(arg, nScenarios))
  } else if (length(arg) == nScenarios) {
    # Vector has same length as number of scenarios
    return(arg)
  } else {
    # Invalid length
    cli::cli_abort(c(
      "Invalid argument length:",
      "x" = "{argName} must have length 1 or same length as pkmlFilePaths",
      "i" = "{argName} has length {length(arg)}, pkmlFilePaths has length {nScenarios}"
    ))
  }
}
