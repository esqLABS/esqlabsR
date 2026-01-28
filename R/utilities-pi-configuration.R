#' Read Parameter Identification configurations from Excel
#'
#' @param piTaskNames Character vector. Names of the parameter identification
#'   tasks that are defined in the Excel file. If `NULL` (default), all tasks
#'   specified in the Excel file will be read.
#' @param projectConfiguration A `ProjectConfiguration` object holding base
#'   project information.
#'
#' @details Reads PI task configuration from the Excel file defined in
#'   `ProjectConfiguration` and creates `PITaskConfiguration` objects. If a PI
#'   task that is specified in `piTaskNames` is not found in the Excel file, an
#'   error is thrown.
#'
#'   The function expects the Excel file to have a "PIConfiguration" sheet with
#'   the following columns: `PITaskName`, `Algorithm`, `CIMethod`,
#'   `PrintEvaluationFeedback`, `AutoEstimateCI`, `SimulationRunOptions`,
#'   `ObjectiveFunctionOptions`. It also expects a "PIParameters" sheet with
#'   `PITaskName`, `Scenario`, `Container Path`, `Parameter Name`, `Value`,
#'   `Units`, `MinValue`, `MaxValue`, `StartValue`, `Group` columns, a
#'   "PIOutputMappings" sheet with `PITaskName`, `Scenario`, `OutputPath`,
#'   `ObservedDataSheet`, `Scaling`, `xOffset`, `yOffset`, `Weight` columns, an
#'   "AlgorithmOptions" sheet with `PITaskName`, `OptionName`, `OptionValue`
#'   columns, and a "CIOptions" sheet with `PITaskName`, `OptionName`,
#'   `OptionValue` columns.
#'
#' @returns A named list of `PITaskConfiguration` objects.
#'
#' @export
readPITaskConfigurationFromExcel <- function(
  piTaskNames = NULL,
  projectConfiguration
) {
  validateIsString(piTaskNames, nullAllowed = TRUE)
  validateIsOfType(projectConfiguration, ProjectConfiguration)

  # Get path to PI configuration file
  piFilePath <- projectConfiguration$parameterIdentificationFile

  # Check file exists
  if (!file.exists(piFilePath)) {
    stop(messages$fileNotFound(piFilePath))
  }

  # Define expected sheets
  expectedSheets <- c(
    "PIConfiguration",
    "PIParameters",
    "PIOutputMappings",
    "AlgorithmOptions",
    "CIOptions"
  )

  # Validate required sheets exist
  actualSheets <- readxl::excel_sheets(piFilePath)
  missingSheets <- setdiff(expectedSheets, actualSheets)
  if (length(missingSheets) > 0) {
    stop(
      messages$messagePISheet(
        "missingSheets",
        missingSheets,
        filePath = piFilePath
      )
    )
  }

  # Read all sheets into named list
  allSheets <- list(
    piConfiguration = .readPIConfigurationSheet(piFilePath),
    piParameters = .readPIParametersSheet(piFilePath),
    piOutputMappings = .readPIOutputMappingsSheet(piFilePath),
    algorithmOptions = .readAlgorithmOptionsSheet(piFilePath),
    ciOptions = .readCIOptionsSheet(piFilePath)
  )

  # Get all PI task names from mandatory sheets (PIParameters, PIOutputMappings)
  allTaskNames <- unique(c(
    allSheets$piParameters$PITaskName,
    allSheets$piOutputMappings$PITaskName
  ))

  if (is.null(piTaskNames)) {
    piTaskNames <- allTaskNames
  } else {
    # Validate requested PI task names exist
    missingTasks <- setdiff(piTaskNames, allTaskNames)
    if (length(missingTasks) > 0) {
      stop(messages$errorPINotFound("task", missingTasks, allTaskNames))
    }
  }

  # Read scenario configuration
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = NULL,
    projectConfiguration = projectConfiguration
  )
  availableScenarios <- names(scenarioConfigurations)

  # Create PITaskConfiguration objects for each task
  piTaskConfigurations <- vector("list", length(piTaskNames)) |>
    setNames(piTaskNames)

  for (taskName in piTaskNames) {
    # Filter all sheets for this task
    taskData <- lapply(
      allSheets,
      \(df) dplyr::filter(df, PITaskName == taskName)
    )

    # Validate task configuration data has 1 required row
    .validateSingleRow(taskData$piConfiguration, taskName, "PIConfiguration")
    .validateSingleRow(taskData$piParameters, taskName, "PIParameters")
    .validateSingleRow(taskData$piOutputMappings, taskName, "PIOutputMappings")

    # Validate scenarios exist
    referencedScenarios <- unique(
      c(taskData$piParameters$Scenario, taskData$piOutputMappings$Scenario)
    )
    referencedScenarios <- referencedScenarios[!is.na(referencedScenarios)]
    missingScenarios <- setdiff(referencedScenarios, availableScenarios)
    if (length(missingScenarios) > 0) {
      stop(messages$errorPINotFound(
        "scenario",
        missingScenarios[[1]],
        availableScenarios
      ))
    }

    # Convert sheet data to lists
    piConfiguration <- as.list(taskData$piConfiguration[1, ])
    piParameters <- as.list(taskData$piParameters[1, ])
    piOutputMappings <- as.list(taskData$piOutputMappings[1, ])
    piConfiguration$algorithmOptions <- .longFormatToNamedList(
      taskData$algorithmOptions
    )
    piConfiguration$ciOptions <- .longFormatToNamedList(taskData$ciOptions)

    # Get scenario info
    primaryScenario <- referencedScenarios[[1]]
    scenarioConfig <- scenarioConfigurations[[primaryScenario]]

    # Create PITaskConfiguration object
    piTaskConfig <- PITaskConfiguration$new(projectConfiguration)
    piTaskConfig$piTaskName <- taskName
    piTaskConfig$scenarioName <- primaryScenario
    piTaskConfig$modelFile <- scenarioConfig$modelFile
    piTaskConfig$piConfiguration <- piConfiguration
    piTaskConfig$piParameters <- piParameters
    piTaskConfig$piOutputMappings <- piOutputMappings

    piTaskConfigurations[[taskName]] <- piTaskConfig
  }

  return(piTaskConfigurations)
}

#' Read PIConfiguration sheet
#' @param piFilePath Path to ParameterIdentification.xlsx
#' @returns Data frame
#' @keywords internal
#' @noRd
.readPIConfigurationSheet <- function(piFilePath) {
  expectedColumns <- c(
    "PITaskName",
    "Algorithm",
    "CIMethod",
    "PrintEvaluationFeedback",
    "AutoEstimateCI",
    "SimulationRunOptions",
    "ObjectiveFunctionOptions"
  )

  colTypes <- c(
    "text",    # PITaskName
    "text",    # Algorithm
    "text",    # CIMethod
    "logical", # PrintEvaluationFeedback
    "logical", # AutoEstimateCI
    "text",    # SimulationRunOptions
    "text"     # ObjectiveFunctionOptions
  )

  # Validate header
 header <- readExcel(path = piFilePath, sheet = "PIConfiguration", n_max = 0)

  if (!identical(names(header), expectedColumns)) {
    stop(messages$errorWrongXLSStructure(
      filePath = piFilePath,
      expectedColNames = expectedColumns,
      optionalMessage = "Sheet: PIConfiguration"
    ))
  }

  # Read data
  data <- readExcel(
    path = piFilePath,
    sheet = "PIConfiguration",
    col_types = colTypes
  )

  data <- dplyr::filter(data, !dplyr::if_all(dplyr::everything(), is.na))
  data <- dplyr::filter(data, !is.na(PITaskName))

  return(data)
}

#' Read PIParameters sheet
#' @param piFilePath Path to ParameterIdentification.xlsx
#' @returns Data frame
#' @keywords internal
#' @noRd
.readPIParametersSheet <- function(piFilePath) {
  expectedColumns <- c(
    "PITaskName",
    "Scenario",
    "Container Path",
    "Parameter Name",
    "Value",
    "Units",
    "MinValue",
    "MaxValue",
    "StartValue",
    "Group"
  )

  colTypes <- c(
    "text",    # PITaskName
    "text",    # Scenario
    "text",    # Container Path
    "text",    # Parameter Name
    "numeric", # Value
    "text",    # Units
    "numeric", # MinValue
    "numeric", # MaxValue
    "numeric", # StartValue
    "text"     # Group
  )

  # Validate header
  header <- readExcel(path = piFilePath, sheet = "PIParameters", n_max = 0)

  if (!identical(names(header), expectedColumns)) {
    stop(messages$errorWrongXLSStructure(
      filePath = piFilePath,
      expectedColNames = expectedColumns,
      optionalMessage = "Sheet: PIParameters"
    ))
  }

  # Read data
  data <- readExcel(
    path = piFilePath,
    sheet = "PIParameters",
    col_types = colTypes
  )

  data <- dplyr::filter(data, !dplyr::if_all(dplyr::everything(), is.na))
  data <- dplyr::filter(data, !is.na(PITaskName))

  return(data)
}

#' Read PIOutputMappings sheet
#' @param piFilePath Path to ParameterIdentification.xlsx
#' @returns Data frame
#' @keywords internal
#' @noRd
.readPIOutputMappingsSheet <- function(piFilePath) {
  expectedColumns <- c(
    "PITaskName",
    "Scenario",
    "ObservedDataSheet",
    "DataSet",
    "Scaling",
    "xOffset",
    "yOffset",
    "Weight"
  )

  colTypes <- c(
    "text",    # PITaskName
    "text",    # Scenario
    "text",    # ObservedDataSheet
    "text",    # DataSet
    "text",    # Scaling
    "numeric", # xOffset
    "numeric", # yOffset
    "numeric"  # Weight
  )

  # Validate header
  header <- readExcel(path = piFilePath, sheet = "PIOutputMappings", n_max = 0)

  if (!identical(names(header), expectedColumns)) {
    stop(messages$errorWrongXLSStructure(
      filePath = piFilePath,
      expectedColNames = expectedColumns,
      optionalMessage = "Sheet: PIOutputMappings"
    ))
  }

  # Read data
  data <- readExcel(
    path = piFilePath,
    sheet = "PIOutputMappings",
    col_types = colTypes
  )

  data <- dplyr::filter(data, !dplyr::if_all(dplyr::everything(), is.na))
  data <- dplyr::filter(data, !is.na(PITaskName))

  return(data)
}

#' Read AlgorithmOptions sheet
#' @param piFilePath Path to ParameterIdentification.xlsx
#' @returns Data frame
#' @keywords internal
#' @noRd
.readAlgorithmOptionsSheet <- function(piFilePath) {
  expectedColumns <- c("PITaskName", "OptionName", "OptionValue")
  colTypes <- c("text", "text", "text")

  # Validate header
  header <- readExcel(path = piFilePath, sheet = "AlgorithmOptions", n_max = 0)

  if (!identical(names(header), expectedColumns)) {
    stop(messages$errorWrongXLSStructure(
      filePath = piFilePath,
      expectedColNames = expectedColumns,
      optionalMessage = "Sheet: AlgorithmOptions"
    ))
  }

  # Read data
  data <- readExcel(
    path = piFilePath,
    sheet = "AlgorithmOptions",
    col_types = colTypes
  )

  data <- dplyr::filter(data, !dplyr::if_all(dplyr::everything(), is.na))
  data <- dplyr::filter(data, !is.na(PITaskName), !is.na(OptionName))

  return(data)
}

#' Read CIOptions sheet
#' @param piFilePath Path to ParameterIdentification.xlsx
#' @returns Data frame
#' @keywords internal
#' @noRd
.readCIOptionsSheet <- function(piFilePath) {
  expectedColumns <- c("PITaskName", "OptionName", "OptionValue")
  colTypes <- c("text", "text", "text")

  # Validate header
  header <- readExcel(path = piFilePath, sheet = "CIOptions", n_max = 0)

  if (!identical(names(header), expectedColumns)) {
    stop(messages$errorWrongXLSStructure(
      filePath = piFilePath,
      expectedColNames = expectedColumns,
      optionalMessage = "Sheet: CIOptions"
    ))
  }

  # Read data
  data <- readExcel(
    path = piFilePath,
    sheet = "CIOptions",
    col_types = colTypes
  )

  data <- dplyr::filter(data, !dplyr::if_all(dplyr::everything(), is.na))
  data <- dplyr::filter(data, !is.na(PITaskName), !is.na(OptionName))

  return(data)
}

#' Convert long format options to named list
#' @param df Data frame with OptionName, OptionValue columns
#' @returns Named list
#' @keywords internal
#' @noRd
.longFormatToNamedList <- function(df) {
  if (nrow(df) == 0) {
    return(NULL)
  }

  result <- as.list(df$OptionValue)
  names(result) <- trimws(df$OptionName)

  # Try to convert numeric values
  result <- lapply(result, function(x) {
    numVal <- suppressWarnings(as.numeric(x))
    if (!is.na(numVal)) numVal else x
  })

  return(result)
}

#' Validate data frame has at least one row for a PI task
#' @param df Data frame filtered for a specific task
#' @param taskName Name of the PI task
#' @param sheetName Name of the sheet for error messages
#' @keywords internal
#' @noRd
.validateSingleRow <- function(df, taskName, sheetName) {
  nRows <- nrow(df)
  if (nRows == 0) {
    stop(messages$messagePISheet("taskMissing", taskName, sheetName))
  }
  if (nRows > 1) {
    warning(messages$messagePISheet("duplicate", taskName, sheetName))
  }
}
