#' Create Parameter Identification tasks
#'
#' @description Creates ParameterIdentification objects from PI configurations.
#'   Each PITaskConfiguration produces one ParameterIdentification object.
#'
#' @param piTaskConfigurations Named list of `PITaskConfiguration` objects
#'
#' @returns Named list of `ParameterIdentification` objects
#'
#' @export
createPITasks <- function(piTaskConfigurations) {
  piTaskConfigurations <- ospsuite.utils::toList(piTaskConfigurations)
  .validatePITaskConfigurations(piTaskConfigurations)

  # Get project configuration
  projectConfiguration <- piTaskConfigurations[[1]]$projectConfiguration

  # Collect unique observed data sheets from all task configurations
  observedDataSheets <- unique(unlist(lapply(
    piTaskConfigurations,
    function(config) {
      config$piOutputMappings$ObservedDataSheet
    }
  )))
  observedDataSheets <- observedDataSheets[!is.na(observedDataSheets)]

  # Load observed data for specific sheets
  observedData <- loadObservedData(
    projectConfiguration,
    sheets = observedDataSheets
  )

  # Load scenario configurations for output path resolution
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = NULL,
    projectConfiguration = projectConfiguration
  )

  # Create PI task for each configuration
  piTasks <- vector("list", length(piTaskConfigurations)) |>
    setNames(names(piTaskConfigurations))

  for (taskName in names(piTaskConfigurations)) {
    piTasks[[taskName]] <- .createSinglePITask(
      piTaskConfig = piTaskConfigurations[[taskName]],
      observedData = observedData,
      scenarioConfigurations = scenarioConfigurations
    )
  }

  return(piTasks)
}

#' Validate PI task configurations
#' @param piTaskConfigurations List to validate
#' @keywords internal
#' @noRd
.validatePITaskConfigurations <- function(piTaskConfigurations) {
  if (!is.list(piTaskConfigurations) || length(piTaskConfigurations) == 0) {
    stop(messages$errorPITask("configurationsNotList"))
  }

  for (piConfig in piTaskConfigurations) {
    if (!isOfType(piConfig, "PITaskConfiguration")) {
      stop(messages$errorPITask("configurationsInvalidType"))
    }
  }

  invisible(TRUE)
}

#' Create a single PI task
#' @param piTaskConfig PITaskConfiguration object
#' @param observedData Named list of observed DataSet objects
#' @param scenarioConfigurations Named list of ScenarioConfiguration objects
#' @returns ParameterIdentification object
#' @keywords internal
#' @noRd
.createSinglePITask <- function(
  piTaskConfig,
  observedData,
  scenarioConfigurations
) {
  projectConfiguration <- piTaskConfig$projectConfiguration

  # Load simulation directly from model file
  simulationPath <- file.path(
    projectConfiguration$modelFolder,
    piTaskConfig$modelFile
  )
  simulation <- ospsuite::loadSimulation(
    filePath = simulationPath,
    loadFromCache = FALSE
  )

  # Create PIParameters
  piParameters <- .createPIParametersFromConfig(
    piParamsConfig = piTaskConfig$piParameters,
    simulation = simulation
  )

  # Create PIOutputMapping
  outputMappings <- .createPIOutputMappingFromConfig(
    piOutputConfig = piTaskConfig$piOutputMappings,
    simulation = simulation,
    observedData = observedData,
    scenarioConfigurations = scenarioConfigurations,
    projectConfiguration = projectConfiguration
  )

  # Create PIConfiguration
  piConfiguration <- .createPIConfigurationFromConfig(
    configOptions = piTaskConfig$piConfiguration
  )

  # Create ParameterIdentification object
  piTask <- ospsuite.parameteridentification::ParameterIdentification$new(
    simulations = simulation,
    parameters = piParameters,
    outputMappings = outputMappings,
    configuration = piConfiguration
  )

  return(piTask)
}

#' Create PIParameters from configuration
#' @param piParamsConfig Named list with parameter configuration
#' @param simulation Simulation object
#' @returns List containing one PIParameters object
#' @keywords internal
#' @noRd
.createPIParametersFromConfig <- function(piParamsConfig, simulation) {
  # Build parameter path
  containerPath <- piParamsConfig$`Container Path`
  parameterName <- piParamsConfig$`Parameter Name`
  paramPath <- paste0(containerPath, "|", parameterName)

  # Validate bounds
  minValue <- piParamsConfig$MinValue
  maxValue <- piParamsConfig$MaxValue
  startValue <- piParamsConfig$StartValue

  if (!(minValue <= startValue && startValue <= maxValue)) {
    stop(
      messages$errorPIInvalidBounds(paramPath, minValue, startValue, maxValue)
    )
  }

  # Get parameter from simulation
  param <- ospsuite::getParameter(paramPath, container = simulation$root)

  if (is.null(param)) {
    stop(messages$errorPIPathNotFound("parameter", paramPath, simulation$name))
  }

  # Create PIParameters object
  piParam <- ospsuite.parameteridentification::PIParameters$new(
    parameters = param
  )

  piParam$minValue <- minValue
  piParam$maxValue <- maxValue
  piParam$startValue <- startValue

  return(list(piParam))
}

#' Create PIOutputMapping from configuration
#' @param piOutputConfig Named list with output mapping configuration
#' @param simulation Simulation object
#' @param observedData Named list of observed DataSet objects
#' @param scenarioConfigurations Named list of ScenarioConfiguration objects
#' @param projectConfiguration ProjectConfiguration object
#' @returns List containing PIOutputMapping objects
#' @keywords internal
#' @noRd
.createPIOutputMappingFromConfig <- function(
    piOutputConfig,
    simulation,
    observedData,
    scenarioConfigurations,
    projectConfiguration) {
  scaling <- piOutputConfig$Scaling %||% "log"
  scenarioName <- piOutputConfig$Scenario

  # Get output paths from scenario configuration
  if (is.na(scenarioName) || is.null(scenarioName) || nchar(scenarioName) == 0) {
    stop(messages$errorPITask("scenarioRequired"))
  }

  if (!scenarioName %in% names(scenarioConfigurations)) {
    stop(messages$errorPINotFound(
      "scenario",
      scenarioName,
      names(scenarioConfigurations)
    ))
  }

  scenarioConfig <- scenarioConfigurations[[scenarioName]]
  outputPaths <- scenarioConfig$outputPaths

  if (length(outputPaths) == 0) {
    stop(messages$errorPITask("noOutputPath"))
  }

  outputMappings <- list()

  # Create one output mapping per output path
  for (outputPath in outputPaths) {
    # Get quantity from simulation
    quantity <- ospsuite::getQuantity(outputPath, container = simulation$root)
    if (is.null(quantity)) {
      stop(
        messages$errorPIPathNotFound("quantity", outputPath, simulation$name)
      )
    }

    # Create PIOutputMapping
    outputMapping <- ospsuite.parameteridentification::PIOutputMapping$new(
      quantity = quantity
    )

    for (dataSetName in names(observedData)) {
      outputMapping$addObservedDataSets(observedData[[dataSetName]])
    }

    outputMapping$scaling <- scaling

    outputMappings[[length(outputMappings) + 1]] <- outputMapping
  }

  return(outputMappings)
}

#' Create PIConfiguration from configuration options
#' @param configOptions Named list of configuration options
#' @returns PIConfiguration object
#' @keywords internal
#' @noRd
.createPIConfigurationFromConfig <- function(configOptions) {
  piConfig <- ospsuite.parameteridentification::PIConfiguration$new()

  # Map column names to PIConfiguration property names
  optionMapping <- list(
    "Algorithm" = "algorithm",
    "CIMethod" = "ciMethod",
    "PrintEvaluationFeedback" = "printEvaluationFeedback",
    "AutoEstimateCI" = "autoEstimateCI"
  )

  # Apply top-level options
  for (excelName in names(optionMapping)) {
    propName <- optionMapping[[excelName]]
    if (
      excelName %in% names(configOptions) && !is.na(configOptions[[excelName]])
    ) {
      value <- configOptions[[excelName]]
      # Dynamic type conversion based on PIConfiguration property type
      currentValue <- piConfig[[propName]]
      if (is.logical(currentValue)) {
        value <- as.logical(value)
      } else if (is.numeric(currentValue)) {
        value <- as.numeric(value)
      }
      piConfig[[propName]] <- value
    }
  }

  # Apply AlgorithmOptions; get defaults and merge with user options
  algorithm <- piConfig$algorithm
  if (!is.null(algorithm) && nchar(algorithm) > 0) {
    algDefaults <- ospsuite.parameteridentification:::AlgorithmDefaults
    defaultAlgOptions <- algDefaults[[algorithm]] %||% list()

    userAlgOptions <- configOptions$algorithmOptions %||% list()
    for (optName in names(userAlgOptions)) {
      value <- userAlgOptions[[optName]]
      if (!is.numeric(value)) {
        numValue <- suppressWarnings(as.numeric(value))
        if (!is.na(numValue)) {
          value <- numValue
        }
      }
      defaultAlgOptions[[optName]] <- value
    }

    piConfig$algorithmOptions <- defaultAlgOptions
  }

  # Apply CIOptions; get defaults and merge with user options
  ciMethod <- piConfig$ciMethod
  if (!is.null(ciMethod) && nchar(ciMethod) > 0) {
    ciDefaults <- ospsuite.parameteridentification:::CIDefaults
    defaultCIOptions <- ciDefaults[[ciMethod]] %||% list()

    userCIOptions <- configOptions$ciOptions %||% list()
    for (optName in names(userCIOptions)) {
      value <- userCIOptions[[optName]]
      if (!is.numeric(value)) {
        numValue <- suppressWarnings(as.numeric(value))
        if (!is.na(numValue)) {
          value <- numValue
        }
      }
      defaultCIOptions[[optName]] <- value
    }

    piConfig$ciOptions <- defaultCIOptions
  }

  return(piConfig)
}
