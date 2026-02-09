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
  validateIsOfType(piTaskConfigurations, PITaskConfiguration)
  piTaskConfigurations <- toList(piTaskConfigurations)

  # Extract all unique scenario configurations from all PI tasks
  allScenarioConfigs <- list()
  for (piTaskConfig in piTaskConfigurations) {
    scenarioConfigs <- piTaskConfig$scenarioConfiguration
    for (scenarioName in names(scenarioConfigs)) {
      if (!scenarioName %in% names(allScenarioConfigs)) {
        allScenarioConfigs[[scenarioName]] <- scenarioConfigs[[scenarioName]]
      }
    }
  }

  # Create all scenarios
  if (length(allScenarioConfigs) == 0) {
    stop(messages$errorPITask("noScenarios"))
  }
  scenarios <- createScenarios(
    scenarioConfigurations = allScenarioConfigs,
    stopIfParameterNotFound = TRUE
  )

  # Create PI task for each configuration
  piTasks <- vector("list", length(piTaskConfigurations)) |>
    setNames(names(piTaskConfigurations))

  for (taskName in names(piTaskConfigurations)) {
    piTasks[[taskName]] <- .createSinglePITask(
      piTaskConfiguration = piTaskConfigurations[[taskName]],
      scenarios = scenarios
    )
  }

  return(piTasks)
}

#' Run Parameter Identification tasks
#'
#' @description Executes parameter identification for all PI tasks.
#'   Handles failures gracefully - continues with other tasks if one fails.
#'
#' @param piTasks Named list of `ParameterIdentification` objects (from createPITasks)
#'
#' @returns Named list of PI results. Each result contains:
#'   - task: Original ParameterIdentification object
#'   - result: PIResult object from task$run() or NULL if failed
#'   - finalParameters: Optimized parameter values
#'   - convergence: Convergence information
#'   - success: Logical indicating success/failure
#'   - error: Error message if failed
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # After creating PI tasks
#' piResults <- runPI(piTasks)
#' }
runPI <- function(piTasks) {
  piTasks <- ospsuite.utils::toList(piTasks)
  validateIsOfType(piTasks, "list")

  piResults <- list()

  for (piTaskName in names(piTasks)) {
    piTask <- piTasks[[piTaskName]]

    result <- tryCatch(
      {
        piResult <- piTask$run()

        # Extract final parameters
        finalParams <- sapply(piTask$parameters, function(p) p$currValue)

        # Return success result
        list(
          task = piTask,
          result = piResult,
          finalParameters = finalParams,
          convergence = piResult$convergence,
          success = TRUE
        )
      },
      error = function(e) {
        warning(messages$warningPIOptimizationFailed(piTaskName, e$message))

        # Return failure result
        list(
          task = piTask,
          result = NULL,
          finalParameters = NULL,
          convergence = NULL,
          success = FALSE,
          error = e$message
        )
      }
    )

    piResults[[piTaskName]] <- result
  }

  successCount <- sum(sapply(piResults, function(r) r$success))
  failCount <- length(piResults) - successCount

  return(piResults)
}

#' Create a single PI task
#' @param piTaskConfiguration PITaskConfiguration object
#' @param scenarios Named list of Scenario objects
#' @returns ParameterIdentification object
#' @keywords internal
#' @noRd
.createSinglePITask <- function(piTaskConfiguration, scenarios) {
  # Get project configuration
  projectConfiguration <- piTaskConfiguration$projectConfiguration

  # Extract simulations from scenarios
  taskScenarioNames <- names(piTaskConfiguration$scenarioConfiguration)
  simulations <- vector("list", length(taskScenarioNames)) |>
    setNames(taskScenarioNames)

  for (scenarioName in taskScenarioNames) {
    simulations[[scenarioName]] <- scenarios[[scenarioName]]$simulation
  }

  # Create PIParameters
  piParameters <- .createPIParametersFromConfig(
    configurations = piTaskConfiguration$piParameters,
    scenarios = scenarios
  )

  # Create PIOutputMapping
  outputMappings <- .createPIOutputMappingFromConfig(
    configurations = piTaskConfiguration$piOutputMappings,
    scenarios = scenarios,
    piTaskConfiguration = piTaskConfiguration
  )

  # Create PIConfiguration
  piConfiguration <- .createPIConfigurationFromConfig(
    configurations = piTaskConfiguration$piConfiguration
  )

  # Create ParameterIdentification
  piTask <- ospsuite.parameteridentification::ParameterIdentification$new(
    simulations = simulations,
    parameters = piParameters,
    outputMappings = outputMappings,
    configuration = piConfiguration
  )

  return(piTask)
}

#' Create PIParameters from configuration
#' @param configurations List where each element is a parameter row from
#'   PIParameters sheet
#' @param scenarios Named list of Scenario objects
#' @returns List containing PIParameters objects
#' @keywords internal
#' @noRd
.createPIParametersFromConfig <- function(configurations, scenarios) {
  # Extract groups (use row index as unique group if Group is NA)
  groups <- sapply(seq_along(configurations), function(i) {
    group <- configurations[[i]]$Group
    if (is.na(group)) paste0("_ungrouped_", i) else as.character(group)
  })
  uniqueGroups <- unique(groups)

  piParams <- vector("list", length(uniqueGroups))

  for (i in seq_along(uniqueGroups)) {
    group <- uniqueGroups[i]
    rowIndices <- which(groups == group)

    # Collect all parameters from all rows in this group
    allParameters <- list()
    firstRow <- configurations[[rowIndices[1]]]

    # Validate bounds for first row
    if (
      !(firstRow$MinValue <= firstRow$StartValue &&
        firstRow$StartValue <= firstRow$MaxValue)
    ) {
      firstPath <- paste0(
        firstRow$`Container Path`,
        "|",
        firstRow$`Parameter Name`
      )
      stop(messages$errorPIInvalidBounds(
        firstPath,
        firstRow$MinValue,
        firstRow$StartValue,
        firstRow$MaxValue
      ))
    }

    for (rowIdx in rowIndices) {
      paramRow <- configurations[[rowIdx]]

      # Build parameter path
      containerPath <- paramRow$`Container Path`
      parameterName <- paramRow$`Parameter Name`
      paramPath <- paste0(containerPath, "|", parameterName)

      # Validate bounds match within group
      if (
        paramRow$MinValue != firstRow$MinValue ||
          paramRow$MaxValue != firstRow$MaxValue ||
          paramRow$StartValue != firstRow$StartValue
      ) {
        stop(messages$errorPIGroupBoundsMismatch(group, paramPath))
      }

      # Get scenario names this parameter applies to
      scenarioNames <- .splitCommaString(paramRow$Scenarios)

      if (length(scenarioNames) == 0) {
        stop(messages$errorPITask("scenarioRequired"))
      }

      # Get parameter from each scenario's simulation
      for (scenarioName in scenarioNames) {
        scenario <- scenarios[[scenarioName]]

        if (is.null(scenario)) {
          stop(messages$errorPINotFound(
            "scenario",
            scenarioName,
            names(scenarios)
          ))
        }

        simulation <- scenario$simulation
        param <- ospsuite::getParameter(paramPath, container = simulation)

        if (is.null(param)) {
          stop(messages$errorPIPathNotFound(
            "parameter",
            paramPath,
            simulation$name
          ))
        }

        allParameters[[length(allParameters) + 1]] <- param
      }
    }

    # Create one PIParameters for this group
    piParam <- ospsuite.parameteridentification::PIParameters$new(
      parameters = if (length(allParameters) == 1) {
        allParameters[[1]]
      } else {
        allParameters
      }
    )

    piParam$minValue <- firstRow$MinValue
    piParam$maxValue <- firstRow$MaxValue
    piParam$startValue <- firstRow$StartValue

    piParams[[i]] <- piParam
  }

  return(piParams)
}

#' Create PIOutputMapping from configuration
#' @param configurations List where each element is an output mapping row from
#'   PIOutputMappings sheet
#' @param scenarios Named list of Scenario objects
#' @param piTaskConfiguration PITaskConfiguration object
#' @returns List containing PIOutputMapping objects
#' @keywords internal
#' @noRd
.createPIOutputMappingFromConfig <- function(
  configurations,
  scenarios,
  piTaskConfiguration
) {
  outputMappings <- list()

  # Collect unique observed data sheets from all output mappings
  observedDataSheets <- unique(sapply(
    configurations,
    function(mapping) mapping$ObservedDataSheet
  ))
  observedDataSheets <- observedDataSheets[!is.na(observedDataSheets)]

  # Load observed data for specific sheets
  observedData <- if (length(observedDataSheets) > 0) {
    loadObservedData(
      piTaskConfiguration$projectConfiguration,
      sheets = observedDataSheets
    )
  } else {
    list()
  }

  for (i in seq_along(configurations)) {
    mappingRow <- configurations[[i]]

    scaling <- mappingRow$Scaling %||% "log"

    # Get scenario names this output mapping applies to
    scenarioNames <- .splitCommaString(mappingRow$Scenarios)

    if (length(scenarioNames) == 0) {
      stop(messages$errorPITask("scenarioRequired"))
    }

    # Create output mappings for each scenario
    for (scenarioName in scenarioNames) {
      scenario <- scenarios[[scenarioName]]

      if (is.null(scenario)) {
        stop(messages$errorPINotFound(
          "scenario",
          scenarioName,
          names(scenarios)
        ))
      }

      scenarioConfig <- scenario$scenarioConfiguration
      outputPaths <- scenarioConfig$outputPaths

      if (length(outputPaths) == 0) {
        stop(messages$errorPITask("noOutputPath"))
      }

      # Create one output mapping per output path for this scenario
      for (outputPath in outputPaths) {
        # Get quantity from this scenario's simulation
        quantity <- ospsuite::getQuantity(
          outputPath,
          container = scenario$simulation
        )

        if (is.null(quantity)) {
          stop(messages$errorPIPathNotFound(
            "quantity",
            outputPath,
            scenario$simulation$name
          ))
        }

        # Create PIOutputMapping
        outputMapping <- ospsuite.parameteridentification::PIOutputMapping$new(
          quantity = quantity
        )

        # Add observed data set specified in DataSet column
        dataSetName <- mappingRow$DataSet
        if (is.null(dataSetName) || is.na(dataSetName) || nchar(dataSetName) == 0) {
          stop(messages$errorPITask("dataSetRequired"))
        }

        dataSet <- observedData[[dataSetName]]
        if (is.null(dataSet)) {
          stop(messages$errorPINotFound(
            "dataset",
            dataSetName,
            names(observedData)
          ))
        }

        outputMapping$addObservedDataSets(dataSet)
        outputMapping$scaling <- scaling

        outputMappings[[length(outputMappings) + 1]] <- outputMapping
      }
    }
  }

  return(outputMappings)
}

#' Create PIConfiguration from configuration options
#' @param configurations Named list of configuration options from
#'   PIConfiguration sheet
#' @returns PIConfiguration object
#' @keywords internal
#' @noRd
.createPIConfigurationFromConfig <- function(configurations) {
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
      excelName %in%
        names(configurations) &&
        !is.na(configurations[[excelName]])
    ) {
      value <- configurations[[excelName]]
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

    userAlgOptions <- configurations$algorithmOptions %||% list()
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

    userCIOptions <- configurations$ciOptions %||% list()
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
