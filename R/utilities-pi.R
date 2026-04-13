#' Create Parameter Identification tasks
#'
#' @description Creates ParameterIdentification objects from PI configurations.
#'   Each PITaskConfiguration produces one ParameterIdentification object.
#'
#' @param piTaskConfigurations Named list of `PITaskConfiguration` objects
#' @param stopIfParameterNotFound Logical. If `TRUE` (default), an error is
#'   thrown when a parameter defined in the scenario configuration cannot be
#'   found in the simulation. Set to `FALSE` to continue silently when scenario
#'   parameters are absent from the model.
#'
#' @returns Named list of `ParameterIdentification` objects
#'
#' @export
createPITasks <- function(
  piTaskConfigurations,
  stopIfParameterNotFound = TRUE
) {
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
    stop(messages$errorPINoScenariosConfigured())
  }
  scenarios <- createScenarios(
    scenarioConfigurations = allScenarioConfigs,
    stopIfParameterNotFound = stopIfParameterNotFound
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
#' @param piTasks Named list of `ParameterIdentification` objects usually
#'   created using `createPITasks`
#'
#' @returns Named list of PI results. Each result contains:
#'   - task: original `ParameterIdentification` object
#'   - result: `PIResult` object from `task$run()`, or NULL if failed
#'   - error: Error message string if failed, absent on success
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
  validateIsOfType(piTasks, "ParameterIdentification")

  piResults <- list()

  for (piTaskName in names(piTasks)) {
    piTask <- piTasks[[piTaskName]]

    message(messages$messageRunningPITask(piTaskName))

    result <- tryCatch(
      {
        piResult <- piTask$run()

        list(
          task = piTask,
          result = piResult
        )
      },
      error = function(e) {
        warning(messages$warningPIOptimizationFailed(piTaskName, e$message))

        list(
          task = piTask,
          result = NULL,
          error = e$message
        )
      }
    )

    piResults[[piTaskName]] <- result
  }

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
  # Group rows by (Group, Container Path, Parameter Name). Rows with the same
  # group and path across multiple scenarios create one PIParameters object;
  # different paths always create separate objects, even with the same group.
  # If all Group values are NA, each row is independent. If some rows have an
  # explicit group, all NA rows are consolidated into a single shared group.
  allNA <- all(sapply(configurations, function(cfg) is.na(cfg$Group)))
  groups <- sapply(seq_along(configurations), function(i) {
    cfg <- configurations[[i]]
    group <- cfg$Group
    containerPath <- cfg$`Container Path`
    parameterName <- cfg$`Parameter Name`

    if (is.na(group)) {
      if (allNA) paste0("_ungrouped_", i) else "_ungrouped"
    } else {
      paste(as.character(group), containerPath, parameterName, sep = "__")
    }
  })
  uniqueGroups <- unique(groups)

  piParams <- vector("list", length(uniqueGroups))

  for (i in seq_along(uniqueGroups)) {
    groupKey <- uniqueGroups[i]
    rowIndices <- which(groups == groupKey)

    # Collect all parameters from all rows in this group
    allParameters <- list()
    firstRow <- configurations[[rowIndices[1]]]

    # Validate bounds for first row
    for (colName in c("MinValue", "MaxValue", "StartValue")) {
      if (is.na(firstRow[[colName]])) {
        stop(messages$errorPIColumnRequired(colName, "PIParameters"))
      }
    }
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
        originalGroup <- if (is.na(paramRow$Group)) {
          "_ungrouped"
        } else {
          as.character(paramRow$Group)
        }
        stop(messages$errorPIGroupBoundsMismatch(originalGroup, paramPath))
      }

      # Get scenario names this parameter applies to
      scenarioNames <- .splitCommaString(paramRow$Scenarios)

      if (length(scenarioNames) == 0) {
        stop(messages$errorPIColumnRequired("Scenarios", "PIParameters"))
      }

      # Get parameter from each scenario's simulation
      for (scenarioName in scenarioNames) {
        scenario <- scenarios[[scenarioName]]

        if (is.null(scenario)) {
          stop(messages$errorPIScenarioNotFound(
            scenarioName,
            names(scenarios)
          ))
        }

        simulation <- scenario$simulation
        param <- ospsuite::getParameter(paramPath, container = simulation)

        if (is.null(param)) {
          stop(messages$errorPIParameterNotFound(
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

  # Collect all PI-specified output paths per scenario and update simulations
  scenarioOutputPaths <- list()
  for (i in seq_along(configurations)) {
    mappingRow <- configurations[[i]]
    for (scenarioName in .splitCommaString(mappingRow$Scenarios)) {
      scenarioOutputPaths[[scenarioName]] <- unique(c(
        scenarioOutputPaths[[scenarioName]],
        mappingRow$OutputPath
      ))
    }
  }
  for (scenarioName in names(scenarioOutputPaths)) {
    scenario <- scenarios[[scenarioName]]
    if (!is.null(scenario)) {
      ospsuite::setOutputs(
        quantitiesOrPaths = scenarioOutputPaths[[scenarioName]],
        simulation = scenario$simulation
      )
    }
  }

  for (i in seq_along(configurations)) {
    mappingRow <- configurations[[i]]

    scaling <- mappingRow$Scaling %||% "log"
    outputPath <- mappingRow$OutputPath

    # Get scenario names this output mapping applies to
    scenarioNames <- .splitCommaString(mappingRow$Scenarios)

    if (length(scenarioNames) == 0) {
      stop(messages$errorPIColumnRequired("Scenarios", "PIOutputMappings"))
    }

    # Create one output mapping per scenario for this row's OutputPath
    for (scenarioName in scenarioNames) {
      scenario <- scenarios[[scenarioName]]

      if (is.null(scenario)) {
        stop(messages$errorPIScenarioNotFound(
          scenarioName,
          names(scenarios)
        ))
      }

      # Get quantity from this scenario's simulation
      quantity <- ospsuite::getQuantity(
        outputPath,
        container = scenario$simulation
      )

      if (is.null(quantity)) {
        stop(messages$errorPIOutputQuantityNotFound(
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
      if (
        is.null(dataSetName) ||
          is.na(dataSetName) ||
          nchar(dataSetName) == 0
      ) {
        stop(messages$errorPIColumnRequired("DataSet", "PIOutputMappings"))
      }

      dataSet <- observedData[[dataSetName]]
      if (is.null(dataSet)) {
        stop(messages$errorPIDatasetNotFound(
          dataSetName,
          names(observedData)
        ))
      }

      outputMapping$addObservedDataSets(dataSet)
      outputMapping$scaling <- scaling

      xOffset <- mappingRow$xOffset
      yOffset <- mappingRow$yOffset
      xFactor <- mappingRow$xFactor
      yFactor <- mappingRow$yFactor
      if (
        !all(
          c(is.na(xOffset), is.na(yOffset), is.na(xFactor), is.na(yFactor))
        )
      ) {
        outputMapping$setDataTransformations(
          labels = dataSetName,
          xOffsets = if (is.na(xOffset)) 0 else xOffset,
          yOffsets = if (is.na(yOffset)) 0 else yOffset,
          xFactors = if (is.na(xFactor)) 1 else xFactor,
          yFactors = if (is.na(yFactor)) 1 else yFactor
        )
      }

      weight <- mappingRow$Weight
      if (!is.null(weight)) {
        outputMapping$setDataWeights(setNames(list(weight), dataSetName))
      }

      outputMappings[[length(outputMappings) + 1]] <- outputMapping
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

  # Apply SimulationRunOptions
  numberOfCores <- configurations[["numberOfCores"]]
  checkForNegativeValues <- configurations[["checkForNegativeValues"]]
  hasNumberOfCores <- !is.null(numberOfCores) && !is.na(numberOfCores)
  hasCheckForNegativeValues <- !is.null(checkForNegativeValues) &&
    !is.na(checkForNegativeValues)
  if (hasNumberOfCores || hasCheckForNegativeValues) {
    simRunOptions <- ospsuite::SimulationRunOptions$new()
    if (hasNumberOfCores) {
      simRunOptions$numberOfCores <- as.integer(numberOfCores)
    }
    if (hasCheckForNegativeValues) {
      simRunOptions$checkForNegativeValues <- as.logical(checkForNegativeValues)
    }
    piConfig$simulationRunOptions <- simRunOptions
  }

  # Apply AlgorithmOptions; get defaults and merge with user options
  algorithm <- piConfig$algorithm
  if (!is.null(algorithm) && !is.na(algorithm) && nchar(algorithm) > 0) {
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

  # Apply ObjectiveFunctionOptions; read defaults and merge with user options
  ofoMapping <- list(
    "ObjectiveFunctionType" = "objectiveFunctionType",
    "ResidualWeightingMethod" = "residualWeightingMethod",
    "RobustMethod" = "robustMethod",
    "ScaleVar" = "scaleVar",
    "LinScaleCV" = "linScaleCV",
    "LogScaleSD" = "logScaleSD"
  )

  ofoOptions <- piConfig$objectiveFunctionOptions
  for (excelName in names(ofoMapping)) {
    propName <- ofoMapping[[excelName]]
    if (
      excelName %in%
        names(configurations) &&
        !is.na(configurations[[excelName]])
    ) {
      ofoOptions[[propName]] <- configurations[[excelName]]
    }
  }
  piConfig$objectiveFunctionOptions <- ofoOptions

  # Apply CIOptions; get defaults and merge with user options
  ciMethod <- piConfig$ciMethod
  if (!is.null(ciMethod) && !is.na(ciMethod) && nchar(ciMethod) > 0) {
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
