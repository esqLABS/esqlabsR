# dataframe extraction helpers ----

#' Extract PK parameters dataframe from a list of `SimulationResults` objects
#'
#' This function processes simulation result batches by parameterPath and
#' extracts PK parameters into a dataframe.
#'
#' @param simulationResultsBatch List of simulation result batches.
#' @param parameterPaths List of parameter paths corresponding to the simulation
#' results.
#' @param customOutputFunctions Optional list of custom output functions. The sensitivities
#' will be calculated for the outputs of these functions.
#'
#' @returns A dataframe containing the PK parameters (or values of custom
#' function) from all simulation results.
#'
#' @keywords internal
#' @noRd
.simulationResultsBatchToPKDataFrame <- function(
  simulationResultsBatch,
  parameterPaths,
  customOutputFunctions
) {
  batchResultsList <- list()

  for (i in seq_along(simulationResultsBatch)) {
    batchResultsList[[i]] <- .simulationResultsToPKDataFrame(
      simulationResultsBatch[[i]],
      parameterPaths[i],
      customOutputFunctions
    )
  }
  pkDataFrame <- dplyr::bind_rows(batchResultsList)

  return(pkDataFrame)
}

#' Calculate PK parameters dataframe from simulation results
#'
#' This function calculates standard PK analyses from simulation results and
#' converts them into a dataframe. If custom output functions are provided, it
#' calculates user-defined PK analyses and integrates them with the standard PK
#' data.
#'
#' @param simulationResults List of simulation results from which PK analyses
#'   are to be calculated.
#' @param parameterPath Path to the parameter in the simulation results.
#' @param customOutputFunctions Optional list of custom output functions for
#'   user-defined PK analyses.
#'
#' @returns A dataframe containing the combined standard and user-defined PK
#'   data.
#'
#' @keywords internal
#' @noRd
.simulationResultsToPKDataFrame <- function(
  simulationResults,
  parameterPath,
  customOutputFunctions = NULL
) {
  # calculate standard pkAnalyses
  pkDataList <- userPKDataList <-
    stats::setNames(
      vector("list", length(simulationResults)),
      names(simulationResults)
    )
  for (simResult in names(simulationResults)) {
    pkDataFrame <- pkAnalysesToDataFrame(
      calculatePKAnalyses(
        simulationResults[[simResult]]
      )
    )
    pkDataList[[simResult]] <- pkDataFrame
  }
  pkData <- dplyr::bind_rows(pkDataList, .id = "ParameterFactor")

  # calculate user-defined pkAnalyses
  if (!is.null(customOutputFunctions)) {
    for (simResult in names(simulationResults)) {
      userPKDataFrame <- .calculateCustomPK(
        simulationResults[[simResult]],
        customOutputFunctions
      )
      userPKDataList[[simResult]] <- userPKDataFrame
    }
    userPKData <- dplyr::bind_rows(userPKDataList, .id = "ParameterFactor")
  } else {
    userPKData <- data.frame()
  }

  # combine standard and user-defined PK data
  pkData <- dplyr::bind_rows(pkData, userPKData)

  # modify and format data
  pkData <- dplyr::rename(
    pkData,
    OutputPath = QuantityPath,
    PKParameter = Parameter,
    PKParameterValue = Value
  )

  pkData <- .addParameterColumns(pkData, simulationResults, parameterPath)
  pkData <- dplyr::group_by(pkData, ParameterPath, PKParameter) |>
    dplyr::group_modify(.f = ~ .computePercentChange(.)) |>
    dplyr::ungroup()

  pkData <- dplyr::select(pkData, -dplyr::any_of("IndividualId"))
  pkData <- dplyr::relocate(
    pkData,
    "OutputPath",
    dplyr::starts_with("Parameter"),
    dplyr::starts_with("PK"),
    Unit
  ) |>
    dplyr::arrange(ParameterPath, PKParameter, ParameterFactor)

  return(pkData)
}

# dataframe modification helpers ----

#' Calculate custom PK values
#'
#' This function calculates user-defined PK values from simulation results using
#' custom output functions.
#'
#' @param simulationResults `SimulationResults` object containing the simulation
#'   data.
#' @param customOutputFunctions Named list of custom output functions to
#'   calculate PK values.
#'
#' @returns A dataframe containing the custom PK values.
#'
#' @keywords internal
#' @noRd
.calculateCustomPK <- function(simulationResults, customOutputFunctions) {
  # validate customOutputFunctions
  .validateIsNamedList(customOutputFunctions, nullAllowed = TRUE)
  validateIsOfType(customOutputFunctions, "function", nullAllowed = TRUE)

  # extract all output paths
  outputPaths <- simulationResults$allQuantityPaths

  # extract simulation result values
  simulationResultsDf <- simulationResultsToDataFrame(simulationResults)

  userPKValuePathList <- stats::setNames(
    vector("list", length(outputPaths)),
    outputPaths
  )

  for (outputPath in outputPaths) {
    # filter the data frame for the current output path
    outputData <- dplyr::filter(simulationResultsDf, paths == outputPath)
    x <- outputData$Time
    y <- outputData$simulationValues

    userPKValueList <- stats::setNames(
      vector("list", length(customOutputFunctions)),
      names(customOutputFunctions)
    )

    # calculate user-defined PK values using user-defined functions
    for (customFunctionName in names(customOutputFunctions)) {
      customOutputFunction <- customOutputFunctions[[customFunctionName]]
      formalNames <- names(formals(customOutputFunction))

      # user-defined functions should have either 'x', 'y',
      # or both 'x' and 'y' as parameters
      userPKValue <- switch(
        paste(sort(formalNames), collapse = ","),
        "x,y" = customOutputFunction(x = x, y = y),
        "x" = customOutputFunction(x = x),
        "y" = customOutputFunction(y = y),
        stop(messages$invalidCustomFunctionParameters(formalNames))
      )

      userPKValueList[[customFunctionName]] <- data.frame(
        Parameter = customFunctionName,
        Value = userPKValue,
        IndividualId = simulationResults$allIndividualIds[1],
        QuantityPath = outputPath,
        Unit = NA
      )
    }
    userPKValuePathList[[outputPath]] <- dplyr::bind_rows(userPKValueList)
  }

  # combined and prepare PK data to match calculatePKAnalyses() output
  userPKDataFrame <- dplyr::bind_rows(userPKValuePathList)
  userPKDataFrame <- dplyr::select(
    userPKDataFrame,
    IndividualId,
    QuantityPath,
    Parameter,
    Value,
    Unit
  )

  return(userPKDataFrame)
}

#' @title Percent change in PK parameters
#'
#' @description Compute %change in PK parameters and their sensitivity
#'
#' @param data A dataframe returned by `pkAnalysesAsDataFrame()` and with
#'   columns renamed to follow `UpperCamel` case.
#'
#' @keywords internal
#' @noRd
.computePercentChange <- function(data) {
  # baseline values with a scaling of 1, i.e. no scaling
  baseDataFrame <- dplyr::filter(data, ParameterFactor == 1.0)

  # Handle case where no baseline data (ParameterFactor == 1.0) exists
  if (nrow(baseDataFrame) == 0) {
    parameterPath <- unique(data$ParameterPath)[1]
    pkParameter <- unique(data$PKParameter)[1]

    warning(
      messages$warningSensitivityPKParameterNotCalculated(
        parameterPath,
        pkParameter
      ),
      call. = FALSE
    )

    return(
      dplyr::mutate(
        data,
        PKPercentChange = NA_real_,
        SensitivityPKParameter = NA_real_
      )
    )
  }

  # baseline values for parameters of interest
  ParameterBaseValue <- baseDataFrame |> dplyr::pull(ParameterValue)
  PKParameterBaseValue <- baseDataFrame |> dplyr::pull(PKParameterValue)

  # add columns with %change and sensitivity
  # reference: https://docs.open-systems-pharmacology.org/shared-tools-and-example-workflows/sensitivity-analysis#mathematical-background
  data |>
    dplyr::mutate(
      PKPercentChange = ((PKParameterValue - PKParameterBaseValue) /
        PKParameterBaseValue) *
        100,
      # delta PK / PK
      SensitivityPKParameter = ((PKParameterValue - PKParameterBaseValue) /
        PKParameterBaseValue) *
        # p / delta p
        (ParameterBaseValue / (ParameterValue - ParameterBaseValue))
    )
}

#' @keywords internal
#' @noRd
.convertToWide <- function(
  data,
  pkParameterNames = names(ospsuite::StandardPKParameter)
) {
  dataWide <- tidyr::pivot_wider(
    data,
    names_from = PKParameter,
    values_from = c(
      PKParameterValue,
      Unit,
      PKPercentChange,
      SensitivityPKParameter
    ),
    names_glue = "{PKParameter}_{.value}"
  ) |>
    dplyr::rename_all(
      ~ stringr::str_remove(.x, "PK$|PKParameter$|_PKParameterValue")
    ) |>
    # metrics for each parameter are grouped together
    dplyr::select(
      dplyr::matches("Output|^Parameter"),
      dplyr::matches(pkParameterNames)
    ) |>
    dplyr::arrange(OutputPath)

  return(dataWide)
}

#' @title Add columns with details about parameter paths
#'
#' @description
#'
#' Adds columns with additional details about parameter paths:
#' - name,
#' - reference values
#' - scaled values
#'
#' @param data A dataframe returned by `pkAnalysesAsDataFrame()` or by
#'   `simulationResultsToDataFrame()`.
#' @inheritParams .simulationResultsToTimeSeriesDataFrame
#'
#' @note Note that the function will work only with a single parameter path.
#'
#' @keywords internal
#' @noRd
.addParameterColumns <- function(data, simulationResults, parameterPath) {
  parameter <- getAllParametersMatching(
    parameterPath,
    purrr::pluck(simulationResults, 1L, "simulation")
  )

  data |>
    dplyr::mutate(
      ParameterPath = purrr::pluck(parameter[[1]], "path"),
      ParameterValue = purrr::pluck(parameter[[1]], "value"),
      ParameterUnit = purrr::pluck(parameter[[1]], "unit"),
      ParameterFactor = as.numeric(ParameterFactor),
      ParameterPathUserName = names(parameterPath) %||% NA_character_,
    ) |>
    dplyr::mutate(ParameterValue = ParameterValue * ParameterFactor)
}

# variationRange handlers ----

#' @title Validate variation range vector
#'
#' @description
#'
#' Checks that the values entered to vary parameter:
#'
#' - are all numeric
#' - are all unique
#' - include base scaling (i.e. a scaling of 1.0)
#'
#' @param variationRange A numeric vector of values representing scaling
#'   factors.
#'
#' @keywords internal
#' @noRd
.validateVariationRange <- function(variationRange) {
  # only numbers allowed
  validateIsNumeric(variationRange)

  # extract only unique values
  variationRange <- unique(variationRange)

  # if there is no scaling factor of 1.0 (corresponding to no scaling), add it
  if (!any(dplyr::near(1.0, variationRange))) {
    variationRange <- c(1.0, variationRange)
  }

  # return sorted vector of scaling values
  return(sort(variationRange))
}

#' Normalize variation range to a list
#'
#' Ensures that `variationRange` is normalized to a list, either by converting a
#' vector or validating that a provided list is of the correct length matching
#' `parameterPaths`.
#'
#' @param variationRange A vector or list of variation values.
#' @param parameterPaths A single or a vector of the parameter path(s) to be
#'   varied.
#' @returns A named list of `variationRange` values.
#'
#' @keywords internal
#' @noRd
.normalizeVariationRange <- function(variationRange, parameterPaths) {
  variationRange <- toList(variationRange)

  if (length(variationRange) == 1) {
    # Ensure the lengths of variationRange and parameterPath are equal
    variationRange <- rep(variationRange, length(parameterPaths))
  } else if (length(variationRange) != length(parameterPaths)) {
    stop(
      messages$invalidVariationRangeLength()
    )
  }

  names(variationRange) <- parameterPaths

  return(variationRange)
}

#' Transform variation range from absolute to relative
#'
#' This function transforms absolute values in `variationRange` to relative
#' values based on `initialValues` when `variationType` is set to "absolute".
#'
#' @param variationRange A named list of variation values (absolute or
#'   relative).
#' @param initialValues A named list of initial parameter values.
#' @param variationType A string specifying the variation type ("absolute" or
#'   "relative").
#' @returns A list of transformed variationRange values.
#'
#' @keywords internal
#' @noRd
.transformVariationRange <- function(
  variationRange,
  initialValues,
  variationType
) {
  if (variationType == "absolute") {
    variationRange <- purrr::map2(variationRange, initialValues, ~ .x / .y)
  }

  return(variationRange)
}

# validation helpers ----

#' Inform user if any non-standard PK parameters have been specified
#'
#' @keywords internal
#' @noRd
.validatePKParameters <- function(pkParameters) {
  if (
    !is.null(pkParameters) &&
      !isIncluded(pkParameters, ospsuite::allPKParameterNames())
  ) {
    nsPKNames <- pkParameters[
      !pkParameters %in% ospsuite::allPKParameterNames()
    ]

    message(
      "Following PK parameters are specified but were not calculated:\n",
      paste0(nsPKNames, collapse = "\n")
    )
  }
}

#' Validate Named List
#'
#' Check if the object is a named list. Allows NULL if `nullAllowed` is TRUE.
#'
#' @param object The object to validate.
#' @param nullAllowed Logical. If TRUE, allows NULL. Default is FALSE.
#' @keywords internal
#' @noRd
.validateIsNamedList <- function(object, nullAllowed = FALSE) {
  validateIsOfType(object, "list", nullAllowed = nullAllowed)

  if (!is.null(object)) {
    listNames <- names(object)
    argName <- deparse(substitute(object))
    if (hasEmptyStrings(listNames)) {
      stop(messages$errorNotNamedList(argName))
    }
  }
}

# Sensitivity calculation ----

#' @name sensitivityCalculation
#' @title Carry out and visualize sensitivity analysis (with OSPSuite)
#'
#' @param simulation An object of type `Simulation`.
#' @param outputPaths Path (or a vector of paths) to the output(s) for which the
#'   sensitivity will be analyzed.
#' @param parameterPaths A single or a vector of the parameter path(s) to be
#'   varied. Can also be a named vector, where the names are user-defined
#'   labels. These names will be stored and used in downstream plotting
#'   functions (e.g., as legend labels) if provided.
#' @param variationRange Optional numeric vector or list defining the scaling of
#'   the parameters. The same variation range is applied to all specified
#'   parameters unless a list is provided, in which case the length of the list
#'   must match the length of `parameterPaths`, allowing individual variation
#'   for each parameter. If not specified, the following vector will be used:
#'   c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5, 6, 7, 8, 9,
#'   10).
#' @param variationType A string specifying whether the values in
#'   `variationRange` are applied as `"absolute"` or `"relative"` scaling. When
#'   set to `"absolute"`, the values are interpreted as absolute parameter
#'   values. When set to `"relative"`, the values are interpreted as scaling
#'   factors relative to the initial parameter values. Default is `"relative"`.
#' @param pkParameters A vector of names of PK parameters for which the
#'   sensitivities will be calculated. For a full set of available standard PK
#'   parameters, run `names(ospsuite::StandardPKParameter)`. By default, the
#'   following parameters are considered: `"C_max"`, `"t_max"`, `"AUC_inf"`. If
#'   `NULL`, all available PK-parameters (including the user-defined) will be
#'   calculated.
#' @param customOutputFunctions A named list with custom function(s) for output
#'   calculations. User-defined functions should have either 'x', 'y', or both
#'   'x' and 'y' as parameters which correspond to x-Dimension (time) or
#'   y-Dimension values from simulation results. The output of the function is a
#'   single numerical value for each output and parameter path, which is then
#'   included in the returned dataframe of PK parameters.
#' @param saOutputFilePath Path to excel file in which PK-parameter data should
#'   be saved. If a file already exists, it will be overwritten. Default is
#'   `NULL`, meaning the data will not be saved to a spreadsheet.
#' @param simulationRunOptions Optional instance of a `SimulationRunOptions`
#'   used during the simulation run
#'
#' @family sensitivity-calculation
#'
#' @returns
#'
#' A list containing following objects:
#' - `SimulationResults`
#' - specified output paths
#' - specified parameter paths
#' - A data frame of PK parameters
#'
#' @examples
#' \dontrun{
#' simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
#' simulation <- loadSimulation(simPath)
#' outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
#' parameterPaths <- c(
#'   "Aciclovir|Lipophilicity",
#'   "Events|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
#'   "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR-Aciclovir|GFR fraction"
#' )
#'
#' # extract the results into a list of dataframes
#' sensitivityCalculation(
#'   simulation = simulation,
#'   outputPaths = outputPaths,
#'   parameterPaths = parameterPaths
#' )
#'
#' # Calculate sensitivity for a user-defined function that computes the
#' # averate of the simulated y-values
#' customOutputFunctions <- list(
#'   "Average" = function(y) mean(y)
#' )
#' sensitivityCalculation(
#'   simulation = simulation,
#'   outputPaths = outputPaths,
#'   parameterPaths = parameterPaths,
#'   customOutputFunctions = customOutputFunctions
#' )
#' }
#'
#' @export
sensitivityCalculation <- function(
  simulation,
  outputPaths,
  parameterPaths,
  variationRange = c(seq(0.1, 1, by = 0.1), seq(2, 10, by = 1)),
  variationType = c("relative", "absolute"),
  pkParameters = c("C_max", "t_max", "AUC_inf"),
  customOutputFunctions = NULL,
  saOutputFilePath = NULL,
  simulationRunOptions = NULL
) {
  # Input validation ------------------------------------------------------

  # Validate vector arguments of character type
  .validateCharVector(outputPaths)
  .validateCharVector(parameterPaths)
  .validateCharVector(names(parameterPaths), nullAllowed = TRUE)
  .validateCharVector(variationType)
  .validateCharVector(pkParameters, nullAllowed = TRUE)

  variationType <- match.arg(variationType)

  # Check for non-standard PK parameters
  .validatePKParameters(pkParameters)

  # Validate customOutputFunctions
  .validateIsNamedList(customOutputFunctions, nullAllowed = TRUE)
  validateIsOfType(customOutputFunctions, "function", nullAllowed = TRUE)

  # Fail early to avoid costly failure after analysis is already carried out.
  if (!is.null(saOutputFilePath)) {
    validateIsFileExtension(saOutputFilePath, "xlsx")
  }

  # Creating SimulationResults batches ------------------------------------

  # Normalize variationRange
  variationRange <- .normalizeVariationRange(variationRange, parameterPaths)

  # Store old simulation outputs and set user defined
  oldOutputSelections <- simulation$outputSelections$allOutputs
  setOutputs(quantitiesOrPaths = outputPaths, simulation = simulation)

  # Store initial value for each parameter
  initialValues <- vector("double", length(parameterPaths))
  names(initialValues) <- parameterPaths

  # Classify parameters and retrieve initial values
  constantParamPaths <- list()
  formulaParamPaths <- list()

  for (parameterPath in parameterPaths) {
    # Check if the parameter is given by an explicit formula
    isExplicitFormulaByPath <- ospsuite::isExplicitFormulaByPath(
      path = parameterPath,
      simulation = simulation
    )

    # Classify as formula or constant parameter
    if (isExplicitFormulaByPath) {
      formulaParamPaths <- c(formulaParamPaths, parameterPath)
    } else {
      constantParamPaths <- c(constantParamPaths, parameterPath)
    }

    # Store the initial values for this parameter
    initialValues[[parameterPath]] <- ospsuite::getQuantityValuesByPath(
      quantityPaths = parameterPath,
      simulation = simulation
    )
  }

  # Transform and validate variationRange
  variationRange <- .transformVariationRange(
    variationRange,
    initialValues,
    variationType
  )
  variationRange <- lapply(variationRange, .validateVariationRange)

  # Initialize batchResultsIdMap
  batchResultsIdMap <- vector("list", length(parameterPaths))
  names(batchResultsIdMap) <- parameterPaths

  for (parameterPath in parameterPaths) {
    # Initialize batchResultsIdMap for the current parameter
    batchResultsIdMap[[parameterPath]] <- vector(
      "list",
      length(variationRange[[parameterPath]])
    )
    names(batchResultsIdMap[[parameterPath]]) <- variationRange[[parameterPath]]
  }

  constantParamPaths <- unlist(constantParamPaths, use.names = FALSE)
  formulaParamPaths <- unlist(formulaParamPaths, use.names = FALSE)

  simulationBatches <- list()

  # Create one batch for all constant parameters
  if (length(constantParamPaths) > 0) {
    constantBatch <- createSimulationBatch(
      simulation = simulation,
      parametersOrPaths = constantParamPaths
    )

    # Add run values. While varying one parameter, the values of remaining
    # constant parameters remain at their initial values.
    for (constantParamPath in constantParamPaths) {
      for (scaleFactorIdx in seq_along(variationRange[[constantParamPath]])) {
        # Change the value of the varied parameter
        runValues <- initialValues[constantParamPaths]
        runValues[[constantParamPath]] <-
          variationRange[[constantParamPath]][[scaleFactorIdx]] *
          runValues[[constantParamPath]]

        # Add run values and store the ID in the `batchResultsIdMap`
        batchResultsIdMap[[constantParamPath]][[scaleFactorIdx]] <-
          constantBatch$addRunValues(parameterValues = runValues)
      }
    }

    simulationBatches <- c(simulationBatches, constantBatch)
  }

  # Add batches for formula parameters
  for (formulaParamPath in formulaParamPaths) {
    formulaBatch <- createSimulationBatch(
      simulation = simulation,
      parametersOrPaths = formulaParamPath
    )

    # Add run values.
    for (scaleFactorIdx in seq_along(variationRange[[formulaParamPath]])) {
      batchResultsIdMap[[formulaParamPath]][[scaleFactorIdx]] <-
        formulaBatch$addRunValues(
          parameterValues = variationRange[[formulaParamPath]][[
            scaleFactorIdx
          ]] *
            initialValues[[formulaParamPath]]
        )
    }

    simulationBatches <- c(simulationBatches, formulaBatch)
  }

  # Simulate all batches in parallel
  simulationBatchesResults <- runSimulationBatches(
    simulationBatches = simulationBatches,
    simulationRunOptions = simulationRunOptions
  )

  # Remove top-level names to flatten the list in the next step
  names(simulationBatchesResults) <- NULL

  # Unlist to gather all results, using batchResultsIdMap to filter by
  # parameter/scale factor
  simulationBatchesResults <- unlist(simulationBatchesResults)

  # Nest simulation results by parameter path and factor values
  simulationResultsBatch <- batchResultsIdMap

  for (parameterPath in names(batchResultsIdMap)) {
    for (parameterFactor in names(batchResultsIdMap[[parameterPath]])) {
      resultId <- batchResultsIdMap[[parameterPath]][[parameterFactor]]
      resultSim <- purrr::pluck(simulationBatchesResults, resultId)
      if (!is.null(resultSim)) {
        simulationResultsBatch[[parameterPath]][[parameterFactor]] <-
          purrr::pluck(simulationBatchesResults, resultId)
      } else {
        simulationResultsBatch[[parameterPath]][[parameterFactor]] <- NULL
        warning(
          messages$sensitivityAnalysisSimulationFailure(
            parameterPath,
            parameterFactor
          )
        )
      }
    }
  }

  # Extract and save data frames ------------------------------------------

  # Extract data frame for PK parameters
  pkData <- .simulationResultsBatchToPKDataFrame(
    simulationResultsBatch,
    parameterPaths,
    customOutputFunctions
  )

  # Filter out unneeded PK parameters
  if (!is.null(pkParameters)) {
    filterValues <- c(pkParameters, names(customOutputFunctions))
    pkData <- dplyr::filter(pkData, PKParameter %in% filterValues)
  }

  # Write each data frame in a list to a separate sheet in Excel
  if (!is.null(saOutputFilePath)) {
    # If there is no data to write to Excel sheet, inform user and do nothing.
    if (nrow(pkData) == 0L) {
      warning(messages$noPKDataToWrite())
    } else {
      # Convert tidy data to wide format
      pkParameterNames <- c(
        ospsuite::allPKParameterNames(),
        names(customOutputFunctions)
      )
      pkDataWide <- .convertToWide(pkData, pkParameterNames)

      # Write to a spreadsheet with one sheet per output path.
      .writeExcel(data = pkDataWide, path = saOutputFilePath)
    }
  }

  # Return sensitivity calculation results --------------------------------

  # Final list with needed objects and data frames for plotting functions.
  results <- list(
    "simulationResults" = simulationResultsBatch,
    "outputPaths" = outputPaths,
    "parameterPaths" = parameterPaths,
    "pkData" = pkData
  )

  # Reset simulation outputs
  oldOutputSelections <- simulation$outputSelections$allOutputs
  clearOutputs(simulation = simulation)

  for (outputSelection in oldOutputSelections) {
    ospsuite::addOutputs(
      quantitiesOrPaths = outputSelection$path,
      simulation = simulation
    )
  }

  class(results) <- c("SensitivityCalculation", class(results))

  return(results)
}
