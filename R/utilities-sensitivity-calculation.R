# dataframe extraction helpers ------------------------------

#' Extract PK parameters dataframe from a list of `SimulationResults` objects
#'
#' This function processes simulation result batches by parameterPath and
#' extracts PK parameters into a dataframe.
#'
#' @param simulationResultsBatch List of simulation result batches.
#' @param parameterPaths List of parameter paths corresponding to the simulation
#' results.
#' @param customOutputFunctions Optional list of custom output functions for
#' user-defined PK analyses.
#'
#' @return A dataframe containing the PK parameters from all simulation results.
#'
#' @keywords internal
#' @noRd
.simulationResultsBatchToPKDataFrame <- function(simulationResultsBatch,
                                                 parameterPaths,
                                                 customOutputFunctions) {
  purrr::map2_dfr(
    .x = simulationResultsBatch,
    .y = parameterPaths,
    .f = ~ .simulationResultsToPKDataFrame(.x, .y, customOutputFunctions)
  )
}

#' Calculate PK parameters dataframe from simulation results
#'
#' This function calculates standard PK analyses from simulation results and
#' converts them into a dataframe. If custom output functions are provided,
#' it calculates user-defined PK analyses and integrates them with the standard
#' PK data.
#'
#' @param simulationResults List of simulation results from which PK analyses
#' are to be calculated.
#' @param parameterPath Path to the parameter in the simulation results.
#' @param customOutputFunctions Optional list of custom output functions for
#' user-defined PK analyses.
#'
#' @return A dataframe containing the combined standard and user-defined PK data.
#'
#' @keywords internal
#' @noRd
.simulationResultsToPKDataFrame <- function(simulationResults,
                                            parameterPath,
                                            customOutputFunctions = NULL) {
  # calculate standard pkAnalyses
  pkDataList <- userPKDataList <-
    setNames(
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
    OutputPath       = QuantityPath,
    PKParameter      = Parameter,
    PKParameterValue = Value
  )

  pkData <- .addParameterColumns(pkData, simulationResults, parameterPath)
  pkData <- dplyr::group_by(pkData, ParameterPath, PKParameter) %>%
    dplyr::group_modify(.f = ~ .computePercentChange(.)) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      "OutputPath",
      dplyr::starts_with("Parameter"),
      dplyr::starts_with("PK"),
      Unit,
      dplyr::everything(),
      -c("IndividualId")
    ) %>%

    dplyr::arrange(ParameterPath, PKParameter, ParameterFactor)

  return(pkData)
}

#' Calculate custom PK values
#'
#' This function calculates user-defined PK values from simulation results using
#' custom output functions.
#'
#' @param simulationResults `SimulationResults` object containing the simulation
#' data.
#' @param customOutputFunctions Named list of custom output functions to calculate
#' PK values.
#'
#' @return A dataframe containing the custom PK values.
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

  userPKValuePathList <- setNames(
    vector("list", length(outputPaths)),
    outputPaths
  )

  for (outputPath in outputPaths) {
    # filter the data frame for the current output path
    outputData <- dplyr::filter(simulationResultsDf, paths == outputPath)
    x <- outputData$Time
    y <- outputData$simulationValues

    userPKValueList <- setNames(
      vector("list", length(customOutputFunctions)),
      names(customOutputFunctions)
    )

    # calculate user-defined PK values using user-defined functions
    for (customFunctionName in names(customOutputFunctions)) {
      customOutputFunction <- customOutputFunctions[[customFunctionName]]
      formalNames <- names(formals(customOutputFunction))

      # user-defined functions should have either 'x', 'y',
      # or both 'x' and 'y' as parameters
      userPKValue <- switch(paste(sort(formalNames), collapse = ","),
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
    IndividualId, QuantityPath, Parameter, Value, Unit
  )

  return(userPKDataFrame)
}


# dataframe modification helpers ------------------------------

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

  # baseline values for parameters of interest
  ParameterBaseValue <- baseDataFrame %>% dplyr::pull(ParameterValue)
  PKParameterBaseValue <- baseDataFrame %>% dplyr::pull(PKParameterValue)

  # add columns with %change and sensitivity
  # reference: https://docs.open-systems-pharmacology.org/shared-tools-and-example-workflows/sensitivity-analysis#mathematical-background
  data %>%
    dplyr::mutate(
      PercentChangePK = ((PKParameterValue - PKParameterBaseValue) / PKParameterBaseValue) * 100,
      SensitivityPKParameter =
      # delta PK / PK
        ((PKParameterValue - PKParameterBaseValue) / PKParameterBaseValue) *
          # p / delta p
          (ParameterBaseValue / (ParameterValue - ParameterBaseValue))
    )
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
#'  `simulationResultsToDataFrame()`.
#'  @inheritParams .simulationResultsToTimeSeriesDataFrame
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

  data %>%
    dplyr::mutate(
      ParameterPath   = purrr::pluck(parameter[[1]], "path"),
      ParameterValue  = purrr::pluck(parameter[[1]], "value"),
      ParameterUnit  = purrr::pluck(parameter[[1]], "unit"),
      ParameterFactor = as.numeric(ParameterFactor)
    ) %>%
    dplyr::mutate(ParameterValue = ParameterValue * ParameterFactor)
}

# dataframe modification helpers ------------------------------

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

  # baseline values for parameters of interest
  ParameterBaseValue <- baseDataFrame %>% dplyr::pull(ParameterValue)
  PKParameterBaseValue <- baseDataFrame %>% dplyr::pull(PKParameterValue)

  # add columns with %change and sensitivity
  # reference: https://docs.open-systems-pharmacology.org/shared-tools-and-example-workflows/sensitivity-analysis#mathematical-background
  data %>%
    dplyr::mutate(
      PKPercentChange = ((PKParameterValue - PKParameterBaseValue) / PKParameterBaseValue) * 100,
      SensitivityPKParameter =
      # delta PK / PK
        ((PKParameterValue - PKParameterBaseValue) / PKParameterBaseValue) *
          # p / delta p
          (ParameterBaseValue / (ParameterValue - ParameterBaseValue))
    )
}

#' @keywords internal
#' @noRd
.convertToWide <- function(data) {
  data %>%
    tidyr::pivot_wider(
      names_from  = PKParameter,
      values_from = c(PKParameterValue, Unit, PKPercentChange, SensitivityPKParameter),
      names_glue  = "{PKParameter}_{.value}"
    ) %>%
    dplyr::rename_all(~ stringr::str_remove(.x, "PK$|PKParameter$|_PKParameterValue")) %>%
    # all metrics for each parameter should live together
    dplyr::select(
      dplyr::matches("Output|^Parameter"),
      dplyr::matches(names(ospsuite::StandardPKParameter))
    )
}

# validation helpers ------------------------------

#' @title Validate variation range
#'
#' @description
#'
#' Checks that the values entered to vary parameter:
#'
#' - are all numeric
#' - are all unique
#' - include base scaling (i.e. a scaling of 1.0)
#'
#' @inheritParams sensitivityCalculation
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
  sort(variationRange)
}

#' Validate vector arguments of `character` type
#'
#' @param argVector A vector of `character` type.
#'
#' @description
#'
#' If the parameter in your function accepts vectors of `character` type, this
#' can help you validate the following aspects:
#'
#' - the elements are indeed of `character` type
#' - none of the entries are duplicated
#' - there are no empty strings (`""`)
#'
#' @return Error if validation is unsuccessful; otherwise, `NULL`.
#'
#' @examples
#'
#' x <- c("a", "b", "a")
#' # this will produce error
#' # validateCharVectors(x)
#'
#' # this will return `NULL`
#' y <- c("a", "b", "c")
#' validateCharVectors(y)
#'
#' @keywords internal
#' @noRd
.validateCharVectors <- function(argVector) {
  argName <- deparse(substitute(argVector))

  if (!isOfType(argVector, "character", nullAllowed = TRUE)) {
    stop(paste0("Only values of `character` type are allowed in `", argName, "` argument."))
  }

  if (!hasOnlyDistinctValues(argVector)) {
    stop(paste0("Only distinct values are allowed in `", argName, "` argument."))
  }

  if (any(nchar(argVector) == 0L)) {
    stop(paste0("Values in `", argName, "` argument can't be an empty string."))
  }
}

#' Inform user if any non-standard PK parameters have been specified
#'
#' @keywords internal
#' @noRd
.validatePKParameters <- function(pkParameters) {
  if (!is.null(pkParameters) && !isIncluded(pkParameters, names(ospsuite::StandardPKParameter))) {
    nsPKNames <- pkParameters[!pkParameters %in% names(ospsuite::StandardPKParameter)]

    message(
      "Following non-standard PK parameters will not be calculated:\n",
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

# plotting helpers ------------------------------

#' @name savePlotList
#' @title Save a list of plots
#'
#' @param plotlist A list of plots (ideally form `sensitivityTimeProfiles()` or
#'   `sensitivitySpiderPlot()`).
#' @param plot.type A string specifying the prefix for plot filename.
#' @inheritParams sensitivitySpiderPlot
#'
#' @seealso sensitivityTimeProfiles, sensitivitySpiderPlot
#'
#' @examples
#'
#' # first check out examples for `sensitivityTimeProfiles()` and
#' # `sensitivitySpiderPlot()`
#'
#' @keywords internal
#' @noRd
.savePlotList <- function(plotlist,
                          plot.type,
                          outputFolder = "",
                          width = 16,
                          height = 9,
                          dpi = 300) {
  purrr::walk2(
    .x = plotlist,
    .y = seq_along(plotlist),
    .f = ~ ggplot2::ggsave(
      filename = paste0(outputFolder, plot.type, "OutputPath", .y, ".png"),
      plot = .x,
      height = height,
      width = width,
      units = "cm",
      dpi = dpi
    )
  )
}

#' Filter out data not needed for plotting
#'
#' @param data Internal data frame used while plotting.
#' @inheritParams sensitivitySpiderPlot
#'
#' @keywords internal
#' @noRd
.filterPlottingData <- function(data,
                                outputPaths = NULL,
                                parameterPaths = NULL,
                                pkParameters = NULL) {
  if (!is.null(outputPaths)) {
    data <- dplyr::filter(data, OutputPath %in% outputPaths)
  }

  if (!is.null(parameterPaths)) {
    data <- dplyr::filter(data, ParameterPath %in% parameterPaths)
  }

  if (!is.null(pkParameters)) {
    data <- dplyr::filter(data, PKParameter %in% pkParameters)
  }

  return(data)
}
