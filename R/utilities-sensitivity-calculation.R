# dataframe extraction helpers ------------------------------

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
.simulationResultsBatchToPKDataFrame <- function(simulationResultsBatch,
                                                 parameterPaths,
                                                 customOutputFunctions) {
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
#' @returns A dataframe containing the combined standard and user-defined PK data.
#'
#' @keywords internal
#' @noRd
.simulationResultsToPKDataFrame <- function(simulationResults,
                                            parameterPath,
                                            customOutputFunctions = NULL) {
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
    OutputPath       = QuantityPath,
    PKParameter      = Parameter,
    PKParameterValue = Value
  )

  pkData <- .addParameterColumns(pkData, simulationResults, parameterPath)
  pkData <- dplyr::group_by(pkData, ParameterPath, PKParameter) %>%
    dplyr::group_modify(.f = ~ .computePercentChange(.)) %>%
    dplyr::ungroup()

  pkData <- dplyr::select(pkData, -dplyr::any_of("IndividualId"))
  pkData <- dplyr::relocate(
    pkData,
    "OutputPath",
    dplyr::starts_with("Parameter"),
    dplyr::starts_with("PK"),
    Unit
  ) %>%
    dplyr::arrange(ParameterPath, PKParameter, ParameterFactor)

  return(pkData)
}

# dataframe modification helpers ------------------------------

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
    userPKDataFrame, IndividualId, QuantityPath, Parameter, Value, Unit
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
.convertToWide <- function(data,
                           pkParameterNames = names(ospsuite::StandardPKParameter)) {
  dataWide <- tidyr::pivot_wider(
    data,
    names_from  = PKParameter,
    values_from = c(PKParameterValue, Unit, PKPercentChange, SensitivityPKParameter),
    names_glue  = "{PKParameter}_{.value}"
  ) %>%
    dplyr::rename_all(~ stringr::str_remove(.x, "PK$|PKParameter$|_PKParameterValue")) %>%
    # metrics for each parameter are grouped together
    dplyr::select(
      dplyr::matches("Output|^Parameter"),
      dplyr::matches(pkParameterNames)
    ) %>%
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
      ParameterPath = purrr::pluck(parameter[[1]], "path"),
      ParameterValue = purrr::pluck(parameter[[1]], "value"),
      ParameterUnit = purrr::pluck(parameter[[1]], "unit"),
      ParameterFactor = as.numeric(ParameterFactor),
      ParameterPathUserName = names(parameterPath) %||% NA_character_,
    ) %>%
    dplyr::mutate(ParameterValue = ParameterValue * ParameterFactor)
}

# variationRange handlers -------------------------------------------------

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
#' @param variationRange A numeric vector of values representing scaling factors.
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
#' Ensures that `variationRange` is normalized to a list, either by converting
#' a vector or validating that a provided list is of the correct length matching
#' `parameterPaths`.
#'
#' @param variationRange A vector or list of variation values.
#' @param parameterPaths A single or a vector of the parameter path(s) to be
#' varied.
#' @returns A named list of `variationRange` values.
#'
#' @keywords internal
#' @noRd
.normalizeVariationRange <- function(variationRange, parameterPaths) {
  variationRange <- toList(variationRange)

  if (length(variationRange) == 1) {
    variationRange <- rep(variationRange, length(parameterPaths))
  }
  # Ensure the lengths of variationRange and parameterPath are equal
  else if (length(variationRange) != length(parameterPaths)) {
    cli::cli_abort(
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
#' @param variationRange A named list of variation values (absolute or relative).
#' @param initialValues A named list of initial parameter values.
#' @param variationType A string specifying the variation type ("absolute" or "relative").
#' @returns A list of transformed variationRange values.
#'
#' @keywords internal
#' @noRd
.transformVariationRange <- function(variationRange, initialValues, variationType) {
  if (variationType == "absolute") {
    variationRange <- purrr::map2(variationRange, initialValues, ~ .x / .y)
  }

  return(variationRange)
}

# validation helpers ------------------------------

#' Validate vector arguments of `character` type
#'
#' @param argVector A vector of `character` type.
#' @param nullAllowed Boolean flag if `NULL` is accepted for the object. Default
#' is `FALSE`.
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
#' @returns Error if validation is unsuccessful; otherwise, `NULL`.
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
.validateCharVectors <- function(argVector, nullAllowed = FALSE) {
  argName <- deparse(substitute(argVector))

  # Handle NULL cases based on nullAllowed
  if (is.null(argVector) && !nullAllowed) {
    cli::cli_abort("The argument `{argName}` cannot be NULL.")
  }

  # Skip further checks if NULL is allowed and the argument is NULL
  if (is.null(argVector)) {
    return()
  }

  # Check if the argument is of type character
  if (!isOfType(argVector, "character")) {
    cli::cli_abort(c(
      "x" = "The argument `{argName}` must be of type {.val character}.",
      "i" = "Ensure that the provided value is a character vector."
    ))
  }

  # Ensure all values are distinct
  if (!hasOnlyDistinctValues(argVector)) {
    cli::cli_abort(c(
      "x" = "The argument `{argName}` must contain only distinct values.",
      "i" = "Remove any duplicate entries from the `{argName}`."
    ))
  }

  # Check for empty string values
  if (any(nchar(argVector) == 0L)) {
    cli::cli_abort(c(
      "x" = "The argument `{argName}` contains empty strings.",
      "i" = "Ensure that `{argName}` does not have any empty entries."
    ))
  }
}


#' Inform user if any non-standard PK parameters have been specified
#'
#' @keywords internal
#' @noRd
.validatePKParameters <- function(pkParameters) {
  if (!is.null(pkParameters) && !isIncluded(pkParameters, ospsuite::allPKParameterNames())) {
    nsPKNames <- pkParameters[!pkParameters %in% ospsuite::allPKParameterNames()]

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
