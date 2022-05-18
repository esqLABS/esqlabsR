# dataframe extraction helpers ------------------------------

#' Extract time-series dataframe from a list of `SimulationResults` objects
#'
#' @param simulationResultsBatch A **list** of `SimulationResults` R6 objects.
#' @inheritParams sensitivityCalculation
#'
#' @keywords internal
#' @noRd
.simulationResultsBatchToTimeSeriesDataFrame <- function(simulationResultsBatch,
                                                         parameterPaths,
                                                         outputPaths) {
  purrr::map2_dfr(
    .x = simulationResultsBatch,
    .y = parameterPaths,
    .f = ~ .simulationResultsToTimeSeriesDataFrame(.x, .y, outputPaths = outputPaths)
  )
}

#' Extract time-series dataframe from `SimulationResults` object
#'
#' @param simulationResults A **single** instance of `SimulationResults` R6 object.
#' @param parameterPath A **single** parameter path.
#' @inheritParams sensitivityCalculation
#'
#' @keywords internal
#' @noRd
.simulationResultsToTimeSeriesDataFrame <- function(simulationResults,
                                                    parameterPath,
                                                    outputPaths) {
  purrr::map_dfr(
    .x  = simulationResults,
    .f  = ~ simulationResultsToDataFrame(.x, quantitiesOrPaths = outputPaths),
    .id = "ParameterFactor"
  ) %>%
    dplyr::rename(
      Concentration = simulationValues,
      OutputPath    = paths,
      Dimension     = dimension,
      Unit          = unit
    ) %>%
    .addParameterColumns(simulationResults, parameterPath) %>%
    dplyr::select(
      "OutputPath",
      dplyr::starts_with("Parameter"),
      Time, Concentration,
      dplyr::everything(),
      -c("IndividualId")
    ) %>%
    dplyr::arrange(ParameterPath, ParameterFactor)
}

#' Extract PK parameters dataframe from a list of `SimulationResults` objects
#'
#' @inheritParams .simulationResultsBatchToTimeSeriesDataFrame
#'
#' @keywords internal
#' @noRd
.simulationResultsBatchToPKDataFrame <- function(simulationResultsBatch,
                                                 parameterPaths) {
  purrr::map2_dfr(
    .x = simulationResultsBatch,
    .y = parameterPaths,
    .f = ~ .simulationResultsToPKDataFrame(.x, .y)
  )
}

#' Extract PK parameters dataframe from `Parameter` object
#'
#' @inheritParams .simulationResultsToTimeSeriesDataFrame
#'
#' @keywords internal
#' @noRd
.simulationResultsToPKDataFrame <- function(simulationResults, parameterPath) {
  purrr::map_dfr(
    .x  = simulationResults,
    .f  = ~ pkAnalysesToDataFrame(calculatePKAnalyses(.x)),
    .id = "ParameterFactor"
  ) %>%
    dplyr::rename(
      OutputPath       = QuantityPath,
      PKParameter      = Parameter,
      PKParameterValue = Value
    ) %>%
    .addParameterColumns(simulationResults, parameterPath) %>%
    dplyr::group_by(ParameterPath, PKParameter) %>%
    dplyr::group_modify(.f = ~ .computePercentChange(.)) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      "OutputPath",
      dplyr::starts_with("Parameter"),
      dplyr::starts_with("PK"),
      Unit, PercentChangePK,
      dplyr::everything(),
      -c("IndividualId")
    ) %>%
    dplyr::arrange(ParameterPath, PKParameter, ParameterFactor)
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
        ((PKParameterValue - PKParameterBaseValue) / PKParameterValue) *
          # p / delta p
          (ParameterValue / (ParameterValue - ParameterBaseValue))
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
      ParameterFactor = as.numeric(ParameterFactor)
    ) %>%
    dplyr::mutate(ParameterValue = ParameterValue * ParameterFactor)
}

#' @keywords internal
#' @noRd
.convertToWide <- function(data) {
  data %>%
    tidyr::pivot_wider(
      names_from  = PKParameter,
      values_from = c(PKParameterValue, Unit, PercentChangePK, SensitivityPKParameter),
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
    .y = seq(1:length(plotlist)),
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
