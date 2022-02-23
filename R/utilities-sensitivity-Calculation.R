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
  # baseline simulation value with a scaling of 1, i.e. no scaling
  baseValue <- data %>%
    dplyr::filter(ParameterFactor == 1.0) %>%
    dplyr::pull(PKParameterValue)

  # baseline parameter value with a scaling of 1, i.e. no scaling
  # NOT to be confused with `PKParameterValue`
  ParameterBaseValue <- data %>%
    dplyr::filter(ParameterFactor == 1.0) %>%
    dplyr::pull(ParameterValue)

  # add columns with %change and sensitivity
  # reference: https://docs.open-systems-pharmacology.org/shared-tools-and-example-workflows/sensitivity-analysis#mathematical-background
  data %>%
    dplyr::mutate(
      PercentChangePK = ((PKParameterValue - baseValue) / baseValue) * 100,
      SensitivityPKParameter =
      # delta PK / PK
        ((PKParameterValue - baseValue) / baseValue) *
          # p / delta p
          (ParameterValue / (ParameterValue - ParameterBaseValue))
    )
}

#' @title Validate variation range
#'
#' @description
#'
#' Checks that the values entered to vary parameter:
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
  ospsuite.utils::validateIsNumeric(variationRange)

  # extract only unique values
  variationRange <- unique(variationRange)

  # if there is no scaling factor of 1.0 (corresponding to no scaling), add it
  if (!any(dplyr::near(1.0, variationRange))) {
    variationRange <- c(1.0, variationRange)
  }

  # return sorted vector of scaling values
  sort(variationRange)
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
#'  @inheritParams .extractSimBatchResults
#'
#' @note Note that the function will work only with a single parameter path.
#'
#' @keywords internal
#' @noRd
.addParameterColumns <- function(data, parameter) {
  data %>%
    dplyr::mutate(
      ParameterPath = purrr::pluck(parameter, "path"),
      ParameterValue = purrr::pluck(parameter, "value"),
      ParameterFactor = as.numeric(ParameterFactor)
    ) %>%
    dplyr::mutate(ParameterValue = ParameterValue * ParameterFactor)
}

#' @title Run batch simulations and extract results in a dataframe
#'
#' @param parameter A single path of the parameter to be varied.
#' @inheritParams sensitivityCalculation
#'
#' @note Note that the function will work only with a single parameter path.
#'
#' @keywords internal
#' @noRd
.extractSimBatchResults <- function(simulation,
                                    parameter,
                                    variationRange = c(seq(0.1, 1, by = 0.1), seq(2, 10, by = 1))) {
  # check provided variation range using custom function
  variationRange <- .validateVariationRange(variationRange)

  # create simulation batch for efficient calculations
  simBatch <- createSimulationBatch(simulation, parametersOrPaths = parameter)

  # for each parameter, set the value to `referenceValue * scaleFactor`
  # and run simulations with these parameter values
  purrr::walk(
    .x = c(purrr::pluck(parameter, "value") * variationRange),
    .f = ~ simBatch$addRunValues(.x)
  )

  # use `unlist()` because we only have one `simBatch` here
  simulationResultsBatch <- unlist(runSimulationBatches(simBatch))

  # use names for parameter factor
  simulationResultsBatch <- purrr::set_names(simulationResultsBatch, variationRange)

  simulationResultsBatch
}

#' Extract time-series dataframe from a list of `SimulationResults` objects
#'
#' @param simulationResultsBatch A **list** of `SimulationResults` R6 objects.
#' @param parameters A **list** of `Parameter` R6 objects.
#'
#' @keywords internal
#' @noRd
.simulationResultsToTimeSeriesDataFrame <- function(simulationResultsBatch, outputPaths, parameters) {
  purrr::map2_dfr(
    .x = simulationResultsBatch,
    .y = parameters,
    .f = ~ .extractTimeSeriesData(.x, .y, outputPaths = outputPaths)
  )
}

#' Extract PK parameters dataframe from a list of `SimulationResults` objects
#'
#' @inheritParams .simulationResultsToTimeSeriesDataFrame
#'
#' @keywords internal
#' @noRd
.simulationResultsToPKDataFrame <- function(simulationResultsBatch, parameters) {
  purrr::map2_dfr(
    .x = simulationResultsBatch,
    .y = parameters,
    .f = ~ .extractPKData(.x, .y)
  )
}

#' Extract time-series dataframe from `SimulationResults` object
#'
#' @param simResults A **single** instance of `SimulationResults` R6 object.
#' @param parameter A **single** instance of `Parameter` R6 object.
#' @inheritParams sensitivityCalculation
#'
#' @keywords internal
#' @noRd
.extractTimeSeriesData <- function(simulationResults, outputPaths, parameter) {
  purrr::map_dfr(
    .x  = simulationResults,
    .f  = ~ simulationResultsToDataFrame(.x, quantitiesOrPaths = outputPaths),
    .id = "ParameterFactor"
  ) %>%
    dplyr::rename(
      Concentration = simulationValues,
      OutputPath = paths,
      Dimension = dimension,
      Unit = unit
    ) %>%
    .addParameterColumns(parameter) %>%
    dplyr::select(
      dplyr::starts_with("Parameter"),
      Time, Concentration,
      dplyr::everything(),
      -c("IndividualId")
    ) %>%
    dplyr::arrange(ParameterPath, ParameterFactor)
}

#' Extract PK parameters dataframe from `Parameter` object
#'
#' @inheritParams .extractTimeSeriesData
#'
#' @keywords internal
#' @noRd
.extractPKData <- function(simulationResults, parameter) {
  purrr::map_dfr(
    .x  = simulationResults,
    .f  = ~ pkAnalysesToDataFrame(calculatePKAnalyses(.x)),
    .id = "ParameterFactor"
  ) %>%
    dplyr::rename(
      OutputPath = QuantityPath,
      PKParameter = Parameter,
      PKParameterValue = Value
    ) %>%
    .addParameterColumns(parameter) %>%
    dplyr::group_by(ParameterPath, PKParameter) %>%
    dplyr::group_modify(.f = ~ .computePercentChange(.)) %>%
    ungroup() %>%
    dplyr::select(
      dplyr::starts_with("Parameter"),
      dplyr::starts_with("PK"),
      Unit, PercentChangePK,
      dplyr::everything(),
      -c("IndividualId")
    ) %>%
    dplyr::arrange(ParameterPath, PKParameter, ParameterFactor)
}

#' @keywords internal
#' @noRd
.convertToWide <- function(data) {
  data %>%
    tidyr::pivot_wider(
      names_from = PKParameter,
      values_from = c(PKParameterValue, Unit, PercentChangePK, SensitivityPKParameter),
      names_glue = "{PKParameter}_{.value}"
    ) %>%
    # columns that should not be included in the excel sheets
    dplyr::select(-c(".rowid")) %>%
    dplyr::rename_all(~ stringr::str_remove(.x, "_PKParameterValue")) %>%
    dplyr::rename_all(~ stringr::str_remove(.x, "PK$|PKParameter$")) %>%
    # all metrics for each parameter should live together
    dplyr::select(
      dplyr::matches("Output|^Parameter"),
      dplyr::matches("Unknown"),
      dplyr::matches("C_max"),
      dplyr::matches("C_max_norm"),
      dplyr::matches("C_min"),
      dplyr::matches("C_min_norm"),
      dplyr::matches("t_max"),
      dplyr::matches("t_min"),
      dplyr::matches("C_trough"),
      dplyr::matches("C_trough_norm"),
      dplyr::matches("AUC_tEnd"),
      dplyr::matches("AUC_tEnd_norm"),
      dplyr::matches("AUCM_tEnd"),
      dplyr::matches("AUC_inf"),
      dplyr::matches("AUC_inf_norm"),
      dplyr::matches("AUC_tEnd_inf"),
      dplyr::matches("AUC_tEnd_inf_norm"),
      dplyr::matches("CL"),
      dplyr::matches("MRT"),
      dplyr::matches("FractionAucEndToInf"),
      dplyr::matches("Thalf"),
      dplyr::matches("Vss"),
      dplyr::matches("Vd"),
      dplyr::matches("Tthreshold")
    )
}

#' @keywords internal
#' @noRd
.addRowid <- function(data) {
  data %>%
    tidyr::nest(data = -OutputPath) %>%
    dplyr::mutate(.rowid = paste0("OutputPath", seq(1:nrow(.)))) %>%
    tidyr::unnest(cols = c(data))
}


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
                          width = NA,
                          height = NA,
                          units = c("in", "cm", "mm", "px"),
                          dpi = 300) {
  purrr::walk2(
    .x = plotlist,
    .y = seq(1:length(plotlist)),
    .f = ~ ggsave(
      paste0(plot.type, "OutputPath", .y, ".png"),
      plot = .x,
      height = height,
      width = width,
      units = units,
      dpi = dpi
    )
  )
}
