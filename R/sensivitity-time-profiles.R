#' @name sensitivityTimeProfiles
#' @title Sensitivity Time Profiles for Pharmacokinetic Parameters
#'
#' @description
#' Generates time profiles for pharmacokinetic parameters under various
#' sensitivity scenarios. This function plots concentration-time profiles
#' for each scaled parameter specified, illustrating the dynamics of
#' pharmacokinetic responses to parameter variations.
#'
#' @param sensitivityCalculation The `SensitivityCalculation` object returned by
#' `sensitivityCalculation()`.
#' @param outputPaths,parameterPaths,pkParameters A single or a vector of the
#' output path(s), parameter path(s), and PK parameters to be displayed,
#' respectively. If `NULL`, all included paths and parameters present in the
#' supplied `SensitivityCalculation` object will be displayed in the
#' visualization.
#' A separate plot will be generated for each output path. Each plot will
#' contain a spider plot panel for each PK parameter, and the sensitivities
#' for each parameter will be displayed as lines.
#' @param xAxisScale Character string, either "log" (logarithmic scale) or "lin"
#' (linear scale), to set the x-axis scale. Default is "lin".
#' @param yAxisScale Character string, either "log" or "lin", sets the y-axis
#' scale similarly to `xAxisScale`. Default is "log".
#' @param observedData Optional. A named set of `DataSet` objects containing
#' observed data. If provided, observed data will be included in the plot. This
#' allows for direct comparison between observed and simulated data within the
#' visualizations.
#' @param defaultPlotConfiguration An object of class `DefaultPlotConfiguration`
#' used to customize plot aesthetics. Plot-specific settings provided directly
#' to the function, such as `xAxisScale`, will take precedence over any
#' modifications in `defaultPlotConfiguration`. If not provided, default
#' settings are applied.
#'
#' Supported parameters for `defaultPlotConfiguration` include:
#' - `legendPosition`: Specifies the position of the plot legend.
#' - `legendTitle`: Sets the title displayed for the legend.
#' - `linesAlpha`: Alpha transparency for the line elements.
#' - `linesColor`: Color of the line elements.
#' - `linesSize`: Thickness of the line elements.
#' - `title`: Main title text for the plot.
#' - `titleSize`: Font size of the plot title.
#' - `xAxisScale`: Scale type for the x-axis (`"log"` or `"lin"`).
#' - `xLabel`: Label text for the x-axis.
#' - `yAxisScale`: Scale type for the y-axis (`"log"` or `"lin"`).
#' - `yLabel`: Label text for the y-axis.
#'
#' @import ggplot2
#'
#' @family sensitivity-calculation
#'
#' @return A `patchwork` object containing the combined ggplot objects if a
#' single output path is specified, or a list of `patchwork` objects for
#' multiple output paths.
#'
#' @examples
#' \dontrun{
#' simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
#' simulation <- loadSimulation(simPath)
#' outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
#' parameterPaths <- c(
#'   "Aciclovir|Lipophilicity",
#'   "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
#'   "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR fraction"
#' )
#'
#' results <- sensitivityCalculation(
#'   simulation = simulation,
#'   outputPaths = outputPaths,
#'   parameterPaths = parameterPaths
#' )
#'
#' # Print plots with default settings
#' sensitivityTimeProfiles(results)
#'
#' # Print plots with linear y-axis values
#' sensitivityTimeProfiles(results, yAxisScale = "lin")
#'
#' # Print plots with custom configuration settings
#' myPlotConfiguration <- createEsqlabsPlotConfiguration()
#' myPlotConfiguration$linesColor <- c("#4D8076", "#C34A36")
#' myPlotConfiguration$subtitle <- "Custom settings"
#' sensitivityTimeProfiles(results, defaultPlotConfiguration = myPlotConfiguration)
#' }
#' @export
sensitivityTimeProfiles <- function(sensitivityCalculation,
                                    outputPaths = NULL,
                                    parameterPaths = NULL,
                                    xAxisScale = NULL,
                                    yAxisScale = NULL,
                                    observedData = NULL,
                                    defaultPlotConfiguration = NULL) {
  # input validation ------------------------

  # fail early if the object is of wrong type
  validateIsOfType(sensitivityCalculation, "SensitivityCalculation")
  if (is.null(defaultPlotConfiguration)) {
    defaultPlotConfiguration <- createEsqlabsPlotConfiguration()
  } else {
    validateIsOfType(defaultPlotConfiguration, "DefaultPlotConfiguration")
  }

  # validate vector arguments of character type
  .validateCharVectors(outputPaths)
  .validateCharVectors(parameterPaths)

  # default time profiles plot configuration setup ----

  timeProfilesConfiguration <- list(
    legendPosition = "bottom",
    legendTitle = "Parameter factor",
    linesAlpha = 0.7,
    linesSize = 1.4,
    title = NULL,
    titleSize = 14,
    xAxisScale = "lin",
    xLabel = NULL,
    yAxisScale = "log",
    yLabel = NULL
  )
  # override default plot configuration with function parameters
  customPlotConfiguration <- defaultPlotConfiguration$clone()
  if (!is.null(xAxisScale)) customPlotConfiguration$xAxisScale <- xAxisScale
  if (!is.null(yAxisScale)) customPlotConfiguration$yAxisScale <- yAxisScale

  # override only default configuration values with settings for spider plot
  customPlotConfiguration <- .updatePlotConfiguration(
    customPlotConfiguration, timeProfilesConfiguration
  )

  # validate plot configuration for valid options
  plotConfigurationList <- purrr::map(
    purrr::set_names(names(customPlotConfiguration)),
    ~ customPlotConfiguration[[.]]
  )
  ospsuite.utils::validateIsOption(
    plotConfigurationList,
    .getPlotConfigurationOptions(names(timeProfilesConfiguration))
  )

  # prepare data ------------------------

  # extract the needed dataframe from the object
  data <- .simulationResultsBatchToTimeSeriesDataFrame(
    simulationResultsBatch = sensitivityCalculation$simulationResults,
    parameterPaths         = sensitivityCalculation$parameterPaths,
    outputPaths            = sensitivityCalculation$outputPaths
  )

  # filter out data not needed for plotting
  data <- .filterPlottingData(
    data,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    pkParameters = NULL # not relevant
  )

  # split long parameter path names for plotting
  data <- dplyr::mutate(
    data,
    ParameterPath = purrr::map_chr(
      ParameterPath, .splitParameterName,
      equalLines = TRUE
    )
  )

  # add observed data to time series data frame
  if (!is.null(observedData)) {
    observedDataFrame <- .dataSetsToTimeSeriesDataFrame(observedData, data)
  } else {
    observedDataFrame <- NULL
  }

  # list of plots ------------------------

  # create plot for each output path
  lsPlots <- purrr::map(
    .x = data %>% split(.$OutputPath),
    .f = ~ .createTimeProfiles(
      .x,
      observedDataFrame = observedDataFrame,
      defaultPlotConfiguration = customPlotConfiguration
    )
  )

  # print plots without producing warnings
  suppressWarnings(purrr::walk(lsPlots, ~ print(.x)))
}

#' @keywords internal
#' @noRd
.createTimeProfiles <- function(data,
                                observedDataFrame,
                                defaultPlotConfiguration) {
  # update data dependent plot configuration
  plotConfiguration <- defaultPlotConfiguration$clone()
  plotConfiguration <- .updatePlotConfiguration(
    plotConfiguration,
    list(title = unique(data$OutputPath))
  )

  # create axis labels
  if (is.null(plotConfiguration$xLabel)) {
    plotConfiguration$xLabel <- paste0(
      unique(data$TimeDimension), " [",
      unique(data$TimeUnit), "]"
    )
  }
  if (is.null(plotConfiguration$yLabel)) {
    plotConfiguration$yLabel <- paste0(
      unique(data$Dimension), " [",
      unique(data$Unit), "]"
    )
  }

  # calculate y-axis breaks and limits -------
  pLimits <- .calculateLimits(data$Concentration)
  pBreaks <- .calculateBreaks(
    data$Concentration,
    m = 4, Q = c(0.01, 0.1, 100, 1000),
    scaling = plotConfiguration$yAxisScale
  )
  cBreaks <- c(
    min(data$ParameterFactor, na.rm = TRUE), 1,
    max(data$ParameterFactor, na.rm = TRUE)
  )
  cBreaks <- unique(ifelse(cBreaks == 0, 1, cBreaks))

  # map each parameter path to its own plot ----

  plotList <- purrr::map(
    unique(data$ParameterPath),
    ~ {
      dataSubset <- dplyr::filter(data, ParameterPath == .x)

      # combine original data subset with observed data
      # add observed data if not-null and output path is concentration
      addObeservedData <- !is.null(observedDataFrame) &&
        all(grepl("Concentration", dataSubset$Dimension))
      if (addObeservedData) {
        dataSubset <- dplyr::bind_rows(
          list(
            simulated = dataSubset,
            observed = observedDataFrame %>%
              dplyr::mutate(ParameterPath = .x)
          ),
          .id = "dataType"
        )
      }

      # basic plot setup -------------------------

      plot <- ggplot() +
        geom_line(
          data = dplyr::filter(dataSubset, ParameterFactor != 1.0),
          aes(
            x = Time,
            y = Concentration,
            group = ParameterFactor,
            color = ParameterFactor
          ),
          linewidth = plotConfiguration$linesSize,
          alpha = plotConfiguration$linesAlpha,
          na.rm = TRUE
        ) +
        geom_line(
          data = dplyr::filter(dataSubset, ParameterFactor == 1.0),
          aes(Time, Concentration),
          color = "black",
          linewidth = plotConfiguration$linesSize,
          alpha = plotConfiguration$linesAlpha,
          na.rm = TRUE
        )

      # add line for observed data
      if (addObeservedData) {
        plot <- plot +
          geom_line(
            data = dplyr::filter(dataSubset, dataType == "observed"),
            aes(Time, Concentration, linetype = dataSet)
          ) +
          scale_linetype_manual(
            values = rep(
              c("dashed", "dotted", "dotdash", "longdash", "twodash"),
              length.out = length(unique(dataSubset$dataSet))
            ),
            name = "Observed data"
          )
      }

      # adjusting axis scales
      if (isTRUE(plotConfiguration$xAxisScale == "log")) {
        plot <- plot + scale_x_log10()
      }
      if (isTRUE(plotConfiguration$yAxisScale == "log")) {
        plot <- plot +
          scale_y_log10(
            limits = replace(pLimits, pLimits == 0, 0.001),
            breaks = pBreaks,
            labels = scales::label_number()
          )
      } else {
        plot <- plot +
          scale_y_continuous(
            limits = pLimits,
            breaks = pBreaks,
            labels = scales::label_number()
          )
      }

      # finalize plot ----------------------------

      # note: facet wrap on unique PK parameter to obtain facet titles
      plot <- plot +
        facet_wrap(~ParameterPath) +
        labs(
          x = plotConfiguration$xLabel,
          y = plotConfiguration$yLabel,
          title = NULL,
          color = plotConfiguration$legendTitle
        )

      # theme adjustments
      plot <- plot +
        theme_bw(base_size = 11) +
        theme(
          legend.position = plotConfiguration$legendPosition,
          panel.grid.minor = element_blank(),
          text = element_text(size = 11)
        ) +
        guides(
          color = guide_colorbar(
            title = plotConfiguration$legendTitle,
            ticks = FALSE,
            draw.ulim = FALSE,
            draw.llim = FALSE,
            title.position = "top"
          )
        )

      if (addObeservedData) {
        plot <- plot +
          guides(
            linetype = guide_legend(
              title.position = "top",
              nrow = length(unique(dataSubset$dataType))
            )
          )
      }

      # apply color scales
      if (is.null(plotConfiguration$linesColor)) {
        plot <- plot +
          colorspace::scale_color_continuous_diverging(
            palette = "Berlin",
            mid = log10(1),
            transform = "log",
            breaks = cBreaks
          )
      } else {
        plot <- plot +
          scale_color_gradient2(
            name = plotConfiguration$legendTitle,
            low = colorspace::darken(
              plotConfiguration$linesColor[1],
              amount = 0.15
            ),
            mid = "black",
            high = colorspace::darken(
              plotConfiguration$linesColor[2],
              amount = 0.15
            ),
            transform = "log",
            midpoint = 1,
            breaks = cBreaks
          )
      }

      return(plot)
    }
  )

  # compile individual plots -----------------

  plotPatchwork <- patchwork::wrap_plots(plotList) +
    patchwork::plot_annotation(
      title = plotConfiguration$title,
      subtitle = plotConfiguration$subtitle,
      theme = theme(
        plot.title = element_text(size = plotConfiguration$titleSize)
      )
    ) +
    patchwork::plot_layout(
      guides = "collect", axes = "collect", ncol = length(plotList)
    ) &
    theme(legend.position = plotConfiguration$legendPosition)

  return(plotPatchwork)
}

#' Batch convert `DataSet` objects to time-series dataframes
#'
#' Converts each `DataSet` in a list to a dataframe using a specified reference
#' for unit conversions.
#'
#' @param dataSetList List of `DataSet` R6 objects to be converted.
#' @param timeSeriesDataFrame Reference `data.frame` for unit conversion with
#' columns `TimeUnit` and `Unit`.
#'
#' @return A single `data.frame` combining all the converted data sets.
#'
#' @keywords internal
#' @noRd
.dataSetsToTimeSeriesDataFrame <- function(dataSetList,
                                           timeSeriesDataFrame) {
  purrr::map2(
    .x = dataSetList,
    .y = names(dataSetList),
    .f = ~ .dataSetToTimeSeriesDataFrame(.x, .y, timeSeriesDataFrame)
  ) %>%
    purrr::list_rbind()
}

#' Extract time-series dataframe from a `DataSet` object
#'
#' This function converts the observed data stored in a `DataSet` object into a
#' dataframe that aligns with a given reference `timeSeriesDataFrame` created
#' using `.simulationResultsBatchToTimeSeriesDataFrame()` by matching
#' units and renaming column names. It also adds an entry at time zero.
#'
#' @param dataSet A `DataSet` R6 object that needs to be converted.
#' @param dataSetName A character string representing the name of the dataset to
#' be added to the dataframe as column `dataSet`. If not specified, defaults to
#' `NA`.
#' @param timeSeriesDataFrame A `data.frame` containing the time series, used as
#' a reference for unit conversion. Must include `TimeUnit` and `Unit` columns
#' for time and concentration units, respectively.
#'
#' @return A `data.frame` object that contains the time series data from the
#' `dataSet` object with units converted to match those in `timeSeriesDataFrame`,
#' and includes the dataset name in column `$dataSet`when provided.
#'
#' @keywords internal
#' @noRd
.dataSetToTimeSeriesDataFrame <- function(dataSet,
                                          dataSetName = NA,
                                          timeSeriesDataFrame) {
  validateIsOfType(dataSet, "DataSet")

  # convert observed data to a DataFrame
  obsData <- ospsuite::dataSetToDataFrame(dataSet)

  # extract, validate and convert units to match timeSeriesDataFrame
  timeSeriesDataFrame <- timeSeriesDataFrame %>%
    dplyr::filter(grepl("Concentration", Dimension))
  timeUnit <- unique(timeSeriesDataFrame$TimeUnit)
  concentrationUnit <- unique(timeSeriesDataFrame$Unit)
  ospsuite.utils::validateEnumValue(timeUnit, ospUnits$Time)
  ospsuite.utils::validateEnumValue(
    concentrationUnit,
    ospUnits$`Concentration [molar]`
  )

  obsDataConverted <- ospsuite:::.unitConverter(
    obsData,
    xUnit = timeUnit,
    yUnit = concentrationUnit
  )

  # rename and select columns
  obsDataConverted <- obsDataConverted %>%
    dplyr::rename(
      Time = xValues,
      Concentration = yValues,
      TimeUnit = xUnit,
      Unit = yUnit
    )
  obsDataConverted <- obsDataConverted %>%
    dplyr::select(
      intersect(names(obsDataConverted), names(timeSeriesDataFrame))
    )


  obsDataConverted <- obsDataConverted %>%
    dplyr::add_row(
      Time = 0, Concentration = 0,
      TimeUnit = timeUnit, Unit = concentrationUnit
    )

  obsDataConverted <- obsDataConverted %>%
    dplyr::mutate(dataSet = dataSetName)

  return(obsDataConverted)
}
