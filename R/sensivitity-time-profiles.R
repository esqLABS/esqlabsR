#' @name sensitivityTimeProfiles
#' @title Create a concentration-time profile plot
#'
#' @inheritParams sensitivitySpiderPlot
#' @inheritParams sensitivityCalculation
#' @inheritParams colorspace::scale_color_continuous_qualitative
#'
#' @import ggplot2
#' @import dplyr
#' @import colorspace
#'
#' @family sensitivity-calculation
#'
#' @return
#'
#' A single `ggplot` object if a single output path is specified.
#'
#' A list of `ggplot` objects if multiple output paths are specified.
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
#' # extract the results into a list of dataframes
#' results <- sensitivityCalculation(
#'   simulation = simulation,
#'   outputPaths = outputPaths,
#'   parameterPaths = parameterPaths
#' )
#'
#' # print plots
#' sensitivityTimeProfiles(results)
#'
#' # print and save plots
#' if (FALSE) {
#'   sensitivityTimeProfiles(
#'     results,
#'     savePlots = TRUE,
#'     height = 6,
#'     width = 12
#'   )
#' }
#' }
#' @export
sensitivityTimeProfiles <- function(sensitivityCalculation,
                                    outputPaths = NULL,
                                    parameterPaths = NULL,
                                    xAxisScale = NULL,
                                    yAxisScale = NULL,
                                    # observedData = NULL,
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
    xAxisScale = "lin",
    xLabel = NULL,
    yLabel = NULL,
    yAxisScale = "log",
    legendPosition = "right",
    legendTitle    = "Parameter factor",
    linesSize      = 1.4,
    linesAlpha     = 0.7,
    title          = NULL,
    titleSize      = 14
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

  data <- dplyr::mutate(
    data,
    ParameterPath = purrr::map_chr(ParameterPath, .splitParameterName)
  )

  # filter out data not needed for plotting
  data <- .filterPlottingData(
    data,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    pkParameters = NULL # not relevant
  )

  # list of plots ------------------------

  # create plot for each output path
  lsPlots <- purrr::map(
    .x = data %>% split(.$OutputPath),
    .f = ~ .createTimeProfiles(
      .x,
      defaultPlotConfiguration = customPlotConfiguration
    )
  )

  # print plots without producing warnings
  suppressWarnings(purrr::walk(lsPlots, ~ print(.x)))
}

#' @keywords internal
#' @noRd
.createTimeProfiles <- function(data,
                                defaultPlotConfiguration) {
  # update data dependent plot configuration
  plotConfiguration <- defaultPlotConfiguration$clone()
  plotConfiguration <- .updatePlotConfiguration(
    plotConfiguration,
    list(title = unique(data$OutputPath))
  )

  # create axis labels
  if (is.null(plotConfiguration$xLabel)) {
    plotConfiguration$xLabel <- paste0(unique(data$TimeDimension), " [",
                                       unique(data$TimeUnit), "]")
  }
  if (is.null(plotConfiguration$yLabel)) {
    plotConfiguration$yLabel <- paste0(unique(data$Dimension), " [",
                                       unique(data$Unit), "]")
  }

  # calculate y-axis breaks and limits -------
  pLimits <- .calculateLimits(data$Concentration)
  pBreaks <- .calculateBreaks(data$Concentration, m = 5, Q = c(0, 100, 1000))

  # map each parameter path to its own plot ----

  plotList <- purrr::map(
    unique(data$ParameterPath),
    ~ {
      dataSubset <- dplyr::filter(data, ParameterPath == .x)

      plot <- ggplot() +
        geom_line(
          data = dplyr::filter(dataSubset, ParameterFactor != 1.0),
          aes(x = Time,
              y = Concentration,
              group = ParameterFactor,
              color = ParameterFactor),
          size = plotConfiguration$linesSize,
          alpha = plotConfiguration$linesAlpha,
          na.rm = TRUE
        ) +
        geom_line(
          data = dplyr::filter(dataSubset, ParameterFactor == 1.0),
          aes(Time, Concentration),
          color = "black",
          size = plotConfiguration$linesSize,
          alpha = plotConfiguration$linesAlpha,
          na.rm = TRUE
        ) +
        facet_wrap(~ParameterPath, scales = "free") +
        theme_bw(base_size = 11) +
        labs(
          x = plotConfiguration$xLabel,
          y = plotConfiguration$yLabel,
          title = NULL,
          color = plotConfiguration$legendTitle
        )

      # adjusting axis scales
      if (isTRUE(plotConfiguration$xAxisScale == "log")) {
        plot <- plot + scale_x_log10()
      }
      if (isTRUE(plotConfiguration$yAxisScale == "log")) {
        plot <- plot +
          scale_y_log10(
            limits = (pLimits + 1),
            breaks = pBreaks
          )
      }

      plot <- plot +
        theme(
          legend.position = plotConfiguration$legendPosition,
          panel.grid.minor = element_blank()
        ) +
        guides(colour = guide_colourbar(
          ticks = TRUE,
          ticks.linewidth = 0.8,
          ticks.colour = "black",
          draw.ulim = FALSE,
          draw.llim = FALSE
        ))

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
