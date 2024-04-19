#' @name sensitivitySpiderPlot
#' @title Sensitivity Spider Plot for Pharmacokinetic Parameters
#'
#' @param sensitivityCalculation The `SensitivityCalculation` object returned by
#'   `sensitivityCalculation()`.
#' @param outputPaths,parameterPaths,pkParameters A single or a vector of the
#'   output path(s), parameter path(s), and PK parameters to be displayed,
#'   respectively. If `NULL`, all included paths and parameters present in the
#'   supplied `SensitivityCalculation` object will be displayed in the
#'   visualization.
#' @param yAxisType Character string, either "percent" (percentage change) or
#'   "absolute" (absolute values), for y-axis data normalization. Default is "percent".
#' @param xAxisScale Character string, either "log" (logarithmic scale) or "lin"
#'   (linear scale), to set the x-axis scale. Default is "log".
#' @param yAxisScale Character string, either "log" or "lin", sets the y-axis scale
#'   similarly to `xAxisScale`. Default is "lin".
#' @param yAxisFacetScales Character string, either "fixed" or "free", determines
#'   the scaling across y-axes of different facets. Default is "fixed".
#' @param defaultPlotConfiguration An object of class `DefaultPlotConfiguration`
#' used to customize plot aesthetics.
#'
#'   Supported parameters include:
#'   - `legendPosition`: Position of the legend on the plot.
#'   - `legendTitle`: Title displayed for the legend.
#'   - `linesAlpha`: Alpha transparency for line elements.
#'   - `linesColor`: Color of the line elements.
#'   - `linesSize`: Thickness of the line elements.
#'   - `pointsShape`: Shape of the point elements.
#'   - `pointsSize`: Size of the point elements.
#'   - `subtitle`: Subtitle text for the plot.
#'   - `title`: Main title text for the plot.
#'   - `titleSize`: Font size of the plot title.
#'   - `xAxisScale`: Scale type for the x-axis (`"log"` or `"lin"`).
#'   - `xLabel`: Label text for the x-axis.
#'   - `xValuesLimits`: Numeric vector specifying the limits for x-values.
#'   - `yAxisLimits`: Numeric vector specifying the limits for y-values.
#'   - `yAxisScale`: Scale type for the y-axis (`"log"` or `"lin"`).
#'   - `yAxisTicks`: Number of ticks on the y-axis.
#'   - `yLabel`: Label text for the y-axis.
#'   - `yValuesLimits`: Numeric vector specifying the limits for y-values.
#'
#'   Default values are set to provide a standardized look, but each parameter
#'   can be tailored to fit specific visual needs. Modifying these parameters
#'   will directly affect the aesthetics of the output plots.
#'
#' @import ggplot2
#'
#' @family sensitivity-calculation
#'
#' @return A `patchwork` object containing the combined ggplot objects if a
#'   single output path is specified, or a list of `patchwork` objects for
#'   multiple output paths.
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
#' sensitivitySpiderPlot(results)
#'
#' # Print plots with absolute y-axis values
#' sensitivitySpiderPlot(results, yAxisType = "absolute", yAxisFacetScales = "free")
#'
#' # Print plots with custom configuration settings
#' myPlotConfiguration <- createEsqlabsPlotConfiguration()
#' myPlotConfiguration$pointsShape <- 22
#' myPlotConfiguration$subtitle <- "Custom settings"
#' sensitivitySpiderPlot(results, defaultPlotConfiguration = myPlotConfiguration)
#' }
#' @export
sensitivitySpiderPlot <- function(sensitivityCalculation,
                                  outputPaths = NULL,
                                  parameterPaths = NULL,
                                  pkParameters = NULL,
                                  yAxisType = "percent",
                                  xAxisScale = "log",
                                  yAxisScale = "lin",
                                  yAxisFacetScales = "fixed",
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
  .validateCharVectors(pkParameters)

  ospsuite.utils::validateIsIncluded(yAxisType, c("percent", "absolute"))
  ospsuite.utils::validateIsIncluded(xAxisScale, c("lin", "log"))
  ospsuite.utils::validateIsIncluded(yAxisScale, c("lin", "log"))
  ospsuite.utils::validateIsIncluded(yAxisFacetScales, c("fixed", "free"))

  # extract and filter data ----------------

  data <- sensitivityCalculation$pkData
  data <- .filterPlottingData(
    data,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    pkParameters = pkParameters
  )

  # list of plots ------------------------

  # create plot for each output path
  lsPlots <- purrr::map(
    .x = data %>% split(.$OutputPath),
    .f = ~ .createSpiderPlot(
      .x,
      yAxisType = yAxisType,
      xAxisScale = xAxisScale,
      yAxisScale = yAxisScale,
      yAxisFacetScales = yAxisFacetScales,
      defaultPlotConfiguration = defaultPlotConfiguration
    )
  )

  # print plots without producing warnings
  suppressWarnings(purrr::walk2(lsPlots, names(lsPlots), ~ .printPlot(.x, .y)))
}

#' @keywords internal
#' @noRd
.createSpiderPlot <- function(data,
                              yAxisType = "percent",
                              xAxisScale = "log",
                              yAxisScale = "lin",
                              yAxisFacetScales = "fixed",
                              defaultPlotConfiguration) {
  # override default configuration with settings for spider plot
  spiderPlotConfiguration <- list(
    legendPosition = "bottom",
    legendTitle    = "Parameter",
    linesSize      = 1.4,
    pointsShape    = 21,
    pointsSize     = 2,
    title          = unique(data$OutputPath),
    titleSize      = 14L,
    xAxisScale     = xAxisScale,
    xLabel         = "Input parameter value [% of reference]",
    yAxisScale     = yAxisScale,
    yAxisTicks     = 10L,
    yLabel         = NULL
  )
  plotConfiguration <- .updatePlotConfiguration(
    defaultPlotConfiguration, spiderPlotConfiguration
  )

  # getting the scales right
  data <- dplyr::mutate(data,
    ParameterFactor = ParameterFactor * 100,
    PercentChangePK = PercentChangePK + 100
  )

  if (yAxisType == "percent") {
    yColumn <- sym("PercentChangePK")
    data$Unit <- "% of reference"
  } else {
    yColumn <- sym("PKParameterValue")
  }

  # filter according to value limits
  if (!is.null(plotConfiguration$yValuesLimits)) {
    data <- dplyr::filter(data, !!yColumn >= plotConfiguration$yValuesLimits[1])
    data <- dplyr::filter(data, !!yColumn <= plotConfiguration$yValuesLimits[2])
  }
  if (!is.null(plotConfiguration$xValuesLimits)) {
    data <- dplyr::filter(data, ParameterFactor >= plotConfiguration$xValuesLimits[1])
    data <- dplyr::filter(data, ParameterFactor <= plotConfiguration$xValuesLimits[2])
  }

  # create plots for each PK parameter
  plotList <- purrr::map(
    unique(data$PKParameter),
    ~ {
      dataSubset <- dplyr::filter(data, PKParameter == .x)
      baseDataSubset <- dplyr::filter(dataSubset, ParameterFactor == 100)

      if (isTRUE(yAxisFacetScales == "fixed")) {
        if (is.null(plotConfiguration$yLabel)) {
          plotConfiguration$yLabel <- switch(yAxisType,
            "percent"  = "PK-Parameter value [% of reference]",
            "absolute" = "PK-Parameter value",
            "PK-Parameter value"
          )
        }
        pBreaks <- .calculateBreaks(data[, yColumn],
          m = plotConfiguration$yAxisTicks,
          Q = c(0, 100, 200, 300)
        )
        pLimits <- .calculateLimits(data[, yColumn])
      } else {
        plotConfiguration$yLabel <- paste0(
          dataSubset$PKParameter[1], " [", dataSubset$Unit[1], "]"
        )
        pBreaks <- .calculateBreaks(
          values = dataSubset[, yColumn],
          m = plotConfiguration$yAxisTicks
        )
        pLimits <- NULL
      }

      if (!is.null(plotConfiguration$yAxisLimits)) {
        pLimits <- plotConfiguration$yAxisLimits
      }

      plot <- ggplot(
        dataSubset,
        aes(x = ParameterFactor, y = !!sym(yColumn), group = ParameterPath)
      ) +
        geom_line(
          aes(group = ParameterPath, color = as.factor(ParameterPath)),
          linewidth = plotConfiguration$linesSize,
          alpha = plotConfiguration$linesAlpha,
          na.rm = TRUE
        ) +
        geom_point(
          size  = plotConfiguration$pointsSize,
          shape = plotConfiguration$pointsShape[1],
          na.rm = TRUE
        )

      if (isTRUE(plotConfiguration$xAxisScale == "log")) {
        plot <- plot + scale_x_log10()
      }

      if (isTRUE(plotConfiguration$yAxisScale == "log")) {
        plot <- plot +
          scale_y_log10(
            limits = pLimits,
            breaks = pBreaks,
            minor_breaks = pBreaks
          )
      } else {
        plot <- plot +
          scale_y_continuous(
            limits = pLimits,
            breaks = pBreaks,
            minor_breaks = pBreaks
          )
      }

      plot <- plot +
        geom_hline(
          data = baseDataSubset,
          aes(yintercept = !!sym(yColumn)),
          linetype = "dotted",
          linewidth = 0.5,
          color = "black",
          na.rm = TRUE
        ) +
        geom_vline(
          xintercept = 100,
          linetype = "dotted",
          linewidth = 0.5,
          color = "black",
          na.rm = TRUE
        )

      plot <- plot +
        facet_wrap(~PKParameter, scales = yAxisFacetScales) +
        labs(
          x = plotConfiguration$xLabel,
          y = plotConfiguration$yLabel,
          title = NULL,
          color = plotConfiguration$legendTitle
        ) +
        theme_bw(base_size = 11) +
        theme(
          legend.position = plotConfiguration$legendPosition,
          panel.grid.minor = element_blank(),
          text = element_text(size = 11)
        ) +
        guides(col = guide_legend(
          nrow = length(unique(data$PKParameter)),
          title.position = "top"
        ))

      if (is.null(plotConfiguration$linesColor)) {
        plot <- plot + scale_color_brewer(palette = "Dark2")
      } else {
        plot <- plot + scale_color_manual(values = plotConfiguration$linesColor)
      }

      return(plot)
    }
  )

  plotPatchwork <- patchwork::wrap_plots(plotList) +
    patchwork::plot_annotation(
      title = plotConfiguration$title,
      subtitle = defaultPlotConfiguration$subtitle,
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

#' @keywords internal
#' @noRd
.updatePlotConfiguration <- function(plotConfiguration, plotOverrideConfig) {
  defaultValues <- esqlabsR::createEsqlabsPlotConfiguration()

  for (name in names(plotOverrideConfig)) {
    if (name %in% names(plotConfiguration)) {
      if (is.null(defaultValues[[name]]) ||
        all(plotConfiguration[[name]] == defaultValues[[name]])) {
        plotConfiguration[[name]] <- plotOverrideConfig[[name]]
      }
    } else {
      warning(paste("Unknown configuration option:", name))
    }
  }

  return(plotConfiguration)
}

#' @keywords internal
#' @noRd
.calculateBreaks <- function(values, ...) {
  args <- list(...)

  args$dmin <- min(na.omit(values))
  args$dmax <- max(na.omit(values))
  breaks <- do.call(labeling::extended, args)
  breaks <- round(breaks, 2)

  return(breaks)
}

#' @keywords internal
#' @noRd
.calculateLimits <- function(x) {
  limits <- c(
    (if (min(x, na.rm = TRUE) < 0) 1.01 else 0.99) * min(x),
    (if (max(x, na.rm = TRUE) > 0) 1.01 else 0.99) * max(x)
  )

  return(limits)
}
