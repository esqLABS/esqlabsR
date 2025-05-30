#' @name sensitivitySpiderPlot
#' @title Sensitivity Spider Plot for Pharmacokinetic Parameters
#'
#' @description
#' Creates spider plots for sensitivity calculation. A spider plot is a
#' visualization technique that displays the sensitivity of a model output to
#' changes in model parameters. Each plot displays the sensitivity of a set of
#' PK parameters for a single output path to changes in model parameters. The x-axis
#' represents the value of model parameters (absolute or percent from default value),
#' and the y-axis represents the sensitivity of the output to changes in the model
#' parameter.
#'
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
#' (linear scale), to set the x-axis scale. Default is "log".
#' @param yAxisScale Character string, either "log" or "lin", sets the y-axis scale
#' similarly to `xAxisScale`. Default is "lin".
#' @param xAxisType Character string, either "percent" (percentage change) or
#' "absolute" (absolute values) for PK parameter values, for x-axis data normalization.
#' Default is "percent".
#' @param yAxisType Character string, either "percent" (percentage change) or
#' "absolute" (absolute values) for PK parameter values, for y-axis data normalization.
#' Default is "percent".
#' @param yAxisFacetScales Character string, either "fixed" or "free", determines
#' the scaling across y-axes of different facets. Default is "fixed".
#' If "fixed", all facetes within one plot will have the same range, which allows
#' for easier comparison between different PK parameters. If "free", each facet
#' will have its own range, which allows for better visualization of the single
#' PK parameters sensitivity.
#' @param defaultPlotConfiguration An object of class `DefaultPlotConfiguration`
#' used to customize plot aesthetics. Plot-specific settings provided directly
#' to the function, such as `xAxisScale`, will take precedence over any
#' modifications in `defaultPlotConfiguration`.
#'
#' Supported parameters include:
#' - `legendPosition`: Position of the legend on the plot.
#' - `legendTitle`: Title displayed for the legend.
#' - `linesAlpha`: Alpha transparency for line elements.
#' - `linesColor`: Color of the line elements.
#' - `linesSize`: Thickness of the line elements.
#' - `pointsShape`: Shape of the point elements.
#' - `pointsSize`: Size of the point elements.
#' - `subtitle`: Subtitle text for the plot.
#' - `title`: Main title text for the plot.
#' - `titleSize`: Font size of the plot title.
#' - `xAxisScale`: Scale type for the x-axis (`"log"` or `"lin"`).
#' - `xLabel`: Label text for the x-axis.
#' - `xValuesLimits`: Numeric vector specifying the limits for x-values.
#' - `yAxisLimits`: Numeric vector specifying the limits for y-values.
#' - `yAxisScale`: Scale type for the y-axis (`"log"` or `"lin"`).
#' - `yAxisTicks`: Number of ticks on the y-axis.
#' - `yLabel`: Label text for the y-axis.
#' - `yValuesLimits`: Numeric vector specifying the limits for y-values.
#'
#' Default values are set to provide a standardized look, but each parameter
#' can be tailored to fit specific visual needs. Modifying these parameters
#' will directly affect the aesthetics of the output plots.
#'
#' @import ggplot2
#'
#' @family sensitivity-calculation
#'
#' @returns A `patchwork` object containing the combined ggplot objects if a
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
#'
#' # Use named parameter paths to customize legend labels
#' namedParameterPaths <- c(
#'   "Lipophilicity" = "Aciclovir|Lipophilicity",
#'   "Dose" = "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
#'   "GFR fraction" = "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR fraction"
#' )
#'
#' resultsNamed <- sensitivityCalculation(
#'   simulation = simulation,
#'   outputPaths = outputPaths,
#'   parameterPaths = namedParameterPaths
#' )
#'
#' sensitivitySpiderPlot(resultsNamed)
#' }
#'
#' @export
sensitivitySpiderPlot <- function(sensitivityCalculation,
                                  outputPaths = NULL,
                                  parameterPaths = NULL,
                                  pkParameters = NULL,
                                  xAxisScale = NULL,
                                  yAxisScale = NULL,
                                  xAxisType = "percent",
                                  yAxisType = "percent",
                                  yAxisFacetScales = "fixed",
                                  defaultPlotConfiguration = NULL) {
  # Input validation -------------------------------------

  validateIsOfType(sensitivityCalculation, "SensitivityCalculation")

  .validateCharVectors(outputPaths, nullAllowed = TRUE)
  .validateCharVectors(parameterPaths, nullAllowed = TRUE)
  .validateCharVectors(pkParameters, nullAllowed = TRUE)

  # Plot configuration setup -----------------------------

  spiderPlotConfiguration <- list(
    legendPosition = "bottom",
    legendTitle    = "Parameter",
    linesSize      = 1.4,
    pointsShape    = 21L,
    pointsSize     = 2,
    title          = NULL,
    titleSize      = 14,
    xAxisScale     = "log",
    xLabel         = NULL,
    yAxisScale     = "lin",
    yAxisTicks     = 10L,
    yLabel         = NULL
  )

  # apply configuration overrides and validate
  customPlotConfiguration <- .applyPlotConfiguration(
    defaultPlotConfiguration = defaultPlotConfiguration,
    plotOverrideConfig       = spiderPlotConfiguration,
    xAxisScale               = xAxisScale,
    yAxisScale               = yAxisScale
  )

  # validate separately as not supported by DefaultPlotConfiguration
  validateIsOption(
    list(
      yAxisFacetScales = yAxisFacetScales,
      xAxisType        = xAxisType,
      yAxisType        = yAxisType
    ),
    .getPlotConfigurationOptions(c("yAxisFacetScales", "xAxisType", "yAxisType"))
  )

  # Prepare data -----------------------------------------

  data <- sensitivityCalculation$pkData
  data <- .filterPlottingData(
    data              = data,
    outputPaths       = outputPaths,
    parameterPaths    = parameterPaths,
    pkParameters      = pkParameters
  )

  # set scales and parameterPath labels
  data <- dplyr::mutate(data,
    ParameterFactor = ParameterFactor * 100,
    PKPercentChange = PKPercentChange + 100,
    ParameterPathLabel = dplyr::coalesce(ParameterPathUserName, ParameterPath)
  )

  # Create list of plots ---------------------------------

  splitData <- split(data, data$OutputPath)
  lsPlots <- stats::setNames(
    vector("list", length(names(splitData))), names(splitData)
  )

  # create plot for each output path
  for (outputPath in names(splitData)) {
    lsPlots[[outputPath]] <- .createSpiderPlot(
      splitData[[outputPath]],
      xAxisType = xAxisType,
      yAxisType = yAxisType,
      yAxisFacetScales = yAxisFacetScales,
      defaultPlotConfiguration = customPlotConfiguration
    )
  }

  return(lsPlots)
}

#' @keywords internal
#' @noRd
.createSpiderPlot <- function(data,
                              xAxisType = "percent",
                              yAxisType = "percent",
                              yAxisFacetScales = "fixed",
                              defaultPlotConfiguration) {
  # update data dependent plot configuration
  plotConfiguration <- defaultPlotConfiguration$clone()
  plotConfiguration <- .updatePlotConfiguration(
    plotConfiguration,
    list(title = unique(data$OutputPath))
  )

  # select percent or absolute column for y-axis
  if (xAxisType == "percent") {
    xColumn <- sym("ParameterFactor")
  } else {
    xColumn <- sym("ParameterValue")
  }
  if (yAxisType == "percent") {
    yColumn <- sym("PKPercentChange")
    data$Unit <- "% of reference"
  } else {
    yColumn <- sym("PKParameterValue")
  }

  # filter according to axis value limits
  if (!is.null(plotConfiguration$yValuesLimits)) {
    data <- dplyr::filter(data, !!yColumn >= plotConfiguration$yValuesLimits[1])
    data <- dplyr::filter(data, !!yColumn <= plotConfiguration$yValuesLimits[2])
  }
  if (!is.null(plotConfiguration$xValuesLimits)) {
    data <- dplyr::filter(data, !!xColumn >= plotConfiguration$xValuesLimits[1])
    data <- dplyr::filter(data, !!xColumn <= plotConfiguration$xValuesLimits[2])
  }

  # loop through unique PKParameters
  pkParams <- unique(data$PKParameter)
  plotList <- stats::setNames(vector("list", length(pkParams)), pkParams)

  for (param in pkParams) {
    dataSubset <- dplyr::filter(data, PKParameter == param)
    baseDataSubset <- dplyr::filter(dataSubset, ParameterFactor == 100)

    # Set axis limits and labels -------------------------

    if (isTRUE(yAxisFacetScales == "fixed")) {
      # same label for all plots based on yAxisType
      if (is.null(plotConfiguration$yLabel)) {
        plotConfiguration$yLabel <- switch(yAxisType,
          "percent"  = "PK-Parameter value [% of reference]",
          "absolute" = "PK-Parameter value",
          "PK-Parameter value"
        )
      }
      # limits based on entire data
      pLimits <- .calculateLimits(unlist(data[, yColumn]))
    } else { # free y-axis scale
      plotConfiguration$yLabel <- paste0(
        dataSubset$PKParameter[1], " [", dataSubset$Unit[1], "]"
      )
      pLimits <- NULL
    }

    # x-axis label based on xAxisType
    plotConfiguration$xLabel <- plotConfiguration$xLabel %||%
      switch(xAxisType,
        "percent"  = "Input parameter value [% of reference]",
        "absolute" = "Input parameter value",
        "PK-Parameter value"
      )

    # override y-axis limits if specified in configuration
    if (!is.null(plotConfiguration$yAxisLimits)) {
      pLimits <- plotConfiguration$yAxisLimits
    }

    # Basic plot setup -----------------------------------

    plot <- ggplot(
      dataSubset,
      aes(x = .data[[xColumn]], y = .data[[yColumn]], group = ParameterPathLabel)
    ) +
      geom_line(
        aes(group = ParameterPathLabel, color = as.factor(ParameterPathLabel)),
        linewidth = plotConfiguration$linesSize,
        alpha = plotConfiguration$linesAlpha,
        na.rm = TRUE
      ) +
      geom_point(
        size  = plotConfiguration$pointsSize,
        shape = plotConfiguration$pointsShape[1],
        na.rm = TRUE
      )

    # adjusting axis scales
    if (isTRUE(plotConfiguration$xAxisScale == "log")) {
      plot <- plot + scale_x_log10()
    }

    if (isTRUE(plotConfiguration$yAxisScale == "log")) {
      plot <- plot +
        scale_y_log10(
          limits = pLimits,
          expand = expansion(mult = c(0.01, 0.1)),
          breaks = scales::breaks_log(
            n = plotConfiguration$yAxisTicks
          ),
          labels = scales::label_log()
        )
    } else {
      plot <- plot +
        scale_y_continuous(
          limits = pLimits,
          breaks = scales::breaks_extended(
            n = plotConfiguration$yAxisTicks
          ),
          labels = scales::label_number_auto()
        )
    }

    # initial parameter value reference marker
    plot <- plot +
      geom_hline(
        data = baseDataSubset,
        aes(yintercept = .data[[yColumn]]),
        linetype = "dotted",
        linewidth = 0.5,
        color = "black",
        na.rm = TRUE
      )

    # vertical line only for percent x-axis at 100
    if (xAxisType == "percent") {
      plot <- plot +
        geom_vline(
          xintercept = 100,
          linetype = "dotted",
          linewidth = 0.5,
          color = "black",
          na.rm = TRUE
        )
    }

    # Finalize plot --------------------------------------

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

    # apply color scales
    if (is.null(plotConfiguration$linesColor)) {
      plot <- plot + scale_color_brewer(palette = "Dark2")
    } else {
      plot <- plot + scale_color_manual(values = plotConfiguration$linesColor)
    }

    plotList[[param]] <- plot
  }

  # Compile individual plots -----------------------------

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
