#' @name sensitivitySpiderPlot
#' @title Sensitivity spider plot for PK parameters
#'
#' @param sensitivityCalculation The `SensitivityCalculation` object returned by
#'   `sensitivityCalculation()`.
#' @param outputPaths,parameterPaths,pkParameters A single or a vector of the
#'   output path(s), parameter path(s), and PK parameters to be displayed,
#'   respectively. If `NULL`, all included paths and parameters present in the
#'   supplied `SensitivityCalculation` object will be displayed in the
#'   visualization.
#' @param yAxisType Character, either "percent" or "absolute", specifying the
#' type of data for the y-axis.
#' @param xAxisScale Character, either "log" or "lin", specifying whether to set
#' the x-axis scale logarithmically or linearly.
#' @param yAxisScale Character, either "log" or "lin", specifying whether to set
#' the y-axis scale logarithmically or linearly.
#' @param yAxisFacetScales Character, either "fixed" or "free", managing scaling
#' across different facets for the y-axis.
#'
#' @import ggplot2
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
#' sensitivitySpiderPlot(results)
#'
#' # print and save plots
#' if (FALSE) {
#'   sensitivitySpiderPlot(
#'     results,
#'     savePlots = TRUE,
#'     height = 6,
#'     width = 12
#'   )
#' }
#' }
#' @export
sensitivitySpiderPlot <- function(sensitivityCalculation,
                                  outputPaths = NULL,
                                  parameterPaths = NULL,
                                  pkParameters = NULL,
                                  yAxisType  = "percent",
                                  xAxisScale = "log",
                                  yAxisScale = "lin",
                                  yAxisFacetScales = "fixed",
                                  defaultPlotConfiguration = NULL) {
  # input validation ------------------------

  # fail early if the object is of wrong type
  validateIsOfType(sensitivityCalculation, "SensitivityCalculation")
  if (is.null(defaultPlotConfiguration)) {
    defaultPlotConfiguration <- esqlabsR::createEsqlabsPlotConfiguration()
  } else {
    validateIsOfType(defaultPlotConfiguration, "DefaultPlotConfiguration")
  }

  # validate vector arguments of character type
  .validateCharVectors(outputPaths)
  .validateCharVectors(parameterPaths)
  .validateCharVectors(pkParameters)

  #validate these!!
  #yAxisType
  #xAxisScale
  #yAxisScale
  #yAxisFacetScales

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
                              yAxisType  = "percent",
                              xAxisScale = "log",
                              yAxisScale = "lin",
                              yAxisFacetScales = "fixed",
                              defaultPlotConfiguration) {

  # override default configuration with settings for spider plot
  spiderPlotConfiguration <- list(
    title          = unique(data$OutputPath),
    legendPosition = "bottom",
    linesSize      = 1.4,
    pointsSize     = 2,
    pointsShape    = 21,
    pointsSize     = 2,
    xAxisScale     = xAxisScale,
    yAxisScale     = yAxisScale,
    yAxisTicks     = 10L
  )
  plotConfiguration <- .updatePlotConfiguration(
    defaultPlotConfiguration, spiderPlotConfiguration)

  # getting the scales right
  data <- dplyr::mutate(data,
    ParameterFactor = ParameterFactor * 100,
    PercentChangePK = PercentChangePK + 100
  )

  plotList <- purrr::map(
    unique(data$PKParameter),
    ~ {
      dataSubset <- dplyr::filter(data, PKParameter == .x)
      baseDataSubset <- dplyr::filter(dataSubset, ParameterFactor == 100)

      if(yAxisType == "percent") {
        yColumn <- sym("PercentChangePK")
        dataSubset$Unit <- "% of reference"
      } else {
        yColumn <- sym("PKParameterValue")
      }

      if (isTRUE(yAxisFacetScales == "fixed")) {
        if (yAxisType == "percent") {
          plotConfiguration$yLabel <- "PK-Parameter value [% of reference]"
        } else {
          plotConfiguration$yLabel <- "PK-Parameter value"
        }
        pBreaks <- .calculateBreaks(data[, yColumn],
                                    m = plotConfiguration$yAxisTicks,
                                    Q = c(100, 200, 300))
        pLimits <- .calculateLimits(data[, yColumn])

      } else {
        plotConfiguration$yLabel <- paste0(
          dataSubset$PKParameter[1], " [", dataSubset$Unit[1], "]")
        pBreaks <- .calculateBreaks(values = dataSubset[, yColumn],
                                    m = plotConfiguration$yAxisTicks,
                                    Q = c(100, 200, 300))
        pLimits <- NULL
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
          size = plotConfiguration$pointsSize,
          shape = plotConfiguration$pointsShape[1],
          na.rm = TRUE)

      if (isTRUE(plotConfiguration$xAxisScale == "log")) {
        plot <- plot + scale_x_log10()
      }

      if (isTRUE(plotConfiguration$yAxisScale == "log")) {
        plot <- plot + scale_y_log10(
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
        facet_wrap(~PKParameter, scales = yAxisFacetScales) + #free_y
        labs(
          x = plotConfiguration$xLabel,
          y = plotConfiguration$yLabel,
          title = plotConfiguration$title,
          group = "Parameter",
          color = "Parameter"
        ) +
        theme_bw(base_size = 14) +
        theme(
          legend.position = plotConfiguration$legendPosition,
          panel.grid.minor = element_blank(),
          text = element_text(size = 16) #,  family="mono"
        ) +
        guides(col = guide_legend(nrow = 3, title.position = "top")) +
        scale_color_brewer(palette = "Dark2")

      return(plot)
    }
  )

  plotPatchwork <- patchwork::wrap_plots(plotList) +
    patchwork::plot_annotation(
      title = plotConfiguration$title,
      theme = theme(plot.title = element_text(size = 20))) +
    patchwork::plot_layout(
      guide = "collect", axes = "collect", ncol = length(plotList)
    ) &
    theme(legend.position = plotConfiguration$legendPosition)

  return(plotPatchwork)
}

#' @keywords internal
#' @noRd
.updatePlotConfiguration <- function(plotConfiguration, plotOverrideConfig) {

  defaultValues <- esqlabsR::createEsqlabsPlotConfiguration()

  for (name in names(plotOverrideConfig)) {
    print(name)
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
  limits <- c((if (min(x, na.rm = TRUE) < 0) 1.01 else 0.99) * min(x),
              (if (max(x, na.rm = TRUE) > 0) 1.01 else 0.99) * max(x))

  return(limits)
}

