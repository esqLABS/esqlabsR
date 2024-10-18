#' @name sensitivityTornadoPlot
#' @title Tornado Plot for Sensitivity Analysis
#'
#' @description
#' Generates tornado plots to visualize the results of sensitivity analysis.
#' Each plot shows the effect of modifying parameters by a specific scaling factor
#' (`parameterFactor`) and its reciprocal on specific model outputs. This
#' visualization helps to assess the impact of parameter changes on the results,
#' highlighting the model's sensitivity to these parameters.
#'
#' @param sensitivityCalculation The `SensitivityCalculation` object returned by
#' `sensitivityCalculation()`.
#' @param outputPaths,parameterPaths,pkParameters A single or a vector of the
#' output path(s), parameter path(s), and PK parameters to be displayed,
#' respectively. If `NULL`, all included paths and parameters present in the
#' supplied `SensitivityCalculation` object will be displayed in the
#' visualization.
#' A separate plot will be generated for each output path. Each plot will
#' contain a tornado plot panel for each PK parameter, and the sensitivities
#' for each parameter will be displayed as lines.
#' @param parameterFactor Numeric; the scaling factor used to adjust parameters
#' during sensitivity analysis used in the tornado plot. Both the `parameterFactor`
#' and its reciprocal (`1/parameterFactor`) must be included in the
#' `variationRange` specified in the `sensitivityCalculation`. Default is 0.1.
#' @param defaultPlotConfiguration An object of class `DefaultPlotConfiguration`
#' used to customize plot aesthetics.
#'
#' Supported parameters include:
#' - `legendPosition`: Position of the legend on the plot.
#' - `legendTitle`: Title displayed for the legend.
#' - `linesColor`: Color of the bar elements.
#' - `subtitle`: Subtitle text for the plot.
#' - `title`: Main title text for the plot.
#' - `titleSize`: Font size of the plot title.
#' - `xLabel`: Label text for the x-axis.
#' - `yLabel`: Label text for the y-axis.
#'
#' Default values are set to provide a standardized look, but each parameter can
#' be tailored to fit specific visual needs. Modifying these parameters will
#' directly affect the aesthetics of the output plots.
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
#'   parameterPaths = parameterPaths,
#'   variationRange = c(seq(0.1, 1, by = 0.1), seq(2, 10, by = 1)),
#' )
#'
#' # Print plots with default settings
#' sensitivityTornadoPlot(results)
#'
#' # Print plots with specific parameter scaling factor
#' sensitivityTornadoPlot(results, parameterFactor = 0.5)
#'
#' # Print plots with custom configuration settings
#' myPlotConfiguration <- createEsqlabsPlotConfiguration()
#' myPlotConfiguration$legendPosition <- "bottom"
#' myPlotConfiguration$subtitle <- "Custom settings"
#' sensitivityTornadoPlot(results, defaultPlotConfiguration = myPlotConfiguration)
#' }
#'
#' @export
sensitivityTornadoPlot <- function(sensitivityCalculation,
                                   outputPaths = NULL,
                                   parameterPaths = NULL,
                                   pkParameters = NULL,
                                   parameterFactor = 0.1,
                                   defaultPlotConfiguration = NULL) {
  # input validation -------------------------

  # fail early if the object is of wrong type
  validateIsOfType(sensitivityCalculation, "SensitivityCalculation")
  ospsuite.utils::validateIsOption(
    list(parameterFactor = parameterFactor),
    .getPlotConfigurationOptions("parameterFactor")
  )

  # validate vector arguments of character type
  .validateCharVectors(outputPaths, nullAllowed = TRUE)
  .validateCharVectors(parameterPaths, nullAllowed = TRUE)
  .validateCharVectors(pkParameters, nullAllowed = TRUE)

  data <- sensitivityCalculation$pkData

  # validate data contains required parameterFactor results
  parameterFactors <- c(parameterFactor, 1 / parameterFactor)
  if (!all(parameterFactors %in% data$ParameterFactor)) {
    stop(messages$noParameterFactor(data, parameterFactor))
  }

  # plot configuration setup ------------

  # default tornado plot configuration
  tornadoPlotConfiguration <- list(
    legendPosition = "right",
    legendTitle = "Parameter Factor",
    subtitle = NULL,
    title = NULL,
    titleSize = 14,
    yLabel = "Parameter",
    xLabel = "Input parameter value [% of reference]"
  )

  # apply configuration overrides and validate
  customPlotConfiguration <- .applyPlotConfiguration(
    defaultPlotConfiguration = defaultPlotConfiguration,
    plotOverrideConfig       = tornadoPlotConfiguration
  )

  # extract and prepare data -----------------

  data <- .filterPlottingData(
    data,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    pkParameters = pkParameters
  )

  # ordered levels for parameter plotting
  data <- dplyr::group_by(
    data,
    OutputPath, ParameterPath, PKParameter, ParameterFactor
  )
  data <- dplyr::mutate(
    data,
    PKMeanPercentChange = mean(PKPercentChange, na.rm = TRUE)
  )
  data <- dplyr::ungroup(data)
  data <- dplyr::arrange(data, dplyr::desc(abs(PKMeanPercentChange)))
  data <- dplyr::mutate(
    data,
    ParameterPath = purrr::map_chr(ParameterPath, .splitParameterName)
  )
  data$ParameterPath <- factor(data$ParameterPath,
    levels = rev(unique(data$ParameterPath))
  )

  data <- dplyr::filter(
    data,
    ParameterFactor %in% c(parameterFactor, 1 / parameterFactor)
  )

  # list of plots ----------------------------

  splitData <- split(data, data$OutputPath)
  lsPlots <- setNames(
    vector("list", length(names(splitData))), names(splitData)
  )

  # create plot for each output path
  for (outputPath in names(splitData)) {
    lsPlots[[outputPath]] <- .createTornadoPlot(
      splitData[[outputPath]],
      defaultPlotConfiguration = customPlotConfiguration
    )
  }

  return(lsPlots)
}

#' @keywords internal
#' @noRd
.createTornadoPlot <- function(data,
                               defaultPlotConfiguration) {
  # update data dependent plot configuration
  plotConfiguration <- defaultPlotConfiguration$clone()
  plotConfiguration <- .updatePlotConfiguration(
    plotConfiguration,
    list(title = unique(data$OutputPath))
  )

  # calculate x-axis limits  -------
  pLimits <- .calculateLimits(data$PKPercentChange)
  pLimits[1] <- -1 * max(abs(pLimits))
  pLimits[2] <- max(abs(pLimits))

  # Loop through each unique PKParameter -------------------
  pkParams <- unique(data$PKParameter)
  plotList <- setNames(vector("list", length(pkParams)), pkParams)

  for (param in pkParams) {
    dataSubset <- dplyr::filter(data, PKParameter == param)

    # basic plot setup -------------------------

    plot <- ggplot(
      dataSubset,
      aes(
        x = ParameterPath,
        y = PKPercentChange,
        fill = as.factor(ParameterFactor)
      )
    ) +
      geom_col(
        color = "grey",
        width = 0.9,
        na.rm = TRUE
      ) +
      coord_flip()

    plot <- plot +
      geom_hline(
        yintercept = 0,
        color = "grey",
        linewidth = 1
      )

    plot <- plot +
      scale_y_continuous(
        limits = pLimits,
        breaks = scales::breaks_extended(),
        labels = scales::label_number_auto()
      )

    # finalize plot ----------------------------

    # note: facet wrap on unique PK parameter to obtain facet titles
    plot <- plot +
      facet_wrap(~PKParameter, scales = "fixed") +
      labs(
        x = plotConfiguration$yLabel, # x/y label swap because of coord-flip()
        y = plotConfiguration$xLabel,
        title = NULL,
        fill = plotConfiguration$legendTitle
      ) +
      theme_light(
        base_size = 11
      ) +
      theme(
        legend.position = plotConfiguration$legendPosition,
        panel.grid.minor = element_blank(),
        text = element_text(size = 11),
        axis.text.y = element_text(margin = margin(l = 20, unit = "pt"))
      )

    # apply color scales
    if (is.null(plotConfiguration$linesColor)) {
      plot <- plot + scale_fill_brewer(palette = "Set2")
    } else {
      pLevels <- levels(as.factor(data$ParameterFactor))
      pColor <- plotConfiguration$linesColor[1:length(pLevels)]
      names(pColor) <- pLevels
      plot <- plot + scale_fill_manual(
        values = colorspace::lighten(pColor, amount = 0.2)
      )
    }

    plotList[[param]] <- plot
  }


  # compile individual plots -----------------

  plotPatchwork <- patchwork::wrap_plots(plotList, ncol = 1) +
    patchwork::plot_annotation(
      title = plotConfiguration$title,
      subtitle = plotConfiguration$subtitle,
      theme = theme(
        plot.title = element_text(size = plotConfiguration$titleSize)
      )
    ) +
    patchwork::plot_layout(
      guides = "collect", axes = "collect", ncol = 1
    ) &
    theme(legend.position = plotConfiguration$legendPosition)

  return(plotPatchwork)
}

#' @keywords internal
#' @noRd
.splitParameterName <- function(x, equalLines = FALSE) {
  xBreak <- x
  if (!is.null(x)) {
    n <- stringr::str_count(x, stringr::fixed("|"))
    if (isTRUE(n >= 3)) {
      xBreak <- sub("((?:[^|]*\\|){2}[^|]*)\\|", "\\1|\n", x)
    } else if (equalLines) {
      xBreak <- paste0(xBreak, "\n")
    }
  }

  return(xBreak)
}
