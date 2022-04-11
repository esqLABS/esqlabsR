#' @name sensitivityTimeProfiles
#' @title Create a concentration-time profile plot
#'
#' @inheritParams sensitivitySpiderPlot
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
                                    xAxisLog = FALSE,
                                    yAxisLog = TRUE,
                                    palette = "Cold",
                                    savePlots = FALSE,
                                    width = NA,
                                    height = NA,
                                    dpi = 300) {
  # fail early if the object is of wrong type
  validateIsOfType(sensitivityCalculation, "SensitivityCalculation")

  # extract the needed dataframe from the object
  data <- .simulationResultsBatchToTimeSeriesDataFrame(
    simulationResultsBatch = sensitivityCalculation$simulationResults,
    parameterPaths         = sensitivityCalculation$parameterPaths,
    outputPaths            = sensitivityCalculation$outputPaths
  )

  # create plot for each output path
  ls_plots <- purrr::map(
    .x = data %>% split(.$OutputPath),
    .f = ~ .createTimeProfiles(
      .x,
      xAxisLog = xAxisLog,
      yAxisLog = yAxisLog,
      palette = palette
    )
  )

  if (savePlots) {
    .savePlotList(
      ls_plots,
      plot.type = "Profile_",
      height = height,
      width = width,
      dpi = dpi
    )
  }

  # print plots without producing warnings
  suppressWarnings(purrr::walk2(ls_plots, names(ls_plots), ~ printPlot(.x, .y)))
}

#' @keywords internal
#' @noRd
#'
#' @note
#'
#' **WARNING**: Although these are internal functions, DO NOT prepend them with
#' `.`. These are S3 methods and not regular functions.
#'
#' @references
#' Adapted from:
#' https://stackoverflow.com/questions/62558043/continuous-color-bar-with-separators-instead-of-ticks
guide_longticks <- function(...) {
  guide <- guide_colourbar(...)
  class(guide) <- c("guide", "guide_longticks", "colourbar", "colorbar")
  guide
}

#' @keywords internal
#' @noRd
guide_gengrob.guide_longticks <- function(guide, theme) {
  dir <- guide$direction
  guide <- NextMethod()
  is_ticks <- grep("^ticks$", guide$layout$name)
  ticks <- guide$grobs[is_ticks][[1]]
  if (dir == "vertical") {
    ticks$x1 <- rep(tail(ticks$x1, 1), length(ticks$x1))
  } else {
    ticks$y1 <- rep(tail(ticks$y1, 1), length(ticks$y1))
  }

  guide$grobs[[is_ticks]] <- ticks
  guide
}


#' @keywords internal
#' @noRd
.createTimeProfiles <- function(data, xAxisLog = FALSE, yAxisLog = TRUE, palette = NULL) {
  plot <- ggplot() +
    geom_line(
      data = dplyr::filter(data, ParameterFactor != 1.0),
      aes(Time, Concentration, group = ParameterFactor, color = ParameterFactor),
      alpha = 0.5,
      na.rm = TRUE
    ) +
    colorspace::scale_color_continuous_qualitative(
      palette = palette,
      breaks = c(min(data$ParameterFactor), 1, max(data$ParameterFactor)),
      limits = c(min(data$ParameterFactor), max(data$ParameterFactor))
    ) +
    geom_line(
      data = dplyr::filter(data, ParameterFactor == 1.0),
      aes(Time, Concentration),
      color = "black",
      na.rm = TRUE
    ) +
    facet_wrap(~ParameterPath, labeller = label_wrap_gen(width = 0)) +
    theme_bw(base_size = 10) +
    labs(
      x = paste0(unique(data$TimeDimension), " [", unique(data$TimeUnit), "]"),
      y = paste0(unique(data$Dimension), " [", unique(data$Unit), "]"),
      title = unique(data$OutputPath),
      color = "Parameter factor"
    )

  if (xAxisLog) {
    plot <- plot + scale_x_log10()
  }

  if (yAxisLog) {
    plot <- plot +
      scale_y_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      )
  }

  plot +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    guides(colour = guide_longticks(
      ticks = TRUE,
      ticks.linewidth = 0.8,
      ticks.colour = "black",
      draw.ulim = FALSE,
      draw.llim = FALSE
    ))
}

#' @keywords internal
#' @noRd
printPlot <- function(plot, pathName) {
  print(paste0("Creating plot for path: ", pathName))
  print(plot)
}
