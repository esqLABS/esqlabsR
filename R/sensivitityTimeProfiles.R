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
                                    xAxisLog = FALSE,
                                    yAxisLog = TRUE,
                                    palette = "Cold",
                                    savePlots = FALSE,
                                    outputFolder = "",
                                    width = 16,
                                    height = 9,
                                    dpi = 300) {
  # input validation ------------------------

  # fail early if the object is of wrong type
  validateIsOfType(sensitivityCalculation, "SensitivityCalculation")
  validateIsCharacter(outputFolder, nullAllowed = FALSE)

  # validate vector arguments of character type
  .validateCharVectors(outputPaths)
  .validateCharVectors(parameterPaths)

  # filter data ------------------------

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

  # list of plots ------------------------

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

  # if needed, save plots with given specs
  if (savePlots) {
    .savePlotList(
      ls_plots,
      plot.type = "Profile_",
      outputFolder = outputFolder,
      height = height,
      width = width,
      dpi = dpi
    )

    return(invisible())
  }

  # print plots without producing warnings
  suppressWarnings(purrr::walk2(ls_plots, names(ls_plots), ~ printPlot(.x, .y)))
}


#' @keywords internal
#' @noRd
.createTimeProfiles <- function(data,
                                xAxisLog = FALSE,
                                yAxisLog = TRUE,
                                palette = NULL) {
  plot <- ggplot() +
    geom_line(
      data = dplyr::filter(data, ParameterFactor != 1.0),
      aes(Time, Concentration, group = ParameterFactor, color = ParameterFactor),
      size = 1.4,
      alpha = 0.7,
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
      size = 1.4,
      alpha = 0.7,
      na.rm = TRUE
    ) +
    facet_wrap(~ParameterPath, labeller = label_wrap_gen(width = 0)) +
    theme_bw(base_size = 9) +
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
    plot <- plot + scale_y_log10()
  }

  plot +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    guides(colour = guide_colourbar(
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
