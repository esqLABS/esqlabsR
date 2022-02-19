#' @name sensitivityTimeProfiles
#' @title Create a Concentration-time profile plot
#'
#' @inheritParams sensitivitySpiderPlot
#'
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#'
#' library(ospsuite)
#'
#' simPath <- system.file("extdata", "Aciclovir.pkml", package = "esqlabsR")
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
#'     units = "in",
#'     height = 6,
#'     width = 12
#'   )
#' }
#'
#' @export

sensitivityTimeProfiles <- function(sensitivityAnalysis,
                                    xAxisLog = FALSE,
                                    yAxisLog = TRUE,
                                    savePlots = FALSE,
                                    width = NA,
                                    height = NA,
                                    units = c("in", "cm", "mm", "px"),
                                    dpi = 300) {
  # fail early if the object is of wrong type
  validateIsOfType(sensitivityAnalysis, "SensitivityAnalysis")

  # extrat the needed dataframe from the object
  data <- sensitivityAnalysis$tsData

  # create plot for each output path
  ls_profile_plots <- purrr::map(
    .x = data %>% split(.$OutputPath),
    .f = ~ .createTimeProfiles(
      .x,
      xAxisLog = xAxisLog,
      yAxisLog = yAxisLog
    )
  )

  # print plots
  purrr::walk(ls_profile_plots, plot)

  if (savePlots) {
    .savePlotList(
      ls_profile_plots,
      plot.type = "Profile_",
      height = height,
      width = width,
      units = units,
      dpi = dpi
    )
  }
}


#' @noRd

.createTimeProfiles <- function(data, xAxisLog = FALSE, yAxisLog = TRUE) {
  plot <- ggplot() +
    geom_line(
      data = dplyr::filter(data, ParameterFactor != 1.0),
      aes(Time, Concentration, group = ParameterFactor, color = ParameterFactor),
      alpha = 0.5
    ) +
    colorspace::scale_color_continuous_qualitative(
      breaks = c(min(data$ParameterFactor), max(data$ParameterFactor))
    ) +
    geom_line(
      data = dplyr::filter(data, ParameterFactor == 1.0),
      aes(Time, Concentration),
      color = "black"
    ) +
    facet_wrap(~ParameterPath, labeller = label_wrap_gen(width = 15)) +
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
    )
}
