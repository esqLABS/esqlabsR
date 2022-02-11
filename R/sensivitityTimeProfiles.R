#' @name sensivitityTimeProfiles
#' @title Create a Concentration-time profile plot
#'
#' @param data The `pkData` dataframe in a list of dataframes returned by
#'   `sensitivityCalculation()`.
#' @param logConcentration A logical indicating if the concentration should be
#'   plotted on a log scale (Default: `TRUE`).
#'
#' @import ggplot2
#'
#' @examples
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
#'
#' # extract the results into a list of dataframes
#' ls_results <- sensitivityCalculation(
#'   simulation = simulation,
#'   outputPaths = outputPaths,
#'   parameterPaths = parameterPaths
#' )
#'
#' # time-series data
#' tsData <- ls_results$tsData
#'
#' # generate profile plot for each output path
#' ls_profile_plots <- purrr::map(
#'   .x = tsData %>% split(.$OutputPath),
#'   .f = ~ sensivitityTimeProfiles(.x, logConcentration = TRUE)
#' )
#'
#' @seealso savePlotList
#' @export

sensivitityTimeProfiles <- function(data, logConcentration = TRUE) {
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
    facet_wrap(~ParameterPath) +
    theme_bw(base_size = 14) +
    labs(
      x = paste0(unique(tsData$TimeDimension), " [", unique(tsData$TimeUnit), "]"),
      y = paste0("Concentration [", unique(tsData$unit), "]"),
      title = unique(data$OutputPath),
      color = "Parameter factor"
    )

  if (logConcentration) {
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
