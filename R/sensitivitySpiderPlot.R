#' @name sensitivitySpiderPlot
#' @title Sensitivity spider plot for PK parameters
#'
#' @param sensitivityCalculation The `SensitivityCalculation` object returned by
#'   `esqlabsR::sensitivityCalculation()`.
#' @param xAxisLog,yAxisLog Logical that decides whether to display the axis on
#'   logarithmic scale.
#' @param savePlots Logical that decides whether you wish to save created
#'   plot(s). They are not saved by default. Note that if there are multiple
#'   output paths in your model, there will be multiple plots that will be saved.
#' @inheritParams ggplot2::ggsave
#'
#' @import ggplot2
#'
#' @return
#'
#' A list of dataframes with time-series and PK parameters data.
#'
#' This function also prints Concentration-time profile plots and Sensitivity
#' spider plots.
#'
#' @examples
#'
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
#'     units = "in",
#'     height = 6,
#'     width = 12
#'   )
#' }
#'
#' @export
sensitivitySpiderPlot <- function(sensitivityCalculation,
                                  xAxisLog = TRUE,
                                  yAxisLog = FALSE,
                                  savePlots = FALSE,
                                  width = NA,
                                  height = NA,
                                  units = c("in", "cm", "mm", "px"),
                                  dpi = 300) {
  # fail early if the object is of wrong type
  validateIsOfType(sensitivityCalculation, "SensitivityCalculation")

  # extrat the needed dataframe from the object
  data <- sensitivityCalculation$pkData

  # create plot for each output path
  ls_spider_plots <- purrr::map(
    .x = data %>% split(.$OutputPath),
    .f = ~ .createSpiderPlot(
      .x,
      xAxisLog = xAxisLog,
      yAxisLog = yAxisLog
    )
  )

  if (savePlots) {
    .savePlotList(
      ls_spider_plots,
      plot.type = "Spider_",
      height = height,
      width = width,
      units = units,
      dpi = dpi
    )
  }

  # return plots
  ls_spider_plots
}

#' @noRd

.createSpiderPlot <- function(data, xAxisLog = TRUE, yAxisLog = FALSE) {
  data <- dplyr::mutate(data,
    ParameterFactor = ParameterFactor * 100,
    PercentChangePK = PercentChangePK + 100
  )

  plot <- ggplot(
    data,
    aes(x = ParameterFactor, y = PercentChangePK, group = ParameterPath)
  ) +
    geom_line(
      aes(group = ParameterPath, color = as.factor(ParameterPath)),
      size = 1.2,
      alpha = 0.8
    ) +
    geom_point(size = 2, shape = 21)

  if (xAxisLog) {
    plot <- plot + scale_x_log10()
  }

  if (yAxisLog) {
    plot <- plot + scale_y_log10()
  } else {
    plot <- plot +
      scale_y_continuous(
        breaks = seq(0, max(data$PercentChangePK) + 100, 100),
        labels = as.character(seq(0, max(data$PercentChangePK) + 100, 100))
      )
  }

  plot <- plot +
    geom_hline(
      yintercept = 100,
      linetype = "dotted",
      color = "black",
      size = 0.5
    ) +
    geom_vline(
      xintercept = 100,
      linetype = "dotted",
      color = "black",
      size = 0.5
    ) +
    facet_wrap(~PKParameter, scales = "free_y") +
    labs(
      x = "Input parameter value [% of reference]",
      y = paste0("PK-parameter value [% of reference]"),
      title = unique(data$OutputPath),
      group = "Parameter",
      color = "Parameter"
    ) +
    theme_bw(base_size = 10) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    guides(col = guide_legend(nrow = 3)) +
    scale_color_brewer(palette = "Dark2")

  plot
}
