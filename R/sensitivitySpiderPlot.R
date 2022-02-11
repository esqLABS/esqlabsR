#' @name sensitivitySpiderPlot
#' @title Spider plot for PK parameters
#'
#' @param data The `pkData` dataframe in a list of dataframes returned by
#'   `sensitivityCalculation()`.
#'
#' @import ggplot2
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
#' ls_results <- sensitivityCalculation(
#'   simulation = simulation,
#'   outputPaths = outputPaths,
#'   parameterPaths = parameterPaths
#' )
#'
#' # PK parameters data
#' pkData <- ls_results$pkData
#'
#' # generate profile plot for a given output path
#' ls_spider_plots <- purrr::map(
#'   .x = pkData %>% split(.$OutputPath),
#'   .f = ~ sensitivitySpiderPlot(.x)
#' )
#'
#' @seealso savePlotList
#'
#' @export

sensitivitySpiderPlot <- function(data) {
  data <- dplyr::mutate(data,
    ParameterFactor = ParameterFactor * 100,
    PercentChangePK = PercentChangePK + 100
  )

  ggplot(
    data,
    aes(x = ParameterFactor, y = PercentChangePK, group = ParameterPath)
  ) +
    geom_line(
      aes(group = ParameterPath, color = as.factor(ParameterPath)),
      size = 1.2,
      alpha = 0.8
    ) +
    geom_point(size = 2, shape = 21) +
    scale_x_log10() +
    scale_y_continuous(
      breaks = seq(0, max(data$PercentChangePK) + 100, 100),
      labels = as.character(seq(0, max(data$PercentChangePK) + 100, 100))
    ) +
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
    theme_bw(base_size = 14) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    guides(col = guide_legend(nrow = 3)) +
    scale_color_brewer(palette = "Dark2")
}
