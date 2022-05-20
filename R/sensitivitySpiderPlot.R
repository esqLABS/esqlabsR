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
#' @param xAxisLog,yAxisLog Logical that decides whether to display the axis on
#'   logarithmic scale.
#' @param savePlots Logical that decides whether you wish to save created
#'   plot(s). They are not saved by default. Note that if there are multiple
#'   output paths in your model, there will be multiple plots that will be
#'   saved.
#' @param outputFolder A character string describing path folder where plots
#'   need to be saved. The path must be relative to the current working
#'   directory. By default (`""`), the plots will be saved in the current
#'   working directory (which can be found using `getwd()` function). This
#'   parameter is relevant only if `savePlots` is set to `TRUE`.
#' @param width,height Plot size in inches. If not supplied, uses the size of
#'   current graphics device.
#' @inheritParams ggplot2::ggsave
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
                                  xAxisLog = TRUE,
                                  yAxisLog = FALSE,
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
  .validateCharVectors(pkParameters)

  # filter data ------------------------

  # extrat the needed dataframe from the object
  data <- sensitivityCalculation$pkData

  # filter out data not needed for plotting
  data <- .filterPlottingData(
    data,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    pkParameters = pkParameters
  )

  # list of plots ------------------------

  # create plot for each output path
  ls_plots <- purrr::map(
    .x = data %>% split(.$OutputPath),
    .f = ~ .createSpiderPlot(
      .x,
      xAxisLog = xAxisLog,
      yAxisLog = yAxisLog
    )
  )

  # if needed, save plots with given specs
  if (savePlots) {
    .savePlotList(
      ls_plots,
      plot.type = "Spider_",
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
.createSpiderPlot <- function(data,
                              xAxisLog = TRUE,
                              yAxisLog = FALSE) {
  # getting the scales right
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
      size = 1.4,
      alpha = 0.7,
      na.rm = TRUE
    ) +
    geom_point(size = 2, shape = 21, na.rm = TRUE)

  if (xAxisLog) {
    plot <- plot + scale_x_log10()
  }

  if (yAxisLog) {
    plot <- plot + scale_y_log10()
  } else {
    # how many labels on the Y-axis?
    n_breaks <- labeling::extended(
      min(na.omit(data$PercentChangePK)),
      max(na.omit(data$PercentChangePK)),
      m = 10L
    )

    # there always needs to be a label at `y = 100`
    plot <- plot +
      scale_y_continuous(
        breaks = c(100, n_breaks),
        minor_breaks = n_breaks
      )
  }

  plot <- plot +
    geom_hline(
      yintercept = 100,
      linetype = "dotted",
      color = "black",
      size = 0.5,
      na.rm = TRUE
    ) +
    geom_vline(
      xintercept = 100,
      linetype = "dotted",
      color = "black",
      size = 0.5,
      na.rm = TRUE
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
