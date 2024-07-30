#' @name sensitivityTimeProfiles
#' @title Time Profile plots for Sensitivity Analysis
#'
#' @description
#' Creates time profiles for selected outputs generated in a sensitivity analysis.
#' This function plots time profiles for each specified output path,
#' illustrating the dynamics of model outputs to parameter variations.
#'
#' @param sensitivityCalculation The `SensitivityCalculation` object returned by
#' `sensitivityCalculation()`.
#' @param outputPaths,parameterPaths A single or a vector of the
#' output path(s) to be plotted for parameter path(s) which impact is analyzed,
#' respectively. If `NULL`, all included paths and parameters present in the
#' supplied `SensitivityCalculation` object will be displayed in the
#' visualization.
#' A separate plot will be generated for each output path, and a separate curve
#' will be generated for each parameter variation. A separate panel is created
#' for each varied parameter.
#' @param xAxisScale Character string, either "log" (logarithmic scale) or "lin"
#' (linear scale), to set the x-axis scale. Default is "lin".
#' @param yAxisScale Character string, either "log" or "lin", sets the y-axis
#' scale similarly to `xAxisScale`. Default is "log".
#' @param observedData Optional. A set of `DataSet` objects containing observed
#' data. If provided, observed data will be plotted together with the simulated data
#' based on `OutputPath` dimension for direct comparison within the visualizations.
#' Will be added only to plots with matching y dimension.
#' @param defaultPlotConfiguration An object of class `DefaultPlotConfiguration`
#' used to customize plot aesthetics. Plot-specific settings provided directly
#' to the function, such as `xAxisScale`, will take precedence over any
#' modifications in `defaultPlotConfiguration`. If not provided, default
#' settings are applied.
#'
#' Supported parameters for `defaultPlotConfiguration` include:
#' - `legendPosition`: Specifies the position of the plot legend.
#' - `legendTitle`: Sets the title displayed for the legend.
#' - `linesAlpha`: Alpha transparency for the line elements.
#' - `linesColor`: Color of the line elements.
#' - `linesSize`: Thickness of the line elements.
#' - `pointsShape`: Shape of the point elements for observed data.
#' - `title`: Main title text for the plot.
#' - `titleSize`: Font size of the plot title.
#' - `xAxisScale`: Scale type for the x-axis (`"log"` or `"lin"`).
#' - `xLabel`: Label text for the x-axis.
#' - `yAxisScale`: Scale type for the y-axis (`"log"` or `"lin"`).
#' - `yLabel`: Label text for the y-axis.
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
#'   parameterPaths = parameterPaths
#' )
#'
#' # Print plots with default settings
#' sensitivityTimeProfiles(results)
#'
#' # Print plots with linear y-axis values
#' sensitivityTimeProfiles(results, yAxisScale = "lin")
#'
#' # Print plots with custom configuration settings
#' myPlotConfiguration <- createEsqlabsPlotConfiguration()
#' myPlotConfiguration$linesColor <- c("#4D8076", "#C34A36")
#' myPlotConfiguration$subtitle <- "Custom settings"
#' sensitivityTimeProfiles(results, defaultPlotConfiguration = myPlotConfiguration)
#' }
#' @export
sensitivityTimeProfiles <- function(sensitivityCalculation,
                                    outputPaths = NULL,
                                    parameterPaths = NULL,
                                    xAxisScale = NULL,
                                    yAxisScale = NULL,
                                    observedData = NULL,
                                    defaultPlotConfiguration = NULL) {
  # input validation ------------------------

  # fail early if the object is of wrong type
  validateIsOfType(sensitivityCalculation, "SensitivityCalculation")
  validateIsOfType(observedData, DataSet, nullAllowed = TRUE)
  if (is.null(defaultPlotConfiguration)) {
    defaultPlotConfiguration <- createEsqlabsPlotConfiguration()
  } else {
    validateIsOfType(defaultPlotConfiguration, "DefaultPlotConfiguration")
  }

  # validate vector arguments of character type
  .validateCharVectors(outputPaths)
  .validateCharVectors(parameterPaths)

  # default time profiles plot configuration setup ----
  timeProfilesConfiguration <- list(
    legendPosition = "bottom",
    legendTitle = "Parameter factor",
    linesAlpha = 0.7,
    linesSize = 1.4,
    # pointsShape = NULL,
    title = NULL,
    titleSize = 14,
    xAxisScale = "lin",
    xLabel = NULL,
    yAxisScale = "log",
    yLabel = NULL
  )

  # override default plot configuration with function parameters
  customPlotConfiguration <- defaultPlotConfiguration$clone()
  if (!is.null(xAxisScale)) customPlotConfiguration$xAxisScale <- xAxisScale
  if (!is.null(yAxisScale)) customPlotConfiguration$yAxisScale <- yAxisScale

  # override only default configuration values with settings for spider plot
  customPlotConfiguration <- .updatePlotConfiguration(
    customPlotConfiguration, timeProfilesConfiguration
  )

  # validate plot configuration for valid options
  plotConfigurationList <- purrr::map(
    purrr::set_names(names(customPlotConfiguration)),
    ~ customPlotConfiguration[[.]]
  )
  ospsuite.utils::validateIsOption(
    plotConfigurationList,
    .getPlotConfigurationOptions(names(timeProfilesConfiguration))
  )

  # prepare data ------------------------

  # extract the needed dataframe from the object
  data <- .aggregateSimulationAndObservedData(
    simulationResults      = sensitivityCalculation$simulationResults,
    dataSets               = observedData,
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

  # split long parameter path names for plotting
  data <- dplyr::mutate(
    data,
    ParameterPath = dplyr::if_else(
      is.na(ParameterPath),
      ParameterPath,
      purrr::map_chr(ParameterPath, .splitParameterName, equalLines = TRUE)
    )
  )

  # list of plots ------------------------

  # create plot for each output path
  lsPlots <- purrr::map(
    .x = data %>% split(.$OutputPath),
    .f = ~ .createTimeProfiles(
      .x,
      defaultPlotConfiguration = customPlotConfiguration
    )
  )

  return(lsPlots)
}

#' @keywords internal
#' @noRd
.createTimeProfiles <- function(data,
                                defaultPlotConfiguration) {
  # update data dependent plot configuration
  plotConfiguration <- defaultPlotConfiguration$clone()
  plotConfiguration <- .updatePlotConfiguration(
    plotConfiguration,
    list(title = unique(data$OutputPath))
  )

  # create axis labels
  if (is.null(plotConfiguration$xLabel)) {
    plotConfiguration$xLabel <- paste0(
      unique(data$xDimension), " [",
      unique(data$xUnit), "]"
    )
  }
  if (is.null(plotConfiguration$yLabel)) {
    plotConfiguration$yLabel <- paste0(
      unique(data$yDimension), " [",
      unique(data$yUnit), "]"
    )
  }

  # calculate y-axis breaks and limits -------
  pLimits <- .calculateLimits(data$yValues,
                              scaling = plotConfiguration$yAxisScale)
  pBreaks <- .calculateBreaks(
    data$yValues,
    m = 4, Q = c(0.01, 0.1, 100, 1000),
    scaling = plotConfiguration$yAxisScale
  )
  cBreaks <- c(
    min(data$ParameterFactor, na.rm = TRUE), 1,
    max(data$ParameterFactor, na.rm = TRUE)
  )
  cBreaks <- unique(ifelse(cBreaks == 0, 1, cBreaks))

  # map each parameter path to its own plot ----
  plotList <- purrr::map(
    unique(data$ParameterPath) %>% .[!is.na(.)],
    ~ {
      dataSubset <- dplyr::filter(data, ParameterPath == .x)

      # combine original data subset with observed data
      # add observed data if not-null
      if ("observed" %in% data$dataType) {
        observedData <- dplyr::filter(data, dataType == "observed") %>%
          dplyr::select(-ParameterPath)
        hasObservedData <- isTRUE(nrow(observedData) != 0)
      } else {
        hasObservedData <- FALSE
      }

      # basic plot setup -------------------------

      plot <- ggplot() +
        geom_line(
          data = dplyr::filter(dataSubset, ParameterFactor != 1.0),
          aes(
            x = xValues,
            y = yValues,
            group = ParameterFactor,
            color = ParameterFactor
          ),
          linewidth = plotConfiguration$linesSize,
          alpha = plotConfiguration$linesAlpha,
          na.rm = TRUE
        ) +
        geom_line(
          data = dplyr::filter(dataSubset, ParameterFactor == 1.0),
          aes(xValues, yValues),
          color = "black",
          linewidth = plotConfiguration$linesSize,
          alpha = plotConfiguration$linesAlpha,
          na.rm = TRUE
        )

      # add symbols for observed data
      if (hasObservedData) {
        plot <- plot +
          geom_point(
            data = observedData,
            aes(xValues, yValues, shape = `Study Id`)
          ) +
          scale_shape_manual(
            values = rep(
              plotConfiguration$pointsShape,
              length.out = length(unique(observedData$`Study Id`))
            ),
            name = "Observed data"
          )
      }

      # adjusting axis scales
      if (isTRUE(plotConfiguration$xAxisScale == "log")) {
        plot <- plot + scale_x_log10()
      }
      if (isTRUE(plotConfiguration$yAxisScale == "log")) {
        plot <- plot +
          scale_y_log10(
            limits = replace(pLimits, pLimits == 0, 0.001),
            breaks = pBreaks,
            labels = scales::label_number()
          )
      } else {
        plot <- plot +
          scale_y_continuous(
            limits = pLimits,
            breaks = pBreaks,
            labels = scales::label_number()
          )
      }

      # finalize plot ----------------------------

      # note: facet wrap on unique PK parameter to obtain facet titles
      plot <- plot +
        facet_wrap(~ParameterPath) +
        labs(
          x = plotConfiguration$xLabel,
          y = plotConfiguration$yLabel,
          title = NULL,
          color = plotConfiguration$legendTitle
        )

      # theme adjustments
      plot <- plot +
        theme_bw(base_size = 11) +
        theme(
          legend.position = plotConfiguration$legendPosition,
          panel.grid.minor = element_blank(),
          text = element_text(size = 11)
        ) +
        guides(
          color = guide_colorbar(
            title = plotConfiguration$legendTitle,
            ticks = FALSE,
            draw.ulim = FALSE,
            draw.llim = FALSE,
            title.position = "top",
            order = 1
          ),
          shape = guide_legend(order = 2)
        )

      if (hasObservedData) {
        plot <- plot +
          guides(
            shape = guide_legend(
              title.position = "top",
              nrow = length(unique(observedData$dataType))
            )
          )
      }

      # apply color scales
      if (is.null(plotConfiguration$linesColor)) {
        plot <- plot +
          colorspace::scale_color_continuous_diverging(
            palette = "Berlin",
            mid = log10(1),
            transform = "log",
            breaks = cBreaks
          )
      } else {
        plot <- plot +
          scale_color_gradient2(
            name = plotConfiguration$legendTitle,
            low = colorspace::darken(
              plotConfiguration$linesColor[1],
              amount = 0.15
            ),
            mid = "black",
            high = colorspace::darken(
              plotConfiguration$linesColor[2],
              amount = 0.15
            ),
            transform = "log",
            midpoint = 1,
            breaks = cBreaks
          )
      }

      return(plot)
    }
  )

  # compile individual plots -----------------
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


#' Aggregate simulation results and observed data into a combined dataframe
#'
#' This function performs aggregation from a list of `SimulationResults` objects
#' as returned by `sensitivityCalculation()` and a list of `DataSet` objects.
#' `DataSets` are added to each quantity path (`outputPaths`) when the `yDimension`
#' from `DataSet` can be converted to the x- and y-Dimensions of `SimulationResults`.
#'
#' @param simulationResults A list of `SimulationResults` objects returned by
#' `sensitivityCalculation()`.
#' @param dataSets A list of `DataSet` objects containing observed data.
#' @param parameterPaths A character vector of parameter paths.
#' @param outputPaths A character vector of output paths.
#'
#' @return A combined `data.frame` containing both simulation results and
#' observed data.
#'
#' @keywords internal
#' @noRd
.aggregateSimulationAndObservedData <- function(simulationResults,
                                                dataSets,
                                                parameterPaths,
                                                outputPaths) {
  if (!identical(names(simulationResults), parameterPaths)) {
    stop("The names of the simulationResults and parameterPaths must be the same")
  }
  validateIsOfType(simulationResults, "list")

  parameterPathList <- setNames(
    vector("list", length(parameterPaths)),
    parameterPaths
  )
  # iterate over each `parameterPath`, combine `simulationResults` for each
  # `parameterFactor` and add `DataSets` by `outputPath`
  for (parameterPath in names(simulationResults)) {
    simulationResultsPath <- simulationResults[[parameterPath]]
    parameterFactor <- as.list(names(simulationResultsPath))

    outputPathList <- setNames(
      vector("list", length(outputPaths)),
      outputPaths
    )

    for (outputPath in outputPaths) {
      dataCombined <- DataCombined$new()

      # add multiple simulation results to dataCombined
      # uses parameterFactor from sensitivity analysis as name
      for (i in seq_along(parameterFactor)) {
        dataCombined$addSimulationResults(
          simulationResultsPath[[i]],
          quantitiesOrPaths = outputPath,
          names = parameterFactor[i],
          groups = parameterPath
        )
      }

      if (!is.null(dataSets)) {
        validateIsOfType(dataSets, "list")

        for (j in seq_along(dataSets)) {
          dataCombinedClone <- dataCombined$clone()
          unitsConvertable <- TRUE

          tryCatch(
            {
              # try to add data sets and convert units
              dataCombinedClone$addDataSets(dataSets[[j]])
              convertUnits(dataCombinedClone)
            },
            error = function(e) {
              unitsConvertable <<- FALSE
            }
          )

          if (unitsConvertable) {
            dataCombined$addDataSets(dataSets[[j]])
          }
        }
      }
      outputPathList[[outputPath]] <- convertUnits(dataCombined)
    }
    parameterPathList[[parameterPath]] <- dplyr::bind_rows(
      outputPathList,
      .id = "OutputPath"
    )
  }

  combinedDf <- dplyr::bind_rows(parameterPathList, .id = "ParameterPath")
  # use names derived from parameterFactor to create a numeric column
  combinedDf <- dplyr::rowwise(combinedDf) %>%
    dplyr::mutate(
      ParameterFactor = if (dataType == "simulated") as.numeric(name) else NA_real_
    ) %>%
    dplyr::ungroup()

  # select and arrange columns
  combinedDf <- dplyr::select(
    combinedDf,
    OutputPath,
    dplyr::starts_with("Parameter"),
    xValues,
    yValues,
    dplyr::everything(),
    -IndividualId,
    -name
  )

  return(combinedDf)
}
