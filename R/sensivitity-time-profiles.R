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
#' @param xUnits,yUnits Lists of units for the x-axis and y-axis, respectively.
#' If a list of length one is provided, it will be applied to all `outputPaths`
#' if conversion is possible. If a list of multiple units is provided, the units
#' list should correspond to the `outputPaths`, and units conversion will be
#' applied accordingly. If `NULL`, default units from the simulation results
#' will be used.
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
#' @returns A `patchwork` object containing the combined ggplot objects if a
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
                                    xUnits = NULL,
                                    yUnits = NULL,
                                    observedData = NULL,
                                    defaultPlotConfiguration = NULL) {
  # Input validation -------------------------------------

  validateIsOfType(sensitivityCalculation, "SensitivityCalculation")
  validateIsOfType(observedData, DataSet, nullAllowed = TRUE)
  validateIsOfType(xUnits, "list", nullAllowed = TRUE)
  validateIsOfType(yUnits, "list", nullAllowed = TRUE)

  .validateCharVectors(outputPaths, nullAllowed = TRUE)
  .validateCharVectors(parameterPaths, nullAllowed = TRUE)

  # Plot configuration setup -----------------------------

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

  # apply configuration overrides and validate
  customPlotConfiguration <- .applyPlotConfiguration(
    defaultPlotConfiguration = defaultPlotConfiguration,
    plotOverrideConfig       = timeProfilesConfiguration,
    xAxisScale               = xAxisScale,
    yAxisScale               = yAxisScale
  )

  # Prepare data -----------------------------------------

  data <- .aggregateSimulationAndObservedData(
    simulationResults        = sensitivityCalculation$simulationResults,
    dataSets                 = observedData,
    parameterPaths           = sensitivityCalculation$parameterPaths,
    outputPaths              = sensitivityCalculation$outputPaths,
    xUnits                   = xUnits,
    yUnits                   = yUnits
  )

  # filter out data not needed for plotting
  data <- .filterPlottingData(
    data,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    pkParameters = NULL # not relevant
  )

  # modify long parameter path names for plotting
  data <- dplyr::mutate(
    data,
    ParameterPath = dplyr::if_else(
      is.na(ParameterPath),
      ParameterPath,
      purrr::map_chr(ParameterPath, .splitParameterName, equalLines = TRUE)
    )
  )

  # Create list of plots ---------------------------------

  splitData <- split(data, data$OutputPath)
  lsPlots <- stats::setNames(
    vector("list", length(names(splitData))), names(splitData)
  )

  # create plot for each output path
  for (outputPath in names(splitData)) {
    subsetData <- splitData[[outputPath]]

    lsPlots[[outputPath]] <- .createTimeProfiles(
      splitData[[outputPath]],
      defaultPlotConfiguration = customPlotConfiguration
    )
  }

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

  # calculate y-axis limits and color legend breaks
  pLimits <- .calculateLimits(data$yValues,
    scaling = plotConfiguration$yAxisScale
  )
  cBreaks <- c(
    min(data$ParameterFactor, na.rm = TRUE), 1,
    max(data$ParameterFactor, na.rm = TRUE)
  )
  cBreaks <- unique(ifelse(cBreaks == 0, 1, cBreaks))

  # Loop through unique ParameterPath
  paramPaths <- unique(data$ParameterPath)
  paramPaths <- paramPaths[!is.na(paramPaths)]
  plotList <- stats::setNames(vector("list", length(paramPaths)), paramPaths)

  for (paramPath in paramPaths) {
    dataSubset <- dplyr::filter(data, ParameterPath == paramPath)

    # replace zeros dynamically to avoid warning when log transform
    dataSubset$yValues <- ifelse(dataSubset$yValues <= 0,
      pLimits[1], dataSubset$yValues
    )

    # combine original data subset with observed data
    if ("observed" %in% data$dataType) {
      observedData <- dplyr::filter(data, dataType == "observed") %>%
        dplyr::select(-ParameterPath)
      hasObservedData <- isTRUE(nrow(observedData) != 0)
    } else {
      hasObservedData <- FALSE
    }

    # Basic plot setup -----------------------------------

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

    # adjust axis scales
    if (isTRUE(plotConfiguration$xAxisScale == "log")) {
      plot <- plot + scale_x_log10()
    }
    if (isTRUE(plotConfiguration$yAxisScale == "log")) {
      plot <- plot +
        scale_y_log10(
          limits = pLimits,
          expand = expansion(mult = c(0.01, 0.1)),
          breaks = scales::breaks_log(),
          labels = scales::label_log()
        )
    } else {
      plot <- plot +
        scale_y_continuous(
          limits = pLimits,
          breaks = scales::breaks_extended(),
          labels = scales::label_number_auto()
        )
    }

    # Finalize plot --------------------------------------

    plot <- plot +
      facet_wrap(~ParameterPath) +
      labs(
        x = plotConfiguration$xLabel,
        y = plotConfiguration$yLabel,
        title = NULL,
        color = plotConfiguration$legendTitle
      )

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

    plotList[[paramPath]] <- plot
  }

  # Compile individual plots -----------------------------

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
#' @returns A combined `data.frame` containing both simulation results and
#' observed data.
#'
#' @keywords internal
#' @noRd
.aggregateSimulationAndObservedData <- function(simulationResults,
                                                dataSets,
                                                parameterPaths,
                                                outputPaths,
                                                xUnits,
                                                yUnits) {
  if (!identical(names(simulationResults), parameterPaths)) {
    stop(messages$invalidSimulationResultNames(
      names(simulationResults), parameterPaths
    ))
  }
  validateIsOfType(simulationResults, "list")

  # validate if xUnits are valid unit for time dimension
  lapply(xUnits, validateEnumValue, ospUnits$Time, TRUE)

  # prepare and validate units to be applied to outputPaths
  xUnits <- .adjustUnits(xUnits, outputPaths)
  yUnits <- .adjustUnits(yUnits, outputPaths)

  parameterPathList <- stats::setNames(
    vector("list", length(parameterPaths)),
    parameterPaths
  )
  # iterate over parameterPaths, combine simulation results and datasets by outputPath
  for (parameterPath in names(simulationResults)) {
    simulationResultsPath <- simulationResults[[parameterPath]]
    parameterFactor <- as.list(names(simulationResultsPath))

    outputPathList <- stats::setNames(
      vector("list", length(outputPaths)),
      outputPaths
    )

    for (outputPath in outputPaths) {
      dataCombined <- DataCombined$new()

      # add multiple simulation results to dataCombined
      for (i in seq_along(parameterFactor)) {
        dataCombined$addSimulationResults(
          simulationResultsPath[[i]],
          quantitiesOrPaths = outputPath,
          names = parameterFactor[i],
          groups = parameterPath
        )
      }

      # add dataSets when units are convertable for outputPath
      if (!is.null(dataSets)) {
        validateIsOfType(dataSets, "list")
        for (j in seq_along(dataSets)) {
          dataCombinedClone <- dataCombined$clone()

          if (.isConvertableUnit(
            dataCombinedClone$addDataSets(dataSets[[j]])
          )) {
            dataCombined$addDataSets(dataSets[[j]])
          }
        }
      }

      # clone the combined data to check for specified unit conversion
      dataCombinedSpecifiedClone <- dataCombined$clone()

      if (.isConvertableUnit(
        dataCombinedSpecifiedClone,
        xUnit = xUnits[[outputPath]],
        yUnit = yUnits[[outputPath]]
      )
      ) {
        outputPathList[[outputPath]] <- convertUnits(
          dataCombinedSpecifiedClone,
          xUnit = xUnits[[outputPath]],
          yUnit = yUnits[[outputPath]]
        )
      } else {
        outputPathList[[outputPath]] <- convertUnits(dataCombined)
      }
    }
    parameterPathList[[parameterPath]] <- dplyr::bind_rows(
      outputPathList,
      .id = "OutputPath"
    )
  }

  combinedDf <- dplyr::bind_rows(parameterPathList, .id = "ParameterPath")
  # cConvert parameterFactor to numeric for simulated data
  combinedDf <- dplyr::rowwise(combinedDf) %>%
    dplyr::mutate(
      ParameterFactor = if (dataType == "simulated") as.numeric(name) else NA_real_
    ) %>%
    dplyr::ungroup()

  # select and arrange columns
  combinedDf <- dplyr::relocate(
    combinedDf,
    OutputPath,
    dplyr::starts_with("Parameter"),
    xValues,
    yValues
  ) %>%
    dplyr::select(
      -IndividualId, -name
    )

  return(combinedDf)
}

#' Test if unit conversion is possible in `DataCombined` objects.
#'
#' @param dataCombined `DataCombined` object to test.
#' @param xUnit Optional unit for the x-dimension.
#' @param yUnit Optional unit for the y-dimension.
#'
#' @returns TRUE if conversion is possible, otherwise FALSE.
#'
#' @keywords internal
#' @noRd
.isConvertableUnit <- function(dataCombined, xUnit = NULL, yUnit = NULL) {
  unitsConvertable <- TRUE

  tryCatch(
    {
      convertUnits(dataCombined, xUnit = xUnit, yUnit = yUnit)
    },
    error = function(e) {
      unitsConvertable <<- FALSE
    }
  )

  return(unitsConvertable)
}

#' Validate if unit is valid for any dimension in `ospUnits`
#'
#' Checks if a given unit is valid across all dimensions in `ospUnits`.
#'
#' @keywords internal
#' @noRd
.validateUnitInOspUnits <- function(unit, nullAllowed = FALSE) {
  unitDimensions <- names(ospUnits)

  for (dimension in unitDimensions) {
    tryCatch(
      {
        ospsuite.utils::validateEnumValue(
          unit, ospUnits[[dimension]],
          nullAllowed = nullAllowed
        )
        return(NULL)
      },
      error = function(e) {
        # do nothing, continue to the next dimension
      }
    )
  }

  # error if unit is not valid in any dimension
  stop(ospsuite.utils::messages$errorValueNotInEnum(ospUnits, unit))
}

#' Normalize units to match the length of outputPaths
#'
#' Validate and normalize the units to ensure they match the length of
#' `outputPaths`.
#'
#' @keywords internal
#' @noRd
.adjustUnits <- function(units, outputPaths) {
  if (is.null(units)) {
    return(rep(list(NULL), length(outputPaths)))
  }
  # validate if units are valid in any dimension
  lapply(units, .validateUnitInOspUnits, TRUE)

  # check the lengths and extend or add NULL
  if (length(units) == 1) {
    units <- rep(units, length(outputPaths))
  } else if (length(units) < length(outputPaths)) {
    units <- c(units, rep(list(NULL), length(outputPaths) - length(units)))
  }

  names(units) <- outputPaths

  return(units)
}
