#' Plot multiple `PlotMapping` in one plot.
#'
#' @param dataMappingList A single or a list of `PlotMapping` objects
#' @param plotConfiguration An object of type `PlotConfiguration`
#' @param ... Any parameter that can be interpreted by the default [plot()] function
#' @export
plotMultiPanel <- function(dataMappingList, plotConfiguration, ...) {
  dataMappingList <- toList(dataMappingList)

  nrOfCols <- plotConfiguration$nrOfCols
  # If no number of columns provided, calculate the number needed from
  # the amount of PlotMappings. Calculated in such way that the numbers of columns
  # and rows are as close as possible
  if (is.null(nrOfCols)) {
    nrOfCols <- ceiling(sqrt(length(dataMappingList)))
  }
  # Nr of rows is calculated from the number of columns such that all PlotMappings
  # can be plottet
  nrOfRows <- ceiling(length(dataMappingList) / nrOfCols)

  # If no width and height are provided, automatically calculate based on the number of PlotMappings
  width <- plotConfiguration$width %||% esqlabsEnv$widthPerPlotMapping * nrOfCols
  height <- plotConfiguration$height %||% esqlabsEnv$heightPerPlotMapping * nrOfRows

  openOuptutDevice(plotConfiguration, width, height)

  split.screen(c(nrOfRows, nrOfCols))
  for (idx in seq_along(dataMappingList)) {
    screen(idx)
    dataMappingList[[idx]]$plot(cex = 1 - 0.1 * (nrOfRows - 1), ...)

    if (length(dataMappingList) > 1) {
      figureAddLabel(letters[[idx]], offset = c(0, -0.1))
    }
  }

  if (plotConfiguration$addTitle) {
    title(plotConfiguration$outputName, outer = TRUE, line = -1, cex = 1.2)
  }

  close.screen(all.screens = TRUE)

  closeOutputDevice(plotConfiguration)
}

#' Plot XYData
#'
#' @description Draw XYData on top of an existing plot using the `points` method.
#'
#' @param xySeries An `XYData` object to be plotted
#' @param xUnit Target unit of x-axis.
#' @param yUnit Target unit of y-axis.
#' If `TRUE`,
#' @param ... Any parameter that can be interpreted by the default [plot()] function
#' @import ospsuite
#' @keywords internal
plotXYData <- function(xySeries, xUnit = NULL, yUnit = NULL, ...) {
  validateIsOfType(xySeries, "XYData")
  points(xySeries$xValuesProcessed(xUnit),
    xySeries$yValuesProcessed(yUnit),
    type = xySeries$type,
    lty = xySeries$lty,
    pch = xySeries$pch,
    col = xySeries$color,
    ...
  )
  if (!is.null(xySeries$yError)) {
    plotErrorBars(xySeries$xValuesProcessed(xUnit),
      xySeries$yValuesProcessed(yUnit),
      xySeries$yErrorProcessed(yUnit),
      col = xySeries$color,
      ...
    )
  }
}

#' Plot XYData quantiles
#'
#' @description Draw XYData on top of an existing plot using the `points` method.
#'
#' @param xySeries An `XYData` object to be plotted
#' @inheritParams getQuantilesYData
#' @param xUnit Target unit of x-axis.
#' @param yUnit Target unit of y-axis.
#' @param ... Any parameter that can be interpreted by the default [plot()] function
#' @import ospsuite
#' @export
plotXYDataAggregated <- function(xySeries, xUnit = NULL, yUnit = NULL,
                                 quantiles = c(0.05, 0.5, 0.95), ...) {
  validateIsOfType(xySeries, "XYData")
  # Get the quantiles for data - lower/mid/upper
  aggregatedData <- getQuantilesYData(
    xValues = xySeries$xValuesProcessed(xUnit),
    yValues = xySeries$yValuesProcessed(yUnit),
    quantiles = quantiles
  )

  # Draw the shaded area
  polygon(c(aggregatedData[[3]]$xValues, rev(aggregatedData[[1]]$xValues)),
    c(aggregatedData[[3]]$yValues, rev(aggregatedData[[1]]$yValues)),
    # Add 50% transparency for the color
    col = paste0(xySeries$color, "80"),
    border = 0
  )
  # Draw the middle quantile line
  points(aggregatedData[[2]]$xValues,
    aggregatedData[[2]]$yValues,
    type = xySeries$type,
    lty = xySeries$lty,
    pch = xySeries$pch,
    col = xySeries$color,
    ...
  )
}

#' Plot individual time-values profile
#' @description Create a 2D-plot of the x-y data sets stored in `dataMapping`
#'
#' @param dataMapping A `DataMapping` object with `XYData`
#' @param ... Any parameter that can be interpreted by the default [plot()] function
plotIndividualProfile <- function(dataMapping, ...) {
  plotTimeValues(dataMapping, aggregated = FALSE, ...)
}

#' Create a box-plot of data
#'
#' @param dataMapping A `DataMapping` object with `XYData`
#' @param ... Any parameter that can be interpreted by the default [boxplot()] function
#' @import ospsuite
#' @export
plotBoxPlot <- function(dataMapping, ...) {
  validateIsOfType(dataMapping, "DataMapping")
  legendEntries <- vector(mode = "character", length = length(dataMapping$xySeries))

  allData <- vector(mode = "list", length = length(dataMapping$xySeries))
  for (i in seq_along(dataMapping$xySeries)) {
    legendEntries[i] <- paste0(i, ": ", dataMapping$xySeries[[i]]$label)
    allData[[i]] <- dataMapping$xySeries[[i]]$yValues
  }

  boxplot(allData,
    xlab = dataMapping$xLab,
    ylab = dataMapping$yLab,
    main = dataMapping$title
  )

  # Draw a legend, if specified
  if (dataMapping$addLegend) {
    .figureAddLegend(
      x = dataMapping$legendPosition,
      legend = legendEntries,
      col = NULL,
      pch = NULL,
      lty = rep(0, length(legendEntries)),
      ...
    )
  }
}

#' Plot individual time-values profile
#' @description Create a 2D-plot of the x-y data sets stored in `dataMapping`.
#' Population simulation results are plotted as quantiles
#'
#' @param dataMapping A `DataMapping` object with `XYData`
#' @param ... Any parameter that can be interpreted by the default [plot()] function
plotPopulationQuantiles <- function(dataMapping, ...) {
  plotTimeValues(dataMapping, aggregated = TRUE, ...)
}

#' Plot time-values profile
#' @description Create a 2D-plot of the x-y data sets stored in `dataMapping`
#'
#' @import ospsuite
#' @param dataMapping A `DataMapping` object with `XYData`
#' @param aggregated Boolean. If `FALSE`, simulation data containing multiple
#'   individuals (population simulation) are plotted separately for each
#'   individual. If `TRUE`, population simulation results are plotted as
#'   mid-percentile and lower/upper percentile bands around.
#' @param ... Any parameter that can be interpreted by the default [plot()] function
#' @import hash
#' @keywords internal
plotTimeValues <- function(dataMapping, aggregated, ...) {
  validateIsOfType(dataMapping, "DataMapping")
  legendEntries <- c()
  legendColors <- c()
  legendLty <- c()
  legendPch <- c()

  # Count the number of groups plus the number of data sets that are not assigned to any group.
  # Based on this number, the colors and other graphics argument values will be defined
  nrOfEntries <- length(dataMapping$groupings) + length(dataMapping$ungroupedSeries)
  # Generate default values in case specific values for color etc are not set
  colors <- esqLABS_colors(nrOfColors = nrOfEntries)
  pchArr <- rep(0:24, times = (dataMapping$xySeriesCount %/% 26 + 1))
  ltyArr <- 1:nrOfEntries
  graphicsParIdx <- 1

  # If logarithmic scaling of y-axis has been selected and manually provided y-lim
  # is non-positive, use the automatically calculated values
  if (isCharInString("y", dataMapping$log) && !all(dataMapping$yLim > 0)) {
    dataMapping$yLim <- NULL
  }

  # Create an empty plot
  # If axis labels have not been set by the user, generate defaults based
  # on Dimension and unit
  plot(NULL, NULL,
    xlim = dataMapping$xLim,
    ylim = dataMapping$yLim,
    xlab = dataMapping$xLab %||% paste0(dataMapping$xDimension, " [", dataMapping$xUnit, "]"),
    ylab = dataMapping$yLab %||% paste0(dataMapping$yDimension, " [", dataMapping$yUnit, "]"),
    log = dataMapping$log,
    main = dataMapping$title,
    ...
  )

  # Add the current data set to the legend.
  updateLegend <- function(legendConfiguration) {
    legendEntries <<- c(legendEntries, legendConfiguration$legendEntry)
    legendColors <<- c(legendColors, legendConfiguration$color)
    legendPch <<- c(legendPch, legendConfiguration$pch)
    legendLty <<- c(legendLty, legendConfiguration$lty)
  }

  pchParIdx <- 0
  for (i in seq_along(dataMapping$groupings)) {
    legendConfiguration <- list("legendEntry" = names(dataMapping$groupings)[[i]], "color" = NULL, "pch" = NA, "lty" = 0)
    # Plot every entry within the group with the same graphical parameters except for pch
    for (j in seq_along(dataMapping$groupings[[i]])) {
      pchParIdx <- pchParIdx + 1
      xySeriesEntry <- dataMapping$xySeries[[dataMapping$groupings[[i]][[j]]]]

      # We have to track if any of the graphic parameters for the XYData
      # were NULL, so these are reset to NULL after plotting the XYData
      resetPch <- FALSE
      resetLty <- FALSE
      resetColor <- FALSE

      if (is.null(xySeriesEntry$lty)) {
        xySeriesEntry$lty <- ltyArr[[graphicsParIdx]]
        resetLty <- TRUE
      }
      if (is.null(xySeriesEntry$pch)) {
        xySeriesEntry$pch <- pchArr[[pchParIdx]]
        resetPch <- TRUE
      }
      if (is.null(xySeriesEntry$color)) {
        xySeriesEntry$color <- colors[[graphicsParIdx]]
        resetColor <- TRUE
      }

      legendConfiguration$color <- xySeriesEntry$color
      # If symbols are plotted for the data set, set its pch for the legend
      if (.isPoint(xySeriesEntry$type)) {
        legendConfiguration$pch <- xySeriesEntry$pch
      }
      # If line is plotted for the data set, set its lty for the legend
      if (.isLine(xySeriesEntry$type)) {
        legendConfiguration$lty <- xySeriesEntry$lty
      }

      # If XYSeriesEntry is simulation data, choose whether to plot individual values
      # or aggregated data
      if (xySeriesEntry$dataType == XYDataTypes$Simulated && aggregated) {
        plotXYDataAggregated(
          xySeriesEntry,
          dataMapping$xUnit,
          dataMapping$yUnit,
          quantiles = dataMapping$populationQuantiles,
          ...
        )
      } else {
        plotXYData(
          xySeriesEntry,
          dataMapping$xUnit,
          dataMapping$yUnit,
          ...
        )
      }

      # Reset the entry's graphics parameters
      if (resetPch) {
        xySeriesEntry$pch <- NULL
      }
      if (resetLty) {
        xySeriesEntry$lty <- NULL
      }
      if (resetColor) {
        xySeriesEntry$color <- NULL
      }
    }
    updateLegend(legendConfiguration)
    graphicsParIdx <- graphicsParIdx + 1
  }
  # pchParIdx == 0 is when no grouping exists
  if (pchParIdx == 0) {
    pchParIdx <- 1
  }
  # Process XYData that are in no grouping
  for (xySeriesName in dataMapping$ungroupedSeries) {
    xySeriesEntry <- dataMapping$xySeries[[xySeriesName]]
    # We have to track if any of the graphic parameters for the XYData
    # were NULL, so these are reset to NULL after plotting the XYData
    resetPch <- FALSE
    resetLty <- FALSE
    resetColor <- FALSE

    if (is.null(xySeriesEntry$lty)) {
      xySeriesEntry$lty <- ltyArr[[graphicsParIdx]]
      resetLty <- TRUE
    }
    if (is.null(xySeriesEntry$pch)) {
      xySeriesEntry$pch <- pchArr[[pchParIdx]]
      resetPch <- TRUE
    }
    if (is.null(xySeriesEntry$color)) {
      xySeriesEntry$color <- colors[[graphicsParIdx]]
      resetColor <- TRUE
    }

    legendConfiguration <- list("legendEntry" = xySeriesName, "color" = xySeriesEntry$color, "pch" = NA, "lty" = 0)
    # If symbols are plotted for the data set, set its pch for the legend
    if (.isPoint(xySeriesEntry$type)) {
      legendConfiguration$pch <- xySeriesEntry$pch
    }
    # If line is plotted for the data set, set its lty for the legend
    if (.isLine(xySeriesEntry$type)) {
      legendConfiguration$lty <- xySeriesEntry$lty
    }

    # If XYSeriesEntry is simulation data, choose whether to plot individual values
    # or aggregated data
    if (xySeriesEntry$dataType == XYDataTypes$Simulated && aggregated) {
      plotXYDataAggregated(
        xySeriesEntry,
        dataMapping$xUnit,
        dataMapping$yUnit,
        quantiles = dataMapping$populationQuantiles,
        ...
      )
    } else {
      plotXYData(
        xySeriesEntry,
        dataMapping$xUnit,
        dataMapping$yUnit,
        ...
      )
    }
    updateLegend(legendConfiguration)

    if (resetPch) {
      xySeriesEntry$pch <- NULL
    }
    if (resetLty) {
      xySeriesEntry$lty <- NULL
    }
    if (resetColor) {
      xySeriesEntry$color <- NULL
    }

    graphicsParIdx <- graphicsParIdx + 1
    pchParIdx <- graphicsParIdx
  }

  # Draw a legend, if specified
  if (dataMapping$addLegend && (length(legendEntries) > 0)) {
    .figureAddLegend(
      x = dataMapping$legendPosition,
      legend = legendEntries,
      col = legendColors,
      pch = legendPch,
      lty = legendLty,
      ...
    )
  }
}

#' Plot a predicted-versus-observed goodness of fit plot
#'
#' @param dataMapping THe `DataMapping` object for which the goodness-of-fit
#' plot is to be drawn. For each group within the `dataMapping`, simulated
#' and observed values are compared.
#' @param foldDistance Numerical value for the fold-distance lines to be drawn.
#'   Default is 2.
#' @param timeDiffThreshold Allowed difference between observed and simulated
#'   time values in minutes. Default is 10. If for a certain observed point no
#'   simulated time point exists within the defined threshold, the value is not
#'   considered.
#' @param ... Any parameter that can be interpreted by the default [plot()] function
#'
#' @details Observed data points are drawn on the x, simulated values on the y axis.
#' @import ospsuite
#' @export
plotPredictedVsObserved <- function(dataMapping, foldDistance = 2, timeDiffThreshold = 10, ...) {
  validateIsOfType(dataMapping, "DataMapping")
  legendEntries <- c()
  legendColors <- c()
  legendPch <- c()

  # Count the number of groups plus the number of data sets that are not assigned to any group.
  # Based on this number, the colors and other graphics argument values will be defined
  nrOfEntries <- length(dataMapping$groupings) + length(dataMapping$ungroupedSeries)
  # Generate default values in case specific values for color etc are not set
  colors <- esqLABS_colors(nrOfColors = nrOfEntries)
  pchArr <- 1:dataMapping$xySeriesCount
  ltyArr <- 1:nrOfEntries
  graphicsParIdx <- 1

  # Convert timeDiffThreshold to xUnit of the dataMapping
  if (!is.null(timeDiffThreshold)) {
    timeDiffThreshold <- toUnit(quantityOrDimension = "Time", values = timeDiffThreshold, sourceUnit = "min", targetUnit = dataMapping$xUnit)
  }

  # If logarithmic scaling of y-axis has been selected and manually provided y-lim
  # is non-positive, use the automatically calculated values
  if (isCharInString("y", dataMapping$log) && !all(dataMapping$yLim > 0)) {
    dataMapping$yLim <- NULL
  }

  # Create an empty plot
  plot(NULL, NULL,
    xlim = dataMapping$yLim + abs(dataMapping$yLim) * c(-0.1, 0.1),
    ylim = dataMapping$yLim + abs(dataMapping$yLim) * c(-0.1, 0.1),
    xlab = "Observed values",
    ylab = "Simulated values",
    log = if (isCharInString("y", dataMapping$log)) {
      "xy"
    } else {
      ""
    },
    main = dataMapping$title,
    ...
  )
  # Draw the identity line
  points(dataMapping$yLim, dataMapping$yLim, type = "l")
  # Plot the fold deviation lines.
  for (f in foldDistance) {
    points(dataMapping$yLim, dataMapping$yLim * 1 / f, type = "l", lty = 2)
    points(dataMapping$yLim, dataMapping$yLim * f, type = "l", lty = 2)
  }

  graphicsParIdx <- 1
  for (groupingIdx in seq_along(dataMapping$groupings)) {
    grouping <- dataMapping$groupings[[groupingIdx]]

    # Combine all values for observed data
    dataPointsX <- c()
    dataPointsY <- c()
    dataErrorY <- c()
    simulatedResults <- list()
    # First filter simulated results from observed data
    for (dataName in grouping) {
      xySeries <- dataMapping$xySeries[[dataName]]
      if (xySeries$dataType == XYDataTypes$Simulated) {
        simulatedResults <- append(simulatedResults, xySeries)
        next
      }
      # Collapse all observed data
      dataPointsX <- c(dataPointsX, xySeries$xValuesProcessed(dataMapping$xUnit))
      dataPointsY <- c(dataPointsY, xySeries$yValuesProcessed(dataMapping$yUnit))
    }
    # Plot the simulated-observed pairs
    for (simulatedResult in simulatedResults) {
      # Apply scaling to simulated results
      simulatedPointsX <- simulatedResult$xValuesProcessed(dataMapping$xUnit)
      simulatedPointsY <- simulatedResult$yValuesProcessed(dataMapping$yUnit)
      # Iterate through each observed data point and find the simulated value
      # with the closest x-value.
      for (i in seq_along(dataPointsX)) {
        idx <- getIndexClosestToValue(dataPointsX[[i]], (simulatedPointsX), thresholdAbs = timeDiffThreshold)
        # In case of population simulation, idx may have more than one entry
        for (pointIdx in idx) {
          points(dataPointsY[[i]], simulatedPointsY[[pointIdx]],
            pch = pchArr[[graphicsParIdx]],
            col = colors[[graphicsParIdx]]
          )
        }
      }
    }
    legendEntries <- c(legendEntries, names(dataMapping$groupings)[[groupingIdx]])
    legendColors <- c(legendColors, colors[[graphicsParIdx]])
    legendPch <- c(legendPch, pchArr[[graphicsParIdx]])
    graphicsParIdx <- graphicsParIdx + 1
  }

  # Draw a legend, if specified
  if (dataMapping$addLegend) {
    .figureAddLegend(
      x = dataMapping$legendPosition,
      legend = legendEntries,
      col = legendColors,
      pch = legendPch,
      lty = NULL,
      ...
    )
  }
}

#' Calculate the root mean square error for groupings in `dataMapping`s
#'
#'
#' @param dataMappingList A `DataMapping` or a list of `DataMapping` objects.
#' @param timeDiffThreshold Allowed difference between observed and simulated
#'   time values in minutes. Default is 10. If for a certain observed point no
#'   simulated time point exists within the defined threshold, the value is not
#'   considered.
#'
#' @details The error is calculated for each group separately and added up. For
#'   each group, the error is defined as the root of the sum of the squared
#'   residuals between each simulated result and observed data.
#'
#' @return Total error for all groups across all provided data mappings.
#' @import ospsuite
#' @export
calculateRMSE <- function(dataMappingList, timeDiffThreshold = 10) {
  dataMappingList <- toList(dataMappingList)

  error <- 0
  for (dataMapping in dataMappingList) {
    # Convert timeDiffThreshold to xUnit of the dataMapping
    if (!is.null(timeDiffThreshold)) {
      timeDiffThreshold <- toUnit(
        quantityOrDimension = "Time",
        values = timeDiffThreshold,
        sourceUnit = "min",
        targetUnit = dataMapping$xUnit
      )
    }

    for (grouping in dataMapping$groupings) {
      dataPointsX <- c()
      dataPointsY <- c()
      simulatedResults <- list()
      # First filter simulated results from observed data
      for (dataName in grouping) {
        xySeries <- dataMapping$xySeries[[dataName]]
        if (xySeries$dataType == XYDataTypes$Simulated) {
          simulatedResults <- append(simulatedResults, xySeries)
          next
        }
        # Collapse all observed data
        dataPointsX <- c(dataPointsX, xySeries$xValuesProcessed(dataMapping$xUnit))
        dataPointsY <- c(dataPointsY, xySeries$yValuesProcessed(dataMapping$yUnit))
      }
      # Calculate the distance between each point of the observed data to each simulated result
      for (simulatedResult in simulatedResults) {
        # Apply scaling to simulated results
        simulatedPointsX <- simulatedResult$xValuesProcessed(dataMapping$xUnit)
        simulatedPointsY <- simulatedResult$yValuesProcessed(dataMapping$yUnit)
        for (i in seq_along(dataPointsX)) {
          idx <- getIndexClosestToValue(dataPointsX[[i]], (simulatedPointsX), timeDiffThreshold)
          # In case of population simulation, idx may have more than one entry
          for (pointIdx in idx) {
            error <- error + (simulatedPointsY[[pointIdx]] - dataPointsY[[i]])^2
          }
        }
      }
    }
  }
  return(sqrt(error))
}
