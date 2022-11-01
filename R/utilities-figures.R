#' esqLABS color palette
#'
#' Returns the list of colors extrapolated between the esqLABS colors blue, red,
#' and green.
#'
#' For `nrOfColors` == 1, the esqLABS-blue is returned
#' For `nrOfColors` == 2, the esqLABS-blue and green are returned
#' For `nrOfColors` == 3, the esqLABS-blue, red, and green are returned
#' For `nrOfColors` > 3, the three esqLABS colors are fixed, and the remaining
#' colors are extrapolated from blue to red to green. If `nrOfColors` is uneven,
#' the blue-to-red section becomes one color more than the red-to-green section.
#' In this implementation, blue-to-green is not considered.
#'
#' @param nrOfColors Positive integer defining the number of colors to be generated.
#'
#' @return A list of colors as HEX values.
#' @export
esqLABS_colors <- function(nrOfColors) {
  # esqLABS colors in HSV model
  esqRed_hsv <- rgb2hsv(235, 23, 51, maxColorValue = 255)
  esqBlue_hsv <- rgb2hsv(13, 141, 218, maxColorValue = 255)
  esqGreen_hsv <- rgb2hsv(38, 176, 66, maxColorValue = 255)
  # default color palette.
  esq_palette <- c(
    hsv(esqBlue_hsv[1], esqBlue_hsv[2], esqBlue_hsv[3]),
    hsv(esqRed_hsv[1], esqRed_hsv[2], esqRed_hsv[3]),
    hsv(esqGreen_hsv[1], esqGreen_hsv[2], esqGreen_hsv[3])
  )

  # pre-calculate distances between blue and red and red and green.
  deltaH_b_r <- (esqRed_hsv[1] - esqBlue_hsv[1])
  deltaS_b_r <- max(esqRed_hsv[2], esqBlue_hsv[2]) - min(esqRed_hsv[2], esqBlue_hsv[2])
  deltaV_b_r <- max(esqRed_hsv[3], esqBlue_hsv[3]) - min(esqRed_hsv[3], esqBlue_hsv[3])

  deltaH_r_g <- abs(esqRed_hsv[1] - (esqGreen_hsv[1] + 1))
  deltaS_r_g <- max(esqRed_hsv[2], esqGreen_hsv[2]) - min(esqRed_hsv[2], esqGreen_hsv[2])
  deltaV_r_g <- max(esqRed_hsv[3], esqGreen_hsv[3]) - min(esqRed_hsv[3], esqGreen_hsv[3])

  if (nrOfColors < 0) {
    stop(messages$nrOfColorsShouldBePositive(nrOfColors))
  }
  if (nrOfColors == 0) {
    return(c())
  }
  if (nrOfColors == 2) {
    palette <- c(esq_palette[1], esq_palette[3])
    return(palette)
  }
  if (nrOfColors <= 3) {
    palette <- esq_palette[1:nrOfColors]
    return(palette)
  }

  nrOfColorsToGenerate <- nrOfColors - 3

  palette <- esq_palette[1]
  nrOfColors_first <- nrOfColorsToGenerate %/% 2 + nrOfColorsToGenerate %% 2
  nrOfColors_second <- nrOfColorsToGenerate %/% 2
  # calculate the first half - blue to red.
  # Index starts with 1 as it defines the number of colors.
  for (i in 1:nrOfColors_first) {
    deltaH <- deltaH_b_r / (nrOfColors_first + 1)
    deltaS <- deltaS_b_r / (nrOfColors_first + 1)
    deltaV <- deltaV_b_r / (nrOfColors_first + 1)

    h <- esqBlue_hsv[1] + deltaH * i
    if (h > 1) {
      h <- h - 1
    }
    s <- min(esqBlue_hsv[2], esqRed_hsv[2]) + deltaS * i
    v <- min(esqBlue_hsv[3], esqRed_hsv[3]) + deltaV * i

    palette <- c(palette, hsv(h, s, v))
  }

  palette <- c(palette, esq_palette[2])
  # calculate the second half - red to green.
  # Index starts with 1 as it defines the number of colors.
  if (nrOfColors_second > 0) {
    for (i in 1:nrOfColors_second) {
      deltaH <- deltaH_r_g / (nrOfColors_second + 1)
      deltaS <- deltaS_r_g / (nrOfColors_second + 1)
      deltaV <- deltaV_r_g / (nrOfColors_second + 1)

      h <- esqRed_hsv[1] + deltaH * i
      if (h > 1) {
        h <- h - 1
      }
      s <- min(esqGreen_hsv[2], esqRed_hsv[2]) + deltaS * i
      v <- min(esqGreen_hsv[3], esqRed_hsv[3]) + deltaV * i

      palette <- c(palette, hsv(h, s, v))
    }
  }
  palette <- c(palette, esq_palette[3])

  return(palette)
}

#' Returns the HSV values for a given R color name
#'
#' @param color vector of any of the three kinds of R color specifications,
#'   i.e., either a color name (as listed by colors()), a hexadecimal string of
#'   the form "#rrggbb" or "#rrggbbaa" (see rgb), or a positive integer `i`
#'   meaning `palette()[i]`.
#'
#' @return A matrix with a column for each color. The three rows of the matrix
#'   indicate hue, saturation and value and are named "h", "s", and "v"
#'   accordingly.
#' @export
#' @import ospsuite ospsuite.utils
#'
#' @examples
#' col2hsv("yellow")
#' @export
col2hsv <- function(color) {
  validateIsString(color)
  rgb <- col2rgb(color)
  return(rgb2hsv(rgb))
}

#' Possible entries for the `outputDevice` field of a `ProjectConfiguration` object
#' @export
GraphicsDevices <- enum(list("PNG"))


#' @title Create an instance of `DefaultPlotConfiguration` R6 class
#' @rdname createEsqlabsPlotConfiguration
#'
#' @description
#'
#' An instance of `DefaultPlotConfiguration` R6 class is needed for creating
#' visualizations with the `{ospsuite}` package.
#'
#' The default attributes of the class are chosen to reflect the corporate
#' standards adopted by esqLABS GmbH.
#'
#' @return An instance of `DefaultPlotConfiguration` R6 class.
#'
#' @examples
#' createEsqlabsPlotConfiguration()
#'
#' @family create-plotting-configurations
#'
#' @export
createEsqlabsPlotConfiguration <- function() {
  defaultPlotConfiguration <- ospsuite::DefaultPlotConfiguration$new()

  defaultPlotConfiguration$titleSize <- 14
  defaultPlotConfiguration$xLabelSize <- 8
  defaultPlotConfiguration$yLabelSize <- 8
  defaultPlotConfiguration$xAxisLabelTicksSize <- 8
  defaultPlotConfiguration$yAxisLabelTicksSize <- 8
  defaultPlotConfiguration$legendTitleSize <- 6
  defaultPlotConfiguration$legendPosition <- tlf::LegendPositions$outsideTopRight

  return(defaultPlotConfiguration)
}

#' @title Create an instance of `PlotGridConfiguration` R6 class
#' @rdname createEsqlabsPlotGridConfiguration
#'
#' @description
#'
#' An instance of `PlotGridConfiguration` R6 class from `{tlf}` package is
#' needed for creating a grid of multiple visualizations created using the
#' `{ospsuite}` package.
#'
#' The default attributes of the class are chosen to reflect the corporate
#' standards adopted by esqLABS GmbH.
#'
#' @return An instance of `PlotGridConfiguration` R6 class.
#'
#' @examples
#' createEsqlabsPlotGridConfiguration()
#'
#' @family create-plotting-configurations
#'
#' @export
createEsqlabsPlotGridConfiguration <- function() {
  plotGridConfiguration <- tlf::PlotGridConfiguration$new()

  plotGridConfiguration$tagLevels <- "a"

  return(plotGridConfiguration)
}

#' @param projectConfiguration Object of class `ProjectConfiguration`
#' that contains information about the output paths.
#'
#' @title Create an instance of `ExportConfiguration` R6 class
#' @rdname createEsqlabsExportConfiguration
#'
#' @description
#'
#' An instance of `ExportConfiguration` R6 class from `{tlf}` package is needed
#' for saving the plots and plot grids created using the `{ospsuite}` package.
#'
#' The default attributes of the class are chosen to reflect the corporate
#' standards adopted by esqLABS GmbH.
#'
#' @return An instance of `ExportConfiguration` R6 class.
#'
#' @examples
#' myProjConfig <- ProjectConfiguration$new()
#' createEsqlabsExportConfiguration(myProjConfig)
#'
#' @family create-plotting-configurations
#'
#' @export
createEsqlabsExportConfiguration <- function(projectConfiguration) {
  exportConfiguration <- tlf::ExportConfiguration$new()

  exportConfiguration$path <- projectConfiguration$outputFolder
  exportConfiguration$dpi <- 300
  # NULL is not supported by ExportConfiguration, so we should assign here
  # something useful. NULL in the ProjectConfiguration currently means "do not
  # export".
  exportConfiguration$format <- projectConfiguration$outputDevice %||% "PNG"
  exportConfiguration$width <- 18
  exportConfiguration$height <- 18
  exportConfiguration$units <- "cm"

  return(exportConfiguration)
}

#' Generate plots as defined in excel file `projectConfiguration$plotsFile`
#'
#' @param simulatedScenarios A list of simulated scenarios as returned by `runScenarios()`
#' @param observedData A list of `DataSet` objects
#' @param projectConfiguration Object of class `ProjectConfiguration`
#' that contains information about the output paths and the excel file
#' where plots are defined.
#'
#'
#' @return A list of `ggplot` objects
#'
#' @export
plotsFromExcel <- function(simulatedScenarios, observedData, projectConfiguration) {
  validateIsOfType(observedData, DataSet)
  validateIsOfType(projectConfiguration, ProjectConfiguration)
  dfDataCombined <- readExcel(projectConfiguration$plotsFile, sheet = "DataCombined")

  # warnings for invalid data in plot definitions from excel
  # scenario not present in simulatedScenarios
  missingScenarios <- setdiff(setdiff(dfDataCombined$scenario, names(simulatedScenarios)), NA)
  if (length(missingScenarios) != 0) {
    warning(messages$warningInvalidScenarioName(missingScenarios, projectConfiguration$plotsFile))
    dfDataCombined <- dfDataCombined[!(dfDataCombined$scenario %in% missingScenarios),]
  }
  # data set name not present in observedData
  missingDataSets <- setdiff(setdiff(dfDataCombined$dataSet, names(observedData)), NA)
  if (length(missingDataSets) != 0) {
    warning(messages$warningInvalidDataSetName(missingDataSets, projectConfiguration$plotsFile))
    dfDataCombined <- dfDataCombined[!(dfDataCombined$dataSet %in% missingDataSets),]
  }

  # create named list of DataCombined objects
  dataCombinedNames <- unique(dfDataCombined$DataCombinedName)
  dataCombinedList <- list()
  for (name in dataCombinedNames) {
    dataCombinedList[[name]] <- DataCombined$new()
    # add data to DataCombined object
    # add simulated data
    simulated <- dplyr::filter(dfDataCombined, DataCombinedName == name, dataType == "simulated")
    if (nrow(simulated) > 0) {
      for (j in 1:nrow(simulated)) {
        dataCombinedList[[name]]$addSimulationResults(
          simulationResults = simulatedScenarios[[simulated[j,]$scenario]]$results,
          quantitiesOrPaths = simulated[j,]$path,
          groups = simulated[j,]$group,
          names = simulated[j,]$label)
      }
    }

    # add observed data
    observed <- dplyr::filter(dfDataCombined, DataCombinedName == name, dataType == "observed")
    if (nrow(observed) > 0) {
      dataSets <- observedData[observed$dataSet]
      dataCombinedList[[name]]$addDataSets(dataSets, groups = observed$group)
    }
  }

  # read sheet "plotConfiguration"
  dfPlotConfigurations <- readExcel(projectConfiguration$plotsFile, sheet = "plotConfiguration")
  # remove dataCombineds that have not been generated due to missing data
  # or are not defined in sheet "DataCombined"
  dfPlotConfigurations <- dfPlotConfigurations[dfPlotConfigurations$DataCombinedName %in% dfDataCombined$DataCombinedName,]
  # merge limit columns to character columns xAxisLimits and yAxisLimits
  dfPlotConfigurations <- dplyr::mutate(dfPlotConfigurations, xAxisLimits = ifelse(!is.na(xLimLower) & !is.na(xLimUpper),
                                                                                   paste0("c(", xLimLower, ",", xLimUpper, ")"), NA),
                                        xLimLower = NULL, xLimUpper = NULL)
  dfPlotConfigurations <- dplyr::mutate(dfPlotConfigurations, yAxisLimits = ifelse(!is.na(yLimLower) & !is.na(yLimUpper),
                                                                                   paste0("c(", yLimLower, ",", yLimUpper, ")"), NA),
                                        yLimLower = NULL, yLimUpper = NULL)

  # create a list of plotConfiguration objects as defined in sheet "plotConfiguration"
  plotConfigurationList <- apply(dfPlotConfigurations[, !(names(dfPlotConfigurations) %in% c("plotID", "DataCombinedName", "plotType"))],
                                 1, .plotConfigurationFromRow)

  # create a list of plots from dataCombinedList and plotConfigurationList
  plotList <- lapply(seq_along(plotConfigurationList), \(i) {
    dataCombined = dataCombinedList[[dfPlotConfigurations$DataCombinedName[i]]]
    switch (dfPlotConfigurations$plotType[i],
            individual = plotIndividualTimeProfile(dataCombined, plotConfigurationList[[i]]),
            population = plotPopulationTimeProfile(dataCombined, plotConfigurationList[[i]]),
            observedVsSimulated = plotObservedVsSimulated(dataCombined, plotConfigurationList[[i]]),
            residualsVsSimulated = plotResidualsVsSimulated(dataCombined, plotConfigurationList[[i]]),
            residualsVsTime = plotResidualsVsTime(dataCombined, plotConfigurationList[[i]])
    )
  })
  names(plotList) <- dfPlotConfigurations$plotID

  # read sheet "plotGrids" with info for plotGridConfigurations
  colTypes <- c("text", "text")
  dfPlotGrids <- readExcel(projectConfiguration$plotsFile, sheet = "plotGrids", col_types = colTypes)
  dfPlotGrids$plotIDs <- strsplit(dfPlotGrids$plotIDs, split = "[[:punct:]]")

  # create plotGridConfiguration objects and add plots from plotList
  validPlotIDs <- as.character(dfPlotConfigurations$plotID)
  multiPanelPlots <- apply(dfPlotGrids, 1, \(row) {
    plotGridConfiguration <- createEsqlabsPlotGridConfiguration()
    plotGridConfiguration$title <- row$title
    plotGridConfiguration$addPlots(plotList[intersect(row$plotIDs, validPlotIDs)])
    if (length(invalidPlotIDs <- setdiff(row$plotIDs, validPlotIDs)) != 0) {
      warning(messages$warningInvalidPlotID(invalidPlotIDs, row$title))
    }
    plotGrid(plotGridConfiguration)
  })
  names(multiPanelPlots) <- dfPlotGrids$title

  return(multiPanelPlots)
}

.plotConfigurationFromRow <- function(...) {
  plotConfiguration <- createEsqlabsPlotConfiguration()
  lapply(seq_along(...), function(i) {
    value <- ...[[i]]
    colName <- names(...)[[i]]
    if (!is.na(value)) {
      # numeric columns
      if (colName %in% c("xAxisLimits", "yAxisLimits")) {
        plotConfiguration[[colName]] <- eval(parse(text = value))
      # character columns
      } else {
        plotConfiguration[[colName]] <- value
      }
    }}
  )
  return(plotConfiguration)
}
