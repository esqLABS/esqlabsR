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
  exportConfiguration$format <- "PNG"
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
#' @param stopIfNotFound If TRUE (default), the function stops if any of the
#' simulated results or observed data are not found. If FALSE a warning is printed.
#'
#'
#' @return A list of `ggplot` objects
#'
#' @import tidyr
#'
#' @export
createPlotsFromExcel <- function(simulatedScenarios, observedData, projectConfiguration, stopIfNotFound = TRUE) {
  validateIsOfType(observedData, "DataSet", nullAllowed = TRUE)
  validateIsOfType(projectConfiguration, "ProjectConfiguration")
  # read sheet "plotGrids" with info for plotGridConfigurations
  dfPlotGrids <- readExcel(file.path(projectConfiguration$paramsFolder, projectConfiguration$plotsFile), sheet = "plotGrids")
  # Exit early if no PlotGrid is defined
  if (dim(dfPlotGrids)[[1]] == 0) {
    return()
  }
  # read sheet "DataCombined"
  dfDataCombined <- readExcel(file.path(projectConfiguration$paramsFolder, projectConfiguration$plotsFile), sheet = "DataCombined")
  # read sheet "plotConfiguration"
  dfPlotConfigurations <- readExcel(file.path(projectConfiguration$paramsFolder, projectConfiguration$plotsFile), sheet = "plotConfiguration")
  # read sheet "exportConfiguration"
  dfExportConfigurations <- readExcel(file.path(projectConfiguration$paramsFolder, projectConfiguration$plotsFile), sheet = "exportConfiguration") %>%
    rename(name = outputName)
  dfDataCombined <- .validateDataCombinedFromExcel(dfDataCombined, simulatedScenarios, observedData, stopIfNotFound)
  dataCombinedNames <- unique(dfDataCombined$DataCombinedName)
  dfPlotConfigurations <- .validatePlotConfigurationFromExcel(dfPlotConfigurations, dataCombinedNames)
  dfPlotGrids <- .validatePlotGridsFromExcel(dfPlotGrids, unique(dfPlotConfigurations$plotID))

  # Only consider plotIDs that are specified in the plot grids
  validPlotIDs <- as.character(unique(unlist(dfPlotGrids$plotIDs)))
  # create a list of plotConfiguration objects as defined in sheet "plotConfiguration"
  plotConfiguration <- createEsqlabsPlotConfiguration()
  plotConfigurationList <- apply(
    dfPlotConfigurations[dfPlotConfigurations$plotID %in% validPlotIDs, !(names(dfPlotConfigurations) %in% c("plotID", "DataCombinedName", "plotType"))],
    1, .createConfigurationFromRow,
    defaultConfiguration = plotConfiguration
  )
  names(plotConfigurationList) <- validPlotIDs

  # Valid DataCombined. Only consider those that are used in plots
  validDataCombined <- unique(dfPlotConfigurations[dfPlotConfigurations$plotID %in% validPlotIDs, ]$DataCombinedName)

  # create named list of DataCombined objects. Only create for DataCombined that are used in plotConfiguration
  dataCombinedList <- lapply(validDataCombined, \(name) {
    dataCombined <- DataCombined$new()
    # add data to DataCombined object
    # add simulated data
    simulated <- filter(dfDataCombined, DataCombinedName == name, dataType == "simulated")
    if (nrow(simulated) > 0) {
      for (j in 1:nrow(simulated)) {
        dataCombined$addSimulationResults(
          simulationResults = simulatedScenarios[[simulated[j, ]$scenario]]$results,
          quantitiesOrPaths = simulated[j, ]$path,
          groups = simulated[j, ]$group,
          names = simulated[j, ]$label
        )
      }
    }

    # add observed data
    observed <- filter(dfDataCombined, DataCombinedName == name, dataType == "observed")
    if (nrow(observed) > 0) {
      dataSets <- observedData[observed$dataSet]
      dataCombined$addDataSets(dataSets, names = observed$label, groups = observed$group)
    }
    dataCombined
  })
  names(dataCombinedList) <- validDataCombined

  # apply data transformations
  dfTransform <- filter(dfDataCombined, !is.na(xOffsets) | !is.na(yOffsets) | !is.na(xScaleFactors) | !is.na(yScaleFactors)) %>%
    replace_na(list(xOffsets = 0, yOffsets = 0, xScaleFactors = 1, yScaleFactors = 1))
  # Apply data transformations if specified in the excel file
  if (dim(dfTransform)[[1]] != 0) {
    apply(dfTransform, 1, \(row) {
      dataCombinedList[[row[["DataCombinedName"]]]]$setDataTransformations(
        forNames = row[["label"]], xOffsets = as.numeric(row[["xOffsets"]]), yOffsets = as.numeric(row[["yOffsets"]]),
        xScaleFactors = as.numeric(row[["xScaleFactors"]]), yScaleFactors = as.numeric(row[["yScaleFactors"]])
      )
    })
  }

  # create a list of plots from dataCombinedList and plotConfigurationList
  plotList <- lapply(validPlotIDs, \(plotId) {
    dataCombined <- dataCombinedList[[dfPlotConfigurations[dfPlotConfigurations$plotID == plotId, ]$DataCombinedName]]
    switch(dfPlotConfigurations[dfPlotConfigurations$plotID == plotId, ]$plotType,
      individual = plotIndividualTimeProfile(dataCombined, plotConfigurationList[[plotId]]),
      population = plotPopulationTimeProfile(dataCombined, plotConfigurationList[[plotId]]),
      observedVsSimulated = plotObservedVsSimulated(dataCombined, plotConfigurationList[[plotId]]),
      residualsVsSimulated = plotResidualsVsSimulated(dataCombined, plotConfigurationList[[plotId]]),
      residualsVsTime = plotResidualsVsTime(dataCombined, plotConfigurationList[[plotId]])
    )
  })
  names(plotList) <- validPlotIDs

  # create plotGridConfiguration objects and add plots from plotList
  multiPanelPlots <- apply(dfPlotGrids, 1, \(row) {
    plotGridConfiguration <- createEsqlabsPlotGridConfiguration()
    plotGridConfiguration$title <- row$title
    plotsToAdd <- plotList[intersect(unlist(row$plotIDs), validPlotIDs)]
    # Have to remove NULL instances. NULL can be produced e.g. when trying to create
    # a simulated vs observed plot without any groups
    plotsToAdd <- plotsToAdd[lengths(plotsToAdd) != 0]
    # Cannot create a plot grid if no plots are added. Skip
    if (length(plotsToAdd) == 0) {
      return(NULL)
    }
    plotGridConfiguration$addPlots(plots = plotsToAdd)
    if (length(invalidPlotIDs <- setdiff(unlist(row$plotIDs), validPlotIDs)) != 0) {
      warning(messages$warningInvalidPlotID(invalidPlotIDs, row$title))
    }
    plotGrid(plotGridConfiguration)
  })
  names(multiPanelPlots) <- dfPlotGrids$name

  if (nrow(dfExportConfigurations) > 0) {
    # create a list of ExportConfiguration objects from dfExportConfigurations
    exportConfiguration <- createEsqlabsExportConfiguration(projectConfiguration)
    exportConfigurations <- apply(select(dfExportConfigurations, -plotGridName), 1, .createConfigurationFromRow, defaultConfiguration = exportConfiguration)
    # export plotGrid if defined in exportConfigurations
    lapply(seq_along(exportConfigurations), function(i) {
      exportConfigurations[[i]]$savePlot(multiPanelPlots[[dfExportConfigurations$plotGridName[i]]])
    })
  }

  return(multiPanelPlots)
}

#' @keywords internal
.createConfigurationFromRow <- function(defaultConfiguration, ...) {
  columns <- c(...)
  newConfiguration <- defaultConfiguration$clone()

  lapply(seq_along(columns), function(i) {
    value <- columns[[i]]
    colName <- names(columns)[[i]]
    if (!is.na(value)) {
      # Check if the field name is supported by the configuration class
      if (!.validateClassHasField(object = newConfiguration, field = colName)){
        stop(messages$invalidConfigurationPropertyFromExcel(propertyName = colName,
                                                            configurationType = class(newConfiguration)[[1]]))
      }
      # columns with single numeric values and numeric vectors
      if (colName %in% c("xAxisLimits", "yAxisLimits", "width", "height", "dpi")) {
        newConfiguration[[colName]] <- eval(parse(text = value))
        # character columns
      } else {
        newConfiguration[[colName]] <- value
      }
    }
  })

  return(newConfiguration)
}

.validateDataCombinedFromExcel <- function(dfDataCombined, simulatedScenarios, observedData, stopIfNotFound) {
  # mandatory column label is empty - throw error
  missingLabel <- sum(is.na(dfDataCombined$label))
  if (missingLabel > 0) {
    stop(messages$missingLabel())
  }

  # mandatory column dataType is empty - throw error
  missingLabel <- sum(is.na(dfDataCombined$dataType))
  if (missingLabel > 0) {
    stop(messages$missingDataType())
  }

  # dataType == simulated, but no scenario defined - throw error
  missingLabel <- sum(is.na(dfDataCombined[dfDataCombined$dataType == "simulated", ]$scenario))
  if (missingLabel > 0) {
    stop(messages$missingScenarioName())
  }

  # dataType == simulated, but no path defined - throw error
  missingLabel <- is.na(dfDataCombined[dfDataCombined$dataType == "simulated", ]$path)
  if (sum(missingLabel) > 0) {
    stop(messages$stopNoPathProvided(dfDataCombined[missingLabel & dfDataCombined$dataType == "simulated", ]$DataCombinedName))
  }

  # dataType == observed, but no data set defined - throw error
  missingLabel <- is.na(dfDataCombined[dfDataCombined$dataType == "observed", ]$dataSet)
  if (sum(missingLabel) > 0) {
    stop(messages$stopNoDataSetProvided(dfDataCombined[missingLabel & dfDataCombined$dataType == "observed", ]$DataCombinedName))
  }

  # warnings for invalid data in plot definitions from excel
  # scenario not present in simulatedScenarios
  missingScenarios <- setdiff(setdiff(dfDataCombined$scenario, names(simulatedScenarios)), NA)
  if (length(missingScenarios) != 0) {
    if (stopIfNotFound) {
      stop(messages$stopInvalidScenarioName(missingScenarios))
    }
    warning(messages$warningInvalidScenarioName(missingScenarios))
    dfDataCombined <- dfDataCombined[!(dfDataCombined$scenario %in% missingScenarios), ]
  }
  # data set name not present in observedData
  missingDataSets <- setdiff(setdiff(dfDataCombined$dataSet, names(observedData)), NA)
  if (length(missingDataSets) != 0) {
    if (stopIfNotFound) {
      stop(messages$stopInvalidDataSetName(missingDataSets))
    }
    warning(messages$warningInvalidDataSetName(missingDataSets))
    dfDataCombined <- dfDataCombined[!(dfDataCombined$dataSet %in% missingDataSets), ]
  }

  return(dfDataCombined)
}

.validatePlotConfigurationFromExcel <- function(dfPlotConfigurations, dataCombinedNames) {
  # mandatory column DataCombinedName is empty - throw error
  missingLabel <- sum(is.na(dfPlotConfigurations$DataCombinedName))
  if (missingLabel > 0) {
    stop(messages$missingDataCombinedName())
  }

  # mandatory column plotType is empty - throw error
  missingLabel <- sum(is.na(dfPlotConfigurations$plotType))
  if (missingLabel > 0) {
    stop(messages$missingPlotType())
  }

  # DataCombined that are not defined in the DataCombined sheet. Stop if any.
  missingDataCombined <- setdiff(setdiff(dfPlotConfigurations$DataCombinedName, dataCombinedNames), NA)
  if (length(missingDataCombined) != 0) {
    stop(messages$stopInvalidDataCombinedName(missingDataCombined))
  }

  # merge limit columns to character columns xAxisLimits and yAxisLimits
  dfPlotConfigurations <- mutate(dfPlotConfigurations,
    xAxisLimits = ifelse(!is.na(xLimLower) & !is.na(xLimUpper),
      paste0("c(", xLimLower, ",", xLimUpper, ")"), NA
    ),
    xLimLower = NULL, xLimUpper = NULL
  )
  dfPlotConfigurations <- mutate(dfPlotConfigurations,
    yAxisLimits = ifelse(!is.na(yLimLower) & !is.na(yLimUpper),
      paste0("c(", yLimLower, ",", yLimUpper, ")"), NA
    ),
    yLimLower = NULL, yLimUpper = NULL
  )

  return(dfPlotConfigurations)
}

.validatePlotGridsFromExcel <- function(dfPlotGrids, plotIDs) {
  # mandatory column plotIDs is empty - throw error
  missingLabel <- sum(is.na(dfPlotGrids$plotIDs))
  if (missingLabel > 0) {
    stop(messages$missingPlotIDs())
  }
  # Remove white spaces
  dfPlotGrids$plotIDs <- gsub(dfPlotGrids$plotIDs, pattern = " ", replacement = "", fixed = TRUE)
  dfPlotGrids$plotIDs <- strsplit(dfPlotGrids$plotIDs, split = ",")

  # plotIDs that are not defined in the plotConfiguration sheet. Stop if any.
  missingPlots <- setdiff(setdiff(unique(unlist(dfPlotGrids$plotIDs)), plotIDs), NA)
  if (length(missingPlots) != 0) {
    stop(messages$errorInvalidPlotID(missingPlots))
  }

  return(dfPlotGrids)
}
