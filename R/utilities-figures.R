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
#' @param plotGridNames Names of the plot grid specified in the sheet `plotGrids`
#' for which the figures will be created. If `NULL` (default), all plot grids
#' specified in the excel sheet will be created. If a plot grid with a given name
#' does not exist, an error is thrown.
#'
#' @return A list of `ggplot` objects
#'
#' @import tidyr
#'
#' @export
createPlotsFromExcel <- function(plotGridNames = NULL, simulatedScenarios, observedData, projectConfiguration, stopIfNotFound = TRUE) {
  validateIsOfType(observedData, "DataSet", nullAllowed = TRUE)
  validateIsOfType(projectConfiguration, "ProjectConfiguration")
  validateIsString(plotGridNames, nullAllowed = TRUE)

  # read sheet "plotGrids" with info for plotGridConfigurations
  dfPlotGrids <- readExcel(file.path(projectConfiguration$paramsFolder, projectConfiguration$plotsFile), sheet = "plotGrids")
  # read sheet "exportConfiguration"
  dfExportConfigurations <- readExcel(file.path(projectConfiguration$paramsFolder, projectConfiguration$plotsFile), sheet = "exportConfiguration") %>%
    rename(name = outputName)
  # Filter for only specified plot grids
  if (!is.null(plotGridNames)) {
    # Throw an error if a plot grid name that is passed is not defined in the excel file
    missingPlotGrids <- setdiff(plotGridNames, unique(dfPlotGrids$name))
    if (length(missingPlotGrids) != 0) {
      stop(messages$invalidPlotGridNames(missingPlotGrids))
    }

    dfPlotGrids <- dplyr::filter(dfPlotGrids, name %in% plotGridNames)
    dfExportConfigurations <- dplyr::filter(dfExportConfigurations, plotGridName %in% plotGridNames)
  }

  # Exit early if no PlotGrid is defined
  if (dim(dfPlotGrids)[[1]] == 0) {
    return()
  }
  # read sheet "plotConfiguration"
  dfPlotConfigurations <- readExcel(file.path(projectConfiguration$paramsFolder, projectConfiguration$plotsFile), sheet = "plotConfiguration")

  # Pre-process the sheet 'plotGrids'
  dfPlotGrids <- .validatePlotGridsFromExcel(dfPlotGrids, unique(dfPlotConfigurations$plotID))
  # Filter and validate only used plot configurations
  dfPlotConfigurations <- dplyr::filter(dfPlotConfigurations, plotID %in% unlist(unique(dfPlotGrids$plotIDs)))
  # Filter and validate only used data combined
  dataCombinedList <- createDataCombinedFromExcel(file.path(projectConfiguration$paramsFolder, projectConfiguration$plotsFile),
                                                  sheet = "DataCombined", dataCombinedNames = unique(dfPlotConfigurations$DataCombinedName),
                                                  simulatedScenarios, observedData, stopIfNotFound)
  dfPlotConfigurations <- .validatePlotConfigurationFromExcel(dfPlotConfigurations, names(dataCombinedList))

  # create a list of plotConfiguration objects as defined in sheet "plotConfiguration"
  plotConfiguration <- createEsqlabsPlotConfiguration()
  plotConfigurationList <- apply(
    dfPlotConfigurations[, !(names(dfPlotConfigurations) %in% c("plotID", "DataCombinedName", "plotType", "quantiles", "foldDistance"))],
    1, .createConfigurationFromRow,
    defaultConfiguration = plotConfiguration
  )
  names(plotConfigurationList) <- dfPlotConfigurations$plotID

  # create a list of plots from dataCombinedList and plotConfigurationList
  plotList <- lapply(dfPlotConfigurations$plotID, \(plotId) {
    dataCombined <- dataCombinedList[[dfPlotConfigurations[dfPlotConfigurations$plotID == plotId, ]$DataCombinedName]]
    switch(dfPlotConfigurations[dfPlotConfigurations$plotID == plotId, ]$plotType,
      individual = plotIndividualTimeProfile(dataCombined, plotConfigurationList[[plotId]]),
      population = {
        quantiles <- dfPlotConfigurations[dfPlotConfigurations$plotID == plotId, ]$quantiles
        if (is.na(quantiles)) {
          plotPopulationTimeProfile(dataCombined, plotConfigurationList[[plotId]])
        } else {
          plotPopulationTimeProfile(dataCombined, plotConfigurationList[[plotId]],
            quantiles = as.numeric(unlist(strsplit(quantiles, split = ",")))
          )
        }
      },
      observedVsSimulated = {
        foldDist <- dfPlotConfigurations[dfPlotConfigurations$plotID == plotId, ]$foldDistance
        if (is.na(foldDist)) {
          plotObservedVsSimulated(dataCombined, plotConfigurationList[[plotId]])
        } else {
          plotObservedVsSimulated(dataCombined, plotConfigurationList[[plotId]],
            foldDistance = as.numeric(unlist(strsplit(foldDist, split = ",")))
          )
        }
      },
      residualsVsSimulated = plotResidualsVsSimulated(dataCombined, plotConfigurationList[[plotId]]),
      residualsVsTime = plotResidualsVsTime(dataCombined, plotConfigurationList[[plotId]])
    )
  })
  names(plotList) <- dfPlotConfigurations$plotID

  # create plotGridConfiguration objects and add plots from plotList
  defaultPlotGridConfig <- createEsqlabsPlotGridConfiguration()
  plotGrids <- apply(dfPlotGrids, 1, \(row) {
    plotGridConfiguration <- .createConfigurationFromRow(defaultConfiguration = defaultPlotGridConfig,
                                                         row[!(names(row) %in% c("name", "plotIDs"))])

    plotsToAdd <- plotList[intersect(unlist(row$plotIDs), dfPlotConfigurations$plotID)]
    # Have to remove NULL instances. NULL can be produced e.g. when trying to create
    # a simulated vs observed plot without any groups
    plotsToAdd <- plotsToAdd[lengths(plotsToAdd) != 0]
    # Cannot create a plot grid if no plots are added. Skip
    if (length(plotsToAdd) == 0) {
      return(NULL)
    }
    # When only one plot is in the grid, do not show panel labels
    if (length(plotsToAdd) == 1) {
      plotGridConfiguration$tagLevels <- NULL
    }
    plotGridConfiguration$addPlots(plots = plotsToAdd)
    if (length(invalidPlotIDs <- setdiff(unlist(row$plotIDs), dfPlotConfigurations$plotID)) != 0) {
      warning(messages$warningInvalidPlotID(invalidPlotIDs, row$title))
    }
    plotGrid(plotGridConfiguration)
  })
  names(plotGrids) <- dfPlotGrids$name

  dfExportConfigurations <- .validateExportConfigurationsFromExcel(dfExportConfigurations, plotGrids)
  if (nrow(dfExportConfigurations) > 0) {
    # create a list of ExportConfiguration objects from dfExportConfigurations
    exportConfiguration <- createEsqlabsExportConfiguration(projectConfiguration)
    exportConfigurations <- apply(select(dfExportConfigurations, -plotGridName), 1, .createConfigurationFromRow, defaultConfiguration = exportConfiguration)
    # export plotGrid if defined in exportConfigurations
    lapply(seq_along(exportConfigurations), function(i) {
      exportConfigurations[[i]]$savePlot(plotGrids[[dfExportConfigurations$plotGridName[i]]])
    })
  }

  return(plotGrids)
}

#' Create a plotConfiguration or exportConfiguration objects from a row of sheet
#' 'plotConfiguration' or 'exportConfiguration'
#'
#' @param defaultConfiguration default plotConfiguration or exportConfiguration
#' @param ... row with configuration properties
#' @return A customized plot- or exportConfiguration object
#' @keywords internal
.createConfigurationFromRow <- function(defaultConfiguration, ...) {
  columns <- c(...)
  newConfiguration <- defaultConfiguration$clone()

  lapply(seq_along(columns), function(i) {
    value <- columns[[i]]
    colName <- names(columns)[[i]]
    if (!is.na(value)) {
      # Check if the field name is supported by the configuration class
      if (!.validateClassHasField(object = newConfiguration, field = colName)) {
        stop(messages$invalidConfigurationPropertyFromExcel(
          propertyName = colName,
          configurationType = class(newConfiguration)[[1]]
        ))
      }
      # For fields that require multiple values (e.g., axis limits require the
      # upper and the lower limit value), values are separated by a ','.
      # Split the input string first
      value <- unlist(strsplit(value, split = ","))

      # Expected type of the field to cast the value to the
      # correct type. For fields that do not have a default value (NULL), we have
      # to assume character until a better solution is found
      expectedType <- "character"
      # Try to get the expected type of the field from the default value
      defVal <- newConfiguration[[colName]]
      if (!is.null(defVal)) {
        expectedType <- typeof(defVal)
      }
      # Special treatment for axis limits, as we know their data type but cannot
      # get it with the proposed generic way since default limits are not set
      if (colName %in% c("xAxisLimits", "yAxisLimits")) {
        expectedType <- "double"
      }

      # Caste the value and set it
      newConfiguration[[colName]] <- as(
        object = value,
        Class = expectedType
      )
    }
  })

  return(newConfiguration)
}


#' Validate and process the 'plotConfiguration' sheet
#'
#' @param dfPlotConfigurations Data frame created by reading the ' plotConfiguration' sheet
#' @param dataCombinedNames Names of the 'DataCombined' that are referenced in the plot configurations
#'
#' @return Processed `dfPlotConfigurations`
#' @keywords internal
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

  return(dfPlotConfigurations)
}

#' Validate and process the 'plotGrids' sheet
#'
#' @param dfPlotGrids Data frame created by reading the ' plotGrids' sheet
#' @param plotIDs IDs of the plots that are referenced in the plot grids
#'
#' @return Processed `dfPlotGrids`
#' @keywords internal
.validatePlotGridsFromExcel <- function(dfPlotGrids, plotIDs) {
  # mandatory column plotIDs is empty - throw error
  missingLabel <- sum(is.na(dfPlotGrids$plotIDs))
  if (missingLabel > 0) {
    stop(messages$missingPlotIDs())
  }
  # Remove white spaces
  dfPlotGrids$plotIDs <- strsplit(x = dfPlotGrids$plotIDs, split = ",", fixed = TRUE)
  # Remove leading/trailing whitespaces
  dfPlotGrids$plotIDs <- lapply(dfPlotGrids$plotIDs, \(x){trimws(x)})

  # plotIDs that are not defined in the plotConfiguration sheet. Stop if any.
  missingPlots <- setdiff(setdiff(unique(unlist(dfPlotGrids$plotIDs)), plotIDs), NA)
  if (length(missingPlots) != 0) {
    stop(messages$errorInvalidPlotID(missingPlots))
  }

  return(dfPlotGrids)
}

#' Validate and process the 'exportConfiguration' sheet
#'
#' @param dfExportConfigurations Data frame created by reading the 'exportConfiguration' sheet
#' @param plotGrids List of multipanel plots created previously
#'
#' @return Processed `dfExportConfigurations`
#' @keywords internal
.validateExportConfigurationsFromExcel <- function(dfExportConfigurations, plotGrids) {
  # mandatory column outputName is empty - throw warning, remove rows
  missingName <- sum(is.na(dfExportConfigurations$name))
  if (missingName > 0) {
    dfExportConfigurations <- dfExportConfigurations[!is.na(dfExportConfigurations$name), ]
    warning(messages$missingOutputFileName())
  }

  plotGrids <- purrr::compact(plotGrids)
  missingPlotGrids <- setdiff(dfExportConfigurations$plotGridName, names(plotGrids))
  if (length(missingPlotGrids) != 0) {
    dfExportConfigurations <- dfExportConfigurations[!(dfExportConfigurations$plotGridName %in% missingPlotGrids), ]
    warning(messages$missingPlotGrids(missingPlotGrids))
  }

  return(dfExportConfigurations)
}
