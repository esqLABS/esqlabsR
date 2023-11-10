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
#' @import grDevices
#' @return A list of colors as HEX values.
#' @import grDevices
#' @export
esqLABS_colors <- function(nrOfColors) {
  # esqLABS colors in HSV model
  esqRedHSV <- grDevices::rgb2hsv(234, 94, 94, maxColorValue = 255)
  esqBlueHSV <- grDevices::rgb2hsv(74, 189, 203, maxColorValue = 255)
  esqGreenHSV <- grDevices::rgb2hsv(118, 187, 96, maxColorValue = 255)
  # default color palette.
  esq_palette <- c(
    hsv(esqBlueHSV[1], esqBlueHSV[2], esqBlueHSV[3]),
    hsv(esqRedHSV[1], esqRedHSV[2], esqRedHSV[3]),
    hsv(esqGreenHSV[1], esqGreenHSV[2], esqGreenHSV[3])
  )

  # pre-calculate distances between blue and red and red and green.
  deltaH_b_r <- (esqRedHSV[1] - esqBlueHSV[1])
  deltaS_b_r <- max(esqRedHSV[2], esqBlueHSV[2]) - min(esqRedHSV[2], esqBlueHSV[2])
  deltaV_b_r <- max(esqRedHSV[3], esqBlueHSV[3]) - min(esqRedHSV[3], esqBlueHSV[3])

  deltaH_r_g <- abs(esqRedHSV[1] - (esqGreenHSV[1] + 1))
  deltaS_r_g <- max(esqRedHSV[2], esqGreenHSV[2]) - min(esqRedHSV[2], esqGreenHSV[2])
  deltaV_r_g <- max(esqRedHSV[3], esqGreenHSV[3]) - min(esqRedHSV[3], esqGreenHSV[3])

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

    h <- esqBlueHSV[1] + deltaH * i
    if (h > 1) {
      h <- h - 1
    }
    s <- min(esqBlueHSV[2], esqRedHSV[2]) + deltaS * i
    v <- min(esqBlueHSV[3], esqRedHSV[3]) + deltaV * i

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

      h <- esqRedHSV[1] + deltaH * i
      if (h > 1) {
        h <- h - 1
      }
      s <- min(esqGreenHSV[2], esqRedHSV[2]) + deltaS * i
      v <- min(esqGreenHSV[3], esqRedHSV[3]) + deltaV * i

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
#' @import ospsuite ospsuite.utils grDevices
#'
#' @examples
#' col2hsv("yellow")
#' @export
col2hsv <- function(color) {
  validateIsString(color)
  rgb <- col2rgb(color)
  return(grDevices::rgb2hsv(rgb))
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

  # Size
  defaultPlotConfiguration$titleSize <- 10
  defaultPlotConfiguration$xLabelSize <- 9
  defaultPlotConfiguration$yLabelSize <- 9
  defaultPlotConfiguration$xAxisLabelTicksSize <- 8
  defaultPlotConfiguration$yAxisLabelTicksSize <- 8
  defaultPlotConfiguration$legendKeysSize <- 6

  defaultPlotConfiguration$xLabelMargin <- c(10, 0, 0, 0)
  defaultPlotConfiguration$yLabelMargin <- c(0, 0, 10, 0)

  # Lines size
  defaultPlotConfiguration$linesSize <- 0.5

  # Points size
  defaultPlotConfiguration$pointsSize <- 1.75

  # Error bars size
  defaultPlotConfiguration$errorbarsSize <- 0.65
  defaultPlotConfiguration$errorbarsCapSize <- 2.75

  # Legend appearance
  # defaultPlotConfiguration$legendBorderColor <- "grey10"
  # defaultPlotConfiguration$legendBorderType <- 1
  defaultPlotConfiguration$legendPosition <- tlf::LegendPositions$outsideTopLeft

  # Axis appearance
  defaultPlotConfiguration$yAxisLabelTicksAngle <- 0

  # Colors
  defaultPlotConfiguration$pointsColor <- esqlabsEnv$colorPalette
  defaultPlotConfiguration$ribbonsFill <- esqlabsEnv$colorPalette
  defaultPlotConfiguration$linesColor <- esqlabsEnv$colorPalette

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
createEsqlabsPlotGridConfiguration <- function() { # nolint: object_length_linter.
  plotGridConfiguration <- tlf::PlotGridConfiguration$new()

  plotGridConfiguration$tagLevels <- "a"
  plotGridConfiguration$tagSize <- 11
  plotGridConfiguration$titleSize <- 12

  plotGridConfiguration$titleHorizontalJustification <- 0.5

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
createEsqlabsExportConfiguration <- function(projectConfiguration) { # nolint: object_length_linter.
  exportConfiguration <- esqlabsR::ExportConfiguration$new()

  exportConfiguration$path <- projectConfiguration$outputFolder
  exportConfiguration$dpi <- 300
  # NULL is not supported by ExportConfiguration, so we should assign here
  # something useful. NULL in the ProjectConfiguration currently means "do not
  # export".
  exportConfiguration$format <- "PNG"
  exportConfiguration$width <- 18
  # exportConfiguration$height <- 18
  exportConfiguration$heightPerRow <- 12
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
createPlotsFromExcel <- function(
    plotGridNames = NULL,
    simulatedScenarios,
    observedData,
    projectConfiguration,
    stopIfNotFound = TRUE) {
  validateIsOfType(observedData, "DataSet", nullAllowed = TRUE)
  validateIsOfType(projectConfiguration, "ProjectConfiguration")
  validateIsString(plotGridNames, nullAllowed = TRUE)

  # read sheet "plotGrids" with info for plotGridConfigurations
  dfPlotGrids <- readExcel(projectConfiguration$plotsFile,
    sheet = "plotGrids"
  )
  # read sheet "exportConfiguration"
  dfExportConfigurations <- readExcel(projectConfiguration$plotsFile,
    sheet = "exportConfiguration"
  ) %>%
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
  dfPlotConfigurations <- readExcel(projectConfiguration$plotsFile,
    sheet = "plotConfiguration"
  )

  # Filter and validate plotGrids
  dfPlotGrids <- dplyr::filter(dfPlotGrids, !if_all(everything(), is.na))
  dfPlotGrids <- .validatePlotGridsFromExcel(dfPlotGrids, unique(dfPlotConfigurations$plotID))

  # Filter and validate only used plot configurations
  dfPlotConfigurations <- dplyr::filter(dfPlotConfigurations, plotID %in% unlist(unique(dfPlotGrids$plotIDs)))

  # Filter and validate only used data combined
  dataCombinedList <- createDataCombinedFromExcel(
    file = projectConfiguration$plotsFile,
    sheet = "DataCombined",
    dataCombinedNames = unique(dfPlotConfigurations$DataCombinedName),
    simulatedScenarios = simulatedScenarios,
    observedData = observedData,
    stopIfNotFound = stopIfNotFound
  )

  dfPlotConfigurations <- .validatePlotConfigurationFromExcel(dfPlotConfigurations, names(dataCombinedList))

  # create a list of plotConfiguration objects as defined in sheet "plotConfiguration"
  defaultPlotConfiguration <- createEsqlabsPlotConfiguration()
  plotConfigurationList <- apply(dfPlotConfigurations, 1, \(row){
    plotConfiguration <- .createConfigurationFromRow(
      defaultConfiguration = defaultPlotConfiguration,
      # Have to exclude all columns that should not be vectorized
      row[!(names(row) %in% c(
        "plotID",
        "DataCombinedName",
        "plotType",
        "title",
        "subtitle",
        "xLabel",
        "yLabel",
        "aggregation",
        "quantiles",
        "nsd",
        "foldDistance"
      ))]
    )
    if (!is.na(row[["title"]])) {
      plotConfiguration$title <- row[["title"]]
    }
    return(plotConfiguration)
  })
  names(plotConfigurationList) <- dfPlotConfigurations$plotID

  # create a list of plots from dataCombinedList and plotConfigurationList
  plotList <- lapply(dfPlotConfigurations$plotID, \(plotId) {
    dataCombined <- dataCombinedList[[dfPlotConfigurations[dfPlotConfigurations$plotID == plotId, ]$DataCombinedName]]
    switch(dfPlotConfigurations[dfPlotConfigurations$plotID == plotId, ]$plotType,
      # Individual time profile
      individual = plotIndividualTimeProfile(dataCombined, plotConfigurationList[[plotId]]),
      # Population time profile
      population = {
        aggregation <- dfPlotConfigurations[dfPlotConfigurations$plotID == plotId, ]$aggregation
        quantiles <- dfPlotConfigurations[dfPlotConfigurations$plotID == plotId, ]$quantiles
        nsd <- dfPlotConfigurations[dfPlotConfigurations$plotID == plotId, ]$nsd
        args <- list()
        args$dataCombined <- dataCombined
        args$defaultPlotConfiguration <- plotConfigurationList[[plotId]]
        # Is aggregation defined?
        if (!is.null(aggregation) && !is.na(aggregation)) {
          args$aggregation <- aggregation
        }
        # quantiles defined?
        if (!is.null(quantiles) && !is.na(quantiles)) {
          args$quantiles <- as.numeric(unlist(strsplit(quantiles, split = ",")))
        }
        # if nsd is defined, add it to the args
        if (!is.null(nsd) && !is.na(nsd)) {
          args$nsd <- as.numeric(nsd)
        }
        do.call(plotPopulationTimeProfile, args)
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
    plotGridConfiguration <- .createConfigurationFromRow(
      defaultConfiguration = defaultPlotGridConfig,
      row[!(names(row) %in% c("name", "plotIDs", "title"))]
    )

    if (!is.na(row$title)) {
      plotGridConfiguration$title <- row$title
    }

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

  ## Remove rows that are entirely empty
  dfExportConfigurations <- dplyr::filter(dfExportConfigurations, !if_all(everything(), is.na))
  dfExportConfigurations <- .validateExportConfigurationsFromExcel(dfExportConfigurations, plotGrids)
  if (nrow(dfExportConfigurations) > 0) {
    # create a list of ExportConfiguration objects from dfExportConfigurations
    defaultExportConfiguration <- createEsqlabsExportConfiguration(projectConfiguration)
    exportConfigurations <- apply(dfExportConfigurations, 1, \(row){
      exportConfiguration <- .createConfigurationFromRow(
        defaultConfiguration = defaultExportConfiguration,
        row[!(names(row) %in% c("plotGridName", "name"))]
      )
      # Replace "\" and "/" by "_" so the file name does not result in folders
      name <- row[["name"]]
      name <- gsub(pattern = "\\", "_", name, fixed = TRUE)
      name <- gsub(pattern = "/", "_", name, fixed = TRUE)
      exportConfiguration$name <- name
      return(exportConfiguration)
    })
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
      # Alternatively, the values can be enclosed in "" in case the title should contain a ','.
      # Split the input string by ',' but do not split within ""
      value <- unlist(trimws(scan(
        text = as.character(value), what = "character", sep = ",",
        quiet = TRUE
      )))

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
      if (colName %in% c(
        "xAxisLimits", "yAxisLimits",
        "xValuesLimits", "yValuesLimits"
      )) {
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

  # plotIDs must be unique
  duplicated_plotIDs <- dfPlotConfigurations$plotID[duplicated(dfPlotConfigurations$plotID)]
  if (length(duplicated_plotIDs) > 0) {
    stop(messages$PlotIDsMustBeUnique(duplicated_plotIDs))
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

  # plotGrids names must be unique
  duplicated_PlotGridsNames <- dfPlotGrids$name[duplicated(dfPlotGrids$name)]
  if (length(duplicated_PlotGridsNames) > 0) {
    stop(messages$PlotGridsNamesMustBeUnique(duplicated_PlotGridsNames))
  }

  # The values can be enclosed in "" in case the title should contain a ','.
  # Split the input string by ',' but do not split within ""
  # Have to do it one row at a time, otherwise it returns one separate list entry
  # for each plot it (and not lists of plot ids)
  dfPlotGrids$plotIDs <- lapply(dfPlotGrids$plotIDs, \(plotId){
    unlist(trimws(scan(
      text = as.character(plotId), what = "character", sep = ",",
      quiet = TRUE
    )))
  })

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
