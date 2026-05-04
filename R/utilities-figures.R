#' esqLABS color palette
#'
#' Returns the list of colors extrapolated between the esqLABS colors blue, red,
#' and green.
#'
#' For `nrOfColors` == 1, the esqLABS-blue is returned For `nrOfColors` == 2,
#' the esqLABS-blue and green are returned For `nrOfColors` == 3, the
#' esqLABS-blue, red, and green are returned For `nrOfColors` > 3, the three
#' esqLABS colors are fixed, and the remaining colors are extrapolated from blue
#' to red to green. If `nrOfColors` is uneven, the blue-to-red section becomes
#' one color more than the red-to-green section. In this implementation,
#' blue-to-green is not considered.
#'
#' @param nrOfColors Positive integer defining the number of colors to be
#'   generated.
#'
#' @import grDevices
#' @returns A list of colors as HEX values.
#' @import grDevices
#' @export
esqlabsColors <- function(nrOfColors) {
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
  deltaS_b_r <- max(esqRedHSV[2], esqBlueHSV[2]) -
    min(esqRedHSV[2], esqBlueHSV[2])
  deltaV_b_r <- max(esqRedHSV[3], esqBlueHSV[3]) -
    min(esqRedHSV[3], esqBlueHSV[3])

  deltaH_r_g <- abs(esqRedHSV[1] - (esqGreenHSV[1] + 1))
  deltaS_r_g <- max(esqRedHSV[2], esqGreenHSV[2]) -
    min(esqRedHSV[2], esqGreenHSV[2])
  deltaV_r_g <- max(esqRedHSV[3], esqGreenHSV[3]) -
    min(esqRedHSV[3], esqGreenHSV[3])

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

  return(palette) # nolint: return_linter.
}

#' Returns the HSV values for a given R color name
#'
#' @param color vector of any of the three kinds of R color specifications,
#'   i.e., either a color name (as listed by colors()), a hexadecimal string of
#'   the form "#rrggbb" or "#rrggbbaa" (see rgb), or a positive integer `i`
#'   meaning `palette()[i]`.
#'
#' @returns A matrix with a column for each color. The three rows of the matrix
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

#' @title Create a `DefaultPlotConfiguration` list of properties
#' @rdname createEsqlabsPlotConfiguration
#'
#' @description
#'
#' A list of plot properties needed for creating visualizations with the `{ospsuite}` package.
#'
#' The default attributes of the class are chosen to reflect the corporate
#' standards adopted by esqLABS GmbH.
#'
#' @param plotType Optional one of `"timeProfiles"`, `"spiderPlot"`, `"tornadoPlot` adding plot type specific settings
#'
#' @returns A named list
#'
#' @examples
#' createEsqlabsPlotConfiguration()
#'
#' @family create-plotting-configurations
#'
#' @export
createEsqlabsPlotConfiguration <- function(plotType = NULL) {
  defaultPlotConfiguration <- list()
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
  defaultPlotConfiguration$legendPosition <- "top"
  defaultPlotConfiguration$legendJustification <- "left"

  # Axis appearance
  defaultPlotConfiguration$yAxisLabelTicksAngle <- 0

  # Colors
  defaultPlotConfiguration$pointsColor <- esqlabsEnv$colorPalette
  defaultPlotConfiguration$ribbonsFill <- esqlabsEnv$colorPalette
  defaultPlotConfiguration$linesColor <- esqlabsEnv$colorPalette

  # Add specific default properties for spiderPlot, tornadoPlot and timeProfiles
  plotConfigurationForType <- .plotConfigurationFromType(plotType)
  ospsuite.utils::validateIsOfType(defaultPlotConfiguration, "list")
  for (propertyName in names(plotConfigurationForType)) {
    defaultPlotConfiguration[[propertyName]] <- defaultPlotConfiguration[[
      propertyName
    ]] %||%
      plotConfigurationForType[[propertyName]]
  }
  return(defaultPlotConfiguration)
}

#' @title Create an instance of `PlotGridConfiguration` R6 class
#' @rdname createEsqlabsPlotGridConfiguration
#'
#' @description
#'
#' An instance of `PlotGridConfiguration` R6 class is
#' needed for creating a grid of multiple visualizations created using the
#' `{ospsuite}` package.
#'
#' The default attributes of the class are chosen to reflect the corporate
#' standards adopted by esqLABS GmbH.
#'
#' @returns An instance of `PlotGridConfiguration` R6 class.
#'
#' @examples
#' createEsqlabsPlotGridConfiguration()
#'
#' @family create-plotting-configurations
#'
#' @export
createEsqlabsPlotGridConfiguration <- function() {
  # nolint: object_length_linter.
  plotGridConfiguration <- PlotGridConfiguration$new()

  plotGridConfiguration$tagLevels <- "a"
  plotGridConfiguration$tagSize <- 11
  plotGridConfiguration$titleSize <- 12

  plotGridConfiguration$titleHorizontalJustification <- 0.5

  return(plotGridConfiguration)
}

#' Generate plots as defined in excel file `projectConfiguration$plotsFile`
#'
#' @param simulatedScenarios A list of simulated scenarios as returned by
#'   `runScenarios()`. Can be `NULL` if no simulated data is required for the
#'   plots.
#' @param observedData A list of `DataSet` objects. Can be `NULL` if no observed
#'   data is required for the plots.
#' @param projectConfiguration Object of class `ProjectConfiguration` that
#'   contains information about the output paths and the excel file where plots
#'   are defined.
#' @param stopIfNotFound If TRUE (default), the function stops if any of the
#'   simulated results or observed data are not found. If FALSE a warning is
#'   printed.
#'
#' @param plotGridNames Names of the plot grid specified in the sheet
#'   `plotGrids` for which the figures will be created. If `NULL` (default), all
#'   plot grids specified in the excel sheet will be created. If a plot grid
#'   with a given name does not exist, an error is thrown.
#'
#' @param dataCombinedList A (named) list of `DataCombined` objects as input to
#'   create plots defined in the `plotGridNames` argument. Missing
#'   `DataCombined` will be created from the Excel file (default behavior).
#'   Defaults to `NULL`, in which case all `DataCombined` are created from
#'   Excel.
#'
#' @returns A list of `ggplot` objects
#'
#' @import tidyr
#'
#' @export
createPlotsFromExcel <- function(
  plotGridNames = NULL,
  simulatedScenarios = NULL,
  observedData = NULL,
  dataCombinedList = NULL,
  projectConfiguration,
  stopIfNotFound = TRUE
) {
  validateIsOfType(observedData, "DataSet", nullAllowed = TRUE)
  validateIsOfType(projectConfiguration, "ProjectConfiguration")
  validateIsString(plotGridNames, nullAllowed = TRUE)
  validateIsOfType(dataCombinedList, "DataCombined", nullAllowed = TRUE)
  if (!typeof(dataCombinedList) %in% c("list", "NULL")) {
    stop(messages$errorDataCombinedListMustBeList(typeof(dataCombinedList)))
  }
  plotConfigurations <- .readPlotConfigurations(
    projectConfiguration = projectConfiguration,
    plotGridNames = plotGridNames
  )
  dfPlotConfigurations <- plotConfigurations$plotConfigurations
  dfPlotGrids <- plotConfigurations$plotGrids

  # Exit early if no plotGrids are defined
  if (is.null(dfPlotGrids)) {
    return(NULL)
  }

  # Get the names of data combined that are required for creation of the plots
  dataCombinedNames <- unique(dfPlotConfigurations$DataCombinedName)
  # Do not create DataCombined that are already passed
  if (!is.null(dataCombinedList)) {
    dataCombinedNames <- setdiff(dataCombinedNames, names(dataCombinedList))
  }
  # Filter and validate only used data combined
  dataCombinedListFromExcel <- createDataCombinedFromExcel(
    projectConfiguration = projectConfiguration,
    dataCombinedNames = dataCombinedNames,
    simulatedScenarios = simulatedScenarios,
    observedData = observedData,
    stopIfNotFound = stopIfNotFound
  )

  # Add entries from to the provided list of DataCombined.
  dataCombinedListFromExcel[names(dataCombinedList)] <- dataCombinedList
  dataCombinedList <- dataCombinedListFromExcel

  # Handles potential issues with ill or undefined Excel columns
  dfPlotConfigurations <- .validatePlotConfigurationFromExcel(
    dfPlotConfigurations,
    names(dataCombinedList)
  )
  plotList <- lapply(
    seq_along(dfPlotConfigurations$plotID),
    function(rowIndex) {
      # Plot Configuration Row as a named list
      # All undefined variables (NA in data.frame row) are set to NULL
      plotConfigurationRow <- sapply(
        dfPlotConfigurations[rowIndex, ],
        function(x){
          if(anyNA(x)){
            return()
            }
          return(x)
        }, 
        simplify = FALSE,
        USE.NAMES = TRUE
      )
        
      .validateLogScaleAxisLimits(
        plotConfigurationRow, 
        plotID = plotConfigurationRow$plotID
      )
      allowedPlotType <- isIncluded(
        dfPlotConfigurations$plotType[rowIndex],
        parentValues = c("individual", "population", "observedVsSimulated", "residualsVsSimulated", "residualsVsTime")
        )
      if(!allowedPlotType){
        stop(messages$missingOrWrongPlotType(dfPlotConfigurations$plotType[rowIndex]), call. = FALSE)
      }
      dataCombined <- dataCombinedList[[plotConfigurationRow$DataCombinedName]]
      switch(
        dfPlotConfigurations$plotType[rowIndex],
        # Individual time profile
        individual = {
          defaultConfiguration <- formals(ospsuite::plotTimeProfile)
          ospsuite::plotTimeProfile(
            plotData = dataCombined,
            xUnit = .fieldFromExcel(
              "xUnit",
              plotConfigurationRow,
              defaultConfiguration
            ),
            yUnit = .fieldFromExcel(
              "yUnit",
              plotConfigurationRow,
              defaultConfiguration
            ),
            xScale = .fieldFromExcel(
              "xAxisScale",
              plotConfigurationRow,
              defaultConfiguration
            ),
            xScaleArgs = .fieldFromExcel(
              "xValuesLimits",
              plotConfigurationRow,
              defaultConfiguration
            ),
            yScale = .fieldFromExcel(
              "yAxisScale",
              plotConfigurationRow,
              defaultConfiguration
            ),
            yScaleArgs = .fieldFromExcel(
              "yValuesLimits",
              plotConfigurationRow,
              defaultConfiguration
            )
          )
        },
        # Population time profile
        population = {
          defaultConfiguration <- formals(ospsuite::plotTimeProfile)
          ospsuite::plotTimeProfile(
            plotData = dataCombined,
            xUnit = .fieldFromExcel(
              "xUnit",
              plotConfigurationRow,
              defaultConfiguration
            ),
            yUnit = .fieldFromExcel(
              "yUnit",
              plotConfigurationRow,
              defaultConfiguration
            ),
            aggregation = .fieldFromExcel(
              "aggregation",
              plotConfigurationRow,
              defaultConfiguration
            ),
            quantiles = .fieldFromExcel(
              "quantiles",
              plotConfigurationRow,
              defaultConfiguration
            ),
            nsd = .fieldFromExcel(
              "nsd",
              plotConfigurationRow,
              defaultConfiguration
            )
          )
        },
        observedVsSimulated = {
          defaultConfiguration <- formals(ospsuite::plotPredictedVsObserved)
          ospsuite::plotPredictedVsObserved(
            plotData = dataCombined,
            yUnit = .fieldFromExcel(
              "yUnit",
              plotConfigurationRow,
              defaultConfiguration
            ),
            xyScale = .fieldFromExcel(
              "xyScale", 
              plotConfigurationRow, 
              defaultConfiguration
              ),
            comparisonLineVector = .fieldFromExcel(
              "foldDistance",
              plotConfigurationRow,
              defaultConfiguration
            ),
            lloqOnBothAxes = .fieldFromExcel(
              "lloqOnBothAxes",
              plotConfigurationRow,
              defaultConfiguration
            )
          )
        },
        residualsVsSimulated = {
          defaultConfiguration <- formals(ospsuite::plotResidualsVsCovariate)
          ospsuite::plotResidualsVsCovariate(
            plotData = dataCombined,
            xUnit = .fieldFromExcel(
              "xUnit",
              plotConfigurationRow,
              defaultConfiguration
            ),
            yUnit = .fieldFromExcel(
              "yUnit",
              plotConfigurationRow,
              defaultConfiguration
            ),
            xAxis = "predicted"
          )
        },
        residualsVsTime = {
          defaultConfiguration <- formals(ospsuite::plotResidualsVsCovariate)
          ospsuite::plotResidualsVsCovariate(
            plotData = dataCombined,
            xUnit = .fieldFromExcel(
              "xUnit",
              plotConfigurationRow,
              defaultConfiguration
            ),
            yUnit = .fieldFromExcel(
              "yUnit",
              plotConfigurationRow,
              defaultConfiguration
            ),
            xAxis = "time"
          )
        }
      ) +
        .excelTheme() +
        ggplot2::labs(
          title = .fieldFromExcel("title", plotConfigurationRow),
          subtitle = .fieldFromExcel("subtitle", plotConfigurationRow)
        )
    }
  )
  # create a list of plots from dataCombinedList and plotConfigurationList
  names(plotList) <- dfPlotConfigurations$plotID

  # create plotGridConfiguration objects and add plots from plotList
  defaultPlotGridConfig <- createEsqlabsPlotGridConfiguration()
  plotGrids <- apply(dfPlotGrids, 1, \(row) {
    plotGridConfiguration <- .createConfigurationFromRow(
      defaultConfiguration = defaultPlotGridConfig,
      row[!(names(row) %in% c("name", "plotIDs", "title"))]
    )

    # Ignore if title is not defined or no 'title' column is present
    if (!is.na(row$title) && !is.null(row$title)) {
      plotGridConfiguration$title <- row$title
    }

    plotsToAdd <- plotList[intersect(
      unlist(row$plotIDs),
      dfPlotConfigurations$plotID
    )]
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
    if (
      length(
        invalidPlotIDs <- setdiff(
          unlist(row$plotIDs),
          dfPlotConfigurations$plotID
        )
      ) !=
        0
    ) {
      warning(messages$warningInvalidPlotID(invalidPlotIDs, row$title))
    }
    plotGrid(plotGridConfiguration)
  })
  names(plotGrids) <- dfPlotGrids$name

  return(plotGrids)
}

#' Parse and validate comma-separated Excel field
#'
#' Parses comma-separated values from Excel and validates using ospsuite.utils.
#' Provides Excel-specific error context (plotID, field name) for common issues.
#'
#' @param value Raw value from Excel cell
#' @param fieldName Name of the field for error messages
#' @param plotID Optional plot ID for error context
#' @param expectedLength Expected number of values (NULL for any length)
#' @param expectedType Expected type ("numeric" or "character")
#' @returns Parsed and validated vector
#' @keywords internal
.parseExcelMultiValueField <- function(
  value,
  fieldName,
  plotID = NULL,
  expectedLength = NULL,
  expectedType = "numeric"
) {
  originalValue <- value

  # Parse using scan (existing method)
  parsed <- unlist(trimws(scan(
    text = as.character(value),
    what = "character",
    sep = ",",
    quiet = TRUE
  )))

  # Detect common error: space-separated instead of comma-separated
  if (!is.null(expectedLength) && length(parsed) != expectedLength) {
    # Check if might be space-separated
    spaceSplit <- unlist(strsplit(trimws(as.character(originalValue)), "\\s+"))
    if (length(spaceSplit) == expectedLength) {
      # Check if all parts look numeric (for numeric fields)
      if (expectedType == "numeric") {
        numericTest <- suppressWarnings(as.numeric(spaceSplit))
        if (!any(is.na(numericTest))) {
          # User likely used spaces instead of commas
          stop(
            messages$excelFieldFormatError(
              fieldName,
              originalValue,
              plotID,
              "comma-separated"
            ),
            call. = FALSE
          )
        }
      }
    }
  }

  # Validate length using ospsuite.utils
  if (!is.null(expectedLength)) {
    tryCatch(
      ospsuite.utils::validateIsOfLength(parsed, expectedLength),
      error = function(e) {
        stop(
          messages$excelFieldLengthError(
            fieldName,
            originalValue,
            plotID,
            expectedLength,
            length(parsed)
          ),
          call. = FALSE
        )
      }
    )
  }

  # Validate type and convert if needed
  if (expectedType == "numeric") {
    numericParsed <- suppressWarnings(as.numeric(parsed))
    tryCatch(
      ospsuite.utils::validateIsNumeric(numericParsed),
      error = function(e) {
        stop(
          messages$excelFieldTypeError(
            fieldName,
            originalValue,
            plotID,
            "numeric"
          ),
          call. = FALSE
        )
      }
    )
    return(numericParsed)
  }

  return(parsed)
}

#' Validate that log scale axes do not have limits containing zero
#'
#' @param plotConfiguration A named list from plot configuration excel row
#' whose undefined values are set to `NULL`
#' @param plotID Optional plot ID for the warning message
#'
#' @keywords internal
#' @noRd
.validateLogScaleAxisLimits <- function(plotConfiguration, plotID = NULL) {
  axisChecks <- list(
    list(
      scale = "xAxisScale",
      limits = c("xAxisLimits", "xValuesLimits"),
      axis = "x"
    ),
    list(
      scale = "yAxisScale",
      limits = c("yAxisLimits", "yValuesLimits"),
      axis = "y"
    )
  )
  for (check in axisChecks) {
    scaleValue <- plotConfiguration[[check$scale]]
    for (limitsField in check$limits) {
      limitsValue <- plotConfiguration[[limitsField]]
      if (is.null(limitsValue)) {
        next
      }
      limitsValue <- .parseExcelMultiValueField(
        limitsValue,
        plotID = plotID,
        fieldName = limitsField,
        expectedLength = 2
      )
      if (is.null(scaleValue)) {
        next
      }
      if (all(scaleValue == ospsuite.plots::AxisScales$log, 0 %in% limitsValue)) {
        warning(messages$warningLogScaleWithZeroLimit(
          plotID = plotID,
          axisLimitsField = limitsField,
          axis = check$axis
        ))
      }
    }
  }
  return(invisible())
}

#' Create a plotConfiguration object from a row of sheet
#' 'plotConfiguration'
#'
#' @param defaultConfiguration default plotConfiguration
#' @param ... row with configuration properties
#' @returns A customized plotConfiguration object
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
      # Special treatment for axis limits - parse and validate early with clear errors
      if (
        colName %in%
          c(
            "xAxisLimits",
            "yAxisLimits",
            "xValuesLimits",
            "yValuesLimits"
          )
      ) {
        # Use wrapper function with ospsuite.utils validation
        value <- .parseExcelMultiValueField(
          value = value,
          fieldName = colName,
          plotID = if ("plotID" %in% names(columns)) {
            columns[["plotID"]]
          } else {
            NULL
          },
          expectedLength = 2,
          expectedType = "numeric"
        )
        # Set directly (already validated and converted)
        newConfiguration[[colName]] <- value
      } else {
        # For other fields, use existing logic
        # For fields that require multiple values, values are separated by a ','.
        # Alternatively, the values can be enclosed in "" in case the title should contain a ','.
        # Split the input string by ',' but do not split within ""
        value <- unlist(trimws(scan(
          text = as.character(value),
          what = "character",
          sep = ",",
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

        # Caste the value and set it
        newConfiguration[[colName]] <- methods::as(
          object = value,
          Class = expectedType
        )
      }
    }
  })

  return(newConfiguration)
}

#' Validate and process the 'plotConfiguration' sheet
#'
#' @param dfPlotConfigurations Data frame created by reading the '
#'   plotConfiguration' sheet
#' @param dataCombinedNames Names of the 'DataCombined' that are referenced in
#'   the plot configurations
#'
#' @returns Processed `dfPlotConfigurations`
#' @keywords internal
.validatePlotConfigurationFromExcel <- function(
  dfPlotConfigurations,
  dataCombinedNames
) {
  # mandatory column DataCombinedName is empty - throw error
  missingLabel <- sum(is.na(dfPlotConfigurations$DataCombinedName))
  if (missingLabel > 0) {
    stop(messages$missingDataCombinedName())
  }

  # plotIDs must be unique
  duplicated_plotIDs <- dfPlotConfigurations$plotID[duplicated(
    dfPlotConfigurations$plotID
  )]
  if (length(duplicated_plotIDs) > 0) {
    stop(messages$PlotIDsMustBeUnique(duplicated_plotIDs))
  }

  # mandatory column plotType is empty - throw error
  missingLabel <- sum(is.na(dfPlotConfigurations$plotType))
  if (missingLabel > 0) {
    stop(messages$missingPlotType())
  }

  # DataCombined that are not defined in the DataCombined sheet. Stop if any.
  missingDataCombined <- setdiff(
    setdiff(dfPlotConfigurations$DataCombinedName, dataCombinedNames),
    NA
  )
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
#' @returns Processed `dfPlotGrids`
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
  # Split the input string by ',' but do not split within "" Have to do it one
  # row at a time, otherwise it returns one separate list entry for each plot it
  # (and not lists of plot ids)
  dfPlotGrids$plotIDs <- lapply(dfPlotGrids$plotIDs, \(plotId) {
    unlist(trimws(scan(
      text = as.character(plotId),
      what = "character",
      sep = ",",
      quiet = TRUE
    )))
  })

  # plotIDs that are not defined in the plotConfiguration sheet. Stop if any.
  missingPlots <- setdiff(
    setdiff(unique(unlist(dfPlotGrids$plotIDs)), plotIDs),
    NA
  )
  if (length(missingPlots) != 0) {
    stop(messages$errorInvalidPlotID(missingPlots))
  }

  return(dfPlotGrids)
}

#' Update Plot Configuration with Overrides
#'
#' Updates a plot configuration object `plotConfiguration` with explicitly
#' defined overrides from `plotOverrideConfig` list. It retains any custom
#' settings in `plotConfiguration` that deviate from the defaults
#'
#' @param plotConfiguration A plot configuration object.
#' @param plotOverrideConfig A list with new configuration settings to apply.
#'
#' @keywords internal
#' @noRd
.updatePlotConfiguration <- function(plotConfiguration, plotOverrideConfig) {
  defaultValues <- createEsqlabsPlotConfiguration()

  for (name in names(plotOverrideConfig)) {
    if (!name %in% names(plotConfiguration)) {
      warning(messages$UnknownPlotConfiguration(name))
      next
    }

    if (is.null(defaultValues[[name]]) && is.null(plotConfiguration[[name]])) {
      plotConfiguration[[name]] <- plotOverrideConfig[[name]]
    } else if (
      !is.null(defaultValues[[name]]) && !is.null(plotConfiguration[[name]])
    ) {
      if (all(plotConfiguration[[name]] == defaultValues[[name]])) {
        plotConfiguration[[name]] <- plotOverrideConfig[[name]]
      }
    }
  }

  return(plotConfiguration)
}

#' Apply Specific Configuration Overrides to Default Plot Configuration
#'
#' This function applies specific configuration overrides to the default plot
#' configuration. It first applies any additional parameters provided via `...`,
#' then updates the default configuration with overrides from the
#' `plotOverrideConfig` list, but only if the corresponding values have not
#' already been set by the additional parameters. Finally, it validates the
#' final configuration to ensure all options are valid.
#'
#' @param defaultPlotConfiguration An object of class `DefaultPlotConfiguration`
#'   or a list of such objects.
#' @param plotOverrideConfig A list with new configuration settings to apply.
#' @param ... Additional parameters to override specific configuration settings
#'   dynamically.
#'
#' @keywords internal
#' @noRd
.applyPlotConfiguration <- function(
  defaultPlotConfiguration = NULL,
  plotOverrideConfig = NULL,
  ...
) {
  # validate input defaultPlotConfiguration
  if (is.null(defaultPlotConfiguration)) {
    defaultPlotConfiguration <- createEsqlabsPlotConfiguration()
  } else {
    validateIsOfType(defaultPlotConfiguration, "DefaultPlotConfiguration")
  }

  # Clone the `DefaultPlotConfiguration` object
  # If a list of configurations is passed, clone only the first configuration
  # in the list. List processing not supported yet.
  if (inherits(defaultPlotConfiguration, "list")) {
    customPlotConfiguration <- defaultPlotConfiguration[[1]]$clone()
  } else {
    customPlotConfiguration <- defaultPlotConfiguration$clone()
  }

  # Capture additional parameters passed through ... and override
  additionalParams <- list(...)
  for (param in names(additionalParams)) {
    if (!is.null(additionalParams[[param]])) {
      customPlotConfiguration[[param]] <- additionalParams[[param]]
    }
  }

  # override only default configuration values with settings in plotOverrideConfig
  customPlotConfiguration <- .updatePlotConfiguration(
    customPlotConfiguration,
    plotOverrideConfig
  )

  # convert to list and validate final plot configuration
  plotConfigurationList <- purrr::map(
    purrr::set_names(names(customPlotConfiguration)),
    ~ customPlotConfiguration[[.]]
  )
  optionNames <- unique(c(names(plotOverrideConfig), names(additionalParams)))
  ospsuite.utils::validateIsOption(
    plotConfigurationList,
    .getPlotConfigurationOptions(optionNames)
  )

  return(customPlotConfiguration)
}


#' Calculate axis limits
#'
#' This function calculates axis limits based on minimum and maximum values.
#'
#' @param x Numeric vector for which limits are calculated.
#'
#' @keywords internal
#' @noRd
.calculateLimits <- function(x, scaling = NULL) {
  if (!is.null(scaling) && scaling == "log") {
    limits <- c(
      min(x[x > 0], na.rm = TRUE) * 0.9,
      max(x[x > 0], na.rm = TRUE) * 1.1
    )
  } else {
    limits <- c(
      (if (min(x, na.rm = TRUE) <= 0) 1.01 else 0.99) * min(x, na.rm = TRUE),
      (if (max(x, na.rm = TRUE) > 0) 1.01 else 0.99) * max(x, na.rm = TRUE)
    )
  }

  return(limits)
}

#' Get valid plot configuration options
#'
#' Generates a list of valid configuration options for plotting. Each
#' configuration option specifies constraints, including data type, allowable
#' values, and value ranges, formatted to facilitate validation with
#' `ospsuite::validateIsOption` function.
#'
#' @returns A list of lists, each containing type specifications and constraints
#'   for a plot configuration parameter.
#' @keywords internal
#' @noRd
.getPlotConfigurationOptions <- function(names) {
  plotConfigurationOptions <- list(
    legendPosition = list(
      type = "character",
      allowedValues = c("left", "right", "bottom", "top", "none")
    ),
    legendTitle = list(
      type = "character",
      nullAllowed = TRUE
    ),
    linesAlpha = list(
      type = "numeric",
      valueRange = c(0, 1)
    ),
    linesSize = list(
      type = "numeric",
      valueRange = c(0.1, 10)
    ),
    parameterFactor = list(
      type = "numeric",
      valueRange = c(1e-16, 1e16)
    ),
    pointsShape = list(
      type = "integer",
      valueRange = c(0L, 25L)
    ),
    pointsSize = list(
      type = "numeric",
      valueRange = c(0.1, 10)
    ),
    subtitle = list(
      type = "character",
      nullAllowed = TRUE
    ),
    title = list(
      type = "character",
      nullAllowed = TRUE
    ),
    titleSize = list(
      type = "numeric"
    ),
    xAxisScale = list(
      type = "character",
      allowedValues = c("log", "lin")
    ),
    xLabel = list(
      type = "character",
      nullAllowed = TRUE
    ),
    yAxisFacetScales = list(
      type = "character",
      allowedValues = c("fixed", "free")
    ),
    yAxisScale = list(
      type = "character",
      allowedValues = c("log", "lin")
    ),
    yAxisTicks = list(
      type = "integer",
      valueRange = c(1L, 20L)
    ),
    xAxisType = list(
      type = "character",
      allowedValues = c("percent", "absolute")
    ),
    yAxisType = list(
      type = "character",
      allowedValues = c("percent", "absolute")
    ),
    yLabel = list(
      type = "character",
      nullAllowed = TRUE
    )
  )

  return(plotConfigurationOptions[names])
}

#' Read and validate plot configurations from the excel file
#'
#' @param projectConfiguration Object of class `ProjectConfiguration`
#' @param plotGridNames Names of the plot grid specified in the sheet
#'   `plotGrids`
#'
#' @returns A named list with configurations 'plotGrids' and 'dfPlotConfigurations'
#' @noRd
.readPlotConfigurations <- function(projectConfiguration, plotGridNames) {
  # read sheet "plotGrids" with info for plotGridConfigurations
  dfPlotGrids <- readExcel(projectConfiguration$plotsFile, sheet = "plotGrids")

  # Filter for only specified plot grids
  if (!is.null(plotGridNames)) {
    # Throw an error if a plot grid name that is passed is not defined in the excel file
    missingPlotGrids <- setdiff(plotGridNames, unique(dfPlotGrids$name))
    if (length(missingPlotGrids) != 0) {
      stop(messages$invalidPlotGridNames(missingPlotGrids))
    }

    dfPlotGrids <- dplyr::filter(dfPlotGrids, name %in% plotGridNames)
  }

  # Exit early if no PlotGrid is defined
  if (dim(dfPlotGrids)[[1]] == 0) {
    return()
  }

  # read sheet "plotConfiguration"
  dfPlotConfigurations <- readExcel(
    projectConfiguration$plotsFile,
    sheet = "plotConfiguration"
  )

  # Filter and validate plotGrids
  dfPlotGrids <- dplyr::filter(
    dfPlotGrids,
    !dplyr::if_all(dplyr::everything(), is.na)
  )
  dfPlotGrids <- .validatePlotGridsFromExcel(
    dfPlotGrids,
    unique(dfPlotConfigurations$plotID)
  )

  # Filter and validate only used plot configurations
  dfPlotConfigurations <- dplyr::filter(
    dfPlotConfigurations,
    plotID %in% unlist(unique(dfPlotGrids$plotIDs))
  )

  return(list(
    plotGrids = dfPlotGrids,
    plotConfigurations = dfPlotConfigurations
  ))
}


#' ggplot2 theme layer default values to add on each plot defined in Excel
#' 
#' @keywords internal
.excelTheme <- function() {
  ggplot2::theme(
    # element_textbox_simple wraps and break line if title is too long
    plot.title = ggtext::element_textbox_simple(size = 10, hjust = 0),
    axis.title.x = ggplot2::element_text(
      size = 9,
      margin = ggplot2::margin(t = 10)
    ),
    axis.title.y = ggplot2::element_text(
      size = 9,
      margin = ggplot2::margin(r = 10)
    ),
    # tick labels cannot use markdown because they need to be vectorised
    axis.text.x = ggplot2::element_text(size = 8),
    axis.text.y = ggplot2::element_text(size = 8, angle = 0),
    # Legend
    legend.key.size = ggplot2::unit(6, "pt"),
    legend.position = "top",
    legend.justification = "left",
    # ESQLabs color palette used as default when adding scale_color_discrete()
    palette.colour.discrete = esqlabsR::esqlabsColors
  )
}

#' Map field name from PlotConfiguration table with `{ospsuite.plots}` function
#' If the field is undefined (either empty cell or undefined column), 
#' `plotConfigurationRow[[fieldName]]` will be `NULL` and 
#' default `{esqlabsR}` or `{ospsuite.plots}` values will be used
#'
#' @param fieldName Name of field to map in Excel table
#' @param plotConfigurationRow 
#' Named list of values defined in a row of an Excel PlotConfiguration table
#' @param defaultConfiguration 
#' Named list of default `{esqlabsR}` or `{ospsuite}` values for the plot configuration
#' @importFrom stats na.exclude
#' @keywords internal
.fieldFromExcel <- function(
  fieldName,
  plotConfigurationRow,
  defaultConfiguration = NULL
) {
  if (fieldName %in% "foldDistance") {
    if (is.null(plotConfigurationRow[["foldDistance"]])) {
      return(defaultConfiguration[["comparisonLineVector"]])
    }
    foldDistance <- .parseExcelMultiValueField(
      plotConfigurationRow[["foldDistance"]]
      )
    return(ospsuite.plots::getFoldDistanceList(folds = foldDistance))
  }
  if (fieldName %in% c("xAxisScale", "yAxisScale")) {
    # Use linear as default value for an axis scale
    if (is.null(plotConfigurationRow[[fieldName]])) {
      return(ospsuite.plots::AxisScales$linear)
    }
    return(plotConfigurationRow[[fieldName]])
  }
  # xyScale field only used by obs vs simulated plots
  # Default value for xyScale is log creating log-log obs vs simulated
  if (fieldName %in% "xyScale") {
    xyScale <- c(
      plotConfigurationRow[["xAxisScale"]], 
      plotConfigurationRow[["yAxisScale"]]
      ) |>
      unique()
    # Default value for xyScale is log creating log-log obs vs simulated
    if(isEmpty(xyScale)){
      return(defaultConfiguration[[fieldName]])
    }
    # Note that at this stage .validateLogScaleAxisLimits was already assessed
    if (length(xyScale) > 1){
      stop(
        messages$conflictingAxesScales(plotConfigurationRow$plotID),
        call. = FALSE
      )
    }
    return(xyScale)
  }
  # Issue #991: default value should be TRUE
  if (fieldName %in% "lloqOnBothAxes"){
    if(is.null(plotConfigurationRow[["lloqOnBothAxes"]])){
      return(TRUE)
    }
    return(as.logical(plotConfigurationRow[["lloqOnBothAxes"]]))
  }
  # Default value except by ospsuite.plots for axis limits values is empty list
  if (fieldName %in% c("xValuesLimits", "yValuesLimits")) {
    if (is.null(plotConfigurationRow[[fieldName]])) {
      return(list())
    }
    valuesLimits <- .parseExcelMultiValueField(
      plotConfigurationRow[[fieldName]]
      )
    return(list(limits = valuesLimits))
  }
  # Any other undefined field will use the default value 
  # matching its name within ospsuite.plots function arguments
  if (isEmpty(plotConfigurationRow[[fieldName]])) {
    if (is.language(defaultConfiguration[[fieldName]])) {
      return(eval(defaultConfiguration[[fieldName]]))
    }
    return(defaultConfiguration[[fieldName]])
  }
  return(plotConfigurationRow[[fieldName]])
}

#' Return list of properties from a plot type
#'
#' @param plotType One of `"timeProfiles"`, `"spiderPlot"`, `"tornadoPlot"`
#' @returns A list
#' @keywords internal
.plotConfigurationFromType <- function(plotType = NULL) {
  allowedPlotType <- isIncluded(plotType, c("timeProfiles", "spiderPlot", "tornadoPlot"))
  if (!allowedPlotType) {
    warning(messages$wrongPlotTypeInPlotConfiguration(plotType), call. = FALSE)
    return(NULL)
  }
  if (plotType %in% "timeProfiles") {
    timeProfilesConfiguration <- list(
      legendPosition = "bottom",
      legendJustification = "center",
      legendTitle = "Parameter factor",
      linesAlpha = 0.7,
      linesSize = 1.4,
      # linesColor = colorspace::diverging_hcl(2, palette = "Berlin"),
      pointsShape = ospsuite.plots::ospShapeNames,
      title = NULL,
      titleSize = 14,
      xAxisScale = ospsuite.plots::AxisScales$linear,
      xLabel = NULL,
      yAxisScale = ospsuite.plots::AxisScales$log,
      yLabel = NULL
    )
    return(timeProfilesConfiguration)
  }
  if (plotType %in% "spiderPlot") {
    spiderPlotConfiguration <- list(
      legendPosition = "bottom",
      legendJustification = "center",
      legendTitle = "Parameter",
      linesSize = 1.4,
      linesAlpha = 0.75,
      linesColor = esqlabsEnv$colorPalette,
      pointsShape = "circle",
      pointsSize = 2,
      title = NULL,
      titleSize = 14,
      xAxisScale = ospsuite.plots::AxisScales$log,
      xLabel = NULL,
      yAxisScale = ospsuite.plots::AxisScales$linear,
      yAxisTicks = 10L,
      yLabel = NULL
    )
    return(spiderPlotConfiguration)
  }
  tornadoPlotConfiguration <- list(
      legendPosition = "right",
      legendTitle = "Parameter Factor",
      subtitle = NULL,
      title = NULL,
      titleSize = 14,
      linesColor = esqlabsEnv$colorPalette,
      linesAlpha = 0.75,
      xLabel = "Change in PK parameter [% relative to baseline]",
      yLabel = "Parameter"
    )
  return(tornadoPlotConfiguration)
}
