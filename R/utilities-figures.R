# Section validation adapter ----
#
# Registered in `.validationAdapters` (R/validation.R) and called by
# `.runProjectValidation()`. The plots adapter covers all three
# sub-sections (`dataCombined`, `plotConfiguration`, `plotGrids`)
# since they are tightly coupled by inner cross-refs and the
# data-class shape on `project$plots` is a single named list.

#' @keywords internal
#' @noRd
.plotsValidatorAdapter <- function(project) {
  .validatePlots(project$plots)
}

#' Validate the `plots` section of a Project
#'
#' Covers `dataCombined`, `plotConfiguration`, and `plotGrids`:
#'   * dataCombined entries must declare a non-empty `scenario` on
#'     each simulated row.
#'   * plotConfiguration must declare `plotID`, `DataCombinedName`, and
#'     `plotType` columns; `plotID` must be unique; `DataCombinedName`
#'     must reference a known dataCombined entry.
#'   * plotGrids entries reference `plotConfiguration$plotID` via a
#'     comma-separated `plotIDs` string; unknown ids are surfaced as a
#'     warning (not a critical error) since unknown grid ids fail
#'     softly during plot creation.
#'
#' Cross-section references that escape this section (dataCombined ->
#' scenarios) are validated in `.validateCrossReferences()`.
#'
#' @param plots Named list from `project$plots` (`NULL` when the JSON
#'   omits the `plots` section).
#' @return validationResult.
#' @keywords internal
#' @noRd
.validatePlots <- function(plots) {
  result <- validationResult$new()

  if (is.null(plots)) {
    result$add_warning("Data", "No plots defined")
    return(result)
  }

  dataCombined <- plots$dataCombined
  plotConfig <- plots$plotConfiguration

  if (is.null(dataCombined) || length(dataCombined) == 0) {
    result$add_warning("Data", "dataCombined is empty")
  } else {
    for (dcName in names(dataCombined)) {
      dc <- dataCombined[[dcName]]
      for (entry in dc$simulated %||% list()) {
        if (is.null(entry$scenario) || identical(entry$scenario, "")) {
          result$add_critical_error(
            "Missing Fields",
            paste0(
              "Simulated entry in dataCombined '",
              dcName,
              "' is missing 'scenario'"
            )
          )
        }
      }
    }
  }

  if (is.null(plotConfig) || nrow(plotConfig) == 0) {
    result$add_warning("Data", "plotConfiguration is empty")
  } else {
    for (col in c("plotID", "DataCombinedName", "plotType")) {
      if (!col %in% names(plotConfig)) {
        result$add_critical_error(
          "Missing Fields",
          paste0("plotConfiguration is missing required column '", col, "'")
        )
      }
    }

    if ("plotID" %in% names(plotConfig)) {
      result <- .check_no_duplicates(
        plotConfig$plotID[!is.na(plotConfig$plotID)],
        "plotID",
        result
      )
    }

    if ("DataCombinedName" %in% names(plotConfig)) {
      invalidDataCombinedRefs <- setdiff(
        plotConfig$DataCombinedName[!is.na(plotConfig$DataCombinedName)],
        names(dataCombined %||% list())
      )
      if (length(invalidDataCombinedRefs) > 0) {
        result$add_critical_error(
          "Invalid Reference",
          paste0(
            "plotConfiguration references unknown DataCombinedName: ",
            paste(invalidDataCombinedRefs, collapse = ", ")
          )
        )
      }
    }
  }

  plotGrids <- plots$plotGrids
  if (
    !is.null(plotGrids) &&
      nrow(plotGrids) > 0 &&
      !is.null(plotConfig) &&
      nrow(plotConfig) > 0
  ) {
    if ("plotIDs" %in% names(plotGrids) && "plotID" %in% names(plotConfig)) {
      allGridIds <- unlist(lapply(
        plotGrids$plotIDs[!is.na(plotGrids$plotIDs)],
        function(x) trimws(strsplit(x, ",")[[1]])
      ))
      invalidGridRefs <- setdiff(allGridIds, plotConfig$plotID)
      if (length(invalidGridRefs) > 0) {
        result$add_warning(
          "Invalid Reference",
          paste0(
            "plotGrids references unknown plotIDs: ",
            paste(invalidGridRefs, collapse = ", ")
          )
        )
      }
    }
  }

  result
}

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

#' @title Create an instance of `DefaultPlotConfiguration` R6 class
#' @rdname createEsqlabsPlotConfiguration
#'
#' @description
#'
#' An instance of `DefaultPlotConfiguration` R6 class from `{tlf}` package is
#' needed for creating visualizations with the `{ospsuite}` package.
#'
#' The default attributes of the class are chosen to reflect the corporate
#' standards adopted by esqLABS GmbH.
#'
#' @returns An instance of `DefaultPlotConfiguration` R6 class.
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
  plotGridConfiguration <- tlf::PlotGridConfiguration$new()

  plotGridConfiguration$tagLevels <- "a"
  plotGridConfiguration$tagSize <- 11
  plotGridConfiguration$titleSize <- 12

  plotGridConfiguration$titleHorizontalJustification <- 0.5

  return(plotGridConfiguration)
}

#' @param outputFolder Path to the folder where the results will be stored.
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
#' @returns An instance of `ExportConfiguration` R6 class.
#'
#' @examples
#' myProject <- Project$new()
#' createEsqlabsExportConfiguration(myProject$outputFolder)
#'
#' @family create-plotting-configurations
#'
#' @export
createEsqlabsExportConfiguration <- function(outputFolder) {
  # nolint: object_length_linter.
  # Specifying the namespace because we want to use the ExportConfiguration
  # from esqlabsR and not from TLF
  exportConfiguration <- esqlabsR::ExportConfiguration$new()

  exportConfiguration$path <- outputFolder
  exportConfiguration$dpi <- 300
  # NULL is not supported by ExportConfiguration, so we should assign here
  # something useful. NULL in the ProjectConfiguration currently means "do not
  # export".
  exportConfiguration$format <- "png"
  exportConfiguration$width <- 18
  exportConfiguration$heightPerRow <- 12
  exportConfiguration$units <- "cm"
  return(exportConfiguration)
}

#' Generate plots from a Project
#'
#' @description
#' Reads `project$plots$plotConfiguration` and `project$plots$plotGrids`
#' (both data.frames) to build the requested plot grids. DataCombined
#' objects are resolved via [createDataCombined()] internally unless
#' supplied via `dataCombinedList`.
#'
#' @param project A `Project` (see [loadProject()]).
#' @param plotGridNames Names of plot grids to build. If `NULL` (default),
#'   all grids declared in `project$plots$plotGrids` are built.
#' @param simulatedScenarios Named list of simulated scenarios from
#'   [runScenarios()].
#' @param dataCombinedList Optional pre-built named list of `DataCombined`
#'   objects. If `NULL`, the function builds them via [createDataCombined()].
#' @param stopIfNotFound If `TRUE`, errors when a referenced DataCombined or
#'   simulated/observed entry cannot be resolved.
#' @param validate Logical. If `TRUE` (default), runs the relevant
#'   section validators via [validateProject()] before building the
#'   plots and aborts with a formatted summary on critical errors. Set
#'   to `FALSE` to skip the pre-flight check (e.g. when the caller has
#'   already validated the project).
#'
#' @returns A named list of plot-grid objects (one per `plotGridName`), or
#'   an empty list when the project has no plots section.
#'
#' @import tidyr
#'
#' @export
createPlots <- function(
  project,
  plotGridNames = NULL,
  simulatedScenarios = NULL,
  dataCombinedList = NULL,
  stopIfNotFound = TRUE,
  validate = TRUE
) {
  ospsuite.utils::validateIsOfType(project, "Project")
  if (isTRUE(validate)) {
    .ensureValid(
      project,
      sections = c("plots", "scenarios", "observedData", "crossReferences"),
      opName = "createPlots"
    )
  }
  if (is.null(project$plots)) {
    return(list())
  }
  cfgDf <- project$plots$plotConfiguration %||% data.frame()
  gridDf <- project$plots$plotGrids %||% data.frame()
  if (nrow(gridDf) == 0) return(list())
  if (is.null(plotGridNames)) {
    plotGridNames <- gridDf$name
  }
  # Filter to only requested grids and any plot configs they reference.
  gridDf <- gridDf[gridDf$name %in% plotGridNames, , drop = FALSE]
  if (nrow(gridDf) == 0) return(list())

  # Build DataCombined for the configs referenced by the requested grids.
  if (is.null(dataCombinedList)) {
    dataCombinedList <- createDataCombined(
      project,
      plotGridNames = plotGridNames,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = stopIfNotFound
    )
  }

  .createPlotGridsFromDataFrames(
    dfPlotConfigurations = cfgDf,
    dfPlotGrids = gridDf,
    dataCombinedList = dataCombinedList
  )
}

#' Generate plots from Excel (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")` Use [createPlots()] with a
#' [Project][loadProject()].
#'
#' @param plotGridNames Names of plot grids to build. `NULL` builds all.
#' @param simulatedScenarios Named list of simulated scenarios.
#' @param observedData Named list of observed `DataSet` objects.
#' @param dataCombinedList Optional pre-built `DataCombined` objects.
#' @param projectConfiguration A `ProjectConfiguration` pointing at a
#'   `Plots.xlsx`.
#' @param outputFolder Optional override for the export-configuration's
#'   output folder.
#' @param stopIfNotFound If `TRUE`, errors on unresolved references.
#'
#' @returns Named list of plot-grid objects.
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
  outputFolder = NULL,
  stopIfNotFound = TRUE
) {
  lifecycle::deprecate_soft(
    when = "5.7.0",
    what = "createPlotsFromExcel()",
    with = "createPlots(project)",
    details = "Migrate the Plots.xlsx workflow to a JSON Project."
  )
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
  dfExportConfigurations <- plotConfigurations$exportConfigurations

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

  plotGrids <- .createPlotGridsFromDataFrames(
    dfPlotConfigurations = dfPlotConfigurations,
    dfPlotGrids = dfPlotGrids,
    dataCombinedList = dataCombinedList
  )

  ## Remove rows that are entirely empty
  dfExportConfigurations <- dplyr::filter(
    dfExportConfigurations,
    !dplyr::if_all(dplyr::everything(), is.na)
  )
  dfExportConfigurations <- .validateExportConfigurationsFromExcel(
    dfExportConfigurations,
    plotGrids
  )
  if (nrow(dfExportConfigurations) > 0) {
    # create a list of ExportConfiguration objects from dfExportConfigurations
    outputFolder <- outputFolder %||%
      file.path(
        projectConfiguration$outputFolder,
        "Figures",
        format(Sys.time(), "%F %H-%M")
      )

    defaultExportConfiguration <- createEsqlabsExportConfiguration(outputFolder)
    exportConfigurations <- apply(dfExportConfigurations, 1, \(row) {
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
      exportConfigurations[[
        i
      ]]$savePlot(plotGrids[[dfExportConfigurations$plotGridName[i]]])
    })
  }

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
#' @param plotConfiguration A plot configuration object
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
    if (!is.null(scaleValue) && scaleValue == "log") {
      for (limitsField in check$limits) {
        limitsValue <- plotConfiguration[[limitsField]]
        if (!is.null(limitsValue) && 0 %in% limitsValue) {
          warning(messages$warningLogScaleWithZeroLimit(
            plotID = plotID,
            axisLimitsField = limitsField,
            axis = check$axis
          ))
        }
      }
    }
  }
}

#' Create a plotConfiguration or exportConfiguration objects from a row of sheet
#' 'plotConfiguration' or 'exportConfiguration'
#'
#' @param defaultConfiguration default plotConfiguration or exportConfiguration
#' @param ... row with configuration properties
#' @returns A customized plot- or exportConfiguration object
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
  # (and not lists of plot ids). Skipped when plotIDs is already a list-column
  # (e.g. when this validator runs a second time inside the shared helper).
  if (!is.list(dfPlotGrids$plotIDs)) {
    dfPlotGrids$plotIDs <- lapply(dfPlotGrids$plotIDs, \(plotId) {
      unlist(trimws(scan(
        text = as.character(plotId),
        what = "character",
        sep = ",",
        quiet = TRUE
      )))
    })
  }

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

#' Validate and process the 'exportConfiguration' sheet
#'
#' @param dfExportConfigurations Data frame created by reading the
#'   'exportConfiguration' sheet
#' @param plotGrids List of multipanel plots created previously
#'
#' @returns Processed `dfExportConfigurations`
#' @keywords internal
.validateExportConfigurationsFromExcel <- function(
  dfExportConfigurations,
  plotGrids
) {
  # mandatory column outputName is empty - throw warning, remove rows
  missingName <- sum(is.na(dfExportConfigurations$name))
  if (missingName > 0) {
    dfExportConfigurations <- dfExportConfigurations[
      !is.na(dfExportConfigurations$name),
    ]
    warning(messages$missingOutputFileName())
  }

  plotGrids <- purrr::compact(plotGrids)
  missingPlotGrids <- setdiff(
    dfExportConfigurations$plotGridName,
    names(plotGrids)
  )
  if (length(missingPlotGrids) != 0) {
    dfExportConfigurations <- dfExportConfigurations[
      !(dfExportConfigurations$plotGridName %in% missingPlotGrids),
    ]
    warning(messages$missingPlotGrids(missingPlotGrids))
  }

  return(dfExportConfigurations)
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
#' @returns A named list with configurations 'plotGrids',
#'   'dfPlotConfigurations', and 'exportConfigurations'
#' @noRd
.readPlotConfigurations <- function(projectConfiguration, plotGridNames) {
  # read sheet "plotGrids" with info for plotGridConfigurations
  dfPlotGrids <- readExcel(projectConfiguration$plotsFile, sheet = "plotGrids")

  # read sheet "exportConfiguration"
  dfExportConfigurations <- readExcel(
    projectConfiguration$plotsFile,
    sheet = "exportConfiguration"
  ) |>
    dplyr::rename(name = outputName)

  # Filter for only specified plot grids
  if (!is.null(plotGridNames)) {
    # Throw an error if a plot grid name that is passed is not defined in the excel file
    missingPlotGrids <- setdiff(plotGridNames, unique(dfPlotGrids$name))
    if (length(missingPlotGrids) != 0) {
      stop(messages$invalidPlotGridNames(missingPlotGrids))
    }

    dfPlotGrids <- dplyr::filter(dfPlotGrids, name %in% plotGridNames)
    # Filter export configurations for specified plot grids only
    dfExportConfigurations <- dplyr::filter(
      dfExportConfigurations,
      plotGridName %in% plotGridNames
    )
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
    exportConfigurations = dfExportConfigurations,
    plotConfigurations = dfPlotConfigurations
  ))
}

# Build named list of plot-grid objects from data.frame configurations.
# Shared by createPlots(project, ...) and createPlotsFromExcel().
#
# - dfPlotConfigurations: data.frame with one row per plot, columns
#   include plotID, DataCombinedName, plotType, title, subtitle, plus
#   axis/styling fields. Rows whose DataCombinedName is not present in
#   `dataCombinedList` are pruned by `.validatePlotConfigurationFromExcel()`.
# - dfPlotGrids: data.frame with one row per grid, columns include name,
#   plotIDs, title.
# - dataCombinedList: named list of DataCombined objects keyed by name.
#
# @keywords internal
# @noRd
.createPlotGridsFromDataFrames <- function(
  dfPlotConfigurations,
  dfPlotGrids,
  dataCombinedList
) {
  dfPlotConfigurations <- .validatePlotConfigurationFromExcel(
    dfPlotConfigurations,
    names(dataCombinedList)
  )
  dfPlotGrids <- .validatePlotGridsFromExcel(
    dfPlotGrids,
    unique(dfPlotConfigurations$plotID)
  )

  defaultPlotConfiguration <- createEsqlabsPlotConfiguration()
  plotConfigurationList <- apply(dfPlotConfigurations, 1, \(row) {
    plotConfiguration <- .createConfigurationFromRow(
      defaultConfiguration = defaultPlotConfiguration,
      row[
        !(names(row) %in%
          c(
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
          ))
      ]
    )
    if (!is.na(row[["title"]])) {
      plotConfiguration$title <- row[["title"]]
    }
    if ("subtitle" %in% names(row) && !is.na(row[["subtitle"]])) {
      plotConfiguration$subtitle <- row[["subtitle"]]
    }
    .validateLogScaleAxisLimits(plotConfiguration, row[["plotID"]])
    return(plotConfiguration)
  })
  names(plotConfigurationList) <- dfPlotConfigurations$plotID

  plotList <- lapply(dfPlotConfigurations$plotID, \(plotId) {
    dataCombined <- dataCombinedList[[
      dfPlotConfigurations[
        dfPlotConfigurations$plotID == plotId,
      ]$DataCombinedName
    ]]
    switch(
      dfPlotConfigurations[dfPlotConfigurations$plotID == plotId, ]$plotType,
      individual = plotIndividualTimeProfile(
        dataCombined,
        plotConfigurationList[[plotId]]
      ),
      population = {
        aggregation <- dfPlotConfigurations[
          dfPlotConfigurations$plotID == plotId,
        ]$aggregation
        quantiles <- dfPlotConfigurations[
          dfPlotConfigurations$plotID == plotId,
        ]$quantiles
        nsd <- dfPlotConfigurations[
          dfPlotConfigurations$plotID == plotId,
        ]$nsd
        args <- list()
        args$dataCombined <- dataCombined
        args$defaultPlotConfiguration <- plotConfigurationList[[plotId]]
        if (!is.null(aggregation) && !is.na(aggregation)) {
          args$aggregation <- aggregation
        }
        if (!is.null(quantiles) && !is.na(quantiles)) {
          args$quantiles <- as.numeric(unlist(strsplit(quantiles, split = ",")))
        }
        if (!is.null(nsd) && !is.na(nsd)) {
          args$nsd <- as.numeric(nsd)
        }
        do.call(plotPopulationTimeProfile, args)
      },
      observedVsSimulated = {
        foldDist <- dfPlotConfigurations[
          dfPlotConfigurations$plotID == plotId,
        ]$foldDistance
        if (is.na(foldDist)) {
          plotObservedVsSimulated(
            dataCombined,
            plotConfigurationList[[plotId]]
          )
        } else {
          plotObservedVsSimulated(
            dataCombined,
            plotConfigurationList[[plotId]],
            foldDistance = as.numeric(unlist(strsplit(foldDist, split = ",")))
          )
        }
      },
      residualsVsSimulated = plotResidualsVsSimulated(
        dataCombined,
        plotConfigurationList[[plotId]]
      ),
      residualsVsTime = plotResidualsVsTime(
        dataCombined,
        plotConfigurationList[[plotId]]
      )
    )
  })
  names(plotList) <- dfPlotConfigurations$plotID

  defaultPlotGridConfig <- createEsqlabsPlotGridConfiguration()
  plotGrids <- apply(dfPlotGrids, 1, \(row) {
    plotGridConfiguration <- .createConfigurationFromRow(
      defaultConfiguration = defaultPlotGridConfig,
      row[!(names(row) %in% c("name", "plotIDs", "title"))]
    )
    if (!is.na(row$title) && !is.null(row$title)) {
      plotGridConfiguration$title <- row$title
    }
    plotsToAdd <- plotList[intersect(
      unlist(row$plotIDs),
      dfPlotConfigurations$plotID
    )]
    plotsToAdd <- plotsToAdd[lengths(plotsToAdd) != 0]
    if (length(plotsToAdd) == 0) {
      return(NULL)
    }
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
  plotGrids
}

# Public CRUD: plots ----

.validPlotTypes <- c(
  "individual",
  "population",
  "observedVsSimulated",
  "residualsVsSimulated",
  "residualsVsTime"
)

# Reject non-empty-scalar-string arguments uniformly across plot
# add/remove fns.
#
# @keywords internal
# @noRd
.requireNonEmptyString <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || nchar(x) == 0) {
    cli::cli_abort("{.arg {arg}} must be a non-empty string")
  }
  invisible(x)
}

# Normalise `...` for use as a single row in `as.data.frame()`:
# - NULL becomes NA so the column is kept (otherwise it is silently
#   dropped).
# - Multi-length vectors and lists are wrapped in `list(...)` so they
#   become a 1-element list-column instead of recycling into multiple
#   rows (e.g. `xValuesLimits = c(0, 100)` would otherwise expand to
#   two rows).
#
# @keywords internal
# @noRd
.namedDotsAsRow <- function(...) {
  dots <- list(...)
  lapply(dots, function(v) {
    if (is.null(v)) {
      return(NA)
    }
    if (length(v) > 1L || is.list(v)) {
      return(list(v))
    }
    v
  })
}

.checkDataCombinedEntry <- function(entry, dataType) {
  required <- if (dataType == "simulated") {
    c("label", "scenario", "path")
  } else {
    c("label", "dataSet")
  }
  for (field in required) {
    val <- entry[[field]]
    if (is.null(val) || (length(val) == 1L && is.na(val))) {
      cli::cli_abort(
        "DataCombined {dataType} entry is missing required field {.field {field}}."
      )
    }
  }
  invisible(TRUE)
}

.splitPlotIDs <- function(plotIdsStr) {
  if (is.null(plotIdsStr) || is.na(plotIdsStr) || !nzchar(plotIdsStr)) {
    return(character())
  }
  trimws(unlist(strsplit(as.character(plotIdsStr), ",", fixed = TRUE)))
}

#' Add a plot configuration to a Project
#'
#' Append a new row to `project$plots$plotConfiguration`. Errors if
#' `plotID` already exists, if `dataCombinedName` is not present in
#' `project$plots$dataCombined`, or if `plotType` is not one of the
#' supported types.
#'
#' @param project A `Project` object.
#' @param plotID Character scalar. Unique plot identifier.
#' @param dataCombinedName Character scalar. Must reference an existing
#'   DataCombined name on the project. Stored in the `DataCombinedName`
#'   column to match the JSON schema.
#' @param plotType Character scalar. One of `"individual"`,
#'   `"population"`, `"observedVsSimulated"`,
#'   `"residualsVsSimulated"`, `"residualsVsTime"`.
#' @param ... Optional plot-configuration fields, e.g. `title`,
#'   `subtitle`, `xUnit`, `yUnit`, `xAxisScale`, `yAxisScale`,
#'   `xValuesLimits`, `yValuesLimits`, `aggregation`, `quantiles`,
#'   `nsd`, `foldDistance`.
#' @returns The `project` object, invisibly.
#' @export
#' @family plots
addPlot <- function(project, plotID, dataCombinedName, plotType, ...) {
  validateIsOfType(project, "Project")
  .requireNonEmptyString(plotID, "plotID")
  .requireNonEmptyString(dataCombinedName, "dataCombinedName")
  .requireNonEmptyString(plotType, "plotType")

  existingPlots <- project$plots$plotConfiguration
  if (
    !is.null(existingPlots) &&
      nrow(existingPlots) > 0 &&
      plotID %in% existingPlots$plotID
  ) {
    cli::cli_abort("plot {.val {plotID}} already exists")
  }

  if (!(dataCombinedName %in% names(project$plots$dataCombined))) {
    cli::cli_abort(
      "dataCombinedName {.val {dataCombinedName}} not found in project"
    )
  }

  if (!(plotType %in% .validPlotTypes)) {
    cli::cli_abort(c(
      "Invalid plotType {.val {plotType}}.",
      "i" = "Must be one of: {.val {.validPlotTypes}}."
    ))
  }

  newRow <- c(
    list(
      plotID = plotID,
      DataCombinedName = dataCombinedName,
      plotType = plotType
    ),
    .namedDotsAsRow(...)
  )
  newRowDf <- as.data.frame(newRow, stringsAsFactors = FALSE)

  project$plots$plotConfiguration <- as.data.frame(dplyr::bind_rows(
    existingPlots,
    newRowDf
  ))
  project$.markModified()
  invisible(project)
}

#' Remove a plot configuration from a Project
#'
#' Drop the row with matching `plotID`. Warns (no-op) if `plotID` is
#' not found, and warns when the plot is referenced by any `plotGrids`
#' entry.
#'
#' @param project A `Project` object.
#' @param plotID Character scalar.
#' @returns The `project` object, invisibly.
#' @export
#' @family plots
removePlot <- function(project, plotID) {
  validateIsOfType(project, "Project")
  .requireNonEmptyString(plotID, "plotID")

  df <- project$plots$plotConfiguration
  if (is.null(df) || nrow(df) == 0 || !(plotID %in% df$plotID)) {
    cli::cli_warn("plot {.val {plotID}} not found; no-op.")
    return(invisible(project))
  }

  grids <- project$plots$plotGrids
  if (!is.null(grids) && nrow(grids) > 0) {
    referencingGrids <- grids$name[vapply(
      grids$plotIDs,
      function(s) plotID %in% .splitPlotIDs(s),
      logical(1)
    )]
    if (length(referencingGrids) > 0) {
      cli::cli_warn(c(
        "Removed plot {.val {plotID}} is still referenced by {length(referencingGrids)} plot grid{?s}:",
        "*" = "{referencingGrids}"
      ))
    }
  }

  project$plots$plotConfiguration <- df[
    which(df$plotID != plotID),
    ,
    drop = FALSE
  ]
  project$.markModified()
  invisible(project)
}

#' Add a plot grid to a Project
#'
#' Append a new row to `project$plots$plotGrids`. Errors if `name`
#' already exists or if any of the supplied `plotIDs` are not present
#' in `project$plots$plotConfiguration`.
#'
#' @param project A `Project` object.
#' @param name Character scalar. Unique plot-grid name.
#' @param plotIDs Character vector of `plotID`s to include in the grid.
#'   Stored internally as a comma-separated string.
#' @param ... Optional plot-grid fields, e.g. `title`, `subtitle`.
#' @returns The `project` object, invisibly.
#' @export
#' @family plots
addPlotGrid <- function(project, name, plotIDs, ...) {
  validateIsOfType(project, "Project")
  .requireNonEmptyString(name, "name")
  if (
    !is.character(plotIDs) ||
      length(plotIDs) == 0L ||
      any(is.na(plotIDs)) ||
      any(nchar(plotIDs) == 0)
  ) {
    cli::cli_abort("{.arg plotIDs} must be a non-empty character vector")
  }

  existingGrids <- project$plots$plotGrids
  if (
    !is.null(existingGrids) &&
      nrow(existingGrids) > 0 &&
      name %in% existingGrids$name
  ) {
    cli::cli_abort("plot grid {.val {name}} already exists")
  }

  existingPlotIDs <- project$plots$plotConfiguration$plotID
  if (is.null(existingPlotIDs)) {
    cli::cli_abort(c(
      "no plots are defined; add plots before creating a plot grid.",
      "i" = "use {.fn addPlot} to add plots referenced by {.arg plotIDs}."
    ))
  }
  unknown <- setdiff(plotIDs, existingPlotIDs)
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "{.arg plotIDs} references unknown plotIDs:",
      "x" = "{.val {unknown}}"
    ))
  }

  newRow <- c(
    list(
      name = name,
      plotIDs = paste(plotIDs, collapse = ", ")
    ),
    .namedDotsAsRow(...)
  )
  newRowDf <- as.data.frame(newRow, stringsAsFactors = FALSE)

  project$plots$plotGrids <- as.data.frame(dplyr::bind_rows(
    existingGrids,
    newRowDf
  ))
  project$.markModified()
  invisible(project)
}

#' Remove a plot grid from a Project
#'
#' Drop the row with matching `name`. Warns (no-op) if `name` is not
#' present.
#'
#' @param project A `Project` object.
#' @param name Character scalar.
#' @returns The `project` object, invisibly.
#' @export
#' @family plots
removePlotGrid <- function(project, name) {
  validateIsOfType(project, "Project")
  .requireNonEmptyString(name, "name")

  df <- project$plots$plotGrids
  if (is.null(df) || nrow(df) == 0 || !(name %in% df$name)) {
    cli::cli_warn("plot grid {.val {name}} not found; no-op.")
    return(invisible(project))
  }

  project$plots$plotGrids <- df[which(df$name != name), , drop = FALSE]
  project$.markModified()
  invisible(project)
}

#' Add a DataCombined to a Project
#'
#' Append a new DataCombined entry (one or more simulated and/or
#' observed rows) to `project$plots$dataCombined`. Mirrors the JSON
#' `plots.dataCombined[]` shape — one call per DataCombined.
#'
#' @param project A `Project` object.
#' @param name Character scalar. Unique DataCombined name.
#' @param simulated List of named lists. Each must include `label`,
#'   `scenario`, and `path`. Optional fields: `group`, `xOffsets`,
#'   `xOffsetsUnits`, `yOffsets`, `yOffsetsUnits`, `xScaleFactors`,
#'   `yScaleFactors`.
#' @param observed List of named lists. Each must include `label` and
#'   `dataSet`. Optional fields: same as `simulated` minus `scenario`
#'   and `path`.
#' @returns The `project` object, invisibly.
#' @export
#' @family dataCombined
addDataCombined <- function(
  project,
  name,
  simulated = list(),
  observed = list()
) {
  validateIsOfType(project, "Project")
  .requireNonEmptyString(name, "name")

  if (name %in% names(project$plots$dataCombined)) {
    cli::cli_abort("DataCombined {.val {name}} already exists")
  }

  if (length(simulated) == 0L && length(observed) == 0L) {
    cli::cli_abort(
      "addDataCombined requires at least one simulated or observed entry"
    )
  }

  for (e in simulated) .checkDataCombinedEntry(e, "simulated")
  for (e in observed) .checkDataCombinedEntry(e, "observed")

  if (is.null(project$plots)) {
    project$plots <- list(
      dataCombined = list(),
      plotConfiguration = data.frame(),
      plotGrids = data.frame()
    )
  }
  project$plots$dataCombined[[name]] <- list(
    simulated = simulated,
    observed = observed
  )
  project$.markModified()
  invisible(project)
}

#' Remove a DataCombined from a Project
#'
#' Drop the named entry from `project$plots$dataCombined`. Warns (and
#' is a no-op) if `name` is not present, and warns about any
#' `plotConfiguration` rows that still reference it.
#'
#' @param project A `Project` object.
#' @param name Character scalar. DataCombined name to remove.
#' @returns The `project` object, invisibly.
#' @export
#' @family dataCombined
removeDataCombined <- function(project, name) {
  validateIsOfType(project, "Project")
  .requireNonEmptyString(name, "name")

  if (!(name %in% names(project$plots$dataCombined))) {
    cli::cli_warn("DataCombined {.val {name}} not found; no-op.")
    return(invisible(project))
  }

  plotCfg <- project$plots$plotConfiguration
  if (!is.null(plotCfg) && nrow(plotCfg) > 0) {
    referencingPlots <- plotCfg$plotID[
      !is.na(plotCfg$DataCombinedName) & plotCfg$DataCombinedName == name
    ]
    if (length(referencingPlots) > 0) {
      cli::cli_warn(c(
        "Removed DataCombined {.val {name}} is still referenced by {length(referencingPlots)} plot{?s}:",
        "*" = "{referencingPlots}"
      ))
    }
  }

  project$plots$dataCombined[[name]] <- NULL
  project$.markModified()
  invisible(project)
}
