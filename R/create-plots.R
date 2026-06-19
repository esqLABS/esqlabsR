# Plot generation ----
#
# Public createPlots / createPlotsFromExcel plus the parsing, validation,
# and configuration helpers they use. Stateless: pulls a Project's
# dataCombined / plotConfiguration / plotGrids and returns a named list of
# plot-grid objects.

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

#' @rdname createPlots
#' @export
createPlotsFromExcel <- function(...) {
  lifecycle::deprecate_soft(
    what = "createPlotsFromExcel()",
    with = "createPlots()",
    when = "6.0.0"
  )
  createPlots(...)
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
#' Check if the `object` contains an active binding with the name `field`
#'
#' @param object A class or an instance of a class to check
#' @param field Name of the field
#'
#' @returns `TRUE` if the `object` has an active binding `field`, `FALSE`
#'   otherwise.
#' @keywords internal
.validateClassHasField <- function(object, field) {
  if (!any(names(object) == field)) {
    return(FALSE)
  }
  return(TRUE)
}

#' Validate the plotConfiguration sheet read from Excel
#'
#' @param dfPlotConfigurations Data frame created by reading the
#'   plotConfiguration sheet.
#' @param dataCombinedNames Names of the DataCombined that are referenced in
#'   the plot configurations.
#'
#' @returns Processed `dfPlotConfigurations`.
#' @keywords internal
#' @noRd
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

#' Build named list of plot-grid objects from data.frame configurations.
#'
#' Used by `createPlots(project, ...)`.
#'
#' @param dfPlotConfigurations data.frame with one row per plot, columns
#'   include plotID, DataCombinedName, plotType, title, subtitle, plus
#'   axis/styling fields. Rows whose DataCombinedName is not present in
#'   `dataCombinedList` are pruned by `.validatePlotConfigurationFromExcel()`.
#' @param dfPlotGrids data.frame with one row per grid, columns include name,
#'   plotIDs, title.
#' @param dataCombinedList named list of DataCombined objects keyed by name.
#'
#' @noRd
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
