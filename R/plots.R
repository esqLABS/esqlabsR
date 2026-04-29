# Plot generation ----

#' Generate plots from a Project
#'
#' @param project Object of class `Project` that
#'   contains information about the output paths and plots configuration.
#' @param simulatedScenarios A list of simulated scenarios as returned by
#'   `runScenarios()`. Can be `NULL` if no simulated data is required for the
#'   plots.
#' @param stopIfNotFound If TRUE (default), the function stops if any of the
#'   simulated results or observed data are not found. If FALSE a warning is
#'   printed.
#'
#' @param plotGridNames Names of the plot grids for which the figures will be
#'   created. If `NULL` (default), all plot grids will be created. If a plot
#'   grid with a given name does not exist, an error is thrown.
#'
#' @param dataCombinedList A (named) list of `DataCombined` objects as input to
#'   create plots defined in the `plotGridNames` argument. Missing
#'   `DataCombined` will be created from the project configuration (default
#'   behavior). Defaults to `NULL`, in which case all `DataCombined` are
#'   created from the project configuration.
#' @param validate If `TRUE` (default), the `plots` and `crossReferences`
#'   sections of the project are validated before plotting. Any critical
#'   errors abort the call with a formatted message. Set to `FALSE` to skip
#'   the pre-flight check. Validation is skipped automatically if no plots
#'   are configured, or if a full [validateProject()] has already succeeded
#'   since the last project mutation.
#'
#' @returns A list of `ggplot` objects
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
  validateIsOfType(project, "Project")
  validateIsString(plotGridNames, nullAllowed = TRUE)
  validateIsOfType(dataCombinedList, "DataCombined", nullAllowed = TRUE)
  if (!typeof(dataCombinedList) %in% c("list", "NULL")) {
    stop(messages$errorDataCombinedListMustBeList(typeof(dataCombinedList)))
  }

  if (isTRUE(validate) && !is.null(project$plots)) {
    .ensureValid(
      project,
      sections = c("plots", "crossReferences"),
      opName = "create plots"
    )
  }

  plotConfigurations <- .getPlotConfigurations(
    project = project,
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
  dataCombinedListFromConfig <- createDataCombined(
    project = project,
    dataCombinedNames = dataCombinedNames,
    simulatedScenarios = simulatedScenarios,
    stopIfNotFound = stopIfNotFound
  )
  # Add entries from the provided list of DataCombined.
  dataCombinedListFromConfig[names(dataCombinedList)] <- dataCombinedList
  dataCombinedList <- dataCombinedListFromConfig

  dfPlotConfigurations <- .validatePlotConfiguration(
    dfPlotConfigurations,
    names(dataCombinedList)
  )

  # create a list of plotConfiguration objects as defined in sheet "plotConfiguration"
  defaultPlotConfiguration <- createEsqlabsPlotConfiguration()
  plotConfigurationList <- apply(dfPlotConfigurations, 1, \(row) {
    plotConfiguration <- .createConfigurationFromRow(
      defaultConfiguration = defaultPlotConfiguration,
      # Have to exclude all columns that should not be vectorized
      # Excluding title and subtitle because they should not be processed,
      # e.g., split by ","
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
    # Apply title and subtitle properties
    if (!is.na(row[["title"]])) {
      plotConfiguration$title <- row[["title"]]
    }

    if ("subtitle" %in% names(row) && !is.na(row[["subtitle"]])) {
      plotConfiguration$subtitle <- row[["subtitle"]]
    }

    # Check for log scale with zero in axis limits
    .validateLogScaleAxisLimits(plotConfiguration, row[["plotID"]])

    return(plotConfiguration)
  })
  names(plotConfigurationList) <- dfPlotConfigurations$plotID

  # create a list of plots from dataCombinedList and plotConfigurationList
  plotList <- lapply(dfPlotConfigurations$plotID, \(plotId) {
    dataCombined <- dataCombinedList[[
      dfPlotConfigurations[
        dfPlotConfigurations$plotID == plotId,
      ]$DataCombinedName
    ]]
    switch(
      dfPlotConfigurations[dfPlotConfigurations$plotID == plotId, ]$plotType,
      # Individual time profile
      individual = plotIndividualTimeProfile(
        dataCombined,
        plotConfigurationList[[plotId]]
      ),
      # Population time profile
      population = {
        aggregation <- dfPlotConfigurations[
          dfPlotConfigurations$plotID == plotId,
        ]$aggregation
        quantiles <- dfPlotConfigurations[
          dfPlotConfigurations$plotID == plotId,
        ]$quantiles
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
        foldDist <- dfPlotConfigurations[
          dfPlotConfigurations$plotID == plotId,
        ]$foldDistance
        if (is.na(foldDist)) {
          plotObservedVsSimulated(dataCombined, plotConfigurationList[[plotId]])
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

# Internal helpers: parsing, validation, configuration ----

#' Parse and validate comma-separated field value
#'
#' Parses comma-separated values and validates using ospsuite.utils.
#' Provides error context (plotID, field name) for common issues.
#'
#' @param value Raw string value
#' @param fieldName Name of the field for error messages
#' @param plotID Optional plot ID for error context
#' @param expectedLength Expected number of values (NULL for any length)
#' @param expectedType Expected type ("numeric" or "character")
#' @returns Parsed and validated vector
#' @keywords internal
.parseMultiValueField <- function(
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
            messages$fieldFormatError(
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
          messages$fieldLengthError(
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
          messages$fieldTypeError(
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

#' Create a plotConfiguration object from a row of sheet 'plotConfiguration'
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
        stop(messages$invalidConfigurationProperty(
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
        value <- .parseMultiValueField(
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
.validatePlotConfiguration <- function(
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
.validatePlotGrids <- function(dfPlotGrids, plotIDs) {
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

#' Check if the `object` contains active binding with the name `field`
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
