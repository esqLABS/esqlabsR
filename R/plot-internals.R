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

#' Validate and process the 'exportConfiguration' sheet
#'
#' @param dfExportConfigurations Data frame created by reading the
#'   'exportConfiguration' sheet
#' @param plotGrids List of multipanel plots created previously
#'
#' @returns Processed `dfExportConfigurations`
#' @keywords internal
.validateExportConfigurations <- function(
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

#' Read plot configurations from a Project object
#'
#' Reads plot grids, plot configurations, and export configurations from
#' `project$plots` instead of Excel files.
#'
#' @param project Object of class `Project`
#' @param plotGridNames Names of the plot grids to filter for. If `NULL`,
#'   all plot grids are returned.
#'
#' @returns A list with elements `plotGrids`, `exportConfigurations`, and
#'   `plotConfigurations`, or `NULL` if no plot grids are defined.
#' @keywords internal
.getPlotConfigurations <- function(project, plotGridNames) {
  dfPlotGrids <- project$plots$plotGrids
  dfExportConfigurations <- project$plots$exportConfiguration

  # Handle empty export configurations (no columns)
  if (ncol(dfExportConfigurations) == 0) {
    dfExportConfigurations <- data.frame(
      plotGridName = character(0),
      name = character(0)
    )
  }

  # Rename outputName to name if present (legacy column name from Excel)
  if ("outputName" %in% names(dfExportConfigurations)) {
    dfExportConfigurations <- dplyr::rename(
      dfExportConfigurations,
      name = outputName
    )
  }

  # Filter for only specified plot grids
  if (!is.null(plotGridNames)) {
    missingPlotGrids <- setdiff(plotGridNames, unique(dfPlotGrids$name))
    if (length(missingPlotGrids) != 0) {
      stop(messages$invalidPlotGridNames(missingPlotGrids))
    }

    dfPlotGrids <- dplyr::filter(dfPlotGrids, name %in% plotGridNames)
    dfExportConfigurations <- dplyr::filter(
      dfExportConfigurations,
      plotGridName %in% plotGridNames
    )
  }

  # Exit early if no PlotGrid is defined
  if (nrow(dfPlotGrids) == 0) {
    return()
  }

  dfPlotConfigurations <- project$plots$plotConfiguration

  # Filter and validate plotGrids
  dfPlotGrids <- dplyr::filter(
    dfPlotGrids,
    !dplyr::if_all(dplyr::everything(), is.na)
  )

  # Exit early if all rows were NA

  if (nrow(dfPlotGrids) == 0) {
    return()
  }

  dfPlotGrids <- .validatePlotGrids(
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
