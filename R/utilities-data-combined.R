# DataCombined creation ----

#' Generate DataCombined objects from a Project
#'
#' @param dataCombinedNames Names of the DataCombined objects that will be
#'   created. If a DataCombined with a given name is not defined in the
#'   project configuration, an error is thrown. Can be used together with
#'   `plotGridNames`.
#' @param plotGridNames Names of the plot grid specified in the plot
#'   configuration. Each data combined used by the specified plot grids will be
#'   created. Can be used together with `dataCombinedNames`.
#' @param project Object of class `Project` that
#'   contains information about the output paths and plots configuration.
#' @param simulatedScenarios A list of simulated scenarios as returned by
#'   `runScenarios()`
#' @param stopIfNotFound If TRUE (default), the function stops if any of the
#'   simulated results or observed data are not found. If FALSE a warning is
#'   printed.
#'
#' @returns A list of `DataCombined` objects, or an empty list if both
#'   `dataCombinedNames` and `plotGridNames` are `NULL` or `stopIfNotFound =
#'   TRUE` and the specified `DataCombined` could not be created.
#'
#' @import tidyr
#'
#' @export
createDataCombined <- function(
  project,
  dataCombinedNames = NULL,
  plotGridNames = NULL,
  simulatedScenarios = NULL,
  stopIfNotFound = TRUE
) {
  validateIsOfType(project, "Project")
  observedData <- loadObservedData(project)
  validateIsString(plotGridNames, nullAllowed = TRUE)

  # Exit early if no data combined names or plot grid names are provided
  if (is.null(dataCombinedNames) && is.null(plotGridNames)) {
    return(list())
  }

  # If plotGridNames are provided, extract the names of required data combined
  # and add them to the passed data combined names
  if (!is.null(plotGridNames)) {
    # Combine the passed data combined names with the names required for
    # the passed plots
    dataCombinedNames <- union(
      dataCombinedNames,
      .extractDataCombinedNamesForPlots(
        project = project,
        plotGridNames = plotGridNames
      )
    )
  }

  dfDataCombined <- project$plots$dataCombined
  dfDataCombined <- dplyr::filter(
    dfDataCombined,
    DataCombinedName %in% dataCombinedNames
  )

  missingNames <- setdiff(
    dataCombinedNames[!is.na(dataCombinedNames)],
    dfDataCombined$DataCombinedName
  )
  if (length(missingNames) > 0) {
    stop(messages$stopDataCombinedNamesNotFound(missingNames))
  }

  dfDataCombined <- .validateDataCombined(
    dfDataCombined,
    simulatedScenarios,
    observedData,
    stopIfNotFound
  )

  # create named list of DataCombined objects
  dataCombinedList <- lapply(unique(dfDataCombined$DataCombinedName), \(name) {
    dataCombined <- DataCombined$new()
    # add data to DataCombined object
    # add simulated data
    simulated <- dplyr::filter(
      dfDataCombined,
      DataCombinedName == name,
      dataType == "simulated"
    )
    if (nrow(simulated) > 0) {
      for (j in seq_len(nrow(simulated))) {
        # Check if the output has been simulated
        # If yes, add it to the DataCombined
        if (
          any(
            simulatedScenarios[[
              simulated[j, ]$scenario
            ]]$results$allQuantityPaths ==
              simulated[j, ]$path
          )
        ) {
          dataCombined$addSimulationResults(
            simulationResults = simulatedScenarios[[
              simulated[j, ]$scenario
            ]]$results,
            quantitiesOrPaths = simulated[j, ]$path,
            groups = simulated[j, ]$group,
            names = simulated[j, ]$label
          )
        } else {
          if (stopIfNotFound) {
            stop(messages$stopWrongOutputPath(
              dataCombinedName = name,
              scenarioName = simulated[j, ]$scenario,
              path = simulated[j, ]$path
            ))
          }
          warning(messages$stopWrongOutputPath(
            dataCombinedName = name,
            scenarioName = simulated[j, ]$scenario,
            path = simulated[j, ]$path
          ))
        }
      }
    }

    # add observed data
    observed <- dplyr::filter(
      dfDataCombined,
      DataCombinedName == name,
      dataType == "observed"
    )
    if (nrow(observed) > 0) {
      dataSets <- observedData[observed$dataSet]
      dataCombined$addDataSets(
        dataSets,
        names = observed$label,
        groups = observed$group
      )
    }
    return(dataCombined)
  })
  names(dataCombinedList) <- unique(dfDataCombined$DataCombinedName)

  # apply data transformations
  dfTransform <- dplyr::filter(
    dfDataCombined,
    !is.na(xOffsets) |
      !is.na(yOffsets) |
      !is.na(xScaleFactors) |
      !is.na(yScaleFactors)
  )
  # Apply data transformations if specified
  if (dim(dfTransform)[[1]] != 0) {
    apply(dfTransform, 1, \(row) {
      # Get the data frame of the Data combined to retrieve units and MW
      dataCombinedDf <- dataCombinedList[[row[[
        "DataCombinedName"
      ]]]]$toDataFrame()
      singleRow <- dataCombinedDf[dataCombinedDf$name == row[["label"]], ][1, ]

      # Check if x/yOffsetsUnits are defined when x/yOffsets are non empty.
      if (
        (!is.na(row[["xOffsets"]]) & is.na(row[["xOffsetsUnits"]])) |
          (!is.na(row[["yOffsets"]]) & is.na(row[["yOffsetsUnits"]]))
      ) {
        stop(messages$offsetUnitsNotDefined(row[[
          "DataCombinedName"
        ]]))
      }

      # If offsets are defined, convert them to the default unit of the data
      # Extract the base unit of the data (or simulation result) and the unit
      # defined for the offset.
      # We don't have to check for NAs because 'toUnit()' returns NA for NA
      xDimension <- singleRow$xDimension
      xBaseUnit <- row[["xOffsetsUnits"]]
      xTargetUnit <- singleRow$xUnit
      # Empty units should be converted to "" for the dimension "Fraction" or "Dimensionless"
      if (is.na(xTargetUnit)) {
        xTargetUnit <- ""
      }
      row[["xOffsets"]] <- toUnit(
        quantityOrDimension = xDimension,
        values = as.numeric(row[["xOffsets"]]),
        targetUnit = xTargetUnit,
        sourceUnit = xBaseUnit
      )

      yDimension <- singleRow$yDimension
      yBaseUnit <- row[["yOffsetsUnits"]]
      yTargetUnit <- singleRow$yUnit
      yMW <- singleRow$molWeight
      # Empty units should be converted to "" for the dimension "Fraction" or "Dimensionless"
      if (is.na(yTargetUnit)) {
        yTargetUnit <- ""
      }
      row[["yOffsets"]] <- toUnit(
        quantityOrDimension = yDimension,
        values = as.numeric(row[["yOffsets"]]),
        targetUnit = yTargetUnit,
        sourceUnit = yBaseUnit,
        molWeight = yMW,
        molWeightUnit = ospUnits$`Molecular weight`$`g/mol`
      )

      dataCombinedList[[row[["DataCombinedName"]]]]$setDataTransformations(
        forNames = row[["label"]],
        xOffsets = as.numeric(row[["xOffsets"]]),
        yOffsets = as.numeric(row[["yOffsets"]]),
        xScaleFactors = as.numeric(row[["xScaleFactors"]]),
        yScaleFactors = as.numeric(row[["yScaleFactors"]])
      )
    })
  }

  return(dataCombinedList)
}

# Internal helpers ----

#' Validate and process a DataCombined data frame
#'
#' @param dfDataCombined Data frame with DataCombined definitions
#' @param simulatedScenarios List of simulated scenarios as created by
#'   `runScenarios()`
#' @param observedData Observed data objects
#' @param stopIfNotFound if `TRUE`, throw an error if a simulated result of an
#'   observed data are not found
#'
#' @returns Processed `dfDataCombined`
#' @keywords internal
.validateDataCombined <- function(
  dfDataCombined,
  simulatedScenarios,
  observedData,
  stopIfNotFound
) {
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
  missingLabel <- sum(is.na(
    dfDataCombined[dfDataCombined$dataType == "simulated", ]$scenario
  ))
  if (missingLabel > 0) {
    stop(messages$missingScenarioName())
  }

  # dataType == simulated, but no path defined - throw error
  missingLabel <- is.na(
    dfDataCombined[dfDataCombined$dataType == "simulated", ]$path
  )
  if (sum(missingLabel) > 0) {
    stop(messages$stopNoPathProvided(dfDataCombined[
      dfDataCombined$dataType == "simulated",
    ]$DataCombinedName[missingLabel]))
  }

  # dataType == observed, but no data set defined - throw error
  missingLabel <- is.na(
    dfDataCombined[dfDataCombined$dataType == "observed", ]$dataSet
  )
  if (sum(missingLabel) > 0) {
    stop(messages$stopNoDataSetProvided(dfDataCombined[
      dfDataCombined$dataType == "observed",
    ]$DataCombinedName[missingLabel]))
  }

  # Store the names of all DataCombined before filtering. This is required
  # to create empty rows for DataCombined for which no data exists. This way,
  # empty data combined can still be created.
  dcNames <- unique(dfDataCombined$DataCombinedName)

  # scenario not present in simulatedScenarios
  missingScenarios <- setdiff(
    setdiff(dfDataCombined$scenario, names(simulatedScenarios)),
    NA
  )
  if (length(missingScenarios) != 0) {
    if (stopIfNotFound) {
      stop(messages$warningInvalidScenarioName(missingScenarios))
    }
    warning(messages$warningInvalidScenarioName(missingScenarios))
    dfDataCombined <- dplyr::filter(
      dfDataCombined,
      (dataType == "observed") | !(scenario %in% missingScenarios)
    )
  }
  # data set name not present in observedData
  missingDataSets <- setdiff(
    setdiff(dfDataCombined$dataSet, names(observedData)),
    NA
  )
  if (length(missingDataSets) != 0) {
    if (stopIfNotFound) {
      stop(messages$stopInvalidDataSetName(missingDataSets))
    }
    warning(messages$warningInvalidDataSetName(missingDataSets))
    dfDataCombined <- dfDataCombined[
      !(dfDataCombined$dataSet %in% missingDataSets),
    ]
  }
  # Identify the names of DataCombined that have been completely removed
  missingDc <- setdiff(dcNames, unique(dfDataCombined$DataCombinedName))
  # Create empty rows for each missing DataCombined
  for (name in missingDc) {
    dfDataCombined[nrow(dfDataCombined) + 1, 1] <- name
  }

  return(dfDataCombined)
}


#' Extract names of DataCombined required for the creation of specified plots
#'
#' @param plotGridNames Names of the plot grid specified in the plot
#'   configuration.
#' @param project Object of class `Project` that
#'   contains information about the output paths and plots configuration.
#'
#' @returns A list with the names of required DataCombined
#' @noRd
.extractDataCombinedNamesForPlots <- function(
  project,
  plotGridNames
) {
  plotConfigurations <- .getPlotConfigurations(
    project = project,
    plotGridNames = plotGridNames
  )
  dfPlotConfigurations <- plotConfigurations$plotConfigurations
  dataCombinedNames <- unique(dfPlotConfigurations$DataCombinedName)

  return(dataCombinedNames)
}


#' @rdname createDataCombined
#' @export
createDataCombinedFromExcel <- function(...) {
  lifecycle::deprecate_soft(
    what = "createDataCombinedFromExcel()",
    with = "createDataCombined()",
    when = "6.0.0"
  )
  createDataCombined(...)
}

# add/remove DataCombined ----

.dataCombinedTransformCols <- c(
  "xOffsets",
  "xOffsetsUnits",
  "yOffsets",
  "yOffsetsUnits",
  "xScaleFactors",
  "yScaleFactors"
)

# Reject non-empty-scalar-string arguments uniformly across add/remove fns.
.requireNonEmptyString <- function(x, arg) {
  if (!is.character(x) || length(x) != 1 || is.na(x) || nchar(x) == 0) {
    stop(paste0(arg, " must be a non-empty string"))
  }
  invisible(x)
}

# Replace NULL values in `...` with NA so that `as.data.frame()` keeps the
# column (otherwise NULL entries are silently dropped).
.namedDotsAsRow <- function(...) {
  dots <- list(...)
  dots <- lapply(dots, function(v) if (is.null(v)) NA else v)
  dots
}

.buildDataCombinedRow <- function(dataCombinedName, dataType, entry) {
  required <- if (dataType == "simulated") {
    c("label", "scenario", "path")
  } else {
    c("label", "dataSet")
  }
  for (field in required) {
    if (is.null(entry[[field]]) || is.na(entry[[field]])) {
      msgFn <- if (dataType == "simulated") {
        messages$dataCombinedSimulatedMissingField
      } else {
        messages$dataCombinedObservedMissingField
      }
      stop(msgFn(field))
    }
  }
  row <- list(
    DataCombinedName = dataCombinedName,
    dataType = dataType,
    label = entry$label,
    scenario = if (dataType == "simulated") entry$scenario else NA,
    path = if (dataType == "simulated") entry$path else NA,
    dataSet = if (dataType == "observed") entry$dataSet else NA,
    group = entry$group %||% NA
  )
  for (col in .dataCombinedTransformCols) {
    row[[col]] <- entry[[col]] %||% NA
  }
  as.data.frame(row, stringsAsFactors = FALSE)
}

#' Add a DataCombined to a Project
#'
#' @description Append a new DataCombined entry (one or more simulated and/or
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
#'   `dataSet`. Optional fields: same as `simulated` minus `scenario`/`path`.
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

  if (name %in% project$plots$dataCombined$DataCombinedName) {
    stop(messages$dataCombinedNameExists(name))
  }

  rows <- c(
    lapply(simulated, function(e) .buildDataCombinedRow(name, "simulated", e)),
    lapply(observed, function(e) .buildDataCombinedRow(name, "observed", e))
  )
  if (length(rows) == 0) {
    stop("addDataCombined requires at least one simulated or observed entry")
  }

  project$plots$dataCombined <- as.data.frame(dplyr::bind_rows(
    c(list(project$plots$dataCombined), rows)
  ))
  project$.markModified()
  invisible(project)
}

#' Remove a DataCombined from a Project
#'
#' @description Drop all rows in `project$plots$dataCombined` matching the
#' given name. Warns (and is a no-op) if `name` is not present, and warns
#' about any `plotConfiguration` rows that still reference it.
#'
#' @param project A `Project` object.
#' @param name Character scalar. DataCombined name to remove.
#' @returns The `project` object, invisibly.
#' @export
#' @family dataCombined
removeDataCombined <- function(project, name) {
  validateIsOfType(project, "Project")
  .requireNonEmptyString(name, "name")

  df <- project$plots$dataCombined
  if (is.null(df) || nrow(df) == 0 || !(name %in% df$DataCombinedName)) {
    cli::cli_warn(messages$dataCombinedNotFound(name))
    return(invisible(project))
  }

  plotCfg <- project$plots$plotConfiguration
  if (!is.null(plotCfg) && nrow(plotCfg) > 0) {
    referencingPlots <- plotCfg$plotID[
      !is.na(plotCfg$DataCombinedName) & plotCfg$DataCombinedName == name
    ]
    if (length(referencingPlots) > 0) {
      cli::cli_warn(messages$dataCombinedReferencedByPlot(
        name,
        referencingPlots
      ))
    }
  }

  project$plots$dataCombined <- df[df$DataCombinedName != name, , drop = FALSE]
  project$.markModified()
  invisible(project)
}
