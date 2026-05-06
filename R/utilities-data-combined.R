#' Generate DataCombined objects from a Project
#'
#' @description
#' Builds [`ospsuite::DataCombined`] objects from a JSON-driven
#' [Project][loadProject()]. The project's `plots$dataCombined` section
#' declares the simulated/observed entries; `loadObservedData(project)`
#' resolves observed sources internally. Either `dataCombinedNames` or
#' `plotGridNames` (or both) selects which DataCombined to build.
#'
#' @param project A `Project` (see [loadProject()]).
#' @param dataCombinedNames Names of the DataCombined entries to build. If
#'   any name is not declared in `project$plots$dataCombined`, an error is
#'   thrown.
#' @param plotGridNames Names of plot grids whose DataCombined dependencies
#'   should be built. Combined with `dataCombinedNames` if both are given.
#' @param simulatedScenarios A named list of simulated scenarios (as
#'   returned by [runScenarios()]).
#' @param stopIfNotFound If `TRUE` (default), the function errors when a
#'   referenced simulated path or observed dataSet cannot be resolved. If
#'   `FALSE`, a warning is emitted and the entry is skipped.
#'
#' @returns A named list of `DataCombined` objects, one per requested name.
#'   Empty list when no names are requested.
#'
#' @export
createDataCombined <- function(
  project,
  dataCombinedNames = NULL,
  plotGridNames = NULL,
  simulatedScenarios = NULL,
  stopIfNotFound = TRUE
) {
  ospsuite.utils::validateIsOfType(project, "Project")
  validateIsString(plotGridNames, nullAllowed = TRUE)

  if (is.null(dataCombinedNames) && is.null(plotGridNames)) {
    return(list())
  }

  observedData <- loadObservedData(project)

  if (!is.null(plotGridNames)) {
    dataCombinedNames <- union(
      dataCombinedNames,
      .extractDataCombinedNamesForPlotsFromProject(project, plotGridNames)
    )
  }

  allSpecs <- project$plots$dataCombined %||% list()
  missingNames <- setdiff(
    dataCombinedNames[!is.na(dataCombinedNames)],
    names(allSpecs)
  )
  if (length(missingNames) > 0) {
    stop(messages$stopDataCombinedNamesNotFound(missingNames))
  }

  dfDataCombined <- .specsToDataCombinedDataFrame(
    allSpecs[intersect(names(allSpecs), dataCombinedNames)]
  )

  .createDataCombinedFromProcessedDF(
    dfDataCombined = dfDataCombined,
    simulatedScenarios = simulatedScenarios,
    observedData = observedData,
    stopIfNotFound = stopIfNotFound
  )
}

# Convert the named-list `dataCombined` spec from project$plots into the
# flat data.frame the legacy Excel-driven code path uses. One row per
# entry (simulated or observed). Missing optional fields become NA.
#
# @keywords internal
# @noRd
.specsToDataCombinedDataFrame <- function(specs) {
  if (length(specs) == 0) {
    return(data.frame(
      DataCombinedName = character(0),
      dataType = character(0),
      label = character(0),
      stringsAsFactors = FALSE
    ))
  }
  rows <- list()
  for (name in names(specs)) {
    spec <- specs[[name]]
    for (entry in spec$simulated %||% list()) {
      rows[[length(rows) + 1L]] <- .specEntryToRow(name, "simulated", entry)
    }
    for (entry in spec$observed %||% list()) {
      rows[[length(rows) + 1L]] <- .specEntryToRow(name, "observed", entry)
    }
  }
  if (length(rows) == 0) {
    # All requested names declared no simulated or observed entries.
    # Fall back to one empty row per name so downstream code creates
    # empty DataCombined objects (matching legacy behaviour).
    return(data.frame(
      DataCombinedName = names(specs),
      dataType = NA_character_,
      label = NA_character_,
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
}

.specEntryToRow <- function(dataCombinedName, dataType, entry) {
  list(
    DataCombinedName = dataCombinedName,
    dataType = dataType,
    label = entry$label %||% NA_character_,
    scenario = entry$scenario %||% NA_character_,
    path = entry$path %||% NA_character_,
    dataSet = entry$dataSet %||% NA_character_,
    group = entry$group %||% NA_character_,
    xOffsets = entry$xOffsets %||% NA_real_,
    xOffsetsUnits = entry$xOffsetsUnits %||% NA_character_,
    yOffsets = entry$yOffsets %||% NA_real_,
    yOffsetsUnits = entry$yOffsetsUnits %||% NA_character_,
    xScaleFactors = entry$xScaleFactors %||% NA_real_,
    yScaleFactors = entry$yScaleFactors %||% NA_real_
  )
}

# Find DataCombined names referenced by the requested plot grids.
#
# @keywords internal
# @noRd
.extractDataCombinedNamesForPlotsFromProject <- function(
  project,
  plotGridNames
) {
  gridDf <- project$plots$plotGrids %||% data.frame()
  if (nrow(gridDf) == 0) return(character(0))
  selectedGrids <- gridDf[gridDf$name %in% plotGridNames, , drop = FALSE]
  if (nrow(selectedGrids) == 0) return(character(0))
  ids <- unique(unlist(strsplit(selectedGrids$plotIDs, "\\s*,\\s*")))
  cfgDf <- project$plots$plotConfiguration %||% data.frame()
  if (nrow(cfgDf) == 0) return(character(0))
  unique(cfgDf$DataCombinedName[cfgDf$plotID %in% ids])
}

#' Generate DataCombined objects from Excel (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")` Use [createDataCombined()] with a
#' [Project][loadProject()].
#'
#' @param projectConfiguration A `ProjectConfiguration` object pointing at
#'   a `Plots.xlsx`.
#' @param dataCombinedNames Names of DataCombined entries to build.
#' @param plotGridNames Names of plot grids whose DataCombined dependencies
#'   should be built.
#' @param simulatedScenarios Named list of simulated scenarios.
#' @param observedData Named list of observed `DataSet` objects.
#' @param stopIfNotFound If `TRUE`, errors on unresolved references; if
#'   `FALSE`, warns and skips.
#'
#' @returns Named list of `DataCombined` objects.
#' @import tidyr
#' @export
createDataCombinedFromExcel <- function(
  projectConfiguration,
  dataCombinedNames = NULL,
  plotGridNames = NULL,
  simulatedScenarios = NULL,
  observedData = NULL,
  stopIfNotFound = TRUE
) {
  lifecycle::deprecate_soft(
    when = "5.7.0",
    what = "createDataCombinedFromExcel()",
    with = "createDataCombined(project)",
    details = "Migrate the Plots.xlsx workflow to a JSON Project."
  )
  validateIsOfType(observedData, "DataSet", nullAllowed = TRUE)
  validateIsOfType(projectConfiguration, "ProjectConfiguration")
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
        projectConfiguration = projectConfiguration,
        plotGridNames = plotGridNames
      )
    )
  }

  dfDataCombined <- readExcel(
    path = projectConfiguration$plotsFile,
    sheet = "DataCombined"
  )
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

  .createDataCombinedFromProcessedDF(
    dfDataCombined = dfDataCombined,
    simulatedScenarios = simulatedScenarios,
    observedData = observedData,
    stopIfNotFound = stopIfNotFound
  )
}

# Build named list of DataCombined objects from a flat data.frame whose
# rows describe simulated/observed entries. Shared between the JSON-driven
# createDataCombined() and the deprecated createDataCombinedFromExcel().
#
# @keywords internal
# @noRd
.createDataCombinedFromProcessedDF <- function(
  dfDataCombined,
  simulatedScenarios,
  observedData,
  stopIfNotFound
) {
  dfDataCombined <- .validateDataCombinedFromExcel(
    dfDataCombined,
    simulatedScenarios,
    observedData,
    stopIfNotFound
  )

  dataCombinedList <- lapply(unique(dfDataCombined$DataCombinedName), \(name) {
    dataCombined <- DataCombined$new()
    simulated <- dplyr::filter(
      dfDataCombined,
      DataCombinedName == name,
      dataType == "simulated"
    )
    if (nrow(simulated) > 0) {
      for (j in seq_len(nrow(simulated))) {
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

  dfTransform <- dplyr::filter(
    dfDataCombined,
    !is.na(xOffsets) |
      !is.na(yOffsets) |
      !is.na(xScaleFactors) |
      !is.na(yScaleFactors)
  )
  if (dim(dfTransform)[[1]] != 0) {
    apply(dfTransform, 1, \(row) {
      dataCombinedDf <- dataCombinedList[[row[[
        "DataCombinedName"
      ]]]]$toDataFrame()
      singleRow <- dataCombinedDf[dataCombinedDf$name == row[["label"]], ][1, ]

      if (
        (!is.na(row[["xOffsets"]]) & is.na(row[["xOffsetsUnits"]])) |
          (!is.na(row[["yOffsets"]]) & is.na(row[["yOffsetsUnits"]]))
      ) {
        stop(messages$offsetUnitsNotDefined(row[["DataCombinedName"]]))
      }

      xDimension <- singleRow$xDimension
      xBaseUnit <- row[["xOffsetsUnits"]]
      xTargetUnit <- singleRow$xUnit
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

#' Validate and process the 'DataCombined' sheet
#'
#' @param dfDataCombined Data frame created by reading the ' DataCombined' sheet
#' @param simulatedScenarios List of simulated scenarios as created by
#'   `runScenarios()`
#' @param observedData Observed data objects
#' @param stopIfNotFound if `TRUE`, throw an error if a simulated result of an
#'   observed data are not found
#'
#' @returns Processed `dfDataCombined`
#' @keywords internal
.validateDataCombinedFromExcel <- function(
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

  # warnings for invalid data in plot definitions from excel
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
#' @param plotGridNames Names of the plot grid specified in the sheet
#'   `plotGrids`.
#' @param projectConfiguration Object of class `ProjectConfiguration` that
#'   contains information about the output paths and the excel file where plots
#'   are defined.
#'
#' @returns A list with the names of required DataCombined
#' @noRd
.extractDataCombinedNamesForPlots <- function(
  projectConfiguration,
  plotGridNames
) {
  dfPlotConfigurations <- .readPlotConfigurations(
    projectConfiguration = projectConfiguration,
    plotGridNames = plotGridNames
  )$plotConfigurations
  dataCombinedNames <- unique(dfPlotConfigurations$DataCombinedName)

  return(dataCombinedNames)
}
