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

  allDataCombined <- project$plots$dataCombined %||% list()
  selected <- allDataCombined[
    intersect(names(allDataCombined), dataCombinedNames)
  ]

  missingNames <- setdiff(
    dataCombinedNames[!is.na(dataCombinedNames)],
    names(selected)
  )
  if (length(missingNames) > 0) {
    stop(messages$stopDataCombinedNamesNotFound(missingNames))
  }

  selected <- .validateDataCombined(
    selected,
    simulatedScenarios,
    observedData,
    stopIfNotFound
  )

  # create named list of DataCombined objects
  dataCombinedList <- lapply(names(selected), function(name) {
    spec <- selected[[name]]
    dataCombined <- DataCombined$new()

    # add simulated data
    for (sim in spec$simulated %||% list()) {
      hasPath <- any(
        simulatedScenarios[[sim$scenario]]$results$allQuantityPaths ==
          sim$path
      )
      if (hasPath) {
        dataCombined$addSimulationResults(
          simulationResults = simulatedScenarios[[sim$scenario]]$results,
          quantitiesOrPaths = sim$path,
          groups = sim$group %||% NA,
          names = sim$label
        )
      } else {
        msg <- messages$stopWrongOutputPath(
          dataCombinedName = name,
          scenarioName = sim$scenario,
          path = sim$path
        )
        if (stopIfNotFound) stop(msg) else warning(msg)
      }
    }

    # add observed data
    obsEntries <- spec$observed %||% list()
    if (length(obsEntries) > 0) {
      dataSets <- observedData[vapply(obsEntries, `[[`, character(1), "dataSet")]
      dataCombined$addDataSets(
        dataSets,
        names = vapply(obsEntries, `[[`, character(1), "label"),
        groups = vapply(
          obsEntries,
          function(e) e$group %||% NA_character_,
          character(1)
        )
      )
    }
    dataCombined
  })
  names(dataCombinedList) <- names(selected)

  # apply data transformations
  for (name in names(selected)) {
    spec <- selected[[name]]
    allEntries <- c(spec$simulated %||% list(), spec$observed %||% list())
    for (entry in allEntries) {
      hasOffset <- !is.null(entry$xOffsets) || !is.null(entry$yOffsets)
      hasScale <- !is.null(entry$xScaleFactors) ||
        !is.null(entry$yScaleFactors)
      if (!hasOffset && !hasScale) next

      # Get the data frame of the DataCombined to retrieve units and MW
      dataCombinedDf <- dataCombinedList[[name]]$toDataFrame()
      singleRow <- dataCombinedDf[dataCombinedDf$name == entry$label, ][1, ]

      # Check if x/yOffsetsUnits are defined when x/yOffsets are non empty
      if (
        (!is.null(entry$xOffsets) && is.null(entry$xOffsetsUnits)) ||
          (!is.null(entry$yOffsets) && is.null(entry$yOffsetsUnits))
      ) {
        stop(messages$offsetUnitsNotDefined(name))
      }

      xOffset <- if (!is.null(entry$xOffsets)) {
        toUnit(
          quantityOrDimension = singleRow$xDimension,
          values = as.numeric(entry$xOffsets),
          targetUnit = if (is.na(singleRow$xUnit)) "" else singleRow$xUnit,
          sourceUnit = entry$xOffsetsUnits
        )
      } else {
        NA_real_
      }

      yOffset <- if (!is.null(entry$yOffsets)) {
        toUnit(
          quantityOrDimension = singleRow$yDimension,
          values = as.numeric(entry$yOffsets),
          targetUnit = if (is.na(singleRow$yUnit)) "" else singleRow$yUnit,
          sourceUnit = entry$yOffsetsUnits,
          molWeight = singleRow$molWeight,
          molWeightUnit = ospUnits$`Molecular weight`$`g/mol`
        )
      } else {
        NA_real_
      }

      dataCombinedList[[name]]$setDataTransformations(
        forNames = entry$label,
        xOffsets = xOffset,
        yOffsets = yOffset,
        xScaleFactors = if (!is.null(entry$xScaleFactors)) {
          as.numeric(entry$xScaleFactors)
        } else {
          NA_real_
        },
        yScaleFactors = if (!is.null(entry$yScaleFactors)) {
          as.numeric(entry$yScaleFactors)
        } else {
          NA_real_
        }
      )
    }
  }

  return(dataCombinedList)
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

#' Validate and process a DataCombined named list
#'
#' Walks the nested `selected` named list and ensures each entry carries
#' the fields its `dataType` requires. Entries whose scenario / dataSet
#' is not in the simulated / observed pool are dropped (or trigger an
#' error, if `stopIfNotFound`). DataCombined that lose all entries this
#' way are kept as empty entries so an empty `DataCombined$new()` is still
#' created downstream.
#'
#' @param selected Named list of DataCombined specs (subset of
#'   `project$plots$dataCombined`).
#' @param simulatedScenarios List of simulated scenarios as created by
#'   `runScenarios()`.
#' @param observedData Named list of observed `DataSet` objects.
#' @param stopIfNotFound If `TRUE`, throw an error if a referenced scenario
#'   or dataSet is not found; otherwise warn and drop the offending entry.
#'
#' @returns The (possibly filtered) named list.
#' @keywords internal
.validateDataCombined <- function(
  selected,
  simulatedScenarios,
  observedData,
  stopIfNotFound
) {
  # Required-field checks per entry
  for (name in names(selected)) {
    spec <- selected[[name]]
    for (sim in spec$simulated %||% list()) {
      if (is.null(sim$label) || is.na(sim$label)) stop(messages$missingLabel())
      if (is.null(sim$scenario) || is.na(sim$scenario)) {
        stop(messages$missingScenarioName())
      }
      if (is.null(sim$path) || is.na(sim$path)) {
        stop(messages$stopNoPathProvided(name))
      }
    }
    for (obs in spec$observed %||% list()) {
      if (is.null(obs$label) || is.na(obs$label)) stop(messages$missingLabel())
      if (is.null(obs$dataSet) || is.na(obs$dataSet)) {
        stop(messages$stopNoDataSetProvided(name))
      }
    }
  }

  # Cross-check scenario references against the run pool
  allScenarios <- unlist(lapply(selected, function(spec) {
    vapply(
      spec$simulated %||% list(),
      function(e) e$scenario %||% NA_character_,
      character(1)
    )
  }))
  missingScenarios <- setdiff(
    setdiff(allScenarios, names(simulatedScenarios)),
    NA
  )
  if (length(missingScenarios) > 0) {
    if (stopIfNotFound) {
      stop(messages$warningInvalidScenarioName(missingScenarios))
    }
    warning(messages$warningInvalidScenarioName(missingScenarios))
    selected <- lapply(selected, function(spec) {
      spec$simulated <- Filter(
        function(e) !((e$scenario %||% NA) %in% missingScenarios),
        spec$simulated %||% list()
      )
      spec
    })
  }

  # Cross-check dataSet references against the loaded observedData
  allDataSets <- unlist(lapply(selected, function(spec) {
    vapply(
      spec$observed %||% list(),
      function(e) e$dataSet %||% NA_character_,
      character(1)
    )
  }))
  missingDataSets <- setdiff(
    setdiff(allDataSets, names(observedData)),
    NA
  )
  if (length(missingDataSets) > 0) {
    if (stopIfNotFound) {
      stop(messages$stopInvalidDataSetName(missingDataSets))
    }
    warning(messages$warningInvalidDataSetName(missingDataSets))
    selected <- lapply(selected, function(spec) {
      spec$observed <- Filter(
        function(e) !((e$dataSet %||% NA) %in% missingDataSets),
        spec$observed %||% list()
      )
      spec
    })
  }

  selected
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
