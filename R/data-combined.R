#' Generate DataCombined objects from a Project
#'
#' @description
#' Builds [`ospsuite::DataCombined`] objects from a JSON-driven
#' [Project][loadProject()]. The project's `dataCombined` section declares the
#' simulated/observed entries; `loadObservedData(project)` resolves observed
#' sources internally. Either `dataCombined` or `plotGrids` (or both)
#' selects which DataCombined to build.
#'
#' A simulated entry's `path` may be either a literal model quantity path or an
#' output-path id (a key of the project's `outputPaths` definitions). An id is
#' resolved to its literal path before the entry is built; any value that is not
#' a known id is used as a literal path.
#'
#' @param project A `Project` (see [loadProject()]).
#' @param dataCombined Names of the DataCombined entries to build. If
#'   any name is not declared in `dataCombined` definitions, an error is
#'   thrown.
#' @param plotGrids Names of plot grids whose DataCombined dependencies
#'   should be built. Combined with `dataCombined` if both are given.
#' @param scenarioResults A named list of Scenario Results (as
#'   returned by [runScenarios()]). Not the OSPS `SimulationResults`.
#' @param stopIfNotFound If `TRUE` (default), the function errors when a
#'   referenced simulated path or observed dataSet cannot be resolved. If
#'   `FALSE`, a warning is emitted and the entry is skipped.
#' @param validate If `TRUE` (default), the `dataCombined` section is validated
#'   before any DataCombined is built, so a definition missing a required field
#'   aborts with a clear message instead of failing mid-build.
#'
#' @returns A named list of `DataCombined` objects, one per requested name.
#'   Empty list when no names are requested.
#'
#' @export
createDataCombined <- function(
  project,
  dataCombined = NULL,
  plotGrids = NULL,
  scenarioResults = NULL,
  stopIfNotFound = TRUE,
  validate = TRUE
) {
  validateIsOfType(project, "Project")
  validateIsString(dataCombined, nullAllowed = TRUE)
  validateIsString(plotGrids, nullAllowed = TRUE)

  if (is.null(dataCombined) && is.null(plotGrids)) {
    return(list())
  }

  # Pre-flight the dataCombined shape so a hand-edited definition file with a
  # missing required field (`label` / `scenario` / `path` / `dataSet`) aborts
  # with a clean message here, rather than crashing mid-build on a NULL field.
  if (isTRUE(validate)) {
    project$ensureValid(
      sections = "dataCombined",
      opName = "createDataCombined"
    )
  }

  observedData <- loadObservedData(project)

  if (!is.null(plotGrids)) {
    allGridIds <- names(
      .unwrapDefinitionList(project$definitions$plotGrids) %||% list()
    )
    missingGrids <- setdiff(plotGrids[!is.na(plotGrids)], allGridIds)
    if (length(missingGrids) > 0) {
      cli::cli_abort(messages$plotGridNamesNotFound(missingGrids))
    }
    dataCombined <- union(
      dataCombined,
      .extractDataCombinedNamesForPlotsFromProject(project, plotGrids)
    )
  }

  allSpecs <- .unwrapDefinitionList(project$definitions$dataCombined) %||%
    list()
  missingNames <- setdiff(
    dataCombined[!is.na(dataCombined)],
    names(allSpecs)
  )
  if (length(missingNames) > 0) {
    cli::cli_abort(messages$dataCombinedNamesNotFound(missingNames))
  }

  # An output-path id used in a simulated entry's `path` resolves to its literal
  # model path via this map; a `path` that is not a known id is a literal path.
  outputPaths <- .unwrapDefinitionList(project$definitions$outputPaths) %||%
    list()

  selectedSpecs <- allSpecs[intersect(names(allSpecs), dataCombined)]

  # Capture this frame before the loop so a build-time abort attributes to
  # `createDataCombined()`, not the anonymous `lapply()` closure.
  call <- rlang::current_env()
  dataCombinedList <- lapply(names(selectedSpecs), function(name) {
    .buildDataCombined(
      name = name,
      spec = selectedSpecs[[name]],
      outputPaths = outputPaths,
      scenarioResults = scenarioResults,
      observedData = observedData,
      stopIfNotFound = stopIfNotFound,
      call = call
    )
  })
  names(dataCombinedList) <- names(selectedSpecs)
  dataCombinedList
}

# Find DataCombined names referenced by the requested plot grids.
#
# @keywords internal
# @noRd
.extractDataCombinedNamesForPlotsFromProject <- function(
  project,
  plotGrids
) {
  grids <- .unwrapDefinitionList(project$definitions$plotGrids) %||% list()
  selectedGrids <- grids[intersect(names(grids), plotGrids)]
  if (length(selectedGrids) == 0) {
    return(character(0))
  }
  ids <- unique(unlist(lapply(
    selectedGrids,
    function(g) .splitPlotIDs(g$plotIds)
  )))
  plotConfig <- .unwrapDefinitionList(project$definitions$plots) %||% list()
  referenced <- plotConfig[intersect(names(plotConfig), ids)]
  if (length(referenced) == 0) {
    return(character(0))
  }
  unique(unlist(lapply(referenced, function(p) p$dataCombinedId)))
}

#' @rdname createDataCombined
#' @param ... Passed on to [createDataCombined()].
#' @export
createDataCombinedFromExcel <- function(...) {
  lifecycle::deprecate_soft(
    what = "createDataCombinedFromExcel()",
    with = "createDataCombined()",
    when = "6.0.0"
  )
  createDataCombined(...)
}

# Build a single DataCombined from one JSON spec (its nested `simulated` /
# `observed` entry lists). An empty spec (no entries) yields an empty
# DataCombined carrying just the name. Runtime resolution against
# `scenarioResults` / `observedData` happens here, so it can only run once the
# scenarios have been run; the static shape of the spec is checked at load time
# by `.validateDataCombined()`.
#
# @keywords internal
# @noRd
.buildDataCombined <- function(
  name,
  spec,
  outputPaths,
  scenarioResults,
  observedData,
  stopIfNotFound,
  call = rlang::caller_env()
) {
  dataCombined <- DataCombined$new()

  # Collects entries whose simulated result could not be resolved (only reachable
  # when `stopIfNotFound = FALSE`). Their labels are dropped from the transform
  # step below, which would otherwise operate on a label that was never added.
  skippedLabels <- character(0)

  for (entry in spec$simulated %||% list()) {
    scenarioName <- entry$scenario
    scenarioResult <- scenarioResults[[scenarioName]]
    results <- scenarioResult$results
    # Resolve an output-path id to its literal path; a value that is not a known
    # id is used verbatim as a literal model path.
    path <- outputPaths[[entry$path]] %||% entry$path

    if (!is.null(results) && any(results$allQuantityPaths == path)) {
      dataCombined$addSimulationResults(
        simulationResults = results,
        quantitiesOrPaths = path,
        groups = entry$group %||% NA_character_,
        names = entry$label
      )
    } else {
      # Three distinct reasons the simulated data can't be resolved, each with
      # its own message: the scenario is absent from `scenarioResults`
      # (typo, or not part of the run); it is present but its run produced no
      # results; or the run is fine but the output path was not simulated.
      msg <- if (is.null(scenarioResult)) {
        messages$scenarioNotInResults(
          dataCombinedName = name,
          scenarioName = scenarioName
        )
      } else if (is.null(results)) {
        messages$scenarioRunFailed(
          dataCombinedName = name,
          scenarioName = scenarioName,
          path = path
        )
      } else {
        messages$wrongOutputPath(
          dataCombinedName = name,
          scenarioName = scenarioName,
          path = path
        )
      }
      if (stopIfNotFound) {
        cli::cli_abort(msg, call = call)
      }
      cli::cli_warn(msg)
      skippedLabels <- c(skippedLabels, entry$label)
    }
  }

  observedEntries <- spec$observed %||% list()
  if (length(observedEntries) > 0) {
    dataSetIds <- vapply(observedEntries, function(e) e$dataSet, character(1))
    missingDataSets <- setdiff(dataSetIds, names(observedData))
    if (length(missingDataSets) > 0) {
      if (stopIfNotFound) {
        cli::cli_abort(
          messages$invalidDataSetName(missingDataSets),
          call = call
        )
      }
      cli::cli_warn(messages$combineInvalidDataSetName(missingDataSets))
      keep <- !(dataSetIds %in% missingDataSets)
      # Record the dropped labels so the transform step below skips them too:
      # their data was never added, so a transform would run against an absent
      # (all-NA) row.
      skippedLabels <- c(
        skippedLabels,
        vapply(observedEntries[!keep], function(e) e$label, character(1))
      )
      observedEntries <- observedEntries[keep]
      dataSetIds <- dataSetIds[keep]
    }
    if (length(observedEntries) > 0) {
      dataCombined$addDataSets(
        observedData[dataSetIds],
        names = vapply(observedEntries, function(e) e$label, character(1)),
        groups = vapply(
          observedEntries,
          function(e) e$group %||% NA_character_,
          character(1)
        )
      )
    }
  }

  .applyDataCombinedTransformations(
    dataCombined,
    name = name,
    entries = c(spec$simulated %||% list(), spec$observed %||% list()),
    skippedLabels = skippedLabels
  )

  dataCombined
}

# Apply the per-entry x/y offsets and scale factors declared on a DataCombined
# spec. Only entries carrying at least one transform are touched, and any label
# that was skipped during the build (its data was never added) is left out so
# the unit conversion never runs against an absent row.
#
# @keywords internal
# @noRd
.applyDataCombinedTransformations <- function(
  dataCombined,
  name,
  entries,
  skippedLabels
) {
  df <- dataCombined$toDataFrame()
  for (entry in entries) {
    if (entry$label %in% skippedLabels) {
      next
    }
    hasTransform <- !is.null(entry$xOffsets) ||
      !is.null(entry$yOffsets) ||
      !is.null(entry$xScaleFactors) ||
      !is.null(entry$yScaleFactors)
    if (!hasTransform) {
      next
    }

    if (
      (!is.null(entry$xOffsets) && is.null(entry$xOffsetsUnits)) ||
        (!is.null(entry$yOffsets) && is.null(entry$yOffsetsUnits))
    ) {
      cli::cli_abort(messages$offsetUnitsNotDefined(name))
    }

    singleRow <- df[df$name == entry$label, ][1, ]

    xOffset <- NA_real_
    if (!is.null(entry$xOffsets)) {
      xTargetUnit <- singleRow$xUnit
      if (is.na(xTargetUnit)) {
        xTargetUnit <- ""
      }
      xOffset <- toUnit(
        quantityOrDimension = singleRow$xDimension,
        values = as.numeric(entry$xOffsets),
        targetUnit = xTargetUnit,
        sourceUnit = entry$xOffsetsUnits
      )
    }

    yOffset <- NA_real_
    if (!is.null(entry$yOffsets)) {
      yTargetUnit <- singleRow$yUnit
      if (is.na(yTargetUnit)) {
        yTargetUnit <- ""
      }
      yOffset <- toUnit(
        quantityOrDimension = singleRow$yDimension,
        values = as.numeric(entry$yOffsets),
        targetUnit = yTargetUnit,
        sourceUnit = entry$yOffsetsUnits,
        molWeight = singleRow$molWeight,
        molWeightUnit = ospUnits$`Molecular weight`$`g/mol`
      )
    }

    dataCombined$setDataTransformations(
      forNames = entry$label,
      xOffsets = xOffset,
      yOffsets = yOffset,
      xScaleFactors = as.numeric(entry$xScaleFactors %||% NA_real_),
      yScaleFactors = as.numeric(entry$yScaleFactors %||% NA_real_)
    )
  }
}

# Section validation adapter ----

#' @keywords internal
#' @noRd
.dataCombinedValidatorAdapter <- function(project) {
  .validateDataCombined(
    .unwrapDefinitionList(project$definitions$dataCombined)
  )
}

#' Validate the `dataCombined` section of a Project
#'
#' Static, spec-only checks on the JSON `dataCombined` section: every simulated
#' entry must declare `label`, `scenario`, and `path`; every observed entry must
#' declare `label` and `dataSet`. An empty section is valid. Cross-section
#' reference resolution (a simulated entry's `scenario` against defined
#' scenarios) is handled by the `crossReferences` phase, not here; runtime
#' resolution against scenario results and observed data happens in
#' [createDataCombined()].
#'
#' @param dataCombined Named list of DataCombined specs (from `dataCombined`
#'   definitions), each with nested `simulated` / `observed` entry lists.
#' @returns validationResult.
#' @keywords internal
#' @noRd
.validateDataCombined <- function(dataCombined) {
  result <- validationResult$new()

  if (is.null(dataCombined) || length(dataCombined) == 0) {
    result$addWarning("Data", "No dataCombined defined")
    return(result)
  }

  result <- .checkNoDuplicates(names(dataCombined), "dataCombinedId", result)

  for (id in names(dataCombined)) {
    dc <- dataCombined[[id]]
    for (entry in dc$simulated %||% list()) {
      .checkDataCombinedEntryFields(entry, "simulated", id, result)
    }
    for (entry in dc$observed %||% list()) {
      .checkDataCombinedEntryFields(entry, "observed", id, result)
    }
  }

  result
}

# Record a critical error on `result` for every required field an entry is
# missing. `.isMissingField()` (shared with the write-time gate
# `.checkDataCombinedEntry()`) defines "missing" identically at load time.
#
# @keywords internal
# @noRd
.checkDataCombinedEntryFields <- function(entry, dataType, id, result) {
  required <- .requiredDataCombinedFields(dataType)
  for (field in required) {
    if (.isMissingField(entry[[field]])) {
      result$addCriticalError(
        "Missing Fields",
        paste0(
          "DataCombined '",
          id,
          "' has a ",
          dataType,
          " entry missing required field: ",
          field
        )
      )
    }
  }
  result
}
