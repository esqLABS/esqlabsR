# Plots section: parse + validate + serialize + mutation.
#
# Owns Project$plots end-to-end. Project$plots is a list with three
# data.frames: dataCombined, plotConfiguration, plotGrids. The flat
# in-memory data.frame shape contrasts with the nested JSON shape, so the
# parse/serialize step does a structural conversion in addition to type
# coercion. The plotting *engine* (createPlots() and its dispatchers) lives
# in R/create-plots.R and is independent of this file.
#
# Called by:
#   - Project$.read_json() via .parsePlots()
#   - .runProjectValidation() via .validatePlots()
#   - .projectToJson() via .plotsToJson()
#   - users via the public addPlot / removePlot / addPlotGrid /
#     removePlotGrid / addDataCombined / removeDataCombined functions.

# Constants ----

.validPlotTypes <- c(
  "individual",
  "population",
  "observedVsSimulated",
  "residualsVsSimulated",
  "residualsVsTime"
)

# Parse ----

#' @keywords internal
#' @noRd
.parsePlots <- function(plotsData) {
  if (is.null(plotsData)) {
    return(NULL)
  }
  list(
    dataCombined = .parseNestedDataCombined(plotsData$dataCombined),
    plotConfiguration = .listOfListsToDataFrame(plotsData$plotConfiguration),
    plotGrids = .listOfListsToDataFrame(plotsData$plotGrids)
  )
}

#' Parse the dataCombined section of a Project.json
#'
#' Drops the redundant `name` field from each entry (it is the list key)
#' and re-keys the list by name. The simulated/observed lists are passed
#' through unchanged; per-entry fields are not enumerated, so adding a
#' new optional field at the JSON level does not require a code change
#' here. NULL becomes NULL (kept absent), not NA.
#'
#' @param nestedData List of dataCombined objects with `name`,
#'   `simulated`, and `observed` fields.
#' @returns Named list keyed by DataCombined name, each entry a list with
#'   `simulated` and `observed` (each itself a list of entries).
#' @keywords internal
#' @noRd
.parseNestedDataCombined <- function(nestedData) {
  if (is.null(nestedData) || length(nestedData) == 0) {
    return(list())
  }

  result <- list()
  for (dc in nestedData) {
    result[[dc$name]] <- list(
      simulated = dc$simulated %||% list(),
      observed = dc$observed %||% list()
    )
  }
  result
}

#' Convert a list of named lists to a data.frame, padding missing fields with NA
#' @keywords internal
#' @noRd
.listOfListsToDataFrame <- function(data) {
  if (is.null(data) || length(data) == 0) {
    return(data.frame())
  }
  allCols <- unique(unlist(lapply(data, names)))
  rows <- lapply(data, function(entry) {
    row <- lapply(allCols, function(col) {
      val <- entry[[col]]
      if (is.null(val)) NA else val
    })
    names(row) <- allCols
    as.data.frame(row, stringsAsFactors = FALSE)
  })
  as.data.frame(dplyr::bind_rows(rows))
}

# Validate ----

#' Validate plots section of a Project
#' @param plots List with dataCombined, plotConfiguration, plotGrids
#'   data.frames from project$plots
#' @return validationResult object
#' @keywords internal
.validatePlots <- function(plots) {
  result <- validationResult$new()

  if (is.null(plots)) {
    result$add_warning("Data", "No plots defined")
    return(result)
  }

  dataCombined <- plots$dataCombined
  plotConfig <- plots$plotConfiguration

  # Validate dataCombined
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

  # Validate plotConfiguration
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

    # Inner cross-ref: plotConfiguration -> dataCombined
    if (
      !is.null(dataCombined) &&
        length(dataCombined) > 0 &&
        "DataCombinedName" %in% names(plotConfig)
    ) {
      invalid_dataCombined_refs <- setdiff(
        plotConfig$DataCombinedName[!is.na(plotConfig$DataCombinedName)],
        names(dataCombined)
      )
      if (length(invalid_dataCombined_refs) > 0) {
        result$add_critical_error(
          "Invalid Reference",
          paste0(
            "plotConfiguration references unknown DataCombinedName: ",
            paste(invalid_dataCombined_refs, collapse = ", ")
          )
        )
      }
    }
  }

  # plotGrids -> plotConfiguration inner cross-ref (warning only)
  plotGrids <- plots$plotGrids
  if (
    !is.null(plotGrids) &&
      nrow(plotGrids) > 0 &&
      !is.null(plotConfig) &&
      nrow(plotConfig) > 0
  ) {
    if ("plotIDs" %in% names(plotGrids) && "plotID" %in% names(plotConfig)) {
      all_grid_ids <- unlist(lapply(
        plotGrids$plotIDs[!is.na(plotGrids$plotIDs)],
        function(x) trimws(strsplit(x, ",")[[1]])
      ))
      invalid_grid_refs <- setdiff(all_grid_ids, plotConfig$plotID)
      if (length(invalid_grid_refs) > 0) {
        result$add_warning(
          "Invalid Reference",
          paste0(
            "plotGrids references unknown plotIDs: ",
            paste(invalid_grid_refs, collapse = ", ")
          )
        )
      }
    }
  }

  result
}

#' @keywords internal
#' @noRd
.plotsValidatorAdapter <- function(project) {
  .validatePlots(project$plots)
}

# Serialize ----

#' @keywords internal
#' @noRd
.plotsToJson <- function(plots) {
  if (is.null(plots)) {
    return(list(
      dataCombined = list(),
      plotConfiguration = list(),
      plotGrids = list()
    ))
  }

  list(
    dataCombined = .dataCombinedToNestedJson(plots$dataCombined),
    plotConfiguration = .dataFrameToListOfLists(plots$plotConfiguration),
    plotGrids = .dataFrameToListOfLists(plots$plotGrids)
  )
}

#' Serialize the dataCombined section to JSON-shaped nested list
#'
#' Inverts `.parseNestedDataCombined()`: re-adds the `name` field from the
#' list key. The per-entry shape is passed through unchanged.
#'
#' @param dataCombined Named list as produced by `.parseNestedDataCombined()`.
#' @returns List of dataCombined objects with `name`, `simulated`,
#'   `observed`. Empty `simulated`/`observed` lists are omitted to keep
#'   the JSON terse.
#' @keywords internal
#' @noRd
.dataCombinedToNestedJson <- function(dataCombined) {
  if (is.null(dataCombined) || length(dataCombined) == 0) {
    return(list())
  }
  unname(lapply(names(dataCombined), function(name) {
    dc <- dataCombined[[name]]
    entry <- list(name = name)
    if (length(dc$simulated) > 0) entry$simulated <- dc$simulated
    if (length(dc$observed) > 0) entry$observed <- dc$observed
    entry
  }))
}

#' @keywords internal
#' @noRd
.dataFrameToListOfLists <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list())
  }

  lapply(seq_len(nrow(df)), function(i) {
    row <- as.list(df[i, , drop = FALSE])
    row <- lapply(row, function(x) if (is.na(x)) NULL else x)
    row
  })
}

# CRUD helpers ----

.splitPlotIDs <- function(plotIdsStr) {
  if (is.null(plotIdsStr) || is.na(plotIdsStr) || !nzchar(plotIdsStr)) {
    return(character())
  }
  trimws(unlist(strsplit(as.character(plotIdsStr), ",", fixed = TRUE)))
}

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

.checkDataCombinedEntry <- function(entry, dataType) {
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
  invisible(TRUE)
}

# Public CRUD: plots ----

#' Add a plot configuration to a Project
#'
#' @description Append a new row to `project$plots$plotConfiguration`.
#' Errors if `plotID` already exists, if `dataCombinedName` is not present
#' in `project$plots$dataCombined`, or if `plotType` is not one of the
#' supported types.
#'
#' @param project A `Project` object.
#' @param plotID Character scalar. Unique plot identifier.
#' @param dataCombinedName Character scalar. Must reference an existing
#'   DataCombined name on the project. Stored in the `DataCombinedName`
#'   column to match the JSON schema.
#' @param plotType Character scalar. One of `"individual"`, `"population"`,
#'   `"observedVsSimulated"`, `"residualsVsSimulated"`, `"residualsVsTime"`.
#' @param ... Optional plot-configuration fields, e.g. `title`, `subtitle`,
#'   `xUnit`, `yUnit`, `xAxisScale`, `yAxisScale`, `xValuesLimits`,
#'   `yValuesLimits`, `aggregation`, `quantiles`, `nsd`, `foldDistance`.
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
    stop(messages$plotIDExists(plotID))
  }

  if (!(dataCombinedName %in% names(project$plots$dataCombined))) {
    stop(messages$plotDataCombinedNameNotFound(dataCombinedName))
  }

  if (!(plotType %in% .validPlotTypes)) {
    stop(messages$invalidPlotType(plotType, .validPlotTypes))
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
#' @description Drop the row with matching `plotID`. Warns (no-op) if
#' `plotID` is not found, and warns when the plot is referenced by any
#' `plotGrids` entry.
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
    cli::cli_warn(messages$plotIDNotFound(plotID))
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
      cli::cli_warn(messages$plotReferencedByGrid(plotID, referencingGrids))
    }
  }

  project$plots$plotConfiguration <- df[df$plotID != plotID, , drop = FALSE]
  project$.markModified()
  invisible(project)
}

# Public CRUD: plot grids ----

#' Add a plot grid to a Project
#'
#' @description Append a new row to `project$plots$plotGrids`. Errors if
#' `name` already exists or if any of the supplied `plotIDs` are not
#' present in `project$plots$plotConfiguration`.
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
      length(plotIDs) == 0 ||
      any(is.na(plotIDs)) ||
      any(nchar(plotIDs) == 0)
  ) {
    stop("plotIDs must be a non-empty character vector")
  }

  existingGrids <- project$plots$plotGrids
  if (
    !is.null(existingGrids) &&
      nrow(existingGrids) > 0 &&
      name %in% existingGrids$name
  ) {
    stop(messages$plotGridNameExists(name))
  }

  unknown <- setdiff(plotIDs, project$plots$plotConfiguration$plotID)
  if (length(unknown) > 0) {
    stop(messages$plotGridUnknownPlotIDs(unknown))
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
#' @description Drop the row with matching `name`. Warns (no-op) if
#' `name` is not present.
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
    cli::cli_warn(messages$plotGridNotFound(name))
    return(invisible(project))
  }

  project$plots$plotGrids <- df[df$name != name, , drop = FALSE]
  project$.markModified()
  invisible(project)
}

# Public CRUD: dataCombined ----

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

  if (name %in% names(project$plots$dataCombined)) {
    stop(messages$dataCombinedNameExists(name))
  }

  if (length(simulated) == 0 && length(observed) == 0) {
    stop("addDataCombined requires at least one simulated or observed entry")
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

  if (!(name %in% names(project$plots$dataCombined))) {
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

  project$plots$dataCombined[[name]] <- NULL
  project$.markModified()
  invisible(project)
}
