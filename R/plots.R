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

# Parse ----
#
# Parse the `plots` JSON section into the asymmetric in-memory shape:
#   * dataCombined   : named list keyed by name (drops the redundant `name`
#                      field on each entry).
#   * plotConfiguration / plotGrids : data.frames, NA-padded across rows.
# Returns NULL when the JSON omits the `plots` section.
#
# @keywords internal
# @noRd
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

# Drop the redundant `name` field (it becomes the list key) and re-key the
# list by name. Per-entry sub-lists (`simulated`, `observed`) pass through
# verbatim so adding optional fields at the JSON level does not require a
# code change here.
#
# @keywords internal
# @noRd
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

# Convert a list of named lists (a JSON array of objects) to a data.frame,
# padding missing fields across rows with NA.
#
# @keywords internal
# @noRd
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

# Serialize ----

# JSON object with `dataCombined` / `plotConfiguration` / `plotGrids` arrays;
# `null` when the project carries no plots section.
.plotsToJson <- function(project) {
  plots <- project$plots
  if (is.null(plots)) {
    return(NULL)
  }
  list(
    dataCombined = .dataCombinedToNestedJson(plots$dataCombined),
    plotConfiguration = .dataFrameToListOfLists(plots$plotConfiguration),
    plotGrids = .dataFrameToListOfLists(plots$plotGrids)
  )
}

# Inverts .parseNestedDataCombined: re-adds the `name` field from the list
# key. Empty `simulated`/`observed` lists are omitted to keep the JSON terse.
#
# @keywords internal
# @noRd
.dataCombinedToNestedJson <- function(dataCombined) {
  if (is.null(dataCombined) || length(dataCombined) == 0) {
    return(list())
  }
  unname(lapply(names(dataCombined), function(name) {
    dc <- dataCombined[[name]]
    entry <- list(name = name)
    if (length(dc$simulated) > 0) {
      entry$simulated <- dc$simulated
    }
    if (length(dc$observed) > 0) {
      entry$observed <- dc$observed
    }
    entry
  }))
}

# Convert a data.frame back to a list of named lists. NA cells are dropped
# per row so they round-trip to JSON `null`/absent.
#
# @keywords internal
# @noRd
.dataFrameToListOfLists <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list())
  }
  lapply(seq_len(nrow(df)), function(i) {
    row <- as.list(df[i, , drop = FALSE])
    Filter(function(x) !(length(x) == 1 && is.na(x)), row)
  })
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

  for (e in simulated) {
    .checkDataCombinedEntry(e, "simulated")
  }
  for (e in observed) {
    .checkDataCombinedEntry(e, "observed")
  }

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
