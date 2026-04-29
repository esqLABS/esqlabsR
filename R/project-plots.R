# Public CRUD: plots, plot grids, data combined ----

.validPlotTypes <- c(
  "individual",
  "population",
  "observedVsSimulated",
  "residualsVsSimulated",
  "residualsVsTime"
)

.splitPlotIDs <- function(plotIdsStr) {
  if (is.null(plotIdsStr) || is.na(plotIdsStr) || !nzchar(plotIdsStr)) {
    return(character())
  }
  trimws(unlist(strsplit(as.character(plotIdsStr), ",", fixed = TRUE)))
}

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

  if (!(dataCombinedName %in% project$plots$dataCombined$DataCombinedName)) {
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
