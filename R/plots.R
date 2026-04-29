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

.dataCombinedTransformCols <- c(
  "xOffsets",
  "xOffsetsUnits",
  "yOffsets",
  "yOffsetsUnits",
  "xScaleFactors",
  "yScaleFactors"
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

#' Parse nested dataCombined JSON to flat data.frame
#' @param nestedData List of dataCombined objects with simulated/observed arrays
#' @returns data.frame with DataCombinedName, dataType, and all entry fields
#' @keywords internal
#' @noRd
.parseNestedDataCombined <- function(nestedData) {
  if (is.null(nestedData) || length(nestedData) == 0) {
    return(data.frame())
  }

  transformCols <- c(
    "xOffsets",
    "xOffsetsUnits",
    "yOffsets",
    "yOffsetsUnits",
    "xScaleFactors",
    "yScaleFactors"
  )

  rows <- list()
  for (dataCombined in nestedData) {
    dataCombinedName <- dataCombined$name

    # Process simulated entries
    if (
      !is.null(dataCombined$simulated) && length(dataCombined$simulated) > 0
    ) {
      for (entry in dataCombined$simulated) {
        row <- list(
          DataCombinedName = dataCombinedName,
          dataType = "simulated",
          label = entry$label,
          scenario = entry$scenario,
          path = entry$path,
          dataSet = NA,
          group = entry$group %||% NA
        )
        for (col in transformCols) {
          row[[col]] <- entry[[col]] %||% NA
        }
        rows[[length(rows) + 1]] <- as.data.frame(row, stringsAsFactors = FALSE)
      }
    }

    # Process observed entries
    if (!is.null(dataCombined$observed) && length(dataCombined$observed) > 0) {
      for (entry in dataCombined$observed) {
        row <- list(
          DataCombinedName = dataCombinedName,
          dataType = "observed",
          label = entry$label,
          scenario = NA,
          path = NA,
          dataSet = entry$dataSet,
          group = entry$group %||% NA
        )
        for (col in transformCols) {
          row[[col]] <- entry[[col]] %||% NA
        }
        rows[[length(rows) + 1]] <- as.data.frame(row, stringsAsFactors = FALSE)
      }
    }
  }

  if (length(rows) == 0) {
    return(data.frame())
  }

  do.call(rbind, rows)
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
  if (is.null(dataCombined) || nrow(dataCombined) == 0) {
    result$add_warning("Data", "dataCombined is empty")
  } else {
    for (col in c("DataCombinedName", "dataType")) {
      if (!col %in% names(dataCombined)) {
        result$add_critical_error(
          "Missing Fields",
          paste0("dataCombined is missing required column '", col, "'")
        )
      }
    }

    if ("dataType" %in% names(dataCombined)) {
      invalid_types <- dataCombined$dataType[
        !is.na(dataCombined$dataType) &
          !dataCombined$dataType %in% c("simulated", "observed")
      ]
      if (length(invalid_types) > 0) {
        result$add_critical_error(
          "Validation",
          paste0(
            "Invalid dataType values in dataCombined: ",
            paste(unique(invalid_types), collapse = ", ")
          )
        )
      }

      simulated_rows <- dataCombined[
        !is.na(dataCombined$dataType) & dataCombined$dataType == "simulated",
      ]
      if (nrow(simulated_rows) > 0 && "scenario" %in% names(dataCombined)) {
        missing_scenario <- is.na(simulated_rows$scenario) |
          simulated_rows$scenario == ""
        if (any(missing_scenario)) {
          result$add_critical_error(
            "Missing Fields",
            "Some simulated rows in dataCombined are missing 'scenario'"
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
        nrow(dataCombined) > 0 &&
        "DataCombinedName" %in% names(plotConfig) &&
        "DataCombinedName" %in% names(dataCombined)
    ) {
      invalid_dataCombined_refs <- setdiff(
        plotConfig$DataCombinedName[!is.na(plotConfig$DataCombinedName)],
        dataCombined$DataCombinedName
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

#' Convert flat dataCombined data.frame to nested JSON structure
#' @param df data.frame with DataCombinedName, dataType, and entry fields
#' @returns List of dataCombined objects with simulated/observed arrays
#' @keywords internal
#' @noRd
.dataCombinedToNestedJson <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list())
  }

  transformCols <- c(
    "xOffsets",
    "xOffsetsUnits",
    "yOffsets",
    "yOffsetsUnits",
    "xScaleFactors",
    "yScaleFactors"
  )

  dataCombinedNames <- unique(df$DataCombinedName)

  lapply(dataCombinedNames, function(dataCombinedName) {
    dataCombinedRows <- df[
      df$DataCombinedName == dataCombinedName,
      ,
      drop = FALSE
    ]

    simRows <- dataCombinedRows[
      dataCombinedRows$dataType == "simulated",
      ,
      drop = FALSE
    ]
    obsRows <- dataCombinedRows[
      dataCombinedRows$dataType == "observed",
      ,
      drop = FALSE
    ]

    simulated <- lapply(seq_len(nrow(simRows)), function(i) {
      row <- simRows[i, ]
      entry <- list(
        label = row$label,
        scenario = row$scenario,
        path = row$path,
        group = if (is.na(row$group)) NULL else row$group
      )
      for (col in transformCols) {
        entry[[col]] <- if (is.na(row[[col]])) NULL else row[[col]]
      }
      entry
    })

    observed <- lapply(seq_len(nrow(obsRows)), function(i) {
      row <- obsRows[i, ]
      entry <- list(
        label = row$label,
        dataSet = row$dataSet,
        group = if (is.na(row$group)) NULL else row$group
      )
      for (col in transformCols) {
        entry[[col]] <- if (is.na(row[[col]])) NULL else row[[col]]
      }
      entry
    })

    list(
      name = dataCombinedName,
      simulated = simulated,
      observed = observed
    )
  })
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
