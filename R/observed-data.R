# Observed data: project-driven loader dispatch + section CRUD.
#
# Owns Project$observedData end-to-end. Called by:
#   - users via loadObservedData(project) — the public dispatcher.
#   - createDataCombined() internally to resolve observed sources.
#   - validateProject() via .observedDataValidatorAdapter()
#   - users via the public addObservedData / removeObservedData
#     functions.
#
# The four declared source types in v2.0 Project.json are:
#   - excel  : ospsuite::loadDataSetsFromExcel via importer config
#   - pkml   : ospsuite::loadDataSetFromPKML
#   - script : R script sourced; must return DataSet or list of DataSets
#   - programmatic : DataSets added at runtime via addObservedData(project).
#                    The DataSet itself is not JSON-serializable, so it
#                    is held on the Project's private slot
#                    (.programmaticDataSets) and the JSON sentinel
#                    `{type: "programmatic", name: ...}` is what
#                    survives a round-trip.

# Reach into Project's R6 private slot. R does not enforce R6 privacy
# at runtime; this helper stays narrow to the observed-data module so
# the rest of the codebase does not pick up the pattern.
.projectPrivate <- function(project) {
  project$.__enclos_env__$private
}

# Section validation adapter ----

#' @keywords internal
#' @noRd
.observedDataValidatorAdapter <- function(project) {
  .validateObservedData(project$observedData, project$dataFolder)
}

#' Validate the `observedData` section of a Project
#'
#' Per-entry checks: `type` is set and is one of `excel`, `pkml`,
#' `script`, `programmatic`; per-type required fields are present;
#' referenced files exist on disk (warnings only — missing files do
#' not block parsing).
#'
#' @param observedData List from `project$observedData`.
#' @param dataFolder Resolved absolute path to the project's data
#'   folder, or `NULL` when unset.
#' @return validationResult.
#' @keywords internal
#' @noRd
.validateObservedData <- function(observedData, dataFolder) {
  result <- validationResult$new()

  if (is.null(observedData) || length(observedData) == 0) {
    result$add_warning("Data", "No observedData defined")
    return(result)
  }

  validTypes <- c("excel", "pkml", "script", "programmatic")

  for (i in seq_along(observedData)) {
    entry <- observedData[[i]]
    entryLabel <- paste0("observedData entry ", i)

    if (is.null(entry$type)) {
      result$add_critical_error(
        "Missing Fields",
        paste0(entryLabel, " is missing required field 'type'")
      )
      next
    }

    if (!entry$type %in% validTypes) {
      result$add_critical_error(
        "Invalid Value",
        paste0(
          entryLabel,
          " has invalid type '",
          entry$type,
          "'. Must be one of: ",
          paste(validTypes, collapse = ", ")
        )
      )
      next
    }

    if (entry$type == "excel") {
      if (is.null(entry$file)) {
        result$add_critical_error(
          "Missing Fields",
          paste0(entryLabel, " (excel) is missing required field 'file'")
        )
      } else if (!is.null(dataFolder)) {
        filePath <- file.path(dataFolder, entry$file)
        if (!file.exists(filePath)) {
          result$add_warning(
            "File Not Found",
            paste0(entryLabel, " references non-existent file: ", entry$file)
          )
        }
      }

      if (is.null(entry$importerConfiguration)) {
        result$add_critical_error(
          "Missing Fields",
          paste0(
            entryLabel,
            " (excel) is missing required field 'importerConfiguration'"
          )
        )
      } else if (!is.null(dataFolder)) {
        importerPath <- file.path(dataFolder, entry$importerConfiguration)
        if (!file.exists(importerPath)) {
          result$add_warning(
            "File Not Found",
            paste0(
              entryLabel,
              " references non-existent importer config: ",
              entry$importerConfiguration
            )
          )
        }
      }

      if (is.null(entry$sheets) || length(entry$sheets) == 0) {
        result$add_critical_error(
          "Missing Fields",
          paste0(entryLabel, " (excel) is missing required field 'sheets'")
        )
      }
    }

    if (entry$type %in% c("pkml", "script")) {
      if (is.null(entry$file)) {
        result$add_critical_error(
          "Missing Fields",
          paste0(
            entryLabel,
            " (",
            entry$type,
            ") is missing required field 'file'"
          )
        )
      } else if (!is.null(dataFolder)) {
        filePath <- file.path(dataFolder, entry$file)
        if (!file.exists(filePath)) {
          result$add_warning(
            "File Not Found",
            paste0(entryLabel, " references non-existent file: ", entry$file)
          )
        }
      }
    }
  }

  result
}

#' Load observed data declared in a Project
#'
#' @description
#' Reads the `observedData` declarations from a [Project][loadProject()]
#' and returns the corresponding [`ospsuite::DataSet`] objects. Source
#' types: `excel` (via importer configuration), `pkml`, `script`. The
#' `programmatic` type is reserved for a later milestone and currently
#' errors fast.
#'
#' @param project A `Project` object (see [loadProject()]).
#' @returns A named list of [`ospsuite::DataSet`] objects. Empty list when
#'   `project$observedData` is empty or `NULL`.
#' @examples
#' \dontrun{
#' project <- loadProject("path/to/Project.json")
#' dataSets <- loadObservedData(project)
#' }
#' @export
loadObservedData <- function(project) {
  validateIsOfType(project, "Project")
  if (is.null(project$observedData) || length(project$observedData) == 0) {
    return(list())
  }
  state <- .projectPrivate(project)
  allDataSets <- list()
  for (i in seq_along(project$observedData)) {
    entry <- project$observedData[[i]]
    .validateObservedDataEntry(entry, i)
    dataSets <- switch(
      entry$type,
      "excel" = .loadObservedExcel(entry, project$dataFolder),
      "pkml" = .loadObservedPkml(entry, project$dataFolder),
      "script" = .loadObservedScript(entry, project$dataFolder),
      "programmatic" = NULL
    )
    if (!is.null(dataSets)) {
      allDataSets <- c(allDataSets, dataSets)
    }
  }
  # Merge runtime programmatic store, then cache names.
  allDataSets <- c(allDataSets, state$.programmaticDataSets)
  state$.observedDataNamesCache <- names(allDataSets)
  allDataSets
}

#' Get names of all observed data in a Project
#'
#' Returns the names of all DataSets that would be returned by
#' [loadObservedData()]. On first call this loads the data to discover
#' names; subsequent calls return cached names until a mutation
#' invalidates the cache.
#'
#' @param project A `Project` object (see [loadProject()]).
#' @returns A character vector of DataSet names.
#' @export
#' @family observedData
getObservedDataNames <- function(project) {
  validateIsOfType(project, "Project")
  state <- .projectPrivate(project)
  if (!is.null(state$.observedDataNamesCache)) {
    return(state$.observedDataNamesCache)
  }
  loadObservedData(project)
  state$.observedDataNamesCache %||% character(0)
}

# Public CRUD: observedData ----

#' Add observed data to a Project
#'
#' Add an observedData entry. Accepts either a `DataSet` (creates a
#' `type = "programmatic"` entry keyed by `dataSet$name`) or a
#' configuration list with `type` field (`"excel"`, `"pkml"`, or
#' `"script"`) plus source-specific fields.
#'
#' @param project A `Project` object.
#' @param entry Either a `DataSet` object or a configuration list.
#' @returns The `project` object, invisibly.
#' @export
#' @family observedData
addObservedData <- function(project, entry) {
  validateIsOfType(project, "Project")
  state <- .projectPrivate(project)

  if (inherits(entry, "DataSet")) {
    name <- entry$name
    existingNames <- getObservedDataNames(project)
    if (name %in% existingNames) {
      cli::cli_abort(
        "observedData entry with name {.val {name}} already exists"
      )
    }
    state$.programmaticDataSets[[name]] <- entry
    sentinel <- list(type = "programmatic", name = name)
    # The observedData setter resets the names cache, so the cache must be
    # rebuilt after the write, from the names known before it.
    project$observedData <- c(project$observedData, list(sentinel))
    state$.observedDataNamesCache <- c(existingNames, name)
    project$.markModified()
    cli::cli_inform(c(
      "i" = paste0(
        "For reproducibility, consider declaring this DataSet via a ",
        "script in your Project.json using the {.field observedData} ",
        "field with {.code type = \"script\"} and ",
        "{.code file = \"scripts/your_script.R\"}."
      )
    ))
    return(invisible(project))
  }

  if (is.list(entry)) {
    if (is.null(entry$type)) {
      cli::cli_abort("observedData entry must include a {.field type} field")
    }
    validTypes <- c("excel", "pkml", "script")
    if (!(entry$type %in% validTypes)) {
      cli::cli_abort(c(
        "Invalid observedData entry type {.val {entry$type}}.",
        "i" = "Must be one of: {.val {validTypes}}."
      ))
    }
    state$.observedDataNamesCache <- NULL
    project$observedData <- c(project$observedData, list(entry))
    project$.markModified()
    return(invisible(project))
  }

  cli::cli_abort(
    "observedData entry must be a {.cls DataSet} or a configuration list"
  )
}

#' Remove observed data from a Project
#'
#' Removes by DataSet name (for `type = "programmatic"` entries) or by
#' `file` basename (for `type` `"excel"` / `"pkml"` / `"script"`
#' entries). Warns and is a no-op if no matching entry is found.
#'
#' @param project A `Project` object.
#' @param name DataSet name or config entry file basename.
#' @returns The `project` object, invisibly.
#' @export
#' @family observedData
removeObservedData <- function(project, name) {
  validateIsOfType(project, "Project")
  if (
    !is.character(name) ||
      length(name) != 1L ||
      is.na(name) ||
      nchar(name) == 0
  ) {
    cli::cli_abort("{.arg name} must be a non-empty string")
  }
  state <- .projectPrivate(project)

  if (name %in% names(state$.programmaticDataSets)) {
    state$.programmaticDataSets[[name]] <- NULL
    matchIdx <- which(vapply(
      project$observedData,
      function(e) {
        identical(e$type, "programmatic") && identical(e$name, name)
      },
      logical(1)
    ))
    if (length(matchIdx) > 0L) {
      project$observedData <- project$observedData[-matchIdx[[1]]]
    }
    state$.observedDataNamesCache <- NULL
    project$.markModified()
    return(invisible(project))
  }

  matchIdx <- which(vapply(
    project$observedData,
    function(e) {
      !is.null(e$file) && identical(basename(e$file), name)
    },
    logical(1)
  ))

  if (length(matchIdx) == 0L) {
    cli::cli_warn("observedData entry {.val {name}} not found; no-op.")
    return(invisible(project))
  }

  project$observedData <- project$observedData[-matchIdx[[1]]]
  state$.observedDataNamesCache <- NULL
  project$.markModified()
  invisible(project)
}

# Internal helpers ----

.validateObservedDataEntry <- function(entry, entryIndex) {
  validTypes <- c("excel", "pkml", "script", "programmatic")
  if (is.null(entry$type) || !(entry$type %in% validTypes)) {
    cli::cli_abort(
      messages$observedDataInvalidEntryType(
        entry$type %||% "<unset>",
        validTypes
      )
    )
  }
  required <- switch(
    entry$type,
    "excel" = c("file", "importerConfiguration", "sheets"),
    "pkml" = "file",
    "script" = "file",
    "programmatic" = character(0)
  )
  for (field in required) {
    if (is.null(entry[[field]])) {
      cli::cli_abort(
        messages$observedDataMissingField(entryIndex, entry$type, field)
      )
    }
  }
  invisible(TRUE)
}

.resolveDataPath <- function(file, dataFolder) {
  if (is.null(dataFolder)) {
    cli::cli_abort(messages$observedDataDataFolderNotDeclared(file))
  }
  filePath <- file.path(dataFolder, file)
  if (!file.exists(filePath)) {
    cli::cli_abort(messages$observedDataFileNotFound(filePath))
  }
  filePath
}

.loadObservedExcel <- function(entry, dataFolder) {
  filePath <- .resolveDataPath(entry$file, dataFolder)
  importerPath <- .resolveDataPath(entry$importerConfiguration, dataFolder)
  importerConfig <- ospsuite::loadDataImporterConfiguration(
    configurationFilePath = importerPath
  )
  importerConfig$sheets <- unlist(entry$sheets)
  ospsuite::loadDataSetsFromExcel(
    xlsFilePath = filePath,
    importerConfigurationOrPath = importerConfig,
    importAllSheets = FALSE
  )
}

.loadObservedPkml <- function(entry, dataFolder) {
  filePath <- .resolveDataPath(entry$file, dataFolder)
  ds <- ospsuite::loadDataSetFromPKML(filePath = filePath)
  stats::setNames(list(ds), ds$name)
}

.loadObservedScript <- function(entry, dataFolder) {
  filePath <- .resolveDataPath(entry$file, dataFolder)
  cli::cli_inform(c(
    "i" = "Sourcing observed-data script: {.path {filePath}}"
  ))
  result <- source(filePath, local = TRUE)$value
  if (inherits(result, "DataSet")) {
    return(stats::setNames(list(result), result$name))
  }
  if (
    is.list(result) &&
      length(result) > 0 &&
      all(vapply(result, inherits, logical(1), "DataSet"))
  ) {
    return(result)
  }
  cli::cli_abort(
    messages$observedDataScriptWrongReturnType(filePath, class(result)[[1]])
  )
}
