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

# Print ----

#' @exportS3Method
#' @noRd
print.ObservedDataSource <- function(x, ...) {
  ospsuite.utils::ospPrintClass(x)
  ospsuite.utils::ospPrintItems(
    list(
      "Type" = x$type %||% "",
      "File" = x$file %||% "",
      "Name" = x$name %||% "",
      "Importer Configuration" = x$importerConfiguration %||% "",
      "Sheets" = paste(unlist(x$sheets), collapse = ", ")
    ),
    print_empty = TRUE
  )
  invisible(x)
}

# Section validation adapter ----

#' @keywords internal
#' @noRd
.observedDataValidatorAdapter <- function(project) {
  .validateObservedData(project$definitions$observedData, project$paths$dataFolder)
}

# Single source of truth for observed-data source shape, shared by both
# validation entry points (`.validateObservedData`, the project validator
# adapter, and `.validateObservedDataEntry`, the load/add-time guard) so they
# can never disagree about which fields a source type requires.
#
# The rule an Excel source carries `sheets` (the sheet names to import), while
# `pkml` / `script` sources are a single file and `programmatic` is a runtime
# sentinel with no source fields at all. `sheets` is a required field for
# `excel` in BOTH entry points.
#
# @keywords internal
# @noRd
.observedDataValidTypes <- c("excel", "pkml", "script", "programmatic")

.observedDataRequiredFields <- function(type) {
  switch(
    type,
    "excel" = c("file", "importerConfiguration", "sheets"),
    "pkml" = "file",
    "script" = "file",
    "programmatic" = character(0),
    character(0)
  )
}

# A field is "missing" when it is absent (`NULL`) or an empty collection
# (`length 0`, e.g. `sheets = list()`). Both entry points use this one
# predicate so an empty-but-present `sheets` is treated the same everywhere.
#
# @keywords internal
# @noRd
.observedDataFieldMissing <- function(entry, field) {
  val <- entry[[field]]
  is.null(val) || length(val) == 0L
}

#' Validate the `observedData` section of a Project
#'
#' Per-entry checks: `type` is set and is one of `excel`, `pkml`,
#' `script`, `programmatic`; per-type required fields are present;
#' referenced files exist on disk (warnings only — missing files do
#' not block parsing).
#'
#' @param observedData List from `observedData` definitions.
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

  validTypes <- .observedDataValidTypes

  for (i in seq_along(observedData)) {
    entry <- observedData[[i]]
    entryLabel <- paste0("observedData entry ", i)

    if (is.null(entry$type)) {
      result$add_critical_error(
        "Missing Fields",
        messages$validationObservedDataMissingType(entryLabel)
      )
      next
    }

    if (!entry$type %in% validTypes) {
      result$add_critical_error(
        "Invalid Value",
        messages$validationObservedDataInvalidType(
          entryLabel,
          entry$type,
          validTypes
        )
      )
      next
    }

    # Required-field list is the single source of truth shared with the
    # load/add path (`.validateObservedDataEntry`), so the two entry points
    # never disagree about, e.g., whether an Excel source needs `sheets`.
    for (field in .observedDataRequiredFields(entry$type)) {
      if (.observedDataFieldMissing(entry, field)) {
        result$add_critical_error(
          "Missing Fields",
          messages$validationObservedDataMissingField(
            entryLabel,
            entry$type,
            field
          )
        )
      }
    }

    # File existence is a warning (a missing source does not block parsing).
    if (!is.null(dataFolder)) {
      if (!is.null(entry$file)) {
        filePath <- file.path(dataFolder, entry$file)
        if (!file.exists(filePath)) {
          result$add_warning(
            "File Not Found",
            messages$validationObservedDataFileNotFound(entryLabel, entry$file)
          )
        }
      }
      if (
        identical(entry$type, "excel") && !is.null(entry$importerConfiguration)
      ) {
        importerPath <- file.path(dataFolder, entry$importerConfiguration)
        if (!file.exists(importerPath)) {
          result$add_warning(
            "File Not Found",
            messages$validationObservedDataImporterNotFound(
              entryLabel,
              entry$importerConfiguration
            )
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
#' types: `excel` (via importer configuration), `pkml`, `script`, and
#' `programmatic`. A `programmatic` declaration is a sentinel for a
#' `DataSet` added at runtime with [addObservedData()]; its data lives in
#' the session (not on disk), so it is resolved from the in-memory store
#' and is not reproducible across a reload. Prefer a `script` source for a
#' reproducible programmatic data set.
#'
#' @param project A `Project` object (see [loadProject()]).
#' @returns A named list of [`ospsuite::DataSet`] objects. Empty list when
#'   `observedData` definitions is empty or `NULL`.
#' @examples
#' \dontrun{
#' project <- loadProject("path/to/Project.json")
#' dataSets <- loadObservedData(project)
#' }
#' @export
loadObservedData <- function(project) {
  validateIsOfType(project, "Project")
  project$loadObservedData()
}

# Implementation behind `project$loadObservedData()` / `loadObservedData()`.
# Reaches the runtime programmatic store and the names cache through its own
# `private` (handed in by the method), not through an accessor.
#
# @keywords internal
# @noRd
.loadObservedData_impl <- function(self, private) {
  # Attribute any abort to the public authoring function the user called
  # (the free-function forwarder), not this internal `_impl`.
  rlang::local_error_call(rlang::caller_env(2))
  if (is.null(self$definitions$observedData) || length(self$definitions$observedData) == 0) {
    return(list())
  }
  allDataSets <- list()
  for (i in seq_along(self$definitions$observedData)) {
    entry <- self$definitions$observedData[[i]]
    .validateObservedDataEntry(entry, i)
    dataSets <- switch(
      entry$type,
      "excel" = .loadObservedExcel(entry, self$paths$dataFolder),
      "pkml" = .loadObservedPkml(entry, self$paths$dataFolder),
      "script" = .loadObservedScript(entry, self$paths$dataFolder),
      "programmatic" = NULL
    )
    if (!is.null(dataSets)) {
      allDataSets <- .mergeObservedDataSets(allDataSets, dataSets)
    }
  }
  # Merge runtime programmatic store, then cache names.
  allDataSets <- .mergeObservedDataSets(
    allDataSets,
    private$.programmaticDataSets
  )
  private$.observedDataNamesCache <- names(allDataSets)
  allDataSets
}

# Merge a batch of loaded DataSets into the accumulator, aborting on a name
# collision instead of letting `c()` silently keep both (a duplicate name would
# then shadow the earlier set when the list is indexed by name).
#' @keywords internal
#' @noRd
.mergeObservedDataSets <- function(accumulated, incoming) {
  if (length(incoming) == 0) {
    return(accumulated)
  }
  duplicates <- intersect(names(accumulated), names(incoming))
  if (length(duplicates) > 0) {
    cli::cli_abort(messages$observedDataNameCollision(duplicates))
  }
  c(accumulated, incoming)
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
  project$getObservedDataNames()
}

# Implementation behind `project$getObservedDataNames()` /
# `getObservedDataNames()`.
#
# @keywords internal
# @noRd
.getObservedDataNames_impl <- function(self, private) {
  # Attribute any abort to the public authoring function the user called
  # (the free-function forwarder), not this internal `_impl`.
  rlang::local_error_call(rlang::caller_env(2))
  if (!is.null(private$.observedDataNamesCache)) {
    return(private$.observedDataNamesCache)
  }
  .loadObservedData_impl(self, private)
  private$.observedDataNamesCache %||% character(0)
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
  project$addObservedData(entry)
}

# Implementation behind `project$addObservedData()` / `addObservedData()`.
# Reaches the runtime programmatic store and names cache through `private`.
#
# @keywords internal
# @noRd
.addObservedData_impl <- function(self, private, entry) {
  # Attribute any abort to the public authoring function the user called
  # (the free-function forwarder), not this internal `_impl`.
  rlang::local_error_call(rlang::caller_env(2))
  if (inherits(entry, "DataSet")) {
    name <- entry$name
    existingNames <- .getObservedDataNames_impl(self, private)
    if (name %in% existingNames) {
      cli::cli_abort(
        "observedData entry with name {.val {name}} already exists"
      )
    }
    sentinel <- .asObservedDataSource(list(type = "programmatic", name = name))
    # Update the in-memory section and the runtime store together: under
    # explicit-save `.setSection()` does not touch disk, so both are pure
    # in-memory mutations. Any on-disk id/basename collision is surfaced later,
    # by the serializer, when `saveProject()` reconciles the tree.
    private$.setSection(
      "observedData",
      c(private$.getSection("observedData"), list(sentinel))
    )
    private$.programmaticDataSets[[name]] <- entry
    # The observedData setter resets the names cache, so rebuild it after the
    # write, from the names known before it plus the newly added name.
    private$.observedDataNamesCache <- c(existingNames, name)
    cli::cli_inform(c(
      "i" = paste0(
        "For reproducibility, consider declaring this DataSet via a ",
        "script in your Project.json using the {.field observedData} ",
        "field with {.code type = \"script\"} and ",
        "{.code file = \"scripts/your_script.R\"}."
      )
    ))
    return(invisible(self))
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
    # Validate the full entry shape (per-type required fields), not just the
    # type, so an under-specified config entry is rejected at add time.
    .validateObservedDataEntry(entry, length(self$definitions$observedData) + 1L)
    # Config entries are keyed by `file` basename (see removeObservedData);
    # abort on a duplicate to match the other mutators' convention.
    fileBase <- basename(entry[["file"]])
    existingFiles <- vapply(
      self$definitions$observedData,
      function(e) {
        if (is.null(e[["file"]])) NA_character_ else basename(e[["file"]])
      },
      character(1)
    )
    if (fileBase %in% existingFiles) {
      cli::cli_abort(
        "observedData entry with file {.val {fileBase}} already exists"
      )
    }
    private$.observedDataNamesCache <- NULL
    private$.setSection(
      "observedData",
      c(
        private$.getSection("observedData"),
        list(.asObservedDataSource(entry))
      )
    )
    return(invisible(self))
  }

  cli::cli_abort(
    "observedData entry must be a {.cls DataSet} or a configuration list"
  )
}

#' Remove one or more observed-data sources from a Project
#'
#' Removes by DataSet name (for `type = "programmatic"` entries) or by
#' `file` basename (for `type` `"excel"` / `"pkml"` / `"script"`
#' entries). Vectorizes over a vector of ids, removing each in one in-memory
#' update; persist with [saveProject()]. Warns and skips any id with no
#' matching entry.
#'
#' Unlike the other authoring functions, `addObservedData()` is not
#' vectorized over ids: its second argument is a `DataSet` or a configuration
#' list, not an id, so it adds a single source per call. Add several sources
#' with several calls.
#'
#' @param project A `Project` object.
#' @param id Character vector of ids. An observed-data id comes from the data
#'   source (an OSPS `DataSet` name or a file basename) and is matched
#'   verbatim, not canonicalized.
#' @returns The `project` object, invisibly.
#' @export
#' @family observedData
removeObservedData <- function(project, id) {
  validateIsOfType(project, "Project")
  project$removeObservedData(id)
}

# Implementation behind `project$removeObservedData()` / `removeObservedData()`.
#
# @keywords internal
# @noRd
.removeObservedData_impl <- function(self, private, id) {
  # Attribute any abort to the public authoring function the user called
  # (the free-function forwarder), not this internal `_impl`.
  rlang::local_error_call(rlang::caller_env(2))
  .assertIdVector(id)
  observedData <- private$.getSection("observedData")

  # Resolve every id to the section index it removes (a programmatic sentinel or
  # a file-based entry) before touching anything, so the whole batch is
  # validated first and applied in a single in-memory update, matching the
  # all-or-nothing invariant every other vectorized remove* upholds. Nothing
  # touches disk; persist with saveProject().
  dropIdx <- integer()
  programmaticNames <- character()
  missingIds <- character()
  for (one in id) {
    if (one %in% names(private$.programmaticDataSets)) {
      programmaticNames <- c(programmaticNames, one)
      matchIdx <- which(vapply(
        observedData,
        function(e) {
          identical(e$type, "programmatic") && identical(e$name, one)
        },
        logical(1)
      ))
      dropIdx <- c(dropIdx, matchIdx)
      next
    }
    matchIdx <- which(vapply(
      observedData,
      function(e) {
        !is.null(e[["file"]]) && identical(basename(e[["file"]]), one)
      },
      logical(1)
    ))
    if (length(matchIdx) == 0L) {
      missingIds <- c(missingIds, one)
      next
    }
    dropIdx <- c(dropIdx, matchIdx)
  }

  if (length(missingIds) > 0L) {
    cli::cli_warn("observedData entry {.val {missingIds}} not found; no-op.")
  }
  # Warn once per removed id that is still referenced, then drop everything in a
  # single in-memory update. A not-found id contributes nothing to the update.
  for (one in setdiff(id, missingIds)) {
    .warnIfObservedDataReferenced(self, one)
  }
  if (length(dropIdx) == 0L && length(programmaticNames) == 0L) {
    return(invisible(self))
  }

  if (length(dropIdx) > 0L) {
    observedData <- observedData[-unique(dropIdx)]
  }
  # Update the in-memory section and the runtime store together (both pure
  # in-memory mutations under explicit-save; nothing touches disk until
  # `saveProject()`).
  private$.setSection("observedData", observedData)
  for (name in programmaticNames) {
    private$.programmaticDataSets[[name]] <- NULL
  }
  private$.observedDataNamesCache <- NULL
  invisible(self)
}

# Warn when a removed observedData name is still referenced as a
# `dataSet` by any `dataCombined` observed entry. Removal proceeds
# anyway, leaving the dangling reference for the next validateProject()
# call to surface, matching the .warnIfReferenced() convention used by
# the other remove*() mutators.
#
# @keywords internal
# @noRd
.warnIfObservedDataReferenced <- function(project, name) {
  dataCombined <- .unwrapDefinitionList(project$definitions$dataCombined) %||% list()
  holders <- character()
  for (dcName in names(dataCombined)) {
    observed <- dataCombined[[dcName]]$observed %||% list()
    refs <- vapply(
      observed,
      function(e) as.character(e$dataSet %||% NA_character_),
      character(1)
    )
    if (name %in% refs) {
      holders <- c(holders, dcName)
    }
  }
  if (length(holders) > 0) {
    cli::cli_warn(c(
      "Removed observedData {.val {name}} is still referenced by {length(holders)} dataCombined entr{?y/ies}:",
      "*" = "{holders}",
      "i" = "These dataCombined entries now have a dangling reference. Update or remove them."
    ))
  }
  invisible(NULL)
}

# Internal helpers ----

.validateObservedDataEntry <- function(entry, entryIndex) {
  validTypes <- .observedDataValidTypes
  if (is.null(entry$type) || !(entry$type %in% validTypes)) {
    cli::cli_abort(
      messages$observedDataInvalidEntryType(
        entry$type %||% "<unset>",
        validTypes
      )
    )
  }
  # Same required-field spec and same "missing" rule as the project validator
  # (`.validateObservedData`), so a present-but-empty `sheets` on an Excel
  # source is rejected here too rather than passing add/load and failing later
  # under `validateProject()`.
  for (field in .observedDataRequiredFields(entry$type)) {
    if (.observedDataFieldMissing(entry, field)) {
      cli::cli_abort(
        messages$observedDataMissingField(entryIndex, entry$type, field)
      )
    }
  }
  invisible(TRUE)
}

.resolveDataPath <- function(file, dataFolder, fieldName = "file") {
  if (is.null(dataFolder)) {
    cli::cli_abort(messages$observedDataDataFolderNotDeclared(file))
  }
  # Reject a path that escapes the data folder before checking existence, so a
  # traversal attempt aborts clearly instead of as a misleading "not found".
  filePath <- .resolveProjectPath(file, dataFolder, fieldName)
  if (!file.exists(filePath)) {
    cli::cli_abort(messages$observedDataFileNotFound(filePath))
  }
  filePath
}

.loadObservedExcel <- function(entry, dataFolder) {
  filePath <- .resolveDataPath(entry$file, dataFolder, "file")
  importerPath <- .resolveDataPath(
    entry$importerConfiguration,
    dataFolder,
    "importerConfiguration"
  )
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
    # Re-key by each DataSet's own name so a list return is keyed the same
    # way as a single-DataSet return (the script's own list names, if any,
    # are ignored in favour of the authoritative `$name`).
    names(result) <- vapply(result, function(ds) ds$name, character(1))
    return(result)
  }
  cli::cli_abort(
    messages$observedDataScriptWrongReturnType(filePath, class(result)[[1]])
  )
}
