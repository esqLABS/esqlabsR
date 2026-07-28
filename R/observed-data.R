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
  .validateObservedData(
    project$definitions$observedData,
    project$paths$dataFolder
  )
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
    result$addWarning("Data", "No observedData defined")
    return(result)
  }

  validTypes <- .observedDataValidTypes

  for (i in seq_along(observedData)) {
    entry <- observedData[[i]]
    entryLabel <- paste0("observedData entry ", i)

    if (is.null(entry$type)) {
      result$addCriticalError(
        "Missing Fields",
        messages$observedDataMissingType(entryLabel)
      )
      next
    }

    if (!entry$type %in% validTypes) {
      result$addCriticalError(
        "Invalid Value",
        messages$observedDataInvalidType(
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
        result$addCriticalError(
          "Missing Fields",
          messages$validatorObservedDataMissingField(
            entryLabel,
            entry$type,
            field
          )
        )
      }
    }

    # A source path that escapes the data folder is a critical error, matching
    # the abort `.resolveDataPath()` raises at load time, so `validateProject()`
    # does not report clean on a path the loader would reject. Otherwise file
    # existence is only a warning (a missing source does not block parsing).
    if (!is.null(dataFolder)) {
      if (!is.null(entry$file)) {
        if (.pathEscapesRoot(entry$file, dataFolder)) {
          result$addCriticalError(
            "Path Containment",
            messages$observedDataPathEscapes(entryLabel, entry$file)
          )
        } else {
          filePath <- file.path(dataFolder, entry$file)
          if (!file.exists(filePath)) {
            result$addWarning(
              "File Not Found",
              messages$validatorObservedDataFileNotFound(
                entryLabel,
                entry$file
              )
            )
          }
        }
      }
      if (
        identical(entry$type, "excel") && !is.null(entry$importerConfiguration)
      ) {
        if (.pathEscapesRoot(entry$importerConfiguration, dataFolder)) {
          result$addCriticalError(
            "Path Containment",
            messages$observedDataPathEscapes(
              entryLabel,
              entry$importerConfiguration
            )
          )
        } else {
          importerPath <- file.path(dataFolder, entry$importerConfiguration)
          if (!file.exists(importerPath)) {
            result$addWarning(
              "File Not Found",
              messages$observedDataImporterNotFound(
                entryLabel,
                entry$importerConfiguration
              )
            )
          }
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
#' the session until you save, and [saveProject()] persists it to a PKML
#' file and rewrites the entry as a `pkml` source. A `programmatic` sentinel
#' read from disk with no matching in-session `DataSet` (a hand-authored
#' declaration, or a project opened before it was ever saved) resolves to
#' nothing, and the load warns you by name.
#'
#' @param project A `Project` object (see [loadProject()]).
#' @returns A named list of [`ospsuite::DataSet`] objects. Empty list when
#'   `observedData` definitions is empty or `NULL`.
#'
#' @section Security:
#'   A `script` observed-data source runs the R file it names, with
#'   `source()`, on your machine when the data is resolved. Any R code in
#'   that file executes, so treat a project the same way you would treat a
#'   script someone sends you: only load and resolve observed data from a
#'   project you trust. This applies to `loadObservedData()` and to anything
#'   that resolves observed data for you, such as [createDataCombined()].
#'
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
.loadObservedData_impl <- function(self, private, .call) {
  rlang::local_error_call(.call)
  if (
    is.null(self$definitions$observedData) ||
      length(self$definitions$observedData) == 0
  ) {
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
      "script" = .loadObservedScript(
        entry,
        self$paths$dataFolder,
        projectKey = .projectWarningKey(self)
      ),
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
  # A `programmatic` sentinel resolves to nothing unless this session added its
  # DataSet: the data lives only in the runtime store, so a sentinel read from
  # disk (a reload, or a project opened elsewhere) has no backing DataSet and
  # would silently vanish from the result. Warn by name so the drop is loud
  # instead of surfacing later as an obscure dangling DataCombined reference.
  unresolved <- .unresolvedProgrammaticNames(
    self$definitions$observedData,
    names(allDataSets)
  )
  if (length(unresolved) > 0) {
    # `.loadObservedData_impl()` is reentered often (from `getObservedDataNames()`
    # and `addObservedData()`, and on every `createDataCombined()`/`createPlots()`),
    # so throttle to once per session per project rather than firing on every call.
    cli::cli_warn(
      messages$observedDataProgrammaticUnresolved(unresolved),
      .frequency = "once",
      .frequency_id = paste0(
        "esqlabsR_observed_data_unresolved_programmatic_",
        .projectWarningKey(self)
      )
    )
  }
  private$.observedDataNamesCache <- names(allDataSets)
  allDataSets
}

# A stable per-project key for once-per-session warning frequency ids, so a
# throttled warning fires once per project rather than once globally: two
# projects in the same session each get their own warning. Falls back to a
# constant for a session-only project (no file on disk to key on).
#
# @keywords internal
# @noRd
.projectWarningKey <- function(self) {
  self$info$projectFilePath %||% "session"
}

# Names of `programmatic` sentinels with no backing DataSet in the resolved set
# (their data was never added this session, or was lost across a reload).
#
# @keywords internal
# @noRd
.unresolvedProgrammaticNames <- function(observedData, resolvedNames) {
  programmatic <- Filter(
    function(e) identical(e$type, "programmatic"),
    observedData
  )
  names <- vapply(
    programmatic,
    function(e) e$name %||% NA_character_,
    character(1)
  )
  setdiff(names[!is.na(names)], resolvedNames)
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
.getObservedDataNames_impl <- function(self, private, .call) {
  rlang::local_error_call(.call)
  if (!is.null(private$.observedDataNamesCache)) {
    return(private$.observedDataNamesCache)
  }
  # Reentrant call: pass the attribution call on rather than re-resolving it.
  .loadObservedData_impl(self, private, .call = .call)
  private$.observedDataNamesCache %||% character(0)
}

# Public CRUD: observedData ----

#' Add observed data to a Project
#'
#' @description
#' Adds one observed-data declaration. `entry` is either an
#' [`ospsuite::DataSet`] object (which becomes a `type = "programmatic"`
#' declaration keyed by `dataSet$name`) or a list describing where the data is
#' read from.
#'
#' Such a list always carries a `type`, plus the fields that type needs:
#'
#' | `type` | required fields | optional fields |
#' | --- | --- | --- |
#' | `"excel"` | `file`, `importerConfiguration`, `sheets` | `id` |
#' | `"pkml"` | `file` | `id` |
#' | `"script"` | `file` | `id` |
#'
#' `file` and `importerConfiguration` are paths *relative to the project's data
#' folder* (`project$paths$dataFolder`). A file sitting directly in that folder
#' is `"observed.xlsx"`, not `"Data/observed.xlsx"`; one in a subfolder of it is
#' `"subfolder/observed.xlsx"`. `sheets` lists the Excel sheet names to import.
#' A `script` source runs the R file it names (see the Security section of
#' [loadObservedData()]).
#'
#' `id` names the declaration itself: it becomes the declaration's file under
#' the project's definitions folder (`definitions/observed-data/<id>.json` in
#' the default layout) and is the id [removeObservedData()] matches on. Left
#' out, the `file` basename serves as both. It is not the name the data is
#' known by: each imported [`ospsuite::DataSet`] carries the name its source
#' gives it (the data-set name in an Excel sheet, the name inside a PKML file),
#' and that name, not the declaration's id, is what a `dataCombined` entry
#' references.
#'
#' A `DataSet` you pass lives in the R session until you save. On
#' [saveProject()] it is written to a PKML file named `<DataSet name>.pkml`
#' under the project's data folder and its entry becomes a `pkml` source, so
#' the data survives a reload. Saving therefore needs a data folder to be
#' declared in the project's file paths.
#'
#' @param project A `Project` object.
#' @param entry An [`ospsuite::DataSet`] object, or a configuration list
#'   carrying a `type` (`"excel"`, `"pkml"`, or `"script"`) and that type's
#'   fields, as described above.
#' @param overwrite Logical scalar. When `FALSE` (default), a source whose id
#'   already exists (a `DataSet` name, or a configuration list's `id` or `file`
#'   basename) aborts. When `TRUE`, the existing source with that id is replaced
#'   (last-write-wins) as long as both are the same kind. A cross-kind
#'   replacement is refused in either direction, because it would either strand
#'   the session's `DataSet` or leave two sources resolving to one name: remove
#'   the existing source with [removeObservedData()] first, then add the new
#'   one.
#' @returns The `project` object, invisibly.
#' @export
#' @family observedData
addObservedData <- function(project, entry, overwrite = FALSE) {
  validateIsOfType(project, "Project")
  project$addObservedData(entry, overwrite)
}

# Implementation behind `project$addObservedData()` / `addObservedData()`.
# Reaches the runtime programmatic store and names cache through `private`.
#
# @keywords internal
# @noRd
.addObservedData_impl <- function(
  self,
  private,
  entry,
  overwrite = FALSE,
  .call
) {
  rlang::local_error_call(.call)
  if (inherits(entry, "DataSet")) {
    name <- entry$name
    # The DataSet name becomes the PKML file basename (`<name>.pkml`) when the
    # project is saved, so reject a name that is not a safe filename segment now
    # (a `/` in the name, e.g. from a unit like "mg/kg", would otherwise fail
    # later with an opaque low-level path error at save).
    .validateObservedDataId(paste0(name, ".pkml"), call = .call)
    # Reentrant call: pass the attribution call on rather than re-resolving it.
    existingNames <- .getObservedDataNames_impl(self, private, .call = .call)
    # A DataSet collides on two axes, and both have to be checked: on the name
    # its data resolves under (`existingNames`), and on a declared `id`, which a
    # config entry can carry without ever resolving a data set of that name.
    # Only a *declared* id counts here. An id derived from a `file` basename is
    # not a clash: the sentinel is rewritten to `<name>.pkml` at save, so the
    # two never share a definition file on disk.
    declaredIds <- .observedDataDeclaredIds(self$definitions$observedData)
    clashes <- name %in% existingNames || name %in% declaredIds
    if (clashes && !overwrite) {
      cli::cli_abort(c(
        "observedData entry with name {.val {name}} already exists.",
        "i" = "Pass {.code overwrite = TRUE} to replace it."
      ))
    }
    sentinel <- .asObservedDataSource(list(type = "programmatic", name = name))
    # Update the in-memory section and the runtime store together: under
    # explicit-save `.setSection()` does not touch disk, so both are pure
    # in-memory mutations. Any on-disk id/basename collision is surfaced later,
    # by the serializer, when `saveProject()` reconciles the tree.
    observedData <- private$.getSection("observedData")
    # On overwrite, replace the existing programmatic sentinel that carries this
    # name in place. A DataSet is keyed by its `name`, so only a programmatic
    # sentinel of the same name can be replaced in place.
    replaceIdx <- if (overwrite) {
      which(vapply(
        observedData,
        function(e) {
          identical(e$type, "programmatic") && identical(e$name, name)
        },
        logical(1)
      ))
    } else {
      integer()
    }
    if (length(replaceIdx) > 0L) {
      observedData[[replaceIdx[[1]]]] <- sentinel
    } else if (overwrite && clashes) {
      # The name collides with a file-based (pkml/excel/script) source, not a
      # programmatic one, so there is no programmatic entry to replace. Appending
      # would leave two sources resolving to the same name and only fail later,
      # opaquely, at the next load/save. Abort now, naming the fix.
      cli::cli_abort(c(
        "observedData entry with name {.val {name}} is a file-based source, \\
        not a programmatic one, so it cannot be overwritten with a {.cls DataSet}.",
        "i" = "Remove it first with {.code removeObservedData()} (by its \\
        {.field id}, or its file basename), then add the {.cls DataSet}."
      ))
    } else {
      observedData <- c(observedData, list(sentinel))
    }
    private$.setSection("observedData", observedData)
    private$.programmaticDataSets[[name]] <- entry
    # The observedData setter resets the names cache, so rebuild it after the
    # write. On an overwrite the name is already in `existingNames`, so union
    # rather than append to keep the cache free of duplicates.
    private$.observedDataNamesCache <- union(existingNames, name)
    cli::cli_inform(messages$observedDataProgrammaticAdded(
      name,
      hasDataFolder = !is.null(self$paths$dataFolder)
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
    .validateObservedDataEntry(
      entry,
      length(self$definitions$observedData) + 1L
    )
    # A declared `id` names the entry everywhere (its definition file, and the
    # key `removeObservedData()` matches), so it has to be a usable string
    # before anything derives a filename from it.
    if (
      !is.null(entry[["id"]]) && is.null(.usableObservedDataId(entry[["id"]]))
    ) {
      cli::cli_abort(
        "An observedData entry's {.field id} must be a single non-empty string."
      )
    }
    # Config entries are keyed by the id `removeObservedData()` matches on: the
    # entry's own `id` when it declares one, else its `file` basename. That id
    # also names the definition file, so reject an unsafe one here rather than
    # at save. Abort on a duplicate to match the other mutators' convention,
    # unless overwriting.
    entryId <- .observedDataEntryId(entry, call = .call)
    .validateObservedDataId(entryId, call = .call)
    existingIds <- .observedDataSectionIds(self$definitions$observedData)
    if (entryId %in% existingIds && !overwrite) {
      cli::cli_abort(c(
        "observedData entry with id {.val {entryId}} already exists.",
        "i" = "Pass {.code overwrite = TRUE} to replace it."
      ))
    }
    private$.observedDataNamesCache <- NULL
    observedData <- private$.getSection("observedData")
    # On overwrite, replace the existing entry carrying this id in place;
    # otherwise append (a non-overwrite id clash aborted above).
    replaceIdx <- if (overwrite) which(existingIds == entryId) else integer()
    if (
      length(replaceIdx) > 0L &&
        identical(observedData[[replaceIdx[[1]]]]$type, "programmatic")
    ) {
      # Replacing a programmatic sentinel in place would strand its `DataSet` in
      # the runtime store: nothing would write it at save and nothing would warn
      # that it vanished. Refuse, as the mirror case (a `DataSet` overwriting a
      # file-based source) already does.
      cli::cli_abort(c(
        "observedData entry with id {.val {entryId}} is a programmatic source \\
        holding a {.cls DataSet} in this session, so it cannot be overwritten \\
        with a configuration entry.",
        "i" = "Remove it first with {.code removeObservedData()}, then add the \\
        configuration entry."
      ))
    }
    if (length(replaceIdx) > 0L) {
      observedData[[replaceIdx[[1]]]] <- .asObservedDataSource(entry)
    } else {
      observedData <- c(observedData, list(.asObservedDataSource(entry)))
    }
    private$.setSection("observedData", observedData)
    return(invisible(self))
  }

  cli::cli_abort(
    "observedData entry must be a {.cls DataSet} or a configuration list"
  )
}

#' Remove one or more observed-data sources from a Project
#'
#' Removes by the source's id: its `id` field when the declaration carries one,
#' else the DataSet name (for a `type = "programmatic"` entry that has not been
#' saved yet) or the `file` basename (for `type` `"excel"` / `"pkml"` /
#' `"script"` entries). Vectorizes over a vector of ids, removing each in one
#' in-memory update; persist with [saveProject()]. Warns and skips any id with
#' no matching entry.
#'
#' Note a programmatic source changes its id once saved: [saveProject()] writes
#' the `DataSet` to `<name>.pkml` and rewrites the entry as a `pkml` source, so
#' after a save you remove it by that file basename (`"<name>.pkml"`), not by the
#' original `DataSet` name.
#'
#' Unlike the other authoring functions, `addObservedData()` is not
#' vectorized over ids: its second argument is a `DataSet` or a configuration
#' list, not an id, so it adds a single source per call. Add several sources
#' with several calls.
#'
#' @param project A `Project` object.
#' @param id Character vector of ids. An observed-data id is the declaration's
#'   `id` field, or, when it declares none, comes from the data source itself
#'   (the `DataSet` name of an unsaved programmatic source, or a file basename
#'   for a file-based source). Either way it is matched verbatim, not
#'   canonicalized. A saved programmatic source that declares no `id` is matched
#'   by its `<name>.pkml` basename (see the note above).
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
.removeObservedData_impl <- function(self, private, id, .call) {
  rlang::local_error_call(.call)
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
      function(e) identical(.observedDataEntryIdOrNA(e), one),
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
  dataCombined <- .unwrapDefinitionList(project$definitions$dataCombined) %||%
    list()
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

# The declaration id of every entry in the section, `NA` for one that carries
# nothing usable to derive an id from. One resolver for both kinds, so the
# add-time collision check, the overwrite match, and the removal match all agree
# on what an entry is called.
#
# @keywords internal
# @noRd
.observedDataSectionIds <- function(observedData) {
  vapply(observedData %||% list(), .observedDataEntryIdOrNA, character(1))
}

# Only the ids the declarations state outright. Distinct from
# `.observedDataSectionIds()`, which also derives one for a declaration that
# states none: a derived id can change under the declaration (a programmatic
# sentinel is rewritten to `<name>.pkml` at save), while a declared one never
# does.
#
# @keywords internal
# @noRd
.observedDataDeclaredIds <- function(observedData) {
  ids <- lapply(observedData %||% list(), function(e) {
    .usableObservedDataId(e[["id"]])
  })
  unlist(ids) %||% character()
}

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

.loadObservedScript <- function(entry, dataFolder, projectKey = "") {
  filePath <- .resolveDataPath(entry$file, dataFolder)
  # A script source executes arbitrary R (see the loadObservedData() Security
  # section). Warn the user that this is happening, once per session per project
  # so a project with several script sources, or a repeated resolve, stays quiet
  # after the first. The per-project key means a second, untrusted project in the
  # same session still gets its own warning rather than being silenced by a
  # trusted project that already tripped a global gate.
  cli::cli_warn(
    messages$observedDataScriptSecurityWarn(),
    .frequency = "once",
    .frequency_id = paste0("esqlabsR_observed_data_script_source_", projectKey)
  )
  cli::cli_inform(messages$observedDataScriptSourcing(filePath))
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

# Persist every session-added programmatic DataSet to a PKML file next to the
# project, so it survives a reload. Called from the save path before the tree is
# written: for each `programmatic` entry whose DataSet is in the runtime store,
# write `<dataFolder>/<name>.pkml` and rewrite the section entry to a `pkml`
# source pointing at that file. A programmatic entry with no DataSet in the store
# (an orphan sentinel read from disk) is left untouched.
#
# Returns the character vector of persisted DataSet names; the caller drops these
# from the runtime store only after the whole save commits, so an abort partway
# through the tree write leaves the DataSet recoverable (worst case is an orphan
# PKML file, never lost data).
#
# @keywords internal
# @noRd
.persistProgrammaticObservedData <- function(self, private) {
  observedData <- private$.getSection("observedData")
  toPersist <- which(vapply(
    observedData,
    function(e) {
      # A hand-authored `programmatic` sentinel may carry no `name`; guard it
      # before indexing the store (`store[[NULL]]` aborts). A name-less sentinel
      # has no backing DataSet, so it is left untouched, matching this function's
      # orphan-sentinel contract.
      identical(e$type, "programmatic") &&
        !is.null(e$name) &&
        !is.null(private$.programmaticDataSets[[e$name]])
    },
    logical(1)
  ))
  if (length(toPersist) == 0) {
    return(character(0))
  }

  dataFolder <- self$paths$dataFolder
  if (is.null(dataFolder)) {
    cli::cli_abort(messages$observedDataPersistNoDataFolder(
      observedData[[toPersist[[1]]]]$name
    ))
  }

  # Rewrite the whole section in memory and validate it clean BEFORE writing any
  # PKML file, so a save that aborts (an unsafe name, or a basename that collides
  # with another source's on-disk id) never overwrites an existing file: every
  # write below is known to land on a fresh, collision-free path. This upholds
  # the all-or-nothing save contract for the on-disk data files, not just the
  # definition tree.
  persistedNames <- vapply(
    observedData[toPersist],
    function(e) e$name,
    character(1)
  )
  fileNames <- paste0(persistedNames, ".pkml")
  for (i in seq_along(toPersist)) {
    observedData[[toPersist[[i]]]] <- .asObservedDataSource(list(
      type = "pkml",
      file = fileNames[[i]]
    ))
  }
  # Same checks the serializer runs, but up front: each id a safe filename
  # segment, and no two entries sharing an id.
  ids <- vapply(observedData, .observedDataEntryId, character(1))
  lapply(ids, .validateObservedDataId)
  duplicates <- unique(ids[duplicated(ids)])
  if (length(duplicates) > 0) {
    cli::cli_abort(messages$observedDataPersistIdCollision(duplicates))
  }

  # Materialize the folder (a from-scratch project may declare one that has no
  # directory on disk yet), then write each PKML now that the batch is safe.
  if (!dir.exists(dataFolder)) {
    dir.create(dataFolder, recursive = TRUE, showWarnings = FALSE)
  }
  for (i in seq_along(toPersist)) {
    filePath <- .resolveProjectPath(fileNames[[i]], dataFolder, "file")
    ospsuite::saveDataSetToPKML(
      private$.programmaticDataSets[[persistedNames[[i]]]],
      filePath
    )
  }

  private$.setSection("observedData", observedData)
  # `.setSection("observedData", ...)` already reset the names cache; the entries
  # still resolve to the same DataSet names (now from PKML), so a rebuild on the
  # next read is correct. The runtime-store entries stay until the whole save
  # commits (dropped by the caller after the tree write succeeds), so a save that
  # aborts mid-tree-write leaves the DataSet recoverable rather than lost.
  persistedNames
}
