# Observed data: project-driven loader dispatch + section CRUD.
#
# Owns Project$observedData end-to-end. Called by:
#   - users via loadObservedData(project) — the public dispatcher.
#   - createDataCombined() internally to resolve observed sources.
#   - validateProject() via .observedDataValidatorAdapter()
#   - users via the public addObservedData / removeObservedData
#     functions.
#
# The three declared source types in v2.0 Project.json are:
#   - excel  : ospsuite::loadDataSetsFromExcel via importer config
#   - pkml   : ospsuite::loadDataSetFromPKML
#   - programmatic : DataSets added at runtime via addObservedData(project).
#                    The DataSet itself is not JSON-serializable, so it
#                    is held on the Project's private slot
#                    (.programmaticDataSets), keyed by its source id, and the
#                    JSON sentinel `{id: <source id>, type: "programmatic"}`
#                    is what survives a round-trip.
#
# Every declaration stores its id as its first field, and the section is a list
# named by those ids (`.observedDataSectionById()`).

# Print ----

#' @exportS3Method
#' @noRd
print.ObservedDataSource <- function(x, ...) {
  ospsuite.utils::ospPrintClass(x)
  ospsuite.utils::ospPrintItems(
    list(
      "Id" = x$id %||% "",
      "Type" = x$type %||% "",
      "File" = x$file %||% "",
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
# a `pkml` source is a single file and `programmatic` is a runtime
# sentinel with no source fields at all. `sheets` is a required field for
# `excel` in BOTH entry points.
#
# @keywords internal
# @noRd
.observedDataValidTypes <- c("excel", "pkml", "programmatic")

.observedDataRequiredFields <- function(type) {
  switch(
    type,
    "excel" = c("file", "importerConfiguration", "sheets"),
    "pkml" = "file",
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
#' `programmatic`; per-type required fields are present;
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
#' types: `excel` (via importer configuration), `pkml`, and
#' `programmatic`. A `programmatic` declaration is a sentinel for a
#' `DataSet` added at runtime with [addObservedData()]; its data lives in
#' the session until you save, and [saveProject()] writes it to `<id>.pkml`
#' in the data folder and rewrites the entry as a `pkml` source. A
#' `programmatic` sentinel read from disk with no matching in-session
#' `DataSet` (a declaration written by hand, or a project opened before it was
#' ever saved) resolves to nothing, and the load warns you, naming its id.
#'
#' @param project A `Project` object (see [loadProject()]).
#' @returns A named list of [`ospsuite::DataSet`] objects. Empty list when
#'   `observedData` definitions is empty or `NULL`.
#'
#' @examples
#' \dontrun{
#' project <- loadProject("path/to/Project.json")
#' dataSets <- loadObservedData(project)
#' }
#' @export
loadObservedData <- function(project) {
  ospsuite.utils::validateIsOfType(project, "Project")
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
      "programmatic" = NULL
    )
    if (!is.null(dataSets)) {
      allDataSets <- .mergeObservedDataSets(allDataSets, dataSets)
    }
  }
  # The cache keeps only the names read from files: a session DataSet can be
  # renamed at any time, so its name is always read from the DataSet itself.
  fileDataSetNames <- names(allDataSets)
  allDataSets <- .mergeObservedDataSets(
    allDataSets,
    .programmaticDataSetsByName(private$.programmaticDataSets)
  )
  # A `programmatic` sentinel resolves to nothing unless this session added its
  # DataSet: the data lives only in the runtime store, so a sentinel read from
  # disk (a reload, or a project opened elsewhere) has no backing DataSet and
  # would silently vanish from the result. Warn by id so the drop is loud
  # instead of surfacing later as an obscure dangling DataCombined reference.
  unresolved <- .unresolvedProgrammaticIds(
    self$definitions$observedData,
    names(private$.programmaticDataSets)
  )
  if (length(unresolved) > 0) {
    # `.loadObservedData_impl()` is reentered often (from `getObservedDataNames()`
    # and on every `createDataCombined()`/`createPlots()`), so throttle to once
    # per session per project rather than firing on every call.
    cli::cli_warn(
      messages$observedDataProgrammaticUnresolved(unresolved),
      .frequency = "once",
      .frequency_id = paste0(
        "esqlabsR_observed_data_unresolved_programmatic_",
        .projectWarningKey(self)
      )
    )
  }
  private$.observedDataNamesCache <- fileDataSetNames %||% character(0)
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

# Ids of `programmatic` sentinels with no DataSet in the session's store, which
# is keyed by source id (their data was never added this session, or was lost
# across a reload).
#
# @keywords internal
# @noRd
.unresolvedProgrammaticIds <- function(observedData, storeIds) {
  isProgrammatic <- vapply(
    observedData %||% list(),
    \(e) identical(e$type, "programmatic"),
    logical(1)
  )
  ids <- names(observedData)[isProgrammatic]
  setdiff(ids, storeIds)
}

# The session's DataSets, which the store keeps under their source ids, named
# by their own current names: a `DataSet` can be renamed after it is added, and
# a load returns every data set under its name.
#
# @keywords internal
# @noRd
.programmaticDataSetsByName <- function(store) {
  stats::setNames(
    unname(store),
    vapply(store, \(ds) ds$name, character(1), USE.NAMES = FALSE)
  )
}

# Merge a batch of loaded DataSets into the accumulator, aborting on a name
# collision instead of letting `c()` silently keep both (a duplicate name would
# then shadow the earlier set when the list is indexed by name). A batch can
# repeat a name itself: two session DataSets renamed to one name.
#' @keywords internal
#' @noRd
.mergeObservedDataSets <- function(
  accumulated,
  incoming,
  call = rlang::caller_env()
) {
  if (length(incoming) == 0) {
    return(accumulated)
  }
  incomingNames <- names(incoming)
  duplicates <- unique(c(
    intersect(names(accumulated), incomingNames),
    incomingNames[duplicated(incomingNames)]
  ))
  if (length(duplicates) > 0) {
    cli::cli_abort(messages$observedDataNameCollision(duplicates), call = call)
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
  ospsuite.utils::validateIsOfType(project, "Project")
  project$getObservedDataNames()
}

# Implementation behind `project$getObservedDataNames()` /
# `getObservedDataNames()`.
#
# @keywords internal
# @noRd
.getObservedDataNames_impl <- function(self, private, .call) {
  rlang::local_error_call(.call)
  if (is.null(private$.observedDataNamesCache)) {
    # Reentrant call: pass the attribution call on rather than re-resolving it.
    .loadObservedData_impl(self, private, .call = .call)
  }
  # The cache holds the names read from files; the names of the session's
  # DataSets are read from the DataSets, in the order a load returns them.
  c(
    private$.observedDataNamesCache %||% character(0),
    vapply(
      private$.programmaticDataSets,
      \(ds) ds$name,
      character(1),
      USE.NAMES = FALSE
    )
  )
}

# Public CRUD: observedData ----

#' Add observed data to a Project
#'
#' @description
#' Adds one observed-data source. `entry` is either an [`ospsuite::DataSet`]
#' built in the session, or a list naming the file the data is read from:
#'
#' | `type` | required fields | optional fields |
#' | --- | --- | --- |
#' | `"excel"` | `file`, `importerConfiguration`, `sheets` | `id` |
#' | `"pkml"` | `file` | `id` |
#'
#' Paths are relative to the project's data folder: `"observed.xlsx"`, not
#' `"Data/observed.xlsx"`.
#'
#' Every source has an id: it names the source's definition file and is what
#' [removeObservedData()] takes. The id is the `id` argument, else the `id`
#' field of a list `entry`, else it is taken from the source: the `file` name
#' without its extension, or the name of the `DataSet`. Like every other
#' definition id, it is lowercased and made a safe file name, so
#' `"Laskin 1982.xlsx"` gives the id `laskin_1982`. An id you give warns when
#' that changes it. The source stores its id, so
#' `project$definitions$observedData[[id]]` reads it. An `excel` or `pkml`
#' source read that way, changed and passed back with `overwrite = TRUE`
#' replaces itself. To replace a `DataSet` added in the session, pass the new
#' `DataSet` itself with the same `id` and `overwrite = TRUE`. A `dataCombined`
#' entry refers to the name of a data set inside the source, not to this id.
#'
#' @details
#' A `DataSet` is stored as a `type = "programmatic"` source, and its data
#' stays in the R session until [saveProject()] writes it to `<id>.pkml` in the
#' data folder. Saving therefore needs a data folder. From then on it is a
#' `pkml` source with the same id, and the `DataSet` keeps its own name inside
#' that file.
#'
#' Two files with the same name, such as `data.xlsx` and `data.pkml`, give the
#' same id, so the second is refused. Give it another `id`.
#'
#' Adding a `DataSet` reads no source. Its name is refused at once when
#' another source of the project already has it, for example a `DataSet`
#' added or saved earlier, also with `overwrite = TRUE`. The name of the
#' `DataSet` that `overwrite = TRUE` replaces is free for its replacement. A
#' clash with a data set inside an Excel or PKML source that has not been
#' loaded yet stops the next [loadObservedData()] with an error naming the
#' duplicate. Then remove the
#' `DataSet`'s source with [removeObservedData()], rename the `DataSet`, and
#' add it again.
#'
#' @param project A `Project` object.
#' @param entry An [`ospsuite::DataSet`], or a list with a `type` and that
#'   type's fields (see the table above).
#' @param id Character scalar, the id of the new source. `NULL` (default) uses
#'   the `id` field of a list `entry`, or else takes the id from the source (see
#'   Description).
#' @param overwrite Logical scalar. When `FALSE` (default), an id that already
#'   exists aborts. When `TRUE`, the source with that id is replaced, but only
#'   by one of the same kind (a `DataSet` by a `DataSet`, a list by a list). To
#'   replace one kind with the other, remove the existing source with
#'   [removeObservedData()] first.
#' @returns The `project` object, invisibly.
#' @export
#' @family observedData
addObservedData <- function(project, entry, id = NULL, overwrite = FALSE) {
  ospsuite.utils::validateIsOfType(project, "Project")
  project$addObservedData(entry, id, overwrite)
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
  id = NULL,
  overwrite = FALSE,
  .call
) {
  rlang::local_error_call(.call)
  # `inherits()` rather than `ospsuite.utils::isOfType()`, which is also TRUE
  # for a list of DataSets.
  isDataSet <- inherits(entry, "DataSet")
  if (!isDataSet) {
    if (!is.list(entry)) {
      cli::cli_abort(messages$observedDataEntryNotDataSetOrList())
    }
    if (is.null(entry$type)) {
      cli::cli_abort(messages$observedDataEntryMissingType())
    }
    validTypes <- c("excel", "pkml")
    if (!(entry$type %in% validTypes)) {
      cli::cli_abort(messages$observedDataInvalidEntryType(
        entry$type,
        validTypes
      ))
    }
    # Validate the full entry shape (per-type required fields), not just the
    # type, so an under-specified config entry is rejected at add time.
    .validateObservedDataEntry(
      entry,
      length(self$definitions$observedData) + 1L,
      call = .call
    )
  }

  id <- .resolveObservedDataId(entry, id, isDataSet, call = .call)
  sources <- self$definitions$observedData
  sectionIds <- names(sources) %||% character()
  .assertNoOverwriteClash(
    id,
    sectionIds,
    "observed-data source",
    overwrite,
    call = .call
  )
  idx <- match(id, sectionIds)
  replacing <- !is.na(idx)
  observedData <- private$.getSection("observedData")

  if (isDataSet) {
    # Only a programmatic source can be replaced by a DataSet: a file source
    # replaced in place would leave its data file behind, unread.
    if (replacing && !identical(sources[[idx]]$type, "programmatic")) {
      cli::cli_abort(messages$observedDataNotReplaceableByDataSet(id))
    }
    name <- entry$name
    # Check only the names known without reading a source: the names cache of
    # the file sources, when a lookup has filled it, and the session's
    # DataSets. Reading the sources would import every file and stop on any
    # unreadable one. A clash with a name inside a source not read yet aborts
    # the next load instead (`.mergeObservedDataSets()`). The DataSet this one
    # replaces is left out by its id, so its name is free for its replacement
    # but stays taken when another DataSet has it too.
    namesCache <- private$.observedDataNamesCache
    otherDataSets <- private$.programmaticDataSets
    otherDataSets[[id]] <- NULL
    knownNames <- c(
      namesCache,
      vapply(otherDataSets, \(ds) ds$name, character(1), USE.NAMES = FALSE)
    )
    if (name %in% knownNames) {
      cli::cli_abort(messages$observedDataSetNameExists(name))
    }
    # Saving writes `<id>.pkml` into the data folder, so a source already
    # reading its data from that file would lose it. The replaced source is
    # programmatic and reads no file.
    dataFolder <- self$paths$dataFolder
    savedFile <- paste0(id, ".pkml")
    sourceFiles <- .observedDataResolvedFiles(
      .observedDataSourceFiles(sources),
      dataFolder
    )
    if (.observedDataResolvedFiles(savedFile, dataFolder) %in% sourceFiles) {
      cli::cli_abort(messages$observedDataAddFileCollision(savedFile))
    }

    # Update the in-memory section and the runtime store together: under
    # explicit-save `.setSection()` does not touch disk.
    sentinel <- list(id = id, type = "programmatic")
    if (replacing) {
      observedData[[idx]] <- sentinel
    } else {
      observedData <- c(observedData, list(sentinel))
    }
    private$.setSection("observedData", observedData)
    private$.programmaticDataSets[[id]] <- entry
    # The observedData setter resets the names cache, so restore it after the
    # write: adding a DataSet changes no file source. An unfilled cache stays
    # unfilled: the next lookup reads every source.
    private$.observedDataNamesCache <- namesCache
    cli::cli_inform(messages$observedDataProgrammaticAdded(
      name,
      id,
      hasDataFolder = !is.null(dataFolder)
    ))
    return(invisible(self))
  }

  if (
    replacing &&
      (identical(sources[[idx]]$type, "programmatic") ||
        id %in% names(private$.programmaticDataSets))
  ) {
    # Replacing a programmatic sentinel in place would strand its `DataSet` in
    # the runtime store: nothing would write it at save and nothing would warn
    # that it vanished. The same holds for a source a failed save has already
    # rewritten as `pkml` while its DataSet is still held. Refuse, as the mirror
    # case (a `DataSet` replacing a file source) does.
    cli::cli_abort(messages$observedDataNotReplaceableByEntry(id))
  }
  # The data-set names inside a config source are known only by reading it, so
  # the observedData setter leaves the names cache unset, and the next lookup
  # reads every source.
  record <- unclass(entry)
  record$id <- id
  if (replacing) {
    observedData[[idx]] <- record
  } else {
    observedData <- c(observedData, list(record))
  }
  private$.setSection("observedData", observedData)
  invisible(self)
}

# The id a source added with `addObservedData()` is filed under. An id the
# user typed (the `id` argument, else the `id` field of a list entry) is
# canonicalized like every other definition id, with the standard warning when
# that changes it. Without one, the id is taken from the source (the `file`
# stem, or the DataSet name) and canonicalized silently, since the user typed
# none.
#
# @keywords internal
# @noRd
.resolveObservedDataId <- function(
  entry,
  id,
  isDataSet,
  call = rlang::caller_env()
) {
  typed <- id %||% (if (!isDataSet) entry[["id"]])
  if (!is.null(typed)) {
    .assertIdVector(typed, call = call)
    .assertNonEmptyString(typed, "id", call = call)
    return(.canonicalizeId(typed))
  }
  source <- if (isDataSet) entry$name else entry[["file"]]
  if (!.isNonEmptyString(source)) {
    cli::cli_abort(messages$observedDataNoIdSource(), call = call)
  }
  if (isDataSet) {
    .canonicalizeOneId(source)
  } else {
    .observedDataIdFromFile(source)
  }
}

#' Remove one or more observed-data sources from a Project
#'
#' Removes by the source's id, the `id` field of each entry in
#' `project$definitions$observedData` (also the names of that list). Accepts a
#' vector of ids and removes them all at once; [saveProject()] then deletes
#' their definition files. Warns and skips any id with no matching entry.
#'
#' Unlike the other authoring functions, `addObservedData()` is not
#' vectorized over ids: its second argument is a `DataSet` or a configuration
#' list, not an id, so it adds a single source per call. Add several sources
#' with several calls.
#'
#' @details
#' A `dataCombined` entry refers to a data set by its name, not by the id of its
#' source. When some `dataCombined` entry refers to a data set,
#' `removeObservedData()` reads the removed source to learn the names of its
#' data sets, and warns for each `dataCombined` that still refers to one of
#' them. A source that cannot be read (a missing file, for example) is removed
#' without this warning; [createDataCombined()] reports the reference that no
#' longer resolves when it builds that `DataCombined`.
#'
#' @param project A `Project` object.
#' @param id Character vector of ids. Like every other definition id, each is
#'   lowercased and made a safe file name before it is looked up, with a
#'   warning when that changes it, so `"Scratch measurement"` finds the source
#'   `scratch_measurement`.
#' @returns The `project` object, invisibly.
#' @export
#' @family observedData
removeObservedData <- function(project, id) {
  ospsuite.utils::validateIsOfType(project, "Project")
  project$removeObservedData(id)
}

# Implementation behind `project$removeObservedData()` / `removeObservedData()`.
# Every id is validated before anything changes, and the whole batch is applied
# in a single in-memory update, as every other vectorized remove* does. Nothing
# is written to disk; persist with saveProject().
#
# @keywords internal
# @noRd
.removeObservedData_impl <- function(self, private, id, .call) {
  rlang::local_error_call(.call)
  .assertIdVector(id)
  id <- .canonicalizeId(id)
  observedData <- private$.getSection("observedData")
  sectionIds <- names(observedData) %||% character()

  missingIds <- setdiff(id, sectionIds)
  if (length(missingIds) > 0L) {
    cli::cli_warn("observedData entry {.val {missingIds}} not found; no-op.")
  }
  removedIds <- intersect(id, sectionIds)
  if (length(removedIds) == 0L) {
    return(invisible(self))
  }
  # Warn once per removed id that is still referenced. A `dataCombined` names a
  # data set, not a source, so each removed source's data-set names are learned
  # before the removal takes its DataSet out of the session, and a file source
  # is read only when some `dataCombined` refers to a data set at all.
  references <- .dataCombinedDataSetReferences(self)
  if (length(references) > 0L) {
    for (removedId in removedIds) {
      .warnIfObservedDataReferenced(
        removedId,
        .observedDataSourceDataSetNames(
          observedData[[removedId]],
          private$.programmaticDataSets[[removedId]],
          self$paths$dataFolder
        ),
        references
      )
    }
  }
  # A removed source takes its DataSet out of the session (the store is keyed by
  # source id). That includes a source a failed save has already rewritten as
  # `pkml` while its DataSet is still held.
  storedIds <- intersect(removedIds, names(private$.programmaticDataSets))
  observedData[removedIds] <- NULL
  private$.setSection("observedData", observedData)
  for (storedId in storedIds) {
    private$.programmaticDataSets[[storedId]] <- NULL
  }
  private$.observedDataNamesCache <- NULL
  invisible(self)
}

# Warn when a removed observed-data source is still referenced as a
# `dataSet` by any `dataCombined` observed entry. Removal proceeds
# anyway, leaving the dangling reference for createDataCombined() to
# report when it builds that DataCombined, matching the .warnIfReferenced()
# convention used by the other remove*() mutators.
#
# A `dataSet` reference names a data set, not a source, so a `dataCombined` is
# a holder when it names one of the source's data sets (`dataSetNames`), matched
# exactly. `NULL` (the names could not be learned) warns about nothing.
# `references` is `.dataCombinedDataSetReferences()` of the project.
#
# @keywords internal
# @noRd
.warnIfObservedDataReferenced <- function(id, dataSetNames, references) {
  holders <- names(references)[vapply(
    references,
    \(refs) any(refs %in% dataSetNames),
    logical(1)
  )]
  if (length(holders) > 0) {
    cli::cli_warn(messages$observedDataStillReferenced(id, holders))
  }
  invisible(NULL)
}

# The `dataSet` references of every `dataCombined` that has any, named by the
# `dataCombined` id.
#
# @keywords internal
# @noRd
.dataCombinedDataSetReferences <- function(project) {
  dataCombined <- .unwrapDefinitionList(project$definitions$dataCombined) %||%
    list()
  references <- lapply(dataCombined, function(dc) {
    refs <- vapply(
      dc$observed %||% list(),
      function(e) as.character(e$dataSet %||% NA_character_),
      character(1)
    )
    refs[!is.na(refs)]
  })
  references[lengths(references) > 0L]
}

# The names of the data sets an observed-data source provides, or `NULL` when
# they cannot be learned. A DataSet held in the session under the source's id
# gives its current name, whatever the entry's type: a source a failed save has
# already rewritten as `pkml` still holds it. Otherwise a file source is read,
# and one that cannot be read (a missing file, for example) gives `NULL`, as in
# `.observedDataNamesForCrossReference()`. A `programmatic` entry without its
# DataSet has no names to give.
#
# @keywords internal
# @noRd
.observedDataSourceDataSetNames <- function(entry, dataSet, dataFolder) {
  if (!is.null(dataSet)) {
    return(dataSet$name)
  }
  load <- switch(
    entry$type %||% "",
    "pkml" = .loadObservedPkml,
    "excel" = .loadObservedExcel,
    NULL
  )
  if (is.null(load)) {
    return(NULL)
  }
  tryCatch(
    suppressMessages(suppressWarnings(names(load(entry, dataFolder)))),
    error = function(cnd) NULL
  )
}

# Internal helpers ----

# The data-set names a cross-reference can resolve against, or `NULL` when the
# project cannot be asked.
#
# A declaration's id names the declaration; what a parameter-identification
# output mapping references is the name of a data set INSIDE the source, which
# only the source knows, so answering means loading it. Reading an Excel or PKML
# source is the same kind of filesystem access the section validators already
# do. A load that fails returns `NULL`, since an unreadable source is the
# observedData section's own finding rather than a dangling reference. `NULL`
# therefore means "unresolved", not "nothing defined".
#
# @keywords internal
# @noRd
.observedDataNamesForCrossReference <- function(project) {
  declarations <- project$definitions$observedData %||% list()
  if (length(declarations) == 0L) {
    return(character(0))
  }
  tryCatch(
    suppressMessages(suppressWarnings(project$getObservedDataNames())),
    error = function(cnd) NULL
  )
}

# The id an observed-data source takes from its data file: the basename without
# its extension, canonicalized without a warning, since the user typed no id.
#
# @keywords internal
# @noRd
.observedDataIdFromFile <- function(file) {
  .canonicalizeOneId(tools::file_path_sans_ext(basename(file)))
}

# The `file` of every entry in the section as written (relative to the data
# folder, or absolute), `NA` for one that names none (a programmatic entry).
#
# @keywords internal
# @noRd
.observedDataSourceFiles <- function(observedData) {
  vapply(
    observedData %||% list(),
    \(e) if (.isNonEmptyString(e[["file"]])) e[["file"]] else NA_character_,
    character(1)
  )
}

# Where each of `files` (paths relative to the data folder, or absolute) lies,
# for comparing two names of one file: resolved against the data folder on the
# text alone (`.` and `..` removed, nothing on disk read) and lowercased,
# because Windows and macOS treat names differing only in case as one file.
# `NA` stays `NA`. A path that escapes the data folder resolves like any other
# instead of aborting: it is only compared here, and the load reports it.
# Without a data folder, relative paths resolve against the working directory,
# which keeps them comparable with each other.
#
# @keywords internal
# @noRd
.observedDataResolvedFiles <- function(files, dataFolder) {
  absRoot <- as.character(fs::path_abs(dataFolder %||% "."))
  vapply(
    files,
    function(file) {
      if (is.na(file)) {
        return(NA_character_)
      }
      tolower(.absoluteAgainstRoot(file, absRoot))
    },
    character(1),
    USE.NAMES = FALSE
  )
}

.validateObservedDataEntry <- function(
  entry,
  entryIndex,
  call = rlang::caller_env()
) {
  validTypes <- .observedDataValidTypes
  if (is.null(entry$type) || !(entry$type %in% validTypes)) {
    cli::cli_abort(
      messages$observedDataInvalidEntryType(
        entry$type %||% "<unset>",
        validTypes
      ),
      call = call
    )
  }
  # Same required-field spec and same "missing" rule as the project validator
  # (`.validateObservedData`), so a present-but-empty `sheets` on an Excel
  # source is rejected here too rather than passing add/load and failing later
  # under `validateProject()`.
  #
  # Every missing field is reported at once. Aborting on the first one made an
  # under-specified entry take one call per field to discover, although the
  # required set for each type is fixed and documented.
  required <- .observedDataRequiredFields(entry$type)
  missingFields <- required[vapply(
    required,
    function(field) .observedDataFieldMissing(entry, field),
    logical(1)
  )]
  if (length(missingFields) > 0L) {
    cli::cli_abort(
      messages$observedDataMissingFields(entryIndex, entry$type, missingFields),
      call = call
    )
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

# Persist every session-added programmatic DataSet to a PKML file next to the
# project, so it survives a reload. Called from the save path before the tree is
# written: for each `programmatic` entry whose DataSet is in the runtime store,
# write `<dataFolder>/<id>.pkml` and rewrite the section entry to a `pkml` source
# reading that file, under the same id. A programmatic entry
# with no DataSet in the store (an orphan sentinel read from disk) is left
# untouched.
#
# Returns the character vector of persisted source ids; the caller drops these
# from the runtime store only after the whole save commits, so an abort partway
# through the tree write leaves the DataSet recoverable (worst case is an orphan
# PKML file, never lost data).
#
# Its errors are attributed to `call`, so a save that aborts here reads as
# `Error in saveProject()`. The serializer checks it runs get `call` passed on:
# their default caller is the serializer's own frame, not this one.
#
# @keywords internal
# @noRd
.persistProgrammaticObservedData <- function(
  self,
  private,
  call = rlang::caller_env()
) {
  rlang::local_error_call(call)
  observedData <- private$.getSection("observedData")
  sectionIds <- names(observedData) %||% character()
  toPersist <- which(
    vapply(
      observedData,
      \(e) identical(e$type, "programmatic"),
      logical(1)
    ) &
      sectionIds %in% names(private$.programmaticDataSets)
  )
  if (length(toPersist) == 0) {
    return(character(0))
  }

  persistedIds <- unname(sectionIds[toPersist])
  dataFolder <- self$paths$dataFolder
  if (is.null(dataFolder)) {
    cli::cli_abort(messages$observedDataPersistNoDataFolder(persistedIds[[1]]))
  }

  # Rewrite the whole section in memory and validate it clean BEFORE writing any
  # PKML file, so a save that aborts (an id that is not canonical, a data file
  # another source reads, or an id another source has) never overwrites an
  # existing file: every write below is known to land on a fresh, collision-free
  # path. This upholds the all-or-nothing save contract for the on-disk data
  # files, not just the definition tree.
  fileNames <- paste0(persistedIds, ".pkml")
  for (i in seq_along(toPersist)) {
    observedData[[toPersist[[i]]]] <- list(
      id = persistedIds[[i]],
      type = "pkml",
      file = fileNames[[i]]
    )
  }
  # No write may land on a file another source reads its data from (whatever
  # that source's id). The paths are compared as `.observedDataResolvedFiles()`
  # resolves them: on the text alone, ignoring case. Two names for one file
  # through a symbolic or hard link are not detected.
  targetFiles <- .observedDataResolvedFiles(fileNames, dataFolder)
  otherFiles <- .observedDataResolvedFiles(
    .observedDataSourceFiles(observedData[-toPersist]),
    dataFolder
  )
  clashingFiles <- fileNames[targetFiles %in% otherFiles]
  if (length(clashingFiles) > 0) {
    cli::cli_abort(messages$observedDataPersistFileCollision(clashingFiles))
  }
  # The serializer's checks, run up front: every id canonical, and no two
  # entries filed under one id.
  .serializeObservedDataSet(observedData, call = call)

  # Materialize the folder (a from-scratch project may declare one that has no
  # directory on disk yet), then write each PKML now that the batch is safe.
  if (!dir.exists(dataFolder)) {
    dir.create(dataFolder, recursive = TRUE, showWarnings = FALSE)
  }
  for (i in seq_along(toPersist)) {
    filePath <- .resolveProjectPath(fileNames[[i]], dataFolder, "file")
    ospsuite::saveDataSetToPKML(
      private$.programmaticDataSets[[persistedIds[[i]]]],
      filePath
    )
  }

  private$.setSection("observedData", observedData)
  # `.setSection("observedData", ...)` already reset the names cache; the entries
  # still resolve to the same DataSet names (now from PKML), so a rebuild on the
  # next read is correct. The runtime-store entries stay until the whole save
  # commits (dropped by the caller after the tree write succeeds), so a save that
  # aborts mid-tree-write leaves the DataSet recoverable rather than lost.
  persistedIds
}
