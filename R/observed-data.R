# Observed data: project-driven loader dispatch + section CRUD.
#
# Owns project$definitions$observedDataSources end-to-end. Called by:
#   - users via loadObservedData(project) — the public dispatcher.
#   - createDataCombined() internally to resolve observed sources.
#   - validateProject() via .observedDataSourcesValidatorAdapter()
#   - users via the public addObservedDataSource / removeObservedDataSource
#     functions.
#
# The four declared source types in v2.0 Project.json are:
#   - excel  : ospsuite::loadDataSetsFromExcel via importer config
#   - pkml   : ospsuite::loadDataSetFromPKML
#   - script : R script sourced; must return DataSet or list of DataSets
#   - programmatic : DataSets added at runtime via addObservedDataSource(project).
#                    The DataSet itself is not JSON-serializable, so it
#                    is held on the Project's private slot
#                    (.programmaticDataSets), keyed by its name, and the
#                    JSON sentinel `{id: <DataSet name>, type: "programmatic"}`
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

# Earlier section names ----

# Abort on a project saved by an earlier 6.0.0 development version, which kept
# its observed-data sources under the key `observedData` and in the folder
# `<definitions>/observed-data`. Read under the current names such a project
# would show no sources at all and lose them on the next save, so the load
# stops and names what to change instead. When the current key or folder is
# there too (the project was saved again under the current names), the earlier
# one cannot simply be renamed, so the message says to move its content over.
# `definitionsDir` is `NULL` for a snapshot, which has no folder.
#
# @keywords internal
# @noRd
.abortOnEarlierObservedDataLayout <- function(
  jsonData,
  container,
  definitionsDir = NULL,
  call = rlang::caller_env()
) {
  hasEarlierKey <- "observedData" %in% names(jsonData)
  hasEarlierFolder <- !is.null(definitionsDir) &&
    dir.exists(file.path(definitionsDir, "observed-data"))
  if (!hasEarlierKey && !hasEarlierFolder) {
    return(invisible(NULL))
  }
  # "rename" when only the earlier name is there, "move" when the current one
  # is there as well, `NULL` when the earlier name is absent.
  change <- function(hasEarlier, hasCurrent) {
    if (!hasEarlier) {
      return(NULL)
    }
    if (hasCurrent) "move" else "rename"
  }
  cli::cli_abort(
    messages$observedDataEarlierLayout(
      container,
      keyChange = change(
        hasEarlierKey,
        "observedDataSources" %in% names(jsonData)
      ),
      folderChange = change(
        hasEarlierFolder,
        !is.null(definitionsDir) &&
          dir.exists(file.path(definitionsDir, "observed-data-sources"))
      ),
      definitionsFolder = basename(definitionsDir %||% "")
    ),
    call = call
  )
}

# Section validation adapter ----

#' @keywords internal
#' @noRd
.observedDataSourcesValidatorAdapter <- function(project) {
  .validateObservedData(
    project$definitions$observedDataSources,
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

#' Validate the `observedDataSources` section of a Project
#'
#' Per-entry checks: `type` is set and is one of `excel`, `pkml`,
#' `script`, `programmatic`; per-type required fields are present;
#' referenced files exist on disk (warnings only — missing files do
#' not block parsing).
#'
#' @param observedData List from `observedDataSources` definitions.
#' @param dataFolder Resolved absolute path to the project's data
#'   folder, or `NULL` when unset.
#' @return validationResult.
#' @keywords internal
#' @noRd
.validateObservedData <- function(observedData, dataFolder) {
  result <- validationResult$new()

  if (is.null(observedData) || length(observedData) == 0) {
    result$addWarning("Data", "No observed-data sources defined")
    return(result)
  }

  validTypes <- .observedDataValidTypes

  for (i in seq_along(observedData)) {
    entry <- observedData[[i]]
    entryLabel <- paste0("Observed-data source ", i)

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
#' Reads the observed-data sources from a [Project][loadProject()]
#' and returns the corresponding [`ospsuite::DataSet`] objects. Source
#' types: `excel` (via importer configuration), `pkml`, `script`, and
#' `programmatic`. A `programmatic` declaration is a sentinel for a
#' `DataSet` added at runtime with [addObservedDataSource()]; its data lives in
#' the session until you save, and [saveProject()] persists it to a PKML
#' file and rewrites the entry as a `pkml` source. A `programmatic` sentinel
#' read from disk with no matching in-session `DataSet` (a hand-authored
#' declaration, or a project opened before it was ever saved) resolves to
#' nothing, and the load warns you by name.
#'
#' @param project A `Project` object (see [loadProject()]).
#' @param sources Character vector of the ids of the observed-data sources to
#'   load, or `NULL` (default) to load every source. Only the sources you name
#'   are read, so a source you leave out cannot stop the load with a missing
#'   file or a data-set name another source also uses (and see the Security
#'   section). An id is matched as in [removeObservedDataSource()]. An id that names
#'   no source is an error listing the project's source ids. `character(0)`
#'   returns an empty list.
#' @returns A named list of [`ospsuite::DataSet`] objects: those of the
#'   `excel`, `pkml` and `script` sources in the order the project declares
#'   them, followed by the `DataSet`s added in this session with
#'   [addObservedDataSource()]. Empty list when the project has no observed-data
#'   sources.
#'
#' @section Security:
#'   A `script` observed-data source runs the R file it names, with
#'   `source()`, on your machine when the data is resolved. Any R code in
#'   that file executes, so treat a project the same way you would treat a
#'   script someone sends you: only load and resolve observed data from a
#'   project you trust. This applies to `loadObservedData()` and to anything
#'   that resolves observed data for you, such as [createDataCombined()].
#'   `loadObservedData(project, sources = )` runs only the `script` sources
#'   you select.
#'
#' @examples
#' \dontrun{
#' project <- loadProject("path/to/Project.json")
#' dataSets <- loadObservedData(project)
#' # Only the data of one source
#' dataSets <- loadObservedData(project, sources = "Aciclovir_TimeValuesData.xlsx")
#' }
#' @export
loadObservedData <- function(project, sources = NULL) {
  ospsuite.utils::validateIsOfType(project, "Project")
  project$loadObservedData(sources)
}

# Implementation behind `project$loadObservedData()` / `loadObservedData()`.
# Reaches the runtime programmatic store and the names cache through its own
# `private` (handed in by the method), not through an accessor.
#
# @keywords internal
# @noRd
.loadObservedData_impl <- function(self, private, sources = NULL, .call) {
  rlang::local_error_call(.call)
  if (!is.null(sources)) {
    .assertIdVector(sources, arg = "sources", allowEmpty = TRUE)
  }
  observedData <- private$.getSection("observedDataSources") %||% list()
  sectionIds <- names(observedData) %||% character()
  selected <- seq_along(observedData)
  programmaticDataSets <- private$.programmaticDataSets
  if (!is.null(sources)) {
    matched <- lapply(
      sources,
      .observedDataIndicesOfId,
      sectionIds = sectionIds
    )
    unknown <- unique(sources[lengths(matched) == 0L])
    if (length(unknown) > 0L) {
      cli::cli_abort(messages$observedDataUnknownSources(unknown, sectionIds))
    }
    selected <- sort(unique(unlist(matched)))
    programmaticDataSets <- programmaticDataSets[intersect(
      names(programmaticDataSets),
      sectionIds[selected]
    )]
  }
  if (length(selected) == 0L) {
    return(list())
  }
  allDataSets <- list()
  for (i in selected) {
    entry <- observedData[[i]]
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
  # Merge the DataSets this session added.
  allDataSets <- .mergeObservedDataSets(allDataSets, programmaticDataSets)
  # A `programmatic` sentinel resolves to nothing unless this session added its
  # DataSet: the data lives only in the runtime store, so a sentinel read from
  # disk (a reload, or a project opened elsewhere) has no backing DataSet and
  # would silently vanish from the result. Warn by name so the drop is loud
  # instead of surfacing later as an obscure dangling DataCombined reference.
  unresolved <- .unresolvedProgrammaticNames(
    observedData[selected],
    names(allDataSets)
  )
  if (length(unresolved) > 0) {
    # `.loadObservedData_impl()` is reentered often (from `getObservedDataNames()`
    # and on every `createDataCombined()`/`createPlots()`), so throttle to once
    # per session per project rather than firing on every call.
    frequencyId <- paste0(
      "esqlabsR_observed_data_unresolved_programmatic_",
      .projectWarningKey(self)
    )
    if (!is.null(sources)) {
      # A load of selected sources warns under a key of its own, so it never
      # uses up the warning a full load gives about every such source.
      frequencyId <- paste0(
        frequencyId,
        "_selected_",
        paste(sort(unresolved), collapse = "|")
      )
    }
    cli::cli_warn(
      messages$observedDataProgrammaticUnresolved(unresolved),
      .frequency = "once",
      .frequency_id = frequencyId
    )
  }
  # The names of a subset are not the project's names.
  if (is.null(sources)) {
    private$.observedDataNamesCache <- names(allDataSets)
  }
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
    .observedDataEntryIdOrNA,
    character(1),
    USE.NAMES = FALSE
  )
  setdiff(names[!is.na(names)], resolvedNames)
}

# Merge a batch of loaded DataSets into the accumulator, aborting on a name
# collision instead of letting `c()` silently keep both (a duplicate name would
# then shadow the earlier set when the list is indexed by name).
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
  duplicates <- intersect(names(accumulated), names(incoming))
  if (length(duplicates) > 0) {
    cli::cli_abort(messages$observedDataNameCollision(duplicates), call = call)
  }
  c(accumulated, incoming)
}

#' Get names of all observed data in a Project
#'
#' Returns the names of all DataSets that `loadObservedData(project)` would
#' return, for every source of the project. On first call this loads the data to discover
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
  if (!is.null(private$.observedDataNamesCache)) {
    return(private$.observedDataNamesCache)
  }
  # Reentrant call: pass the attribution call on rather than re-resolving it.
  .loadObservedData_impl(self, private, .call = .call)
  private$.observedDataNamesCache %||% character(0)
}

# Public CRUD: observedDataSources ----

#' Add an observed-data source to a Project
#'
#' @description
#' Adds one observed-data source. `entry` is either an [`ospsuite::DataSet`]
#' built in the session, or a list naming the file the data is read from:
#'
#' | `type` | required fields | optional fields |
#' | --- | --- | --- |
#' | `"excel"` | `file`, `importerConfiguration`, `sheets` | `id` |
#' | `"pkml"` | `file` | `id` |
#' | `"script"` | `file` | `id` |
#'
#' Paths are relative to the project's data folder: `"observed.xlsx"`, not
#' `"Data/observed.xlsx"`. A `script` source runs the R file it names (see the
#' Security section of [loadObservedData()]).
#'
#' `id` names the source: it is the name of its definition file and what
#' [removeObservedDataSource()] takes. It is lowercased and made a safe file name,
#' with a message when that changes it, unless it already names a source of
#' the project or is the `file` basename. Left out, the `file` basename is used
#' as it is. The source stores its id, so `project$definitions$observedDataSources[[id]]`
#' reads it, and a file source read that way and passed back with
#' `overwrite = TRUE` keeps it. Ids that differ only in case count as the same
#' id. A `dataCombined` entry refers to the name of a data set inside the
#' source, not to this id.
#'
#' @details
#' A `DataSet` is stored as a `type = "programmatic"` entry whose id is its
#' name, and its data stays in the R session until [saveProject()] writes it to
#' `<name>.pkml` in the data folder. Saving therefore needs a data folder. From
#' then on it is a `pkml` source that keeps its name as id.
#'
#' Adding a `DataSet` reads no source and runs no script. Its name is refused
#' at once when the project already knows it, for example from a `DataSet` of
#' that name added or saved earlier. A clash with a data set inside an Excel,
#' PKML or script source that has not been loaded yet stops the next
#' [loadObservedData()] with an error naming the duplicate. Then remove the
#' `DataSet` with [removeObservedDataSource()], rename it, and add it again.
#'
#' @param project A `Project` object.
#' @param entry An [`ospsuite::DataSet`], or a list with a `type` and that
#'   type's fields (see the table above).
#' @param overwrite Logical scalar. When `FALSE` (default), an id or `DataSet`
#'   name that already exists aborts. When `TRUE`, the existing source is
#'   replaced, but only by one of the same kind (a `DataSet` by a `DataSet`, a
#'   list by a list). To replace one kind with the other, remove the existing
#'   source with [removeObservedDataSource()] first.
#' @returns The `project` object, invisibly.
#' @export
#' @family observedData
addObservedDataSource <- function(project, entry, overwrite = FALSE) {
  ospsuite.utils::validateIsOfType(project, "Project")
  project$addObservedDataSource(entry, overwrite)
}

# Implementation behind `project$addObservedDataSource()` / `addObservedDataSource()`.
# Reaches the runtime programmatic store and names cache through `private`.
#
# @keywords internal
# @noRd
.addObservedDataSource_impl <- function(
  self,
  private,
  entry,
  overwrite = FALSE,
  .call
) {
  rlang::local_error_call(.call)
  if (inherits(entry, "DataSet")) {
    name <- entry$name
    # The DataSet name is the source's id: it names the definition file
    # (`<name>.json`) and, at save, the PKML file (`<name>.pkml`). Reject a name
    # that is not a safe filename segment now (a `/` in the name, e.g. from a
    # unit like "mg/kg", would otherwise fail later with an opaque low-level
    # path error at save).
    .validateObservedDataId(name, call = .call)
    # Check only the names known without reading a source: the names cache, when
    # a lookup has filled it, and the session's DataSets. Reading the sources
    # would run every `script` source and stop on any unreadable file. A clash
    # with a name inside a source not read yet aborts the next load instead
    # (`.mergeObservedDataSets()`).
    namesCache <- private$.observedDataNamesCache
    existingNames <- union(namesCache, names(private$.programmaticDataSets))
    # Besides the name its data resolves under (`existingNames`), a DataSet
    # collides with the id of another source, whose definition file it would
    # share; a config entry can carry an id without resolving a data set of that
    # name.
    sources <- self$definitions$observedDataSources
    sectionIds <- names(sources) %||% character()
    isProgrammatic <- vapply(
      sources %||% list(),
      \(e) identical(e$type, "programmatic"),
      logical(1)
    )
    # Saving writes `<name>.pkml` into the data folder, so a source already
    # reading its data from that file would lose it, whatever its id.
    dataFolder <- self$paths$dataFolder
    savedFile <- .observedDataResolvedFiles(paste0(name, ".pkml"), dataFolder)
    sourceFiles <- .observedDataResolvedFiles(
      .observedDataSourceFiles(sources),
      dataFolder
    )
    sameId <- .observedDataIdMatches(name, sectionIds)
    clashes <- any(
      name %in% existingNames,
      sameId,
      savedFile %in% sourceFiles
    )
    .abortObservedDataIdCaseClash(name, sectionIds[sameId], overwrite)
    if (clashes && !overwrite) {
      cli::cli_abort(c(
        "Observed-data source with name {.val {name}} already exists.",
        "i" = "Pass {.code overwrite = TRUE} to replace it."
      ))
    }
    sentinel <- list(id = name, type = "programmatic")
    # Update the in-memory section and the runtime store together: under
    # explicit-save `.setSection()` does not touch disk, so both are pure
    # in-memory mutations. Any on-disk id/basename collision is surfaced later,
    # by the serializer, when `saveProject()` reconciles the tree.
    observedData <- private$.getSection("observedDataSources")
    # On overwrite, replace the existing programmatic sentinel with this id in
    # place. Only a programmatic sentinel can be replaced by a DataSet.
    replaceIdx <- if (overwrite) {
      which(isProgrammatic & sameId)
    } else {
      integer()
    }
    replacedId <- character(0)
    if (length(replaceIdx) > 0L) {
      # The replaced source may spell its id in another case, so the checks
      # above may have matched only it. Check the name again against what the
      # other sources are known to provide: the names known without reading a
      # source (less the replaced source's own, its id), their ids, and the
      # files they read. This is best effort like the checks above: a name
      # inside a source not read yet is reported by the next load.
      replacedId <- sectionIds[[replaceIdx[[1]]]]
      others <- seq_along(sectionIds) != replaceIdx[[1]]
      clashesWithOthers <- any(
        name %in% setdiff(existingNames, replacedId),
        sameId & others,
        savedFile %in% sourceFiles[others]
      )
      if (clashesWithOthers) {
        cli::cli_abort(messages$observedDataProgrammaticOverwriteClash(
          name,
          replacedId
        ))
      }
      # The replaced source's DataSet goes.
      private$.programmaticDataSets[[replacedId]] <- NULL
      observedData[[replaceIdx[[1]]]] <- sentinel
    } else if (overwrite && clashes) {
      # The name collides with a file-based (pkml/excel/script) source, not a
      # programmatic one, so there is no programmatic entry to replace. Appending
      # would leave two sources resolving to the same name and only fail later,
      # at the next load/save. Abort now, naming the fix. This is best effort:
      # `clashes` sees only the names known without reading a source, so a
      # clash with a name inside a source not read yet is appended and reported
      # by the next load.
      cli::cli_abort(c(
        "Observed-data source with name {.val {name}} is a file-based source, \\
        not a programmatic one, so it cannot be overwritten with a {.cls DataSet}.",
        "i" = "Remove it first with {.code removeObservedDataSource()} by its \\
        {.field id}, then add the {.cls DataSet}."
      ))
    } else {
      observedData <- c(observedData, list(sentinel))
    }
    private$.setSection("observedDataSources", observedData)
    private$.programmaticDataSets[[name]] <- entry
    # The observedDataSources setter resets the names cache, so restore it after the
    # write when it was filled. A replaced programmatic source resolved to its
    # id, which may be spelled in another case than `name`, so that name leaves
    # the cache; `union()` keeps `name` once when the spelling is the same. An
    # unfilled cache stays unfilled: the next lookup reads every source.
    if (!is.null(namesCache)) {
      private$.observedDataNamesCache <- union(
        setdiff(namesCache, replacedId),
        name
      )
    }
    cli::cli_inform(messages$observedDataProgrammaticAdded(
      name,
      hasDataFolder = !is.null(self$paths$dataFolder)
    ))
    return(invisible(self))
  }

  if (is.list(entry)) {
    if (is.null(entry$type)) {
      cli::cli_abort(
        "An observed-data source must include a {.field type} field"
      )
    }
    validTypes <- c("excel", "pkml", "script")
    if (!(entry$type %in% validTypes)) {
      cli::cli_abort(c(
        "Invalid observed-data source type {.val {entry$type}}.",
        "i" = "Must be one of: {.val {validTypes}}."
      ))
    }
    # Validate the full entry shape (per-type required fields), not just the
    # type, so an under-specified config entry is rejected at add time.
    .validateObservedDataEntry(
      entry,
      length(self$definitions$observedDataSources) + 1L,
      call = .call
    )
    # A declared `id` names the entry everywhere (its definition file, and the
    # key `removeObservedDataSource()` matches), so it has to be a usable string
    # before anything derives a filename from it.
    if (
      !is.null(entry[["id"]]) && is.null(.usableObservedDataId(entry[["id"]]))
    ) {
      cli::cli_abort(
        "An observed-data source's {.field id} must be a single non-empty string."
      )
    }
    # A declared id names a definition file, so it runs through the same
    # canonicalizer every other section's `add*` runs its id through: two ids
    # differing only in case would otherwise be two declarations mapping to one
    # file on a case-insensitive filesystem. An id derived from the `file`
    # basename is the source's own string, which `.validateObservedDataId()`
    # rejects rather than reshapes. Every declaration stores its id, so an `id`
    # that is exactly the id of an existing source (a declaration read from the
    # project and passed back) or equal to the derived one is not one the user
    # typed, and is kept as is.
    existingIds <- names(self$definitions$observedDataSources) %||% character()
    derivedId <- .observedDataEntryIdOrNA(entry[names(entry) != "id"])
    typedId <- !is.null(entry[["id"]]) &&
      !(entry[["id"]] %in% c(existingIds, derivedId))
    if (typedId) {
      entry[["id"]] <- .canonicalizeId(entry[["id"]])
    }
    # Config entries are keyed by the id `removeObservedDataSource()` matches on: the
    # entry's own `id` when it declares one, else its `file` basename. That id
    # also names the definition file, so reject an unsafe one here rather than
    # at save. Abort on a duplicate to match the other mutators' convention,
    # unless overwriting.
    entryId <- .observedDataEntryId(entry, call = .call)
    .validateObservedDataId(entryId, call = .call)
    sameId <- .observedDataIdMatches(entryId, existingIds)
    .abortObservedDataIdCaseClash(entryId, existingIds[sameId], overwrite)
    if (any(sameId) && !overwrite) {
      cli::cli_abort(c(
        "Observed-data source with id {.val {entryId}} already exists.",
        "i" = "Pass {.code overwrite = TRUE} to replace it."
      ))
    }
    # The data-set names inside a config source are known only by reading it, so
    # neither the names it adds nor those of a source it replaces can be patched
    # into the cache: leave it unset, and the next lookup reads every source.
    private$.observedDataNamesCache <- NULL
    observedData <- private$.getSection("observedDataSources")
    # On overwrite, replace the existing entry carrying this id in place;
    # otherwise append (a non-overwrite id clash aborted above).
    replaceIdx <- if (overwrite) which(sameId) else integer()
    if (
      length(replaceIdx) > 0L &&
        identical(observedData[[replaceIdx[[1]]]]$type, "programmatic")
    ) {
      # Replacing a programmatic sentinel in place would strand its `DataSet` in
      # the runtime store: nothing would write it at save and nothing would warn
      # that it vanished. Refuse, as the mirror case (a `DataSet` overwriting a
      # file-based source) already does.
      cli::cli_abort(c(
        "Observed-data source with id {.val {entryId}} is a programmatic source \\
        holding a {.cls DataSet} in this session, so it cannot be overwritten \\
        with a configuration entry.",
        "i" = "Remove it first with {.code removeObservedDataSource()}, then add the \\
        configuration entry."
      ))
    }
    if (length(replaceIdx) > 0L) {
      observedData[[replaceIdx[[1]]]] <- entry
    } else {
      observedData <- c(observedData, list(entry))
    }
    private$.setSection("observedDataSources", observedData)
    return(invisible(self))
  }

  cli::cli_abort(
    "An observed-data source must be a {.cls DataSet} or a configuration list"
  )
}

#' Remove one or more observed-data sources from a Project
#'
#' Removes by the source's id, the `id` field of each entry in
#' `project$definitions$observedDataSources` (also the names of that list). Accepts a
#' vector of ids and removes them all at once; [saveProject()] then deletes
#' their definition files. Warns and skips any id with no matching entry.
#'
#' A `DataSet` added with [addObservedDataSource()] has its name as its id, before
#' and after [saveProject()] writes it to `<name>.pkml`.
#'
#' Unlike the other authoring functions, `addObservedDataSource()` is not
#' vectorized over ids: its second argument is a `DataSet` or a configuration
#' list, not an id, so it adds a single source per call. Add several sources
#' with several calls.
#'
#' @param project A `Project` object.
#' @param id Character vector of ids. An observed-data id is the `id` you gave
#'   the source, or, when you gave none, comes from the data source itself (the
#'   name of a `DataSet` added with [addObservedDataSource()], or the file basename
#'   for a file-based source). Each is matched as the declaration stores it and,
#'   failing that, as [addObservedDataSource()] would have canonicalized it, so an id
#'   taken from the source is found by the source's own spelling and one you
#'   gave by either the form you authored or the canonical form you were given
#'   back.
#' @returns The `project` object, invisibly.
#' @export
#' @family observedData
removeObservedDataSource <- function(project, id) {
  ospsuite.utils::validateIsOfType(project, "Project")
  project$removeObservedDataSource(id)
}

# Implementation behind `project$removeObservedDataSource()` / `removeObservedDataSource()`.
#
# @keywords internal
# @noRd
.removeObservedDataSource_impl <- function(self, private, id, .call) {
  rlang::local_error_call(.call)
  .assertIdVector(id)
  observedData <- private$.getSection("observedDataSources")

  # Resolve every id to the section index it removes (a programmatic sentinel or
  # a file-based entry) before touching anything, so the whole batch is
  # validated first and applied in a single in-memory update, matching the
  # all-or-nothing invariant every other vectorized remove* upholds. Nothing
  # touches disk; persist with saveProject().
  dropIdx <- integer()
  missingIds <- character()
  sectionIds <- names(observedData) %||% character()
  for (one in id) {
    matchIdx <- .observedDataIndicesOfId(one, sectionIds)
    if (length(matchIdx) == 0L) {
      missingIds <- c(missingIds, one)
      next
    }
    dropIdx <- c(dropIdx, matchIdx)
  }

  if (length(missingIds) > 0L) {
    cli::cli_warn("observed-data source {.val {missingIds}} not found; no-op.")
  }
  # Warn once per removed id that is still referenced, then drop everything in a
  # single in-memory update. A not-found id contributes nothing to the update.
  for (one in setdiff(id, missingIds)) {
    .warnIfObservedDataReferenced(self, one)
  }
  if (length(dropIdx) == 0L) {
    return(invisible(self))
  }

  dropIdx <- unique(dropIdx)
  # A removed source takes its DataSet out of the session (keyed by the DataSet
  # name, which is the source's id). That includes a source a failed save has
  # already rewritten as `pkml` while its DataSet is still held.
  programmaticNames <- intersect(
    sectionIds[dropIdx],
    names(private$.programmaticDataSets)
  )
  # Update the in-memory section and the runtime store together (both pure
  # in-memory mutations under explicit-save; nothing touches disk until
  # `saveProject()`).
  private$.setSection("observedDataSources", observedData[-dropIdx])
  for (name in programmaticNames) {
    private$.programmaticDataSets[[name]] <- NULL
  }
  private$.observedDataNamesCache <- NULL
  invisible(self)
}

# Warn when a removed observed-data source name is still referenced as a
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
      "Removed observed-data source {.val {name}} is still referenced by {length(holders)} dataCombined entr{?y/ies}:",
      "*" = "{holders}",
      "i" = "These dataCombined entries now have a dangling reference. Update or remove them."
    ))
  }
  invisible(NULL)
}

# Internal helpers ----

# The data-set names a cross-reference can resolve against, or `NULL` when the
# project cannot be asked.
#
# A declaration's id names the declaration; what a parameter-identification
# output mapping references is the name of a data set INSIDE the source, which
# only the source knows, so answering means loading it. Reading an Excel or PKML
# source is the same kind of filesystem access the section validators already do,
# but a `script` source is arbitrary R and validating a project must never run
# it: a section holding one returns `NULL`, and so does a load that fails, since
# an unreadable source is the observedDataSources section's own finding rather than a
# dangling reference. `NULL` therefore means "unresolved", not "nothing defined".
#
# @keywords internal
# @noRd
.observedDataNamesForCrossReference <- function(project) {
  declarations <- project$definitions$observedDataSources %||% list()
  if (length(declarations) == 0L) {
    return(character(0))
  }
  types <- vapply(
    declarations,
    function(e) as.character(e$type %||% NA_character_),
    character(1)
  )
  if (any(types == "script", na.rm = TRUE)) {
    return(NULL)
  }
  tryCatch(
    suppressMessages(suppressWarnings(project$getObservedDataNames())),
    error = function(cnd) NULL
  )
}

# The positions in the section of the source(s) the user means by `id`, or
# `integer(0)` when it names none. Removing and loading a source both find it
# here.
#
# A lookup, unlike the add-time and save-time checks
# (`.observedDataIdMatches()`): those guard definition file names, which ignore
# case on Windows and macOS, while a lookup finds the one source the user names,
# so it matches the id as stored.
#
# An id this section stores is one of two kinds, so the match takes two passes
# rather than one transform. A derived id is the source's own string (a `file`
# basename, a `DataSet` name), which only the verbatim pass finds; a declared id
# is stored the way `addObservedDataSource()` canonicalized it, so the id as the user
# authored it needs the same transform to reach it. Verbatim first, so a derived
# id that another entry's declared id happens to canonicalize to still wins its
# own match.
#
# @keywords internal
# @noRd
.observedDataIndicesOfId <- function(id, sectionIds) {
  matchIdx <- which(sectionIds %in% id)
  if (length(matchIdx) == 0L) {
    matchIdx <- which(
      sectionIds %in% .silentlyCanonicalized(.canonicalizeId(id))
    )
  }
  matchIdx
}

# Which of `sectionIds` name the same definition file as `id`. Ids that differ
# only in case are one file on Windows and macOS, so they are the same id.
# Adding and saving a source both use this, so an id refused at save is already
# refused where the user adds it.
#
# @keywords internal
# @noRd
.observedDataIdMatches <- function(id, sectionIds) {
  !is.na(sectionIds) & tolower(sectionIds) == tolower(id)
}

# Without `overwrite`, refuse an id whose only match among the section's ids
# (`matched`) is spelled in another case, naming both spellings.
#
# @keywords internal
# @noRd
.abortObservedDataIdCaseClash <- function(
  id,
  matched,
  overwrite,
  call = rlang::caller_env()
) {
  if (overwrite || length(matched) == 0L || id %in% matched) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    messages$observedDataIdCaseClash(id, matched[[1]]),
    call = call
  )
}

# The `file` of every entry in the section as written (relative to the data
# folder, or absolute), `NA` for one that names none (a programmatic entry).
#
# @keywords internal
# @noRd
.observedDataSourceFiles <- function(observedData) {
  vapply(
    observedData %||% list(),
    \(e) .usableObservedDataId(e[["file"]]) %||% NA_character_,
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
# write `<dataFolder>/<id>.pkml` and rewrite the section entry to a `pkml` source
# reading that file, under the same id (the DataSet name). A programmatic entry
# with no DataSet in the store (an orphan sentinel read from disk) is left
# untouched.
#
# Returns the character vector of persisted DataSet names; the caller drops these
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
  observedData <- private$.getSection("observedDataSources")
  sectionIds <- names(observedData) %||% character()
  # A hand-authored `programmatic` sentinel may carry no id; it has no backing
  # DataSet, so it is left untouched, matching this function's orphan-sentinel
  # contract.
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

  persistedNames <- unname(sectionIds[toPersist])
  dataFolder <- self$paths$dataFolder
  if (is.null(dataFolder)) {
    cli::cli_abort(messages$observedDataPersistNoDataFolder(
      persistedNames[[1]]
    ))
  }

  # Rewrite the whole section in memory and validate it clean BEFORE writing any
  # PKML file, so a save that aborts (an unsafe name, a data file another source
  # reads, or an id another source has) never overwrites an existing file: every
  # write below is known to land on a fresh, collision-free path. This upholds
  # the all-or-nothing save contract for the on-disk data files, not just the
  # definition tree.
  fileNames <- paste0(persistedNames, ".pkml")
  for (i in seq_along(toPersist)) {
    observedData[[toPersist[[i]]]] <- list(
      id = persistedNames[[i]],
      type = "pkml",
      file = fileNames[[i]]
    )
  }
  # No write may land on a file another source reads its data from (whatever
  # that source's id), or on a file another write in this batch lands on. The
  # paths are compared as `.observedDataResolvedFiles()` resolves them: on the
  # text alone, ignoring case. Two names for one file through a symbolic or
  # hard link are not detected.
  targetFiles <- .observedDataResolvedFiles(fileNames, dataFolder)
  otherFiles <- .observedDataResolvedFiles(
    .observedDataSourceFiles(observedData[-toPersist]),
    dataFolder
  )
  readByOther <- targetFiles %in% otherFiles
  sharedInBatch <- duplicated(targetFiles) |
    duplicated(targetFiles, fromLast = TRUE)
  clashingFiles <- fileNames[readByOther | sharedInBatch]
  if (length(clashingFiles) > 0) {
    cli::cli_abort(messages$observedDataPersistFileCollision(clashingFiles))
  }
  # The serializer's checks, run up front: every id a safe filename segment, and
  # no two entries filed under one id.
  .serializeObservedDataSet(observedData, call = call)

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

  private$.setSection("observedDataSources", observedData)
  # `.setSection("observedDataSources", ...)` already reset the names cache; the entries
  # still resolve to the same DataSet names (now from PKML), so a rebuild on the
  # next read is correct. The runtime-store entries stay until the whole save
  # commits (dropped by the caller after the tree write succeeds), so a save that
  # aborts mid-tree-write leaves the DataSet recoverable rather than lost.
  persistedNames
}
