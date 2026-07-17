# Definition-files format layer ----
#
# Every authored Project section is stored as a tree of JSON files under
# `definitions/<kind>/`, one subfolder per definition kind beneath the project's
# `definitions/` root (next to `Project.json`), rather than as an array or
# object inside the monolithic `Project.json`. The `definitions/` folder holds
# the project's authored definition files, separated from the referenced working
# files (`Models/`, `Data/`, `Populations/`, `Results/`) and the `Project.json`
# container. Each file holds the same per-definition JSON the monolithic
# serializer emitted, so the in-memory section shapes are unchanged and every
# consumer (`runScenarios()`, `createPlots()`, validation, parameter
# identification) is unaffected.
#
# Direction of truth: for a loaded project, memory is authoritative until the
# user saves. Loading globs each kind's tree into memory; the section mutators
# (every `add*` / `remove*` / `set*` and any write-back through
# `project$<section>`) update memory and set the dirty bit, and `saveProject()`
# reconciles the whole tree to memory (write-if-different plus orphan-delete).
# `snapshotProject()` renders a derived single-file `.esqlabsR` with every
# section inlined, for sharing or archiving; `restoreProject()` materializes
# one back into a tree.
#
# Per-kind subfolder names and granularity (all under `definitions/`):
#   scenarios                -> scenarios/                one file per scenario
#   individuals              -> individuals/              one file per individual
#   populations              -> populations/              one file per population
#   parameterSets            -> parameter-sets/           one file per set
#   initialConditions        -> initial-conditions/       one file per set
#   applications             -> applications/             one file per application
#   outputPaths              -> output-paths/             one file per path
#   observedData             -> observed-data/            one file per declaration
#   dataCombined             -> data-combined/            one file per entry
#   plots                    -> plots/                    one file per plot
#   plotGrids                -> plot-grids/               one file per grid
#   parameterIdentification  -> parameter-identification/ one file per task
#
# A section keyed by an id is one file per id (the id is the filename). The
# unnamed `observedData` list is keyed by the id `removeObservedData()` matches
# on (file basename, or the programmatic DataSet name).
#
# The plots concern is three independent top-level keyed sections, each its own
# kind: `dataCombined` (`data-combined/<dataCombinedId>.json`), `plots`
# (`plots/<plotId>.json`, the plot list keyed by `plotId`), and `plotGrids`
# (`plot-grids/<plotGridId>.json`), one file per definition like every other
# section. The inner cross-references (a grid's `plotIds` -> a plot's `plotId`
# -> a `dataCombinedId`) are validated lazily by `.validatePlots()`, not at
# write, so a dangling inner ref stays a Critical Error surfaced at
# `validateProject()` / `createPlots()`, never a write-time abort. The derived
# single-file snapshot inlines all three as three top-level JSON sections.

# Resolve the definitions root for a project directory. This is the
# folder that holds the project's authored definition files, separated from the
# referenced working files (`Models/`, `Data/`, `Populations/`, `Results/`)
# and the `Project.json` container. Its name is configurable via the
# container's `definitionsFolder` field (default `"definitions"`), passed in by
# the caller (a project's `definitionsFolder` binding); the default keeps every
# existing project working. Each definition kind lives in its own subfolder
# (`<definitionsFolder>/<kind>/`); see `.definitionKindDir()`. Returns NULL when the
# project has no directory (an in-memory project), in which case no definition tree
# is persisted.
#
# @keywords internal
# @noRd
.definitionsDir <- function(projectDirPath, definitionsFolder = "definitions") {
  if (is.null(projectDirPath) || length(projectDirPath) == 0L) {
    return(NULL)
  }
  file.path(projectDirPath, definitionsFolder %||% "definitions")
}

# Resolve the definition directory for one kind (`scenarios`, `individuals`,
# `populations`, etc.) under the project's definitions root. This single
# per-kind resolver is the one place a kind's on-disk location is computed, so
# a future kind slots in by calling it with its own name rather than
# re-deriving a path. The definitions root name is taken from
# `definitionsFolder` (default `"definitions"`). Returns NULL for an in-memory
# project (no directory).
#
# @keywords internal
# @noRd
.definitionKindDir <- function(
  projectDirPath,
  kind,
  definitionsFolder = "definitions"
) {
  root <- .definitionsDir(projectDirPath, definitionsFolder)
  if (is.null(root)) {
    return(NULL)
  }
  file.path(root, kind)
}

# Per-kind definition-tree spec registry ----
#
# Each kind has a spec describing how its section is read from and written to
# the tree. Every kind is one-file-per-definition, keyed by id (the id is the
# filename). A spec is a list with:
#   - kind        : the `definitions/<kind>/` subfolder name.
#   - serialize   : function(section, project) -> a named `id -> json-record`
#                   list. Performs all structural validation, so any abort
#                   happens before a file is touched
#                   (serialize-all-before-write-any).
#   - parse       : function(records, project) -> the in-memory section, given
#                   `records`, the list of per-file JSON records.
#   - inline      : function(jsonData) -> the inline section as it appears in a
#                   single-file `Project.json` snapshot, used as the fallback
#                   when no tree directory exists. Shaped exactly like what the
#                   keyed `parse` consumes.
#
# The scenarios kind has its own dedicated serialize/parse helpers (the file
# content differs structurally from the in-memory record); the others reuse
# their section's existing per-definition JSON shape. The plots concern is three
# independent top-level keyed sections (`dataCombined`, `plots`, `plotGrids`),
# each its own kind like any other section.
#
# @keywords internal
# @noRd
.definitionTreeSpec <- function(kind) {
  specs <- .definitionTreeSpecs()
  spec <- specs[[kind]]
  if (is.null(spec)) {
    cli::cli_abort("No definition-tree spec for kind {.val {kind}}.")
  }
  spec
}

# All definition-tree specs, keyed by section field name.
#
# @keywords internal
# @noRd
.definitionTreeSpecs <- function() {
  list(
    scenarios = list(
      kind = "scenarios",
      serialize = function(section, project) {
        .serializeScenarioSet(section)
      },
      parse = function(records, project) {
        .parseScenarios(records, project$outputPaths)
      },
      inline = function(jsonData) jsonData$scenarios
    ),
    individuals = list(
      kind = "individuals",
      serialize = function(section, project) {
        .serializeIndividualSet(section)
      },
      parse = function(records, project) .parseIndividuals(records),
      inline = function(jsonData) jsonData$individuals
    ),
    populations = list(
      kind = "populations",
      serialize = function(section, project) {
        .serializePopulationSet(section)
      },
      parse = function(records, project) .parsePopulations(records),
      inline = function(jsonData) jsonData$populations
    ),
    parameterSets = list(
      kind = "parameter-sets",
      serialize = function(section, project) {
        .serializeParameterSetSet(section)
      },
      parse = function(records, project) .parseParameterSetTree(records),
      # The inline fallback merges any legacy three-section `Project.json` into
      # the one `parameterSets` map (a clash aborts), then re-expresses it as
      # the per-record list shape the tree parser consumes. A genuinely absent
      # section stays `NULL` (parsed to a bare `list()`); a present empty `{}`
      # becomes an empty record list (parsed to a named-empty list), preserving
      # the absent-vs-empty distinction the monolithic parser kept.
      inline = function(jsonData) {
        .mapSectionToRecords(
          .mergeParameterSetSectionsOrNull(jsonData),
          .parameterSetMapToRecords
        )
      }
    ),
    initialConditions = list(
      kind = "initial-conditions",
      serialize = function(section, project) {
        .serializeInitialConditionSet(section)
      },
      parse = function(records, project) .parseInitialConditionTree(records),
      # Absent (`NULL`) stays `NULL` so the parser yields a bare `list()`; a
      # present empty `{}` becomes an empty record list so the parser yields a
      # named-empty list. This preserves the absent-vs-empty distinction the
      # monolithic parser kept for the map-shaped sections.
      inline = function(jsonData) {
        .mapSectionToRecords(
          jsonData$initialConditions,
          .initialConditionMapToRecords
        )
      }
    ),
    applications = list(
      kind = "applications",
      serialize = function(section, project) {
        .serializeApplicationSet(section)
      },
      parse = function(records, project) .parseApplicationTree(records),
      inline = function(jsonData) {
        .applicationMapToRecords(jsonData$applications)
      }
    ),
    outputPaths = list(
      kind = "output-paths",
      serialize = function(section, project) {
        .serializeOutputPathSet(section)
      },
      parse = function(records, project) .parseOutputPathTree(records),
      # Absent (`NULL`) stays `NULL` so the parser yields a bare `list()`; a
      # present empty `{}` becomes an empty record list so the parser yields a
      # named-empty list. This preserves the absent-vs-empty distinction the
      # monolithic parser kept for the map-shaped sections.
      inline = function(jsonData) {
        .mapSectionToRecords(jsonData$outputPaths, .outputPathMapToRecords)
      }
    ),
    observedData = list(
      kind = "observed-data",
      serialize = function(section, project) {
        .serializeObservedDataSet(section)
      },
      parse = function(records, project) .parseObservedDataTree(records),
      inline = function(jsonData) jsonData$observedData
    ),
    # The plots concern is three independent top-level keyed sections, each its
    # own `definitions/<kind>/` tree: `dataCombined` (folder `data-combined`),
    # `plots` (folder `plots`, the plot list keyed by `plotId`), and `plotGrids`
    # (folder `plot-grids`). Each `serialize` turns its keyed list into an
    # `id -> json-record` map; each `parse` rebuilds the keyed list from the
    # per-file records; each `inline` falls back to its own top-level snapshot
    # section so a single-file snapshot reloads each self-contained.
    dataCombined = list(
      kind = "data-combined",
      serialize = function(section, project) {
        .serializeDataCombinedSet(section)
      },
      parse = function(records, project) .parseDataCombinedTree(records),
      inline = function(jsonData) jsonData$dataCombined
    ),
    plots = list(
      kind = "plots",
      serialize = function(section, project) {
        .serializePlotConfigurationSet(section)
      },
      parse = function(records, project) .parsePlotConfigurationTree(records),
      inline = function(jsonData) jsonData$plots
    ),
    plotGrids = list(
      kind = "plot-grids",
      serialize = function(section, project) {
        .serializePlotGridSet(section)
      },
      parse = function(records, project) .parsePlotGridTree(records),
      inline = function(jsonData) jsonData$plotGrids
    ),
    parameterIdentification = list(
      kind = "parameter-identification",
      serialize = function(section, project) {
        .serializePITaskSet(section)
      },
      parse = function(records, project) .parsePITasks(records),
      inline = function(jsonData) jsonData$parameterIdentification
    )
  )
}

# The list of all tree-backed spec names, each a one-to-one `project$<name>`
# section field. This is the single source of truth for the set of section
# kinds: it is derived from the keys of `.definitionTreeSpecs()`, and the project
# object's section membership check reads it too, so adding or renaming a kind
# is a one-place edit (the spec) rather than three lists kept in lockstep. Used
# by the whole-tree writers, where write order does not matter (each kind writes
# into its own folder independently); the load order that must resolve
# `outputPaths` before scenarios is fixed separately in `Project$.read_json()`.
#
# @keywords internal
# @noRd
.definitionKindNames <- function() {
  names(.definitionTreeSpecs())
}

# Resolve the in-memory section a tree-spec name serializes: the
# `project$<name>` field. Used by the whole-tree writer (`.writeProjectTree`)
# so it can iterate `.definitionKindNames()` uniformly. Strips the printable
# DefinitionList wrapper a section accessor adds on read, so the whole-tree
# writers operate on plain lists.
#
# @keywords internal
# @noRd
.sectionForKind <- function(project, name) {
  .unwrapDefinitionList(project[[name]])
}

# Path to a single definition's file. The filename is `<id>.json`. Callers pass an
# id already validated as a safe single path segment (canonicalized by the
# authoring API and re-checked by the per-section structural validator), so
# the id is used verbatim here.
#
# @keywords internal
# @noRd
.definitionFilePath <- function(dir, id) {
  file.path(dir, paste0(id, ".json"))
}

# Canonical JSON write options shared by every definition file and the snapshot,
# for byte-stable round-trips.
#
# Write-if-different: serialize the content to a string first and, when the
# target file already holds byte-identical content, skip the write entirely.
# This keeps a `saveProject()` from bumping the mtime of an entity the user did
# not change, so `git diff` (and any file-watcher) sees exactly the entities
# that actually changed, which is a core reason the entity-files format exists.
#
# @keywords internal
# @noRd
.writeDefinitionJson <- function(content, path) {
  serialized <- jsonlite::toJSON(
    content,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE,
    digits = NA
  )
  if (file.exists(path)) {
    current <- paste0(
      readLines(path, warn = FALSE, encoding = "UTF-8"),
      collapse = "\n"
    )
    if (identical(as.character(serialized), current)) {
      return(invisible(NULL))
    }
  }
  writeLines(as.character(serialized), path, useBytes = TRUE)
  invisible(NULL)
}

# Load one kind's section into the raw shape its parser consumes.
#
# A tree-format project keeps the section as `definitions/<kind>/*.json`; those
# files are globbed, parsed, and returned as the list of per-file records in
# sorted-filename order for a stable load order. When there is no
# `definitions/<kind>/` directory, the inline section from the `Project.json` is
# used instead, so a derived single-file snapshot (see `snapshotProject()`)
# reloads self-contained. A malformed file is a structural error and aborts the
# load.
#
# @keywords internal
# @noRd
.loadDefinitionTree <- function(
  projectDirPath,
  kind,
  inlineSection = NULL,
  definitionsFolder = "definitions"
) {
  spec <- .definitionTreeSpec(kind)
  # The definitions root and the per-kind subfolder must each be a real
  # directory when present. A path that exists but is a regular file is a
  # corrupted or mis-synced tree, not an absent section, so it must abort rather
  # than silently fall back to the inline section (which would load the project
  # as structurally-valid but empty).
  root <- definitionsFolder %||% "definitions"
  .assertDefinitionTreePathIsDir(
    .definitionsDir(projectDirPath, root),
    root
  )
  dir <- .definitionKindDir(projectDirPath, spec$kind, root)
  .assertDefinitionTreePathIsDir(dir, file.path(root, spec$kind))

  if (is.null(dir) || !dir.exists(dir)) {
    # `inlineSection` is `NULL` for a genuinely absent section and an empty
    # (possibly named) list for a present-but-empty one; the kind's parser
    # turns each into the right empty shape, so pass it through verbatim.
    return(inlineSection)
  }
  # `list.files()` returns paths in the native (unknown) encoding; a non-ASCII
  # filename then makes `sort(method = "radix")` reject the vector on platforms
  # whose native encoding is not UTF-8 (Windows, a non-UTF-8 locale), making
  # the whole project unloadable. Normalize to UTF-8 first so the sort is
  # encoding-stable and a non-ASCII id round-trips everywhere.
  files <- sort(
    enc2utf8(list.files(dir, pattern = "\\.json$", full.names = TRUE)),
    method = "radix"
  )
  # Tag each record with the file it came from so the keyed parsers can name the
  # offending file in a load error and check the inner id against the filename
  # stem. An inline-snapshot fallback record carries no such tag.
  lapply(files, function(f) {
    rec <- .readDefinitionJsonFile(f)
    attr(rec, ".definitionFile") <- f
    rec
  })
}

# Abort when a definition-tree path exists on disk but is a regular file rather
# than a directory. `dir.exists()` is FALSE for both an absent path and a path
# that is a file, so the caller cannot tell a genuinely-missing section (a
# legitimate inline fallback) from a corrupted tree. A NULL path (in-memory
# project) or a genuinely-absent path is fine and returns silently; only an
# existing non-directory aborts, naming the path and the expected `relLabel`.
#
# @keywords internal
# @noRd
.assertDefinitionTreePathIsDir <- function(path, relLabel) {
  if (is.null(path) || !file.exists(path) || dir.exists(path)) {
    return(invisible(NULL))
  }
  cli::cli_abort(c(
    "Project definition path {.file {path}} exists but is not a directory.",
    "x" = "{.file {relLabel}} must be a directory of definition files.",
    "i" = "A regular file here is a corrupted or mis-synced project tree."
  ))
}

# Parse one decoded definition file, aborting with a clear message on malformed
# JSON.
#
# @keywords internal
# @noRd
.readDefinitionJsonFile <- function(f) {
  tryCatch(
    jsonlite::fromJSON(f, simplifyVector = FALSE),
    error = function(e) {
      cli::cli_abort(
        "Failed to parse definition file {.file {f}} as JSON.",
        parent = e
      )
    }
  )
}

# Write a whole section's tree to `definitions/<kind>/`, one file per definition.
#
# Stale-file policy (full-tree reconcile): this writer OWNS the `<kind>/`
# directory. `section` is the authoritative complete set for the kind, so any
# `.json` file the directory holds that is NOT in the freshly-written keep-set
# is a stale entry (a removed definition, or an orphan a hand-edit or a prior run
# dropped in) and is deleted. After a write the directory mirrors the in-memory
# section exactly. This is the "make disk look like my project now" contract
# `saveProject()` and `restoreProject()` both rely on: because they hold the
# whole section in memory, they can and must reconcile the directory to it.
# Each definition file is written write-if-different (see
# `.writeDefinitionJson()`), so an unchanged definition's file (and mtime) is
# left untouched.
#
# A NULL directory (in-memory project) is a silent no-op. The full set is
# serialized in memory first, so a serializer-hostile definition aborts before any
# file is touched.
#
# @keywords internal
# @noRd
.writeDefinitionTree <- function(section, kind, project, projectDirPath) {
  spec <- .definitionTreeSpec(kind)
  dir <- .definitionKindDir(
    projectDirPath,
    spec$kind,
    project$definitionsFolder
  )
  if (is.null(dir)) {
    return(invisible(NULL))
  }
  serialized <- spec$serialize(section, project)

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  keep <- character()
  for (id in names(serialized)) {
    .writeDefinitionJson(serialized[[id]], .definitionFilePath(dir, id))
    keep <- c(keep, paste0(id, ".json"))
  }
  existing <- list.files(dir, pattern = "\\.json$")
  # Delete stale files, checking each `file.remove()` result: a stale entry
  # that fails to delete (a permission or lock problem) would silently survive
  # and re-enter the section on the next `loadProject()`, so the section would
  # not match the set just written. Abort naming the file(s) rather than let a
  # ghost definition reappear.
  stale <- setdiff(existing, keep)
  if (length(stale) > 0L) {
    removed <- file.remove(file.path(dir, stale))
    if (!all(removed)) {
      cli::cli_abort(messages$failedToRemoveStaleDefinitionFiles(
        file.path(dir, stale[!removed])
      ))
    }
  }
  invisible(NULL)
}

# Scenario tree helpers ----
#
# Scenarios keep dedicated serialize helpers because the on-disk file content
# differs structurally from the in-memory `Scenario` record (the named-vector
# `outputPaths` record field rebuilt as the `outputPaths` id array, parsed
# `simulationTime` rejoined, base-unit `steadyStateTime` converted back). The
# other kinds' file content is their existing per-definition JSON shape.

# Serialize a whole set of scenarios to their JSON-list representation, keyed
# by scenario name, without touching disk. Reuses the per-scenario structural
# validator and serializer so any abort happens here, before any file is
# written. The structural validator also enforces that each key is a canonical
# (lowercase, safe) id, so two keys can never collide on a case-insensitive
# filesystem.
#
# @keywords internal
# @noRd
.serializeScenarioSet <- function(scenarios) {
  scenarios <- scenarios %||% list()
  serialized <- list()
  for (name in names(scenarios)) {
    .validateScenarioStructure(scenarios[[name]], name)
    serialized[[name]] <- .scenarioToJson(scenarios[[name]])
  }
  serialized
}

# Per-kind serialize/parse for the non-scenario sections ----
#
# Each `serialize` produces a named `id -> json-record` map (structurally
# validating along the way); each `parse` reassembles the in-memory section
# from the list of per-file records.

# individuals: one file per individual, `{individualId, ...fields}`.
#
# @keywords internal
# @noRd
.serializeIndividualSet <- function(individuals) {
  individuals <- individuals %||% list()
  out <- list()
  for (id in names(individuals)) {
    .validateDefinitionTreeKey(id, "individual")
    indiv <- unclass(individuals[[id]])
    if (!is.null(indiv$parameterSets)) {
      indiv$parameterSets <- as.list(indiv$parameterSets)
    }
    out[[id]] <- c(list(individualId = id), indiv)
  }
  out
}

# populations: one file per population, `{populationId, ...fields}`.
#
# @keywords internal
# @noRd
.serializePopulationSet <- function(populations) {
  populations <- populations %||% list()
  out <- list()
  for (id in names(populations)) {
    .validateDefinitionTreeKey(id, "population")
    pop <- unclass(populations[[id]])
    out[[id]] <- c(list(populationId = id), pop)
  }
  out
}

# parameterSets: one file per set, `{id, parameters: [entries]}`.
#
# @keywords internal
# @noRd
.serializeParameterSetSet <- function(parameterSets) {
  parameterSets <- parameterSets %||% list()
  out <- list()
  for (id in names(parameterSets)) {
    .validateDefinitionTreeKey(id, "parameterSet")
    out[[id]] <- list(
      id = id,
      parameters = unclass(parameterSets[[id]] %||% list())
    )
  }
  out
}

# @keywords internal
# @noRd
.parseParameterSetTree <- function(records) {
  if (is.null(records)) {
    return(list())
  }
  out <- structure(list(), names = character(0L))
  for (rec in records) {
    id <- .keyedTreeRecordId(rec, "id", "parameterSet")
    .assertNoEmptyObjectFields(rec, "parameterSet")
    out[[id]] <- .asParameterSet(rec$parameters %||% list())
  }
  out
}

# Stamp a parameter set's array-of-entries with `c("ParameterSet", "list")` so
# a single set read from `project$parameterSets[[id]]` dispatches its print
# method. The class is a transparent list wrapper; the serializers strip it
# (`.serializeParameterSetSet` / `.parameterSetsToJson`) so it never reaches
# JSON.
#
# @keywords internal
# @noRd
.asParameterSet <- function(parameters) {
  parameters <- parameters %||% list()
  class(parameters) <- c("ParameterSet", "list")
  parameters
}

# Convert an inline map section to the per-record list the tree parser
# consumes, preserving the absent-vs-empty distinction the monolithic parser
# kept for the map-shaped sections: a genuinely absent section (`NULL`) stays
# `NULL` (its parser yields a bare `list()`), while a present empty `{}` becomes
# an empty record `list()` (its parser yields a named-empty list). `toRecords`
# is the kind's map -> record-list helper.
#
# @keywords internal
# @noRd
.mapSectionToRecords <- function(mapSection, toRecords) {
  if (is.null(mapSection)) {
    return(NULL)
  }
  toRecords(mapSection)
}

# Merge the legacy/unified parameter-set sections, but preserve a genuinely
# absent `parameterSets` (no section and no legacy sections) as `NULL` so the
# inline fallback yields a bare `list()`, not a named-empty list.
#
# @keywords internal
# @noRd
.mergeParameterSetSectionsOrNull <- function(jsonData) {
  hasLegacy <- any(
    vapply(
      list(
        jsonData$modelParameterSets,
        jsonData$individualParameterSets,
        jsonData$applicationParameterSets
      ),
      function(x) length(x) > 0L,
      logical(1)
    )
  )
  if (is.null(jsonData$parameterSets) && !hasLegacy) {
    return(NULL)
  }
  .mergeParameterSetSections(jsonData)
}

# Convert the inline `{id: [entries]}` parameter-set map (the snapshot shape)
# to the per-record `[{id, parameters}, ...]` list the tree parser consumes.
#
# @keywords internal
# @noRd
.parameterSetMapToRecords <- function(setMap) {
  setMap <- setMap %||% list()
  lapply(names(setMap), function(id) {
    list(id = id, parameters = setMap[[id]] %||% list())
  })
}

# initialConditions: one file per set, `{id, initialConditions: [entries]}`.
# Each entry is a flat molecule-start-value record `{path, value, unit}`. Its
# own kind (not folded into `parameterSets`) because execution applies it via
# `ospsuite::setQuantityValuesByPath()` rather than `setParameterValuesByPath()`
# and scenarios reference it through a separate field.
#
# @keywords internal
# @noRd
.serializeInitialConditionSet <- function(initialConditions) {
  initialConditions <- initialConditions %||% list()
  out <- list()
  for (id in names(initialConditions)) {
    .validateDefinitionTreeKey(id, "initialConditionSet")
    out[[id]] <- list(
      id = id,
      initialConditions = unclass(initialConditions[[id]] %||% list())
    )
  }
  out
}

# @keywords internal
# @noRd
.parseInitialConditionTree <- function(records) {
  if (is.null(records)) {
    return(list())
  }
  out <- structure(list(), names = character(0L))
  for (rec in records) {
    id <- .keyedTreeRecordId(rec, "id", "initialConditionSet")
    .assertNoEmptyObjectFields(rec, "initialConditionSet")
    out[[id]] <- .asInitialConditionSet(rec$initialConditions %||% list())
  }
  out
}

# Stamp an initial-condition set's array-of-entries with
# `c("InitialConditionSet", "list")` so a single set read from
# `project$initialConditions[[id]]` dispatches its print method. The class is a
# transparent list wrapper; the serializers strip it
# (`.serializeInitialConditionSet` / `.initialConditionsToJson`) so it never
# reaches JSON.
#
# @keywords internal
# @noRd
.asInitialConditionSet <- function(entries) {
  entries <- entries %||% list()
  class(entries) <- c("InitialConditionSet", "list")
  entries
}

# Convert the inline `{id: [entries]}` initial-conditions map (the snapshot
# shape) to the per-record `[{id, initialConditions}, ...]` list the tree parser
# consumes.
#
# @keywords internal
# @noRd
.initialConditionMapToRecords <- function(setMap) {
  setMap <- setMap %||% list()
  lapply(names(setMap), function(id) {
    list(id = id, initialConditions = setMap[[id]] %||% list())
  })
}

# applications: one file per protocol, `{id, parameterSets: [...]}`.
#
# @keywords internal
# @noRd
.serializeApplicationSet <- function(applications) {
  applications <- applications %||% list()
  out <- list()
  for (id in names(applications)) {
    .validateDefinitionTreeKey(id, "application")
    app <- applications[[id]]
    rec <- list(id = id)
    if (!is.null(app$parameterSets) && length(app$parameterSets) > 0L) {
      rec$parameterSets <- as.list(app$parameterSets)
    }
    out[[id]] <- rec
  }
  out
}

# @keywords internal
# @noRd
.parseApplicationTree <- function(records) {
  appsObj <- list()
  for (rec in records) {
    id <- .keyedTreeRecordId(rec, "id", "application")
    .assertNoEmptyObjectFields(rec, "application")
    entry <- list()
    if (!is.null(rec$parameterSets)) {
      entry$parameterSets <- rec$parameterSets
    }
    appsObj[[id]] <- entry
  }
  .parseApplications(appsObj)
}

# Convert the inline `{id: {parameterSets}}` application map (the snapshot
# shape) to the per-record `[{id, parameterSets}, ...]` list the tree parser
# consumes.
#
# @keywords internal
# @noRd
.applicationMapToRecords <- function(appMap) {
  appMap <- appMap %||% list()
  lapply(names(appMap), function(id) {
    rec <- list(id = id)
    ps <- appMap[[id]]$parameterSets
    if (!is.null(ps)) {
      rec$parameterSets <- ps
    }
    rec
  })
}

# outputPaths: one file per path, `{id, path}`.
#
# @keywords internal
# @noRd
.serializeOutputPathSet <- function(outputPaths) {
  outputPaths <- outputPaths %||% list()
  ids <- names(outputPaths)
  if (length(outputPaths) > 0L && (is.null(ids) || any(ids == ""))) {
    cli::cli_abort(c(
      "{.field outputPaths} must be a named map of id to path string.",
      "i" = "Found an entry without an id."
    ))
  }
  out <- list()
  for (id in ids) {
    .validateDefinitionTreeKey(id, "outputPath")
    out[[id]] <- list(id = id, path = outputPaths[[id]])
  }
  out
}

# @keywords internal
# @noRd
.parseOutputPathTree <- function(records) {
  if (is.null(records)) {
    return(list())
  }
  out <- structure(list(), names = character(0L))
  for (rec in records) {
    id <- .keyedTreeRecordId(rec, "id", "outputPath")
    .assertNoEmptyObjectFields(rec, "outputPath")
    out[[id]] <- rec$path
  }
  out
}

# Convert the inline `{id: "path"}` output-path map (the snapshot shape) to the
# per-record `[{id, path}, ...]` list the tree parser consumes.
#
# @keywords internal
# @noRd
.outputPathMapToRecords <- function(pathMap) {
  pathMap <- pathMap %||% list()
  lapply(names(pathMap), function(id) {
    list(id = id, path = pathMap[[id]])
  })
}

# observedData: one file per declaration, keyed by the id `removeObservedData()`
# matches on (file basename, or the programmatic DataSet name).
#
# @keywords internal
# @noRd
.serializeObservedDataSet <- function(observedData) {
  observedData <- observedData %||% list()
  out <- list()
  for (entry in observedData) {
    id <- .observedDataEntryId(entry)
    .validateObservedDataId(id)
    # The on-disk id is the file basename (or the programmatic DataSet name).
    # Two declarations whose `file` differs only by directory derive the same
    # basename and would silently overwrite each other (one file on disk, the
    # second declaration lost on reload). Fail fast naming the collision rather
    # than dropping a declaration.
    if (!is.null(out[[id]])) {
      cli::cli_abort(c(
        "Two observedData declarations map to the same definition file {.file {id}.json}.",
        "x" = "The on-disk id is the file basename (or the programmatic name), \\
        so two sources sharing a basename collide.",
        "i" = "Rename one source so the basenames differ."
      ))
    }
    out[[id]] <- unclass(entry)
  }
  out
}

# @keywords internal
# @noRd
.parseObservedDataTree <- function(records) {
  # The in-memory `observedData` shape is an unnamed list of declarations; an
  # absent section (`NULL`) is a bare `list()`.
  if (is.null(records)) {
    return(list())
  }
  # Drop the transient `.definitionFile` load tag the tree loader attaches; it is
  # used only for error messages, and must not leak into the in-memory record
  # (which would make a tree-loaded section differ from an inline-loaded one).
  # Stamp each declaration with `c("ObservedDataSource", "list")` so a single
  # source dispatches its print method (the serializers strip it before JSON).
  records <- lapply(records, function(rec) {
    attr(rec, ".definitionFile") <- NULL
    .asObservedDataSource(rec)
  })
  unname(records)
}

# Stamp an observed-data declaration with `c("ObservedDataSource", "list")`.
# A transparent list wrapper carried for the print method only; both
# serialize paths (`.serializeObservedDataSet` and `.observedDataToJson`)
# strip it so it never reaches JSON.
#
# @keywords internal
# @noRd
.asObservedDataSource <- function(entry) {
  class(entry) <- c("ObservedDataSource", "list")
  entry
}

# The on-disk id of an observed-data declaration: the file basename for
# `excel`/`pkml`/`script`, the DataSet name for `programmatic`. This is the id
# `removeObservedData()` matches on, so the filename and that id agree.
#
# @keywords internal
# @noRd
.observedDataEntryId <- function(entry) {
  if (identical(entry$type, "programmatic")) {
    id <- entry$name
  } else {
    id <- basename(entry[["file"]] %||% "")
  }
  if (is.null(id) || !nzchar(id)) {
    cli::cli_abort(c(
      "An observedData declaration has no id to name its definition file.",
      "i" = "A file-based entry needs a {.field file}; a programmatic entry \\
      needs a {.field name}."
    ))
  }
  id
}

# The observed-data id becomes a filename via `.definitionFilePath(dir, id)`, so it
# must be a single safe path segment or it could escape the kind directory. A
# programmatic `name` reaches this verbatim (unlike the other keyed kinds, whose
# key is validated by `.validateDefinitionTreeKey`). Reject rather than rewrite: the
# id doubles as the match key for `removeObservedData()`, so canonicalizing it
# would desync the on-disk filename from the match key.
#
# @keywords internal
# @noRd
.validateObservedDataId <- function(id) {
  if (grepl("[/\\]", id) || id %in% c(".", "..") || basename(id) != id) {
    cli::cli_abort(c(
      "observedData id {.val {id}} is not a single safe filename segment.",
      "x" = "It must not contain a path separator or be {.val .} / {.val ..}, \\
      so it cannot escape the observed-data definition directory.",
      "i" = "Rename the source (its {.field file} basename or programmatic \\
      {.field name}) to a single safe filename segment."
    ))
  }
  invisible(NULL)
}

# plots: three keyed kinds, one part of the `project$plots` trio each.
#
# The serializers turn a part into an `id -> json-record` map keyed by the
# part's id field (`dataCombinedId` / `plotId` / `plotGridId`); the parsers
# reassemble the part from the per-file records, validating each record's inner
# id against its filename (`.keyedTreeRecordId`) so the on-disk filename stays
# the authoritative key. Each part's in-memory shape (a keyed list whose entries
# are the per-record fields) is the per-file JSON shape, so the serialize side
# is near-identity (it only strips the entry class) and the file content is
# byte-identical to the inlined snapshot's per-entry shape.

# data-combined: one file per dataCombined entry, the nested JSON object with
# its `dataCombinedId` re-added (the list key). The serializer canonicalizes
# nothing; `.validateDefinitionTreeKey` enforces the key is already canonical.
#
# @keywords internal
# @noRd
.serializeDataCombinedSet <- function(dataCombined) {
  dataCombined <- dataCombined %||% list()
  out <- list()
  for (id in names(dataCombined)) {
    .validateDefinitionTreeKey(id, "dataCombined")
    dc <- dataCombined[[id]]
    rec <- list(dataCombinedId = id)
    if (length(dc$simulated %||% list()) > 0) {
      rec$simulated <- dc$simulated
    }
    if (length(dc$observed %||% list()) > 0) {
      rec$observed <- dc$observed
    }
    out[[id]] <- rec
  }
  out
}

# @keywords internal
# @noRd
.parseDataCombinedTree <- function(records) {
  if (is.null(records)) {
    return(list())
  }
  # Re-tag each record with the filename-checked key, then delegate to the
  # shared nested-dataCombined parser (which re-keys by `dataCombinedId` and
  # drops the id field). The key check guarantees the inner `dataCombinedId`
  # agrees with the filename for a tree-loaded record.
  for (rec in records) {
    .keyedTreeRecordId(rec, "dataCombinedId", "dataCombined")
  }
  .parseNestedDataCombined(lapply(records, function(rec) {
    attr(rec, ".definitionFile") <- NULL
    rec
  }))
}

# plots: one file per plotConfiguration entry, the entry as a JSON object keyed
# by `plotId`.
#
# @keywords internal
# @noRd
.serializePlotConfigurationSet <- function(plotConfiguration) {
  .serializePlotEntrySet(plotConfiguration, "plotId", "plot")
}

# @keywords internal
# @noRd
.parsePlotConfigurationTree <- function(records) {
  .parsePlotEntryTree(records, "plotId", "plot", "Plot")
}

# plot-grids: one file per plotGrids entry, the entry as a JSON object keyed by
# `plotGridId`.
#
# @keywords internal
# @noRd
.serializePlotGridSet <- function(plotGrids) {
  .serializePlotEntrySet(plotGrids, "plotGridId", "plotGrid")
}

# @keywords internal
# @noRd
.parsePlotGridTree <- function(records) {
  .parsePlotEntryTree(records, "plotGridId", "plotGrid", "PlotGrid")
}

# Shared serializer for the two keyed-list plots parts. Each entry of the keyed
# list is already the per-file record shape, so this maps the in-memory
# reference field back to its suffixless on-disk key (`dataCombinedId` ->
# `dataCombined`, `plotIds` -> `plots`), drops the entry class (so `c("Plot",
# "list")` never leaks into JSON), validates the key, and emits an
# `id -> record` map. An empty / NULL part serializes to an empty map.
#
# @keywords internal
# @noRd
.serializePlotEntrySet <- function(entries, idField, definitionLabel) {
  entries <- entries %||% list()
  out <- list()
  for (id in names(entries)) {
    rec <- entries[[id]]
    recId <- rec[[idField]]
    if (
      is.null(recId) ||
        !is.character(recId) ||
        length(recId) != 1L ||
        is.na(recId) ||
        !nzchar(recId)
    ) {
      cli::cli_abort(c(
        "A {.field {definitionLabel}} entry has no usable {.field {idField}}.",
        "x" = "{.field {idField}} must be a single non-empty string."
      ))
    }
    .validateDefinitionTreeKey(id, definitionLabel)
    if (!identical(id, recId)) {
      cli::cli_abort(c(
        "A {.field {definitionLabel}} entry's {.field {idField}} disagrees with its \\
        map key.",
        "x" = "The map key is {.val {id}} but {.field {idField}} is \\
        {.val {recId}}.",
        "i" = "They must agree so the on-disk filename stays the authoritative \\
        key; store the entry under its {.field {idField}}."
      ))
    }
    rec <- .plotRefFieldToKey(rec, class(rec)[[1]])
    class(rec) <- "list"
    out[[id]] <- rec
  }
  out
}

# Shared parser for the two keyed-list plots parts. Validates each record's
# inner id against its filename, drops the load tag, and re-keys the entries
# into a keyed list (each entry classed `c("<idClass>", "list")`).
#
# @keywords internal
# @noRd
.parsePlotEntryTree <- function(records, idField, definitionLabel, idClass) {
  if (is.null(records) || length(records) == 0L) {
    return(list())
  }
  cleaned <- lapply(records, function(rec) {
    .keyedTreeRecordId(rec, idField, definitionLabel)
    attr(rec, ".definitionFile") <- NULL
    rec
  })
  .parsePlotEntries(cleaned, idField, idClass)
}

# parameterIdentification: one file per task, the task's JSON object.
#
# @keywords internal
# @noRd
.serializePITaskSet <- function(tasks) {
  tasks <- tasks %||% list()
  out <- list()
  for (id in names(tasks)) {
    .validateDefinitionTreeKey(id, "parameterIdentification task")
    task <- tasks[[id]]
    if (!identical(id, task$id)) {
      cli::cli_abort(c(
        "A {.field parameterIdentification task}'s {.field id} disagrees with \\
        its map key.",
        "x" = "The map key is {.val {id}} but {.field id} is {.val {task$id}}.",
        "i" = "They must agree so the on-disk filename stays the authoritative \\
        key; store the task under its {.field id}."
      ))
    }
    out[[id]] <- list(
      id = task$id,
      scenarios = as.list(task$scenarios),
      parameters = lapply(task$parameters, .piParameterToJson),
      outputMappings = lapply(task$outputMappings, .piOutputMappingToJson),
      configuration = task$configuration
    )
  }
  out
}

# Structural backstop shared by the keyed kinds: a write-back under a key that
# is not already canonical (mixed case, a forbidden character) is rejected,
# pointing the user at the canonicalizing authoring API. The authoring API
# (`add*` / `set*`) canonicalizes ids before they reach here, so a normal
# mutation always passes; this catches a raw write-back
# (`project$<section>[[key]] <- record`) that bypassed canonicalization. This
# subsumes the old case-insensitive-collision guard: two keys differing only in
# case cannot both be canonical, so they can never both reach the tree.
#
# @keywords internal
# @noRd
.validateDefinitionTreeKey <- function(key, definitionLabel) {
  canonical <- suppressWarnings(.canonicalizeId(key))
  if (!identical(key, canonical)) {
    cli::cli_abort(c(
      "{definitionLabel} id {.val {key}} is not a canonical definition-file id.",
      "i" = "Use the add/set API, which canonicalizes it to {.val {canonical}}, \\
      or store the definition under the key {.val {canonical}}."
    ))
  }
  invisible(NULL)
}

# Load-side id guard shared by the keyed kinds. The write path enforces the
# "id is the filename" contract (`.validateDefinitionTreeKey`), but the load path
# applied none of it: a record missing its id field aborted with an opaque
# base-R `list[[NULL]] <- x` error naming nothing, and a record whose inner id
# disagreed with its filename loaded keyed by the inner id (breaking
# canonicalized references and silently collapsing two files with the same inner
# id into one). This returns the validated key to store the record under,
# given the record, the name of its id field, and a human label for the kind:
#   - the id field must be a non-empty scalar string (else abort naming the
#     file, mirroring the PI-task message);
#   - for a record loaded from a tree file (tagged with `.definitionFile`), the
#     inner id must equal the filename stem, so the on-disk filename stays the
#     authoritative key and two files can never collapse onto one id.
# An inline-snapshot record (no `.definitionFile` tag) skips the filename check.
#
# @keywords internal
# @noRd
.keyedTreeRecordId <- function(record, idField, definitionLabel) {
  file <- attr(record, ".definitionFile")
  id <- record[[idField]]
  if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
    where <- if (is.null(file)) {
      character()
    } else {
      c("i" = "Check {.file {file}}.")
    }
    cli::cli_abort(c(
      "A definition file for kind {.field {definitionLabel}} has no usable \\
      {.field {idField}}.",
      "x" = "{.field {idField}} must be a single non-empty string \\
      (it names the definition and its file).",
      where
    ))
  }
  if (!is.null(file)) {
    stem <- tools::file_path_sans_ext(basename(file))
    if (!identical(id, stem)) {
      cli::cli_abort(c(
        "A definition file for kind {.field {definitionLabel}} has a stored \\
        {.field {idField}} that disagrees with its filename.",
        "x" = "{.field {idField}} is {.val {id}} but the file is \\
        {.val {stem}}.json.",
        "i" = "The filename stem is the definition's id; rename the file or the \\
        {.field {idField}} so they match. Check {.file {file}}."
      ))
    }
  }
  id
}

# Detect the `null -> {}` hand-edit corruption on a keyed record. The natural
# `jsonlite::fromJSON(simplifyVector = FALSE) |> toJSON(auto_unbox = TRUE)`
# round-trip re-emits each JSON `null` as an empty object `{}`, which decodes
# back to a zero-length list with names `character(0)` (a JSON array `[]`
# decodes to a zero-length list with `NULL` names, so the two are
# distinguishable). None of the keyed record shapes
# (scenarios / individuals / populations / parameter-sets / applications /
# output-paths) holds a bare `{}` in any field, so any field that is an empty
# object is the corruption signature, where a scalar (or `null`) was meant.
# Aborts naming the field and the file with the generic fix.
#
# @keywords internal
# @noRd
.assertNoEmptyObjectFields <- function(record, definitionLabel) {
  file <- attr(record, ".definitionFile")
  for (field in names(record)) {
    value <- record[[field]]
    if (
      is.list(value) &&
        length(value) == 0L &&
        identical(names(value), character(0))
    ) {
      where <- if (is.null(file)) {
        character()
      } else {
        c("i" = "Check {.file {file}}.")
      }
      cli::cli_abort(c(
        "A definition of kind {.field {definitionLabel}} has an invalid \\
        {.field {field}}.",
        "x" = "{.field {field}} is an empty object {.code {{}}} where a single \\
        value or {.code null} was expected.",
        "i" = "A hand-edit that turned {.code \"{field}\": null} into {.code {{}}} \\
        (the usual {.pkg jsonlite} round-trip) is the usual cause; restore the \\
        value or remove the field.",
        where
      ))
    }
  }
  invisible(NULL)
}

# Whole-tree writer ----

# Explode an in-memory `Project` into a full on-disk tree project at `dir`: a
# `Project.json` container plus a `definitions/<kind>/` tree (one file per
# definition) for every section. Returns the container path. This is the
# full-tree reconciler that `saveProject()` and `restoreProject()` both drive:
# it serializes every kind and writes each definition write-if-different, and
# each kind's writer owns its `<kind>/` directory (it deletes any `.json` not in
# the freshly-written keep-set), so a re-run reconciles the tree to memory
# exactly, leaving no stale entries. Shared by `saveProject()`,
# `restoreProject()`, and the Excel import.
#
# @keywords internal
# @noRd
.writeProjectTree <- function(project, dir) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  for (kind in .definitionKindNames()) {
    .writeDefinitionTree(.sectionForKind(project, kind), kind, project, dir)
  }
  # Write the container with the inline sections emptied: the tree owns them,
  # matching the on-disk shape `loadProject()` reads for a tree project.
  containerPath <- file.path(dir, "Project.json")
  .saveProjectJson(project, containerPath, containerOnly = TRUE)
  containerPath
}
