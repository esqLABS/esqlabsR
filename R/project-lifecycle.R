# Project lifecycle: load, save, init, examples ----

#' Load a project from a JSON configuration file
#'
#' @description Load a `Project` from a JSON file. This is the
#'   primary entry point for working with esqlabsR projects.
#'
#'   On load, the project is checked for the most common reference mistakes
#'   (for example a scenario referring to an individual or population that is
#'   not defined). Such issues are reported as warnings so that obvious
#'   configuration mistakes surface immediately, but loading still succeeds.
#'   Use [validateProject()] for a full report.
#'
#' @param path Path to the project's JSON file, or to the folder holding it.
#'   Given a folder, the project file inside it is opened: `Project.json` when
#'   it is there, otherwise the single project file the folder carries, so a
#'   project named through `importProjectFromExcel(projectFileName = )` opens
#'   too. Defaults to the working directory.
#'
#' @returns Object of type `Project`
#' @export
#' @family projectPersistence
#' @seealso [saveProject()], [reloadProject()], [snapshotProject()],
#'   [restoreProject()], [projectStatus()].
#'
#' @section Editing a loaded project:
#'   Changes you make to a loaded project — with `addScenario()`,
#'   `setIndividual()`, `removeParameterSet()`, `addOutputPath()`, and the
#'   other add/set/remove functions — live only in your R session until you
#'   save them; the files on disk stay as they are. Reading a section
#'   directly (for example `scenarios` definitions) never changes the project: a
#'   definition changes only through the add/set/remove functions.
#'
#'   The project's own fields are not definitions and have no add/set/remove
#'   function: assign them on the project instead, `project$info$name <- "..."`
#'   for the name and description, and `project$paths$populationsFolder <- "..."`
#'   for a working folder (see [Project] for the full field list). An
#'   observed-data source's `file` and `importerConfiguration` are fields of the
#'   declaration, so they are authored with [addObservedData()]. Every such
#'   assignment is an in-memory edit like any other, saved by [saveProject()].
#'
#'   Write your changes to the project files with [saveProject()]. Discard
#'   unsaved changes and go back to what is saved on disk with
#'   [reloadProject()]. Save the current state of the whole project to a
#'   single file you can archive or share with [snapshotProject()], and
#'   recreate a project folder from such a file (or roll an existing one
#'   back) with [restoreProject()]. Check for unsaved changes and outdated
#'   Excel files with [projectStatus()].
#'
#' @examples
#' \dontrun{
#' project <- loadProject("Project.json")
#' results <- runScenarios(project)
#'
#' # A project folder works too, whatever its project file is called.
#' project <- loadProject("path/to/myProject")
#'
#' # Edits stay in memory until you save.
#' addOutputPath(project, "x", "Organism|A|Concentration in container")
#' saveProject(project)
#' }
loadProject <- function(path = ".") {
  project <- Project$new(projectFilePath = path)
  .warnOnCrossReferenceErrors(project)
  project
}

# Read a project container file and every section's `definitions/<kind>/` tree
# (or inline fallback) into a plain list, doing all of the load's I/O and
# nothing else: no field is assigned anywhere until the whole tree has parsed
# successfully, so a failure partway (a missing file, an unsupported schema
# version, a malformed definition) leaves nothing half-populated. The
# `Project` commits the returned list via `private$.applyLoadedSections()`.
#
# Everything this aborts on is a property of the file, not of the call that
# opened it, and the path is reached from several entrypoints (`loadProject()`,
# `Project$new()`, `ProjectConfiguration()`, `reloadProject()`). Attribute
# those aborts to no function at all rather than to this helper, whose name
# means nothing to the reader.
#
# @keywords internal
# @noRd
.loadProjectTree <- function(path) {
  rlang::local_error_call(NULL)
  jsonPath <- .resolveProjectContainerPath(path)
  if (!fs::file_exists(jsonPath)) {
    cli::cli_abort(messages$fileNotFound(jsonPath))
  }
  jsonData <- tryCatch(
    jsonlite::fromJSON(jsonPath, simplifyVector = FALSE),
    error = function(e) {
      cli::cli_abort(
        "Failed to parse {.file {jsonPath}} as JSON.",
        parent = e
      )
    }
  )
  if (!identical(jsonData$schemaVersion, "2.0")) {
    # A previous-version monolithic snapshot has no `schemaVersion`, so it
    # fails this check for a reason the version number cannot express: it is
    # not a malformed project of the current format, it is an older project
    # that `restoreProject()` upgrades. Name that call rather than reporting
    # a missing schema version. Every entrypoint that opens a project file
    # (`loadProject()`, `Project$new()`, the deprecated
    # `ProjectConfiguration()`, `reloadProject()`) reads it through here, so
    # the one guard covers all of them.
    if (.isLegacySnapshot(jsonData)) {
      cli::cli_abort(messages$legacySnapshotNotLoadable(jsonPath))
    }
    # `version` is bound here because the catalog entry leaves its
    # placeholder unglued, so this call interpolates it exactly once.
    version <- jsonData$schemaVersion %||% "<missing>"
    cli::cli_abort(messages$unsupportedSchemaVersion(version))
  }
  .parseProjectSections(jsonData, jsonPath, dirname(jsonPath))
}

# Turn a decoded container (the `Project.json` object as a plain list) into the
# section list a `Project` commits, resolving each section against the
# `definitions/<kind>/` tree under `projectDirPath` and falling back to the
# section inlined in the container where no tree directory exists.
#
# Split from `.loadProjectTree()` so a container that never came off disk can be
# parsed too: the Excel bridge builds one in memory (`.excelToProjectJson()`)
# and compares the resulting project against a live one, with no import and no
# temporary tree. Point `projectDirPath` at a directory holding no
# `definitions/` for such a container, so every section takes the inline
# fallback.
#
# `jsonPath` is recorded as the project's own file path and named in parse
# errors; for an in-memory container it is the path the project would have.
#
# @keywords internal
# @noRd
.parseProjectSections <- function(jsonData, jsonPath, projectDirPath) {
  rlang::local_error_call(NULL)
  .validateDefinitionsFolder(jsonData$definitionsFolder)
  definitionsFolder <- jsonData$definitionsFolder %||% "definitions"

  # The container separates two concerns: the live working folders
  # (the `filePaths` block) the runtime reads, and the Excel-bridge
  # sheet-name fields (the `excel` block) only the Excel bridge reads. A
  # legacy project carries both sets in one flat `filePaths` block; split
  # it on read so both on-disk shapes load (the field-to-block mapping is
  # fixed, so the partition is deterministic). A new-shape project reads
  # each block from its own key; any Excel field that still appears in
  # `filePaths` (e.g. a hand-edited file) is routed to the Excel store too.
  fp <- jsonData$filePaths %||% list()
  excel <- jsonData$excel %||% list()
  # A hand-edited `Project.json` could carry both the legacy `modelFolder`
  # and the current `simulationsFolder` key. They map to the same slot, so
  # rather than let iteration order decide, warn and drop the legacy key so
  # the current `simulationsFolder` deterministically wins.
  hasSimulationsCollision <- all(
    c("modelFolder", "simulationsFolder") %in% names(fp)
  )
  if (hasSimulationsCollision) {
    cli::cli_warn(messages$duplicateSimulationsFolderKey())
    fp[["modelFolder"]] <- NULL
  }
  filePathsData <- list()
  excelData <- list()
  for (n in names(fp)) {
    # Accept the pre-6.0.0 key `modelFolder` and store it under the current
    # name `simulationsFolder`, so a legacy `Project.json` (or an
    # Excel-imported project whose `Property` column still says
    # `modelFolder`) resolves without a manual edit.
    key <- if (identical(n, "modelFolder")) "simulationsFolder" else n
    if (key %in% .excelFilePathFields) {
      excelData[[key]] <- list(value = fp[[n]], description = "")
    } else {
      filePathsData[[key]] <- list(value = fp[[n]], description = "")
    }
  }
  for (n in names(excel)) {
    excelData[[n]] <- list(value = excel[[n]], description = "")
  }

  # Every authored section is a definition tree under `definitions/<kind>/`; a
  # single-file snapshot with no tree falls back to the inline section in
  # `Project.json`. `.loadDefinitionTree()` resolves tree-vs-inline per kind and
  # the kind's spec parses the raw records into the in-memory shape. Output
  # paths load before scenarios because scenarios dereference their
  # `outputPathIds` against the project-level `outputPaths` map; there is no
  # live `Project` yet to hand the scenarios parser (it reads
  # `project$definitions$outputPaths`), so a plain list exposing that one
  # field stands in for it. The `parameterSets` inline fallback merges any
  # legacy three-section `Project.json` into the one map (a clash aborts the
  # load).
  loadSection <- function(kind, project = NULL) {
    spec <- .definitionTreeSpec(kind)
    records <- .loadDefinitionTree(
      projectDirPath,
      kind,
      spec$inline(jsonData),
      definitionsFolder
    )
    spec$parse(records, project)
  }
  outputPaths <- loadSection("outputPaths")
  scenarios <- loadSection(
    "scenarios",
    list(definitions = list(outputPaths = outputPaths))
  )

  list(
    projectFilePath = jsonPath,
    projectDirPath = projectDirPath,
    schemaVersion = jsonData$schemaVersion,
    esqlabsRVersion = jsonData$esqlabsRVersion,
    name = jsonData$name,
    description = jsonData$description,
    definitionsFolder = jsonData$definitionsFolder,
    defaultSimulationRunOptions = jsonData$defaultSimulationRunOptions,
    filePathsData = filePathsData,
    excelData = excelData,
    outputPaths = outputPaths,
    scenarios = scenarios,
    parameterSets = loadSection("parameterSets"),
    initialConditions = loadSection("initialConditions"),
    individuals = loadSection("individuals"),
    populations = loadSection("populations"),
    applications = loadSection("applications"),
    observedData = loadSection("observedData"),
    # The plots concern is three independent top-level sections, each its own
    # keyed kind: `dataCombined` (`definitions/data-combined/`), `plots`
    # (`definitions/plots/`, the plot list), and `plotGrids`
    # (`definitions/plot-grids/`). Each loads from its own tree (or its own
    # top-level inline snapshot section as the fallback).
    dataCombined = loadSection("dataCombined"),
    plots = loadSection("plots"),
    plotGrids = loadSection("plotGrids"),
    parameterIdentification = loadSection("parameterIdentification")
  )
}

#' Save the project to the disk
#'
#' @description Write your changes to the project files on disk. Changes made
#'   in your R session (e.g. with `addScenario()`, `setIndividual()`,
#'   `removeParameterSet()`) only live in memory until you call
#'   `saveProject()`.
#'
#'   What happens when you save:
#'
#'   - Only files with actual changes are re-written, so `git diff` shows
#'     exactly the definitions you edited.
#'   - If you removed something from the project (e.g. a scenario), its file
#'     in the `definitions/` folder is deleted. Files outside the
#'     `definitions/` folder are never touched.
#'   - The `Project.json` file is updated.
#'
#'   If there is nothing to save, `saveProject()` simply reports that the
#'   project is already up to date. Saving repeatedly is always safe.
#'
#'   Saving does not update the Excel files. If you also work with the Excel
#'   configuration files, refresh them with [exportProjectToExcel()]. Use
#'   [projectStatus()] to check whether project files on disk, the Excel files,
#'   and your R session are in sync.
#'
#' @param project A `Project` loaded from disk with [loadProject()] (or
#'   restored with [restoreProject()]). A project created directly with
#'   `Project$new()` has no folder on disk to save to; use [snapshotProject()]
#'   to write it to a single file, or create a project folder first with
#'   [initProject()].
#'
#' @returns Invisibly, the `project`.
#' @export
#' @family projectPersistence
#' @seealso [loadProject()], [reloadProject()], [snapshotProject()],
#'   [restoreProject()], [projectStatus()].
#' @examples
#' \dontrun{
#' project <- loadProject("Project.json")
#' addOutputPath(project, "PVB", "Organism|PeripheralVenousBlood|...")
#' saveProject(project) # tree now mirrors memory
#' saveProject(project) # clean save: "Project is already up to date; ..."
#' }
saveProject <- function(project) {
  validateIsOfType(project, "Project")
  project$save()
}

# Implementation behind `project$save()` / `saveProject()`. Reads and clears the
# dirty bit through its own `private`; `self` is the `Project` the on-disk
# reconciler needs.
#
# @keywords internal
# @noRd
.saveProject_impl <- function(self, private, .call) {
  rlang::local_error_call(.call)
  if (is.null(self$info$projectFilePath)) {
    cli::cli_abort(messages$saveProjectNoTree())
  }

  # The dirty bit is the memory-vs-tree divergence signal. A clean save is a
  # reassuring, idempotent no-op, never an error.
  if (!private$.isModified()) {
    cli::cli_inform(messages$projectAlreadyUpToDate())
    return(invisible(self))
  }

  # Persist any session-added programmatic DataSet to a PKML file next to the
  # project and rewrite its entry to a `pkml` source, so it survives a reload.
  # This mutates the in-memory `observedData` section before the tree writer
  # serializes it below, and returns the names it persisted; those stay in the
  # runtime store until the whole save commits (below), so a tree write that
  # aborts leaves the DataSet recoverable rather than lost.
  persistedProgrammatic <- .persistProgrammaticObservedData(self, private)

  # Same for a session-injected `Population`: freeze it to `<id>.csv` under the
  # populations folder and rewrite its sentinel to a `csv` source, so it survives
  # a reload. The ids stay in the runtime store until the whole save commits.
  persistedPopulations <- .persistProgrammaticPopulations(self, private)

  # Drive the full-tree reconciler: `.writeProjectTree()` writes every kind's
  # write-if-different, orphan-reconciled tree and the `containerOnly = TRUE`
  # container in one pass, which is exactly `saveProject()`'s contract. Pass the
  # path the project was loaded from so the save updates that container in place;
  # a project loaded from a container that is not called `Project.json` (one an
  # earlier version wrote as `ProjectConfiguration.json`, or one named through
  # the Excel import's `projectFileName`) must not fork a stray `Project.json`
  # next to it.
  .writeProjectTree(
    self,
    self$info$projectDirPath,
    containerPath = self$info$projectFilePath
  )

  # The tree write succeeded, so the persisted DataSets are now file-backed;
  # drop them from the session-only runtime store.
  for (name in persistedProgrammatic) {
    private$.programmaticDataSets[[name]] <- NULL
  }
  # Same for the frozen populations, now file-backed.
  for (id in persistedPopulations) {
    private$.programmaticPopulations[[id]] <- NULL
  }

  # The container just written declares the format this version writes and the
  # version that wrote it, so the handle adopts both: after a save, what the
  # project reports is what its files say.
  private$.schemaVersion <- "2.0"
  private$.esqlabsRVersion <- as.character(utils::packageVersion("esqlabsR"))

  private$.clearModified()
  invisible(self)
}

#' Discard a project's unsaved changes and re-read it from disk
#'
#' @description The undo of saving: discard every unsaved change and re-read
#'   the project from its files on disk, in place. The `Project` stays the
#'   same object, so every variable that points to it stays valid.
#'
#'   `reloadProject()` always re-reads the project's files and updates the
#'   project in place, so it also picks up changes made to the files outside
#'   the R session (for example after [restoreProject()] rolled the project
#'   back). It simply produces no announcement when there was nothing to
#'   discard: unlike a clean [saveProject()], a clean reload prints nothing.
#'
#' @param project A `Project` with a folder on disk. A project that exists
#'   only in the R session has nothing to reload from and aborts.
#'
#' @returns Invisibly, the `project`, with unsaved changes discarded.
#' @export
#' @family projectPersistence
#' @seealso [loadProject()], [saveProject()], [snapshotProject()],
#'   [restoreProject()], [projectStatus()].
#' @examples
#' \dontrun{
#' project <- loadProject("Project.json")
#' addScenario(project, "oops", modelFile = "model.pkml")
#' reloadProject(project) # discard the edit, back to disk
#' }
reloadProject <- function(project) {
  validateIsOfType(project, "Project")
  project$reload()
}

# Implementation behind `project$reload()` / `reloadProject()`.
#
# @keywords internal
# @noRd
.reloadProject_impl <- function(self, private, .call) {
  rlang::local_error_call(.call)
  if (is.null(self$info$projectFilePath)) {
    cli::cli_abort(messages$reloadProjectNoTree())
  }

  # Always re-read from disk, even when the handle is clean: a clean handle can
  # still be stale after `restoreProject(..., overwrite = TRUE)` rolled the tree
  # back, or after an external edit to the JSON, and the blessed rollback idiom
  # relies on reload refreshing in place. "Silent when clean" is about the
  # message only: `.reload()` emits no success announcement, so a clean reload
  # stays quiet; the cross-reference warning re-fires only when there are
  # genuine cross-ref errors, which is correct regardless of the dirty bit.
  private$.reload()
  .warnOnCrossReferenceErrors(self)
  invisible(self)
}

#' Emit a `cli_warn` listing critical cross-reference errors, if any
#'
#' Validates the whole project so every section's references are resolved, not
#' just the scenarios': a freshly loaded project is the moment to say what does
#' not resolve, whichever section holds it.
#'
#' @keywords internal
#' @noRd
.warnOnCrossReferenceErrors <- function(project) {
  results <- .runProjectValidation(project, sections = NULL)
  r <- results$crossReferences
  if (is.null(r) || !r$hasCriticalErrors()) {
    return(invisible(NULL))
  }
  bullets <- vapply(r$critical_errors, function(e) e$message, character(1))
  bullets <- stats::setNames(bullets, rep("x", length(bullets)))
  cli::cli_warn(c(
    "Project has {length(bullets)} unresolved cross-reference{?s}:",
    bullets,
    "i" = "Run {.code validateProject(project)} for the full report."
  ))
  invisible(NULL)
}

#' Check if a directory contains an esqlabsR project
#'
#' @description Checks whether a directory already contains an esqlabsR
#'   project, by looking for a project container: a JSON file declaring the
#'   project schema version, usually `Project.json`. That container and the
#'   `definitions/` tree beside it are the project; the Excel files are an
#'   interchange format, not a source of truth.
#'
#'   A folder holding only Excel configuration files (a `Project.xlsx` and a
#'   `Configurations/` folder, i.e. a pre-6.0.0 project that has not been
#'   migrated yet) is therefore reported as *not* initialized. Turn it into a
#'   project with [importProjectFromExcel()].
#'
#' @param destination A string defining the path to check for an existing
#'   project. Defaults to current working directory.
#'
#' @returns `TRUE` if an esqlabsR project exists in the directory, `FALSE`
#'   otherwise.
#' @export
#' @family projectPersistence
#' @examples
#' \dontrun{
#' # Check if current directory has a project
#' hasProject <- isProjectInitialized()
#'
#' # Check if specific directory has a project
#' hasProject <- isProjectInitialized("path/to/project")
#' }
isProjectInitialized <- function(destination = ".") {
  destination <- fs::path_abs(destination)

  # The canonical container name first, so the common case costs one existence
  # check instead of parsing every JSON file in the folder.
  if (file.exists(file.path(destination, "Project.json"))) {
    return(TRUE)
  }

  length(.projectContainerPaths(destination)) > 0L
}

# Resolve what a caller handed to `loadProject()` / `Project$new()` to the
# absolute path of one project container file.
#
# A path to a file is returned as given (absolute), so naming a container
# directly always works. A path to a FOLDER is resolved to the container inside
# it, which is what makes both `loadProject()` with no argument and
# `loadProject("<projectFolder>")` open a project: `fs::file_exists()` is true
# for a directory, so without this a folder would reach the JSON reader and fail
# as unparseable JSON.
#
# A container is not always called `Project.json` (an Excel import can be told
# to name it, and an earlier version wrote `ProjectConfiguration.json`), so the
# folder is scanned with the same `.projectContainerPaths()` predicate
# `isProjectInitialized()` uses. `Project.json` still wins when it is there, so
# a project that also carries a differently-named container (a leftover, or a
# second study file) opens the canonical one rather than a guess.
#
# @keywords internal
# @noRd
.resolveProjectContainerPath <- function(path) {
  # What this aborts on is a property of the path, not of the call that passed
  # it, and the path arrives from several entrypoints (`loadProject()`,
  # `Project$new()`, `ProjectConfiguration()`). Attribute the abort to no
  # function at all rather than to this helper, whose name means nothing to the
  # reader, exactly as `.loadProjectTree()` does.
  rlang::local_error_call(NULL)
  path <- fs::path_abs(path)
  if (!fs::dir_exists(path)) {
    return(path)
  }

  canonical <- file.path(path, "Project.json")
  if (fs::file_exists(canonical)) {
    return(fs::path_abs(canonical))
  }

  containers <- .projectContainerPaths(path)
  if (length(containers) == 1L) {
    return(fs::path_abs(containers[[1]]))
  }
  # `folder` / `names` are bound here because both catalog entries leave their
  # placeholders unglued, so the raising call interpolates them exactly once.
  folder <- path
  if (length(containers) == 0L) {
    cli::cli_abort(messages$noProjectContainerInFolder(folder))
  }
  names <- fs::path_file(containers)
  cli::cli_abort(messages$multipleProjectContainersInFolder(folder, names))
}

# The paths of every project container directly inside `destination`: a `.json`
# file declaring `schemaVersion` `"2.0"`. The file name is not the
# discriminator, because a container is not always called `Project.json`: an
# Excel import can be told to name it something else
# (`projectFileName = "MyStudy"`), and a project written by an earlier version
# carries `ProjectConfiguration.json`. The declared schema version is what
# `Project` accepts when loading, so it is what decides here too. A `.json` that
# does not parse, or that declares no schema version (a data file, a PK-Sim
# snapshot, a pre-6.0.0 monolithic snapshot), is not a container.
#
# The container's other machine-managed field, `esqlabsRVersion`, deliberately
# plays no part: saving a project passes through whatever version its container
# declared, so a genuine container can carry none at all, and requiring it here
# would miss those projects.
#
# @keywords internal
# @noRd
.projectContainerPaths <- function(destination) {
  if (!fs::dir_exists(destination)) {
    return(character(0))
  }

  jsonFiles <- as.character(fs::dir_ls(
    destination,
    glob = "*.json",
    type = "file",
    fail = FALSE
  ))

  isContainer <- vapply(
    jsonFiles,
    function(path) identical(.readContainerField(path, "schemaVersion"), "2.0"),
    logical(1)
  )

  jsonFiles[isContainer]
}

# One top-level field of a project container, or `NULL` when the file does not
# parse as JSON or does not carry the field. Reading a candidate container is
# always a "maybe this is one" question, so an unparseable file is an answer,
# not an error.
#
# @keywords internal
# @noRd
# Rewrite the fields a freshly copied container cannot carry itself, leaving every
# other field as it is. Used on a container this package has just put on disk
# without going through the serializer (the copied `initProject()` template).
#
# `esqlabsRVersion` becomes the running package version, so the field names the
# version that wrote the project, as it does on every path that goes through the
# serializer. `name` is written only when the caller supplied one, so a template
# that carries a meaningful name of its own keeps it.
#
# @keywords internal
# @noRd
.stampContainerFields <- function(path, name = "") {
  if (!file.exists(path)) {
    return(invisible(NULL))
  }
  parsed <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  parsed$esqlabsRVersion <- as.character(utils::packageVersion("esqlabsR"))
  if (!identical(name, "")) {
    parsed$name <- name
  }
  jsonlite::write_json(
    parsed,
    path,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE,
    digits = NA
  )
  invisible(NULL)
}

.readContainerField <- function(path, field) {
  parsed <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  parsed[[field]]
}

# TRUE when `destination` holds Excel project configuration files: a
# `*Project*.xlsx` workbook, or a `Configurations/` folder. On its own (without a
# `Project.json`) that is a pre-6.0.0 Excel project nobody has migrated yet:
# not a project as `isProjectInitialized()` defines one, but not a free folder
# either, so `initProject()` must not scaffold over it unasked. The markers
# cannot tell such a project apart from the Excel side-cars a migrated project
# exports, which is why this is not the public predicate: where both exist, the
# `Project.json` already answers the question.
#
# @keywords internal
# @noRd
.hasLegacyExcelProject <- function(destination) {
  if (!fs::dir_exists(destination)) {
    return(FALSE)
  }

  # Match on the basename: fs::dir_ls() globs the full path, so a destination
  # directory whose own path contains "Project" would otherwise match any
  # .xlsx inside it.
  xlsxFiles <- fs::path_file(fs::dir_ls(
    destination,
    glob = "*.xlsx",
    fail = FALSE
  ))

  any(grepl("Project", xlsxFiles, fixed = TRUE)) ||
    fs::dir_exists(file.path(destination, "Configurations"))
}

#' Initialize esqlabsR Project Folders and required Files
#'
#' @description
#'
#' Creates a new JSON-based esqlabsR project in `destination`: a
#' `Project.json` file plus a `definitions/` folder holding one file per
#' definition, alongside the working folders (`Models/`, `Data/`,
#' `Populations/`, `Results/`). By default it also writes the optional Excel
#' configuration files from the JSON; set `createExcel = FALSE` for a
#' JSON-only project.
#'
#' @param destination A string defining the path where to initialize the
#'   project. Defaults to the current working directory. The folder is created
#'   if it does not exist yet.
#' @param type Type of project to create: `"minimal"` (default) creates an empty
#'   project with just the directory structure, `"example"` creates a project
#'   with example data, models, and configurations.
#' @param createExcel If `TRUE` (default), generates Excel configuration files
#'   from the JSON. Set to `FALSE` for a JSON-only workflow.
#' @param overwrite If TRUE, overwrites existing project without asking for
#'   permission. If FALSE and a project already exists, asks user for permission
#'   to overwrite.
#' @param name Character scalar written to the `name` field of the new
#'   `Project.json`. Defaults to `""`, which writes no name and leaves whatever
#'   the template carries: no `name` field at all for `type = "minimal"`, and
#'   the example project's own name for `type = "example"`.
#' @returns Invisibly returns `destination`, the path the project was
#'   initialized in.
#' @export
#' @family projectPersistence
initProject <- function(
  destination = ".",
  type = c("minimal", "example"),
  createExcel = TRUE,
  overwrite = FALSE,
  name = ""
) {
  destination <- fs::path_abs(destination)
  type <- match.arg(type)
  # `validateIsString()` checks type only, so it accepts a character vector of
  # any length and an `NA`. Either would reach the container as a JSON array or
  # a `null` in a scalar field and fail somewhere further away.
  validateIsString(name)
  if (length(name) != 1L || is.na(name)) {
    cli::cli_abort(messages$invalidInitProjectName())
  }

  # The destination is about to be filled with the project scaffold, so an
  # absent folder is created rather than rejected: `initProject("myProject")`
  # in an empty parent folder is the first call of the authoring workflow. A
  # no-op when the folder already exists.
  fs::dir_create(destination)

  source_folder <- switch(
    type,
    "minimal" = .projectDirectory("Blank"),
    "example" = .projectDirectory("Example")
  )

  # Is the destination safe to fill? That is a broader question than the public
  # "is there a project here?": an unmigrated legacy Excel project is not a
  # project, but scaffolding over it would bury it, so it asks for consent too.
  if (
    isProjectInitialized(destination) || .hasLegacyExcelProject(destination)
  ) {
    if (overwrite) {
      # Overwrite without asking
      msg <- messages$overwriteDestination(destination)
      cli::cli_inform("{msg}")
    } else {
      if (!.isInteractive()) {
        cli::cli_abort(messages$cannotPromptNonInteractive())
      }
      if (!.confirmOverwrite()) {
        cli::cli_abort(messages$abortedByUser())
      }
      msg <- messages$overwriteDestination(destination)
      cli::cli_inform("{msg}")
    }
    # Overwrite means REPLACE, not merge. Copying the template with
    # `overwrite = TRUE` refreshes the files it ships, but a definition file the
    # old project's `definitions/<kind>/` tree carried and the template does
    # not would survive and re-load as a stale definition. Remove the known
    # project artifacts (the definitions tree and `Project.json`) first, scoped
    # to just those paths so unrelated user files in the destination are left
    # intact.
    .clearProjectArtifacts(destination)
  }

  # Copy template files (just the JSON for minimal, full fixture for example)
  sourceFiles <- list.files(source_folder, full.names = TRUE)
  copied <- file.copy(
    sourceFiles,
    destination,
    recursive = TRUE,
    overwrite = TRUE
  )
  if (!all(copied)) {
    cli::cli_abort(messages$failedToCopyTemplate(sourceFiles[!copied]))
  }

  # The template ships a fixed `esqlabsRVersion`, and every other writer
  # (`saveProject()`, `snapshotProject()`, the Excel bridge) stamps the running
  # package version, so the copied value is restamped here: the field says which
  # version wrote the project, and a scaffold that claimed one version and then
  # reported another after the first save read as a downgrade. `name` is applied
  # in the same pass, before the Excel export below reads the container back.
  .stampContainerFields(file.path(destination, "Project.json"), name = name)

  # Create the working-folder structure. Each folder gets a short `README.md`
  # so it stays tracked under version control (git ignores empty folders) and
  # tells the reader what belongs there. `Models/Snapshots` is scaffolded for
  # PK-Sim / MoBi snapshots even though the package does not load from it yet;
  # it is part of every project's structure. The `definitions/` tree carries
  # the authored project content and needs no placeholder.
  .scaffoldProjectFolders(destination)

  if (createExcel) {
    jsonPath <- file.path(destination, "Project.json")
    project <- loadProject(jsonPath)
    # `initProject()` owns and controls `destination` (its own `overwrite`
    # argument already governed whether to replace an existing scaffold), so it
    # writes the Excel side-cars unconditionally.
    exportProjectToExcel(
      project,
      outputDir = destination,
      overwrite = TRUE,
      silent = TRUE
    )
  }

  invisible(destination)
}

# The working folders that ship with a `README.md` placeholder, each mapped to
# its one-line text. The README keeps the otherwise-empty folder tracked under
# version control (git does not track empty folders) and tells the reader what
# belongs there.
.projectReadmeFolders <- c(
  "Models/Simulations" = "Simulations as *.pkml that will be referenced by scenarios.",
  "Models/Snapshots" = "PK-Sim and MoBi snapshots (*.json). Not loaded by the package yet; reserved for a future release.",
  "Data" = "Observed data files referenced by the project.",
  "Populations" = "Population definitions as *.csv files, loaded by scenarios that reference them.",
  "Results/Figures" = "By default, figures will be saved in this folder.",
  "Results/SimulationResults" = "By default, simulation results will be saved in this folder."
)

# Folders created without a README placeholder. `definitions/` holds the
# authored project content and is never empty in a real project.
.projectPlainFolders <- c("definitions")

# Create the working-folder structure under `destination`, writing a short
# `README.md` placeholder into every README-bearing folder (see
# `.projectReadmeFolders`). An existing `README.md` is left untouched: a user
# may have edited it to document their own project, and `initProject(overwrite
# = TRUE)` must honor the "working folders are left untouched" invariant
# `.clearProjectArtifacts()` documents.
# @keywords internal
# @noRd
.scaffoldProjectFolders <- function(destination) {
  createFolder <- function(folder) {
    dir.create(
      file.path(destination, folder),
      recursive = TRUE,
      showWarnings = FALSE
    )
  }
  for (folder in .projectPlainFolders) {
    createFolder(folder)
  }
  for (folder in names(.projectReadmeFolders)) {
    createFolder(folder)
    readmePath <- file.path(destination, folder, "README.md")
    if (!file.exists(readmePath)) {
      writeLines(.projectReadmeFolders[[folder]], readmePath)
    }
  }
  invisible(destination)
}

# Remove the known esqlabsR project artifacts from `destination` before an
# overwrite, so `initProject(overwrite = TRUE)` REPLACES rather than merges. It
# removes only the scaffold the initializer owns:
#   - the definitions tree (`<destination>/<definitionsFolder>/`), the sole
#     source of the stale-definition leak, because a per-definition file the old tree
#     carried and the template does not would otherwise survive the copy and
#     re-load as a stale definition. The existing project's `definitionsFolder`
#     is read from its container (default `"definitions"`) so a custom
#     tree location is cleared too;
#   - every project container in `destination`, whatever it is named, so an
#     imported project's `<workbook-stem>.json` does not survive as a second,
#     stale container beside the `Project.json` the scaffold writes.
# Everything else in `destination` (working folders, and any unrelated user
# file) is left untouched. `unlink()`/`file.remove()` return values are checked
# so a failed removal aborts loudly rather than leaving a half-cleared project.
#
# @keywords internal
# @noRd
.clearProjectArtifacts <- function(destination) {
  containers <- .projectContainerPaths(destination)

  # The definitions folder name is configurable; read it from each container so
  # a non-default tree location is cleared too. With no container to read (the
  # destination held an unmigrated Excel project), fall back to the default
  # folder name, and clear only that one: a folder no container names is not
  # known to be a definitions tree.
  # Bound outside the `vapply()` lambda: taken inside it, the caller is the
  # lambda, whose `sys.call()` is `FUN(X[[i]], ...)`, so the abort would read
  # `Error in FUN():` and name nothing the user can act on.
  callerEnv <- rlang::caller_env()
  definitionsFolders <- if (length(containers) == 0L) {
    "definitions"
  } else {
    unique(vapply(
      containers,
      function(path) {
        folder <- .readContainerField(path, "definitionsFolder")
        # The container is read straight off disk here, without building a
        # `Project`, so this is the only guard standing between an untrusted
        # `definitionsFolder` and the recursive `unlink()` below. Refusing to
        # clear is the safe outcome: better to abort the overwrite than to
        # delete a directory outside `destination`.
        .validateDefinitionsFolder(folder, call = callerEnv)
        folder %||% "definitions"
      },
      character(1)
    ))
  }

  for (folder in definitionsFolders) {
    definitionsDir <- file.path(destination, folder)
    if (dir.exists(definitionsDir)) {
      failed <- unlink(definitionsDir, recursive = TRUE, force = TRUE)
      if (failed != 0L || dir.exists(definitionsDir)) {
        cli::cli_abort(messages$failedToClearProjectArtifacts(definitionsDir))
      }
    }
  }

  for (containerPath in containers) {
    if (!file.remove(containerPath)) {
      cli::cli_abort(messages$failedToClearProjectArtifacts(containerPath))
    }
  }

  invisible(NULL)
}

# Thin wrapper around base::interactive(), as a package-local binding so
# tests can mock the interactive/non-interactive branch of initProject().
#
# @keywords internal
# @noRd
.isInteractive <- function() {
  interactive()
}

# Ask the interactive user whether to overwrite an existing project.
# Returns TRUE only if they pick "Yes". Wrapped in a package-local helper
# so tests can mock it without touching the `utils` namespace.
#
# @keywords internal
# @noRd
.confirmOverwrite <- function() {
  qs <- sample(c("Absolutely not", "Yes", "No way"))
  out <- utils::menu(
    title = "The destination folder seems to already contain an esqlabsR project. Do you want to overwrite it?",
    choices = qs
  )
  out != 0L && qs[[out]] == "Yes"
}

#' Get the path to the example Project.json
#'
#' @returns A string representing the path to the example
#'   `Project.json` file shipped with the package.
#' @export
#' @family projectPersistence
#' @examples
#' exampleProjectPath()
exampleProjectPath <- function() {
  file.path(.projectDirectory("Example"), "Project.json")
}

#' Get path to esqlabsR project templates
#'
#' esqlabsR comes bundled with project templates in its `inst/extdata`
#' directory. This function makes them easy to access.
#'
#' @param name Name of project directory. If `NULL`, the available names will be
#'   listed.
#' @keywords internal
#' @noRd
.projectDirectory <- function(name = NULL) {
  directory <- system.file("extdata", "projects", package = "esqlabsR")
  if (!is.null(name)) {
    directory <- file.path(directory, name)
  }
  directory
}
