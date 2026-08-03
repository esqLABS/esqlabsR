# Project snapshot / restore ----
#
# The portable single-file `.esqlabsR` concern (distinct from a renv lockfile
# snapshot or a PK-Sim project snapshot): `snapshotProject()` freezes the
# current in-memory state of a `Project` into one self-contained file with every
# section inlined (for sharing, archiving, or stashing before an experiment),
# and `restoreProject()` materializes such a file back into a full
# `definitions/<kind>/` tree project. The two are inverse operations
# (snapshot then restore then snapshot is a fixed point). They sit alongside the
# in-place lifecycle verbs (`loadProject()` / `saveProject()` / `reloadProject()`
# in `R/project-lifecycle.R`); the tree-format machinery they drive
# (`.writeProjectTree()`, the per-kind serialize/parse specs) lives in
# `R/definition-files.R`.

#' Save the whole project to a single shareable snapshot file
#'
#' @description Write a `Project`, exactly as it is in your R session, to a
#'   single self-contained `.esqlabsR` file. Unsaved changes are included, so
#'   the snapshot captures the project as it is right now — which also makes
#'   it a good way to set an experiment aside: snapshot it, then go back to
#'   the saved state with [reloadProject()]. The file content is JSON; the
#'   `.esqlabsR` extension simply marks the file as a portable snapshot, so
#'   it is not confused with the `Project.json` file of a project folder.
#'
#'   Turn a snapshot back into a full project folder with [restoreProject()];
#'   nothing is lost in the round trip.
#'
#' @param project A `Project` object. It does not need a folder on disk; a
#'   project that exists only in the R session can be snapshotted too.
#' @param dir Target folder for the snapshot file (default `"."`). Created if
#'   it does not exist.
#' @param name File name for the snapshot, without extension: the `.esqlabsR`
#'   extension is always added, and any extension you include is replaced
#'   (`"exp.zip"` and `"study.json"` both become `exp.esqlabsR` /
#'   `study.esqlabsR`). When `NULL` (default), a timestamped name
#'   `<projectName>-YYYY-MM-DD-HHMMSS` is used (it sorts by date and is safe
#'   as a Windows file name); `<projectName>` falls back to `"project"` when
#'   the project has no name.
#' @param overwrite If `FALSE` (default), writing over an existing snapshot
#'   file aborts. Pass `TRUE` to replace it.
#'
#' @returns Invisibly, the path of the written snapshot file (always with the
#'   `.esqlabsR` extension).
#' @export
#' @family projectPersistence
#' @seealso [restoreProject()], [loadProject()], [saveProject()].
#' @examples
#' # Create a temporary example project and snapshot it to a single file.
#' dir <- file.path(tempdir(), "snapshot-example")
#' dir.create(dir, showWarnings = FALSE)
#' initProject(dir, type = "example", createExcel = FALSE)
#' project <- loadProject(file.path(dir, "Project.json"))
#' snapshot <- snapshotProject(project, dir = tempdir(), name = "study")
#' snapshot # the path of the snapshot file
snapshotProject <- function(
  project,
  dir = ".",
  name = NULL,
  overwrite = FALSE
) {
  validateIsOfType(project, "Project")

  # Resolve the filename stem. A NULL `name` gets the colon-free timestamped
  # default; a nameless project falls back to the fixed "project" stem.
  if (is.null(name)) {
    projectName <- project$info$name %||% "project"
    stem <- paste0(projectName, "-", format(Sys.time(), "%Y-%m-%d-%H%M%S"))
  } else {
    stem <- name
  }

  # The stem becomes a filename via `file.path(dir, ...)`, so a stem carrying a
  # path separator or a `..` segment could escape `dir` and write over (or
  # delete) an unrelated `.esqlabsR`. Reject it: both an explicit `name` and a
  # `project$info$name`-derived default are checked, so a project whose name contains
  # a separator aborts predictably rather than silently escaping.
  .validateFilenameSegment(stem, messages$invalidSnapshotName)

  # A snapshot IS a `.esqlabsR`; force the extension regardless of what the
  # caller included (idempotent for a `.esqlabsR` stem).
  fileName <- fs::path_ext_set(stem, "esqlabsR")
  path <- file.path(dir, fileName)

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (file.exists(path) && !overwrite) {
    cli::cli_abort(messages$snapshotFileExists(path))
  }

  .saveProjectJson(project, path)
  invisible(path)
}

#' Recreate a project folder from a snapshot file
#'
#' @description Read a snapshot file and recreate a full project folder from
#'   it at `dir`: a `Project.json` file plus the `definitions/` folder with
#'   one file per definition. Returns a freshly loaded `Project` that works
#'   from `dir`.
#'
#'   You only need the file, not a loaded project. The typical use is
#'   sharing: a colleague sends you a `.esqlabsR` file, and
#'   `restoreProject("theirs.esqlabsR", "myproj")` recreates the whole
#'   project folder from it. It is also the way back to an earlier state:
#'   with `overwrite = TRUE` it rolls an existing project folder back to the
#'   snapshot.
#'
#'   Snapshots are usually `.esqlabsR` files written by [snapshotProject()],
#'   but a `Project.json` in which all sections are written out in the file
#'   itself, rather than in a `definitions/` folder (a legacy single-file
#'   project), is accepted too. A monolithic snapshot written by a
#'   previous esqlabsR version (`snapshotProjectConfiguration()`) is also
#'   accepted and upgraded to the current project format on read, through the
#'   Excel bridge; observed data is not carried in such a snapshot, so add it
#'   with [addObservedData()] if a plot or parameter identification needs it.
#'   The result is a normal project: [loadProject()] opens it from `dir` with
#'   exactly the same content.
#'
#' @param snapshot Path to the snapshot file to read (a `.esqlabsR` file, a
#'   legacy single-file `Project.json`, or a monolithic snapshot written by a
#'   previous esqlabsR version). Must exist.
#' @param dir Folder in which the project is recreated (default `"."`).
#'   Created if it does not exist.
#' @param overwrite If `FALSE` (default), `restoreProject()` aborts when
#'   `dir` already contains any files — a project, unrelated files, anything;
#'   restore into a fresh folder only. If `TRUE`, the contents of `dir` are
#'   replaced with the snapshot. When `dir` held an esqlabsR project, a
#'   warning reminds you that a `Project` you loaded from `dir` earlier no
#'   longer matches the files: continue with the returned project, or refresh
#'   the old one with [reloadProject()]. The recommended form is
#'   `p <- restoreProject(snap, dir, overwrite = TRUE)`.
#'
#' @returns A freshly loaded `Project` working from `dir`, with no unsaved
#'   changes.
#' @export
#' @family projectPersistence
#' @seealso [snapshotProject()], [loadProject()], [reloadProject()].
#' @examples
#' # Write a snapshot, then recreate a project folder from it.
#' src <- file.path(tempdir(), "restore-src")
#' dir.create(src, showWarnings = FALSE)
#' initProject(src, type = "example", createExcel = FALSE)
#' snapshot <- snapshotProject(
#'   loadProject(file.path(src, "Project.json")),
#'   dir = tempdir(),
#'   name = "shared"
#' )
#' project <- restoreProject(snapshot, file.path(tempdir(), "restored"))
restoreProject <- function(snapshot, dir = ".", overwrite = FALSE) {
  validateIsString(snapshot)
  validateIsString(dir)
  if (!file.exists(snapshot)) {
    cli::cli_abort(messages$fileNotFound(snapshot))
  }

  # Refuse writing into a `dir` that already holds anything unless
  # `overwrite = TRUE`: restore unpacks into a fresh directory (spec 3.6). The
  # non-empty check is broader than `isProjectInitialized()` on purpose, because
  # a `dir` holding unrelated files, or a partial `definitions/` tree with no
  # `Project.json`, is still not a safe place to unpack into without consent.
  dirNotEmpty <- dir.exists(dir) &&
    length(list.files(dir, all.files = TRUE, no.. = TRUE)) > 0L
  if (dirNotEmpty && !overwrite) {
    cli::cli_abort(messages$restoreDirNotEmpty(dir))
  }
  # Whether the overwrite is replacing a real project tree decides whether to
  # warn about stale handles below (an unrelated non-empty dir has no live
  # `Project` bound to it to go stale).
  replacedExistingTree <- isProjectInitialized(dir)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  jsonData <- jsonlite::fromJSON(snapshot, simplifyVector = FALSE)

  # A previous-version monolithic snapshot (Excel workbooks dumped to one JSON by
  # an older `snapshotProjectConfiguration()`) is not a v6 `Project.json`; upgrade
  # it through the Excel bridge rather than trying to parse it as a v6 snapshot.
  if (.isLegacySnapshot(jsonData)) {
    return(.upgradeLegacySnapshot(
      jsonData,
      dir,
      overwrite,
      replacedExistingTree,
      snapshotDir = dirname(fs::path_abs(snapshot))
    ))
  }

  # A legacy or hand-authored snapshot may carry non-canonical ids (e.g.
  # `Sim_A`, `Aciclovir_PVB`), but the definition tree keys files by canonical id,
  # so the tree writer requires them. Canonicalize every id and every reference
  # to one in the raw snapshot JSON before parsing it, so a legacy single-file
  # `Project.json` migrates losslessly into the tree (definitions and references
  # are transformed together with the same deterministic helper, so foreign keys
  # still resolve). The canonicalized JSON is written to a throwaway file and
  # loaded from there, so the in-memory project the tree is exploded from is
  # already canonical.
  jsonData <- .canonicalizeProjectJsonIds(jsonData)
  canonFile <- tempfile(fileext = ".json")
  on.exit(unlink(canonFile), add = TRUE)
  jsonlite::write_json(
    jsonData,
    canonFile,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE,
    digits = NA
  )

  # Read the canonicalized snapshot into an in-memory `Project`, then explode it
  # into the `definitions/<kind>/` tree at `dir`. Loading the materialized
  # container back returns a fresh tree-backed `Project` bound to `dir`. The
  # local is named `inMemory` (not `snapshotProject`) so it does not shadow the
  # exported `snapshotProject()` function.
  # `Project$new()` rather than `loadProject()`: this load is an internal step
  # towards the tree, and `loadProject()` warns about unresolved
  # cross-references, which the caller would hear twice for one restore (once
  # here, once from the returned project loaded below).
  inMemory <- Project$new(projectFilePath = canonFile)
  # A restore materializes a brand-new tree project at `dir`, so it always
  # writes the canonical `Project.json` container name (the default). Passing
  # `inMemory`'s own `projectFilePath` would be wrong here: that is the
  # throwaway `canonFile` this restore loaded from, not the destination.
  containerPath <- .writeProjectTree(
    inMemory,
    dir,
    containerPath = file.path(dir, "Project.json")
  )
  restored <- loadProject(containerPath)

  # The overwrite replaced a live tree; any `Project` loaded from `dir` before
  # this call now points at stale in-memory state. Warn on the overwrite action
  # (there is no live-object registry to detect a specific handle).
  if (replacedExistingTree && overwrite) {
    cli::cli_warn(messages$restoreOverwroteTree(dir))
  }

  restored
}
