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

#' Freeze a project to a portable single-file snapshot
#'
#' @description Write the current in-memory state of a `Project` to a single
#'   self-contained `.esqlabsR` file with every section inlined. Unsaved edits
#'   are included, so a snapshot is a faithful freeze of the project as it is
#'   right now, and a legitimate "stash my experiment, then [reloadProject()]
#'   back to the saved state" move. The content is JSON; the `.esqlabsR`
#'   extension marks the file as a portable, shareable freeze-frame,
#'   distinguishing it at a glance from the `Project.json` container of a live
#'   tree project.
#'
#'   Materialize a snapshot back into a working tree with [restoreProject()];
#'   snapshot then restore then snapshot is a fixed point.
#'
#' @param project A `Project` object. It need not be bound to a directory; an
#'   in-memory project can be snapshotted too.
#' @param dir Target folder for the snapshot file (default `"."`). Created if
#'   it does not exist.
#' @param name Filename stem for the snapshot (the `.esqlabsR` extension is
#'   always forced, so any extension you include is replaced: `"exp.zip"` and
#'   `"study.json"` both become `exp.esqlabsR` / `study.esqlabsR`). When `NULL`
#'   (default), a colon-free timestamped stem
#'   `<projectName>-YYYY-MM-DD-HHMMSS` is used (sortable and Windows-safe);
#'   `<projectName>` falls back to `"project"` when the project has no name.
#' @param overwrite If `FALSE` (default), writing over an existing snapshot
#'   file aborts. Pass `TRUE` to replace it.
#'
#' @returns Invisibly, the (normalized `.esqlabsR`) path the snapshot was
#'   written to.
#' @export
#' @family project persistence
#' @seealso [restoreProject()], [loadProject()], [saveProject()].
#' @examples
#' # Scaffold a throwaway example project and snapshot it to a single file.
#' dir <- file.path(tempdir(), "snapshot-example")
#' dir.create(dir, showWarnings = FALSE)
#' initProject(dir, type = "example", createExcel = FALSE)
#' project <- loadProject(file.path(dir, "Project.json"))
#' snapshot <- snapshotProject(project, dir = tempdir(), name = "study")
#' snapshot # the normalized .esqlabsR path
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
    projectName <- project$name %||% "project"
    stem <- paste0(projectName, "-", format(Sys.time(), "%Y-%m-%d-%H%M%S"))
  } else {
    stem <- name
  }

  # The stem becomes a filename via `file.path(dir, ...)`, so a stem carrying a
  # path separator or a `..` segment could escape `dir` and write over (or
  # delete) an unrelated `.esqlabsR`. Reject it: both an explicit `name` and a
  # `project$name`-derived default are checked, so a project whose name contains
  # a separator aborts predictably rather than silently escaping.
  .validateSnapshotStem(stem)

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

#' Restore a project tree from a single-file snapshot
#'
#' @description Read a single self-contained snapshot file (a portable
#'   freeze-frame with every section inlined) and materialize it into a full
#'   on-disk tree project at `dir`: a `Project.json` container plus a
#'   `definitions/<kind>/` tree (one file per definition) for every section.
#'   Returns a freshly-loaded `Project` bound to `dir`.
#'
#'   Restore is path-based and needs no loaded project: the driving use case is
#'   sharing, where a colleague hands you a `.esqlabsR` and
#'   `restoreProject("theirs.esqlabsR", "myproj")` recreates the working tree
#'   from scratch. It is also the rollback half of the save-point story: with
#'   `overwrite = TRUE` it rolls a working directory back to a snapshot in
#'   place.
#'
#'   The canonical snapshot form is a `.esqlabsR` file (as written by
#'   [snapshotProject()]), but a plain inlined `Project.json` is also accepted
#'   for back-compatibility (for example, the file [importProjectFromExcel()]
#'   writes). The result is a normal tree project: [loadProject()] reads it back
#'   from `dir` identically (section for section).
#'
#' @param snapshot Path to the snapshot file to read (a `.esqlabsR` file, or a
#'   plain inlined `Project.json`). Must exist.
#' @param dir Target directory for the materialized tree project (default
#'   `"."`). Created if it does not exist.
#' @param overwrite If `FALSE` (default), a non-empty `dir` (any files,
#'   whether a full esqlabsR project, unrelated files, or a partial tree)
#'   aborts; unpack into a fresh directory only. If `TRUE`, the contents of
#'   `dir` are replaced in place (an in-place rollback), and, when `dir` held a
#'   real esqlabsR project, a warning is raised that any `Project` previously
#'   loaded from `dir` is now stale. Rebind to the returned object, or
#'   [reloadProject()] the old handle. The blessed idiom is
#'   `p <- restoreProject(snap, dir, overwrite = TRUE)`.
#'
#' @returns A freshly-loaded `Project`, bound to `dir`, with a clear dirty bit.
#' @export
#' @family project persistence
#' @seealso [snapshotProject()], [loadProject()], [reloadProject()].
#' @examples
#' # Write a snapshot, then materialize it into a fresh tree project.
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

  # A legacy or hand-authored snapshot may carry non-canonical ids (e.g.
  # `Sim_A`, `Aciclovir_PVB`), but the definition tree keys files by canonical id,
  # so the tree writer requires them. Canonicalize every id and every reference
  # to one in the raw snapshot JSON before parsing it, so a legacy single-file
  # `Project.json` migrates losslessly into the tree (definitions and references
  # are transformed together with the same deterministic helper, so foreign keys
  # still resolve). The canonicalized JSON is written to a throwaway file and
  # loaded from there, so the in-memory project the tree is exploded from is
  # already canonical.
  jsonData <- jsonlite::fromJSON(snapshot, simplifyVector = FALSE)
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
  inMemory <- loadProject(canonFile)
  containerPath <- .writeProjectTree(inMemory, dir)
  restored <- loadProject(containerPath)

  # The overwrite replaced a live tree; any `Project` loaded from `dir` before
  # this call now points at stale in-memory state. Warn on the overwrite action
  # (there is no live-object registry to detect a specific handle).
  if (replacedExistingTree && overwrite) {
    cli::cli_warn(messages$restoreOverwroteTree(dir))
  }

  restored
}

# Reject a snapshot filename stem that could escape `dir`. The stem is joined to
# `dir` via `file.path()`, so a stem that is not a single filename segment (it
# holds a `/` or `\` separator, or is `"."` / `".."`) could write outside `dir`
# and clobber an unrelated `.esqlabsR`. Both an explicit `name` and the
# `project$name`-derived default are validated, so a project whose name contains
# a separator aborts predictably rather than silently escaping. A stem must be a
# single non-empty, non-NA character scalar.
#
# @keywords internal
# @noRd
.validateSnapshotStem <- function(stem) {
  if (
    !is.character(stem) ||
      length(stem) != 1L ||
      is.na(stem) ||
      !nzchar(stem) ||
      grepl("[/\\]", stem) ||
      stem %in% c(".", "..")
  ) {
    cli::cli_abort(messages$invalidSnapshotName(stem))
  }
  invisible(NULL)
}
