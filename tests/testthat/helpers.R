# =============================================================================
# Test Helper Functions
# =============================================================================
#
# Test fixtures live under `tests/testthat/data/`:
#
#   - flat files (pkml, single xlsx sheets, csvs) for unit-level tests.
#     Reached via getTestDataFilePath().
#
#   - TestProject/        canonical JSON-first test project. Reached via
#                         testProject().
#
#   - TestProjectExcel/   legacy Excel-shape project kept for round-trip
#                         tests of the Excel import / export bridge.
#                         Reached via testProjectExcelPath() (the entry .xlsx).
#
# For a writable, throwaway project use with_temp_project(), which calls
# initProject(type = "example", createExcel = TRUE) in a temp dir.

#' Get path to a file in `tests/testthat/data/`.
getTestDataFilePath <- function(fileName = "") {
  testthat::test_path("data", fileName)
}

#' Load the canonical test `Project` from a throwaway copy.
#'
#' Saving a loaded project (`saveProject()`) writes to its
#' `definitions/<kind>/` tree. To keep the version-controlled fixture pristine
#' and tests isolated from one another, the fixture is copied to a temporary
#' directory and the project is loaded from the copy. The copy is removed when
#' the calling test finishes.
testProject <- function(envir = parent.frame()) {
  loadProject(file.path(.copyTestProjectDir(envir), "Project.json"))
}

# The Aciclovir PKML fixture that lives INSIDE a copied test project (under its
# own `simulationsFolder`). Use this, rather than the source-tree `pkmlFixture`,
# when a test builds an on-disk `testProject()` and calls
# `createScenariosFromPKML()`, so the stored `modelFile` stays inside the
# project and does not trip the out-of-folder warning.
pkmlInProject <- function(project) {
  file.path(project$paths$simulationsFolder, "Aciclovir.pkml")
}

#' A `Project.json` path inside a fresh throwaway directory.
#'
#' Use this when a test needs a throwaway project location: a project is a
#' directory (the `Project.json` container plus a `definitions/` definition tree
#' alongside it), so writing into the shared session tempdir would scatter a
#' `definitions/` directory there and leak definitions into unrelated
#' `loadProject()` calls. The directory is removed when the calling test
#' finishes.
local_projectPath <- function(envir = parent.frame()) {
  file.path(
    withr::local_tempdir("project_", .local_envir = envir),
    "Project.json"
  )
}

#' Copy the canonical TestProject fixture to a throwaway directory and
#' return that directory. Cleaned up when the calling test finishes.
.copyTestProjectDir <- function(envir = parent.frame()) {
  src <- testthat::test_path("data", "TestProject")
  dest <- withr::local_tempdir("TestProject_", .local_envir = envir)
  file.copy(
    list.files(src, full.names = TRUE),
    dest,
    recursive = TRUE
  )
  dest
}

#' Load the bundled example `Project` from a throwaway copy.
#'
#' Like [testProject()], the bundled example is copied to a temporary
#' directory before loading so that saving edits (`saveProject()`) never
#' touches the version-controlled fixture under `inst/extdata`. The copy is
#' removed when the calling test finishes.
exampleProject <- function(envir = parent.frame()) {
  src <- dirname(exampleProjectPath())
  dest <- withr::local_tempdir("Example_", .local_envir = envir)
  file.copy(list.files(src, full.names = TRUE), dest, recursive = TRUE)
  loadProject(file.path(dest, "Project.json"))
}

#' Path to the legacy Excel `ProjectConfiguration.xlsx` fixture, used by
#' Excel-bridge round-trip tests.
testProjectExcelPath <- function() {
  testthat::test_path(
    "data",
    "TestProjectExcel",
    "ProjectConfiguration.xlsx"
  )
}

#' Redact the throwaway-project absolute prefix from a quoted path in an error
#' message so an `expect_snapshot()` is stable across runs, keeping the
#' project-relative `definitions/...` tail that carries the meaning. Used as the
#' `transform` of snapshots whose error names an absolute definition-file path in a
#' temp directory.
.redactTmpPath <- function(lines) {
  gsub("'[^']*/(definitions(/[^']*)?)'", "'<project>/\\1'", lines)
}

#' Redact a whole quoted absolute path that lives under the session temp
#' directory, so an `expect_snapshot()` of a message naming a per-run temp path
#' (a snapshot file, a target directory) is stable across runs. Both the temp
#' root and the random per-run basename vary run to run, so the whole quoted
#' path is collapsed to `'<tmp-path>'`, keeping a fixed `.esqlabsR` suffix when
#' present so a snapshot-file message still reads as one. Used as the
#' `transform` of snapshots whose message names such a path with no meaningful
#' project-relative tail.
#'
#' Redaction is restricted to quoted paths under `tempdir()`: a quoted absolute
#' path elsewhere is left intact, so a meaningful assertion on a real path is
#' never silently hidden. Separators are normalized to `/` first (so a Windows
#' backslash path matches), and both a Unix (`/tmp/...`) and a drive-prefixed
#' Windows (`C:/...`) temp path are matched.
.redactTmpDir <- function(lines) {
  # Normalize backslashes to forward slashes so a Windows path (in the message
  # or in `tempdir()`) matches the same pattern as a Unix one.
  lines <- gsub("\\\\", "/", lines)
  tmp <- gsub("\\\\", "/", tempdir())
  # Anchor the match to the escaped `tempdir()` prefix, so only a quoted path
  # that actually starts under `tempdir()` is redacted. On Windows `tempdir()`
  # already carries the drive letter (`C:/...`), so escaping it verbatim also
  # matches the drive-prefixed form a message reports; a Unix path starts with
  # `/tmp/...`. Either way, an unrelated absolute path is left intact.
  prefix <- .escapeRegex(tmp)
  # A quoted temp path ending in the snapshot extension keeps that suffix; any
  # other quoted temp path collapses to a bare placeholder.
  lines <- gsub(
    paste0("'", prefix, "[^']*\\.esqlabsR'"),
    "'<tmp-path>.esqlabsR'",
    lines
  )
  gsub(paste0("'", prefix, "[^']*'"), "'<tmp-path>'", lines)
}

#' Escape the regex metacharacters in a literal string so it can be embedded in
#' a pattern as a fixed prefix.
.escapeRegex <- function(x) {
  gsub("([.^$*+?()\\[\\]{}|\\\\])", "\\\\\\1", x, perl = TRUE)
}

#' Extract axis ranges from plots
#'
#' @description
#' Extracts the x and y axis ranges from a list of plots for testing purposes.
#'
#' @param p List of plots where each element contains plot objects.
#'
#' @returns List containing x and y axis ranges for each plot group.
#'
#' @examples
#' \dontrun{
#' # Extract ranges from sensitivity plots
#' ranges <- extractAxisRange(sensitivity_plots)
#' }
extractAxisRange <- function(p) {
  pn <- names(p)

  axisRanges <- purrr::map(pn, function(n) {
    pbs <- purrr::map(seq_along(p[[n]]), ~ ggplot2::ggplot_build(p[[n]][[.x]]))
    xRanges <- purrr::map(pbs, ~ .x$layout$panel_params[[1]]$x.range)
    yRanges <- purrr::map(pbs, ~ .x$layout$panel_params[[1]]$y.range)
    list(
      xRange = unlist(xRanges),
      yRange = unlist(yRanges)
    )
  })
  names(axisRanges) <- pn

  return(axisRanges)
}

#' Summarize sensitivity calculation data
#'
#' @description
#' Creates summary statistics for sensitivity calculation results filtered by parameter paths.
#'
#' @param data Data frame containing sensitivity calculation results.
#' @param path Vector of parameter paths to filter the data by.
#'
#' @returns List containing:
#'   - `charColumnSummary`: Summary of character columns (unique values)
#'   - `numericColumnSummary`: Summary statistics for numeric columns
#'
#' @examples
#' \dontrun{
#' # Summarize PK data for specific parameters
#' summary <- summarizer(results$pkData, parameterPaths[1:3])
#' }
summarizer <- function(data, path) {
  data <- dplyr::filter(data, ParameterPath %in% path)

  list(
    "charColumnSummary" = dplyr::select(data, where(is.character)) |>
      purrr::map_dfr(unique),
    "numericColumnSummary" = dplyr::select(data, where(is.numeric)) |>
      purrr::map_dfr(\(x) as.list(summary(x)), .id = "column")
  )
}

#' Create a temporary project location for testing
#'
#' @description
#' Creates a temporary directory with an initialized esqlabsR project for testing.
#' Uses `withr::defer()` to ensure proper cleanup after the test.
#'
#' @param projectName Optional name for the project. If provided, uses this name in the temporary directory pattern.
#' @param overwrite Whether to overwrite existing project files. Defaults to TRUE.
#'
#' @returns A list containing:
#'   - `path`: Path to the temporary project directory
#'   - `project`: `Project` object loaded from the initialized `Project.json`
#'
#' @examples
#' \dontrun{
#' temp_project <- with_temp_project()
#' temp_project$path
#' temp_project$project
#' }
with_temp_project <- function(projectName = NULL, overwrite = TRUE) {
  prefix <- if (is.null(projectName)) {
    "esqlabsR_test_"
  } else {
    paste0("esqlabsR_", projectName, "_")
  }
  # `local_tempdir()` creates the directory and, scoped to the calling test's
  # frame, removes it when that test exits (even on error).
  temp_dir <- withr::local_tempdir(prefix, .local_envir = parent.frame())

  initProject(
    destination = temp_dir,
    type = "example",
    createExcel = TRUE,
    overwrite = overwrite
  )
  project <- loadProject(file.path(temp_dir, "Project.json"))

  list(
    path = temp_dir,
    project = project
  )
}

# White-box test hooks onto the `Project` internal seam.
#
# The read/write seam (`.getSection`, `.setSection`, the lifecycle markers) is a
# set of `private$` methods on the class, reachable only from within a method of
# the instance, never from a free function. Tests that need to seed raw section
# state (often deliberately invalid, to exercise a validator) or assert on the
# dirty/validation bits reach the live private environment through
# `.__enclos_env__`, the standard R6 white-box test hook. These wrappers keep
# that reach in one documented place instead of scattering it across the suite;
# they are test-only and never part of the package surface.
.projectSeam <- function(project) {
  project$.__enclos_env__$private
}
.getSection <- function(project, kind) {
  .projectSeam(project)$.getSection(kind)
}
.setSection <- function(project, kind, value) {
  .projectSeam(project)$.setSection(kind, value)
}
.markModified <- function(project) {
  .projectSeam(project)$.markModified()
}
.markValidated <- function(project) {
  .projectSeam(project)$.markValidated()
}
.isModified <- function(project) {
  .projectSeam(project)$.isModified()
}
.isValidated <- function(project) {
  .projectSeam(project)$.isValidated()
}
# Seed a read-only info backing field (`.schemaVersion` / `.esqlabsRVersion`)
# through the private seam, standing in for the load machinery. A local binds
# the seam environment first: `.projectSeam(project)$.field <- value` would ask
# R for a `.projectSeam<-` replacement function, which does not exist.
.setInfoField <- function(project, field, value) {
  seam <- .projectSeam(project)
  seam[[paste0(".", field)]] <- value
  invisible(project)
}

# Builds a minimal in-memory `Project` for validation/serialization tests:
# all section fields default to empty, and `...` overrides named fields so a
# test can target one section without loading the full TestProject fixture.
# The section accessors are read-only from the handle, so sections are written
# through the internal `.setSection()` seam the authoring methods use
# (this is test setup standing in for an authoring call, not end-user code).
.fakeProject <- function(...) {
  project <- Project$new()
  # `schemaVersion` / `esqlabsRVersion` are read-only on the object surface
  # (managed by the load/save machinery, not by users), so seed the backing
  # fields through the private seam the load machinery uses, standing in for a
  # load here.
  .setInfoField(project, "schemaVersion", "2.0")
  .setInfoField(project, "esqlabsRVersion", NA_character_)
  sections <- c(
    "outputPaths",
    "scenarios",
    "parameterSets",
    "initialConditions",
    "individuals",
    "populations",
    "applications",
    "observedData",
    "dataCombined",
    "plots",
    "plotGrids"
  )
  for (section in sections) {
    .setSection(project, section, list())
  }
  overrides <- list(...)
  for (nm in names(overrides)) {
    .setSection(project, nm, overrides[[nm]])
  }
  project
}
