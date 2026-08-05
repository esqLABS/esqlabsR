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
#   - TestProjectExcelLegacy/
#                         pre-5.6 Excel project, carrying the workbook shapes
#                         real legacy projects have rather than the modern
#                         spelling of each. Reached via
#                         localLegacyExcelProject().
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

#' Pin the print and precision options a numeric snapshot depends on.
#'
#' A snapshot of tabular output formats differently under another `digits` or
#' `pillar.sigfig`, so a block taking one calls this first. Scoped to the
#' calling frame with `withr::local_options()`, so nothing leaks past the test.
.localSnapshotOptions <- function(.local_envir = parent.frame()) {
  withr::local_options(
    tibble.width = Inf,
    pillar.min_title_chars = Inf,
    pillar.sigfig = 4,
    digits = 4,
    scipen = 999,
    .local_envir = .local_envir
  )
}

#' The observed data set the test project's Aciclovir scenario is fitted against.
#'
#' The name is long and appears in most parameter-identification fixtures, so
#' name it once here rather than pasting the literal into each test.
testObservedDataId <- paste0(
  "Laskin 1982.Group A_Aciclovir_1_Human_MALE_",
  "PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
)

#' Build a `PIParameter` for the test project's EHC parameter.
#'
#' Defaults describe the standard fixture; pass any `PIParameter()` argument to
#' override just that one, so a test shows only what it varies.
testPIParameter <- function(...) {
  do.call(
    PIParameter,
    utils::modifyList(
      list(
        id = "EHC",
        scenarios = "testscenario",
        path = "Organism|Liver|EHC continuous fraction",
        minValue = 0.5,
        maxValue = 1.0,
        startValue = 0.8
      ),
      list(...)
    )
  )
}

#' Build a `PIOutputMapping` for the test project's peripheral venous blood.
#'
#' Overrides work the same way as [testPIParameter()].
testPIOutputMapping <- function(...) {
  do.call(
    PIOutputMapping,
    utils::modifyList(
      list(
        id = "PVB",
        scenarios = "testscenario",
        outputPath = "aciclovir_pvb",
        observedData = testObservedDataId
      ),
      list(...)
    )
  )
}

#' Build a `PITask` over the test project's Aciclovir scenario.
#'
#' Defaults to one [testPIParameter()] and one [testPIOutputMapping()].
#' Overrides work the same way as [testPIParameter()].
testPITask <- function(...) {
  do.call(
    PITask,
    utils::modifyList(
      list(
        id = "t",
        scenarios = "testscenario",
        parameters = list(testPIParameter()),
        outputMappings = list(testPIOutputMapping()),
        configuration = list(algorithm = "BOBYQA")
      ),
      list(...)
    )
  )
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

#' One row of an Excel `Scenarios` sheet, with every required column present.
#'
#' Named arguments override a column's value; passing `NULL` drops the column
#' entirely, which is how a test builds a sheet that omits or misspells one.
scenarioSheetRow <- function(...) {
  row <- list(
    Scenario_name = "s1",
    IndividualId = NA_character_,
    PopulationId = NA_character_,
    ReadPopulationFromCSV = NA,
    ModelParameterSheets = NA_character_,
    ApplicationProtocol = NA_character_,
    SimulationTime = NA_character_,
    SimulationTimeUnit = NA_character_,
    SteadyState = NA,
    SteadyStateTime = NA_real_,
    SteadyStateTimeUnit = NA_character_,
    OverwriteFormulasInSS = NA,
    ModelFile = "m.pkml",
    OutputPathsIds = "op1"
  )
  overrides <- list(...)
  for (column in names(overrides)) {
    row[[column]] <- overrides[[column]]
  }
  row <- row[!vapply(row, is.null, logical(1))]
  data.frame(row, stringsAsFactors = FALSE)
}

#' A writable copy of the `TestProjectExcel` fixture, returning its directory.
#'
#' The Excel-bridge tests import, export and re-import in place, so they need a
#' throwaway copy rather than the version-controlled fixture. The copy is
#' removed when the calling test finishes.
localExcelProjectDir <- function(envir = parent.frame()) {
  workDir <- withr::local_tempdir(.local_envir = envir)
  file.copy(dirname(testProjectExcelPath()), workDir, recursive = TRUE)
  file.path(workDir, "TestProjectExcel")
}

#' Export a project to Excel and read it back, returning the re-imported one.
#'
#' The two temporary directories (the workbook set, and the JSON project built
#' from it) are removed when the calling test finishes.
#'
#' The example project's observed data does not survive the round trip, so the
#' re-imported project always has one reference it cannot resolve. That one
#' warning is muffled here because it says nothing about the section under
#' test; every other warning is left to surface. Two tests assert it directly
#' rather than calling this helper.
excelRoundTrip <- function(project, envir = parent.frame()) {
  excelOut <- withr::local_tempdir(.local_envir = envir)
  exportProjectToExcel(project, outputDir = excelOut, silent = TRUE)
  jsonOut <- withr::local_tempdir(.local_envir = envir)
  .muffleCrossReferenceWarning({
    reimportedJson <- importProjectFromExcel(
      file.path(excelOut, "Project.xlsx"),
      outputDir = jsonOut,
      silent = TRUE
    )
    loadProject(reimportedJson)
  })
}

#' Evaluate `expr`, dropping only the unresolved-cross-reference warning.
.muffleCrossReferenceWarning <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("unresolved cross-reference", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
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

# Legacy Excel fixture (issue #1213) ----
#
# `TestProjectExcelLegacy/` is a pre-5.6 project: two-column `Protein` +
# `Ontogeny` ontogenies, no `OverwriteFormulasInSS` column, quoted multi-value
# cells, a populations CSV folder under the configurations folder, and the 5.x
# parameter-identification sheet layout. The sibling `TestProjectExcel/` fixture
# has the modern spelling of every one of those, so it cannot reproduce what the
# importer does with the legacy ones.
#
# `data-raw/TestProjectExcelLegacy.R` regenerates the workbooks and documents
# each legacy trait.

#' Copy the legacy Excel fixture to a throwaway directory and return it.
#'
#' The committed fixture holds no `.pkml`: the 7 MB public Aciclovir model the
#' sibling `TestProjectExcel/` fixture already carries is copied into the throwaway
#' project's `Models/Simulations/` here, so the copy is a complete, runnable
#' project without a second copy of that binary in the repository.
#'
#' The copy is removed when the calling test finishes. Mutate one workbook of it
#' with [editWorkbookSheets()] to derive a per-defect variant.
localLegacyExcelProject <- function(envir = parent.frame()) {
  src <- testthat::test_path("data", "TestProjectExcelLegacy")
  dest <- withr::local_tempdir("LegacyExcel_", .local_envir = envir)
  file.copy(list.files(src, full.names = TRUE), dest, recursive = TRUE)
  file.copy(
    testthat::test_path(
      "data",
      "TestProjectExcel",
      "Models",
      "Simulations",
      "Aciclovir.pkml"
    ),
    file.path(dest, "Models", "Simulations", "Aciclovir.pkml")
  )
  dest
}

#' Path to the legacy fixture's entry workbook inside a copied project.
legacyExcelProjectPath <- function(projectDir) {
  file.path(projectDir, "ProjectConfiguration.xlsx")
}

#' Rewrite one workbook in place, through a function of all its sheets.
#'
#' Reads every sheet of `path` into a named list of data frames, passes that list
#' to `edit`, and writes the result back. Keeping the whole workbook in the round
#' trip is what makes a variant a *single* change: writing only the edited sheet
#' would silently drop the workbook's other sheets, so the variant would differ
#' from the base in more than the one dimension it means to.
#'
#' Note that this loses the original cell formatting (it is a `readxl` read
#' followed by a `writexl` write), which no importer behaviour depends on. A
#' column's *type* does survive, so an edit that replaces a numeric column with a
#' character one does store text cells.
editWorkbookSheets <- function(path, edit) {
  sheetNames <- readxl::excel_sheets(path)
  sheets <- lapply(sheetNames, function(sheet) {
    as.data.frame(
      readxl::read_excel(path, sheet = sheet, .name_repair = "minimal"),
      check.names = FALSE
    )
  })
  writexl::write_xlsx(edit(stats::setNames(sheets, sheetNames)), path)
  invisible(path)
}

#' Import a copied legacy Excel project and load the result.
#'
#' @returns A list of
#'   - `project`: the loaded `Project`.
#'   - `outputDir`: where the JSON project was written (removed when the calling
#'     test finishes), for assertions about which asset folders travelled.
#'   - `warnings`: every warning message the import raised.
#'
#' The warnings are collected rather than suppressed because most of what these
#' tests pin is *silence*: an import that drops something without a word. A test
#' asserts that no collected warning mentions its own subject, which starts
#' failing as soon as a fix adds one. Gating on the subject rather than using a
#' bare `expect_no_warning()` keeps each test's assertion about its own defect,
#' since a variant may legitimately raise an unrelated warning (a skipped sheet,
#' an unresolved cross-reference) that has nothing to do with what it pins.
importLegacyExcelProject <- function(
  projectDir,
  silent = TRUE,
  ...,
  envir = parent.frame()
) {
  outputDir <- withr::local_tempdir("LegacyOut_", .local_envir = envir)
  warnings <- character()
  collect <- function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
  jsonPath <- withCallingHandlers(
    importProjectFromExcel(
      legacyExcelProjectPath(projectDir),
      outputDir = outputDir,
      silent = silent,
      ...
    ),
    warning = collect
  )
  project <- withCallingHandlers(loadProject(jsonPath), warning = collect)
  list(project = project, outputDir = outputDir, warnings = warnings)
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
