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
#                         Reached via testProjectExcelPath() (the entry
#                         .xlsx) and testProjectExcelConfigurationsPath()
#                         (its Configurations/ folder).
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

#' Path to an Excel side-car in the legacy Excel fixture's
#' `Configurations/` folder.
testProjectExcelConfigurationsPath <- function(...) {
  normalizePath(
    testthat::test_path(
      "data",
      "TestProjectExcel",
      "Configurations",
      ...
    ),
    mustWork = TRUE
  )
}

executeWithTestFile <- function(actionWithFile) {
  # Tie the temp file's lifetime to the calling test's frame so it is removed
  # even if `actionWithFile()` errors.
  newFile <- withr::local_tempfile(.local_envir = parent.frame())
  actionWithFile(newFile)
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
.redactTmpDir <- function(lines) {
  # A quoted path ending in the snapshot extension keeps that suffix; any other
  # quoted absolute path collapses to a bare placeholder.
  lines <- gsub("'/[^']*\\.esqlabsR'", "'<tmp-path>.esqlabsR'", lines)
  gsub("'/[^']*'", "'<tmp-path>'", lines)
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

# Creates a minimal valid set of PI Excel sheets (all 5 sheets) for a single
# task, suitable as a base for tests that manipulate specific fields.
createValidPISheets <- function() {
  list(
    PIOutputMappings = data.frame(
      PITaskName = "Task1",
      Scenarios = "PITestScenario",
      OutputPath = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      ObservedDataSheet = "Laskin 1982.Group A",
      DataSet = "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_",
      Scaling = "log",
      xOffset = NA,
      yOffset = NA,
      xFactor = NA,
      yFactor = NA,
      Weight = NA
    ),
    PIParameters = data.frame(
      PITaskName = "Task1",
      Scenarios = "PITestScenario",
      `Container Path` = "Aciclovir",
      `Parameter Name` = "Lipophilicity",
      Units = "Log Units",
      MinValue = -2,
      MaxValue = 2,
      StartValue = -0.1,
      Group = NA,
      check.names = FALSE
    ),
    PIConfiguration = data.frame(
      PITaskName = "Task1",
      Algorithm = "BOBYQA",
      CIMethod = "hessian",
      PrintEvaluationFeedback = TRUE,
      AutoEstimateCI = FALSE,
      numberOfCores = NA_real_,
      checkForNegativeValues = NA,
      ObjectiveFunctionType = NA,
      ResidualWeightingMethod = NA,
      RobustMethod = NA,
      ScaleVar = NA,
      LinScaleCV = NA,
      LogScaleSD = NA
    ),
    AlgorithmOptions = data.frame(
      PITaskName = character(0),
      OptionName = character(0),
      OptionValue = character(0)
    ),
    CIOptions = data.frame(
      PITaskName = character(0),
      OptionName = character(0),
      OptionValue = character(0)
    )
  )
}

# Builds a minimal in-memory `Project` for validation/serialization tests:
# all section fields default to empty, and `...` overrides named fields so a
# test can target one section without loading the full TestProject fixture.
# The section accessors are read-only from the handle, so sections are written
# through the internal `.setSection()` entry point the authoring functions use
# (this is test setup standing in for an authoring call, not end-user code).
.fakeProject <- function(...) {
  project <- Project$new()
  project$schemaVersion <- "2.0"
  project$esqlabsRVersion <- NA_character_
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
    project$.setSection(section, list())
  }
  overrides <- list(...)
  for (nm in names(overrides)) {
    project$.setSection(nm, overrides[[nm]])
  }
  project
}
