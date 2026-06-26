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

#' Load the canonical test `Project`.
testProject <- function() {
  loadProject(testthat::test_path("data", "TestProject", "Project.json"))
}

#' Load the bundled example `Project`.
exampleProject <- function() {
  loadProject(exampleProjectPath())
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
  newFile <- tempfile()
  actionWithFile(newFile)
  file.remove(newFile)
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
  if (is.null(projectName)) {
    temp_dir <- tempfile("esqlabsR_test_")
  } else {
    temp_dir <- tempfile(paste0("esqlabsR_", projectName, "_"))
  }
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(temp_dir, recursive = TRUE), envir = parent.frame())

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
.fakeProject <- function(...) {
  project <- Project$new()
  project$schemaVersion <- "2.0"
  project$esqlabsRVersion <- NA_character_
  project$outputPaths <- list()
  project$scenarios <- list()
  project$modelParameterSets <- list()
  project$individualParameterSets <- list()
  project$applicationParameterSets <- list()
  project$individuals <- list()
  project$populations <- list()
  project$applications <- list()
  project$observedData <- list()
  project$plots <- NULL
  overrides <- list(...)
  for (nm in names(overrides)) {
    project[[nm]] <- overrides[[nm]]
  }
  project
}
