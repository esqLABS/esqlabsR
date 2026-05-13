# =============================================================================
# Test Helper Functions
# =============================================================================

#' Get path to test data file
#'
#' @description
#' Returns the full path to a file in the test data directory.
#'
#' @param fileName Name of the file in the test data directory. If empty, returns the directory path.
#'
#' @returns Full path to the test data file or directory.
#'
#' @examples
#' \dontrun{
#' # Get path to a specific test file
#' file_path <- getTestDataFilePath("test_data.xlsx")
#'
#' # Get path to test data directory
#' data_dir <- getTestDataFilePath("")
#' }
getTestDataFilePath <- function(fileName = "") {
  testthat::test_path("../data", fileName)
}

getSimulationFilePath <- function(simulationName) {
  getTestDataFilePath(paste0(simulationName, ".pkml"))
}

# Helper function to load a model easily. In the test environment, we do not want to load from cache by default. Instead
# new instances should be created unless specifically specified otherwise
loadTestSimulation <- function(
  simulationName,
  loadFromCache = FALSE,
  addToCache = TRUE
) {
  simFile <- getSimulationFilePath(simulationName)
  sim <- ospsuite::loadSimulation(
    simFile,
    loadFromCache = loadFromCache,
    addToCache = addToCache
  )
  return(sim)
}

executeWithTestFile <- function(actionWithFile) {
  newFile <- tempfile()
  actionWithFile(newFile)
  file.remove(newFile)
}

#' Get path to test project configuration
#'
#' @description
#' Returns the path to the test project configuration file.
#' Currently targets the TestProject as it serves both as an example and test project.
#'
#' @returns Full path to the test project configuration file.
#'
#' @examples
#' \dontrun{
#' config_path <- testProjectConfigurationPath()
#' }
testProjectConfigurationPath <- function() {
  # for now it targets TestProject as it is both an example and a test project
  file.path(exampleDirectory("TestProject"), "ProjectConfiguration.xlsx")
}

#' Get path to the v2.0 example `Project.json`
#'
#' @description
#' Returns the path to the bundled v2.0 example `Project.json` shipped under
#' `inst/extdata/projects/Example`. Shared between the parser and serializer
#' test suites.
#'
#' @returns Full path to the example Project.json file.
example_project_json_path <- function() {
  system.file(
    "extdata",
    "projects",
    "Example",
    "Project.json",
    package = "esqlabsR",
    mustWork = TRUE
  )
}

#' Create test project configuration
#'
#' @description
#' Creates a ProjectConfiguration object from the test project configuration file.
#'
#' @returns Project object for testing.
#'
#' @examples
#' \dontrun{
#' config <- testProjectConfiguration()
#' }
testProjectConfiguration <- function() {
  loadProject(testProjectJSONPath())
}

#' Get path to the test data directory or a subdirectory.
#'
#' @description Used by JSON-based test fixtures. Distinct from
#'   `getTestDataFilePath()`, which targets the legacy `tests/data/`
#'   directory; the JSON fixtures live under `tests/testthat/data/`.
#'
#' @param name Optional subdirectory or file name relative to the
#'   data directory.
#'
#' @returns Full path string.
testDataDirectory <- function(name = NULL) {
  directory <- testthat::test_path("data")
  if (!is.null(name)) {
    directory <- file.path(directory, name)
  }
  directory
}

#' Get path to the canonical test `Project.json`.
testProjectJSONPath <- function() {
  file.path(testDataDirectory("TestProject"), "Project.json")
}

#' Load the canonical test project.
#'
#' @returns A `Project` object loaded from `tests/testthat/data/TestProject/Project.json`.
testProject <- function() {
  loadProject(testProjectJSONPath())
}

#' Get path to test configurations directory
#'
#' @description
#' Returns the normalized path to the test configurations directory with optional subdirectories.
#'
#' @param ... Additional path components to append to the configurations directory.
#'
#' @returns Full normalized path to the test configurations directory or subdirectory.
#'
#' @examples
#' \dontrun{
#' # Get path to configurations directory
#' config_dir <- testConfigurationsPath()
#'
#' # Get path to specific configuration file
#' populations_file <- testConfigurationsPath("Populations.xlsx")
#' }
testConfigurationsPath <- function(...) {
  normalizePath(
    file.path(exampleDirectory("TestProject"), "Configurations", ...),
    mustWork = TRUE
  )
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

local_test_project <- function(
  project_name = "TestProject",
  env = parent.frame()
) {
  temp_dir <- withr::local_tempdir("test_project", .local_envir = env)

  source_dir <- testDataDirectory(project_name)
  file.copy(
    list.files(source_dir, full.names = TRUE),
    temp_dir,
    recursive = TRUE
  )

  list(
    dir = temp_dir,
    project_path = file.path(temp_dir, "Project.json"),
    configurations_dir = file.path(temp_dir, "Configurations")
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
