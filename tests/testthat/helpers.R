# =============================================================================
# Test Helper Functions
# =============================================================================

# Shared test fixtures populated by setup.R. Declaring the environment here
# means every helper function below captures a reference to it via lexical
# scope, regardless of which environment setup.R ends up running in under
# parallel testthat (package namespace vs. attached env).
.testFixtures <- new.env(parent = emptyenv())

# Paths -----------------------------------------------------------------------

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

#' Create test project configuration
#'
#' @description
#' Creates a ProjectConfiguration object from the test project configuration file.
#'
#' @returns ProjectConfiguration object for testing.
#'
#' @examples
#' \dontrun{
#' config <- testProjectConfiguration()
#' }
testProjectConfiguration <- function() {
  createProjectConfiguration(
    testProjectConfigurationPath(),
    ignoreVersionCheck = TRUE
  )
}

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

getSimulationFilePath <- function(simulationName) {
  getTestDataFilePath(paste0(simulationName, ".pkml"))
}

# Simulation loaders ----------------------------------------------------------

#' Load a test simulation by name (from tests/data/<name>.pkml)
#'
#' @description
#' Loads a simulation from the tests/data directory by name. Default is
#' `loadFromCache=FALSE` so each call yields an isolated Simulation instance;
#' ospsuite's Simulation class does not support `$clone()`, and
#' `loadFromCache=TRUE` returns the same .NET object (mutations would leak
#' across tests).
#'
#' @param simulationName Name of the simulation (without .pkml extension).
#' @param loadFromCache Whether to load the simulation from cache. Defaults to FALSE
#'   so each call yields an isolated Simulation instance.
#' @param addToCache Whether to add the simulation to the cache. Defaults to TRUE.
#'
#' @returns A `Simulation` object.
#'
#' @examples
#' \dontrun{
#' simulation <- loadTestSimulation("simple")
#' }
loadTestSimulation <- function(
  simulationName,
  loadFromCache = FALSE,
  addToCache = TRUE
) {
  ospsuite::loadSimulation(
    getSimulationFilePath(simulationName),
    loadFromCache = loadFromCache,
    addToCache = addToCache
  )
}

#' Fresh Aciclovir simulation (from ospsuite's built-in extdata)
#'
#' @description
#' Loads a new Simulation instance each call (Simulation is not cloneable);
#' the PKML parse is cheap. Use this instead of bespoke `loadSimulation()`
#' calls in sensitivity/simulation tests.
#'
#' @returns A fresh `Simulation` object for the Aciclovir model.
#'
#' @examples
#' \dontrun{
#' simulation <- aciclovirSim()
#' }
aciclovirSim <- function() {
  ospsuite::loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite"),
    loadFromCache = FALSE,
    addToCache = FALSE
  )
}

# Shared computed results -----------------------------------------------------

#' Accessor for the shared sensitivity calculation result
#'
#' @description
#' Returns the shared `SensitivityCalculation` result precomputed in `setup.R`.
#' Treat as read-only: sensitivity results are not cloned because the object
#' contains deeply nested simulation references. Snapshot tests only read from
#' `$pkData` and friends.
#'
#' @returns Shared sensitivity calculation result populated by `setup.R`.
#'
#' @examples
#' \dontrun{
#' results <- aciclovirSaResults()
#' }
aciclovirSaResults <- function() .testFixtures$aciclovirSaResults

#' Accessor for the shared Aciclovir sensitivity output path
#'
#' @returns Single output path string used across sensitivity tests.
#'
#' @examples
#' \dontrun{
#' outputPaths <- aciclovirSaOutputPath()
#' }
aciclovirSaOutputPath <- function() .testFixtures$aciclovirSaOutputPath

#' Accessor for the shared Aciclovir sensitivity parameter paths
#'
#' @returns Character vector of parameter paths used across sensitivity tests.
#'
#' @examples
#' \dontrun{
#' parameterPaths <- aciclovirSaParameterPaths()
#' }
aciclovirSaParameterPaths <- function() .testFixtures$aciclovirSaParameterPaths

#' Accessor for the shared Aciclovir sensitivity variation range
#'
#' @returns Numeric vector of variation factors used across sensitivity tests.
#'
#' @examples
#' \dontrun{
#' variationRange <- aciclovirSaVariationRange()
#' }
aciclovirSaVariationRange <- function() .testFixtures$aciclovirSaVariationRange

# Shared TestProject scenarios ------------------------------------------------

#' Accessor for shared `simulatedScenarios` (TestScenario + PopulationScenario)
#'
#' @description
#' Returns the list of simulated scenarios precomputed in `setup.R` for the
#' TestProject. The outer list is value-typed in R, so replacing top-level
#' slots is safe — but the contained `Simulation`/`SimulationResults` objects
#' are R6 references; mutating them leaks across tests in the same worker.
#'
#' @returns Named list of scenario results (one element per scenario name).
#'
#' @examples
#' \dontrun{
#' simulatedScenarios <- testSimulatedScenarios()
#' }
testSimulatedScenarios <- function() .testFixtures$testSimulatedScenarios

#' Accessor for shared observed data loaded from the TestProject
#'
#' @description
#' Returns the observed data (sheet "Laskin 1982.Group A") loaded once per
#' worker in `setup.R`.
#'
#' @returns List of `DataSet` objects keyed by name.
#'
#' @examples
#' \dontrun{
#' observedData <- aciclovirObsData()
#' }
aciclovirObsData <- function() .testFixtures$aciclovirObsData

# Fixtures --------------------------------------------------------------------

#' Create a temporary project location for testing
#'
#' @description
#' Creates a temporary directory with an initialized esqlabsR project for testing.
#' Cleanup is scheduled via `withr::defer()` in the caller's frame.
#'
#' @param projectName Optional name for the project. If provided, uses this name in the temporary directory pattern.
#' @param overwrite Whether to overwrite existing project files. Defaults to TRUE.
#'
#' @returns A list containing:
#'   - `path`: Path to the temporary project directory
#'   - `config`: ProjectConfiguration object for the project
#'
#' @examples
#' \dontrun{
#' # Create temporary project with random name
#' temp_project <- with_temp_project()
#'
#' # Create temporary project with specific name
#' temp_project <- with_temp_project("MyTestProject")
#'
#' # Use the project
#' project_path <- temp_project$path
#' project_config <- temp_project$config
#'
#' # Project will be automatically cleaned up when the function exits
#' }
with_temp_project <- function(projectName = NULL, overwrite = TRUE) {
  # Generate a unique temp directory path
  if (is.null(projectName)) {
    temp_dir <- tempfile("esqlabsR_test_")
  } else {
    temp_dir <- tempfile(paste0("esqlabsR_", projectName, "_"))
  }
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  # Ensure cleanup after test
  withr::defer(unlink(temp_dir, recursive = TRUE), envir = parent.frame())

  # Initialize project
  initProject(destination = temp_dir, overwrite = overwrite)

  # Load project configuration
  project_config <- createProjectConfiguration(
    file.path(temp_dir, "ProjectConfiguration.xlsx"),
    ignoreVersionCheck = TRUE
  )

  # Return list with path and config
  list(
    path = temp_dir,
    config = project_config
  )
}

executeWithTestFile <- function(actionWithFile) {
  newFile <- tempfile()
  actionWithFile(newFile)
  file.remove(newFile)
}

# Create a temporary test project directory with proper cleanup.
# Unlike `with_temp_project()`, this does not call `initProject()` — it clones
# the example directory directly, preserving any extra files like
# `ProjectConfiguration.json`. Used by config-json tests that need the full
# project layout on disk. Follows the fixture pattern from
# testthat.r-lib.org/articles/test-fixtures.html.
local_test_project <- function(
  project_name = "TestProject",
  env = parent.frame()
) {
  # Create temp directory for test
  temp_dir <- withr::local_tempdir("test_project", .local_envir = env)

  # Copy example project to temp directory to avoid modifying the original
  example_dir <- exampleDirectory(project_name)
  file.copy(
    list.files(example_dir, full.names = TRUE),
    temp_dir,
    recursive = TRUE
  )

  # Return the paths needed for testing
  list(
    dir = temp_dir,
    project_config_path = file.path(temp_dir, "ProjectConfiguration.xlsx"),
    snapshot_path = file.path(temp_dir, "ProjectConfiguration.json"),
    configurations_dir = file.path(temp_dir, "Configurations")
  )
}

# Summarizers / plot utilities ------------------------------------------------

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
      purrr::map_df(summary, .id = "column")
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
