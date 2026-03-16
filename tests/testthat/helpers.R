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
  createProjectConfiguration(testProjectConfigurationPath())
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
      purrr::map_df(summary, .id = "column")
  )
}

#' Create a temporary project location for testing
#'
#' @description
#' Creates a temporary directory with an initialized esqlabsR project for testing.
#' Uses `withr::local_tempdir()` to ensure proper cleanup after the test.
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
  project_config <- createProjectConfiguration(file.path(
    temp_dir,
    "ProjectConfiguration.xlsx"
  ))

  # Return list with path and config
  list(
    path = temp_dir,
    config = project_config
  )
}

#' Set up common simulated scenarios and observed data for plot tests
#'
#' @description
#' Creates and returns the common test setup shared between
#' `test-create-plots-from-excel.R` and `test-utilities-data-combined.R`.
#' This avoids duplicating the expensive scenario-running code across test files.
#'
#' @returns A list containing:
#'   - `projectConfiguration`: ProjectConfiguration object
#'   - `scenarioNames`: Vector of scenario names
#'   - `outputPaths`: Output paths string
#'   - `simulatedScenarios`: List of simulated scenarios
#'   - `observedData`: List of observed data sets
#'
#' @examples
#' \dontrun{
#' setup <- setupTestSimulatedScenarios()
#' simulatedScenarios <- setup$simulatedScenarios
#' observedData <- setup$observedData
#' }
setupTestSimulatedScenarios <- function() {
  projectConfiguration <- testProjectConfiguration()
  scenarioNames <- c("TestScenario", "PopulationScenario")
  outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )

  for (scenarioConfiguration in scenarioConfigurations) {
    scenarioConfiguration$outputPaths <- outputPaths
  }

  scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations)
  simulatedScenarios <- runScenarios(scenarios = scenarios)

  importerConfiguration <- ospsuite::loadDataImporterConfiguration(
    configurationFilePath = projectConfiguration$dataImporterConfigurationFile
  )

  observedData <- esqlabsR::loadObservedData(
    projectConfiguration = projectConfiguration,
    sheets = "Laskin 1982.Group A",
    importerConfiguration = importerConfiguration
  )

  list(
    projectConfiguration = projectConfiguration,
    scenarioNames = scenarioNames,
    outputPaths = outputPaths,
    simulatedScenarios = simulatedScenarios,
    observedData = observedData
  )
}

#' Create a temporary test setup with a Plots.xlsx file
#'
#' @description
#' Creates a temporary directory containing a `Plots.xlsx` file built from the
#' provided data frames, and returns a cloned `ProjectConfiguration` whose
#' `configurationsFolder` points to that directory.  The temporary directory is
#' automatically cleaned up when the calling test exits (via
#' `withr::local_tempdir`).
#'
#' @param projectConfiguration The base `ProjectConfiguration` object to clone.
#' @param dataCombinedDf Data frame for the `DataCombined` sheet.
#' @param plotConfigurationDf Data frame for the `plotConfiguration` sheet.
#' @param plotGridsDf Data frame for the `plotGrids` sheet.
#' @param exportConfigurationDf Data frame for the `exportConfiguration` sheet.
#' @param outputFolder Optional path to set as `outputFolder` on the cloned
#'   project configuration.  When `NULL` (the default) the field is left
#'   unchanged.
#' @param env The environment used for cleanup scheduling.  Defaults to
#'   `parent.frame()` so that the temporary directory is removed when the
#'   calling test finishes.
#'
#' @returns A list containing:
#'   - `tempDir`: Path to the temporary directory
#'   - `projectConfiguration`: Cloned `ProjectConfiguration` object
#'
#' @examples
#' \dontrun{
#' localDf <- dataCombinedDf
#' localDf$dataType <- NA
#' setup <- local_plots_test(
#'   projectConfiguration,
#'   dataCombinedDf = localDf,
#'   plotConfigurationDf = plotConfigurationDf,
#'   plotGridsDf = plotGridsDf,
#'   exportConfigurationDf = exportConfigurationDf
#' )
#' createPlotsFromExcel(
#'   simulatedScenarios = simulatedScenarios,
#'   observedData = observedData,
#'   projectConfiguration = setup$projectConfiguration
#' )
#' }
local_plots_test <- function(
  projectConfiguration,
  dataCombinedDf,
  plotConfigurationDf,
  plotGridsDf,
  exportConfigurationDf,
  outputFolder = NULL,
  env = parent.frame()
) {
  tempDir <- withr::local_tempdir("plots_test", .local_envir = env)
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$configurationsFolder <- tempDir
  if (!is.null(outputFolder)) {
    projectConfigurationLocal$outputFolder <- outputFolder
  }

  .writeExcel(
    data = list(
      "DataCombined" = dataCombinedDf,
      "plotConfiguration" = plotConfigurationDf,
      "plotGrids" = plotGridsDf,
      "exportConfiguration" = exportConfigurationDf
    ),
    path = file.path(tempDir, "Plots.xlsx")
  )

  list(
    tempDir = tempDir,
    projectConfiguration = projectConfigurationLocal
  )
}

# Create a temporary test project directory with proper cleanup
# This is a test fixture following the pattern from testthat.r-lib.org/articles/test-fixtures.html
# Returns a list with paths to the project directory and key files
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
