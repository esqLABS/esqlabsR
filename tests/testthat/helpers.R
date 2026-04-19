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
testProjectConfigurationPath <- function() {
  file.path(exampleDirectory("TestProject"), "ProjectConfiguration.xlsx")
}

#' Create a fresh test project configuration object
testProjectConfiguration <- function() {
  createProjectConfiguration(
    testProjectConfigurationPath(),
    ignoreVersionCheck = TRUE
  )
}

#' Get path to a file in the tests/data directory
getTestDataFilePath <- function(fileName = "") {
  testthat::test_path("../data", fileName)
}

#' Get a path inside the TestProject Configurations directory
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
#' Default is `loadFromCache=FALSE` so each call yields an isolated Simulation
#' instance; ospsuite's Simulation class does not support `$clone()`, and
#' `loadFromCache=TRUE` returns the same .NET object (mutations would leak
#' across tests).
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

#' Fresh Aciclovir simulation (from ospsuite's built-in extdata).
#'
#' Loads a new Simulation instance each call (Simulation is not cloneable);
#' the PKML parse is cheap. Use this instead of bespoke `loadSimulation()`
#' calls in sensitivity/simulation tests.
aciclovirSim <- function() {
  ospsuite::loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite"),
    loadFromCache = FALSE,
    addToCache = FALSE
  )
}

# Shared computed results -----------------------------------------------------

#' Accessor for the shared sensitivity calculation result (set in setup.R)
#'
#' Treat as read-only: sensitivity results are not cloned because the object
#' contains deeply nested simulation references. Snapshot tests only read from
#' `$pkData` and friends.
aciclovirSaResults <- function() .testFixtures$aciclovirSaResults

aciclovirSaOutputPath <- function() .testFixtures$aciclovirSaOutputPath
aciclovirSaParameterPaths <- function() .testFixtures$aciclovirSaParameterPaths
aciclovirSaVariationRange <- function() .testFixtures$aciclovirSaVariationRange

# Shared TestProject scenarios ------------------------------------------------

#' Accessor for shared simulatedScenarios (TestScenario + PopulationScenario)
testSimulatedScenarios <- function() .testFixtures$testSimulatedScenarios

#' Accessor for shared observed data loaded from the TestProject
aciclovirObsData <- function() .testFixtures$aciclovirObsData

# Fixtures --------------------------------------------------------------------

#' Create a temporary project location for testing
#'
#' Copies the example TestProject into a temp directory and loads its
#' ProjectConfiguration. Cleanup is scheduled via `withr::defer()` in the
#' caller's frame.
with_temp_project <- function(projectName = NULL, overwrite = TRUE) {
  if (is.null(projectName)) {
    temp_dir <- tempfile("esqlabsR_test_")
  } else {
    temp_dir <- tempfile(paste0("esqlabsR_", projectName, "_"))
  }
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(temp_dir, recursive = TRUE), envir = parent.frame())

  initProject(destination = temp_dir, overwrite = overwrite)

  project_config <- createProjectConfiguration(
    file.path(temp_dir, "ProjectConfiguration.xlsx"),
    ignoreVersionCheck = TRUE
  )

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

#' Copy the example project into a test-local temp dir (auto-cleaned).
#'
#' Unlike `with_temp_project()`, this does not call `initProject()` — it
#' clones the example directory directly, preserving any extra files like
#' `ProjectConfiguration.json`. Used by config-json tests that need the full
#' project layout on disk.
local_test_project <- function(
  project_name = "TestProject",
  env = parent.frame()
) {
  temp_dir <- withr::local_tempdir("test_project", .local_envir = env)
  example_dir <- exampleDirectory(project_name)
  file.copy(
    list.files(example_dir, full.names = TRUE),
    temp_dir,
    recursive = TRUE
  )
  list(
    dir = temp_dir,
    project_config_path = file.path(temp_dir, "ProjectConfiguration.xlsx"),
    snapshot_path = file.path(temp_dir, "ProjectConfiguration.json"),
    configurations_dir = file.path(temp_dir, "Configurations")
  )
}

# Summarizers / plot utilities ------------------------------------------------

#' Summarize sensitivity calculation data by parameter path
summarizer <- function(data, path) {
  data <- dplyr::filter(data, ParameterPath %in% path)

  list(
    "charColumnSummary" = dplyr::select(data, where(is.character)) |>
      purrr::map_dfr(unique),
    "numericColumnSummary" = dplyr::select(data, where(is.numeric)) |>
      purrr::map_df(summary, .id = "column")
  )
}

#' Extract x/y axis ranges from a list of plots
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

  axisRanges
}
