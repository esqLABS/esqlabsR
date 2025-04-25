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

testProjectConfigurationPath <- function() {
  # for now it targets TestProject as it is both an example and a test project
  file.path(exampleDirectory("TestProject"), "ProjectConfiguration.xlsx")
}

testProjectConfiguration <- function() {
  createProjectConfiguration(testProjectConfigurationPath())
}

testConfigurationsPath <- function(...) {
  normalizePath(
    file.path(exampleDirectory("TestProject"), "Configurations", ...),
    mustWork = TRUE
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
    json_path = file.path(temp_dir, "config.json"),
    configurations_dir = file.path(temp_dir, "Configurations")
  )
}
