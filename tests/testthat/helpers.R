getTestDataFilePath <- function(fileName = "") {
  testthat::test_path("../data", fileName)
}

getSimulationFilePath <- function(simulationName) {
  getTestDataFilePath(paste0(simulationName, ".pkml"))
}

# Helper function to load a model easily. In the test environment, we do not want to load from cache by default. Instead
# new instances should be created unless specifically specified otherwise
loadTestSimulation <- function(simulationName, loadFromCache = FALSE, addToCache = TRUE) {
  simFile <- getSimulationFilePath(simulationName)
  sim <- ospsuite::loadSimulation(simFile, loadFromCache = loadFromCache, addToCache = addToCache)
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
  normalizePath(file.path(exampleDirectory("TestProject"), "Configurations", ...), mustWork = T)
}

getTestProject <- function() {
  .testProject <- NULL

  function() {
    if (is.null(.testProject)) {
      .testProject <- Project$new(projectConfiguration = testProjectConfiguration())
    }

    return(.testProject)
  }
}

testProject <- getTestProject()
