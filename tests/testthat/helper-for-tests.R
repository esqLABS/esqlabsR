
getTestDataFilePath <- function(fileName) {
  dataPath <- file.path(getwd(), "..", "data", fsep = .Platform$file.sep)
  file.path(dataPath, fileName, fsep = .Platform$file.sep)
}

getSimulationFilePath <- function(simulationName) {
  getTestDataFilePath(paste0(simulationName, ".pkml"))
}

# Helper function to load a model easily. In the test environment, we do not want to load from cache by default. Instead
# new instances should be created unless specifically specified otherwise
loadTestSimulation <- function(simulationName, loadFromCache = FALSE, addToCache = TRUE) {
  simFile <- getSimulationFilePath(simulationName)
  sim <- loadSimulation(simFile, loadFromCache = loadFromCache, addToCache = addToCache)
}

executeWithTestFile <- function(actionWithFile) {
  newFile <- tempfile()
  actionWithFile(newFile)
  file.remove(newFile)
}


#' Escape a string for possible regular expression match
#'
#' @param string String to be escaped
#'
#' @return string with prefix "\\Q" and suffix "\\E" appended.
#' The resulting string will not be recognized as a regular expression pattern.
escapeForRegex <- function(string) {
  paste0("\\Q", string, "\\E")
}
