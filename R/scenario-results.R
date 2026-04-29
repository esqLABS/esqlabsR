# Export / import scenario results ----

#' Export results of scenario simulations to csv.
#'
#' @param simulatedScenariosResults Named list with `simulation`, `results`, `outputValues`,
#' and `population` as produced by `runScenarios()`.
#' @param project An instance of `Project`
#' @param outputFolder Optional - path to the folder where the results will be
#' stored. If `NULL` (default), a sub-folder in
#' `Project$outputFolder/SimulationResults/<DateSuffix>`.
#' @param saveSimulationsToPKML If `TRUE` (default), simulations corresponding to
#' the results are saved to PKML along with the results.
#'
#' @details For each scenario, a separate csv file will be created. If the scenario
#' is a population simulation, a population is stored along with the results with
#' the file name suffix `_population`. Results can be read with the `importScenarioResults()` function.
#'
#' @export
#'
#' @returns `outputFolder` or the created output folder path, if no `outputFolder` was provided.
#'
#' @examples \dontrun{
#' project <- loadProject("path/to/Project.json")
#' simulatedScenariosResults <- runScenarios(
#'   project = project
#' )
#' exportScenarioResults(simulatedScenariosResults, project)
#' }
exportScenarioResults <- function(
  simulatedScenariosResults,
  project,
  outputFolder = NULL,
  saveSimulationsToPKML = TRUE
) {
  validateIsLogical(saveSimulationsToPKML)

  outputFolder <- outputFolder %||%
    file.path(
      project$outputFolder,
      "SimulationResults",
      format(Sys.time(), "%F %H-%M")
    )

  for (i in seq_along(simulatedScenariosResults)) {
    results <- simulatedScenariosResults[[i]]$results
    scenarioName <- names(simulatedScenariosResults)[[i]]

    # Replace "\" and "/" by "_" so the file name does not result in folders
    scenarioName <- gsub("[\\\\/]", "_", scenarioName)

    outputPath <- file.path(outputFolder, paste0(scenarioName, ".csv"))
    tryCatch(
      {
        # Create a new folder if it does not exist
        if (!dir.exists(paths = outputFolder)) {
          dir.create(path = outputFolder, recursive = TRUE)
        }
        # Save simulations
        if (saveSimulationsToPKML) {
          outputPathSim <- file.path(
            outputFolder,
            paste0(scenarioName, ".pkml")
          )
          ospsuite::saveSimulation(
            simulation = simulatedScenariosResults[[i]]$simulation,
            filePath = outputPathSim
          )
        }
        # Save population
        if (isOfType(simulatedScenariosResults[[i]]$population, "Population")) {
          ospsuite::exportPopulationToCSV(
            simulatedScenariosResults[[i]]$population,
            filePath = file.path(
              outputFolder,
              paste0(scenarioName, "_population.csv")
            )
          )
        }
        # Save results
        ospsuite::exportResultsToCSV(results = results, filePath = outputPath)
      },
      error = function(cond) {
        warning(paste0("Cannot save to path '", outputFolder, "'"))
        message("Original error message:")
        message(cond)
      }
    )
  }
  return(outputFolder)
}

#' Import simulated scenarios from csv and pkml.
#'
#' @param scenarioNames Names of simulated scenarios
#' @param resultsFolder Path to the folder where simulation results as csv and
#' the corresponding simulations as pkml are located.
#'
#' @details This function requires simulation results AND the corresponding
#' simulation files being located in the same folder (`resultsFolder`) and have
#' the names of the scenarios.
#'
#' @returns A named list, where the names are scenario names, and the values are
#' lists with the entries `simulation` being the initialized `Simulation` object with applied parameters,
#' `results` being `SimulatioResults` object produced by running the simulation,
#' and `outputValues` the output values of the `SimulationResults`.
#'
#' @export
#'
#' @examples \dontrun{
#' # First simulate scenarios and save the results
#' project <- loadProject("path/to/Project.json")
#' simulatedScenariosResults <- runScenarios(
#'   project = project
#' )
#' exportScenarioResults(simulatedScenariosResults, project)
#'
#' # Now load the results
#' simulatedScenariosResults <- importScenarioResults(
#'   scenarioNames = c("Scenario1", "Scenario2"),
#'   resultsFolder = pathToTheFolder
#' )
#' }
importScenarioResults <- function(scenarioNames, resultsFolder) {
  simulatedScenariosResults <- list()
  for (i in seq_along(scenarioNames)) {
    scenarioName <- scenarioNames[[i]]
    # Replace "\" and "/" by "_" so the file name does not result in folders.
    # Used only for loading the results, the name of the scenario is not changed.
    scenarioNameForPath <- gsub("[\\\\/]", "_", scenarioName)

    simulation <- loadSimulation(paste0(
      resultsFolder,
      "/",
      scenarioNameForPath,
      ".pkml"
    ))

    results <- importResultsFromCSV(
      simulation = simulation,
      filePaths = paste0(resultsFolder, "/", scenarioNameForPath, ".csv")
    )

    outputValues <- getOutputValues(
      results,
      quantitiesOrPaths = results$allQuantityPaths,
      addMetaData = FALSE
    )
    simulatedScenariosResults[[scenarioNames[[i]]]] <-
      list(
        simulation = simulation,
        results = results,
        outputValues = outputValues
      )
  }

  return(simulatedScenariosResults)
}

#' @rdname exportScenarioResults
#' @export
saveScenarioResults <- function(
  simulatedScenariosResults,
  project,
  outputFolder = NULL,
  saveSimulationsToPKML = TRUE
) {
  lifecycle::deprecate_soft(
    what = "saveScenarioResults()",
    with = "exportScenarioResults()",
    when = "6.0.0"
  )
  exportScenarioResults(
    simulatedScenariosResults = simulatedScenariosResults,
    project = project,
    outputFolder = outputFolder,
    saveSimulationsToPKML = saveSimulationsToPKML
  )
}

#' @rdname importScenarioResults
#' @export
loadScenarioResults <- function(scenarioNames, resultsFolder) {
  lifecycle::deprecate_soft(
    what = "loadScenarioResults()",
    with = "importScenarioResults()",
    when = "6.0.0"
  )
  importScenarioResults(
    scenarioNames = scenarioNames,
    resultsFolder = resultsFolder
  )
}
