# Save / load scenario results ----

#' Save results of scenario simulations to csv.
#'
#' @param simulatedScenariosResults Named list with `simulation`, `results`, `outputValues`,
#' and `population` as produced by `runScenarios()`.
#' @param projectConfiguration A `Project` object (loaded with `loadProject()`)
#' providing the `outputFolder` used to derive the default destination. The
#' argument keeps its historical name for backward compatibility.
#' @param outputFolder Optional - path to the folder where the results will be
#' stored. If `NULL` (default), a sub-folder in
#' `project$outputFolder/SimulationResults/<DateSuffix>`.
#' @param saveSimulationsToPKML If `TRUE` (default), simulations corresponding to
#' the results are saved to PKML along with the results.
#'
#' @details For each scenario, a separate csv file will be created. If the scenario
#' is a population simulation, a population is stored along with the results with
#' the file name suffix `_population`. Results can be read with the `loadScenarioResults()` function.
#'
#' @export
#'
#' @returns `outputFolder` or the created output folder path, if no `outputFolder` was provided.
#'
#' @examples \dontrun{
#' project <- loadProject("Project.json")
#' simulatedScenariosResults <- runScenarios(project)
#' saveScenarioResults(simulatedScenariosResults, project)
#' }
saveScenarioResults <- function(
  simulatedScenariosResults,
  projectConfiguration,
  outputFolder = NULL,
  saveSimulationsToPKML = TRUE
) {
  validateIsLogical(saveSimulationsToPKML)

  outputFolder <- outputFolder %||%
    file.path(
      projectConfiguration$outputFolder,
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
        cli::cli_warn(messages$errorSavingScenarioResult(
          scenarioName = scenarioName,
          conditionMessage = conditionMessage(cond)
        ))
      },
      warning = function(cond) {
        warning(cond)
      }
    )
  }
  return(outputFolder)
}

#' Load simulated scenarios from csv and pkml.
#'
#' @param scenarioNames Names of simulated scenarios
#' @param resultsFolder Path to the folder where simulation results as csv and
#' the corresponding simulations as pkml are located.
#'
#' @details This function requires simulation results AND the corresponding
#' simulation files being located in the same folder (`resultsFolder`) and have
#' the names of the scenarios.
#'
#' @returns A named list keyed by scenario name. Each entry mirrors the record
#' produced by `runScenarios()`: `simulation` (the initialized `Simulation`
#' object with applied parameters), `results` (the `SimulationResults` object
#' reloaded from csv), `outputValues` (the output values extracted for the
#' simulation's recorded output paths), and `population` (the `Population`
#' reloaded from `<scenario>_population.csv` for population scenarios, or `NULL`
#' for individual scenarios).
#'
#' @export
#'
#' @examples \dontrun{
#' # First simulate scenarios and save the results
#' project <- loadProject("Project.json")
#' simulatedScenariosResults <- runScenarios(project)
#' resultsFolder <- saveScenarioResults(simulatedScenariosResults, project)
#'
#' # Now load the results
#' simulatedScenariosResults <- loadScenarioResults(
#'   scenarioNames = names(project$scenarios),
#'   resultsFolder = resultsFolder
#' )
#' }
loadScenarioResults <- function(scenarioNames, resultsFolder) {
  simulatedScenariosResults <- list()
  for (i in seq_along(scenarioNames)) {
    scenarioName <- scenarioNames[[i]]
    # Replace "\" and "/" by "_" so the file name does not result in folders.
    # Used only for loading the results, the name of the scenario is not changed.
    scenarioNameForPath <- gsub("[\\\\/]", "_", scenarioName)

    simulation <- loadSimulation(
      file.path(resultsFolder, paste0(scenarioNameForPath, ".pkml"))
    )

    results <- importResultsFromCSV(
      simulation = simulation,
      filePaths = file.path(resultsFolder, paste0(scenarioNameForPath, ".csv"))
    )

    # Restore the population if a population csv was written for this scenario.
    populationPath <- file.path(
      resultsFolder,
      paste0(scenarioNameForPath, "_population.csv")
    )
    population <- NULL
    if (file.exists(populationPath)) {
      population <- loadPopulation(populationPath)
    }

    # Extract output values for the simulation's recorded output paths and pass
    # the population so the metadata columns match what runScenarios() produced.
    outputQuantities <- getAllQuantitiesMatching(
      results$allQuantityPaths,
      simulation
    )
    outputValues <- getOutputValues(
      results,
      quantitiesOrPaths = outputQuantities,
      population = population,
      addMetaData = FALSE
    )
    simulatedScenariosResults[[scenarioNames[[i]]]] <-
      list(
        simulation = simulation,
        results = results,
        outputValues = outputValues,
        population = population
      )
  }

  return(simulatedScenariosResults)
}
