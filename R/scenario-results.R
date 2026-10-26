# Save / load scenario results ----

#' Save results of scenario simulations to csv.
#'
#' @param simulatedScenariosResults Named list with `simulation`, `results`, `outputValues`,
#' and `population` as produced by `runScenarios()`.
#' @param project A `Project` object (loaded with `loadProject()`) providing
#' the `outputFolder` used to derive the default destination.
#' @param outputFolder Optional - path to the folder where the results will be
#' stored. If `NULL` (default), a sub-folder in
#' `project$paths$outputFolder/SimulationResults/<DateSuffix>`.
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
  project,
  outputFolder = NULL,
  saveSimulationsToPKML = TRUE
) {
  validateIsLogical(saveSimulationsToPKML)

  outputFolder <- outputFolder %||%
    file.path(
      project$paths$outputFolder,
      "SimulationResults",
      format(Sys.time(), "%F %H-%M")
    )

  # Guard up front against distinct scenario names that collapse to the same
  # file-safe name (e.g. "A/B" and "A_B" both become "A_B"): their csv/pkml
  # files would silently overwrite each other. Abort before writing anything.
  scenarioNames <- names(simulatedScenariosResults)
  safeNames <- gsub("[\\\\/]", "_", scenarioNames)
  colliding <- unique(scenarioNames[
    duplicated(safeNames) |
      duplicated(
        safeNames,
        fromLast = TRUE
      )
  ])
  if (length(colliding) > 0L) {
    cli::cli_abort(messages$scenarioResultNameCollision(colliding))
  }

  for (i in seq_along(simulatedScenariosResults)) {
    results <- simulatedScenariosResults[[i]]$results
    scenarioName <- names(simulatedScenariosResults)[[i]]

    # Replace "\" and "/" by "_" so the file name does not result in folders.
    # The sanitized form is used only for file paths; the original is preserved
    # so error messages report the name the user actually passed.
    scenarioNameForPath <- gsub("[\\\\/]", "_", scenarioName)

    outputPath <- file.path(outputFolder, paste0(scenarioNameForPath, ".csv"))
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
            paste0(scenarioNameForPath, ".pkml")
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
              paste0(scenarioNameForPath, "_population.csv")
            )
          )
        }
        # Save results
        ospsuite::exportResultsToCSV(results = results, filePath = outputPath)
      },
      error = function(cond) {
        cli::cli_warn(
          messages$savingScenarioResult(scenarioName = scenarioName),
          parent = cond
        )
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
#' @param scenarios Names of simulated scenarios
#' @param resultsFolder Path to the folder where simulation results as csv and
#' the corresponding simulations as pkml are located.
#'
#' @details This function requires simulation results AND the corresponding
#' simulation files being located in the same folder (`resultsFolder`) and have
#' the names of the scenarios. It checks that `<scenario>.pkml` and
#' `<scenario>.csv` exist for every scenario before it loads any of them, and
#' stops with an error that names the scenario and the missing files.
#'
#' @returns A named list keyed by scenario name. Each entry mirrors the record
#' produced by `runScenarios()`: `simulation` (the initialized `Simulation`
#' object with applied parameters), `results` (the `SimulationResults` object
#' reloaded from csv), `outputValues` (the output values of every output path
#' saved in the csv), and `population` (the `Population` reloaded from
#' `<scenario>_population.csv` for population scenarios, or `NULL` for individual
#' scenarios).
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
#'   scenarios = names(project$definitions$scenarios),
#'   resultsFolder = resultsFolder
#' )
#' }
loadScenarioResults <- function(scenarios, resultsFolder) {
  # Check every expected file before loading any. The backend fails on a
  # missing file with a raw .NET exception that names neither the scenario nor
  # the files this function expects.
  if (!dir.exists(resultsFolder)) {
    cli::cli_abort(messages$scenarioResultsFolderNotFound(resultsFolder))
  }
  for (scenarioName in scenarios) {
    expectedFiles <- paste0(
      gsub("[\\\\/]", "_", scenarioName),
      c(".pkml", ".csv")
    )
    missingFiles <- expectedFiles[
      !file.exists(file.path(resultsFolder, expectedFiles))
    ]
    if (length(missingFiles) > 0L) {
      cli::cli_abort(messages$scenarioResultFilesNotFound(
        scenarioName,
        missingFiles,
        resultsFolder
      ))
    }
  }

  simulatedScenariosResults <- list()
  for (i in seq_along(scenarios)) {
    scenarioName <- scenarios[[i]]
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

    outputValues <- getOutputValues(
      results,
      quantitiesOrPaths = results$allQuantityPaths,
      population = population,
      addMetaData = FALSE
    )
    simulatedScenariosResults[[scenarios[[i]]]] <-
      list(
        simulation = simulation,
        results = results,
        outputValues = outputValues,
        population = population
      )
  }

  return(simulatedScenariosResults)
}
