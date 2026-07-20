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
#' @param scenarios Names of simulated scenarios
#' @param resultsFolder Path to the folder where simulation results as csv and
#' the corresponding simulations as pkml are located.
#' @param project Optional `Project` object (loaded with `loadProject()`) whose
#' scenarios declare the output paths that were run. When supplied, the reloaded
#' `outputValues` are restricted to each scenario's declared output paths, so
#' the reloaded column set matches what `runScenarios()` produced. When `NULL`
#' (default), all output paths recorded in the csv are extracted.
#'
#' @details This function requires simulation results AND the corresponding
#' simulation files being located in the same folder (`resultsFolder`) and have
#' the names of the scenarios.
#'
#' @returns A named list keyed by scenario name. Each entry mirrors the record
#' produced by `runScenarios()`: `simulation` (the initialized `Simulation`
#' object with applied parameters), `results` (the `SimulationResults` object
#' reloaded from csv), `outputValues` (the output values extracted for the
#' scenario's declared output paths when `project` is supplied, otherwise for
#' all recorded output paths), and `population` (the `Population` reloaded from
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
#' # Now load the results, restricting to each scenario's declared output paths
#' simulatedScenariosResults <- loadScenarioResults(
#'   scenarios = names(the scenarios definitions),
#'   resultsFolder = resultsFolder,
#'   project = project
#' )
#' }
loadScenarioResults <- function(scenarios, resultsFolder, project = NULL) {
  validateIsOfType(project, "Project", nullAllowed = TRUE)

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

    # Restrict the reloaded output values to the scenario's declared output
    # paths when a project is supplied, mirroring how `runScenarios()` collects
    # results (see `.collectScenarioResult()`); otherwise fall back to every
    # output path recorded in the csv. This keeps a reload from silently gaining
    # extra series relative to the original run.
    quantitiesOrPaths <- results$allQuantityPaths
    if (!is.null(project)) {
      scenario <- project$definitions$scenarios[[scenarioName]]
      if (!is.null(scenario) && !is.null(scenario$outputPaths)) {
        quantitiesOrPaths <- getAllQuantitiesMatching(
          unname(scenario$outputPaths),
          simulation
        )
      }
    }

    outputValues <- getOutputValues(
      results,
      quantitiesOrPaths = quantitiesOrPaths,
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
