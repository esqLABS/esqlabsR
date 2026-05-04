# Legacy v6 runtime path: runs scenarios that were created via
# `createScenarios()` from `ScenarioConfiguration` / Excel. Soft-
# deprecated via `runScenarios()`'s dispatcher in this file. Body is
# a verbatim move of the v6 `runScenarios()` logic — Chapter 8 deletes.
# @keywords internal
# @noRd
.runLegacyScenarios <- function(scenarios, simulationRunOptions = NULL) {
  scenarios <- ospsuite.utils::toList(scenarios)
  individualSimulations <- list()
  populationScenarios <- list()
  steadyStateGroups <- list()
  for (scenario in scenarios) {
    if (scenario$scenarioType == "Individual") {
      individualSimulations <- c(individualSimulations, scenario$simulation)
    } else {
      populationScenarios <- c(populationScenarios, scenario)
    }
    if (scenario$scenarioConfiguration$simulateSteadyState) {
      ignoreIfFormulaKey <- as.character(
        scenario$scenarioConfiguration$overwriteFormulasInSS
      )
      if (is.null(steadyStateGroups[[ignoreIfFormulaKey]])) {
        steadyStateGroups[[ignoreIfFormulaKey]] <- list(
          simulations = list(),
          times = list()
        )
      }
      steadyStateGroups[[ignoreIfFormulaKey]]$simulations <- c(
        steadyStateGroups[[ignoreIfFormulaKey]]$simulations,
        scenario$simulation
      )
      steadyStateGroups[[ignoreIfFormulaKey]]$times <- c(
        steadyStateGroups[[ignoreIfFormulaKey]]$times,
        scenario$scenarioConfiguration$steadyStateTime
      )
    }
  }
  initialValues <- list()
  for (ignoreIfFormulaKey in names(steadyStateGroups)) {
    group <- steadyStateGroups[[ignoreIfFormulaKey]]
    ignoreIfFormula <- !as.logical(ignoreIfFormulaKey)
    groupValues <- ospsuite::getSteadyState(
      simulations = group$simulations,
      steadyStateTime = group$times,
      ignoreIfFormula = ignoreIfFormula,
      simulationRunOptions = simulationRunOptions
    )
    initialValues <- c(initialValues, groupValues)
  }
  for (ignoreIfFormulaKey in names(steadyStateGroups)) {
    for (simulation in steadyStateGroups[[ignoreIfFormulaKey]]$simulations) {
      ospsuite::setQuantityValuesByPath(
        quantityPaths = initialValues[[simulation$id]]$paths,
        values = initialValues[[simulation$id]]$values,
        simulation = simulation
      )
    }
  }
  simulationResults <- runSimulations(
    simulations = individualSimulations,
    simulationRunOptions = simulationRunOptions
  )
  for (scenario in populationScenarios) {
    populationResults <- runSimulations(
      simulations = scenario$simulation,
      population = scenario$population,
      simulationRunOptions = simulationRunOptions
    )
    simulationResults <- c(simulationResults, populationResults)
  }
  returnList <- vector("list", length(simulationResults))
  for (idx in seq_along(scenarios)) {
    scenario <- scenarios[[idx]]
    scenarioName <- scenario$scenarioConfiguration$scenarioName
    simulation <- scenario$simulation
    id <- simulation$id
    results <- simulationResults[[id]]
    population <- scenario$population
    if (
      !is.null(population) &&
        !isOfType(population, "Population") &&
        is.na(population)
    ) {
      population <- NULL
    }
    outputQuantities <- NULL
    if (!is.null(scenario$scenarioConfiguration$outputPaths)) {
      outputQuantities <- getAllQuantitiesMatching(
        scenario$scenarioConfiguration$outputPaths,
        simulation
      )
    }
    if (is.null(results)) {
      warning(messages$missingResultsForScenario(scenarioName))
      outputValues <- NULL
    } else {
      outputValues <- getOutputValues(
        results,
        quantitiesOrPaths = outputQuantities,
        population = population,
        addMetaData = FALSE
      )
    }
    returnList[[idx]] <- list(
      simulation = simulation,
      results = results,
      outputValues = outputValues,
      population = population
    )
    names(returnList)[[idx]] <- scenarioName
  }
  returnList
}

#' Run a set of scenarios from a `Project`.
#'
#' @description Loads simulations, applies parameters, runs the
#'   simulations, and collects results for one or more scenarios
#'   defined on a parsed [Project]. The project must already have been
#'   loaded with [loadProject()].
#'
#' @param project A [Project] object loaded from a `Project.json` file.
#'   In the legacy positional form, this argument also accepts a list
#'   of [LegacyScenario] objects produced by [createScenarios()] — see
#'   "Details".
#' @param scenarioNames Optional character vector of scenario names to
#'   run. `NULL` (default) runs all scenarios in the project.
#' @param customParams A list with vectors `paths`, `values`, and
#'   `units` — applied to every selected scenario as the final
#'   parameter layer.
#' @param simulationRunOptions Optional [ospsuite::SimulationRunOptions]
#'   for the simulation run. `NULL` (default) uses the package
#'   defaults.
#' @param validate Logical. Reserved for future use; currently
#'   accepted but ignored. Project validation is wired in a later
#'   chapter.
#' @param scenarios `r lifecycle::badge("deprecated")` Legacy alias
#'   for the first positional argument when passing a list of
#'   [LegacyScenario] objects. Use `project = loadProject(...)` and
#'   the modern signature instead.
#'
#' @returns A named list keyed by scenario name. Each entry is a list
#'   with `simulation` (the initialized [ospsuite::Simulation]),
#'   `results` ([ospsuite::SimulationResults]), `outputValues` (the
#'   computed output values, or `NULL` if simulation failed), and
#'   `population` (an [ospsuite::Population] for population
#'   scenarios, or `NULL` for individual scenarios).
#'
#' @details If a scenario's simulation fails, a warning is produced
#'   and `outputValues` for that scenario is `NULL`.
#'
#'   The legacy signatures
#'   `runScenarios(scenariosList, simulationRunOptions)` (positional)
#'   and `runScenarios(scenarios = ..., simulationRunOptions = ...)`
#'   (named) — taking a list of [LegacyScenario] objects produced by
#'   [createScenarios()] — are still accepted but soft-deprecated.
#'   New code should use the JSON-first form shown above.
#'
#' @export
runScenarios <- function(
  project,
  scenarioNames        = NULL,
  customParams         = NULL,
  simulationRunOptions = NULL,
  validate             = TRUE,
  scenarios            = lifecycle::deprecated()
) {
  # Legacy named-arg form.
  if (lifecycle::is_present(scenarios)) {
    lifecycle::deprecate_soft(
      when = "6.0.0",
      what = "runScenarios(scenarios = )",
      with = "runScenarios(project = )",
      details = paste(
        "Pass a Project loaded via loadProject() instead of a list of",
        "LegacyScenario objects produced by createScenarios()."
      )
    )
    return(.runLegacyScenarios(
      scenarios = scenarios,
      simulationRunOptions = simulationRunOptions
    ))
  }

  # Modern path.
  if (inherits(project, "Project")) {
    return(.runScenariosFromProject(
      project = project,
      scenarioNames = scenarioNames,
      customParams = customParams,
      simulationRunOptions = simulationRunOptions,
      validate = validate
    ))
  }

  # Legacy positional form: first arg is a scenarios list, second is
  # simulationRunOptions (so it landed in `scenarioNames` due to the
  # modern signature's positional names).
  lifecycle::deprecate_soft(
    when = "6.0.0",
    what = I("runScenarios(scenariosList)"),
    with = "runScenarios(project)",
    details = paste(
      "Pass a Project loaded via loadProject() instead of a list of",
      "LegacyScenario objects produced by createScenarios()."
    )
  )
  legacySimRunOpts <- simulationRunOptions %||% scenarioNames
  .runLegacyScenarios(
    scenarios = project,
    simulationRunOptions = legacySimRunOpts
  )
}

#' Create `Scenario` objects from `ScenarioConfiguration` objects
#'
#' @description Load simulation. Apply parameters from global XLS. Apply
#' individual physiology. Apply individual model parameters. Set simulation
#' outputs. Set simulation time. initializeSimulation(). Create population
#'
#' @param scenarioConfigurations List of `ScenarioConfiguration` objects to be
#'   simulated. See [createScenarios()] for details.
#' @param customParams A list containing vectors 'paths' with the full paths to
#'   the parameters, 'values' the values of the parameters, and 'units' with the
#'   units the values are in. The values will be applied to all scenarios.
#' @param stopIfParameterNotFound Boolean. If `TRUE` (default) and a custom
#'   parameter is not found, an error is thrown. If `FALSE`, non-existing
#'   parameters are ignored.
#'
#' @returns Named list of `Scenario` objects.
#' @export
createScenarios <- function(
  scenarioConfigurations,
  customParams = NULL,
  stopIfParameterNotFound = TRUE
) {
  .validateScenarioConfigurations(scenarioConfigurations)
  .validateParametersStructure(
    parameterStructure = customParams,
    argumentName = "customParams",
    nullAllowed = TRUE
  )

  scenarios <- purrr::map(
    scenarioConfigurations,
    ~ LegacyScenario$new(
      .x,
      customParams = customParams,
      stopIfParameterNotFound = stopIfParameterNotFound
    )
  ) |>
    purrr::set_names(purrr::map(scenarioConfigurations, ~ .x$scenarioName))

  return(scenarios)
}

#' Save results of scenario simulations to csv.
#'
#' @param simulatedScenariosResults Named list with `simulation`, `results`, `outputValues`,
#' and `population` as produced by `runScenarios()`.
#' @param projectConfiguration An instance of `ProjectConfiguration`
#' @param outputFolder Optional - path to the folder where the results will be
#' stored. If `NULL` (default), a sub-folder in
#' `ProjectConfiguration$outputFolder/SimulationResults/<DateSuffix>`.
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
#' projectConfiguration <- esqlabsR::createProjectConfiguration()
#' scenarioConfigurations <- readScenarioConfigurationFromExcel(
#'   projectConfiguration = projectConfiguration
#' )
#' scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations)
#' simulatedScenariosResults <- runScenarios(
#'   scenarios = scenarios
#' )
#' saveScenarioResults(simulatedScenariosResults, projectConfiguration)
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
        warning(paste0("Cannot save to path '", outputFolder, "'"))
        message("Original error message:")
        message(cond)
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
#' @returns A named list, where the names are scenario names, and the values are
#' lists with the entries `simulation` being the initialized `Simulation` object with applied parameters,
#' `results` being `SimulatioResults` object produced by running the simulation,
#' and `outputValues` the output values of the `SimulationResults`.
#'
#' @export
#'
#' @examples \dontrun{
#' # First simulate scenarios and save the results
#' projectConfiguration <- esqlabsR::createProjectConfiguration()
#' scenarioConfigurations <- readScenarioConfigurationFromExcel(
#'   projectConfiguration = projectConfiguration
#' )
#' scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations)
#' simulatedScenariosResults <- runScenarios(
#'   scenarios = scenarios
#' )
#' saveResults(simulatedScenariosResults, projectConfiguration)
#'
#' # Now load the results
#' scnarioNames <- names(scenarios)
#' simulatedScenariosResults <- loadScenarioResults(
#'   scnarioNames = scnarioNames,
#'   resultsFolder = pathToTheFolder
#' )
#' }
loadScenarioResults <- function(scenarioNames, resultsFolder) {
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
