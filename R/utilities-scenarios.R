#' Run a set of scenarios.
#'
#' @param simulationRunOptions Object of type `SimulationRunOptions` that will be passed
#' to simulation runs. If `NULL`, default options are used.
#' @param scenarios List of `Scenario` objects to be simulated.
#'
#' @returns A named list, where the names are scenario names, and the values are
#' lists with the entries `simulation` being the initialized `Simulation` object with applied parameters,
#' `results` being `SimulatioResults` object produced by running the simulation,
#' `outputValues` the output values of the `SimulationResults`, and `population`
#' the `Population` object if the scenario is a population simulation.
#' @details
#' If simulation of a scenario fails, a warning is produced, and the `outputValues`
#' for this scenario is `NULL`.
#'
#' @export
runScenarios <- function(scenarios, simulationRunOptions = NULL) {
  scenarios <- ospsuite.utils::toList(scenarios)
  # List of individiaul simulations
  individualSimulations <- list()
  # List of population scenarios
  populationScenarios <- list()
  # List of simulation with steady-state
  steadyStateSimulations <- list()
  # Have to store steady-state times separately, because they are not part of the simulation object
  steadyStateTimes <- list()
  for (scenario in scenarios) {
    if (scenario$scenarioType == "Individual") {
      individualSimulations <- c(individualSimulations, scenario$simulation)
    } else {
      populationScenarios <- c(populationScenarios, scenario)
    }

    if (scenario$scenarioConfiguration$simulateSteadyState) {
      steadyStateSimulations <- c(steadyStateSimulations, scenario$simulation)
      steadyStateTimes <- c(
        steadyStateTimes,
        scenario$scenarioConfiguration$steadyStateTime
      )
    }
  }

  # Simulate steady-state concurrently
  if (length(steadyStateSimulations) > 0) {
    initialValues <- ospsuite::getSteadyState(
      simulations = steadyStateSimulations,
      steadyStateTime = steadyStateTimes,
      ignoreIfFormula = TRUE,
      simulationRunOptions = simulationRunOptions
    )
  }

  # Set initial values for steady-state simulations
  for (simulation in steadyStateSimulations) {
    ospsuite::setQuantityValuesByPath(
      quantityPaths = initialValues[[simulation$id]]$paths,
      values = initialValues[[simulation$id]]$values,
      simulation = simulation
    )
  }

  # Run invidual simulations
  simulationResults <- runSimulations(
    simulations = individualSimulations,
    simulationRunOptions = simulationRunOptions
  )

  # Run population simulations sequentially and add the to the list of simulation results
  for (scenario in populationScenarios) {
    populationResults <- runSimulations(
      simulations = scenario$simulation,
      population = scenario$population,
      simulationRunOptions = simulationRunOptions
    )
    simulationResults <- c(simulationResults, populationResults)
  }

  # Create output list with simulation results, simulation objects, and population objects
  returnList <- vector("list", length(simulationResults))
  for (idx in seq_along(scenarios)) {
    scenario <- scenarios[[idx]]
    scenarioName <- scenario$scenarioConfiguration$scenarioName
    simulation <- scenario$simulation
    id <- simulation$id
    results <- simulationResults[[id]]
    population <- scenario$population
    # For the cases when population is set to NA, convert it to NULL
    if (
      !is.null(population) &&
        !isOfType(population, "Population") &&
        is.na(population)
    ) {
      population <- NULL
    }

    # Retrieving quantities from paths to support pattern matching with '*'
    outputQuantities <- NULL
    if (!is.null(scenario$scenarioConfiguration$outputPaths)) {
      outputQuantities <- getAllQuantitiesMatching(
        scenario$scenarioConfiguration$outputPaths,
        simulation
      )
    }

    # If results could not be calculated, show a warning and return NULL
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

  # Call gc() on .NET
  ospsuite::clearMemory()
  return(returnList)
}

#' Create `Scenario` objects from `ScenarioConfiguration` objects
#'
#' @description
#' Load simulation.
#' Apply parameters from global XLS.
#' Apply individual physiology.
#' Apply individual model parameters.
#' Set simulation outputs.
#' Set simulation time.
#' initializeSimulation().
#' Create population
#'
#' @param scenarioConfigurations List of `ScenarioConfiguration` objects to be
#' simulated. See [createScenarios()] for details.
#' @param customParams A list containing vectors 'paths' with the full paths to the
#' parameters, 'values' the values of the parameters, and 'units' with the
#' units the values are in. The values will be applied to all scenarios.
#' @param stopIfParameterNotFound Boolean. If `TRUE` (default) and a custom parameter is not found, an error is thrown. If `FALSE`, non-existing parameters
#' are ignored.
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
    ~ Scenario$new(
      .x,
      customParams = customParams,
      stopIfParameterNotFound = stopIfParameterNotFound
    )
  ) %>%
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
