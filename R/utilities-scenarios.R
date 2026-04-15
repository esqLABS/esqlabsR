#' Prepare a single scenario for simulation
#'
#' @description Loads simulation, creates ospsuite objects (with caching),
#' merges parameters, sets outputs, sets time intervals, initialises the
#' simulation, handles steady-state, and creates the population object.
#' Does NOT run the simulation.
#'
#' @param scenario A `Scenario` object (plain data class).
#' @param pc A `ProjectConfiguration` object.
#' @param customParams Optional parameter structure from the caller.
#' @param cache An environment with `$individuals` and `$populations` named lists.
#' @param simulationRunOptions Optional `SimulationRunOptions` (used for steady-state).
#'
#' @returns A list with `simulation` and `population` (NULL for individual scenarios).
#' @keywords internal
.prepareScenario <- function(scenario, pc, customParams, cache, simulationRunOptions) {
  # 1. Load simulation
  simulation <- ospsuite::loadSimulation(
    filePath = file.path(pc$modelFolder, scenario$modelFile),
    loadFromCache = FALSE
  )
  simulation$name <- scenario$scenarioName

  # 2. Build merged parameter structure
  params <- NULL

  # 2a. Model parameter groups
  if (!is.null(scenario$parameterGroups)) {
    for (groupName in scenario$parameterGroups) {
      groupParams <- pc$modelParameters[[groupName]]
      if (!is.null(groupParams)) {
        params <- extendParameterStructure(
          parameters = params,
          newParameters = groupParams
        )
      }
    }
  }

  # 2b. Individual characteristics + species parameters
  individualCharacteristics <- NULL
  if (!is.null(scenario$individualId) && !is.na(scenario$individualId)) {
    indivData <- pc$individuals[[scenario$individualId]]

    if (is.null(indivData)) {
      warning(messages$warningNoIndividualCharacteristics(
        scenarioName = scenario$scenarioName,
        individualId = scenario$individualId
      ))
    } else {
      # Create or retrieve IndividualCharacteristics from cache
      if (!is.null(cache$individuals[[scenario$individualId]])) {
        individualCharacteristics <- cache$individuals[[scenario$individualId]]
      } else {
        moleculeOntogenies <- .readOntongeniesFromList(indivData$proteinOntogenies)
        individualCharacteristics <- ospsuite::createIndividualCharacteristics(
          species = indivData$species,
          population = indivData$population,
          gender = indivData$gender,
          weight = as.double(indivData$weight),
          height = as.double(indivData$height),
          age = as.double(indivData$age),
          moleculeOntogenies = moleculeOntogenies
        )
        cache$individuals[[scenario$individualId]] <- individualCharacteristics
      }

      # 2c. Species parameters (from modelParameters, applied after individual)
      speciesParams <- pc$modelParameters[[indivData$species]]
      if (!is.null(speciesParams)) {
        params <- extendParameterStructure(
          parameters = params,
          newParameters = speciesParams
        )
      }

      # 2d. Individual parameter sets
      setNames <- pc$individualParameterSetMapping[[scenario$individualId]]
      if (!is.null(setNames)) {
        for (setName in setNames) {
          setParams <- pc$individualParameterSets[[setName]]
          if (!is.null(setParams)) {
            params <- extendParameterStructure(
              parameters = params,
              newParameters = setParams
            )
          } else {
            stop(messages$errorIndividualParameterSetNotFound(
              scenarioName = scenario$scenarioName,
              parameterSetName = setName
            ))
          }
        }
      }
    }
  }

  # 2e. Application parameters
  if (!is.na(scenario$applicationProtocol)) {
    applicationParams <- pc$applications[[scenario$applicationProtocol]]
    if (is.null(applicationParams)) {
      stop(messages$errorApplicationProtocolNotFound(
        scenarioName = scenario$scenarioName,
        applicationProtocol = scenario$applicationProtocol
      ))
    }
    params <- extendParameterStructure(
      parameters = params,
      newParameters = applicationParams
    )
  }

  # 2f. Custom parameters from caller
  if (!is.null(customParams)) {
    params <- extendParameterStructure(
      parameters = params,
      newParameters = customParams
    )
  }

  # 3. Set outputs
  if (!is.null(scenario$outputPaths)) {
    setOutputs(
      quantitiesOrPaths = scenario$outputPaths,
      simulation = simulation
    )
  }

  # 4. Set simulation time intervals
  if (!is.null(scenario$simulationTime)) {
    clearOutputIntervals(simulation)
    for (i in seq_along(scenario$simulationTime)) {
      addOutputInterval(
        simulation = simulation,
        startTime = toBaseUnit(
          quantityOrDimension = ospDimensions$Time,
          values = scenario$simulationTime[[i]][1],
          unit = scenario$simulationTimeUnit
        ),
        endTime = toBaseUnit(
          quantityOrDimension = ospDimensions$Time,
          values = scenario$simulationTime[[i]][2],
          unit = scenario$simulationTimeUnit
        ),
        resolution = scenario$simulationTime[[i]][3] /
          toBaseUnit(
            quantityOrDimension = ospDimensions$Time,
            values = 1,
            unit = scenario$simulationTimeUnit
          )
      )
    }
  }

  # 5. Initialize simulation (apply individual + all params)
  initializeSimulation(
    simulation = simulation,
    individualCharacteristics = individualCharacteristics,
    additionalParams = params,
    stopIfParameterNotFound = TRUE
  )

  # 6. Create population (for population scenarios)
  population <- NULL
  if (scenario$simulationType == "Population") {
    if (is.null(scenario$populationId)) {
      stop(messages$noPopulationIdForPopulationScenario(scenario$scenarioName))
    }
    if (scenario$readPopulationFromCSV) {
      populationPath <- paste0(
        file.path(pc$populationsFolder, scenario$populationId),
        ".csv"
      )
      population <- loadPopulation(populationPath)
    } else {
      # Create or retrieve from cache
      if (!is.null(cache$populations[[scenario$populationId]])) {
        population <- cache$populations[[scenario$populationId]]
      } else {
        popData <- pc$populations[[scenario$populationId]]
        moleculeOntogenies <- .readOntongeniesFromList(popData$proteinOntogenies)
        popArgs <- popData
        popArgs$proteinOntogenies <- NULL
        popArgs$moleculeOntogenies <- moleculeOntogenies
        popResult <- do.call(ospsuite::createPopulationCharacteristics, popArgs)
        popObj <- createPopulation(populationCharacteristics = popResult)
        population <- popObj$population
        cache$populations[[scenario$populationId]] <- population
      }
    }
  }

  # 7. Handle steady state
  if (scenario$simulateSteadyState) {
    ignoreIfFormula <- !scenario$overwriteFormulasInSS
    initialValues <- ospsuite::getSteadyState(
      simulations = list(simulation),
      steadyStateTime = list(scenario$steadyStateTime),
      ignoreIfFormula = ignoreIfFormula,
      simulationRunOptions = simulationRunOptions
    )
    ospsuite::setQuantityValuesByPath(
      quantityPaths = initialValues[[simulation$id]]$paths,
      values = initialValues[[simulation$id]]$values,
      simulation = simulation
    )
  }

  list(
    simulation = simulation,
    population = population
  )
}

#' Execute a single scenario
#'
#' @description Prepares and runs a single scenario, returning results.
#' Delegates setup to `.prepareScenario()`.
#'
#' @param scenario A `Scenario` object (plain data class).
#' @param pc A `ProjectConfiguration` object.
#' @param customParams Optional parameter structure from the caller.
#' @param cache An environment with `$individuals` and `$populations` named lists.
#' @param simulationRunOptions Optional `SimulationRunOptions`.
#'
#' @returns A list with `simulation`, `results`, `outputValues`, `population`.
#' @keywords internal
.executeScenario <- function(scenario, pc, customParams, cache, simulationRunOptions) {
  prepared <- .prepareScenario(
    scenario = scenario,
    pc = pc,
    customParams = customParams,
    cache = cache,
    simulationRunOptions = simulationRunOptions
  )
  simulation <- prepared$simulation
  population <- prepared$population

  # Run simulation
  if (is.null(population)) {
    simulationResults <- runSimulations(
      simulations = simulation,
      simulationRunOptions = simulationRunOptions
    )
  } else {
    simulationResults <- runSimulations(
      simulations = simulation,
      population = population,
      simulationRunOptions = simulationRunOptions
    )
  }

  results <- simulationResults[[simulation$id]]

  # Get output values
  outputQuantities <- NULL
  if (!is.null(scenario$outputPaths)) {
    outputQuantities <- getAllQuantitiesMatching(
      scenario$outputPaths,
      simulation
    )
  }

  outputValues <- NULL
  if (is.null(results)) {
    warning(messages$missingResultsForScenario(scenario$scenarioName))
  } else {
    outputValues <- getOutputValues(
      results,
      quantitiesOrPaths = outputQuantities,
      population = population,
      addMetaData = FALSE
    )
  }

  list(
    simulation = simulation,
    results = results,
    outputValues = outputValues,
    population = population
  )
}

#' Run a set of scenarios.
#'
#' @param projectConfiguration An object of type `ProjectConfiguration` loaded
#'   from JSON. Its `scenarios` field is used to select and run scenarios.
#' @param scenarioNames Optional character vector of scenario names to run. If
#'   `NULL` (default), all scenarios defined in `projectConfiguration` are run.
#' @param customParams A list containing vectors `paths`, `values`, and `units`
#'   that will be applied to all scenarios.
#' @param simulationRunOptions Object of type `SimulationRunOptions` that will
#'   be passed to simulation runs. If `NULL`, default options are used.
#'
#' @returns A named list, where the names are scenario names, and the values are
#'   lists with the entries `simulation` being the initialized `Simulation`
#'   object with applied parameters, `results` being `SimulationResults` object
#'   produced by running the simulation, `outputValues` the output values of the
#'   `SimulationResults`, and `population` the `Population` object if the
#'   scenario is a population simulation.
#' @details If simulation of a scenario fails, a warning is produced, and the
#' `outputValues` for this scenario is `NULL`.
#'
#' @export
runScenarios <- function(
  projectConfiguration,
  scenarioNames = NULL,
  customParams = NULL,
  simulationRunOptions = NULL
) {
  validateIsOfType(projectConfiguration, "ProjectConfiguration")
  .validateParametersStructure(
    parameterStructure = customParams,
    argumentName = "customParams",
    nullAllowed = TRUE
  )

  allScenarios <- projectConfiguration$scenarios
  if (is.null(scenarioNames)) {
    scenarioNames <- names(allScenarios)
  }

  # Validate scenario names
  unknownNames <- setdiff(scenarioNames, names(allScenarios))
  if (length(unknownNames) > 0) {
    stop(paste0(
      "Unknown scenario names: ",
      paste(unknownNames, collapse = ", ")
    ))
  }

  # Run-scoped cache for ospsuite objects
  cache <- new.env(parent = emptyenv())
  cache$individuals <- list()
  cache$populations <- list()

  # Prepare all scenarios (load sim, apply params, set outputs/time, init, SS)
  preparedList <- vector("list", length(scenarioNames))
  for (idx in seq_along(scenarioNames)) {
    scenarioName <- scenarioNames[[idx]]
    scenario <- allScenarios[[scenarioName]]
    preparedList[[idx]] <- .prepareScenario(
      scenario = scenario,
      pc = projectConfiguration,
      customParams = customParams,
      cache = cache,
      simulationRunOptions = simulationRunOptions
    )
    names(preparedList)[[idx]] <- scenarioName
  }

  # Run all individual simulations in one batch (ospsuite runs them in parallel)
  individualSimulations <- list()
  for (idx in seq_along(scenarioNames)) {
    if (is.null(preparedList[[idx]]$population)) {
      individualSimulations <- c(
        individualSimulations,
        preparedList[[idx]]$simulation
      )
    }
  }
  simulationResults <- list()
  if (length(individualSimulations) > 0) {
    simulationResults <- runSimulations(
      simulations = individualSimulations,
      simulationRunOptions = simulationRunOptions
    )
  }

  # Run population simulations sequentially and append their results
  for (idx in seq_along(scenarioNames)) {
    prepared <- preparedList[[idx]]
    if (!is.null(prepared$population)) {
      populationResults <- runSimulations(
        simulations = prepared$simulation,
        population = prepared$population,
        simulationRunOptions = simulationRunOptions
      )
      simulationResults <- c(simulationResults, populationResults)
    }
  }

  # Collect output values for each scenario
  returnList <- vector("list", length(scenarioNames))
  for (idx in seq_along(scenarioNames)) {
    scenarioName <- scenarioNames[[idx]]
    scenario <- allScenarios[[scenarioName]]
    prepared <- preparedList[[idx]]
    simulation <- prepared$simulation
    population <- prepared$population
    results <- simulationResults[[simulation$id]]

    outputQuantities <- NULL
    if (!is.null(scenario$outputPaths)) {
      outputQuantities <- getAllQuantitiesMatching(
        scenario$outputPaths,
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

  ospsuite::clearMemory()
  return(returnList)
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
#' projectConfiguration <- loadProject("path/to/ProjectConfiguration.json")
#' simulatedScenariosResults <- runScenarios(
#'   projectConfiguration = projectConfiguration
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
#' projectConfiguration <- loadProject("path/to/ProjectConfiguration.json")
#' simulatedScenariosResults <- runScenarios(
#'   projectConfiguration = projectConfiguration
#' )
#' saveScenarioResults(simulatedScenariosResults, projectConfiguration)
#'
#' # Now load the results
#' simulatedScenariosResults <- loadScenarioResults(
#'   scenarioNames = c("Scenario1", "Scenario2"),
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
