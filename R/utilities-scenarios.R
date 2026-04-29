# Internal scenario execution helpers ----

#' Merge the parameter layers for a scenario
#'
#' @description Builds the final parameter structure for a scenario by merging
#' (in order, last-write-wins): scenario `modelParameters` (iterated in listed
#' order), species defaults, individual inline `parameters`, application inline
#' `parameters`, then `customParams`. Pure function: no I/O, no simulation load.
#'
#' @param scenario A scenario plain-list (as in `project$scenarios[[name]]`).
#' @param project A `Project` object.
#' @param customParams Optional caller-supplied parameter structure.
#'
#' @returns A parameter structure `list(paths, values, units)`. May be `NULL`
#'   if no layer sets any parameter.
#' @keywords internal
.mergeScenarioParameters <- function(scenario, project, customParams = NULL) {
  params <- NULL

  # 1. Model parameters (iterate in the order listed on the scenario)
  if (!is.null(scenario$modelParameters)) {
    for (setId in scenario$modelParameters) {
      setParams <- project$modelParameters[[setId]]
      if (!is.null(setParams)) {
        params <- extendParameterStructure(
          parameters = params,
          newParameters = setParams
        )
      }
    }
  }

  # 2. Species defaults (only if individual is set and resolves)
  if (!is.null(scenario$individualId) && !is.na(scenario$individualId)) {
    indivData <- project$individuals[[scenario$individualId]]
    if (!is.null(indivData)) {
      speciesParams <- .getSpeciesParameters(indivData$species)
      if (!is.null(speciesParams)) {
        params <- extendParameterStructure(
          parameters = params,
          newParameters = speciesParams
        )
      }

      # 3. Individual inline parameters
      if (!is.null(indivData$parameters)) {
        params <- extendParameterStructure(
          parameters = params,
          newParameters = indivData$parameters
        )
      }
    }
  }

  # 4. Application inline parameters (back-compat with old parallel-vector
  # shape during the transition; the $paths fallback is removed in Task 7)
  if (
    !is.null(scenario$applicationProtocol) &&
      !is.na(scenario$applicationProtocol)
  ) {
    appData <- project$applications[[scenario$applicationProtocol]]
    if (is.null(appData)) {
      stop(messages$errorApplicationProtocolNotFound(
        scenarioName = scenario$scenarioName,
        applicationProtocol = scenario$applicationProtocol
      ))
    }
    if (!is.null(appData$parameters)) {
      params <- extendParameterStructure(
        parameters = params,
        newParameters = appData$parameters
      )
    }
  }

  # 5. Custom params
  if (!is.null(customParams)) {
    params <- extendParameterStructure(
      parameters = params,
      newParameters = customParams
    )
  }

  params
}

#' Prepare a single scenario for simulation
#'
#' @description Loads simulation, creates ospsuite objects (with caching),
#' merges parameters, sets outputs, sets time intervals, initialises the
#' simulation, handles steady-state, and creates the population object.
#' Does NOT run the simulation.
#'
#' @param scenario A `Scenario` object (plain data class).
#' @param project A `Project` object.
#' @param customParams Optional parameter structure from the caller.
#' @param cache An environment with `$individuals` and `$populations` named lists.
#' @param simulationRunOptions Optional `SimulationRunOptions` (used for steady-state).
#'
#' @returns A list with `simulation` and `population` (NULL for individual scenarios).
#' @keywords internal
.prepareScenario <- function(
  scenario,
  project,
  customParams,
  cache,
  simulationRunOptions
) {
  # 1. Load simulation
  simulation <- ospsuite::loadSimulation(
    filePath = file.path(project$modelFolder, scenario$modelFile),
    loadFromCache = FALSE
  )
  simulation$name <- scenario$scenarioName

  # 2. Build merged parameter structure
  params <- .mergeScenarioParameters(scenario, project, customParams)

  # 2b. Individual characteristics (still needed for simulation initialization)
  individualCharacteristics <- NULL
  if (!is.null(scenario$individualId) && !is.na(scenario$individualId)) {
    indivData <- project$individuals[[scenario$individualId]]
    if (is.null(indivData)) {
      warning(messages$warningNoIndividualCharacteristics(
        scenarioName = scenario$scenarioName,
        individualId = scenario$individualId
      ))
    } else {
      if (!is.null(cache$individuals[[scenario$individualId]])) {
        individualCharacteristics <- cache$individuals[[scenario$individualId]]
      } else {
        moleculeOntogenies <- .readOntongeniesFromList(
          indivData$proteinOntogenies
        )
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
    }
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
    if (is.null(scenario$simulationTimeUnit)) {
      stop(messages$stopScenarioMissingTimeUnit(scenario$scenarioName))
    }
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
        file.path(project$populationsFolder, scenario$populationId),
        ".csv"
      )
      population <- loadPopulation(populationPath)
    } else {
      # Create or retrieve from cache
      if (!is.null(cache$populations[[scenario$populationId]])) {
        population <- cache$populations[[scenario$populationId]]
      } else {
        popData <- project$populations[[scenario$populationId]]
        if (is.null(popData)) {
          stop(paste0(
            "Population '",
            scenario$populationId,
            "' referenced by scenario '",
            scenario$scenarioName,
            "' not found in project."
          ))
        }
        moleculeOntogenies <- .readOntongeniesFromList(
          popData$proteinOntogenies
        )
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

#' Collect output values for a single completed scenario
#'
#' @description Given a scenario's simulation, raw results, and population,
#' resolves output quantities and builds the standard return list used by
#' `runScenarios()`.
#'
#' @param scenario A `Scenario` object (plain data class).
#' @param simulation The initialised `Simulation` object.
#' @param results The `SimulationResults` for this simulation (may be `NULL`).
#' @param population The `Population` object (`NULL` for individual scenarios).
#'
#' @returns A list with `simulation`, `results`, `outputValues`, `population`.
#' @keywords internal
.collectScenarioResult <- function(scenario, simulation, results, population) {
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

# Run scenarios ----

#' Run a set of scenarios.
#'
#' @param project An object of type `Project` loaded
#'   from JSON. Its `scenarios` field is used to select and run scenarios.
#' @param scenarioNames Optional character vector of scenario names to run. If
#'   `NULL` (default), all scenarios defined in `project` are run.
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
  project,
  scenarioNames = NULL,
  customParams = NULL,
  simulationRunOptions = NULL
) {
  validateIsOfType(project, "Project")
  .validateParametersStructure(
    parameterStructure = customParams,
    argumentName = "customParams",
    nullAllowed = TRUE
  )

  allScenarios <- project$scenarios
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
      project = project,
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

  # Run population simulations sequentially and append their results.
  # `runSimulations()` only batches independent simulations; each population
  # run requires its own population object, so they cannot be combined in a
  # single batch call.
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
    results <- simulationResults[[prepared$simulation$id]]

    returnList[[idx]] <- .collectScenarioResult(
      scenario = scenario,
      simulation = prepared$simulation,
      results = results,
      population = prepared$population
    )
    names(returnList)[[idx]] <- scenarioName
  }

  return(returnList)
}

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

# Create scenarios from PKML ----

#' Create scenario configurations from PKML files
#'
#' @description
#' Creates scenario configurations from PKML files by extracting available information
#' such as applications, output paths, and simulation time settings. This function
#' creates scenario configuration objects that can be used with the esqlabsR workflow.
#'
#' @param pkmlFilePaths Character vector of paths to PKML files to create scenarios from.
#'   Can be a single string (recycled for all scenarios) or a vector with the same length
#'   as the number of scenarios being created (determined by the longest vector argument).
#' @param project A `Project` object holding base information.
#' @param scenarioNames Character vector. Optional custom names for the scenarios. If `NULL` (default),
#'   scenario names will be extracted from the simulation names in the PKML files.
#'   If provided, must have the same length as `pkmlFilePaths`.
#' @param individualId Character vector. Optional individual IDs to use for scenarios. If `NULL` (default),
#'   no individual will be specified. Can be a single string (recycled for all scenarios) or a vector
#'   with the same length as `pkmlFilePaths`.
#' @param populationId Character vector. Optional population IDs to use for scenarios. If `NULL` (default),
#'   no population will be specified. If provided, sets simulation type to "Population". Can be a single
#'   string (recycled for all scenarios) or a vector with the same length as `pkmlFilePaths`.
#' @param applicationProtocols Character vector. Optional application protocol names to use for scenarios.
#'   If `NULL` (default), application protocols will be set to the scenario name.
#'   Can be a single string (recycled for all scenarios) or a vector with the same length as `pkmlFilePaths`.
#' @param modelParameters Character vector. Optional parameter group names to apply to scenarios.
#'   If `NULL` (default), no parameter groups will be applied. Can be a single string (recycled for all
#'   scenarios) or a vector with the same length as `pkmlFilePaths`. If providing multiple groups per
#'   scenario, separate them with commas in the string.
#' @param paramSheets `r lifecycle::badge("deprecated")` Use `modelParameters` instead.
#' @param outputPaths Character vector or named vector. Optional output paths to use for scenarios. If `NULL` (default),
#'   output paths will be extracted from the PKML files' output selections. Can be a single string
#'   (recycled for all scenarios) or a vector with the same length as `pkmlFilePaths`. If providing
#'   multiple paths per scenario, separate them with commas in the string. Named vectors are supported
#'   where names serve as aliases for the paths, e.g., c("plasma" = "Organism|VenousBlood|Plasma|AKB-9090|Concentration in container").
#' @param simulationTime Character vector. Optional simulation time to use for scenarios as character strings containing one or
#'   multiple time intervals separated by a ';'. Each time interval is a triplet of values <StartTime, EndTime, Resolution>,
#'   where `Resolution` is the number of simulated points per time unit defined in the `simulationTimeUnit`. If `NULL` (default),
#'   simulation time will be extracted from the PKML files' output schema intervals. Can be a single string (recycled for all
#'   scenarios) or a vector with the same length as `pkmlFilePaths`.
#' @param simulationTimeUnit Character vector. Optional simulation time units. Only used when `simulationTime` is provided.
#'   If `NULL` (default), will be extracted from the PKML file's output schema intervals, or set to "min" (minutes) if not available.
#'   Can be a single string (recycled for all scenarios) or a vector with the same length as `pkmlFilePaths`.
#' @param steadyState Logical vector. Whether to simulate steady-state for each scenario. Default is `FALSE`.
#'   Can be a single logical value (recycled for all scenarios) or a vector with the same length as `pkmlFilePaths`.
#' @param steadyStateTime Numeric vector. Steady-state times. Only used when corresponding `steadyState` is `TRUE`.
#'   If `NULL` (default), no steady-state time is set on the scenario, and
#'   `Scenario$new()`'s default of 1000 (in the configured time unit) takes effect at simulation time.
#'   Can be a single numeric value (recycled for all scenarios)
#'   or a vector with the same length as `pkmlFilePaths`.
#' @param steadyStateTimeUnit Character vector. Steady-state time units. Only used when `steadyState = TRUE` and `steadyStateTime` is provided.
#'   If `NULL` (default), "min" will be used. Can be a single string (recycled for all scenarios) or a vector
#'   with the same length as `pkmlFilePaths`.
#' @param overwriteFormulasInSS Logical vector. Whether to overwrite formula-defined parameters with
#'   their steady-state values. When `TRUE`, corresponds to `ignoreIfFormula = FALSE` in
#'   `ospsuite::getSteadyState()` (formulas are overwritten). Default is `FALSE` (formula-defined
#'   parameters are kept unchanged). Can be a single logical value (recycled for all scenarios) or
#'   a vector with the same length as `pkmlFilePaths`.
#' @param readPopulationFromCSV Logical vector. Whether to read population from CSV for each scenario. Default is `FALSE`.
#'   Can be a single logical value (recycled for all scenarios) or a vector with the same length as `pkmlFilePaths`.
#'
#' @details
#' This function extracts the following information from PKML files:
#' * **Applications**: Application protocol names (defaults to scenario name).
#' * **Output paths**: All selected outputs for the simulation from `outputSelections$allOutputs`.
#' * **Simulation time**: Time intervals with start time, end time, and resolution from `outputSchema$intervals`.
#' * **Simulation time unit**: Time unit from the output schema intervals (e.g., "h" for hours).
#'
#' ## Vector Arguments and Recycling
#'
#' All arguments support vectorization to create scenarios with different parameter values:
#' * **Length 1**: The value is recycled (applied to all scenarios).
#' * **Length > 1**: All vector arguments must have the same length, which determines the number of scenarios.
#' * **Mixed lengths**: An error is thrown if vector arguments have inconsistent lengths.
#'
#' The number of scenarios created is determined by the longest vector argument. All shorter vectors
#' (including `pkmlFilePaths`) are recycled to match this length.
#'
#' This allows you to efficiently create multiple scenarios in several ways:
#' * **Same PKML, different settings**: Use a single PKML file with vectors of different parameter values.
#' * **Different PKMLs, same settings**: Use multiple PKML files with single parameter values.
#' * **Different PKMLs, different settings**: Use vectors of both PKML files and parameter values.
#'
#' The function handles duplicate scenario names by appending indices (e.g., "Scenario_1", "Scenario_2").
#'
#' @returns A named list of `Scenario` objects with the names being the scenario names.
#' @export
#'
#' @examples
#' \dontrun{
#' # Load project
#' project <- loadProject("Project.json")
#'
#' # Create scenarios from a single PKML file
#' pkmlPath <- "path/to/simulation.pkml"
#' scenarios <- createScenariosFromPKML(
#'   pkmlFilePaths = pkmlPath,
#'   project = project
#' )
#'
#' # Add to project and run
#' project$scenarios <- c(project$scenarios, scenarios)
#' results <- runScenarios(project, scenarioNames = names(scenarios))
#'
#' # Example of vector recycling - single value applied to all scenarios
#' scenarios <- createScenariosFromPKML(
#'   pkmlFilePaths = c("sim1.pkml", "sim2.pkml", "sim3.pkml"),
#'   project = project,
#'   individualId = "Individual_001", # Recycled to all scenarios
#'   steadyState = TRUE,              # Recycled to all scenarios
#'   steadyStateTime = 1000           # Recycled to all scenarios
#' )
#'
#' # Example of vector arguments - different values per scenario
#' scenarios <- createScenariosFromPKML(
#'   pkmlFilePaths = c("pediatric.pkml", "adult.pkml", "elderly.pkml"),
#'   project = project,
#'   scenarioNames = c("Pediatric", "Adult", "Elderly"),
#'   individualId = c("Child_001", "Adult_001", "Elderly_001"),
#'   applicationProtocols = c("Pediatric_Dose", "Standard_Dose", "Reduced_Dose"),
#'   steadyState = c(FALSE, TRUE, TRUE),
#'   steadyStateTime = c(NA, 2000, 1500)
#' )
#'
#' # Example of PKML recycling - same model, different settings
#' scenarios <- createScenariosFromPKML(
#'   pkmlFilePaths = "base_model.pkml",                    # Single PKML recycled
#'   project = project,
#'   scenarioNames = c("LowDose", "MediumDose", "HighDose"),
#'   individualId = c("Patient1", "Patient2", "Patient3"),
#'   applicationProtocols = c("Low_Protocol", "Med_Protocol", "High_Protocol"),
#'   steadyState = c(FALSE, TRUE, TRUE)                   # Different settings per scenario
#' )
#' }
createScenariosFromPKML <- function(
  pkmlFilePaths,
  project,
  scenarioNames = NULL,
  individualId = NULL,
  populationId = NULL,
  applicationProtocols = NULL,
  modelParameters = NULL,
  outputPaths = NULL,
  simulationTime = NULL,
  simulationTimeUnit = NULL,
  steadyState = FALSE,
  steadyStateTime = NULL,
  steadyStateTimeUnit = NULL,
  overwriteFormulasInSS = FALSE,
  readPopulationFromCSV = FALSE,
  paramSheets = lifecycle::deprecated()
) {
  # Handle deprecated paramSheets argument
  if (lifecycle::is_present(paramSheets)) {
    lifecycle::deprecate_soft(
      what = "createScenariosFromPKML(paramSheets)",
      with = "createScenariosFromPKML(modelParameters)",
      when = "6.0.0"
    )
    modelParameters <- modelParameters %||% paramSheets
  }

  # Validate inputs
  validateIsCharacter(pkmlFilePaths)
  validateIsOfType(project, Project)
  if (!is.null(scenarioNames)) {
    validateIsCharacter(scenarioNames)
  }
  if (!is.null(individualId)) {
    validateIsCharacter(individualId)
  }
  if (!is.null(populationId)) {
    validateIsCharacter(populationId)
  }
  if (!is.null(applicationProtocols)) {
    validateIsCharacter(applicationProtocols)
  }
  if (!is.null(modelParameters)) {
    validateIsCharacter(modelParameters)
  }
  if (!is.null(outputPaths)) {
    validateIsCharacter(outputPaths)
  }
  if (!is.null(simulationTime)) {
    validateIsCharacter(simulationTime)
  }
  if (!is.null(simulationTimeUnit)) {
    validateIsCharacter(simulationTimeUnit)
  }
  validateIsLogical(steadyState)
  if (!is.null(steadyStateTime)) {
    validateIsNumeric(steadyStateTime)
  }
  if (!is.null(steadyStateTimeUnit)) {
    validateIsCharacter(steadyStateTimeUnit)
  }
  validateIsLogical(overwriteFormulasInSS)
  validateIsLogical(readPopulationFromCSV)

  # Get the number of scenarios to create based on vector arguments
  # Note: project is excluded as it should always be a single object
  nScenarios <- .getScenarioCount(
    pkmlFilePaths,
    scenarioNames,
    individualId,
    populationId,
    applicationProtocols,
    modelParameters,
    outputPaths,
    simulationTime,
    simulationTimeUnit,
    steadyState,
    steadyStateTime,
    steadyStateTimeUnit,
    overwriteFormulasInSS,
    readPopulationFromCSV
  )

  # Recycle or validate all vector arguments (including pkmlFilePaths)
  pkmlFilePaths <- .recycleOrValidateVector(
    pkmlFilePaths,
    "pkmlFilePaths",
    nScenarios
  )
  scenarioNames <- .recycleOrValidateVector(
    scenarioNames,
    "scenarioNames",
    nScenarios
  )
  individualId <- .recycleOrValidateVector(
    individualId,
    "individualId",
    nScenarios
  )
  populationId <- .recycleOrValidateVector(
    populationId,
    "populationId",
    nScenarios
  )
  applicationProtocols <- .recycleOrValidateVector(
    applicationProtocols,
    "applicationProtocols",
    nScenarios
  )
  modelParameters <- .recycleOrValidateVector(
    modelParameters,
    "modelParameters",
    nScenarios
  )
  # Special handling for outputPaths to preserve named vectors
  if (!is.null(outputPaths)) {
    if (is.list(outputPaths)) {
      # outputPaths is a list - validate length
      if (length(outputPaths) == 1) {
        # Recycle single list element to all scenarios
        outputPaths <- rep(outputPaths, nScenarios)
      } else if (length(outputPaths) != nScenarios) {
        stop(messages$invalidArgumentLength(
          length(outputPaths),
          nScenarios
        ))
      }
    } else {
      # outputPaths is a vector
      if (length(outputPaths) == 1) {
        # Single vector - recycle entire vector to each scenario (preserves named vectors)
        outputPaths <- rep(list(outputPaths), nScenarios)
      } else if (length(outputPaths) == nScenarios) {
        # Vector matches scenarios length - use as is (for comma-separated strings)
        outputPaths <- .recycleOrValidateVector(
          outputPaths,
          "outputPaths",
          nScenarios
        )
      } else {
        stop(messages$invalidArgumentLength(
          length(outputPaths),
          nScenarios
        ))
      }
    }
  }
  simulationTime <- .recycleOrValidateVector(
    simulationTime,
    "simulationTime",
    nScenarios
  )
  simulationTimeUnit <- .recycleOrValidateVector(
    simulationTimeUnit,
    "simulationTimeUnit",
    nScenarios
  )
  steadyState <- .recycleOrValidateVector(
    steadyState,
    "steadyState",
    nScenarios
  )
  steadyStateTime <- .recycleOrValidateVector(
    steadyStateTime,
    "steadyStateTime",
    nScenarios
  )
  steadyStateTimeUnit <- .recycleOrValidateVector(
    steadyStateTimeUnit,
    "steadyStateTimeUnit",
    nScenarios
  )
  overwriteFormulasInSS <- .recycleOrValidateVector(
    overwriteFormulasInSS,
    "overwriteFormulasInSS",
    nScenarios
  )
  readPopulationFromCSV <- .recycleOrValidateVector(
    readPopulationFromCSV,
    "readPopulationFromCSV",
    nScenarios
  )

  # Initialize variables
  scenarios <- list()
  # Tracks the *original* (pre-rename) scenario names seen so far, so the
  # collision counter below increments per original name. The renamed
  # collision-resolved names are stored as the keys of `scenarios`.
  originalScenarioNames <- c()

  for (i in seq_along(pkmlFilePaths)) {
    pkmlPath <- pkmlFilePaths[[i]]
    # Check if PKML files exist
    if (!file.exists(pkmlPath)) {
      stop(messages$fileNotFound(pkmlPath))
    }

    # Load simulation from PKML file
    simulation <- ospsuite::loadSimulation(
      filePath = pkmlPath,
      loadFromCache = FALSE
    )

    # Extract scenario name from simulation if not provided by user
    if (is.null(scenarioNames)) {
      originalScenarioName <- scenarioName <- simulation$name
    } else {
      originalScenarioName <- scenarioName <- scenarioNames[[i]]
    }

    # Look for duplicated scenario names
    if (scenarioName %in% originalScenarioNames) {
      # Duplicate found: count how many times the original name has already
      # been seen and append the next index. e.g. second occurrence of "S"
      # becomes "S_2", third becomes "S_3", etc.
      existingCount <- sum(originalScenarioNames == originalScenarioName)
      scenarioName <- paste0(scenarioName, "_", existingCount + 1)

      warning(messages$autocorrectDuplicateScenarioNames(
        originalScenarioName,
        scenarioName
      ))
    }

    originalScenarioNames <- c(originalScenarioNames, originalScenarioName)

    simTree <- ospsuite::getSimulationTree(simulation)

    # Create Scenario object
    sc <- Scenario$new()
    sc$scenarioName <- scenarioName
    sc$modelFile <- path_rel(
      pkmlPath,
      start = project$modelFolder
    )

    # Set individual ID
    if (!is.null(individualId)) {
      sc$individualId <- individualId[[i]]
    }

    # Set population ID
    if (!is.null(populationId)) {
      sc$populationId <- populationId[[i]]
      sc$simulationType <- "Population"
    }

    # Set read population from CSV
    sc$readPopulationFromCSV <- readPopulationFromCSV[[i]]

    # Set parameter groups
    if (!is.null(modelParameters)) {
      scenarioParamGroups <- modelParameters[[i]]
      if (
        !is.null(scenarioParamGroups) &&
          !is.na(scenarioParamGroups) &&
          nchar(scenarioParamGroups) > 0
      ) {
        # Split by comma if it's a single string with multiple groups
        if (
          is.character(scenarioParamGroups) && length(scenarioParamGroups) == 1
        ) {
          scenarioParamGroups <- trimws(strsplit(scenarioParamGroups, ",")[[1]])
        }
        sc$modelParameters <- scenarioParamGroups
      }
    }

    # Extract and set application protocol
    if (!is.null(applicationProtocols)) {
      protocolName <- .sanitizeExcelSheetName(
        applicationProtocols[[i]],
        warn = TRUE
      )
    } else {
      # Application protocol name is by default the name of the scenario
      # Sanitize to ensure it's a valid Excel sheet name
      protocolName <- .sanitizeExcelSheetName(scenarioName, warn = TRUE)
    }

    sc$applicationProtocol <- protocolName

    # Extract and set output paths
    if (!is.null(outputPaths)) {
      # Handle different types of outputPaths structure
      if (is.list(outputPaths)) {
        # outputPaths is a list (named vectors case)
        scenarioOutputPaths <- outputPaths[[i]]
      } else {
        # outputPaths is a regular vector (comma-separated strings case)
        scenarioOutputPaths <- outputPaths[i]
      }
      if (
        !is.null(scenarioOutputPaths) &&
          !any(is.na(scenarioOutputPaths)) &&
          all(nchar(scenarioOutputPaths) > 0)
      ) {
        # Split by comma if it's a single string with multiple paths
        if (
          is.character(scenarioOutputPaths) &&
            length(scenarioOutputPaths) == 1 &&
            is.null(names(scenarioOutputPaths))
        ) {
          scenarioOutputPaths <- trimws(strsplit(scenarioOutputPaths, ",")[[1]])
        }
        # Handle named vectors: preserve names if they exist
        sc$outputPaths <- scenarioOutputPaths
      }
    } else {
      # Extract output paths from PKML
      if (!is.null(simulation$outputSelections$allOutputs)) {
        outputPathsFromPKML <- sapply(
          simulation$outputSelections$allOutputs,
          function(x) x$path
        )
        sc$outputPaths <- outputPathsFromPKML
      }
    }

    # Extract and set simulation time
    if (!is.null(simulationTime)) {
      sc$simulationTime <- .parseSimulationTimeIntervals(simulationTime[[i]])
      if (!is.null(simulationTimeUnit)) {
        sc$simulationTimeUnit <- simulationTimeUnit[[i]]
      }
    } else {
      # Extract simulation time from PKML
      if (!is.null(simulation$outputSchema$intervals)) {
        intervals <- simulation$outputSchema$intervals
        if (length(intervals) > 0) {
          # Extract time unit from PKML if not provided
          if (is.null(simulationTimeUnit)) {
            # Get the unit from the first interval's start time
            firstInterval <- intervals[[1]]
            if (!is.null(firstInterval$startTime$displayUnit)) {
              scenarioSimulationTimeUnit <- firstInterval$startTime$displayUnit
            } else {
              # Fallback to minutes if no display unit found
              scenarioSimulationTimeUnit <- "min"
            }
          } else {
            scenarioSimulationTimeUnit <- simulationTimeUnit[[i]]
          }

          # Create time intervals string from all intervals
          timeIntervals <- character()
          for (j in seq_along(intervals)) {
            interval <- intervals[[j]]
            startTime <- interval$startTime$value
            endTime <- interval$endTime$value
            resolution <- interval$resolution$value

            # Get the units from the interval
            startTimeUnit <- interval$startTime$displayUnit
            endTimeUnit <- interval$endTime$displayUnit

            # Convert to target units if different
            if (startTimeUnit != scenarioSimulationTimeUnit) {
              startTimeDisplay <- ospsuite::toUnit(
                quantityOrDimension = ospsuite::ospDimensions$Time,
                values = startTime,
                targetUnit = scenarioSimulationTimeUnit
              )
            } else {
              startTimeDisplay <- startTime
            }

            if (endTimeUnit != scenarioSimulationTimeUnit) {
              endTimeDisplay <- ospsuite::toUnit(
                quantityOrDimension = ospsuite::ospDimensions$Time,
                values = endTime,
                targetUnit = scenarioSimulationTimeUnit
              )
            } else {
              endTimeDisplay <- endTime
            }

            # Format this interval as "start, end, resolution"
            intervalString <- paste(
              startTimeDisplay,
              endTimeDisplay,
              resolution,
              sep = ", "
            )
            timeIntervals <- c(timeIntervals, intervalString)
          }

          # Join all intervals with semicolons and parse into list format
          if (length(timeIntervals) > 0) {
            sc$simulationTime <- .parseSimulationTimeIntervals(paste(
              timeIntervals,
              collapse = "; "
            ))
            sc$simulationTimeUnit <- scenarioSimulationTimeUnit
          }
        }
      }
    }

    # Set steady state configuration
    if (steadyState[[i]]) {
      sc$simulateSteadyState <- TRUE
      if (!is.null(steadyStateTime)) {
        scenarioSteadyStateTime <- steadyStateTime[[i]]
        if (
          !is.null(scenarioSteadyStateTime) && !is.na(scenarioSteadyStateTime)
        ) {
          # Use default time unit if not provided
          timeUnit <- if (is.null(steadyStateTimeUnit)) {
            "min"
          } else {
            steadyStateTimeUnit[[i]]
          }
          sc$steadyStateTime <- ospsuite::toBaseUnit(
            quantityOrDimension = ospsuite::ospDimensions$Time,
            values = scenarioSteadyStateTime,
            unit = timeUnit
          )
        }
      }
      sc$overwriteFormulasInSS <- overwriteFormulasInSS[[i]]
    }

    scenarios[[scenarioName]] <- sc
  }

  return(scenarios)
}

#' Sanitize a string to be a valid Excel sheet name
#'
#' @param sheetName Character string. The proposed sheet name to sanitize.
#' @param warn Logical. Whether to warn if the sheet name was modified. Default is TRUE.
#'
#' @details Sanitizes a string to comply with Excel sheet naming rules:
#' * Must be 31 characters or less
#' * Cannot contain any of these characters: / \ * [ ] : ?
#' * Cannot be empty
#' * Leading/trailing spaces are trimmed
#' If the name becomes empty after sanitization, "Sheet" is used as default.
#' If the name is too long, it's truncated and a suffix may be added to ensure uniqueness.
#' If warn=TRUE and the name was modified, a warning is issued.
#'
#' @returns A valid Excel sheet name.
#' @keywords internal
.sanitizeExcelSheetName <- function(sheetName, warn = TRUE) {
  if (is.null(sheetName) || is.na(sheetName)) {
    return("Sheet")
  }

  # Store original name for comparison
  originalName <- as.character(sheetName)

  # Convert to character and trim spaces
  sheetName <- trimws(originalName)

  # If empty after trimming, use default
  if (nchar(sheetName) == 0) {
    if (warn && originalName != "Sheet") {
      warning(messages$excelSheetEmptyOrInvalid())
    }
    return("Sheet")
  }

  # Remove invalid characters: / \ * [ ] : ?
  sanitizedName <- sheetName
  sanitizedName <- gsub("/", "_", sanitizedName) # Replace /
  sanitizedName <- gsub("\\\\", "_", sanitizedName) # Replace \
  sanitizedName <- gsub("\\*", "_", sanitizedName) # Replace *
  sanitizedName <- gsub("\\[", "_", sanitizedName) # Replace [
  sanitizedName <- gsub("\\]", "_", sanitizedName) # Replace ]
  sanitizedName <- gsub(":", "_", sanitizedName) # Replace :
  sanitizedName <- gsub("\\?", "_", sanitizedName) # Replace ?

  # Trim to 31 characters maximum
  if (nchar(sanitizedName) > 31) {
    sanitizedName <- substr(sanitizedName, 1, 31)
  }

  # Final check - if still empty (unlikely), use default
  if (nchar(trimws(sanitizedName)) == 0) {
    if (warn) {
      warning(messages$excelSheetSanitized(originalName))
    }
    return("Sheet")
  }

  # Warn if the name was changed
  if (warn && sanitizedName != originalName) {
    warning(messages$excelSheetSanitizedInfo(originalName, sanitizedName))
  }

  return(sanitizedName)
}

#' Get the number of scenarios to create based on vector arguments
#'
#' @param pkmlFilePaths Character vector of PKML file paths.
#' @param ... Other vector arguments to check for length consistency.
#'
#' @details Determines the number of scenarios to create based on the length of vector
#' arguments. All vector arguments with length > 1 must have the same length, which
#' determines the final number of scenarios.
#'
#' @returns Integer number of scenarios to create.
#' @keywords internal
.getScenarioCount <- function(pkmlFilePaths, ...) {
  args <- list(...)
  all_args <- c(list(pkmlFilePaths = pkmlFilePaths), args)
  lengths <- sapply(all_args, function(x) if (is.null(x)) 0 else length(x))

  # Filter out NULL arguments (length 0)
  valid_lengths <- lengths[lengths > 0]
  # Get lengths > 1 (vectors that determine scenario count)
  vector_lengths <- valid_lengths[valid_lengths > 1]

  if (length(vector_lengths) == 0) {
    # Case 1 & 2: No vectors with length > 1
    # If pkmlFilePaths exists and has length 1, and all other args are length 1 or NULL
    # Create exactly 1 scenario
    pkml_length <- length(pkmlFilePaths)
    if (pkml_length == 1) {
      return(1) # Cases 1 & 2: Single scenario
    } else {
      # This shouldn't happen since pkmlFilePaths length > 1 would be in vector_lengths
      return(pkml_length)
    }
  } else {
    # Cases 3, 4, 5: At least one vector with length > 1
    if (length(unique(vector_lengths)) == 1) {
      # All vectors with length > 1 have the same length
      return(vector_lengths[1])
    } else {
      # Inconsistent vector lengths
      stop(messages$inconsistentArgumentLengths(vector_lengths))
    }
  }
}

#' Recycle or validate vector arguments for scenario creation
#'
#' @param arg Vector argument to recycle or validate.
#' @param argName Character string name of the argument for error messages.
#' @param nScenarios Integer number of scenarios to create.
#'
#' @details Handles vector recycling for scenario parameters. Single values are
#' recycled to all scenarios, vectors with the correct length are used as-is,
#' and invalid lengths throw an error.
#'
#' @returns Vector with the correct length for all scenarios, or NULL if input was NULL.
#' @keywords internal
.recycleOrValidateVector <- function(arg, argName, nScenarios) {
  if (is.null(arg)) {
    return(NULL)
  }

  if (length(arg) == 1) {
    # Recycle single value to all scenarios
    return(rep(arg, nScenarios))
  } else if (length(arg) == nScenarios) {
    # Vector has same length as number of scenarios
    return(arg)
  } else {
    # Invalid length
    stop(messages$invalidArgumentLengthScenarios(
      argName,
      arg,
      nScenarios
    ))
  }
}

# Internal scenario helpers ----

#' Parse protein ontogenies from a string
#'
#' @param ontogenyString A string in the format
#'   "Molecule1:Ontogeny1,Molecule2:Ontogeny2" or NULL.
#'
#' @returns A list of `MoleculeOntogeny` objects, or NULL.
#' @keywords internal
#' @noRd
.readOntongeniesFromList <- function(ontogenyString) {
  if (
    is.null(ontogenyString) || is.na(ontogenyString) || ontogenyString == ""
  ) {
    return(NULL)
  }
  # The string format is "Molecule1:Ontogeny1,Molecule2:Ontogeny2"
  ontogeniesSplit <- unlist(strsplit(ontogenyString, split = ",", fixed = TRUE))
  ontogeniesSplit <- trimws(ontogeniesSplit)
  moleculeOntogenies <- vector("list", length(ontogeniesSplit))
  for (i in seq_along(ontogeniesSplit)) {
    parts <- unlist(strsplit(ontogeniesSplit[[i]], split = ":", fixed = TRUE))
    if (length(parts) != 2) {
      stop(messages$errorWrongOntogenyStructure(ontogeniesSplit[[i]]))
    }
    protein <- parts[[1]]
    ontogeny <- parts[[2]]
    validateEnumValue(value = ontogeny, enum = ospsuite::StandardOntogeny)
    moleculeOntogenies[[i]] <- ospsuite::MoleculeOntogeny$new(
      molecule = protein,
      ontogeny = ospsuite::StandardOntogeny[[ontogeny]]
    )
  }
  moleculeOntogenies
}

#' Read parameter values from a structured Excel file
#'
#' Each excel sheet must consist of columns 'Container Path', 'Parameter Name',
#' 'Value', and 'Units'.
#'
#' @param paramsXLSpath Path to the excel file
#' @param sheets Names of the excel sheets containing the information about the
#'   parameters. Multiple sheets can be processed. If no sheets are provided,
#'   the first one in the Excel file is used.
#'
#' @returns A list containing vectors `paths` with the full paths to the
#'   parameters, `values` the values of the parameters, and `units` with the
#'   units the values are in.
#' @keywords internal
#' @noRd
.readParametersFromXLS <- function(paramsXLSpath, sheets = NULL) {
  columnNames <- c("Container Path", "Parameter Name", "Value", "Units")
  validateIsString(paramsXLSpath)
  validateIsString(sheets, nullAllowed = TRUE)

  if (is.null(sheets)) {
    sheets <- c(1)
  }

  pathsValuesVector <- vector(mode = "numeric")
  pathsUnitsVector <- vector(mode = "character")

  for (sheet in sheets) {
    data <- readExcel(path = paramsXLSpath, sheet = sheet)

    if (!all(columnNames %in% names(data))) {
      stop(messages$errorWrongXLSStructure(
        filePath = paramsXLSpath,
        expectedColNames = columnNames
      ))
    }

    fullPaths <- paste(
      data[["Container Path"]],
      data[["Parameter Name"]],
      sep = "|"
    )
    pathsValuesVector[fullPaths] <- as.numeric(data[["Value"]])

    pathsUnitsVector[fullPaths] <- tidyr::replace_na(
      data = as.character(data[["Units"]]),
      replace = ""
    )
  }

  return(.parametersVectorToList(pathsValuesVector, pathsUnitsVector))
}

#' Read species-specific parameters from the bundled SpeciesParameters.xlsx
#'
#' @param species Character scalar — the species name (e.g. "Rat", "Mouse").
#' @returns A parameter structure list (`paths`, `values`, `units`) or `NULL`
#'   if no sheet matches the species.
#' @keywords internal
#' @noRd
.getSpeciesParameters <- function(species) {
  filePath <- system.file(
    "extdata",
    "SpeciesParameters.xlsx",
    package = "esqlabsR"
  )
  if (!file.exists(filePath)) {
    return(NULL)
  }

  sheets <- readxl::excel_sheets(filePath)
  if (!species %in% sheets) {
    return(NULL)
  }

  .readParametersFromXLS(paramsXLSpath = filePath, sheets = species)
}
