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
#' @param projectConfiguration A `ProjectConfiguration` object holding base information.
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
#' @param paramSheets Character vector. Optional parameter sheet names to apply to scenarios.
#'   If `NULL` (default), no parameter sheets will be applied. Can be a single string (recycled for all
#'   scenarios) or a vector with the same length as `pkmlFilePaths`. If providing multiple sheets per
#'   scenario, separate them with commas in the string.
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
#'   If `NULL` (default), no steady-state time will be set. Can be a single numeric value (recycled for all scenarios)
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
#' # Load project configuration
#' pc <- loadProject("ProjectConfiguration.json")
#'
#' # Create scenarios from a single PKML file
#' pkmlPath <- "path/to/simulation.pkml"
#' scenarios <- createScenarioConfigurationsFromPKML(
#'   pkmlFilePaths = pkmlPath,
#'   projectConfiguration = pc
#' )
#'
#' # Add to project configuration and run
#' pc$scenarios <- c(pc$scenarios, scenarios)
#' results <- runScenarios(pc, scenarioNames = names(scenarios))
#'
#' # Example of vector recycling - single value applied to all scenarios
#' scenarios <- createScenarioConfigurationsFromPKML(
#'   pkmlFilePaths = c("sim1.pkml", "sim2.pkml", "sim3.pkml"),
#'   projectConfiguration = projectConfiguration,
#'   individualId = "Individual_001", # Recycled to all scenarios
#'   steadyState = TRUE,              # Recycled to all scenarios
#'   steadyStateTime = 1000           # Recycled to all scenarios
#' )
#'
#' # Example of vector arguments - different values per scenario
#' scenarios <- createScenarioConfigurationsFromPKML(
#'   pkmlFilePaths = c("pediatric.pkml", "adult.pkml", "elderly.pkml"),
#'   projectConfiguration = projectConfiguration,
#'   scenarioNames = c("Pediatric", "Adult", "Elderly"),
#'   individualId = c("Child_001", "Adult_001", "Elderly_001"),
#'   applicationProtocols = c("Pediatric_Dose", "Standard_Dose", "Reduced_Dose"),
#'   steadyState = c(FALSE, TRUE, TRUE),
#'   steadyStateTime = c(NA, 2000, 1500)
#' )
#'
#' # Example of PKML recycling - same model, different settings
#' scenarios <- createScenarioConfigurationsFromPKML(
#'   pkmlFilePaths = "base_model.pkml",                    # Single PKML recycled
#'   projectConfiguration = projectConfiguration,
#'   scenarioNames = c("LowDose", "MediumDose", "HighDose"),
#'   individualId = c("Patient1", "Patient2", "Patient3"),
#'   applicationProtocols = c("Low_Protocol", "Med_Protocol", "High_Protocol"),
#'   steadyState = c(FALSE, TRUE, TRUE)                   # Different settings per scenario
#' )
#' }
createScenarioConfigurationsFromPKML <- function(
  pkmlFilePaths,
  projectConfiguration,
  scenarioNames = NULL,
  individualId = NULL,
  populationId = NULL,
  applicationProtocols = NULL,
  paramSheets = NULL,
  outputPaths = NULL,
  simulationTime = NULL,
  simulationTimeUnit = NULL,
  steadyState = FALSE,
  steadyStateTime = NULL,
  steadyStateTimeUnit = NULL,
  overwriteFormulasInSS = FALSE,
  readPopulationFromCSV = FALSE
) {
  # Validate inputs
  validateIsCharacter(pkmlFilePaths)
  validateIsOfType(projectConfiguration, ProjectConfiguration)
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
  if (!is.null(paramSheets)) {
    validateIsCharacter(paramSheets)
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
  # Note: projectConfiguration is excluded as it should always be a single object
  nScenarios <- .getScenarioCount(
    pkmlFilePaths,
    scenarioNames,
    individualId,
    populationId,
    applicationProtocols,
    paramSheets,
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
  paramSheets <- .recycleOrValidateVector(
    paramSheets,
    "paramSheets",
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
  scenarioConfigurations <- list()
  allScenarioNames <- c()

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
    if (scenarioName %in% allScenarioNames) {
      # Duplicated scenarioName found
      # count number of existing scenarios with this name
      existingCount <- sum(allScenarioNames == originalScenarioName)
      # Make scenario name unique by appending index
      scenarioName <- paste0(scenarioName, "_", existingCount + 1)

      # Warn the user
      warning(messages$autocorrectDuplicateScenarioNames(
        originalScenarioName,
        scenarioName
      ))
    }

    allScenarioNames <- c(allScenarioNames, originalScenarioName)

    simTree <- ospsuite::getSimulationTree(simulation)

    # Create Scenario object
    sc <- Scenario$new()
    sc$scenarioName <- scenarioName
    sc$modelFile <- path_rel(
      pkmlPath,
      start = projectConfiguration$modelFolder
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
    if (!is.null(paramSheets)) {
      # paramSheets can be a character vector for this scenario
      scenarioParamSheets <- paramSheets[[i]]
      if (
        !is.null(scenarioParamSheets) &&
          !is.na(scenarioParamSheets) &&
          nchar(scenarioParamSheets) > 0
      ) {
        # Split by comma if it's a single string with multiple sheets
        if (
          is.character(scenarioParamSheets) && length(scenarioParamSheets) == 1
        ) {
          scenarioParamSheets <- trimws(strsplit(scenarioParamSheets, ",")[[1]])
        }
        sc$parameterGroups <- scenarioParamSheets
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

    scenarioConfigurations[[scenarioName]] <- sc
  }

  return(scenarioConfigurations)
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

#' Add a scenario programmatically to a ProjectConfiguration
#'
#' @description Creates a new `Scenario` and adds it to the
#'   `projectConfiguration$scenarios` list after validating all references.
#'
#' @param projectConfiguration A `ProjectConfiguration` object.
#' @param scenarioName Character. Name for the new scenario. Must not already
#'   exist in `projectConfiguration$scenarios`.
#' @param modelFile Character. Name of the `.pkml` model file (relative to
#'   model folder).
#' @param individualId Character or NULL. ID referencing
#'   `projectConfiguration$individuals`.
#' @param populationId Character or NULL. ID referencing
#'   `projectConfiguration$populations`.
#' @param applicationProtocol Character or NULL. Protocol name referencing
#'   `projectConfiguration$applications`.
#' @param parameterGroups Character vector or NULL. Group names referencing
#'   `projectConfiguration$modelParameters`.
#' @param outputPathIds Character vector or NULL. IDs referencing
#'   `projectConfiguration$outputPaths`.
#' @param simulationTime Character or NULL. Format `"start, end, resolution"`
#'   or `"start, end, resolution; start, end, resolution"` for multiple
#'   intervals.
#' @param simulationTimeUnit Character. Time unit string. Default `"h"`.
#' @param steadyState Logical. Whether to simulate steady state. Default
#'   `FALSE`.
#' @param steadyStateTime Numeric. Steady-state time in minutes. Default
#'   `1000`.
#' @param overwriteFormulasInSS Logical. Overwrite formulas during steady
#'   state. Default `FALSE`.
#' @param readPopulationFromCSV Logical. Load population from CSV. Default
#'   `FALSE`.
#'
#' @returns The `projectConfiguration` object, invisibly.
#'
#' @export
#' @family scenario
addScenario <- function(
    projectConfiguration,
    scenarioName,
    modelFile,
    individualId = NULL,
    populationId = NULL,
    applicationProtocol = NULL,
    parameterGroups = NULL,
    outputPathIds = NULL,
    simulationTime = NULL,
    simulationTimeUnit = "h",
    steadyState = FALSE,
    steadyStateTime = 1000,
    overwriteFormulasInSS = FALSE,
    readPopulationFromCSV = FALSE) {
  pc <- projectConfiguration
  errors <- character()

  # Validate required args
  if (!is.character(scenarioName) || length(scenarioName) != 1 || nchar(scenarioName) == 0) {
    errors <- c(errors, "scenarioName must be a non-empty string")
  } else if (scenarioName %in% names(pc$scenarios)) {
    errors <- c(errors, paste0("scenario '", scenarioName, "' already exists"))
  }

  if (!is.character(modelFile) || length(modelFile) != 1 || nchar(modelFile) == 0) {
    errors <- c(errors, "modelFile must be a non-empty string")
  }

  # Validate optional references
  if (!is.null(individualId) && !(individualId %in% names(pc$individuals))) {
    errors <- c(errors, paste0("individualId '", individualId, "' not found in individuals"))
  }
  if (!is.null(populationId) && !(populationId %in% names(pc$populations))) {
    errors <- c(errors, paste0("populationId '", populationId, "' not found in populations"))
  }
  if (!is.null(applicationProtocol) && !(applicationProtocol %in% names(pc$applications))) {
    errors <- c(errors, paste0("applicationProtocol '", applicationProtocol, "' not found in applications"))
  }
  if (!is.null(parameterGroups)) {
    bad <- setdiff(parameterGroups, names(pc$modelParameters))
    if (length(bad) > 0) {
      errors <- c(errors, paste0(
        "parameterGroups not found in modelParameters: ",
        paste(bad, collapse = ", ")
      ))
    }
  }
  if (!is.null(outputPathIds)) {
    bad <- setdiff(outputPathIds, names(pc$outputPaths))
    if (length(bad) > 0) {
      errors <- c(errors, paste0(
        "outputPathIds not found in outputPaths: ",
        paste(bad, collapse = ", ")
      ))
    }
  }

  if (length(errors) > 0) {
    stop(paste0(
      "Cannot add scenario '", scenarioName, "':\n- ",
      paste(errors, collapse = "\n- ")
    ))
  }

  # Build Scenario object
  sc <- Scenario$new()
  sc$scenarioName <- scenarioName
  sc$modelFile <- modelFile
  sc$individualId <- individualId
  sc$applicationProtocol <- applicationProtocol %||% NA

  if (!is.null(populationId)) {
    sc$populationId <- populationId
    sc$simulationType <- "Population"
  }

  sc$parameterGroups <- parameterGroups
  sc$readPopulationFromCSV <- readPopulationFromCSV

  if (!is.null(outputPathIds)) {
    sc$outputPaths <- unname(pc$outputPaths[outputPathIds])
  }

  if (!is.null(simulationTime)) {
    sc$simulationTime <- .parseSimulationTimeIntervals(simulationTime)
    sc$simulationTimeUnit <- simulationTimeUnit
  }

  sc$simulateSteadyState <- steadyState
  sc$steadyStateTime <- steadyStateTime
  sc$overwriteFormulasInSS <- overwriteFormulasInSS

  # Add to configuration
  pc$scenarios[[scenarioName]] <- sc
  # Access private via environment to set modified flag
  pc_env <- pc$.__enclos_env__$private
  pc_env$.modified <- TRUE

  invisible(pc)
}
