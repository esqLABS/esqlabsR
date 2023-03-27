#' Run a set of scenarios.
#'
#' @param scenarioConfigurations List of `ScenarioConfiguration` objects to be
#' simulated.
#' @param customParams A list containing vectors 'paths' with the full paths to the
#' parameters, 'values' the values of the parameters, and 'units' with the
#' units the values are in. The values to be applied to the model.
#' @param saveSimulationsToPKML Logical, defaults to `FALSE`. If `TRUE`,
#' initialized simulations are saved to PKML before simulating. The output folder
#' is the `Results/SimulationResults/<DateSuffix>`. The name of the file is the name of the scenario.
#'
#' @return A named list, where the names are scenario names, and the values are
#' lists with the entries `simulation` being the initialized `Simulation` object with applied parameters,
#' `results` being `SimulatioResults` object produced by running the simulation,
#' and `outputValues` the output values of the `SimulationResults`.
#' @export
runScenarios <- function(scenarioConfigurations, customParams = NULL,
                         saveSimulationsToPKML = FALSE) {
  .validateScenarioConfigurations(scenarioConfigurations)
  validateIsLogical(saveSimulationsToPKML)
  .validateParametersStructure(
    parameterStructure = customParams,
    argumentName = "customParams",
    nullAllowed = TRUE
  )
  # Suffix that will be appended to the name of output folder where the simulations
  # will be saved to, if specified. Have to generate it once before the loop,
  # otherwise multiple folders could be created because of time delay in initialization.
  outputFolderSuffix <- format(Sys.time(), "%F %H-%M")

  simulations <- vector("list", length(scenarioConfigurations))
  scenarioNames <- vector("character", length(scenarioConfigurations))
  populations <- vector("list", length(scenarioConfigurations))

  # For each scenario configuration, create a simulation object
  for (i in seq_along(scenarioConfigurations)) {
    scenarioConfiguration <- scenarioConfigurations[[i]]
    simulation <- initializeScenario(scenarioConfiguration = scenarioConfiguration, customParams = customParams)
    simulations[[i]] <- simulation
    scenarioNames[[i]] <- scenarioConfiguration$scenarioName
    # Defining an empty population as `NA` because test for `NULL` is painful
    population <- NA

    # Create a population for population scenarios
    if (scenarioConfiguration$simulationType == "Population") {
      popCharacteristics <- readPopulationCharacteristicsFromXLS(
        XLSpath = file.path(scenarioConfiguration$projectConfiguration$paramsFolder, scenarioConfiguration$projectConfiguration$populationParamsFile),
        populationName = scenarioConfiguration$populationId,
        sheet = "Demographics"
      )
      population <- createPopulation(populationCharacteristics = popCharacteristics)
    }
    populations[[i]] <- population

    # Save simulation to PKML
    if (saveSimulationsToPKML) {
      outputFolder <- file.path(
        scenarioConfiguration$projectConfiguration$outputFolder,
        "SimulationResults",
        outputFolderSuffix
      )
      # Create a new folder if it does not exist
      if (!dir.exists(paths = outputFolder)) {
        dir.create(path = outputFolder, recursive = TRUE)
      }

      # Save the current simulation
      outputPath <- file.path(outputFolder, paste0(scenarioNames[[i]], ".pkml"))
      tryCatch(
        {
          ospsuite::saveSimulation(
            simulation = simulation,
            filePath = outputPath
          )
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
  }
  names(simulations) <- scenarioNames
  names(scenarioConfigurations) <- scenarioNames
  names(populations) <- scenarioNames

  # Simulate individual simulations concurrently
  individualSimulationsIdx <- is.na(populations)
  simulationResults <- runSimulations(simulations = simulations[individualSimulationsIdx], simulationRunOptions = scenarioConfiguration$simulationRunOptions)

  # Run population simulations sequentially and add the to the list of simulation results
  for (scenarioName in scenarioNames[!individualSimulationsIdx]) {
    populationResults <- runSimulations(
      simulations = simulations[[scenarioName]],
      population = populations[[scenarioName]],
      simulationRunOptions = scenarioConfiguration$simulationRunOptions
    )
    simulationResults <- c(simulationResults, populationResults)
  }

  returnList <- vector("list", length(simulationResults))
  names(returnList) <- scenarioNames
  for (simulationName in scenarioNames) {
    simulation <- simulations[[simulationName]]
    results <- simulationResults[[simulation$id]]

    # Retrieving quantities from paths to support pattern matching with '*'
    outputQuantities <- NULL
    if (!is.null(scenarioConfigurations[[simulationName]]$outputPaths)) {
      outputQuantities <- getAllQuantitiesMatching(
        scenarioConfigurations[[simulationName]]$outputPaths,
        simulation
      )
    }
    outputValues <- getOutputValues(results,
      quantitiesOrPaths = outputQuantities
    )
    returnList[[simulationName]] <- list(
      simulation = simulation, results = results,
      outputValues = outputValues
    )
  }

  return(returnList)
}

#' Initialize a simulation based on scenario definition
#'
#' @description
#' Load simulation.
#' Apply parameters from global XLS.
#' Apply individual physiology.
#' Apply individual model parameters.
#' Apply test parameters (TestParameters.R).
#' Set simulation outputs (OutputPaths.R).
#' Set simulation time.
#' initializeSimulation().
#'
#' @param scenarioConfiguration A `ScenarioConfiguration` object
#' @param customParams A list with three vectors named `paths`, `values`, `units`
#' to be applied to the model
#'
#' @return Initialized `Simulation` object
#' @export
initializeScenario <- function(scenarioConfiguration, customParams = NULL) {
  # Read parameters from the parameters file
  params <- readParametersFromXLS(
    file.path(
      scenarioConfiguration$projectConfiguration$paramsFolder,
      scenarioConfiguration$projectConfiguration$paramsFile
    ),
    scenarioConfiguration$paramSheets
  )

  # Apply individual physiology, if specified
  individualCharacteristics <- NULL
  if (!is.null(scenarioConfiguration$individualId)) {
    individualCharacteristics <- readIndividualCharacteristicsFromXLS(
      XLSpath = file.path(scenarioConfiguration$projectConfiguration$paramsFolder, scenarioConfiguration$projectConfiguration$individualsFile),
      individualId = scenarioConfiguration$individualId,
      nullIfNotFound = TRUE
    )

    if (is.null(individualCharacteristics)) {
      warning(paste0(
        "No individual characteristics for individual id '",
        scenarioConfiguration$individualId, "' found."
      ))
    }

    # Find individual-specific model parameters
    excelSheets <- readxl::excel_sheets(path = file.path(
      scenarioConfiguration$projectConfiguration$paramsFolder,
      scenarioConfiguration$projectConfiguration$individualsFile
    ))

    if (any(excelSheets == scenarioConfiguration$individualId)) {
      indivModelParams <- readParametersFromXLS(file.path(
        scenarioConfiguration$projectConfiguration$paramsFolder,
        scenarioConfiguration$projectConfiguration$individualsFile
      ), sheets = scenarioConfiguration$individualId)

      # Add individual model parameters to the parameters structure
      params <- extendParameterStructure(
        parameters = params,
        newParameters = indivModelParams
      )
    } else {
      warning(paste0(
        "No individual specific model parameters for individual id '",
        scenarioConfiguration$individualId, "' found."
      ))
    }
  }

  # Apply test parameters, if specified
  if (scenarioConfiguration$setTestParameters) {
    warning("INFO: 'scenarioConfiguration$setTestParameters' is set to TRUE,
            parameter values defined in 'InputCode/TestParameters.R' will be applied!")
    params <- extendParameterStructure(
      parameters = params,
      newParameters = getTestParameters()
    )
  }
  if (!is.null(customParams)) {
    params <- extendParameterStructure(
      parameters = params,
      newParameters = customParams
    )
  }

  # Load simulation
  simulation <- ospsuite::loadSimulation(filePath = file.path(
    scenarioConfiguration$projectConfiguration$modelFolder,
    scenarioConfiguration$modelFile
  ), loadFromCache = FALSE)
  # Set the outputs, if new were specified
  if (!is.null(scenarioConfiguration$outputPaths)) {
    clearOutputs(simulation)
    addOutputs(quantitiesOrPaths = scenarioConfiguration$outputPaths, simulation = simulation)
  }
  # Set simulation time if defined by the user.
  if (!is.null(scenarioConfiguration$simulationTime)) {
    setOutputInterval(simulation = simulation, startTime = 0, endTime = scenarioConfiguration$simulationTime, resolution = scenarioConfiguration$pointsPerMinute)
  }

  initializeSimulation(
    simulation = simulation,
    individualCharacteristics = individualCharacteristics,
    additionalParams = params,
    simulateSteadyState = scenarioConfiguration$simulateSteadyState,
    steadyStateTime = scenarioConfiguration$steadyStateTime,
    simulationRunOptions = scenarioConfiguration$simulationRunOptions
  )

  # Set administration protocols
  setApplications(simulation = simulation, scenarioConfiguration = scenarioConfiguration)

  # Call the custom function.
  if (!is.null(scenarioConfiguration$customFunction)) {
    # Set the environment of the custom function to the current environment so it
    # has access to the `simulation` object
    environment(scenarioConfiguration$customFunction) <- environment()
    # Call the custom function with the arguments as defined in the scenario
    # configuration
    do.call(what = scenarioConfiguration$customFunction, args = scenarioConfiguration$customFunctionArgs)
  }

  return(simulation)
}

#' Save results of scenario simulations to csv.
#'
#' @param simulatedScenarios Named list with `simulation`, `results` and `outputValues`
#' as produced by `runScenarios()`
#' @param projectConfiguration An instance of `ProjectConfiguration`
#' @param outputFolder Optional - path to the folder where the results will be
#' stored. If `NULL` (default), a sub-folder in
#' `ProjectConfiguration$outputFolder/SimulationResults/<DateSuffix>`.
#' @param saveSimulationsToPKML If `TRUE` (default), simulations corresponding to
#' the results are saved to PKML along with the results.
#'
#' @details For each scenario, a separate csv file will be created. Results can be read with
#' the `loadScenarioResults()` function.
#'
#' @export
#'
#' @examples \dontrun{
#' projectConfiguration <- esqlabsR::createDefaultProjectConfiguration()
#' scenarioConfigurations <- readScenarioConfigurationFromExcel(
#'   projectConfiguration = projectConfiguration
#' )
#' simulatedScenarios <- runScenarios(
#'   scenarioConfigurations = scenarioConfigurations,
#'   saveSimulationsToPKML = FALSE
#' )
#' saveResults(simulatedScenarios, projectConfiguration)
#' }
saveScenarioResults <- function(simulatedScenarios, projectConfiguration, outputFolder = NULL, saveSimulationsToPKML = TRUE) {
  validateIsLogical(saveSimulationsToPKML)

  outputFolder <- outputFolder %||% file.path(
    projectConfiguration$outputFolder,
    "SimulationResults",
    format(Sys.time(), "%F %H-%M")
  )

  for (i in seq_along(simulatedScenarios)) {
    results <- simulatedScenarios[[i]]$results
    scenarioName <- names(simulatedScenarios)[[i]]

    outputPath <- file.path(outputFolder, paste0(scenarioName, ".csv"))
    tryCatch(
      {
        # Create a new folder if it does not exist
        if (!dir.exists(paths = outputFolder)) {
          dir.create(path = outputFolder, recursive = TRUE)
        }
        ospsuite::exportResultsToCSV(results = results, filePath = outputPath)
        if (saveSimulationsToPKML) {
          outputPathSim <- file.path(outputFolder, paste0(scenarioName, ".pkml"))
          ospsuite::saveSimulation(
            simulation = simulatedScenarios[[i]]$simulation,
            filePath = outputPathSim
          )
        }
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
}

#' Load simulated scenarios from csv and pkml.
#'
#' @param scenarioNames Names of simulated scenarios
#' @param resultsFolder Path to the folder where simulation results as scv and
#' the corresponding simulations as pkml are located.
#'
#' @details This function requires simulation results AND the corresponding
#' simulation files being located in the same folder (`resultsFolder`) and have
#' the names of the scenarios.
#'
#' @return A named list, where the names are scenario names, and the values are
#' lists with the entries `simulation` being the initialized `Simulation` object with applied parameters,
#' `results` being `SimulatioResults` object produced by running the simulation,
#' and `outputValues` the output values of the `SimulationResults`.
#'
#' @export
#'
#' @examples \dontrun{
#' # First simulate scenarios and save the results
#' projectConfiguration <- esqlabsR::createDefaultProjectConfiguration()
#' scenarioConfigurations <- readScenarioConfigurationFromExcel(
#'   projectConfiguration = projectConfiguration
#' )
#' simulatedScenarios <- runScenarios(
#'   scenarioConfigurations = scenarioConfigurations,
#'   saveSimulationsToPKML = TRUE
#' )
#' saveResults(simulatedScenarios, projectConfiguration)
#'
#' # Now load the results
#' scnarioNames <- names(simulatedScenarios)
#' simulatedScenarios <- loadScenarioResults(
#'   scnarioNames = scnarioNames,
#'   resultsFolder = pathToTheFolder
#' )
#' }
loadScenarioResults <- function(scenarioNames, resultsFolder) {
  simulatedScenarios <- list()
  for (i in seq_along(scenarioNames)) {
    simulation <- loadSimulation(paste0(resultsFolder, "/", scenarioNames[[i]], ".pkml"))

    results <- importResultsFromCSV(simulation = simulation, filePaths = paste0(resultsFolder, "/", scenarioNames[[i]], ".csv"))
    outputValues <- getOutputValues(results,
      quantitiesOrPaths = results$allQuantityPaths
    )
    simulatedScenarios[[scenarioNames[[i]]]] <- list(simulation = simulation, results = results, outputValues = outputValues)
  }

  return(simulatedScenarios)
}
