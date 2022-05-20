#' Run a set of scenarios.
#'
#' @param scenarioNames Names of the simulated scenarios. Scenario description is
#' stored in the `Scenarios.xlsx` file.
#' @param scenarioConfiguration A `ScenarioConfiguration` object
#' @param customParams A list containing vectors 'paths' with the full paths to the
#' parameters, 'values' the values of the parameters, and 'units' with the
#' units the values are in. The values to be applied to the model.
#' @param saveSimulationsToPKML Logical, defaults to `FALSE`. If `TRUE`,
#' initialized simulations are saved to PKML before simulating. The output folder
#' is the model folder defined in `ProjectConfiguration` with the subfolder with the
#' current timestamp. The name of the file is the name of the scenario.
#'
#' @return A named list, where the names are scenario names, and the values are
#' lists with the initialized `Simulation` object with applied parameters,
#' `SimulatioResults` objec produced by running the simulation, and output values
#' of the `SimulationResults`.
#' @export
runScenarios <- function(scenarioNames, scenarioConfiguration, customParams = NULL,
                         saveSimulationsToPKML = FALSE) {
  simulations <- vector("list", length(scenarioNames))
  for (i in seq_along(simulations)) {
    scenarioConfiguration$scenarioName <- scenarioNames[[i]]
    simulations[[i]] <- initializeScenario(scenarioConfiguration = scenarioConfiguration, customParams = customParams)
  }
  names(simulations) <- scenarioNames

  # Save simulations to PKML
  if (saveSimulationsToPKML) {
    outputFolder <- file.path(
      scenarioConfiguration$projectConfiguration$modelFolder,
      format(Sys.time(), "%F %H-%M")
    )
    # Create a new folder if it does not exist
    if (!dir.exists(paths = outputFolder)) {
      dir.create(path = outputFolder)
    }
    for (scenarioName in scenarioNames) {
      outputPath <- file.path(outputFolder, paste0(scenarioName, ".pkml"))
      tryCatch(
        {
          ospsuite::saveSimulation(
            simulation = simulations[[scenarioName]],
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

  # Simulate all simulations concurrently
  simulationResults <- runSimulations(simulations = simulations, simulationRunOptions = scenarioConfiguration$simulationRunOptions)

  returnList <- vector("list", length(simulationResults))
  names(returnList) <- scenarioNames
  for (idx in seq_along(scenarioNames)) {
    simulationName <- scenarioNames[[idx]]
    simulation <- simulations[[simulationName]]
    results <- simulationResults[[simulation$id]]
    outputValues <- getOutputValues(results,
      quantitiesOrPaths = getAllQuantitiesMatching(enumValues(OutputPaths), simulation)
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
#' Load simulation
#' Apply parameters from global XLS
#' Apply individual physiology
#' Apply individual model parameters
#' Apply test parameters (TestParameters.R)
#' Set simulation outputs (OutputPaths.R)
#' Set simulation time
#' initializeSimulation()
#'
#' @param scenarioConfiguration A `ScenarioConfiguration` object
#' @param customParams A list with three vectors named `paths`, `values`, `units`
#' to be applied to the model
#'
#' @return Initialized `Simulation` object
#' @export
initializeScenario <- function(scenarioConfiguration, customParams = NULL) {
  # Update `ScenarioConfiguration` with information from excel
  scenarioConfiguration <- readScenarioConfigurationFromExcel(scenarioConfiguration)
  # Read parameters from the parameters file
  params <- readParametersFromXLS(
    file.path(
      scenarioConfiguration$projectConfiguration$paramsFolder,
      scenarioConfiguration$projectConfiguration$paramsFile
    ),
    scenarioConfiguration$paramSheets
  )

  individualCharacteristics <- NULL
  if (!is.null(scenarioConfiguration$individualId)) {
    individualCharacteristics <- readIndividualCharacteristicsFromXLS(
      XLSpath = file.path(scenarioConfiguration$projectConfiguration$paramsFolder, scenarioConfiguration$projectConfiguration$individualPhysiologyFile),
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
      scenarioConfiguration$projectConfiguration$individualParamsFile
    ))

    if (scenarioConfiguration$individualId %in% excelSheets) {
      indivModelParams <- readParametersFromXLS(file.path(
        scenarioConfiguration$projectConfiguration$paramsFolder,
        scenarioConfiguration$projectConfiguration$individualParamsFile
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
  # Set the outputs
  clearOutputs(simulation)
  addOutputs(quantitiesOrPaths = enumValues(OutputPaths), simulation = simulation)
  # Set simulation time
  setOutputInterval(simulation = simulation, startTime = 0, endTime = scenarioConfiguration$simulationTime, resolution = scenarioConfiguration$pointsPerMinute)

  initializeSimulation(
    simulation = simulation,
    individualCharacteristics = individualCharacteristics,
    additionalParams = params,
    simulateSteadyState = scenarioConfiguration$simulateSteadyState,
    steadyStateTime = scenarioConfiguration$steadyStateTime
  )

  # Set administration protocols
  setApplications(simulation = simulation, scenarioConfiguration = scenarioConfiguration)

  return(simulation)
}
