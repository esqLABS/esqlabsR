#' @title Scenario Object
#' @description A class representing a simulation scenario. Contains all the
#' parameters that defines a scenario: Its configuration, the base pkml model,
#' the simulation parameters and the resulting simulation object.
Scenario <- R6::R6Class(
  "Scenario",
  public = list(
    #' @field id The name of the scenario
    id = NULL,
    #' @field status Scenario status can be:
    #' - active: The scenario is ready to be loaded
    #' - inactive: The scenario is not active and will not be loaded/run
    #' - loaded: The scenario is loaded and ready to be run
    #' - warning: #TODO
    #' - error: #TODO
    status = NULL,
    #' @field type Whether it is a population or individual simulation scenario
    type = NULL,
    #' @field simulateSteadyState Whether the scenario is a steady state simulation
    simulateSteadyState = NULL,
    #' @field readPopulationFromCSV Whether the population is read from a CSV file
    readPopulationFromCSV = NULL,
    #' @description Creates a new scenario object
    #' @param project The project in which the scenario is created
    #' @param scenarioConfiguration a `scenarioConfiguration` object
    #' @param status The initial status of the scenario to set
    initialize = function(project, scenarioConfiguration, status = NULL) {
      private$.project <- project
      private$.configuration <- scenarioConfiguration
      self$id <- private$.configuration$id
      self$status <- status %||% "active"
      self$type <- ifelse(!is.na(private$.configuration$population), "population", "individual")
      self$readPopulationFromCSV <- as.logical(private$.configuration$populationFromCSV)
      self$simulateSteadyState <- private$.configuration$steadyState
    },
    #' @description Prints the scenario object
    #' @param lod The level of detail to print
    #' - 1: Print only the scenario id and its status
    #' - 2 (default): Print all scenario parameters.
    print = function(lod = 2) {
      scenarioStatus <- if (self$status == "active") {
        "\U02705"
      } else if (self$status == "inactive") {
        "\U023F8"
      } else if (self$status == "loaded") {
        "\U23FA"
      }

      if (lod == 1) {
        cli_ul()
        cli_li(paste(scenarioStatus, self$id))
        cli_end()
      }

      if (lod >= 2) {
        cli_ul()
        cli_li("Status: {scenarioStatus} {self$status}")
        cli_li("Type: {self$type}")
        cli_li("Simulation Time:")
        simulationTimes <- cli_ul()
        purrr::map(self$simulationTime, ~ cli_li(.x$summary))
        cli_end(simulationTimes)
        cli_li("Simulate steady state: {self$simulateSteadyState %||% FALSE}")
        if (self$simulateSteadyState) {
          steadyStateTime <- cli_ul()
          cli_li("Steady State Time: {self$steadyStateTime$time} {self$steadyStateTime$timeUnit}")
          cli_end(steadyStateTime)
        }
        cli_li("Output Paths:")
        outputPaths <- cli_ul()
        purrr::imap(self$outputPaths, ~ cli_li("{.y}"))
        cli_end(outputPaths)
        cli_li("Configurations:")
        configurations <- cli_ul()
        cli_li("Model: {private$.configuration$model}")
        cli_li("Model Parameters:")
        model_parameters <- cli_ul()
        purrr::map(private$.configuration$modelParameters, ~ cli_li(.x))
        cli_end(model_parameters)
        if (self$type == "population") {
          cli_li("Population: {private$.configuration$population}")
        } else {
          cli_li("Individual: {private$.configuration$individual}")
        }
        cli_li("Applications:")
        applications_parameters <- cli_ul()
        purrr::map(private$.configuration$applications, ~ cli_li(.x))
        cli_end(applications_parameters)
        cli_end(configurations)
      }

      invisible(self)
    },
    #' @description
    #' Add additional parameters to the scenario to apply to the simulation
    #' @param parameters Optional named list with lists 'paths', 'values', and
    #'   'units'.
    addParameters = function(parameters) {
      .validateParametersStructure(parameters)

      private$.additionalParameters <- parameters

      invisible(self)
    },
    #' @description Converts the Scenario object to a deeply nested list,
    #' excluding specified fields like pointers.
    #' @return A list representation of the Scenario object.
    toList = function() {
      # Convert the Scenario object to a list
      scenario_list <- toListRecursive(self)
      return(scenario_list)
    },
    #' @description If status is "active", load the scenario. Loading the
    #' scenario means applying all configuration parameters to the model.
    load = function() {
      if (self$status == "active") {
        # Model Parameters
        private$.applyModelParameters()

        # Individual Parameters
        private$.applyIndividualParameters()

        # Applications parameters
        private$.applyApplicationsParameters()


        # Set output paths
        private$.applyOutputPaths()

        # Set simulation Time
        private$.applySimulationTime()

        # Apply Additional Parameters
        private$.applyAdditionalParameters()


        self$status <- "loaded"
      }
      invisible(self)
    }
  ),
  active = list(
    #' @field model path of the scenario's pkml model file.
    model = function(value) {
      if (!missing(value)) {
        cli::cli_abort(messages$LockedScenarios(self$id))
      }
      if (is.null(private$.model)) {
        private$.model <- file.path(
          private$.project$projectConfiguration$modelFolder,
          private$.configuration$model
        )
      }
      return(private$.model)
    },
    #' @field simulation Loaded simulation for the scenario.
    simulation = function(value) {
      if (!missing(value)) {
        cli::cli_abort(messages$LockedScenarios(self$id))
      }

      if (is.null(private$.simulation)) {
        private$.simulation <- ospsuite::loadSimulation(self$model)
      }
      return(private$.simulation)
    },
    #' @field outputPaths Output paths for the scenario.
    outputPaths = function(value) {
      if (!missing(value)) {
        cli::cli_abort(messages$LockedScenarios(self$id))
      }

      if (is.null(private$.outputPaths) & !all(is.na(private$.configuration$outputPaths))) {
        private$.outputPaths <- list()
        for (outputPath in private$.configuration$outputPaths) {
          private$.outputPaths[[outputPath]] <- private$.project$configurations$outputPaths[[outputPath]]
        }
      }
      return(private$.outputPaths)
    },
    #' @field modelParameters Model parameters to apply to the scenario.
    modelParameters = function(value) {
      if (!missing(value)) {
        cli::cli_abort(messages$LockedScenarios(self$id))
      }

      if (is.null(private$.modelParameters) & !all(is.na(private$.configuration$modelParameters))) {
        private$.modelParameters <- list()

        for (modelParameter in private$.configuration$modelParameters) {
          private$.modelParameters[[modelParameter]] <- private$.project$configurations$modelParameters[[modelParameter]]
        }
      }
      return(private$.modelParameters)
    },
    #' @field individual Individual parameters to apply to the scenario.
    individual = function(value) {
      if (!missing(value)) {
        cli::cli_abort(messages$LockedScenarios(self$id))
      }

      if (is.null(private$.individual) & !all(is.na(private$.configuration$individual))) {
        private$.individual <- private$.project$configurations$individuals[[private$.configuration$individual]]
      }
      return(private$.individual)
    },
    #' @field population Population parameters to apply to the scenario.
    population = function(value) {
      if (!missing(value)) {
        cli::cli_abort(messages$LockedScenarios(self$id))
      }

      if (is.null(private$.population) & !is.na(private$.configuration$population)) {
        if (self$readPopulationFromCSV) {
          private$.population <- private$.project$configurations$populations$fromCSV[[private$.configuration$population]]
        } else {
          private$.population <- private$.project$configurations$populations$fromConfiguration[[private$.configuration$population]]
        }
      }
      return(private$.population)
    },
    #' @field applications Applications parameters to apply to the scenario.
    applications = function(value) {
      if (!missing(value)) {
        cli::cli_abort(messages$LockedScenarios(self$id))
      }

      if (is.null(private$.applications) & !all(is.na(private$.configuration$applications))) {
        private$.applications <- list()
        for (application in private$.configuration$applications) {
          private$.applications[[application]] <- private$.project$configurations$applications[[application]]
        }
      }
      return(private$.applications)
    },
    #' @field simulationTime SimulationTime to run the scenario
    simulationTime = function(value) {
      if (!missing(value)) {
        cli::cli_abort(messages$LockedScenarios(self$id))
      }

      if (is.null(private$.simulationTime) & !all(is.na(private$.configuration$simulationTime))) {
        private$.simulationTime <- purrr::map(
          private$.configuration$simulationTime,
          ~ SimulationTime$new(
            simulationTime = .x,
            simulationTimeUnit = private$.configuration$simulationTimeUnit
          )
        )
      }
      return(private$.simulationTime)
    },
    #' @field steadyStateTime SteadyStateTime to run the scenario
    steadyStateTime = function(value) {
      if (!missing(value)) {
        cli::cli_abort(messages$LockedScenarios(self$id))
      }

      if (is.null(private$.steadyStateTime) & isTRUE(private$.configuration$steadyState)) {
        steadyStateTime <-
          private$.steadyStateTime <- SteadyStateTime$new(
            steadyStateTime =
              if (is.na(private$.configuration$steadyStateTime)) {
                NULL
              } else {
                private$.configuration$steadyStateTime
              },
            steadyStateTimeUnit =
              if (is.na(private$.configuration$steadyStateTimeUnit)) {
                NULL
              } else {
                private$.configuration$steadyStateTimeUnit
              }
          )
      }
      return(private$.steadyStateTime)
    }
  ),
  private = list(
    .scenarioConfigurationData = NULL,
    .configuration = NULL,
    .project = NULL,
    .model = NULL,
    .outputPaths = NULL,
    .modelParameters = NULL,
    .additionalParameters = NULL,
    .applications = NULL,
    .simulation = NULL,
    .individual = NULL,
    .population = NULL,
    .simulationTime = NULL,
    .steadyStateTime = NULL,
    .applyModelParameters = function() {
      allModelParameters <-
        purrr::flatten(self$modelParameters) %>%
        purrr::map(~ .x$parameterObject) %>%
        flattenParameterObjects()

      ospsuite::setParameterValuesByPath(
        simulation = self$simulation,
        parameterPaths = allModelParameters$paths,
        values = allModelParameters$values,
        units = allModelParameters$units,
        stopIfNotFound = FALSE
      )
    },
    # @description Apply an individual to the simulation.
    # For human species, only parameters that do not override formulas are applied.
    # For other species, all parameters returned by `createIndividual` are applied.
    .applyIndividualParameters = function() {
      individualObject <- self$individual$individualObject

      individual <- individualObject$characteristics

      # For human species, only set distributed parameters
      allParamPaths <- individual$distributedParameters$paths
      allParamValues <- individual$distributedParameters$values
      allParamUnits <- individual$distributedParameters$units

      # For other species, also add derived parameters
      if (self$individual$characteristics$specy != ospsuite::Species$Human) {
        allParamPaths <- c(allParamPaths, individual$derivedParameters$paths)
        allParamValues <- c(allParamValues, individual$derivedParameters$values)
        allParamUnits <- c(allParamUnits, individual$derivedParameters$units)
      }

      # Apply characteristics
      ospsuite::setParameterValuesByPath(
        simulation = self$simulation,
        parameterPaths = allParamPaths,
        values = allParamValues,
        units = allParamUnits,
        stopIfNotFound = FALSE
      )

      # Apply individual parameters
      ospsuite::setParameterValuesByPath(
        simulation = self$simulation,
        parameterPaths = individualObject$parameters$paths,
        values = individualObject$parameters$values,
        units = individualObject$parameters$units,
        stopIfNotFound = FALSE
      )
    },
    # @description Apply application protocols to the simulation.
    .applyApplicationsParameters = function() {
      allApplications <-
        purrr::flatten(self$applications) %>%
        purrr::map(~ .x$parameterObject) %>%
        flattenParameterObjects()

      ospsuite::setParameterValuesByPath(
        simulation = self$simulation,
        parameterPaths = allApplications$paths,
        values = allApplications$values,
        units = allApplications$units,
        stopIfNotFound = FALSE
      )
    },
    # @description Apply output intervals (simulation time) to the simulation
    .applySimulationTime = function() {
      if (!is.null(self$simulationTime)) {
        # clear output intervals
        clearOutputIntervals(self$simulation)
        # Iterate through all output intervals and add them to simulation
        for (simulationTime in self$simulationTime) {
          addOutputInterval(
            simulation = self$simulation,
            startTime = toBaseUnit(
              quantityOrDimension = ospsuite::ospDimensions$Time,
              values = simulationTime$startTime,
              unit = simulationTime$simulationTimeUnit
            ),
            endTime = toBaseUnit(
              quantityOrDimension = ospsuite::ospDimensions$Time,
              values = simulationTime$endTime,
              unit = simulationTime$simulationTimeUnit
            ),
            resolution = simulationTime$resolution / toBaseUnit(
              quantityOrDimension = ospsuite::ospDimensions$Time,
              values = 1,
              unit = simulationTime$simulationTimeUnit
            )
          )
        }
      }
    },
    # @description Apply outputpaths to the simulation
    .applyOutputPaths = function() {
      if (!is.null(self$outputPaths)) {
        setOutputs(quantitiesOrPaths = self$outputPaths, simulation = self$simulation)
      }
    },
    # @description Apply additional parameters
    .applyAdditionalParameters = function() {
      # Apply additional parameters
      if (!is.null(private$.additionalParameters)) {
        # Skip if the correct structure is supplied, but no parameters are defined
        if (!isEmpty(private$.additionalParameters$paths)) {
          ospsuite::setParameterValuesByPath(
            parameterPaths = private$.additionalParameters$paths,
            values = private$.additionalParameters$values,
            simulation = self$simulation,
            units = private$.additionalParameters$units
          )
        }
      }
    }
  )
)



# Legacy Code -------------------------------------------------------------



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
#' @return `outputFolder` or the created output folder path, if no `outputFolder` was provided.
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
    saveSimulationsToPKML = TRUE) {
  validateIsLogical(saveSimulationsToPKML)

  outputFolder <- outputFolder %||% file.path(
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
        # Save results
        ospsuite::exportResultsToCSV(results = results, filePath = outputPath)
        # Save simulations
        if (saveSimulationsToPKML) {
          outputPathSim <- file.path(outputFolder, paste0(scenarioName, ".pkml"))
          ospsuite::saveSimulation(
            simulation = simulatedScenariosResults[[i]]$simulation,
            filePath = outputPathSim
          )
        }
        # Save population
        if (isOfType(simulatedScenariosResults[[i]]$population, "Population")) {
          ospsuite::exportPopulationToCSV(simulatedScenariosResults[[i]]$population,
            filePath = file.path(outputFolder, paste0(scenarioName, "_population.csv"))
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
  return(outputFolder)
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

    simulation <- loadSimulation(paste0(resultsFolder, "/", scenarioNameForPath, ".pkml"))

    results <- importResultsFromCSV(
      simulation = simulation,
      filePaths = paste0(resultsFolder, "/", scenarioNameForPath, ".csv")
    )

    outputValues <- getOutputValues(results,
      quantitiesOrPaths = allQuantityPaths
    )
    simulatedScenariosResults[[scenarioNames[[i]]]] <-
      list(simulation = simulation, results = results, outputValues = outputValues)
  }

  return(simulatedScenariosResults)
}
