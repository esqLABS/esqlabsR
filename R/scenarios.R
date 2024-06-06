#' @title Scenario Object
#' @description A class representing a simulation scenario. Contains all the
#' parameters that defines a scenario: Its configuration, the base pkml model,
#' the simulation parameters and the resulting simulation object.
Scenario <- R6::R6Class(
  "Scenario",
  public = list(
    #' @field name The name of the scenario
    name = NULL,
    #' @field status Scenario status can be:
    #' - active: The scenario is ready to be loaded
    #' - inactive: The scenario is not active and will not be loaded/run
    #' - loaded: The scenario is loaded and ready to be run
    #' - warning: #TODO
    #' - error: #TODO
    status = NULL,
    #' @field type Whether it is a population or individual simulation scenario
    type = NULL,
    #' @field steadyState Whether the scenario is a steady state simulation
    simulateSteadyState = NULL,
    #' @description Creates a new scenario object
    #' @param project The project in which the scenario is created
    #' @param scenarioConfigurationData a `ScenarioConfiguration` object
    #' @param status The initial status of the scenario to set
    initialize = function(project, scenarioConfiguration, status = NULL) {
      private$.project <- project
      private$.configuration <- scenarioConfiguration
      self$name <- private$.configuration$id
      self$status <- status %||% "active"
      self$type <- ifelse(!is.na(private$.configuration$population), "population", "individual")
      self$simulateSteadyState <- private$.configuration$steadyState
    },
    #' @description Prints the scenario object
    #' @param lod The level of detail to print
    #' - 1: Print only the scenario name and its status
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
        cli_li(paste(scenarioStatus, self$name))
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
        cli_li("Model: {self$configuration$model}")
        cli_li("Model Parameters:")
        model_parameters <- cli_ul()
        purrr::map(self$configuration$modelParameters, ~ cli_li(.x))
        cli_end(model_parameters)
        cli_li("Individual: {self$configuration$individual}")
        cli_li("Applications:")
        applications_parameters <- cli_ul()
        purrr::map(self$configuration$applications, ~ cli_li(.x))
        cli_end(applications_parameters)
        cli_end(configurations)
      }
    },
    #' @description If status is "active", load the scenario. Loading the
    #' scenario means applying all configuration parameters to the model.
    load = function() {
      if (self$status == "active") {
        # Model Parameters
        private$.applyModelParameters()

        # Individual Parameters
        # private$.applyIndividualParameters()

        # Applications parameters
        private$.applyApplicationsParameters()

        # Population parameters

        # Set output paths
        private$.applyOutputPaths()

        # Set simulation Time
        private$.applySimulationTime()


        self$status <- "loaded"
      }
      invisible(self)
    }
  ),
  active = list(
    #' @field configuration Retrieve all the  parameters corresponding the the
    #' scenario configuration.
    configuration = function(value) {
      if (!missing(value)) {
        stop("Configuration cannot be set, modify scenario configuration by accessing project$configurations$scenarios")
      }

      return(private$.configuration)
    },
    #' @field model path of the scenario's pkml model file.
    model = function() {
      if (is.null(private$.model)) {
        private$.model <- file.path(
          private$.project$projectConfiguration$modelFolder,
          self$configuration$model
        )
      }
      return(private$.model)
    },
    #' @field simulation Loaded simulation for the scenario.
    simulation = function() {
      if (is.null(private$.simulation)) {
        private$.simulation <- ospsuite::loadSimulation(self$model)
      }
      return(private$.simulation)
    },
    #' @field outputPaths Output paths for the scenario.
    outputPaths = function() {
      if (is.null(private$.outputPaths)) {
        private$.outputPaths <- list()
        for (outputPath in self$configuration$outputPaths) {
          private$.outputPaths[[outputPath]] <- private$.project$configurations$outputPaths[[outputPath]]
        }
      }
      return(private$.outputPaths)
    },
    #' @field modelParameters Model parameters to apply to the scenario.
    modelParameters = function() {
      if (is.null(private$.modelParameters)) {
        private$.modelParameters <- list()

        for (modelParameter in self$configuration$modelParameters) {
          private$.modelParameters[[modelParameter]] <- private$.project$configurations$modelParameters[[modelParameter]]
        }
      }
      return(private$.modelParameters)
    },
    #' @field individual Individual parameters to apply to the scenario.
    individual = function() {
      if (is.null(private$.individual)) {
        private$.individual <- private$.project$configurations$individuals[self$configuration$individual]
      }
      return(private$.individual)
    },
    #' @field applications Applications parameters to apply to the scenario.
    applications = function() {
      if (is.null(private$.applications)) {
        private$.applications <- list()
        for (application in self$configuration$applications) {
          private$.applications[[application]] <- private$.project$configurations$applications[[application]]
        }
      }
      return(private$.applications)
    },
    #' @field simulationTime SimulationTime to run the scenario
    simulationTime = function() {
      if (is.null(private$.simulationTime)) {
        private$.simulationTime <- purrr::map(
          self$configuration$simulationTime,
          ~ SimulationTime$new(
            simulationTime = .x,
            simulationTimeUnit = self$configuration$simulationTimeUnit
          )
        )
      }
      return(private$.simulationTime)
    },
    steadyStateTime = function() {
      if (is.null(private$.steadyStateTime)) {
        private$.steadyStateTime <- SteadyStateTime$new(
          steadyStateTime = self$configuration$steadyStateTime,
          steadyStateTimeUnit = self$configuration$steadyStateTimeUnit
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
    .applications = NULL,
    .simulation = NULL,
    .individual = NULL,
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
    .applyIndividualParameters = function() {

      individualObject <- self$individual[[1]]$individualObject

      individual <- individualObject$characteristics

      # For human species, only set distributed parameters
      allParamPaths <- individual$distributedParameters$paths
      allParamValues <- individual$distributedParameters$values
      allParamUnits <- individual$distributedParameters$units

      # For other species, also add derived parameters
      if (self$configuration$individual[[1]]$characteristics$specy != ospsuite::Species$Human) {
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
    .applyOutputPaths = function() {
      if (!is.null(self$outputPaths)) {
        setOutputs(quantitiesOrPaths = self$outputPaths, simulation = self$simulation)
      }
    }
  )
)
