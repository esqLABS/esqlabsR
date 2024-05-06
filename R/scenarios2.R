#' @title Scenario
#' @docType class
#' @description Simulation scenario
#' @format NULL
#' @export
Scenario2 <- R6::R6Class(
  "Scenario2",
  public = list(
    name = NULL,
    status = NULL,
    type = NULL,
    steadyState = NULL,
    initialize = function(project, scenarioConfigurationData, status = NULL) {
      private$.project <- project
      private$.scenarioConfigurationData <- scenarioConfigurationData
      self$name <- private$.scenarioConfigurationData$id
      self$status <- status %||% "active"
      self$type <- ifelse(!is.na(scenarioConfigurationData$population), "population", "individual")
      self$steadyState <-  private$.scenarioConfigurationData$steadyState
    },
    print = function(lod = 2) {
      scenarioState <- if (self$status == "active") {
        "\U02705"
      } else if (self$status == "inactive") {
        "\U023F8"
      } else if (self$status == "loaded") {
        "\U23FA"
      }

      if (lod == 1) {
        cli_ul()
        cli_li(paste(scenarioState, self$name))
        cli_end()
      }

      if (lod >= 2) {
        cli_ul()
        cli_li("Status: {scenarioState} {self$status}")
        cli_li("Type: {self$type}")
        cli_li("simulateSteadyTime: {self$simulateSteadyTime}")
        cli_li("Configurations:")
        configurations <- cli_ul()
        cli_li("Model: {self$configuration$model}")
        cli_li("Model Parameters:")
        model_parameters <- cli_ul()
        purrr::map(names(self$configuration$modelParameters), ~ cli_li(.x))
        cli_end(model_parameters)
        cli_li(paste("Individual:", names(self$configuration$individual)))
        cli_li("Applications:")
        applications_parameters <- cli_ul()
        purrr::map(names(self$configuration$applications), ~ cli_li(.x))
        cli_end(applications_parameters)
        cli_end(configurations)
      }
    },
    load = function() {
      if (self$status == "active"){
        # Model Parameters
        private$.applyModelParameters()

        # Individual Parameters
        private$.applyIndividualParameters()

        # Applications parameters
        private$.applyApplicationsParameters()

        # Population parameters

        # Set output paths

        # Set simulation Time
        self$status <- "loaded"
      }
      invisible(self)
    }
  ),
  active = list(
    configuration = function(value) {
      if (!missing(value)) {
        stop("Configuration cannot be set, modify scenario configuration by accessing project$configurations$scenarios")
      }

      if (is.null(private$.configuration)) {
        private$.configuration <- list()
        private$.configuration$model <- private$.scenarioConfigurationData$model

        private$.configuration$modelParameters <- list()

        for (modelParameter in private$.scenarioConfigurationData$modelParameters) {
          private$.configuration$modelParameters[[modelParameter]] <- private$.project$configurations$modelParameters[[modelParameter]]
        }

        private$.configuration$individual <- private$.project$configurations$individuals[private$.scenarioConfigurationData$individual]

        private$.configuration$applications <- list()
        for (application in private$.scenarioConfigurationData$applications) {
          private$.configuration$applications[[application]] <- private$.project$configurations$applications[[application]]
        }
      }

      return(private$.configuration)
    },
    model = function() {
      if (is.null(private$.model)) {
        private$.model <- file.path(
          private$.project$projectConfiguration$modelFolder,
          self$configuration$model
        )
      }
      return(private$.model)
    },
    simulation = function() {
      if (is.null(private$.simulation)) {
        private$.simulation <- ospsuite::loadSimulation(self$model)
      }
      return(private$.simulation)
    },
    modelParameters = function() {
      if (is.null(private$.modelParameters)) {
        private$.modelParameters <-
          purrr::flatten(self$configuration$modelParameters) %>%
          purrr::map(~ .x$parameterObject) %>%
          flattenParameterObjects()
      }
      return(private$.modelParameters)
    },
    individual = function() {
      if (is.null(private$.individual)) {
        private$.individual <- self$configuration$individual[[1]]$individualObject
      }
      return(private$.individual)
    },
    applications = function(){
      if (is.null(private$.applications)) {
        private$.applications <-
          purrr::flatten(self$configuration$applications) %>%
          purrr::map(~ .x$parameterObject) %>%
          flattenParameterObjects()
      }
      return(private$.applications)
    }
  ),
  private = list(
    .scenarioConfigurationData = NULL,
    .configuration = NULL,
    .project = NULL,
    .model = NULL,
    .modelParameters = NULL,
    .applications = NULL,
    .simulation = NULL,
    .individual = NULL,
    .applyModelParameters = function() {
      ospsuite::setParameterValuesByPath(
        simulation = self$simulation,
        parameterPaths = self$modelParameters$paths,
        values = self$modelParameters$values,
        units = self$modelParameters$units,
        stopIfNotFound = FALSE
      )
    },
    .applyIndividualParameters = function() {
      individual <- self$individual$characteristics

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
        parameterPaths = self$individual$parameters$paths,
        values = self$individual$parameters$values,
        units = self$individual$parameters$units,
        stopIfNotFound = FALSE
      )
    },
    .applyApplicationsParameters = function(){
      ospsuite::setParameterValuesByPath(
        simulation = self$simulation,
        parameterPaths = self$applications$paths,
        values = self$applications$values,
        units = self$applications$units,
        stopIfNotFound = FALSE
      )
    }
  )
)
