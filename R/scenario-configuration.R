#' @title Scenario Configuration Object
#' @description A class holding all the configuration data for a single scenario.
ScenarioConfiguration <-
  R6::R6Class(
    classname = "ScenarioConfiguration",
    public = list(
      #' @description Creates a new scenario object
      #' @param project The project in which the scenario is created
      #' @param scenarioConfigurationData a row from the scenario configuration data frame
      initialize = function(project, scenarioConfigurationData) {
        private$.project <- project
        private$.scenarioConfigurationData$id <- scenarioConfigurationData$Scenario_name
        self$individual <- scenarioConfigurationData$IndividualId
        self$population <- scenarioConfigurationData$PopulationId
        self$populationFromCSV <- scenarioConfigurationData$ReadPopulationFromCSV
        self$modelParameters <- splitCellValues(scenarioConfigurationData$ModelParameterSheets)
        self$applications <- splitCellValues(scenarioConfigurationData$ApplicationProtocol)
        self$simulationTime <- splitCellValues(scenarioConfigurationData$SimulationTime, ";")
        self$simulationTimeUnit <- splitCellValues(scenarioConfigurationData$SimulationTimeUnit, ";")
        self$steadyState <- scenarioConfigurationData$SteadyState
        self$steadyStateTime <- scenarioConfigurationData$SteadyStateTime
        self$steadyStateTimeUnit <- scenarioConfigurationData$SteadyStateTimeUnit
        self$model <- scenarioConfigurationData$ModelFile
        self$outputPaths <- splitCellValues(scenarioConfigurationData$OutputPathsIds)
      },
      #' @description Prints the scenario configuration object
      print = function() {
        print(private$.scenarioConfigurationData)
      },
      #' @description Converts the scenario configuration object to a data frame
      toDataFrame = function(){
        return(
          tibble::tibble(
            Scenario_name = self$id,
            IndividualId = self$individual,
            PopulationId = self$population,
            ReadPopulationFromCSV = self$populationFromCSV,
            ModelParameterSheets = mergeCellValues(self$modelParameters),
            ApplicationProtocol = mergeCellValues(self$applications),
            SimulationTime = mergeCellValues(self$SimulationTime, ";"),
            SimulationTimeUnit = mergeCellValues(self$SimulationTimeUnit, ";"),
            SteadyState = self$steadyState,
            SteadyStateTime = self$steadyStateTime,
            SteadyStateTimeUnit = self$steadyStateTimeUnit,
            ModelFile = self$model,
            OutputPathsIds = mergeCellValues(self$outputPaths)
          )
        )
      }
    ),
    private = list(
      .project = NULL,
      .scenarioConfigurationData = list()
    ),
    active = list(
      #' @field id Scenario name
      id = function(value) {
        if (!missing(value)) {
          stop("id is read-only.")
        }
        return(private$.scenarioConfigurationData$id)
      },
      #' @field individual Individual ID used for the scenario
      individual = function(value) {
        if (!missing(value)) {
          private$.scenarioConfigurationData$individual <- value
        }
        return(private$.scenarioConfigurationData$individual)
      },
      #' @field population Population ID used for the scenario
      population = function(value) {
        if (!missing(value)) {
          private$.scenarioConfigurationData$population <- value
        }
        return(private$.scenarioConfigurationData$population)
      },
      #' @field populationFromCSV Flag indicating if the population should be read from a CSV file
      populationFromCSV = function(value) {
        if (!missing(value)) {
          private$.scenarioConfigurationData$populationFromCSV <- value
        }
        return(private$.scenarioConfigurationData$populationFromCSV)
      },
      #' @field modelParameters Model parameters ID used for the scenario
      modelParameters = function(value) {
        if (!missing(value)) {
          private$.scenarioConfigurationData$modelParameters <- value
        }
        return(private$.scenarioConfigurationData$modelParameters)
      },
      #' @field applications Application protocol ID used for the scenario
      applications = function(value) {
        if (!missing(value)) {
          private$.scenarioConfigurationData$applications <- value
        }
        return(private$.scenarioConfigurationData$applications)
      },
      #' @field simulationTime Simulation time
      simulationTime = function(value) {
        if (!missing(value)) {
          private$.scenarioConfigurationData$simulationTime <- value
        }
        return(private$.scenarioConfigurationData$simulationTime)
      },
      #' @field simulationTimeUnit Simulation time unit
      simulationTimeUnit = function(value) {
        if (!missing(value)) {
          private$.scenarioConfigurationData$simulationTimeUnit <- value
        }
        return(private$.scenarioConfigurationData$simulationTimeUnit)
      },
      #' @field steadyState Flag indicating if the simulation should run in steady state
      steadyState = function(value) {
        if (!missing(value)) {
          if (is.na(value)){
            value <- FALSE
          }
          private$.scenarioConfigurationData$steadyState <- value
        }
        return(private$.scenarioConfigurationData$steadyState)
      },
      #' @field steadyStateTime Steady state time
      steadyStateTime = function(value) {
        if (!missing(value)) {
          private$.scenarioConfigurationData$steadyStateTime <- value
        }
        return(private$.scenarioConfigurationData$steadyStateTime)
      },
      #' @field steadyStateTimeUnit Steady state time unit
      steadyStateTimeUnit = function(value) {
        if (!missing(value)) {
          private$.scenarioConfigurationData$steadyStateTimeUnit <- value
        }
        return(private$.scenarioConfigurationData$steadyStateTimeUnit)
      },
      #' @field model Model file
      model = function(value) {
        if (!missing(value)) {
          private$.scenarioConfigurationData$model <- value
        }
        return(private$.scenarioConfigurationData$model)
      },
      #' @field outputPaths Simulation output paths
      outputPaths = function(value) {
        if (!missing(value)) {
          private$.scenarioConfigurationData$outputPaths <- value
        }
        return(private$.scenarioConfigurationData$outputPaths)
      }
    )
  )



splitCellValues <- function(cellValues, separator = ","){
  if (is.null(cellValues) || is.na(cellValues)) {
    return(NA)
  }
  trimws(
    stringr::str_split_1(
      cellValues,
      separator
    )
  )
}

mergeCellValues <- function(cellValues, separator = ","){
  if (all(is.null(cellValues)) || all(is.na(cellValues))) {
    return(NA)
  }
  paste(cellValues, collapse = separator)
}

