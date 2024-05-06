ScenarioConfiguration2 <-
  R6::R6Class(
    classname = "ScenarioConfiguration2",
    public = list(
      initialize = function(project, scenarioConfigurationData) {
        private$.project <- project
        private$.scenarioConfigurationData$id <- scenarioConfigurationData$Scenario_name
        self$individual <- scenarioConfigurationData$IndividualId
        self$population <- scenarioConfigurationData$PopulationId
        self$populationFromCSV <- scenarioConfigurationData$ReadPopulationFromCSV
        self$modelParameters <- scenarioConfigurationData$ModelParameterSheets
        self$applications <- scenarioConfigurationData$ApplicationProtocol
        self$time <- scenarioConfigurationData$SimulationTime
        self$timeUnit <- scenarioConfigurationData$SimulationTimeUnit
        self$steadyState <- scenarioConfigurationData$SteadyState
        self$steadyStateTime <- scenarioConfigurationData$SteadyStateTime
        self$steadyStateTimeUnit <- scenarioConfigurationData$SteadyStateTimeUnit
        self$model <- scenarioConfigurationData$ModelFile
        self$outputPaths <- scenarioConfigurationData$OutputPathsIds
      },
      print = function() {
        print(private$.scenarioConfigurationData)
      },
      toDataFrame = function(){
        return(
          tibble::tibble(
            Scenario_name = list$id,
            IndividualId = list$individual,
            PopulationId = list$population,
            ReadPopulationFromCSV = list$populationFromCSV,
            ModelParameterSheets = list$modelParameters,
            ApplicationProtocol = list$applications,
            SimulationTime = list$time,
            SimulationTimeUnit = list$timeUnit,
            SteadyState = list$steadyState,
            SteadyStateTime = list$steadyStateTime,
            SteadyStateTimeUnit = list$steadyStateTimeUnit,
            ModelFile = list$model,
            OutputPathsIds = list$outputPaths
          )
        )
      }
    ),
    private = list(
      .project = NULL,
      .scenarioConfigurationData = list()
    ),
    active = list(
      id = function(value) {
        if (!missing(value)) {
          stop("id is read-only.")
        }
        return(private$.scenarioConfigurationData$id)
      },
      individual = function(value) {
        if (!missing(value)) {
          private$.scenarioConfigurationData$individual <- value
        }
        return(private$.scenarioConfigurationData$individual)
      },
      population = function(value) {
        if (!missing(value)) {
          private$.scenarioConfigurationData$population <- value
        }
        return(private$.scenarioConfigurationData$population)
      },
      populationFromCSV = function(value) {
        if (!missing(value)) {
          private$.scenarioConfigurationData$populationFromCSV <- value
        }
        return(private$.scenarioConfigurationData$populationFromCSV
        )
      },
      modelParameters = function(value) {
        if (!missing(value)) {
          private$.scenarioConfigurationData$modelParameters <- value
        }
        return(private$.scenarioConfigurationData$modelParameters)
      },
      applications = function(value) {
        if (!missing(value)) {
          private$.scenarioConfigurationData$applications <- value
        }
        return(private$.scenarioConfigurationData$applications)
      },
      time = function(value) {
        if (!missing(value)) {
          private$.scenarioConfigurationData$time <- value
        }
        return(private$.scenarioConfigurationData$time)
      },
      timeUnit = function(value) {
        if (!missing(value)) {
          private$.scenarioConfigurationData$timeUnit <- value
        }
        return(private$.scenarioConfigurationData$timeUnit)
      },
      steadyState = function(value) {
        if (!missing(value)) {
          if (is.na(value)){
            value <- FALSE
          }
          private$.scenarioConfigurationData$steadyState <- value
        }
        return(private$.scenarioConfigurationData$steadyState)
      },
      steadyStateTime = function(value) {
        if (!missing(value)) {
          private$.scenarioConfigurationData$steadyStateTime <- value
        }
        return(private$.scenarioConfigurationData$steadyStateTime)
      },
      steadyStateTimeUnit = function(value) {
        if (!missing(value)) {
          private$.scenarioConfigurationData$steadyStateTimeUnit <- value
        }
        return(private$.scenarioConfigurationData$steadyStateTimeUnit)
      },
      model = function(value) {
        if (!missing(value)) {
          private$.scenarioConfigurationData$model <- value
        }
        return(private$.scenarioConfigurationData$model)
      },
      outputPaths = function(value) {
        if (!missing(value)) {
          private$.scenarioConfigurationData$outputPaths <- value
        }
        return(private$.scenarioConfigurationData$outputPaths)
      }
    )
  )


ScenarioConfigurationDataFrameToList <- function(dataframe) {
  return(
    list(
      id = dataframe$Scenario_name,
      individual = dataframe$IndividualId,
      population = dataframe$PopulationId,
      populationFromCSV = dataframe$ReadPopulationFromCSV,
      modelParameters = splitCellValues(dataframe$ModelParameterSheets),
      applications = splitCellValues(dataframe$ApplicationProtocol),
      time = splitCellValues(dataframe$SimulationTime, ";"),
      timeUnit = dataframe$SimulationTimeUnit,
      steadyState = dataframe$SteadyState,
      steadyStateTime = dataframe$SteadyStateTime,
      steadyStateTimeUnit = dataframe$SteadyStateTimeUnit,
      model = dataframe$ModelFile,
      outputPaths = splitCellValues(dataframe$OutputPathsIds)
    )
  )
}

splitCellValues <- function(cellValues, separator = ","){
  if (is.null(cellValues) || is.na(cellValues)) {
    return(NULL)
  }
  trimws(
    stringr::str_split_1(
      cellValues,
      separator
    )
  )
}
