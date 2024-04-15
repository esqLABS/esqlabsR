#' @title ScenarioConfiguration
#' @docType class
#' @description An object storing configuration of a specific scenario
#' @format NULL
#' @export
ScenarioConfiguration <- R6::R6Class(
  "ScenarioConfiguration",
  inherit = ospsuite.utils::Printable,
  cloneable = TRUE,
  active = list(
    #' @field simulateSteadyState Boolean representing whether the simulation
    #' will be brought to a steady-state first
    simulateSteadyState = function(value) {
      if (missing(value)) {
        private$.simulateSteadyState
      } else {
        validateIsLogical(value)
        # If the value is `NA`, do not change
        if (is.na(value)) {
          invisible()
        }
        private$.simulateSteadyState <- value
      }
    },
    #' @field readPopulationFromCSV Boolean representing whether the a new population
    #' will be created (value is `FALSE`) or an existing population will be imported
    #' from a csv.
    readPopulationFromCSV = function(value) {
      if (missing(value)) {
        private$.readPopulationFromCsv
      } else {
        validateIsLogical(value)
        # If the value is `NA`, do not change
        if (is.na(value)) {
          invisible()
        }
        private$.readPopulationFromCsv <- value
      }
    },
    #' @field simulationTime Specified simulation time intervals. If `NULL` (default),
    #' simulation time as defined in the `Simulation` object will be used.
    #' Accepted are multiple time intervals separated by a ';'. Each time interval
    #' is a triplet of values <StartTime, EndTime, Resolution>, where `Resolution` is the number of
    #' simulated points per time unit defined in the column `TimeUnit`.
    simulationTime = function(value) {
      if (missing(value)) {
        private$.simulationTime
      } else {
        private$.simulationTime <- .parseSimulationTimeIntervals(value)
      }
    },

    #' @field simulationTimeUnit Unit of the simulation time intervals.
    simulationTimeUnit = function(value) {
      if (missing(value)) {
        private$.simulationTimeUnit
      } else {
        validateUnit(value, ospDimensions$Time)
        private$.simulationTimeUnit <- value
      }
    },

    #' @field steadyStateTime Time in minutes to simulate if simulating steady-state. May be `NULL`.
    steadyStateTime = function(value) {
      if (missing(value)) {
        private$.steadyStateTime
      } else {
        validateIsNumeric(value)
        if (value < 0) {
          stop(messages$valueShouldNotBeNegative("steadyStateTime", value))
        }
        private$.steadyStateTime <- value
      }
    },
    #' @field paramSheets Names of the sheets from the parameters-excel file
    #'   that will be applied to the simulation
    paramSheets = function(value) {
      if (missing(value)) {
        private$.paramSheets
      } else {
        stop(paste0(
          messages$errorPropertyReadOnly("paramSheets"),
          ". Use functions 'addParamSheet' and 'removeParamSheet' to add or remove
a parameter sheet from the list"
        ))
      }
    },
    #' @field simulationType Type of the simulation - "Individual" or
    #'   "Population". If "Population", population characteristics are created
    #'   based on information stored in `populationsFile`. Default is
    #'   "Individual"
    simulationType = function(value) {
      if (missing(value)) {
        private$.simulationType
      } else {
        if (any(c("Individual", "Population") == value)) {
          private$.simulationType <- value
        } else {
          stop(messages$wrongSimulationType())
        }
      }
    },
    #' @field projectConfiguration `ProjectConfiguration` that will be used in scenarios.
    #' Read-only
    projectConfiguration = function(value) {
      if (missing(value)) {
        private$.projectConfiguration
      } else {
        stop(messages$errorPropertyReadOnly("projectConfiguration"))
      }
    }
  ),
  private = list(
    .projectConfiguration = NULL,
    .simulateSteadyState = FALSE,
    .simulationTime = NULL,
    .simulationTimeUnit = ospUnits$Time$min,
    .steadyStateTime = 1000,
    .individualCharacteristics = NULL,
    .paramSheets = NULL,
    .simulationType = "Individual",
    .readPopulationFromCsv = FALSE
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param projectConfiguration An object of class `ProjectConfiguration`.
    #' @return A new `ScenarioConfiguration` object.
    initialize = function(projectConfiguration) {
      private$.projectConfiguration <- projectConfiguration
      private$.paramSheets <- enum(NULL)
    },

    #' @field scenarioName Name of the simulated scenario
    scenarioName = NULL,
    #' @field modelFile Name of the simulation to be loaded (must include the
    #' extension ".pkml"). Must be located in the "modelFolder".
    modelFile = NULL,
    #' @field applicationProtocol Name of the application protocol to be applied. Defined
    #' in the excel file "ApplicationParameters.xlsx"
    applicationProtocol = NULL,
    #' @field individualId Id of the individual as specified in "IndividualParameters.xlsx".
    #' If `NULL` (default), the individual as defined in the simulation file will be simulated.
    individualId = NULL,
    #' @field populationId Id of the population as specified in "Populations.xlsx",
    #' sheet "Demographics". If `ScenarioConfguration$simulationType` is `population`,
    #' a population will be created a the scenario will be simulated as a population
    #' simulation.
    populationId = NULL,
    #' @field outputPaths a list of output paths for which the results will be
    #' calculated. If `NULL` (default), outputs as defined in the simulation
    #' are used.
    outputPaths = NULL,
    #' @description Add the names of sheets in the parameters excel-file
    #' that will be applied to the simulation
    #' @param sheetNames A name or a list of names of the excel sheet
    addParamSheets = function(sheetNames) {
      private$.paramSheets <- enumPut(sheetNames,
        sheetNames,
        enum = private$.paramSheets,
        overwrite = TRUE
      )
    },
    #' @description Remove the names of sheets in the parameters excel-file
    #' from the list of sheets `paramSheets`
    #' @param sheetNames A name or a list of names of the excel sheet.
    #' If `NULL` (default), all sheets are removed
    removeParamSheets = function(sheetNames = NULL) {
      if (is.null(sheetNames)) {
        private$.paramSheets <- enum(NULL)
      } else {
        private$.paramSheets <- enumRemove(keys = sheetNames, enum = private$.paramSheets)
      }
    },
    #' @description
    #' Print the object to the console
    #' @param projectConfiguration Whether to also print project configuration. default to TRUE.
    print = function(projectConfiguration = TRUE) {
      if (projectConfiguration) {
        self$projectConfiguration$print()
      }
      private$printClass()
      private$printLine("Model file name", self$modelFile)
      private$printLine("Scenario name", self$scenarioName)
      private$printLine("Parameters sheets", enumKeys(self$paramSheets))
      private$printLine("Individual Id", self$individualId)
      private$printLine("Population Id", self$populationId)
      private$printLine("Read population from csv file", self$readPopulationFromCSV)
      private$printLine("Application protocol", self$applicationProtocol)
      private$printLine("Simulation time intervals")
      for (i in seq_along(self$simulationTime)) {
        private$printLine("  Interval", i)
        private$printLine("    Start", self$simulationTime[[i]][1])
        private$printLine("    End", self$simulationTime[[i]][2])
        private$printLine("    Resolution", self$simulationTime[[i]][3])
      }
      private$printLine("Simulation time intervals unit", self$simulationTimeUnit)
      private$printLine("Simulate steady-state", self$simulateSteadyState)
      private$printLine("Steady-state time", self$steadyStateTime)
      invisible(self)
    }
  )
)
