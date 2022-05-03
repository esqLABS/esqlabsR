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
    #' @field setTestParameters Boolean representing whether parameters defined in `TestParameters` are to be applied
    #' to the simulation
    setTestParameters = function(value) {
      if (missing(value)) {
        private$.setTestParameters
      } else {
        validateIsLogical(value)
        private$.setTestParameters <- value
      }
    },
    #' @field simulateSteadyState Boolean representing whether the simulation will be brought to a steady-state first
    simulateSteadyState = function(value) {
      if (missing(value)) {
        private$.simulateSteadyState
      } else {
        validateIsLogical(value)
        private$.simulateSteadyState <- value
      }
    },
    #' @field simulationTime Boolean representing whether the simulation will be brought to a steady-state first
    simulationTime = function(value) {
      if (missing(value)) {
        private$.simulationTime
      } else {
        validateIsNumeric(value)
        if (value < 0) {
          stop(messages$valueShouldNotBeNegative("simulationTime", value))
        }
        private$.simulationTime <- value
      }
    },
    #' @field pointsPerMinute Resultion of the ouputs in points per minute
    pointsPerMinute = function(value) {
      if (missing(value)) {
        private$.pointsPerMinute
      } else {
        validateIsNumeric(value)
        if (value < 0) {
          stop(messages$valueShouldNotBeNegative("pointsPerMinute", value))
        }
        private$.pointsPerMinute <- value
      }
    },
    #' @field steadyStateTime Time in minutes to simulate if simulating steady-state. May be NULL
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
    #' @field paramSheets Names of the sheets from the parameters-excel file that will be applied to the simulation
    paramSheets = function(value) {
      if (missing(value)) {
        private$.paramSheets
      } else {
        stop("Field paramSheets is read-only! Use functions 'addParamSheet' and 'removeParamSheet' to add or remove
a parameter sheet from the list")
      }
    },
    #' @field simulationType Type of the simulation - "Individual" or "Population". If "Population", population characteristics
    #' are created based on information stored in `populationParamsFile`.
    #' Default is "Individual"
    simulationType = function(value) {
      if (missing(value)) {
        private$.simulationType
      } else {
        if (value %in% c("Individual", "Population")) {
          private$.simulationType <- value
        } else {
          stop("Wrong value for 'simulationType'! Accepted values are 'Individual and 'Population'")
        }
      }
    },
    #' @field simulationRunOptions Object of type `SimulationRunOptions` that will be passed
    #' to simulation runs. If `NULL`, default options are used
    simulationRunOptions = function(value) {
      if (missing(value)) {
        private$.simulationRunOptions
      } else {
        validateIsOfType(value, SimulationRunOptions, nullAllowed = TRUE)
        private$.simulationRunOptions <- value
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
    .setTestParameters = FALSE,
    .simulateSteadyState = FALSE,
    .simulationTime = NULL,
    .pointsPerMinute = 1,
    .steadyStateTime = 1000,
    .individualCharacteristics = NULL,
    .paramSheets = NULL,
    .simulationType = "Individual",
    .simulationRunOptions = NULL
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
    #' @field outputDevice Output target of the plot. If `NULL` (default), the figure is created in the default "plot"
    #' output. Other values indicate output into a file. A list of supported outputs is provided in `GraphicsDevices`-enum.
    outputDevice = NULL,
    #' @field individualId Id of the individual. If `NULL` (default), the individual as defined in the simulation file will be simulated.
    individualId = NULL,
    #' @description Add the names of sheets in the parameters excel-file
    #' that will be applied to the simulation
    #' @param sheetNames A name or a list of names of the excel sheet
    addParamSheets = function(sheetNames) {
      private$.paramSheets <- enumPut(sheetNames, sheetNames, enum = private$.paramSheets, overwrite = TRUE)
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
    #' @param ... Rest arguments.
    print = function(...) {
      self$projectConfiguration$print()
      private$printClass()
      private$printLine("Model file name", self$modelFile)
      private$printLine("Scenario name", self$scenarioName)
      private$printLine("Parameters sheets", enumKeys(self$paramSheets))
      private$printLine("Individual Id", self$individualId)
      private$printLine("Application protocol", self$applicationProtocol)
      private$printLine("Simulation time", self$simulationTime)
      private$printLine("Points per minute", self$pointsPerMinute)
      private$printLine("Output to PNG", self$outputToPNG)
      private$printLine("Simulate steady-state", self$simulateSteadyState)
      private$printLine("Steady-state time", self$steadyStateTime)
      private$printLine("Set test parameters", self$setTestParameters)
      invisible(self)
    }
  )
)
