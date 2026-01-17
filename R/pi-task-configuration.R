#' @title PITaskConfiguration
#' @docType class
#' @description An object storing configuration for a parameter identification
#'   task. This class holds references to PI task settings defined in
#'   `ParameterIdentification.xlsx`.
#' @format NULL
#' @export
PITaskConfiguration <- R6::R6Class(
  "PITaskConfiguration",
  cloneable = TRUE,
  active = list(
    #' @field projectConfiguration `ProjectConfiguration` that will be used.
    #'   Read-only.
    projectConfiguration = function(value) {
      if (missing(value)) {
        private$.projectConfiguration
      } else {
        stop(messages$errorPropertyReadOnly("projectConfiguration"))
      }
    }
  ),

  private = list(
    .projectConfiguration = NULL
  ),

  public = list(
    #' @description Initialize a new instance of the class
    #' @param projectConfiguration An object of class `ProjectConfiguration`.
    #' @returns A new `PITaskConfiguration` object.
    initialize = function(projectConfiguration) {
      validateIsOfType(projectConfiguration, ProjectConfiguration)
      private$.projectConfiguration <- projectConfiguration
    },
    #' @field piTaskName Name of the PI task. Key for lookup in
    #'   `ParameterIdentification.xlsx`.
    piTaskName = NULL,

    #' @field scenarioName Name of the simulated scenario (from Scenarios.xlsx).
    scenarioName = NULL,

    #' @field modelFile Name of the simulation file (from Scenarios.xlsx).
    modelFile = NULL,

    #' @field piConfiguration Named list of PI settings: algorithm, ciMethod,
    #'   printEvaluationFeedback, autoEstimateCI, simulationRunOptions,
    #'   objectiveFunctionOptions, algorithmOptions, ciOptions.
    piConfiguration = NULL,

    #' @field piParameters Named list of parameter configurations from
    #'   PIParameters sheet.
    piParameters = NULL,

    #' @field piOutputMappings Named list of output mapping configurations from
    #'   PIOutputMappings sheet.
    piOutputMappings = NULL,

    #' @description Print the object to the console
    #' @param className Whether to print the name of the class at the beginning.
    #'   Default is TRUE.
    #' @param projectConfiguration Whether to also print project configuration.
    #'   Default is FALSE.
    print = function(className = TRUE, projectConfiguration = FALSE) {
      if (className) {
        ospsuite.utils::ospPrintClass(self)
      }

      if (projectConfiguration) {
        ospsuite.utils::ospPrintHeader("Project configuration", level = 1)
        self$projectConfiguration$print(className = FALSE)
      }

      ospsuite.utils::ospPrintHeader("PI Task Configuration", level = 1)
      ospsuite.utils::ospPrintItems(
        list(
          "PI Task Name" = self$piTaskName,
          "Scenario Name" = self$scenarioName,
          "Model File" = self$modelFile,
          "Algorithm" = self$piConfiguration$algorithm,
          "CI Method" = self$piConfiguration$ciMethod,
          "Parameter Count" = length(self$piParameters),
          "Output Mapping Count" = length(self$piOutputMappings)
        ),
        print_empty = TRUE
      )
    }
  )
)
