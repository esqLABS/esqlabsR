#' @title PITaskConfiguration
#' @docType class
#' @description An object storing configuration for a parameter identification
#'   (PI) task. This class holds references to PI task settings defined in
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
    },

    #' @field taskName Name of the PI task. Key for lookup in
    #'   `ParameterIdentification.xlsx`. Read-only.
    taskName = function(value) {
      if (missing(value)) {
        private$.taskName
      } else {
        stop(messages$errorPropertyReadOnly("taskName"))
      }
    },

    #' @field scenarioConfiguration Named list of `ScenarioConfiguration`
    #'   objects for the PI task. Read-only.
    scenarioConfiguration = function(value) {
      if (missing(value)) {
        private$.scenarioConfiguration
      } else {
        stop(messages$errorPropertyReadOnly("scenarioConfiguration"))
      }
    },

    #' @field piConfiguration Named list of PI settings: algorithm, ciMethod,
    #'   printEvaluationFeedback, autoEstimateCI, simulationRunOptions,
    #'   objectiveFunctionOptions, algorithmOptions, ciOptions. Read-only.
    piConfiguration = function(value) {
      if (missing(value)) {
        private$.piConfiguration
      } else {
        stop(messages$errorPropertyReadOnly("piConfiguration"))
      }
    },

    #' @field piParameters Named list of parameter configurations from
    #'   PIParameters sheet. Read-only.
    piParameters = function(value) {
      if (missing(value)) {
        private$.piConfiguration
      } else {
        stop(messages$errorPropertyReadOnly("piParameters"))
      }
    },

    #' @field piOutputMappings Named list of output mapping configurations from
    #'   PIOutputMappings sheet. Read-only.
    piOutputMappings = function(value) {
      if (missing(value)) {
        private$.piOutputMappings
      } else {
        stop(messages$errorPropertyReadOnly("piOutputMappings"))
      }
    }
  ),

  private = list(
    .projectConfiguration = NULL,
    .taskName = NULL,
    .scenarioConfiguration = NULL,
    .piConfiguration = NULL,
    .piParameters = NULL,
    .piOutputMappings = NULL
  ),

  public = list(
    #' @description Initialize a new instance of the class
    #' @param taskName Character. Name of the PI task (key for lookup in Excel).
    #' @param projectConfiguration An object of class `ProjectConfiguration`.
    #' @param scenarioConfiguration An object of class `ScenarioConfiguration`
    #'   or a named list of `ScenarioConfiguration` objects.
    #' @param piDefinitions Named list containing:
    #'   - `piConfiguration`: Named list of PI settings
    #'   - `piParameters`: Named list of PI parameter configurations
    #'   - `piOutputMappings`: Named list of PI output mapping configurations
    #' @returns A new `PITaskConfiguration` object.
    initialize = function(
      taskName,
      projectConfiguration,
      scenarioConfiguration,
      piDefinitions = NULL
    ) {
      # Validate required parameters
      validateIsString(taskName)
      validateIsOfType(projectConfiguration, ProjectConfiguration)
      validateIsOfType(scenarioConfiguration, ScenarioConfiguration)
      .validateIsNamedList(piDefinitions, nullAllowed = TRUE)

      private$.taskName <- taskName
      private$.projectConfiguration <- projectConfiguration
      private$.scenarioConfiguration <- toList(scenarioConfiguration)

      private$.piConfiguration <- piDefinitions$piConfiguration
      private$.piParameters <- piDefinitions$piParameters
      private$.piOutputMappings <- piDefinitions$piOutputMappings
    },

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
          "Algorithm" = self$piConfiguration$Algorithm,
          "CI Method" = self$piConfiguration$CIMethod,
          "Parameter Count" = length(self$piParameters),
          "Output Mapping Count" = length(self$piOutputMappings)
        ),
        print_empty = TRUE
      )
    }
  )
)
