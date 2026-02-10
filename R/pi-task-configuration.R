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
        if (is.null(private$.piConfiguration)) {
          return(NULL)
        }

        structure(
          private$.piConfiguration,
          class = c("piConfiguration_view", "list")
        )
      } else {
        stop(messages$errorPropertyReadOnly("piConfiguration"))
      }
    },

    #' @field piParameters Named list of parameter configurations from
    #'   PIParameters sheet. Read-only.
    piParameters = function(value) {
      if (missing(value)) {
        if (is.null(private$.piParameters)) {
          return(NULL)
        }

        structure(
          private$.piParameters,
          class = c("piParameters_view", "list")
        )
      } else {
        stop(messages$errorPropertyReadOnly("piParameters"))
      }
    },

    #' @field piOutputMappings Named list of output mapping configurations from
    #'   PIOutputMappings sheet. Read-only.
    piOutputMappings = function(value) {
      if (missing(value)) {
        if (is.null(private$.piOutputMappings)) {
          return(NULL)
        }

        structure(
          private$.piOutputMappings,
          class = c("piOutputMappings_view", "list")
        )
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
    #' @param scenarioConfiguration Whether to also print scenario
    #'   configurations. Default is FALSE.
    print = function(
      className = TRUE,
      projectConfiguration = FALSE,
      scenarioConfiguration = FALSE
    ) {
      if (className) {
        ospsuite.utils::ospPrintClass(self)
      }

      if (projectConfiguration) {
        ospsuite.utils::ospPrintHeader("Project configuration", level = 1)
        self$projectConfiguration$print(className = FALSE)
      }

      if (scenarioConfiguration) {
        ospsuite.utils::ospPrintHeader("Scenario configurations", level = 1)
        for (scenarioName in names(self$scenarioConfiguration)) {
          self$scenarioConfiguration[[scenarioName]]$print(
            className = FALSE,
            projectConfiguration = FALSE
          )
        }
      }

      # Main section
      ospsuite.utils::ospPrintHeader("PI Task Configuration", level = 1)

      # Extract scenario names and model files
      scenarioNames <- names(self$scenarioConfiguration)
      modelFiles <- sapply(self$scenarioConfiguration, function(x) {
        x$modelFile
      })

      ospsuite.utils::ospPrintItems(
        list(
          "Task Name" = self$taskName,
          "Scenario(s)" = paste(scenarioNames, collapse = " | "),
          "Model File(s)" = paste(modelFiles, collapse = " | "),
          "Algorithm" = self$piConfiguration$Algorithm,
          "CI Method" = self$piConfiguration$CIMethod,
          "Number of Parameters" = length(self$piParameters),
          "Number of Output Mappings" = length(self$piOutputMappings)
        ),
        print_empty = TRUE
      )

      invisible(self)
    }
  )
)

#' @method print piParameters_view
#' @rawNamespace S3method(print, piParameters_view)
print.piParameters_view <- function(x, ...) {
  ospsuite.utils::ospPrintHeader("PI Parameters", level = 2)

  displayFields <- c(
    "Container Path",
    "Parameter Name",
    "Value",
    "Units",
    "MinValue",
    "MaxValue",
    "StartValue"
  )

  # Iterate over each parameter (list of lists)
  for (i in seq_along(x)) {
    paramItems <- x[[i]][intersect(displayFields, names(x[[i]]))]

    if (length(paramItems) > 0) {
      title <- if (length(x) > 1) paste("Parameter", i) else NULL
      ospsuite.utils::ospPrintItems(paramItems, print_empty = TRUE, title = title)
    }
  }

  invisible(x)
}

#' @method print piOutputMappings_view
#' @rawNamespace S3method(print, piOutputMappings_view)
print.piOutputMappings_view <- function(x, ...) {
  ospsuite.utils::ospPrintHeader("PI Output Mappings", level = 2)

  displayFields <- c(
    "ObservedDataSheet",
    "DataSet",
    "Scaling"
  )

  # Iterate over each output mapping (list of lists)
  for (i in seq_along(x)) {
    paramItems <- x[[i]][intersect(displayFields, names(x[[i]]))]

    if (length(paramItems) > 0) {
      title <- if (length(x) > 1) paste("Output Mapping", i) else NULL
      ospsuite.utils::ospPrintItems(paramItems, print_empty = TRUE, title = title)
    }
  }

  invisible(x)
}

#' @method print piConfiguration_view
#' @rawNamespace S3method(print, piConfiguration_view)
print.piConfiguration_view <- function(x, ...) {
  ospsuite.utils::ospPrintHeader("PI Configuration", level = 2)

  displayFields <- c(
    "Algorithm",
    "CIMethod",
    "PrintEvaluationFeedback",
    "AutoEstimateCI",
    "SimulationRunOptions",
    "ObjectiveFunctionOptions"
  )
  paramItems <- x[intersect(displayFields, names(x))]
  paramItems <- purrr::discard(
    paramItems,
    ~ is.null(.x) || (length(.x) == 1 && is.na(.x))
  )

  if (length(paramItems) > 0) {
    ospsuite.utils::ospPrintItems(paramItems, print_empty = TRUE)
  }

  algorithmOptions <- purrr::pluck(x, "algorithmOptions")
  ciOptions <- purrr::pluck(x, "ciOptions")

  if (length(algorithmOptions) > 0) {
    ospsuite.utils::ospPrintHeader("Algorithm Options", level = 3)
    ospsuite.utils::ospPrintItems(algorithmOptions, print_empty = TRUE)
  }

  if (length(ciOptions) > 0) {
    ospsuite.utils::ospPrintHeader("CI Options", level = 3)
    ospsuite.utils::ospPrintItems(ciOptions, print_empty = TRUE)
  }

  invisible(x)
}
