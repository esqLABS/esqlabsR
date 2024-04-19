Project <-
  R6::R6Class(
    classname = "Project",
    public = list(
      projectConfiguration = NULL,
      #' Initialize
      #'
      #' @param projectConfiguration A projectConfiguration object
      initialize = function(projectConfiguration) {
        self$projectConfiguration <- projectConfiguration
      }
    ),
    active = list(
      scenarios = function(scenarios = NULL) {
        scenarioConfigurations <- readScenarioConfigurationFromExcel(
          scenarioNames = scenarios,
          projectConfiguration = self$projectConfiguration
        )
        if (!is.null(private$.scenarioConfigurations) && scenarioConfigurations != private$.scenarioConfigurations) {
          private$.scenarioConfigurations <- scenarioConfigurations
          private$.scenarios <- NULL
        }
        if (is.null(private$.scenarios)) {
          private$.scenarios <- createScenarios(scenarioConfigurations)
        }
        return(private$.scenarios)
      }
    ),
    private = list(
      .scenarioConfigurations = NULL,
      .scenarios = NULL
    )
  )
