#' @title Project Object
#'
#' @description
#' Project Class that contains all workflow component of an {ESQlabsR} project:
#' - Project Configuration
#' - Configurations
#' - Scenarios
#' - Simulation Results
Project <-
  R6::R6Class(
    classname = "Project",
    public = list(
      #' @field projectConfiguration Representation of the project configurations
      #' as defined in the `ProjectConfiguration.xlsx` file.
      projectConfiguration = NULL,
      #' @description Creates a new `Project` object.
      #'
      #' @param projectConfiguration A ProjectConfiguration object created by
      #' `createProjectConfiguration()`
      initialize = function(projectConfiguration) {
        self$projectConfiguration <- projectConfiguration
        private$.availableScenarios <- private$.getAvailableScenarios()
        private$.initializeScenarios()
      },
      #' @description Print the project object
      print = function() {
        cli_ul()
        print(self$projectConfiguration)
        self$configurations$print(lod = 1)
        cli_h1("Scenarios")
        purrr::map(self$scenarios, ~ print(.x, lod = 1))
        cli_h1("Simulation Results")
        cli_ul()
        purrr::map(names(self$simulationResults), ~ cli_li(.x))
      },
      #' @description Reset the loaded configurations by reading the
      #' Configurations files.
      reloadConfigurations = function() {
        private$.configurations <- NULL
      },
      #' @description Activate only some of the available scenarios.
      #' Only activated scenarios will be loaded and run.
      #'
      #' @param scenarios A character vector of scenario names to activate.
      #' If `NULL`, will activate all scenarios.
      #'
      #' @examples
      #' project <- exampleProject()
      #' project$selectScenarios(c("TestScenario", "TestScenario2"))
      selectScenarios = function(scenarios = NULL) {
        if (is.null(scenarios)) {
          for (scenario in self$scenarios) {
            scenarios$status <- "active"
          }
        } else {
          rlang::arg_match(arg = scenarios, values = private$.availableScenarios, multiple = TRUE)
          for (scenario in names(self$scenarios)) {
            if (scenario %in% scenarios) {
              self$scenarios[[scenario]]$status <- "active"
            } else {
              self$scenarios[[scenario]]$status <- "inactive"
            }
          }
        }
        invisible(self)
      },
      #' @description Load all active scenarios.
      #'
      #' @examples
      #' project <- exampleProject()
      #' project$loadScenarios()
      loadScenarios = function() {
        purrr::map(self$scenarios, ~ .x$load(), .progress = "Loading Scenarios")
        invisible(self)
      },
      #' @description run all active scenarios.
      #'
      #' @note If the active scenarios are not already loaded, will load them
      #' and then run the simulations.
      #' @examples
      #' \dontrun{
      #' project <- exampleProject()
      #' project$runScenarios()
      #' }
      runScenarios = function() {
        self$loadScenarios()

        # steadyStateScenarios <-
        #   purrr::keep(self$scenarios, ~ .x$status == "loaded" && .x$steadyState) %>%
        #   purrr::map(~ list(simulation = .x$simulation, steadyState = .x$steadyStateTime)))


        individualScenarios <-
          purrr::keep(self$scenarios, ~ .x$status == "loaded" && .x$type == "individual") %>%
          purrr::map(~ .x$simulation)

        self$simulationResults <- ospsuite::runSimulations(individualScenarios)

        populationScenarios <-
          purrr::keep(self$scenarios, ~ .x$status == "loaded" && .x$type == "population") %>%
          purrr::map(~ list(simulation = .x$simulation, population = .x$population))

        for (populationScenario in populationScenarios) {
          self$simulationResults[[populationScenarios$name]] <- ospsuite::runSimulations(populationScenario$simulation,
            population = populationScenario$population
          )
        }

        invisible(self)
      }
    ),
    active = list(
      #' @field configurations Configurations as defined in the Configurations
      #' Excel files.
      configurations = function(value) {
        if (is.null(private$.configurations)) {
          private$.initializeConfigurations()
        }

        if (!missing(value)) {
          private$.configurations <- value
          private$.newConfigurations <- TRUE
        }

        return(private$.configurations)
      },
      #' @field scenarios Scenarios as defined in the Scenarios Excel file.
      scenarios = function(value) {
        if (identical(private$.scenarios, list()) || private$.newConfigurations) {
          private$.initializeScenarios()
          private$.newConfigurations <- FALSE
        }

        if (!missing(value)) {
          private$.scenarios <- modifyList(private$.scenarios, value)
        }
        invisible(private$.scenarios)
      },
      #' @field simulationResults Simulation results from the run simulations.
      simulationResults = function() {
        if (!missing(value)) {
          cli::cli_abort("Simulation Results cannot be altered.")
        }

        if (identical(private$.simulationResults, list())) {
          message("No Simulation Results available.")
        } else {
          return(private$.simulationResults)
        }
      }
    ),
    private = list(
      .configurations = NULL,
      .newConfigurations = FALSE,
      .availableScenarios = NULL,
      .scenarios = list(),
      .simulationResults = list(),
      .getAvailableScenarios = function() {
        readExcel(
          path = self$projectConfiguration$scenariosFile,
          sheet = "Scenarios"
        )$Scenario_name
      },
      .initializeConfigurations = function() {
        private$.configurations <- Configuration$new(self)
      },
      .initializeScenarios = function() {
        if (is.null(private$.scenarios)) {
          private$.scenarios <- list()
        }
        for (scenario in private$.availableScenarios) {
          private$.scenarios[[scenario]] <- Scenario$new(
            project = self,
            scenarioConfigurationData = self$configurations$scenarios[[scenario]],
            status = if (!is.null(private$.scenarios[[scenario]]$status) && private$.scenarios[[scenario]]$status == "inactive") "inactive" else NULL
          )
        }
      }
    )
  )
