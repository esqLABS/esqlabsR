#' @title WarningManager
#' @docType class
#' @description An object that stores warnings with scenario names, warning codes, and messages.
#'
#' @format NULL
#' @export
WarningManager <- R6::R6Class(
  "WarningManager",

  public = list(
    warnings = list(),

    #' @description Initialize the WarningManager
    initialize = function() {
      self$warnings <- list()
    },

    #' @description Add a warning to the manager.
    #' @param scenario_name Name of the scenario where the warning originated.
    #' @param code Warning code.
    #' @param message Warning message.
    add_warning = function(scenario_name, code, message) {
      if (is.null(self$warnings[[scenario_name]])) {
        self$warnings[[scenario_name]] <- list()
      }
      self$warnings[[scenario_name]][[code]] <- message
    },

    #' @description Remove a warning from the manager.
    #' @param scenario_name Name of the scenario from which the warning should be removed.
    #' @param code Warning code to be removed.
    remove_warning = function(scenario_name, code) {
      if (!is.null(self$warnings[[scenario_name]]) && !is.null(self$warnings[[scenario_name]][[code]])) {
        self$warnings[[scenario_name]][[code]] <- NULL
        # Remove scenario entry if no warnings left
        if (length(self$warnings[[scenario_name]]) == 0) {
          self$warnings[[scenario_name]] <- NULL
        }
      }
    },

    #' @description Retrieve all warnings.
    #' @return A list of warnings.
    get_warnings = function() {
      return(self$warnings)
    }
  ),
  private = list(
    .warningManager = NULL  # Field for WarningManager
  )

)



ScenarioValidator <- R6::R6Class(
  "ScenarioValidator",

  public = list(
    configurations = NULL,
    warning_manager = NULL,

    #' @description Initialize the validator with project configurations and a WarningManager.
    #' @param configurations A list of configurations to be validated.
    #' @param warning_manager An instance of WarningManager.
    initialize = function(configurations, warning_manager) {
      self$configurations <- configurations
      self$warning_manager <- warning_manager
    },

    #' @description Validate a specific scenario's individual.
    #' This method will check if the individual referenced in a scenario exists.
    #' @param scenario_name The name of the scenario to validate.
    validate_scenario = function(scenario_name) {
      scenarios <- self$configurations$scenarios
      individuals <- self$configurations$individuals |> names()
      individual <- scenarios[[scenario_name]]$individual

      # Check if individual exists in the individuals file
      if (!(individual %in% individuals)) {
        self$warning_manager$add_warning(
          scenario_name,
          "INDIVIDUAL_NOT_FOUND",
          paste0("The individual '", individual, "' does not exist in the individuals configuration for scenario '", scenario_name, "'")
        )
      } else {
        # Remove warning if individual is now valid
        self$warning_manager$remove_warning(scenario_name, "INDIVIDUAL_NOT_FOUND")
      }
    },

    validate_scenarios = function() {
      scenario_names <- names(self$configurations$scenarios)
      purrr::walk(scenario_names, ~ self$validate_scenario(.x))
    },
    #' @description Retrieve all warnings.
    #' @return A list of warnings.
    get_warnings = function() {
      return(self$warning_manager$get_warnings())
    }
  )
)



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
        private$.warningManager <- WarningManager$new()  # Initialize WarningManager

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
        if (identical(private$.simulationResults, list())) {
          cli_alert_danger("No Simulation Results available.")
        } else {
          purrr::map(names(self$simulationResults), ~ cli_li(.x))
        }
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
        purrr::map(self$scenarios, ~ .x$load(),
          .progress = "Loading Scenarios"
        )
        invisible(self)
      },
      #' @description run all active scenarios.
      #'
      #' @param simulationRunOptions Object of type `SimulationRunOptions` that will be passed
      #' to simulation runs. If `NULL`, default options are used. description
      #'
      #' @note If the active scenarios are not already loaded, will load them
      #' and then run the simulations.
      #' @examples
      #' \dontrun{
      #' project <- exampleProject()
      #' project$runScenarios()
      #' }
      runScenarios = function(simulationRunOptions = NULL) {
        self$loadScenarios()

        steadyStateScenarios <-
          purrr::keep(self$scenarios, ~ .x$status == "loaded" && .x$simulateSteadyState) %>%
          purrr::map(~ list(simulation = .x$simulation, time = .x$steadyStateTime$timeBaseUnit))

        # Simulate steady-state concurrently
        if (length(steadyStateScenarios) > 0) {
          initialValues <- ospsuite::getSteadyState(
            simulations = purrr::map(steadyStateScenarios, "simulation"),
            steadyStateTime = purrr::map(steadyStateScenarios, ~ .x$time),
            ignoreIfFormula = TRUE,
            simulationRunOptions = simulationRunOptions
          )
        }

        # Set initial values for steady-state simulations
        purrr::imap(
          steadyStateScenarios,
          ~ ospsuite::setQuantityValuesByPath(
            quantityPaths = initialValues[[.y]]$paths,
            values = initialValues[[.y]]$values,
            simulation = .x$simulation
          )
        )


        individualScenarios <-
          purrr::keep(self$scenarios, ~ .x$status == "loaded" && .x$type == "individual") %>%
          purrr::map(~ .x$simulation)


        individualSimulationResults <- ospsuite::runSimulations(individualScenarios,
          simulationRunOptions = simulationRunOptions
        )

        populationScenarios <-
          purrr::keep(self$scenarios, ~ .x$status == "loaded" && .x$type == "population") %>%
          purrr::map(~ list(simulation = .x$simulation, population = .x$population$populationObject))

        populationSimulationResults <-
          purrr::map(
            populationScenarios,
            ~ ospsuite::runSimulations(.x$simulation,
              population = .x$population,
              simulationRunOptions = simulationRunOptions
            )[[1]]
          )

        allSimulationResults <- c(individualSimulationResults, populationSimulationResults)

        private$.simulationResults <- allSimulationResults[names(self$activeScenarios)]

        invisible(self)
      },
      #' @description Validate the project configuration.
      #' This method will check if all references in scenarios are valid and print warnings if any.
      validate = function() {
        validator <- ScenarioValidator$new(self$configurations, private$.warningManager)  # Use private$.warningManager
        validator$validate_scenarios()
        warnings <- private$.warningManager$get_warnings()


        if (length(warnings) > 0) {
          cli::cli_alert_info("Warnings:")
          cli::cli_ul()
          for (scenario_name in names(warnings)) {
            for (code in names(warnings[[scenario_name]])) {
              cli::cli_alert_warning(paste0("Scenario: ", scenario_name, " - ", warnings[[scenario_name]][[code]]))
            }
          }
          cli::cli_end()
        } else {
          cli::cli_alert_success("No warnings found.")
        }
      },

      #' @description Validate a specific scenario.
      #' This method will check if a specific scenario has valid individuals.
      #' @param scenario_name The name of the scenario to validate.
      validate_scenario = function(scenario_name) {
        validator <- ScenarioValidator$new(self$configurations, WarningManager$new())
        validator$validate_scenario(scenario_name)
        warnings <- validator$get_warnings()

        if (length(warnings) > 0) {
          cli::cli_alert_info("Warnings:")
          cli::cli_ul()
          for (scenario_name in names(warnings)) {
            for (code in names(warnings[[scenario_name]])) {
              cli::cli_alert_warning(paste0("Scenario: ", scenario_name, " - ", warnings[[scenario_name]][[code]]))
            }
          }
          cli::cli_end()
        } else {
          cli::cli_alert_success("No warnings found.")
        }
      },

      get_warning_manager = function() {
        return(private$.warningManager)  # Return the WarningManager instance
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
          updated_scenarios <- names(value)

          # Update scenarios
          private$.scenarios <- modifyList(private$.scenarios, value)

          purrr::walk(updated_scenarios, ~ {
            self$validate_scenario(.x)
            # Add a message indicating the validation process
            cli::cli_alert_info(paste("Validated scenario:", .x))
          })

        }
        invisible(private$.scenarios)
      },
      #' @field simulationResults Simulation results from the run simulations.
      simulationResults = function(value) {
        if (!missing(value)) {
          cli::cli_abort("Simulation Results cannot be altered.")
        }
        return(private$.simulationResults)
      },
      #' @field activeScenarios Active scenarios to be loaded and run.
      activeScenarios = function(value) {
        if (!missing(value)) {
          cli::cli_abort("Active Scenarios cannot be altered, use `selectScenarios` instead.")
        }
        return(purrr::keep(self$scenarios, ~ .x$status == "active" | .x$status == "loaded"))
      }
    ),
    private = list(
      .configurations = NULL,
      .newConfigurations = FALSE,
      .availableScenarios = NULL,
      .scenarios = list(),
      .simulationResults = list(),
      # Initialize warningManager as NULL initially
      .warningManager = NULL,
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
            scenarioConfiguration = self$configurations$scenarios[[scenario]],
            status = if (!is.null(private$.scenarios[[scenario]]$status) && private$.scenarios[[scenario]]$status == "inactive") "inactive" else NULL
          )
        }
      }
    )
  )
