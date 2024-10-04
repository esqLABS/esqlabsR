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
      status = function(explicit = TRUE) {

        purrr::walk(self$configurations$scenarios, ~ .x$validate())
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
          if (explicit) {
            cli::cli_alert_success("No warnings found.")
          }
        }
      },

      #' @description Export the entire project to a JSON file
      #' @param filePath The file path where the JSON will be saved
      #' @return NULL
      exportToJSON = function() {

        # Prompt for the directory path
        dirPath <- readline(prompt = "Please enter the directory path where to save the file: ")

        # Validate directory path
        if (!dir.exists(dirPath)) {
          cli::cli_alert_danger("Invalid directory path. Export aborted.")
          return(invisible(NULL))
        }

        # Prompt for the file name
        fileName <- readline(prompt = "Please enter the file name (without extension): ")

        # Construct the full file path by combining the directory path and file name
        filePath <- file.path(dirPath, paste0(fileName, ".json"))

        # Convert the project configuration and scenarios to a list format
        projectData <- list(
          projectConfiguration = self$projectConfiguration$toList(),
          configurations = self$configurations$toList(),
          scenarios = purrr::map(self$scenarios, ~ .x$toList()),  # Convert each scenario to a list
          simulationResults = private$.simulationResults  # Add simulation results
        )

        # Use jsonlite to serialize the list to JSON and save it to a file
        jsonlite::write_json(projectData, path = filePath, pretty = TRUE, auto_unbox = TRUE)

        cli::cli_alert_success(paste("Project successfully exported to", filePath))
        invisible(NULL)
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

          # Trigger validation after configurations are altered
          self$status(explicit = FALSE)
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
