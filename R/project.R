Project <-
  R6::R6Class(
    classname = "Project",
    public = list(
      projectConfiguration = NULL,
      #' @param projectConfiguration A projectConfiguration object
      initialize = function(projectConfiguration) {
        self$projectConfiguration <- projectConfiguration
        private$.availableScenarios <- private$.getAvailableScenarios()
        private$.initializeScenarios()
      },
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
      reloadConfigurations = function() {
        private$.configurations <- NULL
      },
      selectScenarios = function(scenarios = NULL) {
        if (is.null(scenarios)) {
          for (scenario in self$scenarios) {
            scenarios$status <- "active"
          }
        } else {
          ospsuite.utils::validateVectorValues(scenarios, type = "character", allowedValues = private$.availableScenarios)
          for (scenario in names(self$scenarios)) {
            if (scenario %in% scenarios) {
              self$scenarios[[scenario]]$status <- "active"
            } else {
              self$scenarios[[scenario]]$status <- "inactive"
            }
          }
        }
      },
      loadScenarios = function() {
        purrr::map(self$scenarios, ~ .x$load(), .progress = "Loading Scenarios")
        invisible(self)
      },
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
      scenarios = function(value) {
        if (identical(private$.scenarios, list()) || private$.newConfigurations) {
          private$.initializeScenarios()
          private$.newConfigurations <- FALSE
        }

        if (!missing(value)) {
          private$.scenarios <- modifyList(private$.scenarios, value)
        }
        return(private$.scenarios)
      },
      simulationResults = function(value) {
        if (!missing(value)) {
          private$.simulationResults <- modifyList(private$.simulationResults, value)
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
          private$.scenarios[[scenario]] <- Scenario2$new(
            project = self,
            scenarioConfigurationData = self$configurations$scenarios[[scenario]],
            status = if(private$.scenarios[[scenario]]$status == "inactive") "inactive" else NULL
          )
        }
      }
    )
  )
