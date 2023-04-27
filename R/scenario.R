#' @title Scenario
#' @docType class
#' @description Simulation scenario
#' @format NULL
#' @export
Scenario <- R6::R6Class(
  "Scenario",
  inherit = ospsuite.utils::Printable,
  cloneable = FALSE,
  active = list(
    #' @field scenarioConfiguration `scenarioConfiguration` used for creation of this scenario.
    #' Read-only
    scenarioConfiguration = function(value) {
      if (missing(value)) {
        private$.scenarioConfiguration
      } else {
        stop(messages$errorPropertyReadOnly("scenarioConfiguration"))
      }
    },
    #' @field finalCustomParams Custom parameters to be used for the simulation.
    #' Read-only
    #' @description
    #' Custom parameters to be used for the simulation. The final custom parameters
    #' are a combination of parametrization through the excel files and the custom
    #' parameters specified by the user through the `customParams` argument of the
    #' `Scenario` constructor.
    finalCustomParams = function(value) {
      if (missing(value)) {
        private$.finalCustomParams
      } else {
        stop(messages$errorPropertyReadOnly("finalCustomParams"))
      }
    },
    #' @field simulation Simulation object created from the `ScenarioConfiguration`. Read-only
    #' @description Simulation object. Read-only
    simulation = function(value) {
      if (missing(value)) {
        private$.simulation
      } else {
        stop(messages$errorPropertyReadOnly("simulation"))
      }
    },
    #' @field population Population object in case the scenario is a population simulation. Read-only
    population = function(value) {
      if (missing(value)) {
        private$.population
      } else {
        stop(messages$errorPropertyReadOnly("population"))
      }
    },

    #' @field scenarioType Type of the scenario - individual or population. Read-only
    scenarioType = function(value) {
      if (missing(value)) {
        if (!any(is.na(private$.population)) && isOfType(private$.population$population, "Population")) {
          "Population"
        } else {
          "Individual"
        }
      } else {
        stop(messages$errorPropertyReadOnly("scenarioType"))
      }
    }
  ),
  private = list(
    .scenarioConfiguration = NULL,
    .finalCustomParams = NULL,
    .simulation = NULL,
    .population = NA,

    # Private function for initialization of the scenario from the configuration
    .initializeFromConfiguration = function(customParams = NULL) {
      scenarioConfiguration <- private$.scenarioConfiguration
      # Read parameters from the parameters file
      params <- readParametersFromXLS(
        file.path(
          scenarioConfiguration$projectConfiguration$paramsFolder,
          scenarioConfiguration$projectConfiguration$paramsFile
        ),
        scenarioConfiguration$paramSheets
      )

      # Apply individual physiology, if specified
      individualCharacteristics <- NULL
      if (!is.null(scenarioConfiguration$individualId)) {
        individualCharacteristics <- readIndividualCharacteristicsFromXLS(
          XLSpath = file.path(
            scenarioConfiguration$projectConfiguration$paramsFolder,
            scenarioConfiguration$projectConfiguration$individualsFile
          ),
          individualId = scenarioConfiguration$individualId,
          nullIfNotFound = TRUE
        )

        if (is.null(individualCharacteristics)) {
          warning(paste0(
            "No individual characteristics for individual id '",
            scenarioConfiguration$individualId, "' found."
          ))
        }

        # Find individual-specific model parameters
        excelSheets <- readxl::excel_sheets(path = file.path(
          scenarioConfiguration$projectConfiguration$paramsFolder,
          scenarioConfiguration$projectConfiguration$individualsFile
        ))

        if (any(excelSheets == scenarioConfiguration$individualId)) {
          indivModelParams <- readParametersFromXLS(file.path(
            scenarioConfiguration$projectConfiguration$paramsFolder,
            scenarioConfiguration$projectConfiguration$individualsFile
          ), sheets = scenarioConfiguration$individualId)

          # Add individual model parameters to the parameters structure
          params <- extendParameterStructure(
            parameters = params,
            newParameters = indivModelParams
          )
        } else {
          warning(paste0(
            "No individual specific model parameters for individual id '",
            scenarioConfiguration$individualId, "' found."
          ))
        }
      }

      if (!is.null(customParams)) {
        params <- extendParameterStructure(
          parameters = params,
          newParameters = customParams
        )
      }

      # Set administration protocols
      excelFilePath <- file.path(
        scenarioConfiguration$projectConfiguration$paramsFolder,
        scenarioConfiguration$projectConfiguration$scenarioApplicationsFile
      )
      if (any(readxl::excel_sheets(excelFilePath) == scenarioConfiguration$applicationProtocol)) {
        applicationParams <- readParametersFromXLS(excelFilePath, scenarioConfiguration$applicationProtocol)
        params <- extendParameterStructure(
          parameters = params,
          newParameters = applicationParams
        )
      }
      # Save the final custom parameters
      private$.finalCustomParams <- params

      # Load simulation
      simulation <- ospsuite::loadSimulation(filePath = file.path(
        scenarioConfiguration$projectConfiguration$modelFolder,
        scenarioConfiguration$modelFile
      ), loadFromCache = FALSE)
      # Set the outputs, if new were specified
      if (!is.null(scenarioConfiguration$outputPaths)) {
        clearOutputs(simulation)
        addOutputs(quantitiesOrPaths = scenarioConfiguration$outputPaths, simulation = simulation)
      }
      # Set simulation time if defined by the user.
      if (!is.null(scenarioConfiguration$simulationTime)) {
        setOutputInterval(
          simulation = simulation,
          startTime = 0,
          endTime = scenarioConfiguration$simulationTime,
          resolution = scenarioConfiguration$pointsPerMinute
        )
      }

      initializeSimulation(
        simulation = simulation,
        individualCharacteristics = individualCharacteristics,
        additionalParams = params
      )

      return(simulation)
    },

    # Private function to create population from the configuration
    .initializePopulationFromConfiguration = function() {
      scenarioConfiguration <- private$.scenarioConfiguration
      # Defining an empty population as `NA` because test for `NULL` is painful
      population <- NA

      # Create a population for population scenarios
      if (scenarioConfiguration$simulationType == "Population") {
        if (scenarioConfiguration$readPopulationFromCSV) {
          populationPath <- paste0(file.path(
            scenarioConfiguration$projectConfiguration$paramsFolder,
            "Populations",
            scenarioConfiguration$populationId
          ), ".csv")
          population <- loadPopulation(populationPath)
        } else {
          popCharacteristics <- readPopulationCharacteristicsFromXLS(
            XLSpath = file.path(
              scenarioConfiguration$projectConfiguration$paramsFolder,
              scenarioConfiguration$projectConfiguration$populationParamsFile
            ),
            populationName = scenarioConfiguration$populationId,
            sheet = "Demographics"
          )
          population <- createPopulation(populationCharacteristics = popCharacteristics)
        }
      }
      private$.population <- population
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class. Initializes the scenario from
    #' `ScenarioConfiguration` object.
    #' @param scenarioConfiguration An object of class `ScenarioConfiguration`.
    #' @param customParams Custom parameters to be used for the simulation.
    #'  A list containing vectors 'paths' with the full paths to the
    #' parameters, 'values' the values of the parameters, and 'units' with the
    #' units the values are in.
    #' @return A new `Scenario` object.
    initialize = function(scenarioConfiguration, customParams = NULL) {
      private$.scenarioConfiguration <- scenarioConfiguration
      private$.simulation <- private$.initializeFromConfiguration(customParams = customParams)
      private$.initializePopulationFromConfiguration()
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      self$scenarioConfiguration$print()
      private$printClass()
      private$printLine("Scenario type", self$scenarioType)
      invisible(self)
    }
  )
)
