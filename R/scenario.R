#' @title Scenario
#' @docType class
#' @description Simulation scenario
#' @format NULL
#' @export
Scenario <- R6::R6Class(
  "Scenario",
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
        if (isOfType(private$.population, "Population")) {
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
    .initializeFromConfiguration = function(customParams = NULL,
                                            stopIfParameterNotFound = TRUE) {
      scenarioConfiguration <- private$.scenarioConfiguration
      # Read parameters from the parameters file
      params <- readParametersFromXLS(
        scenarioConfiguration$projectConfiguration$modelParamsFile,
        scenarioConfiguration$paramSheets
      )

      # Apply individual physiology, if specified
      individualCharacteristics <- NULL
      # Check for 'NULL' and 'NA'. 'NA' happens when no individual is defined in
      # the excel file.
      if (
        !is.null(scenarioConfiguration$individualId) &&
          !is.na(scenarioConfiguration$individualId)
      ) {
        individualCharacteristics <- readIndividualCharacteristicsFromXLS(
          XLSpath = scenarioConfiguration$projectConfiguration$individualsFile,
          individualId = scenarioConfiguration$individualId,
          nullIfNotFound = TRUE
        )

        if (is.null(individualCharacteristics)) {
          warning(messages$warningNoIndividualCharacteristics(
            scenarioName = scenarioConfiguration$scenarioName,
            individualId = scenarioConfiguration$individualId
          ))
        }

        # Find individual-specific model parameters
        excelSheets <- readxl::excel_sheets(
          path = scenarioConfiguration$projectConfiguration$individualsFile
        )

        if (any(excelSheets == scenarioConfiguration$individualId)) {
          indivModelParams <- readParametersFromXLS(
            scenarioConfiguration$projectConfiguration$individualsFile,
            sheets = scenarioConfiguration$individualId
          )

          # Add individual model parameters to the parameters structure
          params <- extendParameterStructure(
            parameters = params,
            newParameters = indivModelParams
          )
        } else {
          warning(messages$warningNoIndividualSpecificModelParameters(
            scenarioName = scenarioConfiguration$scenarioName,
            individualId = scenarioConfiguration$individualId
          ))
        }
      }

      # Set administration protocols
      excelFilePath <- scenarioConfiguration$projectConfiguration$applicationsFile
      # Checking for 'NA' if administration protocol is not set in excel file.
      if (!is.na(scenarioConfiguration$applicationProtocol)) {
        if (
          !any(
            readxl::excel_sheets(excelFilePath) ==
              scenarioConfiguration$applicationProtocol
          )
        ) {
          stop(messages$errorApplicationProtocolNotFound(
            scenarioName = scenarioConfiguration$scenarioName,
            applicationProtocol = scenarioConfiguration$applicationProtocol
          ))
        }
        applicationParams <- readParametersFromXLS(
          excelFilePath,
          scenarioConfiguration$applicationProtocol
        )
        params <- extendParameterStructure(
          parameters = params,
          newParameters = applicationParams
        )
      }

      if (!is.null(customParams)) {
        params <- extendParameterStructure(
          parameters = params,
          newParameters = customParams
        )
      }

      # Save the final custom parameters
      private$.finalCustomParams <- params

      # Load simulation
      simulation <- ospsuite::loadSimulation(
        filePath = file.path(
          scenarioConfiguration$projectConfiguration$modelFolder,
          scenarioConfiguration$modelFile
        ),
        loadFromCache = FALSE
      )
      # Set simulation name
      simulation$name <- scenarioConfiguration$scenarioName
      # Set the outputs, if new were specified
      if (!is.null(scenarioConfiguration$outputPaths)) {
        setOutputs(
          quantitiesOrPaths = scenarioConfiguration$outputPaths,
          simulation = simulation
        )
      }
      # Set simulation time if defined by the user.
      if (!is.null(scenarioConfiguration$simulationTime)) {
        # clear output intervals
        clearOutputIntervals(simulation)
        # Iterate through all output intervals and add them to simulation
        for (i in seq_along(scenarioConfiguration$simulationTime)) {
          addOutputInterval(
            simulation = simulation,
            startTime = toBaseUnit(
              quantityOrDimension = ospDimensions$Time,
              values = scenarioConfiguration$simulationTime[[i]][1],
              unit = scenarioConfiguration$simulationTimeUnit
            ),
            endTime = toBaseUnit(
              quantityOrDimension = ospDimensions$Time,
              values = scenarioConfiguration$simulationTime[[i]][2],
              unit = scenarioConfiguration$simulationTimeUnit
            ),
            resolution = scenarioConfiguration$simulationTime[[i]][3] /
              toBaseUnit(
                quantityOrDimension = ospDimensions$Time,
                values = 1,
                unit = scenarioConfiguration$simulationTimeUnit
              )
          )
        }
      }

      initializeSimulation(
        simulation = simulation,
        individualCharacteristics = individualCharacteristics,
        additionalParams = params,
        stopIfParameterNotFound = stopIfParameterNotFound
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
          populationPath <- paste0(
            file.path(
              scenarioConfiguration$projectConfiguration$populationsFolder,
              scenarioConfiguration$populationId
            ),
            ".csv"
          )
          population <- loadPopulation(populationPath)
        } else {
          popCharacteristics <- readPopulationCharacteristicsFromXLS(
            XLSpath = scenarioConfiguration$projectConfiguration$populationsFile,
            populationName = scenarioConfiguration$populationId,
            sheet = "Demographics"
          )
          population <- createPopulation(
            populationCharacteristics = popCharacteristics
          )
          # Create population returns a list, in contrast to load population, where the object is returned!
          population <- population$population
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
    #' @param stopIfParameterNotFound Logical. If `TRUE` (default), an error is
    #'   thrown if any of the custom defined parameter does not exist. If `FALSE`,
    #'   non-existent parameters are  ignored.
    #' @returns A new `Scenario` object.
    initialize = function(scenarioConfiguration,
                          customParams = NULL,
                          stopIfParameterNotFound = TRUE) {
      private$.scenarioConfiguration <- scenarioConfiguration
      private$.simulation <- private$.initializeFromConfiguration(
        customParams = customParams,
        stopIfParameterNotFound = stopIfParameterNotFound
      )
      private$.initializePopulationFromConfiguration()
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "Scenario type" = self$scenarioType
      ))
      self$scenarioConfiguration$print(className = FALSE)
      invisible(self)
    }
  )
)
