#' @title SimulationTime
#' @description A class representing the simulation time of a scenario.
SimulationTime <- R6::R6Class(
  classname = "SimulationTime",
  public = list(
    #' @field startTime Start time of the simulation
    startTime = NULL,
    #' @field endTime End time of the simulation
    endTime = NULL,
    #' @field resolution Number of simulated points per time unit
    resolution = NULL,
    #' @field unit Unit of the simulation time intervals
    unit = NULL,
    #' @description Creates a new simulation time object
    #' @param simulationTime A string made of three values `StartTime, EndTime, Resolution`, where `Resolution` is the number of
    #' simulated points per time unit  (defined by the `simulationTimeUnit` argument).
    #' @param simulationTimeUnit A string representing the unit of the simulation time intervals. Must be a valid unit of time as defined in `ospsuite::ospUnits$Time`.
    initialize = function(simulationTime, simulationTimeUnit) {
      private$.rawSimulationTime <- simulationTime
      simulationTimeValues <- as.numeric(unlist(strsplit(simulationTime, ",")))

      validateSimulationTimeValues(simulationTimeValues)
      validateSimulationTimeUnit(simulationTimeUnit)

      self$startTime <- simulationTimeValues[1]
      self$endTime <- simulationTimeValues[2]
      self$resolution <- simulationTimeValues[3]
      self$unit <- simulationTimeUnit
    },
    #' @description Prints a summary of the simulation time.
    print = function() {
      cli_ul()
      cli_inform(self$summary)
    }
  ),
  active = list(
    #' @field timePoints a vector of time points from the start time to the end time with the resolution defined in the simulation time.
    timePoints = function() {
      seq(from = self$startTime, to = self$endTime, by = 1 / self$resolution)
    },
    #' @field timePointsNumber Number of time points in the simulation time.
    timePointsNumber = function() {
      length(self$timePoints)
    },
    #' @field summary A summary of the simulation time.
    summary = function() {
      glue::glue("{self$startTime}{self$unit} to {self$endTime}{self$unit} with resolution of {self$resolution} pts/{self$unit} (total: {self$timePointsNumber} points).")
    }
  ),
  private = list(
    .rawSimulationTime = NULL
  )
)


validateSimulationTimeValues <- function(simulationTimeValues) {
  validateIsNumeric(simulationTimeValues)
  # Validate that all are positive
  if (any(simulationTimeValues < 0)) {
    cli_abort("All values in the simulation time must be positive.")
  }
  # Validate all intervals are of length 3
  if (length(simulationTimeValues) != 3) {
    cli_abort("The simulation time must be a string of three values separated by commas.")
  }
  # Validate all resolution entries are greater than 0
  if (simulationTimeValues[3] <= 0) {
    cli_abort("The resolution must be greater than 0.")
  }
  # Validate all start values are smaller than end values
  if (simulationTimeValues[1] >= simulationTimeValues[2]) {
    cli_abort("The start time must be smaller than the end time.")
  }
}

validateSimulationTimeUnit <- function(simulationTimeUnit) {
  if (!simulationTimeUnit %in% as.character(ospUnits$Time)) {
    cli_abort("The simulation time unit must be a valid unit of time as defined in `ospsuite::ospUnits$Time`.")
  }
}
