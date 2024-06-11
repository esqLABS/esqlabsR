#' @title SteadyStateTime
#' @description A class representing the simulation time of a scenario.
SteadyStateTime <- R6::R6Class(
  classname = "SteadyStateTime",
  public = list(
    #' @field time The steady state time value.
    time = NULL,
    #' @field timeUnit The steady state time unit.
    timeUnit = NULL,
    #' @field timeBaseUnit The steady state time value in the base unit.
    timeBaseUnit = NULL,
    #' @description Initializes the SteadyStateTime object
    #' @param steadyStateTime The steady state time value (default to 1000 minutes)
    #' @param steadyStateTimeUnit The steady state time unit (default to minutes)
    initialize = function(steadyStateTime = 1000, steadyStateTimeUnit = ospUnits$Time$min) {
      private$.validateSteadyStateTimeValue(steadyStateTime)
      private$.validateSteadyStateTimeUnit(steadyStateTimeUnit)

      self$time <- steadyStateTime
      self$timeUnit <- steadyStateTimeUnit

      self$timeBaseUnit <- ospsuite::toBaseUnit(
        quantityOrDimension = ospDimensions$Time,
        values = self$time,
        unit = self$timeUnit
      )
    }
  ),
  active = list(),
  private = list(
    .validateSteadyStateTimeValue = function(steadyStateTime) {
      # Check that value is positive
      if (any(steadyStateTime < 0)) {
        cli_abort("Steady State Time must be positive.")
      }
    },

    .validateSteadyStateTimeUnit = function(steadyStateTimeUnit) {
      # Check that is a valid unit
      if (!steadyStateTimeUnit %in% as.character(ospUnits$Time)) {
        cli_abort("The steady state time unit must be a valid unit of time as defined in `ospsuite::ospUnits$Time`.")
      }
    }

  )
)

