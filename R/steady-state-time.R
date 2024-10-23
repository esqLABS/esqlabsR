#' @title SteadyStateTime
#' @description A class representing the simulation time of a scenario.
#' @keywords internal
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
    initialize = function(steadyStateTime = 1000, steadyStateTimeUnit = ospsuite::ospUnits$Time$min) {
      self$time <- steadyStateTime
      self$timeUnit <- steadyStateTimeUnit

      private$.validateSteadyStateTimeValue()
      private$.validateSteadyStateTimeUnit()


      self$timeBaseUnit <- ospsuite::toBaseUnit(
        quantityOrDimension = ospsuite::ospDimensions$Time,
        values = self$time,
        unit = self$timeUnit
      )
    }
  ),
  active = list(),
  private = list(
    .validateSteadyStateTimeValue = function() {
      # Check that value is positive
      if (any(self$time < 0)) {
        cli_abort("Steady State Time must be positive.")
      }
    },

    .validateSteadyStateTimeUnit = function() {
      # Check that is a valid unit
      if (!self$timeUnit %in% as.character(ospUnits$Time)) {
        cli_abort("The steady state time unit must be a valid unit of time as defined in `ospsuite::ospUnits$Time`.")
      }
    }

  )
)

