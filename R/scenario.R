#' @title Scenario
#'
#' @description Plain data class holding scenario configuration fields.
#' Does not create or hold ospsuite runtime objects. Used by `runScenarios()`
#' which handles all object creation.
#'
#' @field scenarioName Character. Name of the scenario.
#' @field modelFile Character. Name of the `.pkml` model file (relative to model folder).
#' @field applicationProtocol Character or NA. Name of the application protocol.
#' @field individualId Character. ID of the individual from the individuals section.
#' @field populationId Character. ID of the population from the populations section.
#' @field outputPaths Character vector. Simulation output paths.
#' @field simulationType Character. `"Individual"` or `"Population"`.
#' @field readPopulationFromCSV Logical. If `TRUE`, load population from CSV file.
#' @field simulateSteadyState Logical. If `TRUE`, run steady-state before simulation.
#' @field simulationTime List of numeric vectors. Each vector has 3 elements:
#'   `c(start, end, resolution)`. `NULL` means use model default.
#' @field simulationTimeUnit Character. Time unit for simulationTime values.
#' @field steadyStateTime Numeric. Steady-state simulation time in minutes.
#' @field overwriteFormulasInSS Logical. If `TRUE`, overwrite formula parameters
#'   during steady-state.
#' @field parameterGroups Character vector. Names of model parameter groups to apply.
#'
#' @export
#' @family scenario
Scenario <- R6::R6Class(
  "Scenario",
  cloneable = TRUE,
  public = list(
    scenarioName = NULL,
    modelFile = NULL,
    applicationProtocol = NULL,
    individualId = NULL,
    populationId = NULL,
    outputPaths = NULL,
    simulationType = "Individual",
    readPopulationFromCSV = FALSE,
    simulateSteadyState = FALSE,
    simulationTime = NULL,
    simulationTimeUnit = NULL,
    steadyStateTime = 1000,
    #' @field steadyStateTimeUnit Character. Original unit for steadyStateTime
    #'   from the JSON configuration. Used to convert back during Excel export.
    steadyStateTimeUnit = NULL,
    overwriteFormulasInSS = FALSE,
    parameterGroups = NULL,

    #' @description Create a new Scenario.
    initialize = function() {},

    #' @description Print scenario summary.
    #' @param ... Ignored.
    print = function(...) {
      cat("<Scenario>", "\n")
      cat("  Name:           ", self$scenarioName %||% "(none)", "\n")
      cat("  Model:          ", self$modelFile %||% "(none)", "\n")
      cat("  Type:           ", self$simulationType, "\n")
      cat("  Individual:     ", self$individualId %||% "(none)", "\n")
      if (self$simulationType == "Population") {
        cat("  Population:     ", self$populationId %||% "(none)", "\n")
        cat("  CSV Population: ", self$readPopulationFromCSV, "\n")
      }
      if (!is.null(self$applicationProtocol) && !is.na(self$applicationProtocol)) {
        cat("  Protocol:       ", self$applicationProtocol, "\n")
      }
      if (!is.null(self$parameterGroups)) {
        cat("  Param groups:   ", paste(self$parameterGroups, collapse = ", "), "\n")
      }
      if (!is.null(self$outputPaths)) {
        cat("  Output paths:   ", length(self$outputPaths), "path(s)\n")
      }
      if (self$simulateSteadyState) {
        cat("  Steady state:    TRUE (time=", self$steadyStateTime, "min)\n")
      }
      invisible(self)
    }
  )
)
