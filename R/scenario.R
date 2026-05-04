# Scenario ----
#
# Plain-data scenario class produced by `.parseScenarios()` in
# `R/project-parse.R`. Holds the typed shape of a JSON scenario entry
# without any runtime side effects: no Simulation construction, no
# Population loading, no parameter merging. Construction is by direct
# field assignment.

#' @title Scenario
#' @docType class
#' @description Plain-data class holding scenario configuration fields
#'   parsed from a v2.0 `Project.json`. Does not create or hold ospsuite
#'   runtime objects; the runtime is built by [runScenarios()] at
#'   execution time.
#' @format NULL
#' @export
Scenario <- R6::R6Class(
  "Scenario",
  cloneable = TRUE,
  public = list(
    #' @field scenarioName Character. Name of the scenario.
    scenarioName = NULL,
    #' @field modelFile Character. Name of the `.pkml` model file
    #'   (relative to the model folder).
    modelFile = NULL,
    #' @field applicationProtocol Character or `NA`. Name of the
    #'   application protocol; `NA` when absent in JSON.
    applicationProtocol = NULL,
    #' @field individualId Character. ID referencing
    #'   `project$individuals`.
    individualId = NULL,
    #' @field populationId Character or `NULL`. ID referencing
    #'   `project$populations`.
    populationId = NULL,
    #' @field outputPaths Named character vector of literal output paths,
    #'   resolved at parse time from `outputPathIds`. Names are the
    #'   ids referencing `project$outputPaths`; values are the literal
    #'   paths. `NULL` when the scenario declares no `outputPathIds`.
    #'   Round-trip serialization reads `names(outputPaths)` to rebuild
    #'   `outputPathIds`; mutators must preserve the named-vector
    #'   invariant.
    outputPaths = NULL,
    #' @field simulationType Character. `"Individual"` or
    #'   `"Population"`; derived from `populationId` presence.
    simulationType = "Individual",
    #' @field readPopulationFromCSV Logical. If `TRUE`, load population
    #'   from CSV.
    readPopulationFromCSV = FALSE,
    #' @field simulateSteadyState Logical. If `TRUE`, run steady-state
    #'   before the main simulation.
    simulateSteadyState = FALSE,
    #' @field simulationTime List of length-3 numeric vectors
    #'   `c(start, end, resolution)`, parsed from the JSON string.
    simulationTime = NULL,
    #' @field simulationTimeUnit Character. Time unit for
    #'   `simulationTime`, preserved as declared in JSON.
    simulationTimeUnit = NULL,
    #' @field steadyStateTime Numeric. Steady-state time **in base
    #'   unit (minutes)**; converted from the JSON value at parse time.
    steadyStateTime = 1000,
    #' @field steadyStateTimeUnit Character. Original unit for
    #'   `steadyStateTime`, preserved for round-trip serialization.
    steadyStateTimeUnit = NULL,
    #' @field overwriteFormulasInSS Logical. Overwrite formula
    #'   parameters during steady-state.
    overwriteFormulasInSS = FALSE,
    #' @field modelParameters Character vector. Parameter-set group
    #'   names referencing `project$modelParameters`.
    modelParameters = NULL,

    #' @description Create a new Scenario. All fields default;
    #'   the parser populates them by direct assignment.
    initialize = function() {
    },

    #' @description Print a one-line-per-field summary.
    #' @param ... Unused; present for S3 method consistency.
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
      if (
        !is.null(self$applicationProtocol) &&
          !is.na(self$applicationProtocol)
      ) {
        cat("  Protocol:       ", self$applicationProtocol, "\n")
      }
      if (!is.null(self$modelParameters)) {
        cat(
          "  Param groups:   ",
          paste(self$modelParameters, collapse = ", "),
          "\n"
        )
      }
      if (!is.null(self$outputPaths)) {
        cat("  Output paths:   ", length(self$outputPaths), "path(s)\n")
      }
      if (self$simulateSteadyState) {
        cat(
          "  Steady state:    TRUE (time=",
          self$steadyStateTime,
          "min)\n"
        )
      }
      invisible(self)
    }
  )
)
