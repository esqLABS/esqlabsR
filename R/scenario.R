# Scenario ----
#
# Plain-data scenario record produced by `.parseScenarios()` in
# `R/scenarios.R`. Holds the typed shape of a JSON scenario entry
# without any runtime side effects: no Simulation construction, no
# Population loading, no parameter merging. A `Scenario` is a classed
# list with copy semantics: extracted entries are independent copies,
# never references into a `Project`.

# Explicit field list backing `as.list.Scenario()`. Kept in sync with
# the v2.0 `Project.json` scenario entry shape so adding a method on
# `Scenario` does not change the JSON-shape contract.
.scenarioFieldNames <- c(
  "scenarioName",
  "modelFile",
  "applicationProtocol",
  "individualId",
  "populationId",
  "outputPaths",
  "simulationType",
  "readPopulationFromCSV",
  "simulateSteadyState",
  "simulationTime",
  "simulationTimeUnit",
  "steadyStateTime",
  "steadyStateTimeUnit",
  "overwriteFormulasInSS",
  "modelParameterSets"
)

#' Create a Scenario
#'
#' @description Builds a plain-data `Scenario` record holding the
#'   configuration fields of a v2.0 `Project.json` scenario entry. It does
#'   not create or hold ospsuite runtime objects; the runtime is built by
#'   [runScenarios()] at execution time.
#'
#'   A `Scenario` is a named list with copy semantics: an entry extracted
#'   from `project$scenarios` is an independent copy, and writing it back
#'   (e.g. `project$scenarios[[name]] <- sc`) is what mutates the project.
#'
#' @param scenarioName Character. Name of the scenario.
#' @param modelFile Character. Name of the `.pkml` model file (relative to
#'   the model folder).
#' @param applicationProtocol Character or `NA`. Name of the application
#'   protocol; `NA` when absent.
#' @param individualId Character or `NULL`. ID referencing
#'   `project$individuals`.
#' @param populationId Character or `NULL`. ID referencing
#'   `project$populations`.
#' @param outputPaths Named character vector of literal output paths.
#'   Names are the ids referencing `project$outputPaths`; values are the
#'   literal paths. `NULL` when the scenario declares no outputs.
#'   Round-trip serialization reads `names(outputPaths)` to rebuild
#'   `outputPathIds`, so the named-vector invariant must be preserved.
#' @param simulationType Character. `"Individual"` or `"Population"`.
#'   Defaults to `"Population"` when `populationId` is set,
#'   `"Individual"` otherwise.
#' @param readPopulationFromCSV Logical. If `TRUE`, load population from
#'   CSV.
#' @param simulateSteadyState Logical. If `TRUE`, run steady-state before
#'   the main simulation.
#' @param simulationTime List of length-3 numeric vectors
#'   `c(start, end, resolution)`.
#' @param simulationTimeUnit Character. Time unit for `simulationTime`.
#' @param steadyStateTime Numeric. Steady-state time **in base unit
#'   (minutes)**.
#' @param steadyStateTimeUnit Character. Original unit for
#'   `steadyStateTime`, preserved for round-trip serialization.
#' @param overwriteFormulasInSS Logical. Overwrite formula parameters
#'   during steady-state.
#' @param modelParameterSets Character vector. Parameter-set names
#'   referencing `project$modelParameterSets`.
#'
#' @returns A `Scenario` object: a named list carrying exactly the fields
#'   above.
#' @export
Scenario <- function(
  scenarioName = NULL,
  modelFile = NULL,
  applicationProtocol = NULL,
  individualId = NULL,
  populationId = NULL,
  outputPaths = NULL,
  simulationType = if (is.null(populationId)) "Individual" else "Population",
  readPopulationFromCSV = FALSE,
  simulateSteadyState = FALSE,
  simulationTime = NULL,
  simulationTimeUnit = NULL,
  steadyStateTime = 1000,
  steadyStateTimeUnit = NULL,
  overwriteFormulasInSS = FALSE,
  modelParameterSets = NULL
) {
  rec <- stats::setNames(
    vector("list", length(.scenarioFieldNames)),
    .scenarioFieldNames
  )
  env <- environment()
  for (n in .scenarioFieldNames) {
    # `rec[n] <- list(value)` keeps NULL-valued slots so the record always
    # carries the full 15-name shape regardless of which arguments are set.
    rec[n] <- list(env[[n]])
  }
  structure(rec, class = c("Scenario", "list"))
}

#' @exportS3Method
#' @noRd
as.list.Scenario <- function(x, ...) {
  stats::setNames(
    lapply(.scenarioFieldNames, function(n) x[[n]]),
    .scenarioFieldNames
  )
}

#' @exportS3Method
#' @noRd
print.Scenario <- function(x, ...) {
  cat("<Scenario>", "\n")
  cat("  Name:           ", x$scenarioName %||% "(none)", "\n")
  cat("  Model:          ", x$modelFile %||% "(none)", "\n")
  cat("  Type:           ", x$simulationType, "\n")
  cat("  Individual:     ", x$individualId %||% "(none)", "\n")
  if (x$simulationType == "Population") {
    cat("  Population:     ", x$populationId %||% "(none)", "\n")
    cat("  CSV Population: ", x$readPopulationFromCSV, "\n")
  }
  if (
    !is.null(x$applicationProtocol) &&
      !is.na(x$applicationProtocol)
  ) {
    cat("  Protocol:       ", x$applicationProtocol, "\n")
  }
  if (!is.null(x$modelParameterSets)) {
    cat(
      "  Param groups:   ",
      paste(x$modelParameterSets, collapse = ", "),
      "\n"
    )
  }
  if (!is.null(x$outputPaths)) {
    cat("  Output paths:   ", length(x$outputPaths), "path(s)\n")
  }
  if (x$simulateSteadyState) {
    cat(
      "  Steady state:    TRUE (time=",
      x$steadyStateTime,
      "min)\n"
    )
  }
  invisible(x)
}
