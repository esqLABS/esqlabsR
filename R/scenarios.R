# Scenario ----
#
# Plain-data scenario record produced by `.parseScenarios()` below.
# Holds the typed shape of a JSON scenario entry
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

# Parse ----
#
# Internal: parse the JSON `scenarios` array into a named list of
# `Scenario` objects, keyed by scenario name.
#
# `scenariosData` is the raw `simplifyVector = FALSE` shape produced by
# `jsonlite::fromJSON()`: a list of plain named lists. `outputPaths` is
# the project-level lookup table (named list or named character vector
# of `id -> literal path`); used to resolve `outputPathIds`.
#
# This helper handles only what's needed at parse time: field copies,
# `simulationType` derivation from `populationId` presence,
# `simulationTime` string parsing, `steadyStateTime` unit conversion,
# `outputPathIds` -> literal `outputPaths` resolution.
#
# @keywords internal
# @noRd
.parseScenarios <- function(scenariosData, outputPaths) {
  if (is.null(scenariosData)) {
    return(list())
  }

  result <- list()
  for (entry in scenariosData) {
    # Use `[[` rather than `$` throughout: list `$` does partial matching,
    # so `entry$simulationTime` would wrongly resolve to `simulationTimeUnit`
    # (and `steadyStateTime` to `steadyStateTimeUnit`) when only the unit is
    # declared.
    simulationTime <- NULL
    if (!is.null(entry[["simulationTime"]])) {
      simulationTime <- .parseSimulationTimeIntervals(entry[["simulationTime"]])
    }
    # Preserve a declared unit even when `simulationTime` is null, so a
    # standalone `simulationTimeUnit` round-trips instead of being dropped.
    simulationTimeUnit <- entry[["simulationTimeUnit"]]

    steadyStateTime <- 1000
    steadyStateTimeUnit <- NULL
    if (!is.null(entry[["steadyStateTime"]])) {
      if (is.null(entry[["steadyStateTimeUnit"]])) {
        cli::cli_abort(
          "Scenario {.val {entry[['name']]}} has {.field steadyStateTime} set \\
          but {.field steadyStateTimeUnit} is null. Please specify a unit \\
          (e.g. {.val min})."
        )
      }
      steadyStateTime <- ospsuite::toBaseUnit(
        quantityOrDimension = ospDimensions$Time,
        values = entry[["steadyStateTime"]],
        unit = entry[["steadyStateTimeUnit"]]
      )
      steadyStateTimeUnit <- entry[["steadyStateTimeUnit"]]
    }

    scenarioOutputPaths <- NULL
    if (!is.null(entry[["outputPathIds"]])) {
      pathIds <- unlist(entry[["outputPathIds"]])
      unknown <- setdiff(pathIds, names(outputPaths))
      if (length(unknown) > 0) {
        cli::cli_abort(
          "Scenario {.val {entry[['name']]}} references unknown outputPathIds: \\
          {.val {unknown}}."
        )
      }
      scenarioOutputPaths <- stats::setNames(
        unlist(outputPaths[pathIds], use.names = FALSE),
        pathIds
      )
    }

    result[[entry[["name"]]]] <- Scenario(
      scenarioName = entry[["name"]],
      modelFile = entry[["modelFile"]],
      applicationProtocol = entry[["applicationProtocol"]] %||% NA,
      individualId = entry[["individualId"]],
      populationId = entry[["populationId"]],
      outputPaths = scenarioOutputPaths,
      readPopulationFromCSV = entry[["readPopulationFromCSV"]] %||% FALSE,
      simulateSteadyState = isTRUE(entry[["steadyState"]]),
      simulationTime = simulationTime,
      simulationTimeUnit = simulationTimeUnit,
      steadyStateTime = steadyStateTime,
      steadyStateTimeUnit = steadyStateTimeUnit,
      overwriteFormulasInSS = entry[["overwriteFormulasInSS"]] %||% FALSE,
      modelParameterSets = if (!is.null(entry[["modelParameterSets"]])) {
        unlist(entry[["modelParameterSets"]])
      }
    )
  }
  result
}

# Section validation adapters ----
#
# Registered in `.validationAdapters` (R/validation.R) and called by
# `.runProjectValidation()`. Each pulls the right slice of the project
# and delegates to a section-local `.validate*` helper. They only run
# when `validateProject()` (or a targeted `.ensureValid`) is called;
# they do not run during parsing or simulation.

#' @keywords internal
#' @noRd
.scenariosValidatorAdapter <- function(project) {
  .validateScenarios(project$scenarios)
}

#' @keywords internal
#' @noRd
.applicationsValidatorAdapter <- function(project) {
  .validateApplications(project$applications)
}

#' Validate the `scenarios` section of a Project
#'
#' Per-entry checks: `modelFile` is set and non-empty,
#' `simulationType` is one of the supported values, and
#' population-typed scenarios declare a `populationId`.
#'
#' Cross-section reference checks (individualId, modelParameterSets,
#' applicationProtocol, …) live in `.validateCrossReferences()`.
#'
#' @param scenarios Named list of `Scenario` objects from
#'   `project$scenarios`.
#' @return validationResult.
#' @keywords internal
#' @noRd
.validateScenarios <- function(scenarios) {
  result <- validationResult$new()

  if (is.null(scenarios) || length(scenarios) == 0) {
    result$add_warning("Data", "No scenarios defined")
    return(result)
  }

  for (name in names(scenarios)) {
    sc <- scenarios[[name]]

    if (is.null(sc$modelFile) || sc$modelFile == "") {
      result$add_critical_error(
        "Missing Fields",
        paste0("Scenario '", name, "' has no modelFile")
      )
    }

    simType <- sc$simulationType %||% ""
    if (!simType %in% c("Individual", "Population")) {
      result$add_critical_error(
        "Validation",
        paste0(
          "Scenario '",
          name,
          "' has invalid simulationType '",
          simType,
          "'"
        )
      )
    }

    if (
      simType == "Population" &&
        (is.null(sc$populationId) || sc$populationId == "")
    ) {
      result$add_critical_error(
        "Missing Fields",
        paste0("Population scenario '", name, "' has no populationId")
      )
    }

    if (
      simType != "Population" &&
        !is.null(sc$populationId) &&
        sc$populationId != ""
    ) {
      result$add_warning(
        "Validation",
        paste0(
          "Scenario '",
          name,
          "' has a populationId but simulationType is '",
          simType,
          "'; it will load as a Population scenario on the next reload."
        )
      )
    }
  }

  result
}

#' Validate the `applications` section of a Project
#'
#' The applications section is currently a thin wrapper around its
#' `parameterSets` references, all of which are checked in
#' `.validateCrossReferences()`. This adapter exists so that the
#' canonical section list still resolves to a working validator (and so
#' that future shape checks have an obvious home).
#'
#' @param applications Named list from `project$applications`.
#' @return validationResult.
#' @keywords internal
#' @noRd
.validateApplications <- function(applications) {
  result <- validationResult$new()
  if (is.null(applications) || length(applications) == 0) {
    result$add_warning("Data", "No applications defined")
  }
  result
}

#' Run a set of scenarios from a `Project`.
#'
#' @description Loads simulations, applies parameters, runs the
#'   simulations, and collects results for one or more scenarios
#'   defined on a parsed [Project]. The project must already have been
#'   loaded with [loadProject()].
#'
#' @param project A [Project] object loaded from a `Project.json` file.
#' @param scenarioNames Optional character vector of scenario names to
#'   run. `NULL` (default) runs all scenarios in the project.
#' @param customParams A list with vectors `paths`, `values`, and
#'   `units` — applied to every selected scenario as the final
#'   parameter layer.
#' @param simulationRunOptions Optional [ospsuite::SimulationRunOptions]
#'   for the simulation run. `NULL` (default) uses the package
#'   defaults.
#' @param validate Logical. If `TRUE` (default), runs the relevant
#'   section validators via [validateProject()] before simulating and
#'   aborts with a formatted summary on critical errors. Set to
#'   `FALSE` to skip the pre-flight check (e.g. when the caller has
#'   already validated the project).
#'
#' @returns A named list keyed by scenario name. Each entry is a list
#'   with `simulation` (the initialized [ospsuite::Simulation]),
#'   `results` ([ospsuite::SimulationResults]), `outputValues` (the
#'   computed output values, or `NULL` if simulation failed), and
#'   `population` (an [ospsuite::Population] for population
#'   scenarios, or `NULL` for individual scenarios).
#'
#' @details If a scenario's simulation fails, a warning is produced
#'   and `outputValues` for that scenario is `NULL`.
#'
#' @export
runScenarios <- function(
  project,
  scenarioNames = NULL,
  customParams = NULL,
  simulationRunOptions = NULL,
  validate = TRUE
) {
  if (!inherits(project, "Project")) {
    cli::cli_abort(
      "{.arg project} must be a {.cls Project} \
                    (see {.fn loadProject})."
    )
  }
  .runScenariosFromProject(
    project,
    scenarioNames,
    customParams,
    simulationRunOptions,
    validate
  )
}

# Public CRUD: scenarios ----

#' Add a scenario programmatically to a Project
#'
#' Creates a new `Scenario` and adds it to `project$scenarios` after
#' validating all references.
#'
#' @param project A `Project` object.
#' @param scenarioName Character. Name for the new scenario. Must not
#'   already exist in `project$scenarios`.
#' @param modelFile Character. Name of the `.pkml` model file (relative
#'   to model folder).
#' @param individualId Character or `NULL`. ID referencing
#'   `project$individuals`.
#' @param populationId Character or `NULL`. ID referencing
#'   `project$populations`.
#' @param applicationProtocol Character or `NULL`. Protocol name
#'   referencing `project$applications`.
#' @param modelParameterSets Character vector or `NULL`. Set names
#'   referencing `project$modelParameterSets`.
#' @param outputPathIds Character vector or `NULL`. IDs referencing
#'   `project$outputPaths`.
#' @param simulationTime Character or `NULL`. Format
#'   `"start, end, resolution"` or
#'   `"start, end, resolution; start, end, resolution"` for multiple
#'   intervals.
#' @param simulationTimeUnit Character. Time unit string. Default `"h"`.
#' @param steadyState Logical. Whether to simulate steady state. Default
#'   `FALSE`.
#' @param steadyStateTime Numeric. Steady-state time in
#'   `steadyStateTimeUnit`. Default `1000`.
#' @param steadyStateTimeUnit Character. Unit for `steadyStateTime`.
#'   Default `"min"`.
#' @param overwriteFormulasInSS Logical. Overwrite formulas during
#'   steady state. Default `FALSE`.
#' @param readPopulationFromCSV Logical. Load population from CSV.
#'   Default `FALSE`.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family scenario
addScenario <- function(
  project,
  scenarioName,
  modelFile,
  individualId = NULL,
  populationId = NULL,
  applicationProtocol = NULL,
  modelParameterSets = NULL,
  outputPathIds = NULL,
  simulationTime = NULL,
  simulationTimeUnit = "h",
  steadyState = FALSE,
  steadyStateTime = 1000,
  steadyStateTimeUnit = "min",
  overwriteFormulasInSS = FALSE,
  readPopulationFromCSV = FALSE
) {
  validateIsOfType(project, "Project")
  errors <- character()

  if (
    !is.character(scenarioName) ||
      length(scenarioName) != 1L ||
      is.na(scenarioName) ||
      nchar(scenarioName) == 0
  ) {
    errors <- c(errors, "scenarioName must be a non-empty string")
  } else if (scenarioName %in% names(project$scenarios)) {
    errors <- c(
      errors,
      paste0("scenario '", scenarioName, "' already exists")
    )
  }

  if (
    !is.character(modelFile) ||
      length(modelFile) != 1L ||
      is.na(modelFile) ||
      nchar(modelFile) == 0
  ) {
    errors <- c(errors, "modelFile must be a non-empty string")
  }

  checkScalarFK <- function(value, argName, lookup, lookupLabel) {
    if (is.null(value)) {
      return(character())
    }
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        nchar(value) == 0
    ) {
      return(paste0(argName, " must be a non-empty string or NULL"))
    }
    if (!(value %in% names(lookup))) {
      return(paste0(argName, " '", value, "' not found in ", lookupLabel))
    }
    character()
  }
  checkVectorFK <- function(value, argName, lookup, lookupLabel) {
    if (is.null(value)) {
      return(character())
    }
    if (
      !is.character(value) ||
        length(value) == 0L ||
        any(is.na(value)) ||
        any(nchar(value) == 0)
    ) {
      return(paste0(
        argName,
        " must be a non-empty character vector with no NA or empty entries"
      ))
    }
    bad <- setdiff(value, names(lookup))
    if (length(bad) > 0L) {
      return(paste0(
        argName,
        " not found in ",
        lookupLabel,
        ": ",
        paste(bad, collapse = ", ")
      ))
    }
    character()
  }

  errors <- c(
    errors,
    checkScalarFK(
      individualId,
      "individualId",
      project$individuals,
      "individuals"
    ),
    checkScalarFK(
      populationId,
      "populationId",
      project$populations,
      "populations"
    ),
    checkScalarFK(
      applicationProtocol,
      "applicationProtocol",
      project$applications,
      "applications"
    ),
    checkVectorFK(
      modelParameterSets,
      "modelParameterSets",
      project$modelParameterSets,
      "project$modelParameterSets"
    ),
    checkVectorFK(
      outputPathIds,
      "outputPathIds",
      project$outputPaths,
      "outputPaths"
    )
  )

  if (length(errors) > 0L) {
    cli::cli_abort(c(
      "Cannot add scenario {.val {scenarioName}}:",
      stats::setNames(errors, rep("x", length(errors)))
    ))
  }

  sc <- Scenario(
    scenarioName = scenarioName,
    modelFile = modelFile,
    applicationProtocol = applicationProtocol %||% NA,
    individualId = individualId,
    populationId = populationId,
    outputPaths = if (!is.null(outputPathIds)) {
      stats::setNames(
        unlist(project$outputPaths[outputPathIds], use.names = FALSE),
        outputPathIds
      )
    },
    readPopulationFromCSV = readPopulationFromCSV,
    simulateSteadyState = steadyState,
    simulationTime = if (!is.null(simulationTime)) {
      .parseSimulationTimeIntervals(simulationTime)
    },
    simulationTimeUnit = simulationTimeUnit,
    # The field contract stores steadyStateTime in the base unit (minutes);
    # convert from the user-declared unit so a non-minute unit round-trips
    # correctly (the serializer converts back to steadyStateTimeUnit).
    steadyStateTime = ospsuite::toBaseUnit(
      quantityOrDimension = ospDimensions$Time,
      values = steadyStateTime,
      unit = steadyStateTimeUnit
    ),
    steadyStateTimeUnit = steadyStateTimeUnit,
    overwriteFormulasInSS = overwriteFormulasInSS,
    modelParameterSets = modelParameterSets
  )

  project$scenarios[[scenarioName]] <- sc
  project$.markModified()

  invisible(project)
}

#' Remove a scenario from a Project
#' @param project A `Project` object.
#' @param name Character scalar, scenario name.
#' @returns The `project` object, invisibly.
#' @export
#' @family scenario
removeScenario <- function(project, name) {
  validateIsOfType(project, "Project")
  if (
    !is.character(name) ||
      length(name) != 1L ||
      is.na(name) ||
      nchar(name) == 0
  ) {
    cli::cli_abort("{.arg name} must be a non-empty string")
  }
  if (!(name %in% names(project$scenarios))) {
    cli::cli_warn("scenario {.val {name}} not found; no-op.")
    return(invisible(project))
  }
  project$scenarios[[name]] <- NULL
  project$.markModified()
  invisible(project)
}
