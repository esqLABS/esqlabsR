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
    sc <- Scenario$new()
    sc$scenarioName <- entry$name
    sc$modelFile <- entry$modelFile
    sc$applicationProtocol <- entry$applicationProtocol %||% NA
    sc$individualId <- entry$individualId

    if (!is.null(entry$populationId)) {
      sc$populationId <- entry$populationId
      sc$simulationType <- "Population"
    }
    if (!is.null(entry$readPopulationFromCSV)) {
      sc$readPopulationFromCSV <- entry$readPopulationFromCSV
    }
    if (!is.null(entry$modelParameterSets)) {
      sc$modelParameterSets <- unlist(entry$modelParameterSets)
    }
    if (!is.null(entry$simulationTime)) {
      sc$simulationTime <- .parseSimulationTimeIntervals(
        entry$simulationTime
      )
      sc$simulationTimeUnit <- entry$simulationTimeUnit
    }
    if (isTRUE(entry$steadyState)) {
      sc$simulateSteadyState <- TRUE
    }
    if (!is.null(entry$steadyStateTime)) {
      if (is.null(entry$steadyStateTimeUnit)) {
        cli::cli_abort(
          "Scenario {.val {entry$name}} has {.field steadyStateTime} set \\
          but {.field steadyStateTimeUnit} is null. Please specify a unit \\
          (e.g. {.val min})."
        )
      }
      sc$steadyStateTime <- ospsuite::toBaseUnit(
        quantityOrDimension = ospDimensions$Time,
        values = entry$steadyStateTime,
        unit = entry$steadyStateTimeUnit
      )
      sc$steadyStateTimeUnit <- entry$steadyStateTimeUnit
    }
    if (!is.null(entry$overwriteFormulasInSS)) {
      sc$overwriteFormulasInSS <- entry$overwriteFormulasInSS
    }

    if (!is.null(entry$outputPathIds)) {
      pathIds <- unlist(entry$outputPathIds)
      unknown <- setdiff(pathIds, names(outputPaths))
      if (length(unknown) > 0) {
        cli::cli_abort(
          "Scenario {.val {entry$name}} references unknown outputPathIds: \\
          {.val {unknown}}."
        )
      }
      sc$outputPaths <- setNames(
        unlist(outputPaths[pathIds], use.names = FALSE),
        pathIds
      )
    }

    result[[entry$name]] <- sc
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
  .validateScenarios(project$scenarios, project$modelFolder)
}

#' @keywords internal
#' @noRd
.applicationsValidatorAdapter <- function(project) {
  .validateApplications(project$applications)
}

#' Validate the `scenarios` section of a Project
#'
#' Per-entry checks: `modelFile` is set and non-empty, resolves on disk
#' (warning), `simulationType` is one of the supported values, and
#' population-typed scenarios declare a `populationId`.
#'
#' Cross-section reference checks (individualId, modelParameterSets,
#' applicationProtocol, …) live in `.validateCrossReferences()`.
#'
#' @param scenarios Named list of `Scenario` objects from
#'   `project$scenarios`.
#' @param modelFolder Character. Absolute path to the project's model folder,
#'   used to resolve relative `modelFile` paths. May be `NULL`.
#' @return validationResult.
#' @keywords internal
#' @noRd
.validateScenarios <- function(scenarios, modelFolder = NULL) {
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
    } else if (!is.null(modelFolder)) {
      modelFilePath <- file.path(modelFolder, sc$modelFile)
      if (!file.exists(modelFilePath)) {
        result$add_warning(
          "File Not Found",
          paste0(
            "Scenario '",
            name,
            "' references non-existent modelFile: ",
            sc$modelFile
          )
        )
      }
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

  sc <- Scenario$new()
  sc$scenarioName <- scenarioName
  sc$modelFile <- modelFile
  sc$individualId <- individualId
  sc$applicationProtocol <- applicationProtocol %||% NA

  if (!is.null(populationId)) {
    sc$populationId <- populationId
    sc$simulationType <- "Population"
  }

  sc$modelParameterSets <- modelParameterSets
  sc$readPopulationFromCSV <- readPopulationFromCSV

  if (!is.null(outputPathIds)) {
    sc$outputPaths <- setNames(
      unlist(project$outputPaths[outputPathIds], use.names = FALSE),
      outputPathIds
    )
  }

  if (!is.null(simulationTime)) {
    sc$simulationTime <- .parseSimulationTimeIntervals(simulationTime)
    sc$simulationTimeUnit <- simulationTimeUnit
  }

  sc$simulateSteadyState <- steadyState
  sc$steadyStateTime <- steadyStateTime
  sc$steadyStateTimeUnit <- steadyStateTimeUnit
  sc$overwriteFormulasInSS <- overwriteFormulasInSS

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
