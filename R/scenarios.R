# Scenarios section: parse + validate + serialize + mutation.
#
# Owns Project$scenarios end-to-end. Scenarios reference Project$outputPaths
# by id (`outputPathIds` in JSON); the parser/serializer needs the
# outputPaths lookup table to dereference/re-reference, but outputPaths
# itself is owned by R/output-paths.R.
#
# Called by:
#   - Project$.read_json() via .parseScenarios()
#   - .runProjectValidation() via .validateScenarios()
#   - .projectToJson() via .scenariosToJson()
#   - users via the public addScenario / removeScenario functions.

# Parse ----

#' @keywords internal
#' @noRd
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
    if (!is.null(entry$modelParameters)) {
      sc$modelParameters <- unlist(entry$modelParameters)
    }
    if (!is.null(entry$simulationTime)) {
      sc$simulationTime <- .parseSimulationTimeIntervals(
        entry$simulationTime
      )
      sc$simulationTimeUnit <- entry$simulationTimeUnit
    }
    if (!is.null(entry$steadyState) && isTRUE(entry$steadyState)) {
      sc$simulateSteadyState <- TRUE
    }
    if (!is.null(entry$steadyStateTime)) {
      if (is.null(entry$steadyStateTimeUnit)) {
        stop(
          "Scenario '",
          entry$name,
          "' has 'steadyStateTime' set but ",
          "'steadyStateTimeUnit' is null. Please specify a unit (e.g. \"min\")."
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
        stop(
          "Scenario '",
          entry$name,
          "' references unknown outputPathIds: ",
          paste(unknown, collapse = ", ")
        )
      }
      sc$outputPaths <- unname(outputPaths[pathIds])
    }
    result[[entry$name]] <- sc
  }
  result
}

# Validate ----

#' Validate scenarios section of a Project
#' @param scenarios Named list of Scenario objects from project$scenarios
#' @return validationResult object
#' @keywords internal
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

    sim_type <- sc$simulationType %||% ""
    if (!sim_type %in% c("Individual", "Population")) {
      result$add_critical_error(
        "Validation",
        paste0(
          "Scenario '",
          name,
          "' has invalid simulationType '",
          sim_type,
          "'"
        )
      )
    }

    if (
      sim_type == "Population" &&
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

# Serialize ----

#' @keywords internal
#' @noRd
.scenariosToJson <- function(scenarios, outputPaths) {
  if (is.null(scenarios) || length(scenarios) == 0) {
    return(list())
  }

  lapply(scenarios, function(sc) {
    outputPathIds <- NULL
    if (
      !is.null(sc$outputPaths) &&
        length(sc$outputPaths) > 0 &&
        !is.null(outputPaths)
    ) {
      outputPathIds <- names(outputPaths)[match(sc$outputPaths, outputPaths)]
      outputPathIds <- outputPathIds[!is.na(outputPathIds)]
      if (length(outputPathIds) == 0) outputPathIds <- NULL
    }

    simTimeStr <- NULL
    if (!is.null(sc$simulationTime)) {
      intervals <- vapply(
        sc$simulationTime,
        function(int) {
          paste(int, collapse = ", ")
        },
        character(1)
      )
      simTimeStr <- paste(intervals, collapse = "; ")
    }

    list(
      name = sc$scenarioName,
      individualId = sc$individualId,
      populationId = if (sc$simulationType == "Population") {
        sc$populationId
      } else {
        NULL
      },
      readPopulationFromCSV = sc$readPopulationFromCSV,
      modelParameters = as.list(sc$modelParameters),
      applicationProtocol = if (
        is.null(sc$applicationProtocol) || is.na(sc$applicationProtocol)
      ) {
        NULL
      } else {
        sc$applicationProtocol
      },
      simulationTime = simTimeStr,
      simulationTimeUnit = sc$simulationTimeUnit,
      steadyState = sc$simulateSteadyState,
      steadyStateTime = if (
        sc$simulateSteadyState && !is.null(sc$steadyStateTime)
      ) {
        ospsuite::toUnit(
          quantityOrDimension = ospsuite::ospDimensions$Time,
          values = sc$steadyStateTime,
          targetUnit = sc$steadyStateTimeUnit %||% "min"
        )
      } else {
        NULL
      },
      steadyStateTimeUnit = if (sc$simulateSteadyState) {
        sc$steadyStateTimeUnit
      } else {
        NULL
      },
      overwriteFormulasInSS = sc$overwriteFormulasInSS,
      modelFile = sc$modelFile,
      outputPathIds = if (!is.null(outputPathIds)) {
        as.list(outputPathIds)
      } else {
        NULL
      }
    )
  })
}

# Public CRUD ----

#' Add a scenario programmatically to a Project
#'
#' @description Creates a new `Scenario` and adds it to the
#'   `project$scenarios` list after validating all references.
#'
#' @param project A `Project` object.
#' @param scenarioName Character. Name for the new scenario. Must not already
#'   exist in `project$scenarios`.
#' @param modelFile Character. Name of the `.pkml` model file (relative to
#'   model folder).
#' @param individualId Character or NULL. ID referencing
#'   `project$individuals`.
#' @param populationId Character or NULL. ID referencing
#'   `project$populations`.
#' @param applicationProtocol Character or NULL. Protocol name referencing
#'   `project$applications`.
#' @param modelParameters Character vector or NULL. Group names referencing
#'   `project$modelParameters`.
#' @param outputPathIds Character vector or NULL. IDs referencing
#'   `project$outputPaths`.
#' @param simulationTime Character or NULL. Format `"start, end, resolution"`
#'   or `"start, end, resolution; start, end, resolution"` for multiple
#'   intervals.
#' @param simulationTimeUnit Character. Time unit string. Default `"h"`.
#' @param steadyState Logical. Whether to simulate steady state. Default
#'   `FALSE`.
#' @param steadyStateTime Numeric. Steady-state time in minutes. Default
#'   `1000`.
#' @param overwriteFormulasInSS Logical. Overwrite formulas during steady
#'   state. Default `FALSE`.
#' @param readPopulationFromCSV Logical. Load population from CSV. Default
#'   `FALSE`.
#'
#' @returns The `project` object, invisibly.
#'
#' @export
#' @family scenario
addScenario <- function(
  project,
  scenarioName,
  modelFile,
  individualId = NULL,
  populationId = NULL,
  applicationProtocol = NULL,
  modelParameters = NULL,
  outputPathIds = NULL,
  simulationTime = NULL,
  simulationTimeUnit = "h",
  steadyState = FALSE,
  steadyStateTime = 1000,
  overwriteFormulasInSS = FALSE,
  readPopulationFromCSV = FALSE
) {
  validateIsOfType(project, "Project")
  project <- project
  errors <- character()

  # Validate required args
  if (
    !is.character(scenarioName) ||
      length(scenarioName) != 1 ||
      is.na(scenarioName) ||
      nchar(scenarioName) == 0
  ) {
    errors <- c(errors, "scenarioName must be a non-empty string")
  } else if (scenarioName %in% names(project$scenarios)) {
    errors <- c(errors, paste0("scenario '", scenarioName, "' already exists"))
  }

  if (
    !is.character(modelFile) ||
      length(modelFile) != 1 ||
      is.na(modelFile) ||
      nchar(modelFile) == 0
  ) {
    errors <- c(errors, "modelFile must be a non-empty string")
  }

  # Validate optional references
  if (
    !is.null(individualId) && !(individualId %in% names(project$individuals))
  ) {
    errors <- c(
      errors,
      paste0("individualId '", individualId, "' not found in individuals")
    )
  }
  if (
    !is.null(populationId) && !(populationId %in% names(project$populations))
  ) {
    errors <- c(
      errors,
      paste0("populationId '", populationId, "' not found in populations")
    )
  }
  if (
    !is.null(applicationProtocol) &&
      !(applicationProtocol %in% names(project$applications))
  ) {
    errors <- c(
      errors,
      paste0(
        "applicationProtocol '",
        applicationProtocol,
        "' not found in applications"
      )
    )
  }
  if (!is.null(modelParameters)) {
    bad <- setdiff(modelParameters, names(project$modelParameters))
    if (length(bad) > 0) {
      errors <- c(
        errors,
        paste0(
          "modelParameters not found in modelParameters: ",
          paste(bad, collapse = ", ")
        )
      )
    }
  }
  if (!is.null(outputPathIds)) {
    bad <- setdiff(outputPathIds, names(project$outputPaths))
    if (length(bad) > 0) {
      errors <- c(
        errors,
        paste0(
          "outputPathIds not found in outputPaths: ",
          paste(bad, collapse = ", ")
        )
      )
    }
  }

  if (length(errors) > 0) {
    stop(paste0(
      "Cannot add scenario '",
      scenarioName,
      "':\n- ",
      paste(errors, collapse = "\n- ")
    ))
  }

  # Build Scenario object
  sc <- Scenario$new()
  sc$scenarioName <- scenarioName
  sc$modelFile <- modelFile
  sc$individualId <- individualId
  sc$applicationProtocol <- applicationProtocol %||% NA

  if (!is.null(populationId)) {
    sc$populationId <- populationId
    sc$simulationType <- "Population"
  }

  sc$modelParameters <- modelParameters
  sc$readPopulationFromCSV <- readPopulationFromCSV

  if (!is.null(outputPathIds)) {
    sc$outputPaths <- unname(project$outputPaths[outputPathIds])
  }

  if (!is.null(simulationTime)) {
    sc$simulationTime <- .parseSimulationTimeIntervals(simulationTime)
    sc$simulationTimeUnit <- simulationTimeUnit
  }

  sc$simulateSteadyState <- steadyState
  sc$steadyStateTime <- steadyStateTime
  sc$overwriteFormulasInSS <- overwriteFormulasInSS

  # Add to configuration
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
    !is.character(name) || length(name) != 1 || is.na(name) || nchar(name) == 0
  ) {
    stop("name must be a non-empty string")
  }
  if (!(name %in% names(project$scenarios))) {
    cli::cli_warn("scenario {.val {name}} not found; no-op.")
    return(invisible(project))
  }
  project$scenarios[[name]] <- NULL
  project$.markModified()
  invisible(project)
}

