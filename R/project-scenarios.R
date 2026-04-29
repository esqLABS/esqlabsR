# Public CRUD: scenarios and output paths ----

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

#' Add output paths to a Project
#'
#' @param project A `Project` object.
#' @param id Character vector of output path IDs (unique within the call
#'   and not already present in `project$outputPaths`).
#' @param path Character vector of output paths, same length as `id`.
#' @returns The `project` object, invisibly.
#' @export
#' @family scenario
addOutputPath <- function(project, id, path) {
  validateIsOfType(project, "Project")
  errors <- character()

  if (
    !is.character(id) || length(id) < 1 || any(is.na(id)) || any(nchar(id) == 0)
  ) {
    errors <- c(errors, "id must be a non-empty character vector")
  }
  if (!is.character(path) || length(path) != length(id)) {
    errors <- c(
      errors,
      "id and path must be character vectors of the same length"
    )
  }
  if (is.character(id) && any(duplicated(id))) {
    errors <- c(
      errors,
      paste0(
        "duplicate ids within call: ",
        paste(unique(id[duplicated(id)]), collapse = ", ")
      )
    )
  }
  if (is.character(id)) {
    collisions <- intersect(id, names(project$outputPaths))
    if (length(collisions) > 0) {
      errors <- c(
        errors,
        paste0(
          "outputPath id already exists: ",
          paste(collisions, collapse = ", ")
        )
      )
    }
  }

  if (length(errors) > 0) {
    stop(paste0(
      "Cannot add outputPath:\n- ",
      paste(errors, collapse = "\n- ")
    ))
  }

  newPaths <- path
  names(newPaths) <- id
  project$outputPaths <- c(project$outputPaths, newPaths)
  project$.markModified()
  invisible(project)
}

#' Remove an output path from a Project
#' @param project A `Project` object.
#' @param id Character scalar.
#' @returns The `project` object, invisibly.
#' @export
#' @family scenario
removeOutputPath <- function(project, id) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1 || is.na(id) || nchar(id) == 0) {
    stop("id must be a non-empty string")
  }
  if (!(id %in% names(project$outputPaths))) {
    cli::cli_warn("outputPath {.val {id}} not found; no-op.")
    return(invisible(project))
  }
  .warnIfReferenced(project, "outputPath", id)
  project$outputPaths <- project$outputPaths[setdiff(
    names(project$outputPaths),
    id
  )]
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
