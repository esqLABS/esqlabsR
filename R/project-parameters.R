# Public CRUD: model parameters and inline parameters ----

#' Add a parameter to a named model-parameter set
#'
#' @description Adds one parameter entry to the named set in
#' `project$modelParameters`. The set is created on demand if it does not
#' yet exist. Last-write-wins on duplicate paths.
#'
#' @param project A `Project` object.
#' @param id Character scalar, set name. Created if not present.
#' @param containerPath Character scalar.
#' @param parameterName Character scalar.
#' @param value Numeric scalar.
#' @param units Character scalar.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
addModelParameter <- function(
  project,
  id,
  containerPath,
  parameterName,
  value,
  units
) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1 || is.na(id) || nchar(id) == 0) {
    stop("id must be a non-empty string")
  }
  current <- project$modelParameters[[id]]
  project$modelParameters[[id]] <- .addParameterEntry(
    current,
    containerPath,
    parameterName,
    value,
    units
  )
  project$.markModified()
  invisible(project)
}

#' Remove a parameter from a named model-parameter set
#'
#' @description Removes one parameter entry from the named set. If the
#' removed entry was the last in the set, the set itself is auto-removed
#' from `project$modelParameters`. Warns if the set or entry doesn't exist.
#'
#' @param project A `Project` object.
#' @param id Character scalar, set name.
#' @param containerPath Character scalar.
#' @param parameterName Character scalar.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
removeModelParameter <- function(project, id, containerPath, parameterName) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1) {
    stop("id must be a string scalar")
  }
  if (!(id %in% names(project$modelParameters))) {
    cli::cli_warn("model parameter set {.val {id}} not found; no-op.")
    return(invisible(project))
  }
  updated <- .removeParameterEntry(
    project$modelParameters[[id]],
    containerPath,
    parameterName
  )
  if (is.null(updated)) {
    .warnIfReferenced(project, "modelParameterSet", id)
    project$modelParameters[[id]] <- NULL
  } else {
    project$modelParameters[[id]] <- updated
  }
  project$.markModified()
  invisible(project)
}

#' Add a parameter to a named individual
#'
#' @description Convenience wrapper around `addParameter()` that looks up the
#' individual by id, dispatches `addParameter`, and writes the result back.
#' Errors if the id doesn't resolve.
#'
#' @param project A `Project` object.
#' @param individualId Character scalar.
#' @param containerPath Character scalar.
#' @param parameterName Character scalar.
#' @param value Numeric scalar.
#' @param units Character scalar.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
addIndividualParameter <- function(
  project,
  individualId,
  containerPath,
  parameterName,
  value,
  units
) {
  validateIsOfType(project, "Project")
  if (!(individualId %in% names(project$individuals))) {
    stop(paste0("individual '", individualId, "' not found"))
  }
  project$individuals[[individualId]] <- addParameter(
    project$individuals[[individualId]],
    containerPath = containerPath,
    parameterName = parameterName,
    value = value,
    units = units
  )
  project$.markModified()
  invisible(project)
}

#' Remove a parameter from a named individual
#'
#' @inheritParams addIndividualParameter
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
removeIndividualParameter <- function(
  project,
  individualId,
  containerPath,
  parameterName
) {
  validateIsOfType(project, "Project")
  if (!(individualId %in% names(project$individuals))) {
    stop(paste0("individual '", individualId, "' not found"))
  }
  project$individuals[[individualId]] <- removeParameter(
    project$individuals[[individualId]],
    containerPath = containerPath,
    parameterName = parameterName
  )
  project$.markModified()
  invisible(project)
}
