#' Add an application protocol to a Project
#'
#' @param project A `Project` object.
#' @param applicationId Character scalar, unique protocol id.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family application
addApplication <- function(project, applicationId) {
  validateIsOfType(project, "Project")
  if (
    !is.character(applicationId) ||
      length(applicationId) != 1 ||
      is.na(applicationId) ||
      nchar(applicationId) == 0
  ) {
    stop("applicationId must be a non-empty string")
  }
  if (applicationId %in% names(project$applications)) {
    stop(paste0("application '", applicationId, "' already exists"))
  }
  app <- list(parameters = NULL)
  class(app) <- c("Application", "list")
  project$applications[[applicationId]] <- app
  project$modified <- TRUE
  invisible(project)
}

#' Remove an application protocol from a Project
#'
#' @param project A `Project` object.
#' @param applicationId Character scalar.
#' @returns The `project` object, invisibly.
#' @export
#' @family application
removeApplication <- function(project, applicationId) {
  validateIsOfType(project, "Project")
  if (!is.character(applicationId) || length(applicationId) != 1) {
    stop("applicationId must be a string scalar")
  }
  if (!(applicationId %in% names(project$applications))) {
    cli::cli_warn("application {.val {applicationId}} not found; no-op.")
    return(invisible(project))
  }
  .warnIfReferenced(project, "application", applicationId)
  project$applications[[applicationId]] <- NULL
  project$modified <- TRUE
  invisible(project)
}

#' Add a parameter to a named application
#'
#' @param project A `Project` object.
#' @param applicationId Character scalar.
#' @param containerPath Character scalar.
#' @param parameterName Character scalar.
#' @param value Numeric scalar.
#' @param units Character scalar.
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
addApplicationParameter <- function(
  project,
  applicationId,
  containerPath,
  parameterName,
  value,
  units
) {
  validateIsOfType(project, "Project")
  if (!(applicationId %in% names(project$applications))) {
    stop(paste0("application '", applicationId, "' not found"))
  }
  project$applications[[applicationId]] <- addParameter(
    project$applications[[applicationId]],
    containerPath = containerPath,
    parameterName = parameterName,
    value = value,
    units = units
  )
  project$modified <- TRUE
  invisible(project)
}

#' Remove a parameter from a named application
#'
#' @inheritParams addApplicationParameter
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
removeApplicationParameter <- function(
  project,
  applicationId,
  containerPath,
  parameterName
) {
  validateIsOfType(project, "Project")
  if (!(applicationId %in% names(project$applications))) {
    stop(paste0("application '", applicationId, "' not found"))
  }
  project$applications[[applicationId]] <- removeParameter(
    project$applications[[applicationId]],
    containerPath = containerPath,
    parameterName = parameterName
  )
  project$modified <- TRUE
  invisible(project)
}
