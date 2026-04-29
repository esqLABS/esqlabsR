# Applications section: parse + validate + serialize + mutation.
# Owns Project$applications end-to-end. Called by:
#   - Project$.read_json() via .parseApplications()
#   - .runProjectValidation() via .validateApplications()
#   - .projectToJson() via .applicationsToJson()
#   - users via the public addApplication / removeApplication /
#     addApplicationParameter / removeApplicationParameter functions.

# Parse ----

#' @keywords internal
#' @noRd
.parseApplications <- function(appsData) {
  if (is.null(appsData)) {
    return(list())
  }
  result <- list()
  for (id in names(appsData)) {
    entry <- appsData[[id]]
    pset <- NULL
    if (!is.null(entry$parameters)) {
      for (p in entry$parameters) {
        pset <- .addParameterEntry(
          pset,
          p$containerPath,
          p$parameterName,
          p$value,
          p$units %||% ""
        )
      }
    }
    app <- list(parameters = pset)
    class(app) <- c("Application", "list")
    result[[id]] <- app
  }
  result
}

# Validate ----

#' Validate applications section of a Project
#' @param applications Named list from project$applications
#' @return validationResult object
#' @keywords internal
.validateApplications <- function(applications) {
  .validateParameterGroups(applications, "applications")
}

#' @keywords internal
#' @noRd
.applicationsValidatorAdapter <- function(project) {
  .validateApplications(project$applications)
}

# Serialize ----

#' @keywords internal
#' @noRd
.applicationsToJson <- function(applications) {
  if (is.null(applications) || length(applications) == 0) {
    return(structure(list(), names = character(0)))
  }
  result <- list()
  for (id in names(applications)) {
    pset <- applications[[id]]$parameters
    params <- list()
    if (!is.null(pset) && length(pset$paths) > 0) {
      params <- vector("list", length(pset$paths))
      for (j in seq_along(pset$paths)) {
        split <- .splitParameterPathIntoContainerAndName(pset$paths[[j]])
        params[[j]] <- list(
          containerPath = split$containerPath,
          parameterName = split$parameterName,
          value = pset$values[[j]],
          units = pset$units[[j]]
        )
      }
    }
    result[[id]] <- list(parameters = params)
  }
  result
}

# Public CRUD ----

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
  project$.markModified()
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
  project$.markModified()
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
  project$.markModified()
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
  project$.markModified()
  invisible(project)
}
