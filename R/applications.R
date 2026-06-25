# Applications: parse + mutation API for the `applications` and
# `applicationParameterSets` sections.

# Parse ----
#
# Parse the `applications` JSON object. Each entry is stamped with
# `class = c("Application", "list")`. The current schema stores
# applications as a map of name -> object containing only
# `parameterSets`. The map is preserved verbatim except for the class
# attribute and a coercion of `parameterSets` to character.
#
# @keywords internal
# @noRd
.parseApplications <- function(appsData) {
  if (is.null(appsData) || length(appsData) == 0L) {
    return(structure(list(), names = character(0L)))
  }
  result <- list()
  for (id in names(appsData)) {
    entry <- appsData[[id]]
    app <- list()
    if (!is.null(entry$parameterSets)) {
      app$parameterSets <- as.character(unlist(entry$parameterSets))
    }
    class(app) <- c("Application", "list")
    result[[id]] <- app
  }
  result
}

# Public CRUD: applications ----

#' Add an application protocol to a Project
#'
#' @param project A `Project` object.
#' @param applicationId Character scalar, unique protocol id.
#' @param parameterSets Optional character vector of set ids referencing
#'   `project$applicationParameterSets`. Defaults to `NULL`.
#' @returns The `project` object, invisibly.
#' @export
#' @family application
addApplication <- function(project, applicationId, parameterSets = NULL) {
  validateIsOfType(project, "Project")
  if (
    !is.character(applicationId) ||
      length(applicationId) != 1L ||
      is.na(applicationId) ||
      nchar(applicationId) == 0
  ) {
    cli::cli_abort("{.arg applicationId} must be a non-empty string")
  }
  if (applicationId %in% names(project$applications)) {
    cli::cli_abort("application {.val {applicationId}} already exists")
  }
  app <- list()
  if (!is.null(parameterSets)) {
    if (!is.character(parameterSets)) {
      cli::cli_abort(
        "{.arg parameterSets} must be a character vector of set ids"
      )
    }
    bad <- setdiff(
      parameterSets,
      names(project$applicationParameterSets %||% list())
    )
    if (length(bad) > 0L) {
      cli::cli_abort(c(
        "{.arg parameterSets} references undefined application parameter sets:",
        "x" = "{.val {bad}}"
      ))
    }
    app$parameterSets <- parameterSets
  }
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
  if (
    !is.character(applicationId) ||
      length(applicationId) != 1L ||
      is.na(applicationId) ||
      nchar(applicationId) == 0
  ) {
    cli::cli_abort("{.arg applicationId} must be a non-empty string")
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

#' Replace the parameter-set references on an application
#'
#' @param project A `Project` object.
#' @param applicationId Character scalar.
#' @param parameterSets Character vector of set ids (from
#'   `project$applicationParameterSets`). Use `character(0)` to clear.
#' @returns The `project` object, invisibly.
#' @export
#' @family application
setApplicationParameterSets <- function(
  project,
  applicationId,
  parameterSets
) {
  validateIsOfType(project, "Project")
  if (!(applicationId %in% names(project$applications))) {
    cli::cli_abort("application {.val {applicationId}} not found")
  }
  if (!is.character(parameterSets)) {
    cli::cli_abort("{.arg parameterSets} must be a character vector")
  }
  bad <- setdiff(
    parameterSets,
    names(project$applicationParameterSets %||% list())
  )
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "{.arg parameterSets} references undefined application parameter sets:",
      "x" = "{.val {bad}}"
    ))
  }
  project$applications[[applicationId]]$parameterSets <- parameterSets
  project$.markModified()
  invisible(project)
}

# Public CRUD: applicationParameterSets ----

#' Create an application parameter set
#' @param project A `Project` object.
#' @param id Character scalar, set name. Must not already exist.
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
addApplicationParameterSet <- function(project, id) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1L || is.na(id) || nchar(id) == 0) {
    cli::cli_abort("{.arg id} must be a non-empty string")
  }
  if (id %in% names(project$applicationParameterSets)) {
    cli::cli_abort("application parameter set {.val {id}} already exists")
  }
  project$applicationParameterSets[[id]] <- list()
  project$.markModified()
  invisible(project)
}

#' Remove an application parameter set
#' @inheritParams addApplicationParameterSet
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
removeApplicationParameterSet <- function(project, id) {
  validateIsOfType(project, "Project")
  if (!(id %in% names(project$applicationParameterSets))) {
    cli::cli_warn("application parameter set {.val {id}} not found; no-op.")
    return(invisible(project))
  }
  .warnIfReferenced(project, "applicationParameterSet", id)
  project$applicationParameterSets[[id]] <- NULL
  project$.markModified()
  invisible(project)
}

#' Add a parameter entry to a named application parameter set
#'
#' Adds one parameter entry to the named set in
#' `project$applicationParameterSets`. The set is created on demand if
#' it does not yet exist. Last-write-wins on duplicate `(containerPath,
#' parameterName)` pairs.
#'
#' @param project A `Project` object.
#' @param id Character scalar, set name. Created if not present.
#' @param containerPath Character scalar.
#' @param parameterName Character scalar.
#' @param value Numeric scalar.
#' @param units Character scalar.
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
addApplicationParameterEntry <- function(
  project,
  id,
  containerPath,
  parameterName,
  value,
  units
) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1L || is.na(id) || nchar(id) == 0) {
    cli::cli_abort("{.arg id} must be a non-empty string")
  }
  current <- project$applicationParameterSets[[id]]
  project$applicationParameterSets[[id]] <- .addParameterEntry(
    current,
    containerPath,
    parameterName,
    value,
    units
  )
  project$.markModified()
  invisible(project)
}

#' Remove a parameter entry from a named application parameter set
#'
#' Removes one parameter entry from the named set. If the removed entry
#' was the last in the set, the set itself is auto-removed from
#' `project$applicationParameterSets`. Warns if the set or entry doesn't
#' exist.
#'
#' @inheritParams addApplicationParameterEntry
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
removeApplicationParameterEntry <- function(
  project,
  id,
  containerPath,
  parameterName
) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1L) {
    cli::cli_abort("{.arg id} must be a string scalar")
  }
  if (!(id %in% names(project$applicationParameterSets))) {
    cli::cli_warn("application parameter set {.val {id}} not found; no-op.")
    return(invisible(project))
  }
  result <- .removeParameterEntry(
    project$applicationParameterSets[[id]],
    containerPath,
    parameterName
  )
  if (!result$removed) {
    return(invisible(project))
  }
  if (is.null(result$parameters)) {
    .warnIfReferenced(project, "applicationParameterSet", id)
    project$applicationParameterSets[[id]] <- NULL
  } else {
    project$applicationParameterSets[[id]] <- result$parameters
  }
  project$.markModified()
  invisible(project)
}
