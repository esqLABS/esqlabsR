# Applications: parse + mutation API for the `applications` and
# `parameterSets` references on applications.

# Print ----

#' @exportS3Method
#' @noRd
print.Application <- function(x, ...) {
  ospsuite.utils::ospPrintClass(x)
  ospsuite.utils::ospPrintItems(
    list(
      "Parameter Sets" = paste(x$parameterSets, collapse = ", ")
    ),
    print_empty = TRUE
  )
  invisible(x)
}

# Public CRUD: applications ----

#' Add one or more application protocols to a Project
#'
#' Add protocols to `project$applications`, vectorizing over a vector of ids
#' (see the recycling rule under Details). `parameterSets` is
#' vector-valued-per-entity: it is applied whole to every protocol; to give a
#' different set per protocol, pass a list of the same length as `id` (one
#' character vector per protocol).
#'
#' @inherit vectorizedAuthoring details
#'
#' @param project A `Project` object.
#' @param id Character vector of unique protocol ids (the number of protocols
#'   to add). Each is canonicalized to a safe, lowercase id (a warning names
#'   the result if it changed).
#' @param parameterSets Optional character vector of set ids referencing
#'   `project$parameterSets`, applied whole to every protocol. Defaults to
#'   `NULL`. Use a list of the same length as `id` for a per-protocol set.
#' @returns The `project` object, invisibly.
#' @export
#' @family application
addApplication <- function(project, id, parameterSets = NULL) {
  validateIsOfType(project, "Project")
  .assertIdVector(id)
  id <- .canonicalizeId(id)
  n <- length(id)
  perId <- .wholeField(parameterSets, n)

  clash <- intersect(id, names(project$applications))
  if (length(clash) > 0L) {
    cli::cli_abort("application {.val {clash}} already exists")
  }
  call <- rlang::current_env()
  apps <- .collectCanonicalizedRefs(lapply(seq_len(n), function(i) {
    .buildApplicationEntry(project, perId[[i]], call = call)
  }))

  applications <- project$.getSection("applications") %||% list()
  for (i in seq_len(n)) {
    applications[[id[[i]]]] <- apps[[i]]
  }
  project$.setSection("applications", applications)
  invisible(project)
}

#' Remove one or more application protocols from a Project
#'
#' Drop the protocols with matching ids in one write-through. Warns (and
#' skips) any id not present, and warns when a removed protocol is still
#' referenced.
#'
#' @param project A `Project` object.
#' @param id Character vector of application ids to remove. Each is
#'   canonicalized the same way [addApplication()] canonicalizes it.
#' @returns The `project` object, invisibly.
#' @export
#' @family application
removeApplication <- function(project, id) {
  validateIsOfType(project, "Project")
  .assertIdVector(id)
  id <- .canonicalizeId(id)

  missingIds <- setdiff(id, names(project$applications))
  if (length(missingIds) > 0L) {
    cli::cli_warn("application {.val {missingIds}} not found; no-op.")
  }
  toRemove <- intersect(id, names(project$applications))
  if (length(toRemove) == 0L) {
    return(invisible(project))
  }
  for (one in toRemove) {
    .warnIfReferenced(project, "application", one)
  }
  applications <- project$.getSection("applications")
  applications[toRemove] <- NULL
  project$.setSection("applications", applications)
  invisible(project)
}

#' Replace the parameter-set references on one or more applications
#'
#' @inherit vectorizedAuthoring details
#'
#' @param project A `Project` object.
#' @param id Character vector of application ids. Each is canonicalized the
#'   same way [addApplication()] canonicalizes it.
#' @param parameterSets Character vector of set ids (from
#'   `project$parameterSets`), applied whole to every application; use
#'   `character(0)` to clear. To set a different list per application, pass a
#'   list of the same length as `id` (one character vector per application).
#' @returns The `project` object, invisibly.
#' @export
#' @family application
setApplicationParameterSets <- function(
  project,
  id,
  parameterSets
) {
  validateIsOfType(project, "Project")
  .assertIdVector(id)
  id <- .canonicalizeId(id)
  n <- length(id)
  missingIds <- setdiff(id, names(project$applications))
  if (length(missingIds) > 0L) {
    cli::cli_abort("application {.val {missingIds}} not found")
  }
  perId <- .wholeField(parameterSets, n)

  call <- rlang::current_env()
  resolved <- .collectCanonicalizedRefs(lapply(seq_len(n), function(i) {
    sets <- perId[[i]]
    if (!is.character(sets)) {
      cli::cli_abort(
        "{.arg parameterSets} must be a character vector",
        call = call
      )
    }
    sets <- .canonicalizeIdRef(sets)
    bad <- setdiff(sets, names(project$parameterSets %||% list()))
    if (length(bad) > 0L) {
      cli::cli_abort(
        c(
          "{.arg parameterSets} references undefined parameter sets:",
          "x" = "{.val {bad}}"
        ),
        call = call
      )
    }
    sets
  }))

  applications <- project$.getSection("applications")
  for (i in seq_len(n)) {
    applications[[id[[i]]]]$parameterSets <- resolved[[i]]
  }
  project$.setSection("applications", applications)
  invisible(project)
}

# Internal helpers ----

# Build one classed `Application` entry from its (optional) parameterSets
# references, validating the references resolve. Aborts on a problem.
#
# @keywords internal
# @noRd
.buildApplicationEntry <- function(
  project,
  parameterSets,
  call = rlang::caller_env()
) {
  app <- list()
  if (!is.null(parameterSets)) {
    if (!is.character(parameterSets)) {
      cli::cli_abort(
        "{.arg parameterSets} must be a character vector of set ids",
        call = call
      )
    }
    parameterSets <- .canonicalizeIdRef(parameterSets)
    bad <- setdiff(parameterSets, names(project$parameterSets %||% list()))
    if (length(bad) > 0L) {
      cli::cli_abort(
        c(
          "{.arg parameterSets} references undefined parameter sets:",
          "x" = "{.val {bad}}"
        ),
        call = call
      )
    }
    app$parameterSets <- parameterSets
  }
  class(app) <- c("Application", "list")
  app
}

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
