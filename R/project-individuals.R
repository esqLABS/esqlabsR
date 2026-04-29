# Public CRUD: individuals ----

#' Add an individual to a Project
#'
#' @param project A `Project` object.
#' @param individualId Character scalar, unique ID for the individual.
#' @param species Character scalar, species name.
#' @param ... Optional named fields: `population`, `gender`, `weight`,
#'   `height`, `age`, `proteinOntogenies`. Numeric fields are coerced
#'   via `as.double()`. Unknown fields trigger an error.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family individual
addIndividual <- function(project, individualId, species, ...) {
  validateIsOfType(project, "Project")
  errors <- character()

  if (
    !is.character(individualId) ||
      length(individualId) != 1 ||
      is.na(individualId) ||
      nchar(individualId) == 0
  ) {
    errors <- c(errors, "individualId must be a non-empty string")
  } else if (individualId %in% names(project$individuals)) {
    errors <- c(
      errors,
      paste0("individual '", individualId, "' already exists")
    )
  }

  if (
    !is.character(species) ||
      length(species) != 1 ||
      is.na(species) ||
      nchar(species) == 0
  ) {
    errors <- c(errors, "species must be a non-empty string")
  }

  dots <- list(...)
  allowed <- c(
    "population",
    "gender",
    "weight",
    "height",
    "age",
    "proteinOntogenies",
    "parameters"
  )
  unknown <- setdiff(names(dots), allowed)
  if (length(unknown) > 0) {
    errors <- c(
      errors,
      paste0(
        "unknown fields: ",
        paste(unknown, collapse = ", "),
        ". Allowed: ",
        paste(allowed, collapse = ", ")
      )
    )
  }

  if (length(errors) > 0) {
    stop(paste0(
      "Cannot add individual '",
      individualId,
      "':\n- ",
      paste(errors, collapse = "\n- ")
    ))
  }

  entry <- list(species = species, parameters = NULL)
  for (field in c("population", "gender", "proteinOntogenies")) {
    if (!is.null(dots[[field]])) entry[[field]] <- dots[[field]]
  }
  for (field in c("weight", "height", "age")) {
    if (!is.null(dots[[field]])) entry[[field]] <- as.double(dots[[field]])
  }
  if (!is.null(dots$parameters)) {
    pset <- NULL
    for (p in dots$parameters) {
      pset <- .addParameterEntry(
        pset,
        p$containerPath,
        p$parameterName,
        p$value,
        p$units
      )
    }
    entry$parameters <- pset
  }
  class(entry) <- c("Individual", "list")

  project$individuals[[individualId]] <- entry
  project$.markModified()
  invisible(project)
}

#' Remove an individual from a Project
#'
#' @param project A `Project` object.
#' @param individualId Character scalar, ID of the individual to remove.
#' @returns The `project` object, invisibly.
#' @export
#' @family individual
removeIndividual <- function(project, individualId) {
  validateIsOfType(project, "Project")
  if (
    !is.character(individualId) ||
      length(individualId) != 1 ||
      is.na(individualId) ||
      nchar(individualId) == 0
  ) {
    stop("individualId must be a non-empty string")
  }
  if (!(individualId %in% names(project$individuals))) {
    cli::cli_warn("individual {.val {individualId}} not found; no-op.")
    return(invisible(project))
  }
  .warnIfReferenced(project, "individual", individualId)
  project$individuals[[individualId]] <- NULL
  project$.markModified()
  invisible(project)
}
