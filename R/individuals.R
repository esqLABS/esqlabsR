# Individuals section: parse + validate + serialize + mutation.
# Owns Project$individuals end-to-end. Called by:
#   - Project$.read_json() via .parseIndividuals()
#   - .runProjectValidation() via .validateIndividuals()
#   - .projectToJson() via .individualsToJson()
#   - users via the public addIndividual / removeIndividual functions.

# Parse ----

#' @keywords internal
#' @noRd
.parseIndividuals <- function(individualsData) {
  if (is.null(individualsData)) {
    return(list())
  }
  result <- list()
  for (entry in individualsData) {
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
    indiv <- list(
      species = entry$species,
      population = entry$population,
      gender = entry$gender,
      weight = as.double(entry$weight),
      height = as.double(entry$height),
      age = as.double(entry$age),
      proteinOntogenies = entry$proteinOntogenies,
      parameters = pset
    )
    class(indiv) <- c("Individual", "list")
    result[[entry$individualId]] <- indiv
  }
  result
}

# Validate ----

#' Validate individuals section of a Project
#' @param individuals Named list of individuals from project$individuals
#' @return validationResult object
#' @keywords internal
.validateIndividuals <- function(individuals) {
  result <- validationResult$new()

  if (is.null(individuals) || length(individuals) == 0) {
    result$add_warning("Data", "No individuals defined")
    return(result)
  }

  required_fields <- c("species", "gender")
  for (id in names(individuals)) {
    indiv <- individuals[[id]]
    result <- .check_required_fields(
      indiv,
      required_fields,
      paste0("individual '", id, "'"),
      result
    )

    for (num_field in c("weight", "height", "age")) {
      val <- indiv[[num_field]]
      if (!is.null(val) && !is.na(val) && !is.numeric(val)) {
        result$add_warning(
          "Data Type",
          paste0(
            "Field '",
            num_field,
            "' in individual '",
            id,
            "' should be numeric"
          )
        )
      }
    }
  }

  result
}

# Serialize ----

#' @keywords internal
#' @noRd
.individualsToJson <- function(individuals, parameterSetMapping = NULL) {
  if (is.null(individuals) || length(individuals) == 0) {
    return(list())
  }
  result <- vector("list", length(individuals))
  for (i in seq_along(individuals)) {
    id <- names(individuals)[[i]]
    indiv <- individuals[[i]]
    entry <- list(individualId = id)
    for (field in c("species", "population", "gender", "proteinOntogenies")) {
      if (!is.null(indiv[[field]])) entry[[field]] <- indiv[[field]]
    }
    for (field in c("weight", "height", "age")) {
      if (!is.null(indiv[[field]]) && !is.na(indiv[[field]])) {
        entry[[field]] <- as.double(indiv[[field]])
      }
    }
    pset <- indiv$parameters
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
      entry$parameters <- params
    }
    result[[i]] <- entry
  }
  result
}

# Public CRUD ----

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
