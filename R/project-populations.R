# Public CRUD: populations ----

#' Add a population to a Project
#'
#' @param project A `Project` object.
#' @param populationId Character scalar, unique ID.
#' @param species Character scalar.
#' @param numberOfIndividuals Integer, positive.
#' @param ... Optional named fields. Accepted: `proportionOfFemales`,
#'   `weightMin`, `weightMax`, `heightMin`, `heightMax`, `ageMin`, `ageMax`,
#'   `BMIMin`, `BMIMax`, `gender`, `weightUnit`, `heightUnit`, `ageUnit`,
#'   `BMIUnit`, `population`, `diseaseState`. Numeric range fields are
#'   coerced via `as.double()`.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family population
addPopulation <- function(
  project,
  populationId,
  species,
  numberOfIndividuals,
  ...
) {
  validateIsOfType(project, "Project")
  errors <- character()

  if (
    !is.character(populationId) ||
      length(populationId) != 1 ||
      is.na(populationId) ||
      nchar(populationId) == 0
  ) {
    errors <- c(errors, "populationId must be a non-empty string")
  } else if (populationId %in% names(project$populations)) {
    errors <- c(
      errors,
      paste0("population '", populationId, "' already exists")
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

  if (
    !is.numeric(numberOfIndividuals) ||
      length(numberOfIndividuals) != 1 ||
      is.na(numberOfIndividuals) ||
      numberOfIndividuals <= 0
  ) {
    errors <- c(errors, "numberOfIndividuals must be a positive number")
  }

  dots <- list(...)
  numericFields <- c(
    "proportionOfFemales",
    "weightMin",
    "weightMax",
    "heightMin",
    "heightMax",
    "ageMin",
    "ageMax",
    "BMIMin",
    "BMIMax"
  )
  stringFields <- c(
    "gender",
    "weightUnit",
    "heightUnit",
    "ageUnit",
    "BMIUnit",
    "population",
    "diseaseState"
  )
  allowed <- c(numericFields, stringFields)
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
      "Cannot add population '",
      populationId,
      "':\n- ",
      paste(errors, collapse = "\n- ")
    ))
  }

  entry <- list(
    species = species,
    numberOfIndividuals = as.double(numberOfIndividuals)
  )
  for (field in numericFields) {
    if (!is.null(dots[[field]])) entry[[field]] <- as.double(dots[[field]])
  }
  for (field in stringFields) {
    if (!is.null(dots[[field]])) entry[[field]] <- dots[[field]]
  }

  project$populations[[populationId]] <- entry
  project$.markModified()
  invisible(project)
}

#' Remove a population from a Project
#' @param project A `Project` object.
#' @param populationId Character scalar.
#' @returns The `project` object, invisibly.
#' @export
#' @family population
removePopulation <- function(project, populationId) {
  validateIsOfType(project, "Project")
  if (
    !is.character(populationId) ||
      length(populationId) != 1 ||
      is.na(populationId) ||
      nchar(populationId) == 0
  ) {
    stop("populationId must be a non-empty string")
  }
  if (!(populationId %in% names(project$populations))) {
    cli::cli_warn("population {.val {populationId}} not found; no-op.")
    return(invisible(project))
  }
  .warnIfReferenced(project, "population", populationId)
  project$populations[[populationId]] <- NULL
  project$.markModified()
  invisible(project)
}
