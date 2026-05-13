# Section validation adapters ----
#
# Registered in `.validationAdapters` (R/validation.R) and called by
# `.runProjectValidation()`.

#' @keywords internal
#' @noRd
.individualsValidatorAdapter <- function(project) {
  .validateIndividuals(project$individuals)
}

#' @keywords internal
#' @noRd
.individualParameterSetsValidatorAdapter <- function(project) {
  .validateParameterSets(
    project$individualParameterSets,
    "individualParameterSets"
  )
}

#' Validate the `individuals` section of a Project
#'
#' Checks `species` and `gender` are present and warns when numeric
#' fields (`weight`, `height`, `age`) are non-numeric. Cross-references
#' to `individualParameterSets` are validated in
#' `.validateCrossReferences()`.
#'
#' @param individuals Named list from `project$individuals`.
#' @return validationResult.
#' @keywords internal
#' @noRd
.validateIndividuals <- function(individuals) {
  result <- validationResult$new()

  if (is.null(individuals) || length(individuals) == 0) {
    result$add_warning("Data", "No individuals defined")
    return(result)
  }

  requiredFields <- c("species", "gender")
  for (id in names(individuals)) {
    indiv <- individuals[[id]]

    result <- .check_required_fields(
      indiv,
      requiredFields,
      paste0("individual '", id, "'"),
      result
    )

    for (numField in c("weight", "height", "age")) {
      val <- indiv[[numField]]
      if (!is.null(val) && !is.na(val) && !is.numeric(val)) {
        result$add_warning(
          "Data Type",
          paste0(
            "Field '",
            numField,
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

#' Apply an individual to the simulation. For human species, only parameters
#' that do not override formulas are applied. For other species, all parameters
#' returned by `createIndividual` are applied.
#'
#' @param individualCharacteristics `IndividualCharacteristics` describing an
#'   individual. Optional
#' @param simulation `Simulation` loaded from the PKML file
#' @import ospsuite
#' @export
#'
#' @examples
#' \dontrun{
#' simulation <- loadSimulation(filePath = modelPath)
#' humanIndividualCharacteristics <- createIndividualCharacteristics(
#'   species = Species$Human, population = HumanPopulation$European_ICRP_2002,
#'   gender = Gender$Male, weight = 70
#' )
#' applyIndividualParameters(humanIndividualCharacteristics, simulation)
#' }
applyIndividualParameters <- function(individualCharacteristics, simulation) {
  individual <- ospsuite::createIndividual(individualCharacteristics)

  # For human species, only set distributed parameters
  allParamPaths <- individual$distributedParameters$paths
  allParamValues <- individual$distributedParameters$values
  allParamUnits <- individual$distributedParameters$units

  # For other species, also add derived parameters
  if (individualCharacteristics$species != ospsuite::Species$Human) {
    allParamPaths <- c(allParamPaths, individual$derivedParameters$paths)
    allParamValues <- c(allParamValues, individual$derivedParameters$values)
    allParamUnits <- c(allParamUnits, individual$derivedParameters$units)
  }

  ospsuite::setParameterValuesByPath(
    parameterPaths = allParamPaths,
    values = allParamValues,
    simulation = simulation,
    units = allParamUnits,
    stopIfNotFound = FALSE
  )
}

# Public CRUD: individuals ----

#' Add an individual to a Project
#'
#' @param project A `Project` object.
#' @param individualId Character scalar, unique ID for the individual.
#' @param species Character scalar, species name.
#' @param ... Optional named fields: `population`, `gender`, `weight`,
#'   `height`, `age`, `proteinOntogenies`, `parameterSets`. Numeric
#'   fields are coerced via `as.double()`. `parameterSets` is a
#'   character vector of ids referencing
#'   `project$individualParameterSets`. Unknown fields trigger an
#'   error.
#' @returns The `project` object, invisibly.
#' @export
#' @family individual
addIndividual <- function(project, individualId, species, ...) {
  validateIsOfType(project, "Project")
  errors <- character()

  if (
    !is.character(individualId) ||
      length(individualId) != 1L ||
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
      length(species) != 1L ||
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
    "parameterSets"
  )
  unknown <- setdiff(names(dots), allowed)
  if (length(unknown) > 0L) {
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

  if (length(errors) > 0L) {
    cli::cli_abort(c(
      "Cannot add individual {.val {individualId}}:",
      stats::setNames(errors, rep("x", length(errors)))
    ))
  }

  entry <- list(species = species)
  for (field in c("population", "gender", "proteinOntogenies")) {
    if (!is.null(dots[[field]])) entry[[field]] <- dots[[field]]
  }
  for (field in c("weight", "height", "age")) {
    if (!is.null(dots[[field]])) {
      entry[[field]] <- as.double(dots[[field]])
    }
  }
  if (!is.null(dots$parameterSets)) {
    if (!is.character(dots$parameterSets)) {
      cli::cli_abort(
        "{.arg parameterSets} must be a character vector of set ids"
      )
    }
    bad <- setdiff(
      dots$parameterSets,
      names(project$individualParameterSets %||% list())
    )
    if (length(bad) > 0L) {
      cli::cli_abort(c(
        "{.arg parameterSets} references undefined individual parameter sets:",
        "x" = "{.val {bad}}"
      ))
    }
    entry$parameterSets <- dots$parameterSets
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
      length(individualId) != 1L ||
      is.na(individualId) ||
      nchar(individualId) == 0
  ) {
    cli::cli_abort("{.arg individualId} must be a non-empty string")
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

#' Replace the parameter-set references on an individual
#'
#' @param project A `Project` object.
#' @param individualId Character scalar.
#' @param parameterSets Character vector of set ids (from
#'   `project$individualParameterSets`). Use `character(0)` to clear.
#' @returns The `project` object, invisibly.
#' @export
#' @family individual
setIndividualParameterSets <- function(project, individualId, parameterSets) {
  validateIsOfType(project, "Project")
  if (!(individualId %in% names(project$individuals))) {
    cli::cli_abort("individual {.val {individualId}} not found")
  }
  if (!is.character(parameterSets)) {
    cli::cli_abort("{.arg parameterSets} must be a character vector")
  }
  bad <- setdiff(
    parameterSets,
    names(project$individualParameterSets %||% list())
  )
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "{.arg parameterSets} references undefined individual parameter sets:",
      "x" = "{.val {bad}}"
    ))
  }
  project$individuals[[individualId]]$parameterSets <- parameterSets
  project$.markModified()
  invisible(project)
}

# Public CRUD: individualParameterSets ----

#' Create an individual parameter set
#' @param project A `Project` object.
#' @param id Character scalar, set name. Must not already exist.
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
addIndividualParameterSet <- function(project, id) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1L || is.na(id) || nchar(id) == 0) {
    cli::cli_abort("{.arg id} must be a non-empty string")
  }
  if (id %in% names(project$individualParameterSets)) {
    cli::cli_abort("individual parameter set {.val {id}} already exists")
  }
  project$individualParameterSets[[id]] <- list()
  project$.markModified()
  invisible(project)
}

#' Remove an individual parameter set
#' @inheritParams addIndividualParameterSet
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
removeIndividualParameterSet <- function(project, id) {
  validateIsOfType(project, "Project")
  if (!(id %in% names(project$individualParameterSets))) {
    cli::cli_warn("individual parameter set {.val {id}} not found; no-op.")
    return(invisible(project))
  }
  .warnIfReferenced(project, "individualParameterSet", id)
  project$individualParameterSets[[id]] <- NULL
  project$.markModified()
  invisible(project)
}

#' Add a parameter entry to a named individual parameter set
#'
#' Adds one parameter entry to the named set in
#' `project$individualParameterSets`. The set is created on demand if it
#' does not yet exist. Last-write-wins on duplicate paths.
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
addIndividualParameterSetEntry <- function(
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
  current <- project$individualParameterSets[[id]]
  project$individualParameterSets[[id]] <- .addParameterEntry(
    current,
    containerPath,
    parameterName,
    value,
    units
  )
  project$.markModified()
  invisible(project)
}

#' Remove a parameter entry from a named individual parameter set
#'
#' Removes one parameter entry from the named set. If the removed entry
#' was the last in the set, the set itself is auto-removed from
#' `project$individualParameterSets`. Warns if the set or entry doesn't
#' exist.
#'
#' @inheritParams addIndividualParameterSetEntry
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
removeIndividualParameterSetEntry <- function(
  project,
  id,
  containerPath,
  parameterName
) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1L) {
    cli::cli_abort("{.arg id} must be a string scalar")
  }
  if (!(id %in% names(project$individualParameterSets))) {
    cli::cli_warn("individual parameter set {.val {id}} not found; no-op.")
    return(invisible(project))
  }
  result <- .removeParameterEntry(
    project$individualParameterSets[[id]],
    containerPath,
    parameterName
  )
  if (!result$removed) {
    return(invisible(project))
  }
  if (is.null(result$parameters)) {
    .warnIfReferenced(project, "individualParameterSet", id)
    project$individualParameterSets[[id]] <- NULL
  } else {
    project$individualParameterSets[[id]] <- result$parameters
  }
  project$.markModified()
  invisible(project)
}
