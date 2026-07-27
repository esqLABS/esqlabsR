# Parse ----
#
# Parse the `individuals` JSON array into a named list keyed by
# `individualId`. Like `.parsePopulations()`, every field except the key
# is passed through (unknown fields are preserved so newer-schema files
# round-trip); the known numeric fields are coerced via `as.double` and
# `parameterSets` to a character vector. Each entry is stamped with
# `class = c("Individual", "list")` to enable S3 dispatch.
#
# @keywords internal
# @noRd
.parseIndividuals <- function(individualsData) {
  if (is.null(individualsData) || length(individualsData) == 0L) {
    return(list())
  }
  numericFields <- c("weight", "height", "age")
  result <- list()
  for (entry in individualsData) {
    id <- .keyedTreeRecordId(entry, "individualId", "individual")
    .assertNoEmptyObjectFields(entry, "individual")
    indiv <- list()
    for (field in names(entry)) {
      if (field == "individualId") {
        next
      }
      val <- entry[[field]]
      if (is.null(val)) {
        next
      }
      if (field %in% numericFields) {
        val <- as.double(val)
      } else if (field == "parameterSets") {
        val <- as.character(unlist(val))
      }
      indiv[[field]] <- val
    }
    # A gender-less individual (an animal species whose only valid PK-Sim
    # gender is UNKNOWN) defaults to UNKNOWN, mirroring `.buildIndividualEntry()`
    # and the Excel importer, so every stored individual carries a concrete
    # gender regardless of which entrypoint created it.
    if (is.null(indiv$gender)) {
      indiv$gender <- "UNKNOWN"
    }
    class(indiv) <- c("Individual", "list")
    result[[id]] <- indiv
  }
  result
}

# Section validation adapters ----
#
# Registered in `.validationAdapters` (R/validation.R) and called by
# `.runProjectValidation()`.

#' @keywords internal
#' @noRd
.individualsValidatorAdapter <- function(project) {
  .validateIndividuals(project$definitions$individuals)
}

#' Validate the `individuals` section of a Project
#'
#' Checks `species` is present and warns when numeric fields (`weight`,
#' `height`, `age`) are non-numeric. `gender` is optional (an absent gender
#' means the PK-Sim `UNKNOWN` gender, the only valid one for some animal
#' species). Cross-references to `parameterSets` are validated in
#' `.validateCrossReferences()`.
#'
#' @param individuals Named list from `individuals` definitions.
#' @return validationResult.
#' @keywords internal
#' @noRd
.validateIndividuals <- function(individuals) {
  result <- validationResult$new()

  if (is.null(individuals) || length(individuals) == 0) {
    result$addWarning("Data", "No individuals defined")
    return(result)
  }

  requiredFields <- c("species")
  for (id in names(individuals)) {
    indiv <- individuals[[id]]

    result <- .checkRequiredFields(
      indiv,
      requiredFields,
      paste0("individual '", id, "'"),
      result
    )

    # `gender` is optional (an absent gender defaults to UNKNOWN), but a
    # present gender must be a valid `GenderInt` token. This mirrors the
    # authoring check in `.buildIndividualEntry()` so a hand-authored JSON file
    # with an invalid gender (e.g. "" or "foo") is caught here rather than
    # deferring to an opaque PK-Sim error at run time.
    gender <- indiv[["gender"]]
    if (
      !is.null(gender) &&
        !(length(gender) == 1 && !is.na(gender) && gender %in% names(GenderInt))
    ) {
      result$addCriticalError(
        "Data",
        paste0(
          "Field 'gender' in individual '",
          id,
          "' must be one of ",
          paste(names(GenderInt), collapse = ", "),
          " (or omitted)"
        )
      )
    }

    for (numField in c("weight", "height", "age")) {
      val <- indiv[[numField]]
      if (!is.null(val) && !is.na(val) && !is.numeric(val)) {
        result$addWarning(
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

# Print ----

#' @exportS3Method
#' @noRd
print.Individual <- function(x, ...) {
  ospsuite.utils::ospPrintClass(x)
  ospsuite.utils::ospPrintItems(
    list(
      "Species" = x$species %||% "",
      "Population" = x$population %||% "",
      "Gender" = x$gender %||% "",
      "Weight" = x$weight %||% "",
      "Height" = x$height %||% "",
      "Age" = x$age %||% "",
      "Parameter Sets" = paste(x$parameterSets, collapse = ", ")
    ),
    print_empty = TRUE
  )
  invisible(x)
}

# Public CRUD: individuals ----

#' Add one or more individuals to a Project
#'
#' Add individuals to `individuals` definitions, vectorizing over a vector of ids
#' (see the recycling rule under Details). Scalar-per-definition fields (`species`
#' and the `...` fields `population`, `gender`, `weight`, `height`, `age`,
#' `proteinOntogenies`) follow the recycle/align rule; `parameterSets` is
#' vector-valued-per-definition (applied whole to every individual, or one vector
#' per individual via a length-`id` list).
#'
#' @inherit vectorizedAuthoring details
#'
#' @param project A `Project` object.
#' @param id Character vector of unique ids for the individuals (the number of
#'   individuals to add). Each is canonicalized to a safe, lowercase id (a
#'   warning names the result if it changed).
#' @param species Character scalar (recycled) or the same length as `id`,
#'   species name.
#' @param ... Optional named fields: `population`, `gender`, `weight`,
#'   `height`, `age`, `proteinOntogenies`, `parameterSets`, and `overwrite`.
#'   `gender` defaults to `UNKNOWN` when omitted (the only valid PK-Sim gender
#'   for some animal species); when supplied it must be a valid `GenderInt`
#'   token. Numeric fields are coerced via `as.double()`. `parameterSets` is a
#'   character vector of ids referencing `parameterSets` definitions.
#'   `overwrite` is a logical scalar (default `FALSE`): an id that already
#'   exists aborts unless `overwrite = TRUE`, which replaces it
#'   (last-write-wins). Unknown fields trigger an error.
#' @returns The `project` object, invisibly.
#' @export
#' @family individual
addIndividual <- function(project, id, species, ...) {
  validateIsOfType(project, "Project")
  project$addIndividual(id, species, ...)
}

# Implementation behind `project$addIndividual()` / `addIndividual()`.
#
# @keywords internal
# @noRd
.addIndividual_impl <- function(self, private, id, species, ..., .call) {
  rlang::local_error_call(.call)
  .assertIdVector(id)
  id <- .canonicalizeId(id)
  n <- length(id)

  dots <- list(...)
  # `overwrite` arrives through `...`; pull it out (validated) before aligning
  # so it is not mistaken for a per-definition field.
  overwrite <- .validateOverwriteFlag(dots[["overwrite"]])
  dots[["overwrite"]] <- NULL
  # `parameterSets` is the one vector-valued-per-definition field; everything else
  # is scalar-per-definition. `species` is a positional formal, not a `...` field.
  wholeNames <- intersect("parameterSets", names(dots))
  scalarDots <- dots[setdiff(names(dots), wholeNames)]
  perDefinition <- .alignAuthoringArgs(
    id,
    scalarFields = c(list(species = species), scalarDots),
    wholeFields = dots[wholeNames]
  )

  # Validate all N first (all-or-nothing): build every entry before folding any,
  # so an invalid definition in the batch writes nothing. A within-batch
  # duplicate id aborts unless overwriting, in which case the last one wins.
  .assertNoOverwriteClash(
    id,
    names(self$definitions$individuals),
    "individual",
    overwrite
  )
  call <- .call
  entries <- .collectCanonicalizedRefs(lapply(seq_len(n), function(i) {
    .buildIndividualEntry(self, id[[i]], perDefinition[[i]], call = call)
  }))

  # Fold all N into the section in memory, then ONE assignment triggers one
  # write-through.
  individuals <- private$.getSection("individuals") %||% list()
  for (i in seq_len(n)) {
    individuals[[id[[i]]]] <- entries[[i]]
  }
  private$.setSection("individuals", individuals)
  invisible(self)
}

# Build one classed `Individual` entry from its id and per-definition field list,
# validating the same way the scalar path always has (`species` required
# non-empty; `gender` optional, defaulting to `UNKNOWN`; `parameterSets` a
# resolvable character vector). Aborts naming the individual on the first
# problem.
#
# @keywords internal
# @noRd
.buildIndividualEntry <- function(
  project,
  id,
  fields,
  call = rlang::caller_env()
) {
  errors <- character()
  allowed <- c(
    "species",
    "population",
    "gender",
    "weight",
    "height",
    "age",
    "proteinOntogenies",
    "parameterSets"
  )
  unknown <- setdiff(names(fields), allowed)
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

  species <- fields$species
  if (
    !is.character(species) ||
      length(species) != 1L ||
      is.na(species) ||
      nchar(species) == 0
  ) {
    errors <- c(errors, "species must be a non-empty string")
  }

  # `gender` is optional: an absent gender defaults to `UNKNOWN` (the only
  # valid PK-Sim gender for some animal species). A supplied gender must still
  # be a valid `GenderInt` token.
  gender <- fields$gender
  if (is.null(gender)) {
    gender <- "UNKNOWN"
  } else if (
    !is.character(gender) ||
      length(gender) != 1L ||
      is.na(gender) ||
      nchar(gender) == 0
  ) {
    errors <- c(errors, "gender must be a non-empty string")
  } else if (!(gender %in% names(GenderInt))) {
    errors <- c(
      errors,
      paste0(
        "gender must be one of ",
        paste(names(GenderInt), collapse = ", ")
      )
    )
  }

  # weight/height/age are stored as doubles. Coerce a numeric-like value
  # (including a character such as "45") and reject only a value that does not
  # coerce to a single finite number (e.g. "80kg" -> NA) rather than silently
  # storing NA. This matches the set path (`.setOneIndividual()`).
  for (field in c("weight", "height", "age")) {
    value <- fields[[field]]
    if (!is.null(value)) {
      coerced <- suppressWarnings(as.double(value))
      if (length(value) != 1L || is.na(coerced) || !is.finite(coerced)) {
        errors <- c(errors, paste0(field, " must be a single finite number"))
      }
    }
  }

  if (length(errors) > 0L) {
    cli::cli_abort(
      c(
        "Cannot add individual {.val {id}}:",
        stats::setNames(errors, rep("x", length(errors)))
      ),
      call = call
    )
  }

  entry <- list(species = species, gender = gender)
  for (field in c("population", "proteinOntogenies")) {
    if (!is.null(fields[[field]])) entry[[field]] <- fields[[field]]
  }
  for (field in c("weight", "height", "age")) {
    if (!is.null(fields[[field]])) {
      entry[[field]] <- as.double(fields[[field]])
    }
  }
  if (!is.null(fields$parameterSets)) {
    if (!is.character(fields$parameterSets)) {
      cli::cli_abort(
        "{.arg parameterSets} must be a character vector of set ids",
        call = call
      )
    }
    sets <- .canonicalizeIdRef(fields$parameterSets)
    bad <- setdiff(sets, names(project$definitions$parameterSets %||% list()))
    if (length(bad) > 0L) {
      cli::cli_abort(
        c(
          "{.arg parameterSets} references undefined parameter sets:",
          "x" = "{.val {bad}}"
        ),
        call = call
      )
    }
    entry$parameterSets <- sets
  }
  class(entry) <- c("Individual", "list")
  entry
}

#' Remove one or more individuals from a Project
#'
#' Drop the individuals with matching ids in one write-through. Warns (and
#' skips) any id not present, and warns when a removed individual is still
#' referenced.
#'
#' @param project A `Project` object.
#' @param id Character vector of individual ids to remove. Each is
#'   canonicalized the same way [addIndividual()] canonicalizes it.
#' @returns The `project` object, invisibly.
#' @export
#' @family individual
removeIndividual <- function(project, id) {
  validateIsOfType(project, "Project")
  project$removeIndividual(id)
}

# Implementation behind `project$removeIndividual()` / `removeIndividual()`.
#
# @keywords internal
# @noRd
.removeIndividual_impl <- function(self, private, id, .call) {
  rlang::local_error_call(.call)
  .assertIdVector(id)
  id <- .canonicalizeId(id)

  missingIds <- setdiff(id, names(self$definitions$individuals))
  if (length(missingIds) > 0L) {
    cli::cli_warn("individual {.val {missingIds}} not found; no-op.")
  }
  toRemove <- intersect(id, names(self$definitions$individuals))
  if (length(toRemove) == 0L) {
    return(invisible(self))
  }
  for (one in toRemove) {
    .warnIfReferenced(self, "individual", one)
  }
  individuals <- private$.getSection("individuals")
  individuals[toRemove] <- NULL
  private$.setSection("individuals", individuals)
  invisible(self)
}

#' Modify fields of an existing individual
#'
#' @description Changes one or more fields of the individual identified by
#'   `id` and persists the change immediately to the individual definition
#'   (write-through). The `individuals` definitions accessor is read-only, so this
#'   is the way to revise an existing individual in place.
#'
#'   Only the arguments you pass via `...` are changed; every other field
#'   keeps its current value (partial update). Validation matches
#'   [addIndividual()]: numeric fields (`weight`, `height`, `age`) are
#'   coerced via `as.double()`, `gender` (if supplied) must be a non-empty
#'   string, and `parameterSets` (if supplied) must be a character vector of
#'   ids that resolve in `parameterSets` definitions. The required
#'   `species` field, if supplied, must be a non-empty string.
#'
#' @inherit vectorizedAuthoring details
#'
#' @param project A `Project` object.
#' @param id Character vector. Ids of the individuals to modify. Each is
#'   canonicalized the same way [addIndividual()] canonicalizes it, and must
#'   already exist in `individuals` definitions.
#' @param ... Named fields to change. Accepted: `species`, `population`,
#'   `gender`, `weight`, `height`, `age`, `proteinOntogenies`,
#'   `parameterSets`. Scalar-per-definition fields recycle/align across `id`;
#'   `parameterSets` is applied whole (or one vector per individual via a
#'   length-`id` list). Unknown fields trigger an error.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family individual
setIndividual <- function(project, id, ...) {
  validateIsOfType(project, "Project")
  project$setIndividual(id, ...)
}

# Implementation behind `project$setIndividual()` / `setIndividual()`.
#
# @keywords internal
# @noRd
.setIndividual_impl <- function(self, private, id, ..., .call) {
  rlang::local_error_call(.call)
  .assertIdVector(id)
  id <- .canonicalizeId(id)
  n <- length(id)
  missingIds <- setdiff(id, names(self$definitions$individuals))
  if (length(missingIds) > 0L) {
    cli::cli_abort(c(
      "Cannot modify individual {.val {missingIds}}: it does not exist.",
      "i" = "Use {.fn addIndividual} to create it first."
    ))
  }

  dots <- list(...)
  wholeNames <- intersect("parameterSets", names(dots))
  scalarDots <- dots[setdiff(names(dots), wholeNames)]
  perDefinition <- .alignAuthoringArgs(
    id,
    scalarFields = scalarDots,
    wholeFields = dots[wholeNames]
  )
  # Only the field names the caller actually supplied are applied (partial
  # update); the engine carries every supplied field for each definition.
  suppliedNames <- names(dots)

  call <- .call
  entries <- .collectCanonicalizedRefs(lapply(seq_len(n), function(i) {
    .setOneIndividual(
      self,
      id[[i]],
      perDefinition[[i]][suppliedNames],
      call = call
    )
  }))

  individuals <- private$.getSection("individuals")
  for (i in seq_len(n)) {
    individuals[[id[[i]]]] <- entries[[i]]
  }
  private$.setSection("individuals", individuals)
  invisible(self)
}

# Apply a partial-update field set to one existing individual, returning the
# updated classed entry. Validates only the supplied fields, matching the
# scalar partial-update contract. Aborts naming the individual on a problem.
#
# @keywords internal
# @noRd
.setOneIndividual <- function(project, id, fields, call = rlang::caller_env()) {
  allowed <- c(
    "species",
    "population",
    "gender",
    "weight",
    "height",
    "age",
    "proteinOntogenies",
    "parameterSets"
  )
  unknown <- setdiff(names(fields), allowed)
  if (length(unknown) > 0L) {
    cli::cli_abort(
      c(
        "Cannot modify individual {.val {id}}:",
        "x" = "unknown fields: {.val {unknown}}. Allowed: {.val {allowed}}."
      ),
      call = call
    )
  }

  if ("species" %in% names(fields)) {
    species <- fields$species
    if (
      !is.character(species) ||
        length(species) != 1L ||
        is.na(species) ||
        nchar(species) == 0
    ) {
      cli::cli_abort("{.arg species} must be a non-empty string", call = call)
    }
  }
  if ("gender" %in% names(fields)) {
    gender <- fields$gender
    if (
      is.null(gender) ||
        !is.character(gender) ||
        length(gender) != 1L ||
        is.na(gender) ||
        nchar(gender) == 0
    ) {
      cli::cli_abort("{.arg gender} must be a non-empty string", call = call)
    }
    if (!(gender %in% names(GenderInt))) {
      cli::cli_abort(
        "{.arg gender} must be one of {.val {names(GenderInt)}}",
        call = call
      )
    }
  }
  # weight/height/age are stored as doubles. Coerce a numeric-like value
  # (including a character such as "45" from Excel) and reject only a value
  # that does not coerce to a single finite number (e.g. "80kg" -> NA) rather
  # than silently storing NA. A NULL is allowed here: it clears the field via
  # `.coerceNumericField()` below.
  for (field in c("weight", "height", "age")) {
    if (field %in% names(fields)) {
      value <- fields[[field]]
      if (!is.null(value)) {
        coerced <- suppressWarnings(as.double(value))
        if (length(value) != 1L || is.na(coerced) || !is.finite(coerced)) {
          cli::cli_abort(
            "{field} must be a single finite number",
            call = call
          )
        }
      }
    }
  }
  if ("parameterSets" %in% names(fields)) {
    if (!is.character(fields$parameterSets)) {
      cli::cli_abort(
        "{.arg parameterSets} must be a character vector of set ids",
        call = call
      )
    }
    fields$parameterSets <- .canonicalizeIdRef(fields$parameterSets)
    bad <- setdiff(
      fields$parameterSets,
      names(project$definitions$parameterSets %||% list())
    )
    if (length(bad) > 0L) {
      cli::cli_abort(
        c(
          "{.arg parameterSets} references undefined parameter sets:",
          "x" = "{.val {bad}}"
        ),
        call = call
      )
    }
  }

  entry <- project$definitions$individuals[[id]]
  for (field in names(fields)) {
    if (field %in% c("weight", "height", "age")) {
      entry[[field]] <- .coerceNumericField(fields[[field]])
    } else {
      entry[[field]] <- fields[[field]]
    }
  }
  class(entry) <- c("Individual", "list")
  entry
}
