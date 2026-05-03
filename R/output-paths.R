# Output paths section: validate + mutation.
#
# Owns Project$outputPaths end-to-end. outputPaths is a named character
# vector mapping ids to literal paths into a simulation tree. It is
# referenced by scenarios (via outputPathIds) and by plots (via literal
# paths in dataCombined rows).
#
# Called by:
#   - Project$.read_json() — parsed inline as `unlist(jsonData$outputPaths)`
#   - .runProjectValidation() via .validateOutputPaths()
#   - .projectToJson() — serialized inline as pass-through
#     (`outputPaths = project$outputPaths`)
#   - users via the public addOutputPath / removeOutputPath functions.

# Validate ----

#' Validate outputPaths section of a Project
#' @param outputPaths Named character vector from project$outputPaths
#' @return validationResult object
#' @keywords internal
.validateOutputPaths <- function(outputPaths) {
  result <- validationResult$new()

  if (is.null(outputPaths) || length(outputPaths) == 0) {
    result$add_warning("Data", "No output paths defined")
    return(result)
  }

  result <- .check_no_duplicates(names(outputPaths), "outputPathId", result)

  empty_paths <- names(outputPaths)[is.na(outputPaths) | outputPaths == ""]
  if (length(empty_paths) > 0) {
    result$add_critical_error(
      "Missing Fields",
      paste0(
        "Empty output path values for IDs: ",
        paste(empty_paths, collapse = ", ")
      )
    )
  }

  dupe_values <- outputPaths[duplicated(outputPaths) & !is.na(outputPaths)]
  if (length(dupe_values) > 0) {
    result$add_warning(
      "Uniqueness",
      paste0(
        "Multiple IDs point to the same output path: ",
        paste(unique(dupe_values), collapse = ", ")
      )
    )
  }

  result
}

#' @keywords internal
#' @noRd
.outputPathsValidatorAdapter <- function(project) {
  .validateOutputPaths(project$outputPaths)
}

# Public CRUD ----

#' Add output paths to a Project
#'
#' @param project A `Project` object.
#' @param id Character vector of output path IDs (unique within the call
#'   and not already present in `project$outputPaths`).
#' @param path Character vector of output paths, same length as `id`.
#' @returns The `project` object, invisibly.
#' @export
#' @family scenario
addOutputPath <- function(project, id, path) {
  validateIsOfType(project, "Project")
  errors <- character()

  if (
    !is.character(id) || length(id) < 1 || any(is.na(id)) || any(nchar(id) == 0)
  ) {
    errors <- c(errors, "id must be a non-empty character vector")
  }
  if (!is.character(path) || length(path) != length(id)) {
    errors <- c(
      errors,
      "id and path must be character vectors of the same length"
    )
  }
  if (is.character(id) && any(duplicated(id))) {
    errors <- c(
      errors,
      paste0(
        "duplicate ids within call: ",
        paste(unique(id[duplicated(id)]), collapse = ", ")
      )
    )
  }
  if (is.character(id)) {
    collisions <- intersect(id, names(project$outputPaths))
    if (length(collisions) > 0) {
      errors <- c(
        errors,
        paste0(
          "outputPath id already exists: ",
          paste(collisions, collapse = ", ")
        )
      )
    }
  }

  if (length(errors) > 0) {
    stop(paste0(
      "Cannot add outputPath:\n- ",
      paste(errors, collapse = "\n- ")
    ))
  }

  newPaths <- path
  names(newPaths) <- id
  project$outputPaths <- c(project$outputPaths, newPaths)
  project$.markModified()
  invisible(project)
}

#' Remove an output path from a Project
#' @param project A `Project` object.
#' @param id Character scalar.
#' @returns The `project` object, invisibly.
#' @export
#' @family scenario
removeOutputPath <- function(project, id) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1 || is.na(id) || nchar(id) == 0) {
    stop("id must be a non-empty string")
  }
  if (!(id %in% names(project$outputPaths))) {
    cli::cli_warn("outputPath {.val {id}} not found; no-op.")
    return(invisible(project))
  }
  .warnIfReferenced(project, "outputPath", id)
  project$outputPaths <- project$outputPaths[setdiff(
    names(project$outputPaths),
    id
  )]
  project$.markModified()
  invisible(project)
}
