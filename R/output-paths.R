# Output paths section ----
#
# Parse + validate + mutate `project$outputPaths`. The `outputPaths` JSON
# section is a named list mapping output-path ids to literal paths;
# parsing is shallow (no helpers needed beyond the parser default), so
# this file owns validation and mutation.

#' @keywords internal
#' @noRd
.outputPathsValidatorAdapter <- function(project) {
  .validateOutputPaths(project$outputPaths)
}

#' Validate the `outputPaths` section of a Project
#'
#' Checks for duplicate ids, empty literal paths, and warns when two ids
#' map to the same literal path (the round-trip lossiness flagged in the
#' Chapter 2 PR).
#'
#' @param outputPaths Named character vector / list from
#'   `project$outputPaths`.
#' @return validationResult.
#' @keywords internal
#' @noRd
.validateOutputPaths <- function(outputPaths) {
  result <- validationResult$new()

  if (is.null(outputPaths) || length(outputPaths) == 0) {
    result$add_warning("Data", "No output paths defined")
    return(result)
  }

  result <- .check_no_duplicates(names(outputPaths), "outputPathId", result)

  values <- unlist(outputPaths, use.names = FALSE)
  emptyIds <- names(outputPaths)[is.na(values) | values == ""]
  if (length(emptyIds) > 0) {
    result$add_critical_error(
      "Missing Fields",
      paste0(
        "Empty output path values for IDs: ",
        paste(emptyIds, collapse = ", ")
      )
    )
  }

  dupeValues <- values[duplicated(values) & !is.na(values)]
  if (length(dupeValues) > 0) {
    result$add_warning(
      "Uniqueness",
      paste0(
        "Multiple IDs point to the same output path: ",
        paste(unique(dupeValues), collapse = ", ")
      )
    )
  }

  result
}

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
    !is.character(id) ||
      length(id) < 1L ||
      any(is.na(id)) ||
      any(nchar(id) == 0)
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
    cli::cli_abort(c(
      "Cannot add outputPath:",
      stats::setNames(errors, rep("x", length(errors)))
    ))
  }

  newPaths <- as.list(path)
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
  if (!is.character(id) || length(id) != 1L || is.na(id) || nchar(id) == 0) {
    cli::cli_abort("{.arg id} must be a non-empty string")
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
