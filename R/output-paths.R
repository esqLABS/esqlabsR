# Output paths section ----
#
# Parse + validate + mutate `outputPaths` definitions. The `outputPaths` JSON
# section is a named list mapping output-path ids to literal paths;
# parsing is shallow (no helpers needed beyond the parser default), so
# this file owns validation and mutation.

# Public CRUD: output paths ----

#' Add one or more output paths to a Project
#'
#' Add output paths to `outputPaths` definitions, vectorizing over a vector of ids
#' (see the recycling rule under Details). `path` is scalar-per-definition: a
#' single path is recycled to every id, or a length-`id` vector aligns by
#' position.
#'
#' @inherit vectorizedAuthoring details
#'
#' @param project A `Project` object.
#' @param id Character vector of output path ids (unique within the call and
#'   not already present in `outputPaths` definitions). Each is canonicalized.
#' @param path Character vector of output paths, length 1 (recycled) or the
#'   same length as `id`.
#' @returns The `project` object, invisibly.
#' @export
#' @family output path
addOutputPath <- function(project, id, path) {
  validateIsOfType(project, "Project")
  project$addOutputPath(id, path)
}

# Implementation behind `project$addOutputPath()` / `addOutputPath()`.
#
# @keywords internal
# @noRd
.addOutputPath_impl <- function(self, private, id, path, .call) {
  rlang::local_error_call(.call)
  # Route the id-vector check through the shared helper every sibling add* uses,
  # then canonicalize and guard against an in-batch duplicate id (which would
  # otherwise silently overwrite an earlier entry keyed by the same id).
  .assertIdVector(id)
  id <- .canonicalizeId(id)
  .assertNoDuplicateIds(id, "outputPath")

  if (
    !is.character(path) ||
      !(length(path) == 1L || length(path) == length(id))
  ) {
    cli::cli_abort(c(
      "Cannot add outputPath:",
      "x" = "path must be a character vector of length 1 or the same \\
      length as id"
    ))
  }
  if (anyNA(path) || any(nchar(path) == 0)) {
    cli::cli_abort(c(
      "Cannot add outputPath:",
      "x" = "path must contain non-empty strings"
    ))
  }
  clash <- intersect(id, names(self$definitions$outputPaths))
  if (length(clash) > 0L) {
    cli::cli_abort("outputPath {.val {clash}} already exists")
  }

  # Recycle a single path to every id (the scalar-per-definition rule).
  if (length(path) == 1L) {
    path <- rep(path, length(id))
  }
  newPaths <- as.list(path)
  names(newPaths) <- id
  outputPaths <- c(private$.getSection("outputPaths"), newPaths)
  private$.setSection("outputPaths", outputPaths)
  invisible(self)
}

#' Remove one or more output paths from a Project
#'
#' Drop the output paths with matching ids in one write-through. Warns (and
#' skips) any id not present, and warns when a removed output path is still
#' referenced.
#'
#' @param project A `Project` object.
#' @param id Character vector of output-path ids to remove. Each is
#'   canonicalized the same way [addOutputPath()] canonicalizes it.
#' @returns The `project` object, invisibly.
#' @export
#' @family output path
removeOutputPath <- function(project, id) {
  validateIsOfType(project, "Project")
  project$removeOutputPath(id)
}

# Implementation behind `project$removeOutputPath()` / `removeOutputPath()`.
#
# @keywords internal
# @noRd
.removeOutputPath_impl <- function(self, private, id, .call) {
  rlang::local_error_call(.call)
  .assertIdVector(id)
  id <- .canonicalizeId(id)

  missingIds <- setdiff(id, names(self$definitions$outputPaths))
  if (length(missingIds) > 0L) {
    cli::cli_warn("outputPath {.val {missingIds}} not found; no-op.")
  }
  toRemove <- intersect(id, names(self$definitions$outputPaths))
  if (length(toRemove) == 0L) {
    return(invisible(self))
  }
  for (one in toRemove) {
    .warnIfReferenced(self, "outputPath", one)
  }
  outputPaths <- private$.getSection("outputPaths")
  outputPaths <- outputPaths[setdiff(names(outputPaths), toRemove)]
  private$.setSection("outputPaths", outputPaths)
  invisible(self)
}

#' Change the literal path of one or more existing output paths
#'
#' @description Updates the OSPS-notation path string bound to existing
#'   output-path ids and persists the change immediately to the output-path
#'   definition (write-through). The ids themselves are not changed (use
#'   [removeOutputPath()] + [addOutputPath()] to rename), so
#'   every scenario that records these output paths keeps referencing them.
#'   The `outputPaths` definitions accessor is read-only, so this is the way to
#'   change a path in place. The call vectorizes over a vector of ids (see the
#'   recycling rule under Details); `path` is scalar-per-definition (one path
#'   recycled to every id, or a length-`id` vector aligned by position).
#'
#' @inherit vectorizedAuthoring details
#'
#' @param project A `Project` object.
#' @param id Character vector. The output-path ids to modify. Each must
#'   already exist in `outputPaths` definitions.
#' @param path Character vector of new non-empty OSPS-notation path strings,
#'   length 1 (recycled) or the same length as `id`.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family output path
setOutputPath <- function(project, id, path) {
  validateIsOfType(project, "Project")
  project$setOutputPath(id, path)
}

# Implementation behind `project$setOutputPath()` / `setOutputPath()`.
#
# @keywords internal
# @noRd
.setOutputPath_impl <- function(self, private, id, path, .call) {
  rlang::local_error_call(.call)
  .assertIdVector(id)
  id <- .canonicalizeId(id)
  n <- length(id)
  missingIds <- setdiff(id, names(self$definitions$outputPaths))
  if (length(missingIds) > 0L) {
    cli::cli_abort(c(
      "Cannot modify output path {.val {missingIds}}: it does not exist.",
      "i" = "Use {.fn addOutputPath} to create it first."
    ))
  }
  perId <- .recycleField(path, n, "path")
  for (i in seq_len(n)) {
    one <- perId[[i]]
    if (
      !is.character(one) ||
        length(one) != 1L ||
        is.na(one) ||
        nchar(one) == 0
    ) {
      cli::cli_abort("{.arg path} must contain non-empty strings")
    }
  }

  outputPaths <- private$.getSection("outputPaths")
  for (i in seq_len(n)) {
    outputPaths[[id[[i]]]] <- perId[[i]]
  }
  private$.setSection("outputPaths", outputPaths)
  invisible(self)
}

# Section validation adapter ----

#' @keywords internal
#' @noRd
.outputPathsValidatorAdapter <- function(project) {
  .validateOutputPaths(project$definitions$outputPaths)
}

#' Validate the `outputPaths` section of a Project
#'
#' Checks for duplicate ids, empty literal paths, and warns when two ids
#' map to the same literal path (the round-trip lossiness flagged in the
#' Chapter 2 PR).
#'
#' @param outputPaths Named character vector / list from
#'   `outputPaths` definitions.
#' @return validationResult.
#' @keywords internal
#' @noRd
.validateOutputPaths <- function(outputPaths) {
  result <- validationResult$new()

  if (is.null(outputPaths) || length(outputPaths) == 0) {
    result$addWarning("Data", "No output paths defined")
    return(result)
  }

  result <- .checkNoDuplicates(names(outputPaths), "outputPathId", result)

  values <- unlist(outputPaths, use.names = FALSE)
  emptyIds <- names(outputPaths)[is.na(values) | values == ""]
  if (length(emptyIds) > 0) {
    result$addCriticalError(
      "Missing Fields",
      paste0(
        "Empty output path values for IDs: ",
        paste(emptyIds, collapse = ", ")
      )
    )
  }

  dupeValues <- values[duplicated(values) & !is.na(values)]
  if (length(dupeValues) > 0) {
    result$addWarning(
      "Uniqueness",
      paste0(
        "Multiple IDs point to the same output path: ",
        paste(unique(dupeValues), collapse = ", ")
      )
    )
  }

  result
}
