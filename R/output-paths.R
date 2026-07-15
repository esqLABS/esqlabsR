# Output paths section ----
#
# Parse + validate + mutate `project$outputPaths`. The `outputPaths` JSON
# section is a named list mapping output-path ids to literal paths;
# parsing is shallow (no helpers needed beyond the parser default), so
# this file owns validation and mutation.

# Public CRUD: output paths ----

#' Add one or more output paths to a Project
#'
#' Add output paths to `project$outputPaths`, vectorizing over a vector of ids
#' (see the recycling rule under Details). `path` is scalar-per-entity: a
#' single path is recycled to every id, or a length-`id` vector aligns by
#' position.
#'
#' @inherit vectorizedAuthoring details
#'
#' @param project A `Project` object.
#' @param id Character vector of output path ids (unique within the call and
#'   not already present in `project$outputPaths`). Each is canonicalized.
#' @param path Character vector of output paths, length 1 (recycled) or the
#'   same length as `id`.
#' @returns The `project` object, invisibly.
#' @export
#' @family output path
addOutputPath <- function(project, id, path) {
  validateIsOfType(project, "Project")
  # Route the id-vector check through the shared helper every sibling add* uses,
  # then canonicalize (which aborts on an in-batch collision, so no separate
  # duplicate guard is needed).
  .assertIdVector(id)
  id <- .canonicalizeId(id)

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
  clash <- intersect(id, names(project$outputPaths))
  if (length(clash) > 0L) {
    cli::cli_abort("outputPath {.val {clash}} already exists")
  }

  # Recycle a single path to every id (the scalar-per-entity rule).
  if (length(path) == 1L) {
    path <- rep(path, length(id))
  }
  newPaths <- as.list(path)
  names(newPaths) <- id
  outputPaths <- c(project$.getSection("outputPaths"), newPaths)
  project$.setSection("outputPaths", outputPaths)
  invisible(project)
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
  .assertIdVector(id)
  id <- .canonicalizeId(id)

  missingIds <- setdiff(id, names(project$outputPaths))
  if (length(missingIds) > 0L) {
    cli::cli_warn("outputPath {.val {missingIds}} not found; no-op.")
  }
  toRemove <- intersect(id, names(project$outputPaths))
  if (length(toRemove) == 0L) {
    return(invisible(project))
  }
  for (one in toRemove) {
    .warnIfReferenced(project, "outputPath", one)
  }
  outputPaths <- project$.getSection("outputPaths")
  outputPaths <- outputPaths[setdiff(names(outputPaths), toRemove)]
  project$.setSection("outputPaths", outputPaths)
  invisible(project)
}

#' Change the literal path of one or more existing output paths
#'
#' @description Updates the OSPS-notation path string bound to existing
#'   output-path ids and persists the change immediately to the output-path
#'   definition (write-through). The ids themselves are not changed (use
#'   [removeOutputPath()] + [addOutputPath()] to rename), so
#'   every scenario that records these output paths keeps referencing them.
#'   The `project$outputPaths` accessor is read-only, so this is the way to
#'   change a path in place. The call vectorizes over a vector of ids (see the
#'   recycling rule under Details); `path` is scalar-per-entity (one path
#'   recycled to every id, or a length-`id` vector aligned by position).
#'
#' @inherit vectorizedAuthoring details
#'
#' @param project A `Project` object.
#' @param id Character vector. The output-path ids to modify. Each must
#'   already exist in `project$outputPaths`.
#' @param path Character vector of new non-empty OSPS-notation path strings,
#'   length 1 (recycled) or the same length as `id`.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family output path
setOutputPath <- function(project, id, path) {
  validateIsOfType(project, "Project")
  .assertIdVector(id)
  id <- .canonicalizeId(id)
  n <- length(id)
  missingIds <- setdiff(id, names(project$outputPaths))
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

  outputPaths <- project$.getSection("outputPaths")
  for (i in seq_len(n)) {
    outputPaths[[id[[i]]]] <- perId[[i]]
  }
  project$.setSection("outputPaths", outputPaths)
  invisible(project)
}

# Section validation adapter ----

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
