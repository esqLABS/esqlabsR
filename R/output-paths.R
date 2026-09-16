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
#' @param overwrite Logical scalar. When `FALSE` (default), an id that already
#'   exists aborts. When `TRUE`, the existing output path is replaced
#'   (last-write-wins).
#' @returns The `project` object, invisibly.
#' @export
#' @family outputPath
addOutputPath <- function(project, id, path, overwrite = FALSE) {
  validateIsOfType(project, "Project")
  project$addOutputPath(id, path, overwrite)
}

# Implementation behind `project$addOutputPath()` / `addOutputPath()`.
#
# @keywords internal
# @noRd
.addOutputPath_impl <- function(
  self,
  private,
  id,
  path,
  overwrite = FALSE,
  .call
) {
  rlang::local_error_call(.call)
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
  if (anyNA(path) || any(nchar(path) == 0)) {
    cli::cli_abort(c(
      "Cannot add outputPath:",
      "x" = "path must contain non-empty strings"
    ))
  }
  # A within-batch duplicate id or an existing id aborts unless overwriting, in
  # which case the last entry for that id wins.
  .assertNoOverwriteClash(
    id,
    names(self$definitions$outputPaths),
    "outputPath",
    overwrite
  )

  # Recycle a single path to every id (the scalar-per-definition rule).
  if (length(path) == 1L) {
    path <- rep(path, length(id))
  }
  # Assign each id's path by key so an existing id is replaced in place and an
  # in-batch repeat keeps the last value (both only reachable when overwriting).
  outputPaths <- private$.getSection("outputPaths") %||% list()
  for (i in seq_along(id)) {
    outputPaths[[id[[i]]]] <- path[[i]]
  }
  private$.setSection("outputPaths", outputPaths)
  invisible(self)
}

#' Remove one or more output paths from a Project
#'
#' Drop the output paths with matching ids in one in-memory edit. Warns (and
#' skips) any id not present, and warns when a removed output path is still
#' referenced.
#'
#' @param project A `Project` object.
#' @param id Character vector of output-path ids to remove. Each is
#'   canonicalized the same way [addOutputPath()] canonicalizes it.
#' @returns The `project` object, invisibly.
#' @export
#' @family outputPath
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
#'   output-path ids, in memory; write the change to the output-path definition
#'   files with [saveProject()]. The ids themselves are not changed (use
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
#' @family outputPath
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
    if (!.isNonEmptyString(one)) {
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

# Output-path reference matching ----

# Which output-path ids does a `path` value name?
#
# A DataCombined simulated entry's `path` carries either a literal model
# quantity path or an output-path id. The rules below settle which:
#   * a value carrying the OSP separator `|` is a model path in every case, so
#     it is never compared against ids (id canonicalization replaces `|`, so no
#     id can hold one);
#   * an exact key match names that one id;
#   * otherwise the comparison is the canonical one every other project
#     reference resolves with, so `Aciclovir_PVB` finds the stored
#     `aciclovir_pvb`.
#
# Returns zero, one, or (only from a hand-edited tree holding two ids that
# canonicalize alike) several ids.
#
# @keywords internal
# @noRd
.matchOutputPathIds <- function(path, outputPaths) {
  if (
    is.null(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(as.character(path))
  ) {
    return(character(0))
  }
  path <- as.character(path)
  if (grepl("|", path, fixed = TRUE)) {
    return(character(0))
  }
  ids <- names(outputPaths %||% list())
  if (length(ids) == 0L) {
    return(character(0))
  }
  if (path %in% ids) {
    return(path)
  }
  ids[.canonicalizeForCompare(ids) == .canonicalizeForCompare(path)]
}

# Resolve a `path` value to the literal model path to plot.
#
# Returns a list of the literal `path` and `fromId`, the id it was resolved
# from (`NULL` when the value is used verbatim). A value naming more than one id
# is ambiguous and aborts.
#
# @keywords internal
# @noRd
.resolveOutputPathValue <- function(
  path,
  outputPaths,
  call = rlang::caller_env()
) {
  matched <- .matchOutputPathIds(path, outputPaths)
  if (length(matched) == 0L) {
    return(list(path = path, fromId = NULL))
  }
  if (length(matched) > 1L) {
    cli::cli_abort(
      messages$ambiguousOutputPathRef(path, matched),
      call = call
    )
  }
  list(path = outputPaths[[matched]], fromId = matched)
}
