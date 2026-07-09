# Shared vectorization engine for the authoring API.
#
# Every `add*` / `set*` function accepts a vector of ids and vectorizes over
# them under one recycling rule (locked 2026-06-30):
#
#   1. `project` is always length 1. The `id` argument sets N (the entity
#      count) and CANNOT be recycled: when any scalar field has length > 1,
#      `id` must have that same length. A length-1 `id` with all-scalar fields
#      is the ordinary single-entity call (N = 1).
#   2. Every scalar-per-entity field is length 1 (recycled to all N) or length
#      N (aligned by position). Any other length is an error naming the field
#      and the lengths.
#   3. A vector-valued-per-entity field (an individual's / application's
#      `parameterSets`, a scenario's `outputPaths` / `parameterSets`)
#      is applied WHOLE to every entity, never split positionally. To give a
#      different multi-valued list per entity, the caller passes a length-N
#      list (one vector per entity); anything else is the one value applied to
#      every entity. Whole fields are exempt from the length check.
#   4. All-or-nothing: the caller validates all N entities first and writes
#      nothing on any failure, then folds all N into the section and triggers
#      exactly one write-through.
#
# Two families of authoring functions sit outside this id-sets-N rule.
# `addParameterEntry()` / `removeParameterEntry()` vectorize over parameter
# entries (parallel `containerPath` / `parameterName` / `value` / `units`
# vectors) for a single named set, a different axis than the id-sets-N rule.
# `renameScenario()`, `duplicateScenario()`, `addObservedData()`, and the
# parameter-identification adders (`addPITask()` and its per-task sub-entity
# helpers) act on a single definition per call.
#
# This file holds the pure recycle/align/length-check core. It is project-free
# and unit-testable in isolation. The plot mutators in R/plots.R carry their own
# specialized variant (`.recycleScalarArg` / `.dotsToPerEntityFields`), which
# predates this engine; the two share the same rule.

#' Vectorized authoring (the recycling rule)
#'
#' @description Every `add*` / `set*` / `remove*` function in the authoring
#'   API accepts a vector of ids and vectorizes over them in one call (and one
#'   write to disk).
#'
#' @details
#' The id argument sets `N`, the number of definitions to act on, and cannot
#' itself be recycled: when any scalar-per-entity field has length greater than
#' 1, the id vector must have that same length. A length-1 id with all-scalar
#' fields is the ordinary single-definition call.
#'
#' Each scalar-per-entity field is either length 1 (recycled to all `N`
#' definitions) or length `N` (aligned to the ids by position). Any other
#' length is an error naming the field and the lengths.
#'
#' A vector-valued-per-entity field (an individual's or application's
#' `parameterSets`, a scenario's `outputPaths` and `parameterSets`) is
#' applied whole to every definition, never split positionally. To give a
#' different multi-valued list per definition, pass a list of the same length
#' as the id vector (one vector per definition).
#'
#' The call is all-or-nothing: every definition is validated first, and if any
#' fails the whole call aborts and writes nothing. On success all definitions
#' are folded into the section and persisted in a single write-through.
#'
#' Two families of authoring functions sit outside this id-sets-`N` rule.
#' `addParameterEntry()` and `removeParameterEntry()` vectorize over parameter
#' entries (parallel `containerPath` / `parameterName` / `value` / `units`
#' vectors) within a single named set, a different axis than the id-sets-`N`
#' rule described here. `renameScenario()`, `duplicateScenario()`,
#' `addObservedData()`, `addPITask()`, and the per-task
#' parameter-identification sub-entity helpers act on a single definition per
#' call.
#'
#' @name vectorizedAuthoring
#' @keywords internal
NULL

# Validate the `id` vector that sets N for a vectorized authoring call: a
# non-empty character vector with no NA / empty element. Returns the value
# unchanged. The caller canonicalizes ids separately. `arg` names the argument
# in the abort, `call` attributes it to the public caller.
#
# @keywords internal
# @noRd
.assertIdVector <- function(id, arg = "id", call = rlang::caller_env()) {
  if (
    !is.character(id) ||
      length(id) == 0L ||
      any(is.na(id)) ||
      any(nchar(id) == 0)
  ) {
    cli::cli_abort(
      "{.arg {arg}} must be a non-empty character vector with no NA or \\
      empty element.",
      call = call
    )
  }
  invisible(id)
}

# Abort when a batch `add*` id vector repeats the same (canonical) id, naming
# the offenders. Batch `add*` functions fold each entry into the section keyed
# by its id, so a repeated id would silently overwrite the earlier entry rather
# than add a distinct one. `.canonicalizeId()` deliberately lets an identical
# repeat through (it aborts only on distinct pre-images that collapse together),
# so the within-batch guard is the caller's responsibility. Call after
# canonicalization so the check runs on the ids actually used as keys.
#
# @keywords internal
# @noRd
.assertNoDuplicateIds <- function(id, entity, call = rlang::caller_env()) {
  if (anyDuplicated(id) > 0L) {
    cli::cli_abort(
      "duplicate {entity} id{?s} in the batch: {.val {id[duplicated(id)]}}",
      call = call
    )
  }
  invisible(id)
}

# Recycle / align one scalar-per-entity field to N entities. A length-1 value
# is recycled to all N; a length-N value is aligned by position; any other
# length aborts naming the field and the lengths. `NULL` passes through as
# `NULL` (an absent field stays absent, recycled to all N). A list of length N
# also aligns (so a per-entity scalar can be given as a length-N list).
#
# @keywords internal
# @noRd
.recycleField <- function(value, n, field, call = rlang::caller_env()) {
  if (is.null(value)) {
    return(rep(list(NULL), n))
  }
  len <- length(value)
  if (len == 1L) {
    return(rep(list(.element(value, 1L)), n))
  }
  if (len == n) {
    return(lapply(seq_len(n), function(i) .element(value, i)))
  }
  cli::cli_abort(
    c(
      "{.arg {field}} must be length 1 or length {n} (the number of ids).",
      "x" = "It is length {len}."
    ),
    call = call
  )
}

# Resolve a whole-vector-per-entity field to N per-entity values. A length-N
# list aligns by position (one element per entity); any other value (a scalar,
# an atomic vector, or `NULL`) is applied verbatim to every entity. This is the
# "applied whole, never split positionally" rule for vector-valued fields.
#
# @keywords internal
# @noRd
.wholeField <- function(value, n) {
  if (is.list(value) && length(value) == n) {
    return(value)
  }
  rep(list(value), n)
}

# Align an id vector + named scalar/whole field lists into N per-entity field
# sets. `scalarFields` and `wholeFields` are named lists of argument values;
# each scalar field is recycled/aligned (rule 2), each whole field applied
# verbatim (rule 3). Returns a list of N named lists, one per entity, each
# carrying every supplied field (NULL fields preserved as NULL so an `add*`
# builder can tell "absent" from "present"). `call` attributes any length-error
# abort to the public caller.
#
# @keywords internal
# @noRd
.alignAuthoringArgs <- function(
  id,
  scalarFields = list(),
  wholeFields = list(),
  call = rlang::caller_env()
) {
  n <- length(id)
  perScalar <- lapply(names(scalarFields), function(nm) {
    .recycleField(scalarFields[[nm]], n, nm, call = call)
  })
  names(perScalar) <- names(scalarFields)
  perWhole <- lapply(names(wholeFields), function(nm) {
    .wholeField(wholeFields[[nm]], n)
  })
  names(perWhole) <- names(wholeFields)

  lapply(seq_len(n), function(i) {
    fields <- list()
    for (nm in names(perScalar)) {
      fields[nm] <- list(perScalar[[nm]][[i]])
    }
    for (nm in names(perWhole)) {
      fields[nm] <- list(perWhole[[nm]][[i]])
    }
    fields
  })
}

# Extract element `i` of a vector or list, preserving the scalar shape. Used by
# `.recycleField` so a character vector yields a length-1 string and a list
# yields its i-th element.
#
# @keywords internal
# @noRd
.element <- function(value, i) {
  if (is.list(value)) {
    return(value[[i]])
  }
  value[[i]]
}
