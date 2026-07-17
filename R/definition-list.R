# DefinitionList: a read-only, printable wrapper for a project section accessor.
#
# A section accessor (`project$individuals`, `project$scenarios`, ...) returns
# the stored plain named list keyed by id, wrapped in a lightweight
# `c("DefinitionList", "list")` (carrying the section's kind name as an
# attribute) on the way OUT of the active-binding getter. The wrapper does two
# things: it gives the section a print method that shows a count and the
# definition ids, and it makes the section read-only from the handle. The
# accessor's active-binding setter aborts, and the `[[<-` / `$<-` / `[<-`
# replacement methods below abort too, so neither `project$x <- v` nor
# `project$x[["id"]] <- v` nor the nested `project$x[["id"]]$f <- v` can mutate
# the project. The only sanctioned writes are the authoring methods on
# `Project` (which the exported authoring functions forward to); they go through
# the private write seam `private$.setSection()` and read the plain backing list
# via `private$.getSection()`, never this wrapper.
#
# For reads the wrapper is transparent: it is `c("DefinitionList", "list")`, so
# `length()`, `names()`, `[[`, `[`, `c()`, and iteration all dispatch to the
# list defaults. Only `print` / `format` and the aborting assignment operators
# are specialized.

# Wrap a stored section list in the printable `DefinitionList` class, tagging
# it with the section kind name (shown in the print header). Returns the value
# unchanged for a non-list (defensive; every section is a list).
#
# @keywords internal
# @noRd
.asDefinitionList <- function(x, kind) {
  if (!is.list(x)) {
    return(x)
  }
  attr(x, "definitionKind") <- kind
  class(x) <- c("DefinitionList", "list")
  x
}

# Strip the `DefinitionList` wrapper, returning the plain stored list. A setter
# calls this so the backing store never carries the print-only class. A value
# that is not a `DefinitionList` (the common case: a freshly built list) passes
# through unchanged.
#
# @keywords internal
# @noRd
.unwrapDefinitionList <- function(x) {
  if (inherits(x, "DefinitionList")) {
    attr(x, "definitionKind") <- NULL
    class(x) <- setdiff(class(x), "DefinitionList")
  }
  x
}

#' @exportS3Method
#' @noRd
print.DefinitionList <- function(x, ...) {
  ospsuite.utils::ospPrintClass(x)
  ospsuite.utils::ospPrintItems(
    names(x),
    title = .definitionListTitle(x),
    print_empty = TRUE
  )
  invisible(x)
}

#' @exportS3Method
#' @noRd
format.DefinitionList <- function(x, ...) {
  utils::capture.output(print(x, ...))
}

# Build the `ospPrintItems` title line for a section accessor: the section
# kind and the count of definitions it holds (e.g. `individuals (2
# definitions)`), pluralized. Falls back to the generic `definitions` when the
# kind attribute is absent.
#
# @keywords internal
# @noRd
.definitionListTitle <- function(x) {
  kind <- attr(x, "definitionKind") %||% "definitions"
  count <- length(x)
  paste0(
    kind,
    " (",
    count,
    " definition",
    if (count == 1L) "" else "s",
    ")"
  )
}

# Abort an assignment into a section accessor. A section accessor
# (`project$scenarios`, ...) is read-only from the handle: the only sanctioned
# way to change a definition is an authoring function (`addScenario()` /
# `setScenario()` / `removeScenario()` and their per-section siblings) or
# editing the definition's JSON file directly. Every assignment form into the
# accessor (`project$x <- v`, `project$x[["id"]] <- v`, the nested
# `project$x[["id"]]$field <- v`, `project$x[-i] <- v`) routes through one of
# the `DefinitionList` replacement methods below, which call this. Naming the
# section kind keeps the message specific without a brittle kind->function map.
#
# @keywords internal
# @noRd
.definitionListReadOnlyError <- function(
  kind = NULL,
  call = rlang::caller_env()
) {
  section <- if (is.null(kind)) {
    "This project section"
  } else {
    paste0("{.field ", kind, "}")
  }
  cli::cli_abort(
    c(
      paste0(section, " is read-only and cannot be assigned into."),
      "i" = "To change a definition, edit its {.file .json} file or use an \\
      authoring function (e.g. {.fn addScenario} / {.fn setScenario} / \\
      {.fn removeScenario} and their per-section siblings).",
      "i" = "To edit one record, read it, change the copy, then re-submit it \\
      with an authoring function: {.code sc <- project$scenarios[[\"id\"]]; \\
      sc$field <- value; setScenario(project, \"id\", ...)}."
    ),
    call = call
  )
}

# Operator replacement S3 methods use `@export` (not `@exportS3Method`): under
# roxygen2 8.0.0 only the former quotes the non-syntactic generic in the
# `S3method()` directive (`S3method("$<-", DefinitionList)`); the bare
# `@exportS3Method` emits an unquoted directive that the NAMESPACE parser
# rejects. roxygen still detects these as S3 methods, so no `export()` is added.

#' @export
#' @noRd
`[[<-.DefinitionList` <- function(x, ..., value) {
  .definitionListReadOnlyError(attr(x, "definitionKind"))
}

#' @export
#' @noRd
`$<-.DefinitionList` <- function(x, ..., value) {
  .definitionListReadOnlyError(attr(x, "definitionKind"))
}

#' @export
#' @noRd
`[<-.DefinitionList` <- function(x, ..., value) {
  .definitionListReadOnlyError(attr(x, "definitionKind"))
}
