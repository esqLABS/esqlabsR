# ProjectFieldGroup: a writable, printable proxy for a group of related
# `Project` fields (`project$info`, `project$paths`, `project$excel`).
#
# The `Project` object gathers its ~30 flat accessors into four named groups so
# the surface reads as `project$info$name`, `project$paths$modelFolder`,
# `project$excel$modelParamsFile` instead of a flat wall of bindings. Three of
# those groups (`info`, `paths`, `excel`) carry writable fields; this proxy
# backs them. The read-only `definitions` group reuses `DefinitionList` instead.
#
# Encapsulation. R6 locks an instance environment, so a `Project` cannot grow
# per-instance active bindings for the group fields, and a free function in the
# package cannot reach `private$` (there is no package-private scope). The proxy
# resolves both: `Project` builds it from within the group's active binding,
# handing each field a pair of closures that capture `private`. External code
# holds only the proxy, never `private`; the proxy's closures are the only path
# from the proxy back to the project's state. There is no `.__enclos_env__`
# reach and no public setter method for the proxy to call.
#
# Mechanism. The proxy is a plain environment (not R6, which cannot take a
# per-instance field set) whose fields are active bindings created with
# `makeActiveBinding()`. Reading `group$field` calls the field's getter closure;
# assigning `group$field <- value` calls its setter closure (which writes
# `private$` and runs the project's invalidation side effect). A field declared
# read-only (its spec `set` is `NULL`) aborts on assignment. Because the
# closures capture `private` (a reference), a handle taken from one access still
# writes through live state: `x <- project$info; x$name <- "y"` mutates the
# project. `Project` returns a fresh proxy on each group access; the proxy holds
# no state of its own beyond the closures.

# Build a writable field-group proxy from a spec.
#
# `spec` is an ordered named list; each element is `list(get = <fn()>, set =
# <fn(value)> or NULL)`. `get` returns the field's value; `set` writes it (and
# is responsible for any side effect, e.g. the project's dirty bit). A `NULL`
# `set` marks the field read-only: assignment aborts. `group` is the group name
# shown in the abort message. `printer` is a zero-argument function that renders
# the group (the `Project` supplies one that reproduces the group's section of
# `Project$print()`); it is called by `print.ProjectFieldGroup`. `onReadOnly` is
# the handler called when a read-only field is assigned; it receives the field
# name and must abort. The default names the field and its group; the
# `definitions` group passes `.definitionListReadOnlyError` so its message
# points at the authoring functions.
#
# @keywords internal
# @noRd
.projectFieldGroup <- function(
  spec,
  group,
  printer,
  onReadOnly = function(field) .projectFieldReadOnlyError(field, group)
) {
  env <- new.env(parent = emptyenv())
  for (field in names(spec)) {
    makeActiveBinding(
      field,
      .projectFieldAccessor(
        spec[[field]]$get,
        spec[[field]]$set,
        field,
        onReadOnly
      ),
      env
    )
  }
  attr(env, "group") <- group
  attr(env, "printer") <- printer
  class(env) <- c("ProjectFieldGroup", "environment")
  env
}

# Build one field's active-binding function, closing over its getter/setter, its
# name, and the read-only handler. Split out of the loop so each field captures
# its own closures (a closure created inline in a `for` loop would share the
# loop variable).
#
# @keywords internal
# @noRd
.projectFieldAccessor <- function(getter, setter, field, onReadOnly) {
  force(getter)
  force(setter)
  force(field)
  force(onReadOnly)
  function(value) {
    if (missing(value)) {
      return(getter())
    }
    if (is.null(setter)) {
      onReadOnly(field)
    }
    setter(value)
    invisible(value)
  }
}

# Abort an assignment into a read-only group field. Names the field and its
# group so the message is specific (e.g. `info$schemaVersion` is read-only).
# `call = NULL`: the abort fires from inside an active-binding setter, whose
# call frame is the internal accessor closure, not a user-facing function, so
# naming it would be misleading; the message stands on its own.
#
# @keywords internal
# @noRd
.projectFieldReadOnlyError <- function(field, group, call = NULL) {
  cli::cli_abort(
    "{.field {group}${field}} is read-only and cannot be assigned into.",
    call = call
  )
}

# The group carries a project-supplied renderer that reproduces the block
# `Project$print()` shows for this group (same labels, headers, and resolved
# values). Both prints call it, so the whole-project summary and a group's own
# print never drift.
#' @exportS3Method
#' @noRd
print.ProjectFieldGroup <- function(x, ...) {
  attr(x, "printer")()
  invisible(x)
}

#' @exportS3Method
#' @noRd
format.ProjectFieldGroup <- function(x, ...) {
  utils::capture.output(print(x, ...))
}
