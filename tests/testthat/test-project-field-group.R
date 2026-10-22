# Tests for the writable field-group proxy (R/project-field-group.R), which
# backs `project$info`, `project$paths` and `project$excel`. The proxy is an
# environment of active bindings, each closing over a getter/setter pair that
# the `Project` builds from inside the group's own active binding, so external
# code reaches project state only through those closures.

# A spec whose closures read and write a plain environment, standing in for the
# `private` a real `Project` hands its groups.
localSpecStore <- function() {
  store <- new.env(parent = emptyenv())
  store$name <- "initial"
  store$version <- "2.0"
  store$writes <- 0L
  store
}

test_that("a group field reads through its getter", {
  store <- localSpecStore()
  group <- .projectFieldGroup(
    spec = list(name = list(get = \() store$name, set = \(value) NULL)),
    group = "info",
    printer = \() invisible(NULL),
    owner = store
  )

  expect_equal(group$name, "initial")
})

test_that("assigning a group field calls its setter", {
  store <- localSpecStore()
  group <- .projectFieldGroup(
    spec = list(
      name = list(
        get = \() store$name,
        set = function(value) {
          store$name <- value
          store$writes <- store$writes + 1L
        }
      )
    ),
    group = "info",
    printer = \() invisible(NULL),
    owner = store
  )

  group$name <- "changed"

  expect_equal(store$name, "changed")
  # The setter runs the side effect (a real one flips the project's dirty bit).
  expect_equal(store$writes, 1L)
})

test_that("a handle taken from the group still writes through live state", {
  store <- localSpecStore()
  group <- .projectFieldGroup(
    spec = list(
      name = list(get = \() store$name, set = \(value) store$name <- value)
    ),
    group = "info",
    printer = \() invisible(NULL),
    owner = store
  )

  handle <- group
  handle$name <- "via handle"

  expect_equal(group$name, "via handle")
  expect_equal(store$name, "via handle")
})

test_that("each field keeps its own closures rather than sharing the last", {
  # The accessor is built outside the loop for exactly this reason: closures
  # created inline in a `for` loop would all capture the same loop variable and
  # every field would resolve to the last one.
  store <- localSpecStore()
  group <- .projectFieldGroup(
    spec = list(
      name = list(get = \() store$name, set = \(value) store$name <- value),
      version = list(
        get = \() store$version,
        set = \(value) store$version <- value
      )
    ),
    group = "info",
    printer = \() invisible(NULL),
    owner = store
  )

  expect_equal(group$name, "initial")
  expect_equal(group$version, "2.0")

  group$version <- "3.0"

  expect_equal(group$version, "3.0")
  expect_equal(group$name, "initial")
})

test_that("a field with no setter is read-only and names itself when assigned", {
  store <- localSpecStore()
  group <- .projectFieldGroup(
    spec = list(schemaVersion = list(get = \() store$version, set = NULL)),
    group = "info",
    printer = \() invisible(NULL),
    owner = store
  )

  expect_equal(group$schemaVersion, "2.0")
  expect_snapshot(error = TRUE, group$schemaVersion <- "3.0")
})

test_that("a group can supply its own read-only handler", {
  # The `definitions` group passes `.definitionListReadOnlyError` so its message
  # points at the authoring functions instead of naming the field.
  store <- localSpecStore()
  group <- .projectFieldGroup(
    spec = list(scenarios = list(get = \() list(), set = NULL)),
    group = "definitions",
    printer = \() invisible(NULL),
    owner = store,
    onReadOnly = \(field) .definitionListReadOnlyError(field)
  )

  expect_snapshot(error = TRUE, group$scenarios <- list())
})

test_that("the group carries its name, printer and owner", {
  store <- localSpecStore()
  printer <- \() cat("rendered\n")
  group <- .projectFieldGroup(
    spec = list(name = list(get = \() store$name, set = NULL)),
    group = "paths",
    printer = printer,
    owner = store
  )

  expect_s3_class(group, "ProjectFieldGroup")
  expect_equal(attr(group, "group"), "paths")
  # Compared by identity so a proxy from another project instance is rejected
  # rather than swallowed as this one's own write-back.
  expect_identical(attr(group, "owner"), store)
})

test_that("printing a group delegates to the printer it was built with", {
  store <- localSpecStore()
  group <- .projectFieldGroup(
    spec = list(name = list(get = \() store$name, set = NULL)),
    group = "paths",
    printer = \() cat("rendered by the project\n"),
    owner = store
  )

  expect_output(print(group), "rendered by the project")
})

test_that("format returns the printed lines instead of printing them", {
  store <- localSpecStore()
  group <- .projectFieldGroup(
    spec = list(name = list(get = \() store$name, set = NULL)),
    group = "paths",
    printer = \() cat("rendered by the project\n"),
    owner = store
  )

  expect_equal(format(group), "rendered by the project")
})

test_that(".projectFieldReadOnlyError names the field and its group", {
  expect_snapshot(error = TRUE, .projectFieldReadOnlyError("name", "info"))
})
