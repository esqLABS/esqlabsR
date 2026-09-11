# Tests for the read-only section wrapper (R/definition-list.R):
# `.asDefinitionList()` wraps a stored section list on the way out of a
# `Project` accessor, giving it a print method and making every assignment form
# abort. `.unwrapDefinitionList()` takes the wrapper off again before the plain
# list goes back into the store.

test_that(".asDefinitionList tags the list with its kind and class", {
  wrapped <- .asDefinitionList(list(a = 1, b = 2), "scenarios")

  expect_s3_class(wrapped, "DefinitionList")
  expect_equal(attr(wrapped, "definitionKind"), "scenarios")
  expect_named(wrapped, c("a", "b"))
})

test_that(".asDefinitionList passes a non-list through untouched", {
  expect_identical(.asDefinitionList("not a list", "scenarios"), "not a list")
  expect_identical(.asDefinitionList(NULL, "scenarios"), NULL)
})

test_that("a wrapped section still reads like the plain list", {
  wrapped <- .asDefinitionList(list(a = 1, b = 2), "individuals")

  expect_length(wrapped, 2L)
  expect_equal(wrapped[["a"]], 1)
  expect_equal(wrapped$b, 2)
  expect_named(wrapped["a"], "a")
  expect_equal(vapply(wrapped, identity, numeric(1)), c(a = 1, b = 2))
})

test_that(".unwrapDefinitionList restores the plain list", {
  plain <- list(a = 1)
  wrapped <- .asDefinitionList(plain, "scenarios")

  expect_identical(.unwrapDefinitionList(wrapped), plain)
})

test_that(".unwrapDefinitionList passes a plain list through untouched", {
  plain <- list(a = 1)

  expect_identical(.unwrapDefinitionList(plain), plain)
})

test_that(".definitionListTitle counts and pluralizes the definitions", {
  expect_equal(
    .definitionListTitle(.asDefinitionList(list(), "scenarios")),
    "scenarios (0 definitions)"
  )
  expect_equal(
    .definitionListTitle(.asDefinitionList(list(a = 1), "scenarios")),
    "scenarios (1 definition)"
  )
  expect_equal(
    .definitionListTitle(.asDefinitionList(list(a = 1, b = 2), "scenarios")),
    "scenarios (2 definitions)"
  )
})

test_that(".definitionListTitle falls back when the kind is absent", {
  bare <- list(a = 1)
  class(bare) <- c("DefinitionList", "list")

  expect_equal(.definitionListTitle(bare), "definitions (1 definition)")
})

test_that("printing a section shows its kind, count, and ids", {
  expect_snapshot(print(.asDefinitionList(list(one = 1, two = 2), "scenarios")))
})

test_that("printing an empty section still shows the header", {
  expect_snapshot(print(.asDefinitionList(list(), "populations")))
})

test_that("format returns the printed lines instead of printing them", {
  wrapped <- .asDefinitionList(list(one = 1), "scenarios")

  formatted <- expect_no_message(format(wrapped))

  expect_type(formatted, "character")
  expect_true(any(grepl("scenarios (1 definition)", formatted, fixed = TRUE)))
})

# Every assignment form into a section accessor aborts, so neither a whole
# section nor a single record can be replaced through the handle.

test_that("[[<- into a section aborts and names the section", {
  wrapped <- .asDefinitionList(list(a = 1), "scenarios")

  expect_snapshot(error = TRUE, wrapped[["a"]] <- 2)
})

test_that("$<- into a section aborts", {
  wrapped <- .asDefinitionList(list(a = 1), "scenarios")

  expect_snapshot(error = TRUE, wrapped$a <- 2)
})

test_that("[<- into a section aborts", {
  wrapped <- .asDefinitionList(list(a = 1, b = 2), "scenarios")

  expect_snapshot(error = TRUE, wrapped[1] <- list(2))
})

test_that("assigning into a section with no kind still aborts", {
  bare <- list(a = 1)
  class(bare) <- c("DefinitionList", "list")

  expect_snapshot(error = TRUE, bare[["a"]] <- 2)
})
