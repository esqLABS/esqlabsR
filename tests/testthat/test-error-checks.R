context("isOfType")

test_that("It returns TRUE for the correct type of one entry", {
  object <- "one"
  type <- "character"

  expect_true(isOfType(object, type))
})

test_that("It returns FALSE for the wrong type of one entry", {
  object <- 2
  type <- "character"

  expect_false(isOfType(object, type))
})

test_that("It returns TRUE for the correct type of multiple entries", {
  object <- list("one", "two")
  type <- "character"

  expect_true(isOfType(object, type))
})

test_that("It returns FALSE for the wrong type of multiple entries", {
  object <- list("one", 2)
  type <- "character"

  expect_false(isOfType(object, type))
})

test_that("It returns FALSE for NULL if nullAllowed = FALSE", {
  object <- NULL
  type <- "character"

  expect_false(isOfType(object, type))
})

test_that("It returns TRUE for NULL if nullAllowed = TRUE", {
  object <- NULL
  type <- "character"

  expect_true(isOfType(object, type, nullAllowed = TRUE))
})

test_that("It returns FALSE if one of objects is NULL and nullAllowed = FALSE", {
  object <- list("one", NULL, "three")
  type <- "character"

  expect_false(isOfType(object, type))
})

test_that("It returns TRUE if one of objects is NULL and nullAllowed = TRUE", {
  object <- list("one", NULL, "three")
  type <- "character"

  expect_true(isOfType(object, type, nullAllowed = TRUE))
})
