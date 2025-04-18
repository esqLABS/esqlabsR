test_that("It returns NULL if NULL is provided for a list", {
  myList <- c()
  entry <- "one"

  expect_null(removeFromList(entry, listArg = myList))
})

test_that("It does nothing to an empty list", {
  myList <- list()
  entry <- "one"

  expect_equal(removeFromList(entry, listArg = myList), list())
})

test_that("It does nothing if an entry is not in the list", {
  myList <- list("a", "b", "c")
  entry <- "one"

  expect_equal(removeFromList(entry, listArg = myList), myList)
})

test_that("It removes one entry from a vector of primitives", {
  myList <- c("a", "b", "c")
  entry <- "b"

  expect_equal(removeFromList(entry, listArg = myList), list("a", "c"))

  myList <- c(1, 2, 3)
  entry <- 2

  expect_equal(removeFromList(entry, listArg = myList), list(1, 3))
})

test_that("It removes one entry from a list of primitives", {
  myList <- list("a", "b", "c")
  entry <- "b"

  expect_equal(removeFromList(entry, listArg = myList), list("a", "c"))

  myList <- list(1, 2, 3)
  entry <- 2

  expect_equal(removeFromList(entry, listArg = myList), list(1, 3))
})

test_that("It returns the lowest value without a threshold", {
  array <- c(-1, 0, 1, 2, 3)
  value <- 0.9

  expect_equal(getIndexClosestToValue(value = value, array = array), 3)
})

test_that("It returns multiple indeces for multiple lowest values without a threshold", {
  array <- c(-1, 0, 1, 2, 3)
  value <- -0.5

  expect_equal(getIndexClosestToValue(value = value, array = array), c(1, 2))
})

test_that("It returns the lowest value with an absolute threshold", {
  array <- c(-1, 0, 1, 2, 3)
  value <- 0.9

  expect_equal(getIndexClosestToValue(value = value, array = array, thresholdAbs = 0.1), 3)
})

test_that("It returns multiple indeces for multiple lowest values with an absolute", {
  array <- c(-1, 0, 1, 2, 3)
  value <- -0.5

  expect_equal(getIndexClosestToValue(value = value, array = array, thresholdAbs = 0.6), c(1, 2))
})

test_that("It returns NULL if the distance is higher than the absolute threshold", {
  array <- c(-1, 0, 1, 2, 3)
  value <- -0.5

  suppressWarnings(expect_equal(getIndexClosestToValue(value = value, array = array, thresholdAbs = 0.4), NULL))
})

test_that("It returns multiple indeces for multiple lowest values with a relative threshold", {
  array <- c(-1, 0, 1, 2, 3)
  value <- -0.5

  expect_equal(getIndexClosestToValue(value = value, array = array, thresholdRel = 2), c(1, 2))
})

test_that("It returns NULL if the distance is higher than the absolute threshold", {
  array <- c(-1, 0, 1, 2, 3)
  value <- -0.5

  suppressWarnings(expect_null(getIndexClosestToValue(value = value, array = array, thresholdRel = 0.9)))
})

test_that("It only finds exact matches for absolute threshold = 0", {
  array <- c(-1, 0, 1, 2, 3)
  value <- -0.5

  suppressWarnings(expect_null(getIndexClosestToValue(value = value, array = array, thresholdAbs = 0)))
  value <- 1
  expect_equal(getIndexClosestToValue(value = value, array = array, thresholdAbs = 0), 3)
})

test_that("It only finds exact matches for relative threshold = 0", {
  array <- c(-1, 0, 1, 2, 3)
  value <- -0.5

  suppressWarnings(expect_equal(getIndexClosestToValue(value = value, array = array, thresholdRel = 0), NULL))
  value <- 1
  suppressWarnings(expect_equal(getIndexClosestToValue(value = value, array = array, thresholdRel = 0), 3))
})

test_that("It finds a 0 without a threshold", {
  array <- c(-1, 0, 1, 2, 3)
  value <- 0

  expect_equal(getIndexClosestToValue(value = value, array = array), 2)
})

test_that("It finds a 0 with absolute threshold", {
  array <- c(-1, 0, 1, 2, 3)
  value <- 0

  expect_equal(getIndexClosestToValue(value = value, array = array, thresholdAbs = 0.1), 2)
})

test_that("It finds a 0 with relative threshold", {
  array <- c(-1, 0, 1, 2, 3)
  value <- 0

  expect_equal(getIndexClosestToValue(value = value, array = array, thresholdRel = 10), 2)
})

test_that("`compareWithNA()` works as expected", {
  res <- compareWithNA(
    c(NA, "a", "b", NA),
    c(NA, "c", "b", "a")
  )

  expect_equal(res, c(TRUE, FALSE, TRUE, FALSE))
})
