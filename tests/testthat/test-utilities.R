context("removeFromList")

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
