context("Map")

test_that("It throws an error when keys and values have different lengths", {
  keys <- c("a", "b", "c")
  values <- c(4, 5, 6, 7)

  expect_error(map(keys, values), regexp = escapeForRegex("Arguments 'keys, values' must have the same length, but they don't!"))
})

test_that("It throws an error if not all keys are unique", {
  keys <- c("a", "b", "b")
  values <- c(5, 6, 7)

  expect_error(map(keys, values), regexp = escapeForRegex(messages$errorMapKeysNotUnique(keys)))
})

test_that("It can create a Map", {
  keys <- c("a", "b", "c", "d")
  values <- c(4, 5, 6, 7)
  myMap <- map(keys, values)

  expect_equal(names(myMap), keys)
})

test_that("Map get key", {
  keys <- c("a", "b", "c", "d")
  values <- c(4, 5, 6, 7)
  myMap <- map(keys, values)

  expect_equal(mapGetKey(myMap, 2), NULL)
  expect_equal(mapGetKey(myMap, 4), "a")
})

test_that("Map get value", {
  keys <- c("a", "b", "c", "d")
  values <- c(4, 5, 6, 7)
  myMap <- map(keys, values)

  expect_error(mapGetValue(myMap, 2), regexp = escapeForRegex(ospsuite:::messages$errorKeyNotInEnum(2)))
  expect_equal(mapGetValue(myMap, "b"), 5)
})
