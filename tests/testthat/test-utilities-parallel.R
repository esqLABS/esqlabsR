test_that("`executeInParallel()` works as expected", {
  v1 <- 1:4
  v2 <- 8:100

  x <- executeInParallel("mean", list(v1, v2), outputNames = c("res1", "res2"))

  expect_equal(x[["res1"]], mean(v1), tolerance = 0.01)
  expect_equal(x[["res2"]], mean(v2), tolerance = 0.01)

  expect_error(executeInParallel("log", list(letters[1:4], LETTERS[1:4])))
})
