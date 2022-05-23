## context("utilities-figures")
test_that("It returns two colors", {
  expect_equal(length(esqLABS_colors(2)), 2)
})

test_that("It returns ten colors", {
  expect_equal(length(esqLABS_colors(10)), 10)
})
