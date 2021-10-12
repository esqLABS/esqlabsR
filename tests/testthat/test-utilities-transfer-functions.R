
test_that("transfer functions work as expected", {
  expect_equal(hillFunction(0.2, 0.4, 0.5), 0.1142857, tolerance = 0.001)
  expect_equal(foldChangeFunction(0.4, 0.5), 0.8, tolerance = 0.001)
  expect_equal(sineFunction(2, 0.5, 1, 0.1, 0.6), 0.8938926, tolerance = 0.001)
})
