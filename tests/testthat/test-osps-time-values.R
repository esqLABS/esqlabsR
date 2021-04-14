context("OSPSTimeValues")

test_that("It can create a OSPSTimeValues object with Y error", {
  xVals <- c(0, 1, 2, 3)
  yVals <- c(4, 5, 6, 7)
  yError <- c(0.1, 0, 0.1, 1)
  xyData <- OSPSTimeValues$new(xVals, yVals, yError = yError, label = "My XY Data")
  expect_equal(xyData$xValues, xVals)
  expect_equal(xyData$yValues, yVals)
  expect_equal(xyData$yError, yError)

  expect_error(capture.output(print(xyData)), NA)
})
