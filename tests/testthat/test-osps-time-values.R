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

test_that("Correct conversion factor between units for y values", {
  xVals <- c(0, 1, 2, 3)
  yVals <- c(4, 5, 6, 7)
  yError <- c(0.1, 0, 0.1, 1)
  xyData <- OSPSTimeValues$new(xVals, yVals, yError = yError, label = "My XY Data")
  xyData$MW <- 300

  # No conversion
  xyData$yDimension <- Dimensions$`Concentration (molar)`
  yFactor <- xyData$yUnitDimensionFactor(dimension = Dimensions$`Concentration (molar)`, unit = "µmol/l")
  expect_equal(yFactor, 1)
  # µmol/l to mol/l
  yFactor <- xyData$yUnitDimensionFactor(dimension = Dimensions$`Concentration (molar)`, unit = "mol/l")
  expect_equal(yFactor, 1e-6)

  # Molar to mass concentration
  yFactor <- xyData$yUnitDimensionFactor(dimension = Dimensions$`Concentration (mass)`, unit = "µg/l")
  expect_equal(yFactor, 300)

  # Mass to molar concentration
  xyData$yDimension <- Dimensions$`Concentration (mass)`
  yFactor <- xyData$yUnitDimensionFactor(dimension = Dimensions$`Concentration (molar)`, unit = "µmol/l")
  expect_equal(yFactor, 1e3 / xyData$MW * 1e6)
  xyData$yUnit <- "mg/l"
  yFactor <- xyData$yUnitDimensionFactor(dimension = Dimensions$`Concentration (molar)`, unit = "µmol/l")
  expect_equal(yFactor, 1e-3 / xyData$MW * 1e6)

  expect_error(capture.output(print(xyData)), NA)
})

test_that("Correct conversion factor between units for y errror values", {
  xVals <- c(0, 1, 2, 3)
  yVals <- c(4, 5, 6, 7)
  yError <- c(0.1, 0, 0.1, 1)
  xyData <- OSPSTimeValues$new(xVals, yVals, yError = yError, label = "My XY Data")
  xyData$MW <- 300

  # No conversion
  xyData$yDimension <- Dimensions$`Concentration (molar)`
  yFactor <- xyData$yErrorUnitDimensionFactor(dimension = Dimensions$`Concentration (molar)`, unit = "µmol/l")
  expect_equal(yFactor, 1)
  # µmol/l to mol/l
  yFactor <- xyData$yErrorUnitDimensionFactor(dimension = Dimensions$`Concentration (molar)`, unit = "mol/l")
  expect_equal(yFactor, 1e-6)

  # Molar to mass concentration
  yFactor <- xyData$yErrorUnitDimensionFactor(dimension = Dimensions$`Concentration (mass)`, unit = "µg/l")
  expect_equal(yFactor, 300)

  # Mass to molar concentration
  xyData$yDimension <- Dimensions$`Concentration (mass)`
  yFactor <- xyData$yErrorUnitDimensionFactor(dimension = Dimensions$`Concentration (molar)`, unit = "µmol/l")
  expect_equal(yFactor, 1e3 / xyData$MW * 1e6)
  xyData$yErrorUnit <- "mg/l"
  yFactor <- xyData$yErrorUnitDimensionFactor(dimension = Dimensions$`Concentration (molar)`, unit = "µmol/l")
  expect_equal(yFactor, 1e-3 / xyData$MW * 1e6)

  expect_error(capture.output(print(xyData)), NA)
})
