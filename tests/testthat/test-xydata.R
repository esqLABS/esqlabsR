context("XYData")

test_that("It can create an XYData without Y error", {
  xVals <- c(0, 1, 2, 3)
  yVals <- c(4, 5, 6, 7)
  xyData <- XYData$new(xVals, yVals, label = "My XY Data")
  expect_equal(xyData$xValues, xVals)
  expect_equal(xyData$yValues, yVals)

  expect_error(capture.output(print(xyData)), NA)
})

test_that("It can create an XYData with Y error", {
  xVals <- c(0, 1, 2, 3)
  yVals <- c(4, 5, 6, 7)
  yError <- c(0.1, 0, 0.1, 1)
  xyData <- XYData$new(xVals, yVals, yError = yError, label = "My XY Data")
  expect_equal(xyData$xValues, xVals)
  expect_equal(xyData$yValues, yVals)
  expect_equal(xyData$yError, yError)

  expect_error(capture.output(print(xyData)), NA)
})

test_that("It throws an error if the length of the vectors does not match", {
  xVals <- c(0, 1, 2, 3)
  yVals <- c(4, 5, 6)
  yError <- c(0.1, 0, 0.1)
  expect_error(xyData <- XYData$new(xVals, yVals, yError = yError, label = "My XY Data"))
  yVals <- c(4, 5, 6, 7)
  expect_error(xyData <- XYData$new(xVals, yVals, yError = yError, label = "My XY Data"))
})

test_that("It returns maximal x and y values", {
  xVals <- c(0, 1, 2, 3)
  yVals <- c(4, 5, 6, 7)
  yError <- c(0.1, 0, 0.1, 1)
  xyData <- XYData$new(xVals, yVals, yError = yError, label = "My XY Data")
  expect_equal(xyData$xMax, max(xVals))
  expect_equal(xyData$yMax, max(yVals + yError))
  expect_equal(xyData$xMin, min(xVals))
  expect_equal(xyData$yMin, min(yVals - yError))
})

test_that("It returns the correct minimal positive y value", {
  xVals <- c(0, 1, 2, 3)
  yVals <- c(4, 5, 6, 7)
  yError <- c(0.1, 0, 0.1, 1)
  xyData <- XYData$new(xVals, yVals, yError = yError, label = "My XY Data")
  expect_equal(xyData$yMinPositive(), min(yVals - yError))

  yError <- c(4.1, 0, 0.1, 1)
  xyData <- XYData$new(xVals, yVals, yError = yError, label = "My XY Data")
  expect_equal(xyData$yMinPositive(), 5)
})

test_that("It returns maximal x and y values with offset", {
  xVals <- c(0, 1, 2, 3)
  yVals <- c(4, 5, 6, 7)
  yError <- c(0.1, 0, 0.1, 1)
  xyData <- XYData$new(xVals, yVals, yError = yError, label = "My XY Data")
  xyData$xOffset <- 5
  xyData$yOffset <- -4
  expect_equal(xyData$xMax, max(xVals + 5))
  expect_equal(xyData$yMax, max((yVals + yError) - 4))
  expect_equal(xyData$xMin, min(xVals + 5))
  expect_equal(xyData$yMin, min((yVals - yError) - 4))
  expect_equal(xyData$yMinPositive(), 1)

  expect_error(capture.output(print(xyData)), NA)
})

test_that("It returns maximal x and y values with offset and scale factor", {
  xVals <- c(0, 1, 2, 3)
  yVals <- c(4, 5, 6, 7)
  yError <- c(0.1, 0, 0.1, 1)
  xyData <- XYData$new(xVals, yVals, yError = yError, label = "My XY Data")
  xyData$xOffset <- 5
  xyData$yOffset <- -4

  xyData$xFactor <- 0.2
  xyData$yFactor <- 1.2

  expect_equal(xyData$xMax, max(xVals + 5) * 0.2)
  expect_equal(xyData$yMax, max((yVals + yError) - 4) * 1.2)
  expect_equal(xyData$xMin, min(xVals + 5) * 0.2)
  expect_equal(xyData$yMin, min((yVals - yError) - 4) * 1.2)
  expect_equal(xyData$yMinPositive(), 1.2)

  expect_error(capture.output(print(xyData)), NA)
})

test_that("The default data type is 'Unspecified'", {
  xVals <- c(0, 1, 2, 3)
  yVals <- c(4, 5, 6, 7)
  yError <- c(0.1, 0, 0.1, 1)
  xyData <- XYData$new(xVals, yVals, yError = yError, label = "My XY Data")

  expect_equal(xyData$dataType, XYDataTypes$Unspecified)
})

test_that("It can set the data type", {
  xVals <- c(0, 1, 2, 3)
  yVals <- c(4, 5, 6, 7)
  yError <- c(0.1, 0, 0.1, 1)
  xyData <- XYData$new(xVals, yVals, yError = yError, label = "My XY Data")

  xyData$dataType <- XYDataTypes$Simulated
  expect_equal(xyData$dataType, XYDataTypes$Simulated)
  expect_error(xyData$dataType <- "nonExistent")
})

test_that("It can get and set the dimension", {
  xVals <- c(0, 1, 2, 3)
  yVals <- c(4, 5, 6, 7)
  yError <- c(0.1, 0, 0.1, 1)
  xyData <- XYData$new(xVals, yVals, yError = yError, label = "My XY Data")

  expect_equal(xyData$xDimension, Dimensions$Time)
  expect_equal(xyData$yDimension, Dimensions$Dimensionless)
  xyData$xDimension <- Dimensions$Amount
  xyData$yDimension <- Dimensions$`Concentration (molar)`
  expect_equal(xyData$xDimension, Dimensions$Amount)
  expect_equal(xyData$yDimension, Dimensions$`Concentration (molar)`)
  expect_equal(xyData$xUnit, getBaseUnit(Dimensions$Amount))
  expect_equal(xyData$yUnit, getBaseUnit(Dimensions$`Concentration (molar)`))
  expect_equal(xyData$yErrorUnit, getBaseUnit(Dimensions$`Concentration (molar)`))

  expect_error(xyData$xDimension <- "nonExistent")
  expect_error(xyData$yDimension <- "nonExistent")

  expect_error(capture.output(print(xyData)), NA)
})

test_that("It can set the unit", {
  xVals <- c(0, 1, 2, 3)
  yVals <- c(4, 5, 6, 7)
  yError <- c(0.1, 0, 0.1, 1)
  xyData <- XYData$new(xVals, yVals, yError = yError, label = "My XY Data")

  xyData$xUnit <- "h"
  expect_equal(xyData$xUnit, "h")
  expect_error(xyData$Unit <- "Hour")

  xyData$yDimension <- Dimensions$`Concentration (molar)`
  xyData$yUnit <- "nmol/l"
  expect_equal(xyData$yUnit, "nmol/l")
  expect_equal(xyData$yErrorUnit, "µmol/l")

  expect_error(capture.output(print(xyData)), NA)
})

test_that("Correct conversion factor between units for x values", {
  xVals <- c(0, 1, 2, 3)
  yVals <- c(4, 5, 6, 7)
  yError <- c(0.1, 0, 0.1, 1)
  xyData <- XYData$new(xVals, yVals, yError = yError, label = "My XY Data")

  # No conversion
  xFactor <- xyData$xUnitDimensionFactor(dimension = Dimensions$Time, unit = "min")
  expect_equal(xFactor, 1)
  # Minutes to hour
  xFactor <- xyData$xUnitDimensionFactor(dimension = Dimensions$Time, unit = "h")
  expect_equal(xFactor, 1 / 60)

  # Change unit
  xyData$xUnit <- "h"
  xFactor <- xyData$xUnitDimensionFactor(dimension = Dimensions$Time, unit = "h")
  expect_equal(xFactor, 1)
  xFactor <- xyData$xUnitDimensionFactor(dimension = Dimensions$Time, unit = "min")
  expect_equal(xFactor, 60)

  expect_error(capture.output(print(xyData)), NA)
})

test_that("Correct conversion factor between units for y values", {
  xVals <- c(0, 1, 2, 3)
  yVals <- c(4, 5, 6, 7)
  yError <- c(0.1, 0, 0.1, 1)
  xyData <- XYData$new(xVals, yVals, yError = yError, label = "My XY Data")

  # No conversion
  xyData$yDimension <- Dimensions$`Concentration (molar)`
  yFactor <- xyData$yUnitDimensionFactor(dimension = Dimensions$`Concentration (molar)`, unit = "µmol/l")
  expect_equal(yFactor, 1)
  # µmol/l to mol/l
  yFactor <- xyData$yUnitDimensionFactor(dimension = Dimensions$`Concentration (molar)`, unit = "mol/l")
  expect_equal(yFactor, 1e-6)

  # Molar to mass concentration
  yFactor <- xyData$yUnitDimensionFactor(dimension = Dimensions$`Concentration (mass)`, unit = "µg/l", mw = 300)
  expect_equal(yFactor, 300)

  expect_error(capture.output(print(xyData)), NA)
})

test_that("Correct conversion factor between units for y errror values", {
  xVals <- c(0, 1, 2, 3)
  yVals <- c(4, 5, 6, 7)
  yError <- c(0.1, 0, 0.1, 1)
  xyData <- XYData$new(xVals, yVals, yError = yError, label = "My XY Data")

  # No conversion
  xyData$yDimension <- Dimensions$`Concentration (molar)`
  yFactor <- xyData$yErrorUnitDimensionFactor(dimension = Dimensions$`Concentration (molar)`, unit = "µmol/l")
  expect_equal(yFactor, 1)
  # µmol/l to mol/l
  yFactor <- xyData$yErrorUnitDimensionFactor(dimension = Dimensions$`Concentration (molar)`, unit = "mol/l")
  expect_equal(yFactor, 1e-6)

  # Molar to mass concentration
  yFactor <- xyData$yErrorUnitDimensionFactor(dimension = Dimensions$`Concentration (mass)`, unit = "µg/l", mw = 300)
  expect_equal(yFactor, 300)

  expect_error(capture.output(print(xyData)), NA)
})

test_that("Correct XXXProcessed", {
  xVals <- c(0, 1, 2, 3)
  yVals <- c(4, 5, 6, 7)
  yError <- c(0.1, 0, 0.1, 1)
  xyData <- XYData$new(xVals, yVals, yError = yError, label = "My XY Data")
  xyData$xDimension <- "Time"
  xyData$yDimension <- "Concentration (molar)"

  expect_equal(
    c(xyData$xValuesProcessed(), xyData$yValuesProcessed(), xyData$yErrorProcessed()),
    c(xVals, yVals, yError)
  )

  xyData$xOffset <- 1
  xyData$yOffset <- -2
  expect_equal(
    c(xyData$xValuesProcessed(), xyData$yValuesProcessed(), xyData$yErrorProcessed()),
    c(xVals + 1, yVals - 2, yError - 2)
  )

  xyData$xFactor <- 0.2
  xyData$yFactor <- 1.3
  expect_equal(
    c(xyData$xValuesProcessed(), xyData$yValuesProcessed(), xyData$yErrorProcessed()),
    c((xVals + 1) * 0.2, (yVals - 2) * 1.3, (yError - 2) * 1.3)
  )

  # Unit chAnge
  expect_equal(
    c(
      xyData$xValuesProcessed(dimension = "Time", unit = "h"), xyData$yValuesProcessed(dimension = "Concentration (molar)", unit = "pmol/l"),
      xyData$yErrorProcessed(dimension = "Concentration (molar)", unit = "mmol/l")
    ),
    c((xVals + 1) * 0.2 / 60, (yVals - 2) * 1.3 * 1e6, (yError - 2) * 1.3 * 1e-3)
  )

  # Dimension change
  expect_equal(
    c(
      xyData$yValuesProcessed(dimension = "Concentration (mass)", unit = "pg/l", mw = 100),
      xyData$yErrorProcessed(dimension = "Concentration (mass)", unit = "mg/l", mw = 100)
    ),
    c((yVals - 2) * 1.3 * 1e6 * 100, (yError - 2) * 1.3 * 1e-3 * 100)
  )
})

test_that("getXXXProcessed errors", {
  xVals <- c(0, 1, 2, 3)
  yVals <- c(4, 5, 6, 7)
  yError <- c(0.1, 0, 0.1, 1)
  xyData <- XYData$new(xVals, yVals, yError = yError, label = "My XY Data")
  xyData$xDimension <- "Time"
  xyData$yDimension <- "Concentration (molar)"

  expect_error(xyData$xValuesProcessed(dimension = "Energy"), regexp = escapeForRegex(
    messages$errorOneArgumentNullButNotBoth("dimension", "unit")
  ))

  expect_error(xyData$xValuesProcessed(unit = "Energy"), regexp = escapeForRegex(
    messages$errorOneArgumentNullButNotBoth("dimension", "unit")
  ))

  expect_error(xyData$yValuesProcessed(dimension = "Energy"), regexp = escapeForRegex(
    messages$errorOneArgumentNullButNotBoth("dimension", "unit")
  ))

  expect_error(xyData$yErrorProcessed(unit = "Energy"), regexp = escapeForRegex(
    messages$errorOneArgumentNullButNotBoth("dimension", "unit")
  ))

  expect_error(xyData$yErrorProcessed(dimension = "Energy"), regexp = escapeForRegex(
    messages$errorOneArgumentNullButNotBoth("dimension", "unit")
  ))

  expect_error(xyData$yValuesProcessed(unit = "Energy"), regexp = escapeForRegex(
    messages$errorOneArgumentNullButNotBoth("dimension", "unit")
  ))

  expect_error(xyData$xValuesProcessed(dimension = "Energy", unit = "kcal"), regexp = escapeForRegex(
    messages$errorCannotConvertDimensions("Time", "Energy")
  ))

  expect_error(xyData$xValuesProcessed(dimension = "Time", unit = "µmol/l"), regexp = escapeForRegex(
    messages$errorUnitNotSupported("µmol/l", "Time")
  ))

  expect_error(xyData$yValuesProcessed(dimension = "Energy", unit = "kcal"), regexp = escapeForRegex(
    messages$errorCannotConvertDimensions("Concentration (molar)", "Energy")
  ))

  expect_error(xyData$yValuesProcessed(dimension = "Concentration (mass)", unit = "µmol/l", mw = 100), regexp = escapeForRegex(
    messages$errorUnitNotSupported("µmol/l", "Concentration (mass)")
  ))

  expect_error(xyData$yErrorProcessed(dimension = "Energy", unit = "kcal"), regexp = escapeForRegex(
    messages$errorCannotConvertDimensions("Concentration (molar)", "Energy")
  ))

  expect_error(xyData$yErrorProcessed(dimension = "Concentration (mass)", unit = "µmol/l", mw = 100), regexp = escapeForRegex(
    messages$errorUnitNotSupported("µmol/l", "Concentration (mass)")
  ))
})

test_that("It gets meta data", {
  xVals <- c(0, 1, 2, 3)
  yVals <- c(4, 5, 6, 7)
  yError <- c(0.1, 0, 0.1, 1)
  xyData <- XYData$new(xVals, yVals, yError = yError, label = "My XY Data")

  expect_equal(xyData$getAllMetaData(), list())

  xyData$setMetaData(name = "City", value = "Munich")
  expect_equal(xyData$getAllMetaData(), list(City = "Munich"))
  xyData$setMetaData(name = "State", value = "Bavaria")
  expect_equal(xyData$getAllMetaData(), list(City = "Munich", State = "Bavaria"))
  xyData$setMetaData(name = "City")
  expect_equal(xyData$getAllMetaData(), list(State = "Bavaria"))
  xyData$setMetaData(name = "pi", value = c(3, 1, 4))
  expect_equal(xyData$getAllMetaData(), list(State = "Bavaria", pi = c(3, 1, 4)))
})

test_that("Get Multiple Meta Data Entries error", {
  xVals <- c(0, 1, 2, 3)
  yVals <- c(4, 5, 6, 7)
  yError <- c(0.1, 0, 0.1, 1)
  xyData <- XYData$new(xVals, yVals, yError = yError, label = "My XY Data")

  expect_error(xyData$setMetaData(name = c("test1", "test2"), value = 1), regexp = escapeForRegex(messages$errorMultipleMetaDataEntries()))
})
