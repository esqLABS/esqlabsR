## context("utilities-data")

test_that("It can read a properly defined file", {
  dataConf <- DataConfiguration$new(
    dataFolder = getTestDataFilePath(""),
    dataFile = "CompiledDataSet.xlsx",
    compoundPropertiesFile = "Compound_Properties.xlsx",
    dataSheets = c("TestSheet_1")
  )

  observedData <- readOSPSTimeValues(dataConfiguration = dataConf)
  expect_equal(length(observedData[[1]]), 2)
})

## context("stringToInt")

test_that("It converts a single positive number", {
  string <- "21"
  expect_equal(stringToNum(string), 21)
})

test_that("It converts a single negative number", {
  string <- "-21"
  expect_equal(stringToNum(string), -21)
})

test_that("It converts a vector of numbers", {
  string <- c("21", "-21")
  expect_equal(stringToNum(string), c(21, -21))
})

test_that("It converts a non numerics to NA", {
  string <- c("21", "one", "-21")
  expect_equal(stringToNum(string), c(21, NA, -21))
})

test_that("It converts a non numerics LLOQ to zero to NA", {
  string <- c("21", "one", "<5", "-21")
  expect_equal(stringToNum(string), c(21, NA, 0, -21))
})

## context("calculateMeans")
dataSet1 <- ospsuite::DataSet$new()
dataSet2 <- ospsuite::DataSet$new()
dataSet1$setValues(xValues = 1:5, yValues = 1:5)
dataSet2$setValues(xValues = 1:5, yValues = 1:5)

test_that("It throws an error when xDimensions do not match", {
  dataSet1$xDimension <- ospsuite::ospDimensions$Volume
  expect_error(calculateMeans(list(dataSet1, dataSet2)), messages$errorDimensionsDoNotMatch("xDimension"))
  dataSet1$xDimension <- ospsuite::ospDimensions$Time
})

test_that("It throws an error when yDimensions do not match", {
  dataSet1$yDimension <- ospsuite::ospDimensions$Volume
  expect_error(calculateMeans(list(dataSet1, dataSet2)), messages$errorDimensionsDoNotMatch("yDimension"))
  dataSet1$yDimension <- ospsuite::ospDimensions$`Concentration (mass)`
})

test_that("It can calculate means for data sets with different xUnits", {
  dataSet2$setValues(xValues = (1:5)*60, yValues = seq(10, 50, by=10))
  dataSet2$xUnit <- "min"
  meanDataSet <- calculateMeans(list(dataSet1, dataSet2))
  expect_equal(meanDataSet$xValues, dataSet1$xValues)
  expect_equal(meanDataSet$xUnit, "h")
  expect_equal(meanDataSet$yValues, (dataSet1$yValues + dataSet2$yValues)/2,
               tolerance = 0.00001)
})

test_that("It can calculate means for data sets with different yUnits", {
  dataSet2$setValues(xValues = 1:5, yValues = seq(1000, 5000, by=1000))
  dataSet2$xUnit <- "h"
  dataSet2$yUnit <- "µg/l"
  meanDataSet <- calculateMeans(list(dataSet1, dataSet2))
  expect_equal(meanDataSet$yUnit, dataSet1$yUnit)
  expect_equal(meanDataSet$xValues, dataSet1$xValues)
  expect_equal(meanDataSet$yValues, dataSet1$yValues)
})

test_that("Only meta data entries that are equal in all inital data sets are set in the mean data set", {
  dataSet1$addMetaData(name = "meta1", value = "a")
  dataSet2$addMetaData(name = "meta1", value = "a")
  dataSet1$addMetaData(name = "meta2", value = "b")
  dataSet2$addMetaData(name = "meta2", value = "c")
  dataSet1$addMetaData(name = "meta3", value = "d")
  meanDataSet <- calculateMeans(list(dataSet1, dataSet2))
  expect_equal(meanDataSet$metaData, list(meta1="a", `Subject ID` = "mean"))
})

test_that("It sets the yErrorValues to the arithmetic standard deviation", {
  dataSet2$setValues(xValues = 2:5, yValues = 2:5)
  dataSet2$yUnit <- "mg/l"
  meanDataSet <- calculateMeans(list(dataSet1, dataSet2))
  expect_equal(meanDataSet$yErrorValues, c(NA, rep(0, 4)))
})

test_that("It does not use data points with lloq for calculating the mean", {
  dataSet2$LLOQ <- 0.2
  meanDataSet <- calculateMeans(list(dataSet1, dataSet2))
  expect_equal(meanDataSet$yUnit, dataSet1$yUnit)
  expect_equal(meanDataSet$xValues, dataSet1$xValues)
  expect_equal(meanDataSet$yValues, dataSet1$yValues)
})


