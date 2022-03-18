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

## context("stringToNum")

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

## context("calculateMeanDataSet")
dataSet1 <- ospsuite::DataSet$new(name = "data1")
dataSet2 <- ospsuite::DataSet$new(name = "data2")


test_that("It returns an empty DataSet if calculating for empty DataSets", {
  meanDataSet <- calculateMeanDataSet(dataSet1)
  expect_equal(meanDataSet$xValues, numeric())
  expect_equal(meanDataSet$yValues, numeric())
})

dataSet1$setValues(xValues = 1:5, yValues = 1:5)
dataSet2$setValues(xValues = 2:6, yValues = 4:8)

test_that("It can calculate the mean data set for a single data set", {
  dataSet1$addMetaData(name = "meta1", value = "a")
  meanDataSet <- calculateMeanDataSet(dataSet1)
  expect_equal(meanDataSet$xValues, dataSet1$xValues)
  expect_equal(meanDataSet$yValues, dataSet1$yValues)
  expect_equal(meanDataSet$yErrorValues, rep(NaN, 5))
  expect_equal(meanDataSet$name, "Mean")
  expect_equal(meanDataSet$metaData, list(meta1 = "a", `Subject ID` = "mean"))
})

test_that("It can calculate the arithmetic mean and standard deviation (default)", {
  meanDataSet <- calculateMeanDataSet(list(dataSet1, dataSet2))
  expect_equal(meanDataSet$xValues, 1:6)
  expect_equal(meanDataSet$yValues, c(1, 3:6, 8), tolerance = 1e-06)
  expect_equal(meanDataSet$yErrorValues, c(NaN, rep(sqrt(2), 4), NaN), tolerance = 1e-06)
})

test_that("It can calculate the geometric mean and standard deviation", {
  meanDataSet <- calculateMeanDataSet(list(dataSet1, dataSet2), method = "geometric")
  expect_equal(meanDataSet$xValues, 1:6)
  expect_equal(meanDataSet$yValues, c(
    1, geomean(c(2, 4)), geomean(c(3, 5)),
    geomean(c(4, 6)), geomean(c(5, 7)), 8
  ))
  expect_equal(meanDataSet$yErrorValues, c(
    NaN, geosd(c(2, 4)), geosd(c(3, 5)),
    geosd(c(4, 6)), geosd(c(5, 7)), NaN
  ),
  tolerance = 1e-06
  )
})

test_that("It can convert values to outputXunit and outputYunit", {
  # input units are "h" and "mg/l"
  meanDataSet <- calculateMeanDataSet(list(dataSet1, dataSet2), outputXunit = "min", outputYunit = "µg/l")
  expect_equal(meanDataSet$xValues, seq(60, 360, 60))
  expect_equal(meanDataSet$yValues, c(1, 3:6, 8) * 1000, tolerance = 1e-06)
  expect_equal(meanDataSet$yErrorValues, c(NaN, rep(sqrt(2e06), 4), NaN), tolerance = 1e-06)
})

test_that("It can convert values to xUnit and yUnit of first data set, with given molWeights", {
  dataSet1$yDimension <- ospsuite::ospDimensions$`Concentration (molar)`
  dataSet1$yUnit <- ospsuite::ospUnits$`Concentration [molar]`$`mmol/l`
  dataSet1$molWeight <- 2
  dataSet2$molWeight <- 4
  dataSet2$setValues(xValues = seq(120, 360, 60), yValues = 4:8)
  dataSet2$xUnit <- "min"
  meanDataSet <- calculateMeanDataSet(list(dataSet1, dataSet2), outputMolWeight = 5)
  expect_equal(meanDataSet$molWeight, 5)
  expect_equal(meanDataSet$xUnit, "h")
  expect_equal(meanDataSet$xValues, 1:6)
  expect_equal(meanDataSet$yDimension, "Concentration (molar)")
  expect_equal(meanDataSet$yUnit, "mmol/l")
  # dataSet2 yValues in mmol/l: 1.00 1.25 1.50 1.75 2.00
  # dataSet1 yValues in mg/l: 1 2 3 4 5
  expect_equal(meanDataSet$yValues, c(1, 1.5, 2.125, 2.75, 3.375, 2))
  expect_equal(meanDataSet$yErrorValues, c(
    NaN, sd(c(1, 2)), sd(c(1.25, 3)),
    sd(c(1.5, 4)), sd(c(1.75, 5)), NaN
  ), tolerance = 1e-06)
})

test_that("It throws an error when xValues can not be converted to same xUnit", {
  dataSet1 <- ospsuite::DataSet$new(name = "data1")
  dataSet2 <- ospsuite::DataSet$new(name = "data2")
  dataSet1$setValues(xValues = 1:5, yValues = 1:5)
  dataSet2$setValues(xValues = 2:6, yValues = 4:8)
  dataSet2$xDimension <- ospsuite::ospDimensions$Flow
  expect_error(calculateMeanDataSet(list(dataSet1, dataSet2)), "Unit 'l/min' is not defined in dimension 'Time'")
})

test_that("It throws an error when yValues can not be converted to same yUnit", {
  dataSet1 <- ospsuite::DataSet$new(name = "data1")
  dataSet2 <- ospsuite::DataSet$new(name = "data2")
  dataSet1$setValues(xValues = 1:5, yValues = 1:5)
  dataSet2$setValues(xValues = 2:6, yValues = 4:8)
  dataSet1$yDimension <- ospsuite::ospDimensions$Flow
  expect_error(
    calculateMeanDataSet(list(dataSet1, dataSet2)),
    "Unit 'mg/l' is not defined in dimension 'Flow'"
  )
})

test_that("Only meta data entries that are equal in all inital data sets are set in the mean data set", {
  dataSet1 <- ospsuite::DataSet$new(name = "data1")
  dataSet2 <- ospsuite::DataSet$new(name = "data2")
  dataSet1$setValues(xValues = 1:5, yValues = 1:5)
  dataSet2$setValues(xValues = 2:6, yValues = 4:8)
  dataSet1$addMetaData(name = "meta1", value = "a")
  dataSet2$addMetaData(name = "meta1", value = "a")
  dataSet1$addMetaData(name = "meta2", value = "b")
  dataSet2$addMetaData(name = "meta2", value = "c")
  dataSet1$addMetaData(name = "meta3", value = "d")
  dataSet1$addMetaData(name = "meta4", value = "d")
  meanDataSet <- calculateMeanDataSet(list(dataSet1, dataSet2))
  expect_equal(meanDataSet$metaData, list(meta1 = "a", `Subject ID` = "mean"))
})

test_that("It throws an error when molWeights of data sets are different and no outputMolWeight is given", {
  dataSet1$molWeight <- 1
  dataSet2$molWeight <- 2
  expect_error(calculateMeanDataSet(list(dataSet1, dataSet2)), messages$errorOutputMolWeightNeeded())
})

test_that("It can handle the lloqMode argument", {
  dataSet1 <- ospsuite::DataSet$new(name = "data1")
  dataSet2 <- ospsuite::DataSet$new(name = "data2")
  dataSet1$setValues(xValues = 1:5, yValues = 1:5)
  dataSet2$setValues(xValues = 2:6, yValues = 4:8)
  dataSet2$LLOQ <- 6
  # LLOQ/2 --> no difference
  meanDataSet <- calculateMeanDataSet(list(dataSet1, dataSet2), lloqMode = "LLOQ/2")
  expect_equal(meanDataSet$xValues, 1:6)
  expect_equal(meanDataSet$yValues, c(1, 3:6, 8), tolerance = 1e-06)
  expect_equal(meanDataSet$yErrorValues, c(NaN, rep(sqrt(2), 4), NaN), tolerance = 1e-06)
  # LLOQ
  meanDataSet <- calculateMeanDataSet(list(dataSet1, dataSet2), lloqMode = "LLOQ")
  expect_equal(meanDataSet$xValues, 1:6)
  expect_equal(meanDataSet$yValues, c(1, 4, 4.5, 5, 6, 8), tolerance = 1e-06)
  expect_equal(meanDataSet$yErrorValues, c(
    NaN, sd(c(2, 6)), sd(c(3, 6)), sd(c(4, 6)),
    sd(c(5, 7)), NaN
  ), tolerance = 1e-06)
  # ZERO
  meanDataSet <- calculateMeanDataSet(list(dataSet1, dataSet2), lloqMode = "ZERO")
  expect_equal(meanDataSet$xValues, 1:6)
  expect_equal(meanDataSet$yValues, c(1, 1, 1.5, 5, 6, 8), tolerance = 1e-06)
  expect_equal(meanDataSet$yErrorValues, c(
    NaN, sd(c(2, 0)), sd(c(3, 0)), sd(c(4, 6)),
    sd(c(5, 7)), NaN
  ), tolerance = 1e-06)
  # ignore
  meanDataSet <- calculateMeanDataSet(list(dataSet1, dataSet2), lloqMode = "ignore")
  expect_equal(meanDataSet$xValues, 1:6)
  expect_equal(meanDataSet$yValues, c(1, 2, 3, 5, 6, 8), tolerance = 1e-06)
  expect_equal(meanDataSet$yErrorValues, c(NaN, NaN, NaN, sd(c(4, 6)), sd(c(5, 7)), NaN), tolerance = 1e-06)
})

test_that("It sets the LLOQ if it is given for any of the original data sets", {
  dataSet1 <- ospsuite::DataSet$new(name = "data1")
  dataSet2 <- ospsuite::DataSet$new(name = "data2")
  dataSet1$setValues(xValues = 1:5, yValues = 1:5)
  dataSet2$setValues(xValues = 2:5, yValues = 4:7)
  meanDataSet <- calculateMeanDataSet(list(dataSet1, dataSet2))
  expect_equal(meanDataSet$LLOQ, NULL)

  dataSet2$LLOQ <- 2
  meanDataSet <- calculateMeanDataSet(list(dataSet1, dataSet2))
  expect_equal(meanDataSet$LLOQ, 2)

  dataSet1$LLOQ <- 1
  meanDataSet <- calculateMeanDataSet(list(dataSet1, dataSet2))
  expect_equal(meanDataSet$LLOQ, 1.5, tolerance = 1e-06)
})
