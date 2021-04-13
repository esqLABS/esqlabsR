context("utilities-data")

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

context("stringToInt")

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
