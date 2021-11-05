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

test_that("It can calculate means and save the results to a new file", {
  path <- getTestDataFilePath("CompiledDataSet.xlsx")
  sheet <- "Test_MeanData"
  calculateMeans(path, sheet, columnNames = c("Time [h]", "Fraction [%]", "Error [%]"))
  newPath <- getTestDataFilePath("CompiledDataSet_mean.xlsx")
  expect_true(file.exists(newPath))

  meanData <- readExcel(newPath)
  originalData <- readExcel(path, sheet)
  expect_setequal(names(meanData), names(originalData))
  file.remove(newPath)

  calculateMeans(path, sheet, columnNames = c("Time [h]", "Fraction [%]", "Error [%]"),
                 nonGroupingCols = c("Study Id", "Patient Id", "Dose [unit]", "Time [h]"))
  meanData <- readExcel(newPath)
  expect_equal(meanData$`Fraction [%]`, mean(originalData$`Fraction [%]`))
  expect_equal(meanData$`Error [%]`, sd(originalData$`Fraction [%]`))
  file.remove(newPath)
})

test_that("It throws an error when required columns are missing in the file", {
  path <- getTestDataFilePath("CompiledDataSet.xlsx")
  sheet <- "TestSheet_1"
  expect_error(calculateMeans(path, sheet))
})

test_that("It throws an error when the sheet is empty (except for column names)", {
  path <- getTestDataFilePath("CompiledDataSet.xlsx")
  sheet <- "WrongSheet"
  expect_error(
    calculateMeans(path, sheet, columnNames = c("Time [min]", "Fraction [%]", "Error [%]")),
    paste("The provided sheet from file", path, "does not contain any data.")
  )
})
