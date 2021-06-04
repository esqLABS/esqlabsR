context("readParametersFromXLS-data")
dataFolder <- getTestDataFilePath("")

test_that("It can read a properly defined file", {
  paramsXLSpath <- file.path(dataFolder, "Parameters.xlsx")
  sheets <- c("ValidSheet")
  params <- readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheets)

  expect_equal(names(params), c("paths", "values", "units"))
})

test_that("It can read a properly defined file with extra columns", {
  paramsXLSpath <- file.path(dataFolder, "Parameters.xlsx")
  sheets <- c("ValidSheed_extraColumns")
  params <- readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheets)

  expect_equal(names(params), c("paths", "values", "units"))
})

test_that("It throws an error when a sheet has wrong structure", {
  paramsXLSpath <- file.path(dataFolder, "Parameters.xlsx")
  sheets <- c("InvalidSheet")
  expect_error(readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheets),
    regexp =
      messages$errorWrongParamsXLSStructure(paramsXLSpath)
  )
})
