##  context("readParametersFromXLS-data")
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

test_that("It overwrites the value if the path is present in multiple sheets", {
  paramsXLSpath <- file.path(dataFolder, "Parameters.xlsx")
  sheets <- c("ValidSheet", "SecondSheet")
  params <- readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheets)

  paramsPaths <- c("Path1|Param1",
                   "Path2|DistincParam",
                   "Applications|Glucose_iv_infusion|Active")
  expectedVals <- c(5, 1, 0)
  expectedUnits <- c("mg", "µmol", "")
  idx <- match(paramsPaths, params$paths)
  expect_equal(expectedVals, params$values[idx])
  expect_equal(expectedUnits, params$units[idx])
})
