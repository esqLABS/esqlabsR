# context("readParametersFromXLS-data")
dataFolder <- getTestDataFilePath("")

test_that("It can read an empty sheet", {
  paramsXLSpath <- file.path(dataFolder, "Parameters.xlsx")
  sheets <- c("EmptySheet")
  params <- readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheets)

  expect_equal(names(params), c("paths", "values", "units"))
  expect_type(params$values, "double")
  expect_type(params$units, "character")
})

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
  columnNames <- c("Container Path", "Parameter Name", "Value", "Units")
  expect_error(readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheets),
    regexp =
      messages$errorWrongXLSStructure(filePath = paramsXLSpath, expectedColNames = columnNames)
  )
})

test_that("It overwrites the value if the path is present in multiple sheets", {
  paramsXLSpath <- file.path(dataFolder, "Parameters.xlsx")
  sheets <- c("ValidSheet", "SecondSheet")
  params <- readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheets)

  paramsPaths <- c(
    "Path1|Param1",
    "Path2|DistincParam",
    "Applications|Glucose_iv_infusion|Active"
  )
  expectedVals <- c(5, 1, 0)
  expectedUnits <- c("mg", "µmol", "")
  idx <- match(paramsPaths, params$paths)
  expect_equal(expectedVals, params$values[idx])
  expect_equal(expectedUnits, params$units[idx])
})


# context("extendParameterStructure")

test_that("It trows an error if wrong structure is provideed", {
  expect_error(extendParameterStructure(
    parameters = list(
      paths = "one",
      values = 2
    ),
    newParameters = list(
      paths = "one",
      values = 2,
      units = ""
    )
  ),
  regexp = messages$wrongParametersStructure(argumentName = "parameters")
  )
})

test_that("It extends an empty structure by new values", {
  params <- list(paths = c(), values = c(), units = c())
  newParams <- list(
    paths = c("Path1", "Path2"), values = c(1, 2),
    units = c("", "µmol")
  )

  extended <- extendParameterStructure(
    parameters = params,
    newParameters = newParams
  )

  expect_equal(extended$paths, newParams$paths)
  expect_equal(extended$values, newParams$values)
  expect_equal(extended$units, newParams$units)
})

test_that("It extends a structure by empty structure", {
  newParams <- list(paths = c(), values = c(), units = c())
  params <- list(
    paths = c("Path1", "Path2"), values = c(1, 2),
    units = c("", "µmol")
  )

  extended <- extendParameterStructure(
    parameters = params,
    newParameters = newParams
  )

  expect_equal(extended$paths, params$paths)
  expect_equal(extended$values, params$values)
  expect_equal(extended$units, params$units)
})

test_that("It extends a structure by a new structure", {
  params <- list(
    paths = c("Path1", "Path2"), values = c(1, 2),
    units = c("", "µmol")
  )
  newParams <- list(paths = c("Path2", "Path3"), values = c(1, 3), units = c("", "µmol"))

  extended <- extendParameterStructure(
    parameters = params,
    newParameters = newParams
  )

  expect_equal(extended$paths, c("Path1", "Path2", "Path3"))
  expect_equal(extended$values, c(1, 1, 3))
  expect_equal(extended$units, c("", "", "µmol"))
})
