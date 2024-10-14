# context("readParametersFromXLS-data")
# dataFolder <- getTestDataFilePath("")

skip()
skip_on_ci()

test_that("It can read an empty sheet", {
  # paramsXLSpath <- file.path(dataFolder, "Parameters.xlsx")
  # sheets <- c("EmptySheet")
  # params <- readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheets)
  #
  # expect_equal(names(params), c("paths", "values", "units"))
  # expect_type(params$values, "double")
  # expect_type(params$units, "character")
})

test_that("It can read a properly defined file", {
  # paramsXLSpath <- file.path(dataFolder, "Parameters.xlsx")
  # sheets <- c("ValidSheet")
  # params <- readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheets)
  #
  # expect_named(params, c("paths", "values", "units"))
})

test_that("It can read a properly defined file with extra columns", {
  # paramsXLSpath <- file.path(dataFolder, "Parameters.xlsx")
  # sheets <- c("ValidSheed_extraColumns")
  # params <- readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheets)
  #
  # expect_named(params, c("paths", "values", "units"))
})

test_that("It throws an error when a sheet has wrong structure", {
  # paramsXLSpath <- file.path(dataFolder, "Parameters.xlsx")
  # sheets <- "InvalidSheet"
  # columnNames <- c("Container Path", "Parameter Name", "Value", "Units")
  # expect_error(readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheets),
  #   regexp =
  #     messages$errorWrongXLSStructure(filePath = paramsXLSpath, expectedColNames = columnNames)
  # )
})

test_that("It overwrites the value if the path is present in multiple sheets", {
  # paramsXLSpath <- file.path(dataFolder, "Parameters.xlsx")
  # sheets <- c("ValidSheet", "SecondSheet")
  # params <- readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheets)
  #
  # paramsPaths <- c(
  #   "Path1|Param1",
  #   "Path2|DistincParam",
  #   "Applications|Glucose_iv_infusion|Active"
  # )
  # expectedVals <- c(5, 1, 0)
  # expectedUnits <- c("mg", "µmol", "")
  # idx <- match(paramsPaths, params$paths)
  # expect_equal(expectedVals, params$values[idx])
  # expect_equal(expectedUnits, params$units[idx])
})


# context("extendParameterStructure")

test_that("It trows an error if wrong structure is provideed", {
  # expect_error(
  #   extendParameterStructure(
  #     parameters = list(
  #       paths = "one",
  #       values = 2
  #     ),
  #     newParameters = list(
  #       paths = "one",
  #       values = 2,
  #       units = ""
  #     )
  #   ),
  #   regexp = messages$wrongParametersStructure(argumentName = "parameters")
  # )
})

test_that("It extends an empty structure by new values", {
  # params <- list(paths = NULL, values = NULL, units = NULL)
  # newParams <- list(
  #   paths = c("Path1", "Path2"), values = c(1, 2),
  #   units = c("", "µmol")
  # )
  #
  # extended <- extendParameterStructure(
  #   parameters = params,
  #   newParameters = newParams
  # )
  #
  # expect_equal(extended$paths, newParams$paths)
  # expect_equal(extended$values, newParams$values)
  # expect_equal(extended$units, newParams$units)
})

test_that("It extends a structure by empty structure", {
  # newParams <- list(paths = NULL, values = NULL, units = NULL)
  # params <- list(
  #   paths = c("Path1", "Path2"), values = c(1, 2),
  #   units = c("", "µmol")
  # )
  #
  # extended <- extendParameterStructure(
  #   parameters = params,
  #   newParameters = newParams
  # )
  #
  # expect_equal(extended$paths, params$paths)
  # expect_equal(extended$values, params$values)
  # expect_equal(extended$units, params$units)
})

test_that("It extends a structure by a new structure", {
  # params <- list(
  #   paths = c("Path1", "Path2"), values = c(1, 2),
  #   units = c("", "µmol")
  # )
  # newParams <- list(paths = c("Path2", "Path3"), values = c(1, 3), units = c("", "µmol"))
  #
  # extended <- extendParameterStructure(
  #   parameters = params,
  #   newParameters = newParams
  # )
  #
  # expect_equal(extended$paths, c("Path1", "Path2", "Path3"))
  # expect_equal(extended$values, c(1, 1, 3))
  # expect_equal(extended$units, c("", "", "µmol"))
})


# exportParametersToXLS
# simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
# simulation <- loadSimulation(simPath)
#
# param1 <- getParameter(path = "Organism|Weight", simulation)
# param2 <- getParameter(path = "Organism|Age", simulation)

test_that("It writes the excel file with one parameter provided", {
  # withr::with_tempdir(
  #   code = {
  #     xlsPath <- "tmp.xlsx"
  #     exportParametersToXLS(parameters = param1, paramsXLSpath = xlsPath)
  #
  #     # Load from xls and compare
  #     params <- readParametersFromXLS(paramsXLSpath = xlsPath)
  #
  #     expect_equal(params$paths[[1]], param1$path)
  #     expect_equal(params$values[[1]], param1$value)
  #     expect_equal(params$units[[1]], param1$unit)
  #   }
  # )
})

test_that("It writes the excel file with one parameter provided
          and specified sheet", {
  # withr::with_tempdir(
  #   code = {
  #     xlsPath <- "tmp.xlsx"
  #     sheet <- "newSheet"
  #     exportParametersToXLS(
  #       parameters = param1, paramsXLSpath = xlsPath,
  #       sheet = sheet
  #     )
  #
  #     # Load from xls and compare
  #     params <- readParametersFromXLS(paramsXLSpath = xlsPath, sheets = sheet)
  #
  #     expect_equal(params$paths[[1]], param1$path)
  #     expect_equal(params$values[[1]], param1$value)
  #     expect_equal(params$units[[1]], param1$unit)
  #   }
  # )
})

test_that("It writes the excel file with two parameters provided
          and specified sheet", {
  # withr::with_tempdir(
  #   code = {
  #     xlsPath <- "tmp.xlsx"
  #     sheet <- "newSheet"
  #     exportParametersToXLS(
  #       parameters = c(param1, param2), paramsXLSpath = xlsPath,
  #       sheet = sheet
  #     )
  #
  #     # Load from xls and compare
  #     params <- readParametersFromXLS(paramsXLSpath = xlsPath, sheets = sheet)
  #
  #     expect_equal(params$paths[[1]], param1$path)
  #     expect_equal(params$values[[1]], param1$value)
  #     expect_equal(params$units[[1]], param1$unit)
  #
  #     expect_equal(params$paths[[2]], param2$path)
  #     expect_equal(params$values[[2]], param2$value)
  #     expect_equal(params$units[[2]], param2$unit)
  #   }
  # )
})

test_that("It writes the excel file with two parameters provided
          and specified sheet", {
  # withr::with_tempdir(
  #   code = {
  #     xlsPath <- "tmp.xlsx"
  #     sheet <- "newSheet"
  #     params <- list(
  #       paths = c("Container1|Path1", "Container|Second|Third|Path2"),
  #       values = c(1, 2),
  #       units = c("", "µmol")
  #     )
  #     writeParameterStructureToXLS(
  #       parameterStructure = params,
  #       paramsXLSpath = xlsPath,
  #       sheet = sheet
  #     )
  #
  #     # Load from xls and compare
  #     paramsRead <- readParametersFromXLS(paramsXLSpath = xlsPath, sheets = sheet)
  #
  #     expect_equal(paramsRead$paths[[1]], params$paths[[1]])
  #     expect_equal(paramsRead$values[[1]], params$values[[1]])
  #     expect_equal(paramsRead$units[[1]], params$units[[1]])
  #
  #     expect_equal(paramsRead$paths[[2]], params$paths[[2]])
  #     expect_equal(paramsRead$values[[2]], params$values[[2]])
  #     expect_equal(paramsRead$units[[2]], params$units[[2]])
  #   }
  # )
})

test_that("It appends parameters to an already existing parameter excel file", {
  # withr::with_tempdir(
  #   code = {
  #     xlsPath <- "tmp.xlsx"
  #     sheet <- "newSheet"
  #     params <- list(
  #       paths = c("Container1|Path1", "Container|Second|Third|Path2"),
  #       values = c(1, 2),
  #       units = c("", "µmol")
  #     )
  #     writeParameterStructureToXLS(
  #       parameterStructure = params,
  #       paramsXLSpath = xlsPath,
  #       sheet = sheet
  #     )
  #
  #     newParam <- list(
  #       paths = c("Container1|Path2"),
  #       values = c(10),
  #       units = c("")
  #     )
  #
  #     writeParameterStructureToXLS(
  #       parameterStructure = newParam,
  #       paramsXLSpath = xlsPath,
  #       sheet = sheet,
  #       append = TRUE
  #     )
  #
  #
  #     # Load from xls and compare
  #     paramsRead <- readParametersFromXLS(paramsXLSpath = xlsPath, sheets = sheet)
  #
  #     expect_equal(paramsRead$paths[[3]], newParam$paths[[1]])
  #     expect_equal(paramsRead$values[[3]], newParam$values[[1]])
  #     expect_equal(paramsRead$units[[3]], newParam$units[[1]])
  #   }
  # )
})
