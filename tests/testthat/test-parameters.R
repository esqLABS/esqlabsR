dataFolder <- getTestDataFilePath("")

test_that("It can read an empty sheet", {
  paramsXLSpath <- file.path(dataFolder, "Parameters.xlsx")
  sheets <- c("EmptySheet")
  params <- readParametersFromXLS(
    paramsXLSpath = paramsXLSpath,
    sheets = sheets
  )

  expect_equal(names(params), c("paths", "values", "units"))
  expect_type(params$values, "double")
  expect_type(params$units, "character")
})

test_that("It can read a properly defined file", {
  paramsXLSpath <- file.path(dataFolder, "Parameters.xlsx")
  sheets <- c("ValidSheet")
  params <- readParametersFromXLS(
    paramsXLSpath = paramsXLSpath,
    sheets = sheets
  )

  expect_named(params, c("paths", "values", "units"))
})

test_that("It can read a properly defined file with extra columns", {
  paramsXLSpath <- file.path(dataFolder, "Parameters.xlsx")
  sheets <- c("ValidSheed_extraColumns")
  params <- readParametersFromXLS(
    paramsXLSpath = paramsXLSpath,
    sheets = sheets
  )

  expect_named(params, c("paths", "values", "units"))
})

test_that("It throws an error when a sheet has wrong structure", {
  paramsXLSpath <- file.path(dataFolder, "Parameters.xlsx")
  sheets <- "InvalidSheet"
  columnNames <- c("Container Path", "Parameter Name", "Value", "Units")
  expect_error(
    readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheets),
    "Loading from XLS failed"
  )
})

test_that("It overwrites the value if the path is present in multiple sheets", {
  paramsXLSpath <- file.path(dataFolder, "Parameters.xlsx")
  sheets <- c("ValidSheet", "SecondSheet")
  params <- readParametersFromXLS(
    paramsXLSpath = paramsXLSpath,
    sheets = sheets
  )

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


test_that("It trows an error if wrong structure is provideed", {
  expect_error(
    extendParameterStructure(
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
    "wrong structure"
  )
})

test_that("It accepts NULL for parameters and returns newParameters", {
  newParams <- list(
    paths = c("Path1", "Path2"),
    values = c(1, 2),
    units = c("", "µmol")
  )

  extended <- extendParameterStructure(
    parameters = NULL,
    newParameters = newParams
  )

  expect_equal(extended$paths, newParams$paths)
  expect_equal(extended$values, newParams$values)
  expect_equal(extended$units, newParams$units)
})

test_that("It accepts NULL for newParameters and returns parameters", {
  params <- list(
    paths = c("Path1", "Path2"),
    values = c(1, 2),
    units = c("", "µmol")
  )

  extended <- extendParameterStructure(
    parameters = params,
    newParameters = NULL
  )

  expect_equal(extended$paths, params$paths)
  expect_equal(extended$values, params$values)
  expect_equal(extended$units, params$units)
})

test_that("It returns an empty valid structure when both parameters and newParameters are NULL", {
  extended <- extendParameterStructure(
    parameters = NULL,
    newParameters = NULL
  )

  expect_equal(extended, list(paths = NULL, values = NULL, units = NULL))
})

test_that("It extends an empty structure by new values", {
  params <- list(paths = NULL, values = NULL, units = NULL)
  newParams <- list(
    paths = c("Path1", "Path2"),
    values = c(1, 2),
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
  newParams <- list(paths = NULL, values = NULL, units = NULL)
  params <- list(
    paths = c("Path1", "Path2"),
    values = c(1, 2),
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
    paths = c("Path1", "Path2"),
    values = c(1, 2),
    units = c("", "µmol")
  )
  newParams <- list(
    paths = c("Path2", "Path3"),
    values = c(1, 3),
    units = c("", "µmol")
  )

  extended <- extendParameterStructure(
    parameters = params,
    newParameters = newParams
  )

  expect_equal(extended$paths, c("Path1", "Path2", "Path3"))
  expect_equal(extended$values, c(1, 1, 3))
  expect_equal(extended$units, c("", "", "µmol"))
})

test_that("removeModelParameterEntry auto-removes empty parameter sets", {
  project <- testProject()
  addModelParameterEntry(project, "TempSet", "Organism|A", "K", 1.5, "1/h")
  expect_true("TempSet" %in% names(project$modelParameterSets))

  removeModelParameterEntry(project, "TempSet", "Organism|A", "K")
  expect_false("TempSet" %in% names(project$modelParameterSets))
})

# readInitialValuesFromXLS ----

initialValuesXLSpath <- file.path(dataFolder, "InitialValues.xlsx")

test_that("`readInitialValuesFromXLS()` reads a valid sheet and builds molecule paths", {
  initialValues <- readInitialValuesFromXLS(
    filePath = initialValuesXLSpath,
    sheets = "ValidSheet"
  )

  expect_true(.validateParametersStructure(initialValues))
  expect_setequal(
    initialValues$paths,
    c("Organism|Liver|A", "Organism|Liver|B")
  )
})

test_that("`readInitialValuesFromXLS()` returns the values and units of present molecules", {
  initialValues <- readInitialValuesFromXLS(
    filePath = initialValuesXLSpath,
    sheets = "ValidSheet"
  )

  idxA <- which(initialValues$paths == "Organism|Liver|A")
  idxB <- which(initialValues$paths == "Organism|Liver|B")
  expect_equal(initialValues$values[[idxA]], 0.5)
  expect_equal(initialValues$values[[idxB]], 1.0)
  expect_equal(initialValues$units[[idxA]], "µmol")
  expect_equal(initialValues$units[[idxB]], "µmol")
})

test_that("`readInitialValuesFromXLS()` ignores rows where 'Is Present' is FALSE but keeps NA", {
  initialValues <- readInitialValuesFromXLS(
    filePath = initialValuesXLSpath,
    sheets = "ValidSheet"
  )

  expect_false("Organism|Kidney|A" %in% initialValues$paths)
  expect_true("Organism|Liver|B" %in% initialValues$paths)
})

test_that("`readInitialValuesFromXLS()` uses the first sheet when no sheet is provided", {
  initialValues <- readInitialValuesFromXLS(filePath = initialValuesXLSpath)

  expect_setequal(
    initialValues$paths,
    c("Organism|Liver|A", "Organism|Liver|B")
  )
})

test_that("`readInitialValuesFromXLS()` overwrites values when a path appears in multiple sheets", {
  initialValues <- readInitialValuesFromXLS(
    filePath = initialValuesXLSpath,
    sheets = c("ValidSheet", "SecondSheet")
  )

  idxA <- which(initialValues$paths == "Organism|Liver|A")
  expect_equal(initialValues$values[[idxA]], 2.5)
  expect_length(idxA, 1)
})

test_that("`readInitialValuesFromXLS()` returns empty structure when all molecules are absent", {
  initialValues <- readInitialValuesFromXLS(
    filePath = initialValuesXLSpath,
    sheets = "AllAbsent"
  )

  expect_true(.validateParametersStructure(initialValues))
  expect_length(initialValues$paths, 0)
  expect_length(initialValues$values, 0)
  expect_length(initialValues$units, 0)
})

test_that("`readInitialValuesFromXLS()` errors when units are missing for a present molecule", {
  expect_error(
    readInitialValuesFromXLS(
      filePath = initialValuesXLSpath,
      sheets = "MissingUnits"
    ),
    regexp = messages$errorMissingUnitsInInitialValues(
      filePath = initialValuesXLSpath,
      moleculePaths = "Organism|Liver|A"
    ),
    fixed = TRUE
  )
})

test_that("`readInitialValuesFromXLS()` errors when a value is missing for a present molecule", {
  expect_error(
    readInitialValuesFromXLS(
      filePath = initialValuesXLSpath,
      sheets = "MissingValue"
    ),
    regexp = messages$errorMissingValuesInInitialValues(
      filePath = initialValuesXLSpath,
      moleculePaths = "Organism|Liver|A"
    ),
    fixed = TRUE
  )
})

test_that("`readInitialValuesFromXLS()` errors on a non-logical 'Is Present' value", {
  expect_error(
    readInitialValuesFromXLS(
      filePath = initialValuesXLSpath,
      sheets = "BadIsPresent"
    ),
    regexp = messages$errorInvalidIsPresentInInitialValues(
      filePath = initialValuesXLSpath,
      moleculePaths = "Organism|Liver|A"
    ),
    fixed = TRUE
  )
})

test_that("`readInitialValuesFromXLS()` accepts numeric 0/1 for 'Is Present'", {
  initialValues <- readInitialValuesFromXLS(
    filePath = initialValuesXLSpath,
    sheets = "NumericIsPresent"
  )

  expect_setequal(initialValues$paths, "Organism|Liver|A")
  expect_equal(initialValues$values[[1]], 0.5)
})

test_that("`readInitialValuesFromXLS()` warns and keeps the last value for a duplicate path", {
  expect_warning(
    initialValues <- readInitialValuesFromXLS(
      filePath = initialValuesXLSpath,
      sheets = "DuplicatePath"
    ),
    regexp = messages$warningDuplicateInitialValues(
      filePath = initialValuesXLSpath,
      moleculePaths = "Organism|Liver|A"
    ),
    fixed = TRUE
  )

  expect_setequal(initialValues$paths, "Organism|Liver|A")
  expect_equal(initialValues$values[[1]], 2.5)
})

test_that("`readInitialValuesFromXLS()` errors when a present row has a blank container path", {
  expect_error(
    readInitialValuesFromXLS(
      filePath = initialValuesXLSpath,
      sheets = "BlankPath"
    ),
    regexp = messages$errorMissingPathInInitialValues(
      filePath = initialValuesXLSpath,
      sheet = "BlankPath",
      rows = 1
    ),
    fixed = TRUE
  )
})

test_that("`readInitialValuesFromXLS()` errors when a sheet has the wrong structure", {
  columnNames <- c(
    "Container Path",
    "Molecule Name",
    "Is Present",
    "Value",
    "Units",
    "Scale Divisor",
    "Neg. Values Allowed"
  )
  expect_error(
    readInitialValuesFromXLS(
      filePath = initialValuesXLSpath,
      sheets = "InvalidSheet"
    ),
    regexp = messages$errorWrongXLSStructure(
      filePath = initialValuesXLSpath,
      expectedColNames = columnNames
    ),
    fixed = TRUE
  )
})

test_that("`readInitialValuesFromXLS()` validates its arguments", {
  expect_error(
    readInitialValuesFromXLS(filePath = 123),
    regexp = 'argument "filePath" is of type <numeric>, but expected <character>!',
    fixed = TRUE
  )
  expect_error(
    readInitialValuesFromXLS(filePath = initialValuesXLSpath, sheets = 123),
    regexp = 'argument "sheets" is of type <numeric>, but expected <character>!',
    fixed = TRUE
  )
})

test_that("remove*ParameterEntry no-op on missing entry does not mark modified", {
  project <- testProject()
  addModelParameterEntry(project, "MSet", "Organism|A", "K", 1.5, "1/h")
  addApplicationParameterEntry(
    project,
    "ASet",
    "Organism|B",
    "K",
    2,
    "1/h"
  )
  addIndividualParameterEntry(
    project,
    "ISet",
    "Organism|C",
    "K",
    3,
    "1/h"
  )
  project$.markValidated()
  expect_true(project$validatedSinceMutation)

  expect_warning(
    removeModelParameterEntry(project, "MSet", "Organism|A", "Ghost"),
    "not found"
  )
  expect_warning(
    removeApplicationParameterEntry(project, "ASet", "Organism|B", "Ghost"),
    "not found"
  )
  expect_warning(
    removeIndividualParameterEntry(project, "ISet", "Organism|C", "Ghost"),
    "not found"
  )

  expect_true(project$validatedSinceMutation)
})
