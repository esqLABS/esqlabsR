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
    regexp = messages$wrongParametersStructure(argumentName = "parameters")
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

test_that("addModelParameterGroup adds a new group", {
  pc <- testProject()
  initial <- length(pc$modelParameters)
  addModelParameterGroup(
    pc,
    group = "MyLiver",
    paths = c("Organism|Liver|Volume", "Organism|Liver|Q"),
    values = c(1.8, 90),
    units = c("L", "mL/min")
  )
  expect_equal(length(pc$modelParameters), initial + 1)
  grp <- pc$modelParameters[["MyLiver"]]
  expect_equal(grp$paths, c("Organism|Liver|Volume", "Organism|Liver|Q"))
  expect_equal(grp$values, c(1.8, 90))
  expect_equal(grp$units, c("L", "mL/min"))
  expect_true(pc$modified)
})

test_that("addModelParameterGroup errors on duplicate group", {
  pc <- testProject()
  existing <- names(pc$modelParameters)[[1]]
  expect_error(
    addModelParameterGroup(pc, existing, "a", 1, "L"),
    regexp = "already exists"
  )
})

test_that("addModelParameterGroup errors on length mismatch", {
  pc <- testProject()
  expect_error(
    addModelParameterGroup(pc, "Bad", c("a", "b"), c(1), c("L", "L")),
    regexp = "same length"
  )
})

test_that("addModelParameterGroup errors on empty paths", {
  pc <- testProject()
  expect_error(
    addModelParameterGroup(
      pc,
      "Empty",
      paths = character(0),
      values = numeric(0),
      units = character(0)
    ),
    regexp = "non-empty character vector"
  )
})

test_that("addModelParameterGroup errors on non-numeric values", {
  pc <- testProject()
  expect_error(
    addModelParameterGroup(pc, "G", "a", "not-a-number", "L"),
    regexp = "values"
  )
})

test_that("removeModelParameterGroup removes group", {
  pc <- testProject()
  addModelParameterGroup(pc, "Tmp", "a", 1, "L")
  pc$modified <- FALSE
  removeModelParameterGroup(pc, "Tmp")
  expect_false("Tmp" %in% names(pc$modelParameters))
  expect_true(pc$modified)
})

test_that("removeModelParameterGroup warns on missing", {
  pc <- testProject()
  expect_warning(
    removeModelParameterGroup(pc, "NoSuchGroup_ZZ"),
    regexp = "not found"
  )
})

test_that("project$addModelParameterGroup delegates to standalone", {
  pc1 <- testProject()
  pc2 <- testProject()
  addModelParameterGroup(pc1, "X", "a", 1, "L")
  pc2$addModelParameterGroup("X", "a", 1, "L")
  expect_equal(pc1$modelParameters[["X"]], pc2$modelParameters[["X"]])
})

test_that("addApplicationGroup adds a new application group", {
  pc <- testProject()
  initial <- length(pc$applications)
  addApplicationGroup(
    pc,
    protocol = "Oral_10mg",
    paths = c("App|Oral|DoseValue"),
    values = c(10),
    units = c("mg")
  )
  expect_equal(length(pc$applications), initial + 1)
  expect_equal(pc$applications[["Oral_10mg"]]$values, 10)
  expect_true(pc$modified)
})

test_that("addApplicationGroup errors on duplicate protocol", {
  pc <- testProject()
  existing <- names(pc$applications)[[1]]
  expect_error(
    addApplicationGroup(pc, existing, "a", 1, "mg"),
    regexp = "already exists"
  )
})

test_that("removeApplicationGroup removes group", {
  pc <- testProject()
  addApplicationGroup(pc, "TmpApp", "a", 1, "mg")
  pc$modified <- FALSE
  removeApplicationGroup(pc, "TmpApp")
  expect_false("TmpApp" %in% names(pc$applications))
})

test_that("addModelParameterGroup survives round-trip", {
  pc <- testProject()
  addModelParameterGroup(
    pc,
    "RTGroup",
    paths = "Organism|Liver|Volume",
    values = 2.0,
    units = "L"
  )
  tmp <- tempfile(fileext = ".json")
  saveProject(pc, tmp)
  reloaded <- loadProject(tmp)
  expect_equal(reloaded$modelParameters[["RTGroup"]]$values, 2.0)
})

test_that("addModelParameterGroup rejects NA and Inf in values", {
  pc <- testProject()
  expect_error(
    addModelParameterGroup(pc, "G1", "a", c(1, NA), c("L", "L")),
    regexp = "NA|NaN|Inf"
  )
  expect_error(
    addModelParameterGroup(pc, "G2", "a", Inf, "L"),
    regexp = "NA|NaN|Inf"
  )
  expect_error(
    addModelParameterGroup(pc, "G3", c("a", NA), c(1, 2), c("L", "L")),
    regexp = "NA|empty"
  )
})

test_that("addApplicationGroup error message uses the right label", {
  pc <- testProject()
  expect_error(
    addApplicationGroup(pc, "BadApp", c("a", "b"), c(1), c("mg", "mg")),
    regexp = "application 'BadApp'"
  )
})
