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


test_that("addParameter on Individual appends entry to parallel-vector parameters", {
  indiv <- structure(
    list(species = "Human", parameters = NULL),
    class = c("Individual", "list")
  )
  out <- addParameter(
    indiv,
    containerPath = "Organism|Liver",
    parameterName = "Volume",
    value = 1.8,
    units = "L"
  )
  expect_true(inherits(out, "Individual"))
  expect_equal(out$parameters$paths, "Organism|Liver|Volume")
  expect_equal(out$parameters$values, 1.8)
  expect_equal(out$parameters$units, "L")
})

test_that("addParameter on Individual extends existing parameters", {
  indiv <- structure(
    list(
      species = "Human",
      parameters = list(
        paths = "Organism|Kidney|GFR",
        values = 90,
        units = "ml/min"
      )
    ),
    class = c("Individual", "list")
  )
  out <- addParameter(
    indiv,
    containerPath = "Organism|Liver",
    parameterName = "Volume",
    value = 1.8,
    units = "L"
  )
  expect_equal(
    out$parameters$paths,
    c("Organism|Kidney|GFR", "Organism|Liver|Volume")
  )
  expect_equal(out$parameters$values, c(90, 1.8))
})

test_that("addParameter overwrites duplicate path silently (last-write-wins)", {
  indiv <- structure(
    list(
      species = "Human",
      parameters = list(
        paths = "Organism|Liver|Volume",
        values = 1.5,
        units = "L"
      )
    ),
    class = c("Individual", "list")
  )
  out <- addParameter(
    indiv,
    containerPath = "Organism|Liver",
    parameterName = "Volume",
    value = 2.0,
    units = "L"
  )
  expect_equal(out$parameters$paths, "Organism|Liver|Volume")
  expect_equal(out$parameters$values, 2.0)
})

test_that("addParameter on Application works the same as on Individual", {
  app <- structure(
    list(parameters = NULL),
    class = c("Application", "list")
  )
  out <- addParameter(
    app,
    containerPath = "Events|Oral|Schema",
    parameterName = "Dose",
    value = 250,
    units = "mg"
  )
  expect_true(inherits(out, "Application"))
  expect_equal(out$parameters$paths, "Events|Oral|Schema|Dose")
  expect_equal(out$parameters$values, 250)
})

test_that("removeParameter drops the entry by containerPath + parameterName", {
  indiv <- structure(
    list(
      species = "Human",
      parameters = list(
        paths = c("Organism|Liver|Volume", "Organism|Kidney|GFR"),
        values = c(1.8, 90),
        units = c("L", "ml/min")
      )
    ),
    class = c("Individual", "list")
  )
  out <- removeParameter(
    indiv,
    containerPath = "Organism|Liver",
    parameterName = "Volume"
  )
  expect_equal(out$parameters$paths, "Organism|Kidney|GFR")
  expect_equal(out$parameters$values, 90)
})

test_that("removeParameter sets parameters to NULL when last entry removed", {
  indiv <- structure(
    list(
      species = "Human",
      parameters = list(
        paths = "Organism|Liver|Volume",
        values = 1.8,
        units = "L"
      )
    ),
    class = c("Individual", "list")
  )
  out <- removeParameter(
    indiv,
    containerPath = "Organism|Liver",
    parameterName = "Volume"
  )
  expect_null(out$parameters)
})

test_that("removeParameter warns and returns unchanged if entry not found", {
  indiv <- structure(
    list(
      species = "Human",
      parameters = list(
        paths = "Organism|Liver|Volume",
        values = 1.8,
        units = "L"
      )
    ),
    class = c("Individual", "list")
  )
  expect_warning(
    out <- removeParameter(
      indiv,
      containerPath = "Organism|Kidney",
      parameterName = "GFR"
    ),
    regexp = "not found"
  )
  expect_equal(out$parameters$paths, "Organism|Liver|Volume")
})

test_that("addParameter errors on bad input shapes", {
  indiv <- structure(
    list(species = "Human", parameters = NULL),
    class = c("Individual", "list")
  )
  expect_error(
    addParameter(
      indiv,
      containerPath = "",
      parameterName = "x",
      value = 1,
      units = "L"
    ),
    regexp = "containerPath"
  )
  expect_error(
    addParameter(
      indiv,
      containerPath = "a",
      parameterName = "",
      value = 1,
      units = "L"
    ),
    regexp = "parameterName"
  )
  expect_error(
    addParameter(
      indiv,
      containerPath = "a",
      parameterName = "x",
      value = "nope",
      units = "L"
    ),
    regexp = "value"
  )
})
