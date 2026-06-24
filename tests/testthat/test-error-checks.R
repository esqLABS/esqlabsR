# .validateParametersStructure ----------------------------------------------

test_that("`.validateParametersStructure()` accepts a well-formed structure", {
  validStructure <- list(
    paths = c("Organism|A", "Organism|B"),
    values = c(1, 2),
    units = c("µmol", "µmol")
  )
  expect_true(.validateParametersStructure(validStructure))
})

test_that("`.validateParametersStructure()` accepts an empty structure", {
  # Mirrors the empty result produced by `.parametersVectorToList()`, where
  # `paths` is NULL while `values`/`units` are zero-length typed vectors.
  emptyStructure <- list(
    paths = NULL,
    values = numeric(0),
    units = character(0)
  )
  expect_true(.validateParametersStructure(emptyStructure))
})

test_that("`.validateParametersStructure()` honours nullAllowed", {
  expect_true(.validateParametersStructure(NULL, nullAllowed = TRUE))
  expect_error(
    .validateParametersStructure(NULL, nullAllowed = FALSE),
    regexp = messages$wrongParametersStructure(argumentName = NULL),
    fixed = TRUE
  )
})

test_that("`.validateParametersStructure()` rejects wrong names", {
  expect_error(
    .validateParametersStructure(list(paths = "A", values = 1)),
    regexp = messages$wrongParametersStructure(argumentName = NULL),
    fixed = TRUE
  )
})

test_that("`.validateParametersStructure()` rejects non-character paths", {
  expect_error(
    .validateParametersStructure(list(
      paths = c(1, 2),
      values = c(1, 2),
      units = c("µmol", "µmol")
    )),
    regexp = messages$wrongParametersStructure(argumentName = NULL),
    fixed = TRUE
  )
})

test_that("`.validateParametersStructure()` rejects non-numeric values", {
  expect_error(
    .validateParametersStructure(list(
      paths = c("Organism|A", "Organism|B"),
      values = c("1", "2"),
      units = c("µmol", "µmol")
    )),
    regexp = messages$wrongParametersStructure(argumentName = NULL),
    fixed = TRUE
  )
})

test_that("`.validateParametersStructure()` rejects non-character units", {
  expect_error(
    .validateParametersStructure(list(
      paths = c("Organism|A", "Organism|B"),
      values = c(1, 2),
      units = c(1, 2)
    )),
    regexp = messages$wrongParametersStructure(argumentName = NULL),
    fixed = TRUE
  )
})

test_that("`.validateParametersStructure()` rejects vectors of unequal length", {
  expect_error(
    .validateParametersStructure(list(
      paths = c("Organism|A", "Organism|B"),
      values = c(1, 2),
      units = "µmol"
    )),
    regexp = messages$wrongParametersStructure(argumentName = NULL),
    fixed = TRUE
  )
})
