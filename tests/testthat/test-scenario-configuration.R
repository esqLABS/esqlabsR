test_that("`ScenarioConfiguration` active bindings are modified", {
  expect_snapshot({
    mySC <- ScenarioConfiguration$new(
      projectConfiguration = ProjectConfiguration$new()
    )

    mySC$simulateSteadyState <- TRUE

    mySC$simulationTime <- "0, 10, 1"
    mySC$steadyStateTime <- 5
    mySC$simulationType <- "Population"

    mySC$removeParamSheets(NULL)
    mySC$addParamSheets(c("mySheet1", "mySheet2"))

    mySC$print(projectConfiguration = FALSE)
  })
})

test_that("It produces expected errors for incorrect active binding values", {
  mySC <- ScenarioConfiguration$new(
    projectConfiguration = ProjectConfiguration$new()
  )

  expect_error(
    mySC$simulateSteadyState <- 1,
    regexp = messages$errorWrongType("value", "numeric", "logical"),
    fixed = TRUE
  )
  expect_error(
    mySC$steadyStateTime <- -1,
    regexp = messages$valueShouldNotBeNegative("steadyStateTime", -1),
    fixed = TRUE
  )
  expect_error(
    mySC$simulationType <- "X",
    regexp = messages$wrongSimulationType(),
    fixed = TRUE
  )
})

test_that("`addInitialValuesSheets()` and `removeInitialValuesSheets()` manage the sheet list", {
  mySC <- ScenarioConfiguration$new(
    projectConfiguration = ProjectConfiguration$new()
  )

  # Empty by default
  expect_length(enumKeys(mySC$initialValuesSheets), 0)

  mySC$addInitialValuesSheets(c("Global", "Liver"))
  expect_setequal(enumKeys(mySC$initialValuesSheets), c("Global", "Liver"))

  # Removing a single sheet keeps the rest
  mySC$removeInitialValuesSheets("Global")
  expect_setequal(enumKeys(mySC$initialValuesSheets), "Liver")

  # Removing with NULL clears all sheets
  mySC$addInitialValuesSheets("Kidney")
  mySC$removeInitialValuesSheets(NULL)
  expect_length(enumKeys(mySC$initialValuesSheets), 0)
})

test_that("`initialValuesSheets` is read-only", {
  mySC <- ScenarioConfiguration$new(
    projectConfiguration = ProjectConfiguration$new()
  )

  expect_error(
    mySC$initialValuesSheets <- list("Global"),
    regexp = messages$errorPropertyReadOnly("initialValuesSheets"),
    fixed = TRUE
  )
})

test_that("`overwriteFormulasInSS` defaults to FALSE and can be set", {
  mySC <- ScenarioConfiguration$new(
    projectConfiguration = ProjectConfiguration$new()
  )

  expect_false(mySC$overwriteFormulasInSS)

  mySC$overwriteFormulasInSS <- TRUE
  expect_true(mySC$overwriteFormulasInSS)
})

test_that("`overwriteFormulasInSS` treats NA as FALSE", {
  mySC <- ScenarioConfiguration$new(
    projectConfiguration = ProjectConfiguration$new()
  )

  mySC$overwriteFormulasInSS <- NA
  expect_false(mySC$overwriteFormulasInSS)
})

test_that("`overwriteFormulasInSS` rejects non-logical values", {
  mySC <- ScenarioConfiguration$new(
    projectConfiguration = ProjectConfiguration$new()
  )

  expect_error(
    mySC$overwriteFormulasInSS <- "yes",
    regexp = 'argument "value" is of type <character>, but expected <logical>!',
    fixed = TRUE
  )
})
