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
