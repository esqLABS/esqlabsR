##  context("ScenarioConfiguration")

test_that("`ScenarioConfiguration` active bindings are modified", {
  expect_snapshot({
    mySC <- ScenarioConfiguration$new(projectConfiguration = ProjectConfiguration$new())

    mySC$simulateSteadyState <- TRUE

    mySC$simulationTime <- 10
    mySC$steadyStateTime <- 5
    mySC$pointsPerMinute <- 100
    mySC$simulationType <- "Population"

    mySC$removeParamSheets(NULL)
    mySC$addParamSheets(c("mySheet1", "mySheet2"))

    mySC
  })
})

test_that("It produces expected errors for incorrect active binding values", {
  mySC <- ScenarioConfiguration$new(projectConfiguration = ProjectConfiguration$new())

  expect_error(mySC$simulateSteadyState <- 1, messages$errorWrongType("value", "numeric", "logical"))
  expect_error(mySC$simulationTime <- -1, messages$valueShouldNotBeNegative("simulationTime", -1))
  expect_error(mySC$steadyStateTime <- -1, messages$valueShouldNotBeNegative("steadyStateTime", -1))
  expect_error(mySC$pointsPerMinute <- -1, messages$valueShouldNotBeNegative("pointsPerMinute", -1))
  expect_error(mySC$simulationType <- "X", messages$wrongSimulationType())
})
