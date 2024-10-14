##  context("ScenarioConfiguration")

test_that("`ScenarioConfiguration` active bindings are modified", {
  skip()
  expect_snapshot({
    mySC <- ScenarioConfiguration$new(projectConfiguration = ProjectConfiguration$new())

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
  skip()
  mySC <- ScenarioConfiguration$new(projectConfiguration = ProjectConfiguration$new())

  expect_error(mySC$simulateSteadyState <- 1, messages$errorWrongType("value", "numeric", "logical"))
  expect_error(mySC$steadyStateTime <- -1, messages$valueShouldNotBeNegative("steadyStateTime", -1))
  expect_error(mySC$simulationType <- "X", messages$wrongSimulationType())
})
