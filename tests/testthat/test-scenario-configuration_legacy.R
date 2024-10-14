##  context("ScenarioConfiguration")

test_that("`ScenarioConfiguration` active bindings are modified", {
  skip()
  expect_snapshot({
    project <- testProject()

    mySC <- ScenarioConfiguration$new(
      project = project,
      scenarioConfigurationData = project$configurations$scenarios$TestScenario$toDataFrame()
    )

    mySC$simulationTime <- "0, 10, 1"
    mySC$steadyStateTime <- 5

    mySC$print()
  })
})

test_that("It produces expected errors for incorrect active binding values", {
  skip()
  mySC <- ScenarioConfiguration$new(projectConfiguration = ProjectConfiguration$new())

  expect_error(mySC$simulateSteadyState <- 1)
  expect_error(mySC$simulationType <- "X")
})
