##  context("ScenarioConfiguration")

test_that("`ScenarioConfiguration` defaults are as expected", {
  expect_snapshot({
    mySC <- ScenarioConfiguration$new(projectConfiguration = ProjectConfiguration$new())
    mySC
  })
})
