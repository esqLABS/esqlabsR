test_that("PITaskConfiguration can be created with projectConfiguration only", {
  projectConfiguration <- ProjectConfiguration$new()

  piTaskConfig <- PITaskConfiguration$new(
    projectConfiguration = projectConfiguration
  )

  expect_true(isOfType(piTaskConfig, "PITaskConfiguration"))
  expect_identical(piTaskConfig$projectConfiguration, projectConfiguration)

  expect_null(piTaskConfig$piTaskName)
  expect_null(piTaskConfig$scenarioName)
  expect_null(piTaskConfig$modelFile)
  expect_null(piTaskConfig$piParameters)
  expect_null(piTaskConfig$piOutputMappings)
  expect_null(piTaskConfig$piConfiguration)
})

test_that("PITaskConfiguration prints as expected", {
  projectConfiguration <- ProjectConfiguration$new()

  piTaskConfig <- PITaskConfiguration$new(
    projectConfiguration = projectConfiguration
  )
  piTaskConfig$piTaskName <- "piTestTask"

  expect_snapshot(piTaskConfig$print())
})
