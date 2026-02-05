test_that("PITaskConfiguration can be created with required parameters", {
  projectConfiguration <- ProjectConfiguration$new()
  scenarioConfiguration <- ScenarioConfiguration$new(projectConfiguration)
  scenarioConfiguration$scenarioName <- "TestScenario"
  scenarioConfiguration$modelFile <- "Test.pkml"

  expect_no_error(
    piTaskConfig <- PITaskConfiguration$new(
      taskName = "TestTask",
      projectConfiguration = projectConfiguration,
      scenarioConfiguration = scenarioConfiguration,
      piDefinitions <- list(
        piConfiguration = list(),
        piParameters = list(),
        piOutputMappings = list()
      )
    )
  )
  expect_true(isOfType(piTaskConfig, "PITaskConfiguration"))
  expect_equal(piTaskConfig$taskName, "TestTask")
  expect_identical(piTaskConfig$projectConfiguration, projectConfiguration)
  expect_identical(
    piTaskConfig$scenarioConfiguration[[1]],
    scenarioConfiguration
  )
})

test_that("PITaskConfiguration fields are read-only", {
  projectConfiguration <- ProjectConfiguration$new()
  scenarioConfiguration <- ScenarioConfiguration$new(projectConfiguration)

  piTaskConfig <- PITaskConfiguration$new(
    taskName = "TestTask",
    projectConfiguration = projectConfiguration,
    scenarioConfiguration = scenarioConfiguration
  )

  expect_error(
    piTaskConfig$taskName <- "NewName",
    regexp = messages$errorPropertyReadOnly("taskName"),
    fixed = TRUE
  )
  expect_error(
    piTaskConfig$projectConfiguration <- projectConfiguration,
    regexp = messages$errorPropertyReadOnly("projectConfiguration"),
    fixed = TRUE
  )
  expect_error(
    piTaskConfig$scenarioConfiguration <- scenarioConfiguration,
    regexp = messages$errorPropertyReadOnly("scenarioConfiguration"),
    fixed = TRUE
  )
  expect_error(
    piTaskConfig$projectConfiguration <- projectConfiguration,
    regexp = messages$errorPropertyReadOnly("projectConfiguration"),
    fixed = TRUE
  )

  expect_error(
    piTaskConfig$piConfiguration <- list(),
    regexp = messages$errorPropertyReadOnly("piConfiguration"),
    fixed = TRUE
  )
  expect_error(
    piTaskConfig$piParameters <- list(),
    regexp = messages$errorPropertyReadOnly("piParameters"),
    fixed = TRUE
  )
  expect_error(
    piTaskConfig$piOutputMappings <- list(),
    regexp = messages$errorPropertyReadOnly("piOutputMappings"),
    fixed = TRUE
  )
})

test_that("PITaskConfiguration prints as expected", {
  projectConfiguration <- ProjectConfiguration$new()

  piTaskConfig <- PITaskConfiguration$new(
    projectConfiguration = projectConfiguration
  )
  piTaskConfig$piTaskName <- "piTestTask"

  expect_snapshot(piTaskConfig$print())
})
