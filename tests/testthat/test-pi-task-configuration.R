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
  piTaskConfigs <- readPITaskConfigurationFromExcel(
    projectConfiguration = testProjectConfiguration()
  )

  piTaskConfig <- piTaskConfigs[[1]]

  expect_output(piTaskConfig$print(projectConfiguration = TRUE))
  expect_output(piTaskConfig$print(scenarioConfiguration = TRUE))

  expect_snapshot(piTaskConfig$print())
  expect_snapshot(print(piTaskConfig$piConfiguration))
  expect_snapshot(print(piTaskConfig$piParameters))
  expect_snapshot(print(piTaskConfig$piOutputMappings))
})

test_that("PITaskConfiguration prints multiple scenarios, parameters, and mappings", {
  projectConfig <- testProjectConfiguration()
  scenarioConfig1 <- ScenarioConfiguration$new(projectConfig)
  scenarioConfig1$scenarioName <- "Scenario1"
  scenarioConfig1$modelFile <- "model1.pkml"
  scenarioConfig2 <- scenarioConfig1$clone()
  scenarioConfig2$scenarioName <- "Scenario2"
  scenarioConfig2$modelFile <- "model2.pkml"

  piTaskConfig <- PITaskConfiguration$new(
    taskName = "multiTest",
    projectConfiguration = projectConfig,
    scenarioConfiguration = list(
      Scenario1 = scenarioConfig1,
      Scenario2 = scenarioConfig2
    ),
    piDefinitions = list(
      piParameters = list(
        list(`Container Path` = "A", `Parameter Name` = "P1"),
        list(`Container Path` = "B", `Parameter Name` = "P2")
      ),
      piOutputMappings = list(
        list(ObservedDataSheet = "Sheet1", Scaling = "lin"),
        list(ObservedDataSheet = "Sheet2", Scaling = "log")
      )
    )
  )

  expect_snapshot(piTaskConfig$print())
  expect_snapshot(print(piTaskConfig$piParameters))
  expect_snapshot(print(piTaskConfig$piOutputMappings))
})
