## context("readScenarioConfigurationFromExcel")
# Create a project configuration
projectConfiguration <- createDefaultProjectConfiguration(path = "../data/ProjectConfiguration_forTests.xlsx")

test_that("it throws an error when wrong scenario is defined", {
  scenarioNames <- "wrong"
  expect_error(readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  ),
  regexp = messages$scenarioConfigurationNameNotFoundWhenReading(scenarioNames[[1]])
  )
})

test_that("It creates the correct scenario", {
  scenarioNames <- "TestScenario"

  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )
  expect_equal(names(scenarioConfigurations), scenarioNames)
  expect_equal(scenarioConfigurations[[scenarioNames]]$applicationProtocol, "Aciclovir_iv_250mg")
  expect_equal(scenarioConfigurations[[scenarioNames]]$individualId, "Indiv1")
  expect_equal(scenarioConfigurations[[scenarioNames]]$modelFile, "Aciclovir.pkml")
  expect_equal(scenarioConfigurations[[scenarioNames]]$paramSheets, enum(enumValues = "Global"))
  expect_equal(scenarioConfigurations[[scenarioNames]]$pointsPerMinute, 1)
  expect_equal(scenarioConfigurations[[scenarioNames]]$scenarioName, "TestScenario")
  expect_equal(scenarioConfigurations[[scenarioNames]]$setTestParameters, FALSE)
  expect_equal(scenarioConfigurations[[scenarioNames]]$simulateSteadyState, FALSE)
  expect_equal(scenarioConfigurations[[scenarioNames]]$simulationTime, toUnit(
    quantityOrDimension = ospDimensions$Time,
    values = 24,
    targetUnit = ospUnits$Time$min,
    sourceUnit = ospUnits$Time$h
  ))
  expect_equal(scenarioConfigurations[[scenarioNames]]$simulationRunOptions, NULL)
  expect_equal(scenarioConfigurations[[scenarioNames]]$simulationType, "Individual")
  expect_equal(scenarioConfigurations[[scenarioNames]]$steadyStateTime, 1000)
})

test_that("It creates multiple correct scenarios", {
  scenarioNames <- c("TestScenario", "TestScenario2")

  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )
  expect_equal(names(scenarioConfigurations), scenarioNames)
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$applicationProtocol, "Aciclovir_iv_250mg")
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$individualId, "Indiv1")
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$modelFile, "Aciclovir.pkml")
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$paramSheets, enum(enumValues = "Global"))
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$pointsPerMinute, 1)
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$scenarioName, "TestScenario")
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$setTestParameters, FALSE)
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$simulateSteadyState, FALSE)
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$simulationTime, toUnit(
    quantityOrDimension = ospDimensions$Time,
    values = 24,
    targetUnit = ospUnits$Time$min,
    sourceUnit = ospUnits$Time$h
  ))
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$simulationRunOptions, NULL)
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$simulationType, "Individual")
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$steadyStateTime, 1000)

  # Second scenario
  expect_equal(scenarioConfigurations[[scenarioNames[[2]]]]$applicationProtocol, "Aciclovir_iv_250mg")
  expect_equal(scenarioConfigurations[[scenarioNames[[2]]]]$individualId, "Indiv")
  expect_equal(scenarioConfigurations[[scenarioNames[[2]]]]$modelFile, "Aciclovir.pkml")
  expect_equal(scenarioConfigurations[[scenarioNames[[2]]]]$paramSheets, enum(enumValues = "Global"))
  expect_equal(scenarioConfigurations[[scenarioNames[[2]]]]$pointsPerMinute, 1)
  expect_equal(scenarioConfigurations[[scenarioNames[[2]]]]$scenarioName, "TestScenario2")
  expect_equal(scenarioConfigurations[[scenarioNames[[2]]]]$setTestParameters, FALSE)
  expect_equal(scenarioConfigurations[[scenarioNames[[2]]]]$simulateSteadyState, TRUE)
  expect_equal(scenarioConfigurations[[scenarioNames[[2]]]]$simulationTime, toUnit(
    quantityOrDimension = ospDimensions$Time,
    values = 12,
    targetUnit = ospUnits$Time$min,
    sourceUnit = ospUnits$Time$h
  ))
  expect_equal(scenarioConfigurations[[scenarioNames[[2]]]]$simulationRunOptions, NULL)
  expect_equal(scenarioConfigurations[[scenarioNames[[2]]]]$simulationType, "Individual")
  expect_equal(scenarioConfigurations[[scenarioNames[[2]]]]$steadyStateTime, 500)
})
