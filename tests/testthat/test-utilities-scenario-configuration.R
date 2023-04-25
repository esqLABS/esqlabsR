## context("readScenarioConfigurationFromExcel")
# Create a project configuration
projectConfiguration <- createDefaultProjectConfiguration(path = "../data/ProjectConfiguration_forTests.xlsx")

# Template scenario configuration for testing
scenariosDf <- data.frame(list(
  "Scenario_name" = "TestScenario",
  "IndividualId" = "Indiv1",
  "PopulationId" = c(NA),
  "ReadPopulationFromCSV" = c(NA),
  "ModelParameterSheets" = c(NA),
  "ApplicationProtocol" = c(NA),
  "SimulationTime" = c(NA),
  "SimulationTimeUnit" = c(NA),
  "SteadyState" = c(NA),
  "SteadyStateTime" = c(NA),
  "SteadyStateTimeUnit" = c(NA),
  "ModelFile" = c("Aciclovir.pkml"),
  "OutputPathsIds" = c(NA)
))

test_that("it throws an error when wrong scenario is defined", {
  scenarioNames <- "wrong"
  expect_error(
    readScenarioConfigurationFromExcel(
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

test_that("It does not fail on empty rows", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(
    new = "Scenarios.xlsx",
    tmpdir = tempDir,
    code = {
      scenariosDfLocal <- as.data.frame(lapply(scenariosDf, rep, 2))
      scenariosDfLocal[3, ] <- scenariosDfLocal[2, ]
      scenariosDfLocal[2, ] <- scenariosDfLocal[4, ]
      scenariosDfLocal[3, ]$Scenario_name <- "TestScenario2"
      writeExcel(data = list(
        "Scenarios" = scenariosDfLocal,
        "OutputPaths" = data.frame()
      ), path = file.path(tempDir, "Scenarios.xlsx"), )

      scenarioConfigs <- readScenarioConfigurationFromExcel(projectConfiguration = projectConfigurationLocal)
      expect_equal(names(scenarioConfigs), c("TestScenario", "TestScenario2"))
    }
  )
})

test_that("It creates a population scenario", {
  scenarioNames <- "PopulationScenario"

  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )
  expect_equal(names(scenarioConfigurations), scenarioNames)
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$applicationProtocol, "Aciclovir_iv_250mg")
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$individualId, "Indiv")
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$populationId, "TestPopulation")
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$modelFile, "Aciclovir.pkml")
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$paramSheets, enum(enumValues = "Global"))
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$pointsPerMinute, 1)
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$scenarioName, "PopulationScenario")
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$setTestParameters, FALSE)
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$simulateSteadyState, FALSE)
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$simulationTime, toUnit(
    quantityOrDimension = ospDimensions$Time,
    values = 12,
    targetUnit = ospUnits$Time$min,
    sourceUnit = ospUnits$Time$h
  ))
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$simulationRunOptions, NULL)
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$simulationType, "Population")
  expect_equal(scenarioConfigurations[[scenarioNames[[1]]]]$steadyStateTime, 1000)
})

test_that("It creates all scenarios if no name is defined", {
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    projectConfiguration = projectConfiguration
  )
  expect_equal(names(scenarioConfigurations), c("TestScenario", "TestScenario2", "PopulationScenario", "PopulationScenarioFromCSV"))
})

test_that("It correctly applies a custom function", {
  scenarioNames <- "TestScenario"

  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )

  doseParamPath <- "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose"
  doseFactor <- 2
  customFunction <- function(doseFactor, doseParamPath) {
    doseParam <- getParameter(
      path = doseParamPath,
      container = simulation
    )
    doseParam$value <- doseParam$value * doseFactor
  }
  # Simulation without the custom function
  sim <- initializeScenario(scenarioConfigurations[[1]])
  # get the value of dose parameter
  oldVal <- getQuantityValuesByPath(doseParamPath, sim)

  scenarioConfigurations$TestScenario$customFunction <- customFunction
  scenarioConfigurations$TestScenario$customFunctionArgs <- list(
    doseFactor = doseFactor,
    doseParamPath = doseParamPath
  )

  sim <- initializeScenario(scenarioConfigurations[[1]])

  expect_equal(getQuantityValuesByPath(doseParamPath, sim), oldVal * doseFactor)
})

test_that("It throws an error when trying to set wrong arguments for the custom function", {
  scenarioNames <- "TestScenario"

  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )

  customFunction <- function(doseFactor) {
    doseParamPath <- "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose"
    doseParam <- getParameter(
      path = doseParamPath,
      container = simulation
    )
    doseParam$value <- doseParam$value * doseFactor
  }

  scenarioConfigurations$TestScenario$customFunction <- customFunction

  expect_error(scenarioConfigurations$TestScenario$customFunctionArgs <- list(wrong = 2), messages$errorWrongArguments("doseFactor"))
})

test_that("It throws an error when no population is specified for a population scenario", {
  scenarioNames <- c(
    "TestScenario",
    "PopulationScenario"
  )

  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )

  scenarioConfigurations$PopulationScenario$populationId <- NULL

  expect_error(runScenarios(scenarioConfigurations), messages$noPopulationIdForPopulationScenario("PopulationScenario"))
})

test_that("It throws an error when reading wrong file structure for scenario configuration", {
  expectedColumns <- c(
    "Scenario_name", "IndividualId", "PopulationId", "ReadPopulationFromCSV", "ModelParameterSheets", "ApplicationProtocol",
    "SimulationTime", "SimulationTimeUnit", "SteadyState", "SteadyStateTime", "SteadyStateTimeUnit", "ModelFile",
    "OutputPathsIds"
  )
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(
    new = "Scenarios.xlsx",
    tmpdir = tempDir,
    code = {
      scenariosDfLocal <- scenariosDf[3:12]
      writeExcel(data = list(
        "Scenarios" = scenariosDfLocal
      ), path = file.path(tempDir, "Scenarios.xlsx"), )
      expect_error(readScenarioConfigurationFromExcel(projectConfiguration = projectConfigurationLocal),
        regexp = messages$errorWrongXLSStructure(filePath = file.path(
          projectConfigurationLocal$paramsFolder,
          projectConfigurationLocal$scenarioDefinitionFile
        ), expectedColNames = expectedColumns), fixed = TRUE
      )
    }
  )
})
