# Create a project configuration
projectConfiguration <- testProjectConfiguration()

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

# Template scenario configuration for testing with new structure
templateScenarioConfiguration <- ScenarioConfiguration$new(
  projectConfiguration = projectConfiguration
)
templateScenarioConfiguration$scenarioName <- "TestScenario"
templateScenarioConfiguration$modelFile <- "Aciclovir.pkml"
templateScenarioConfiguration$individualId <- "Indiv1"
templateScenarioConfiguration$applicationProtocol <- "IV 250mg 10min"
templateScenarioConfiguration$simulationTime <- "0, 24, 60"
templateScenarioConfiguration$simulationTimeUnit <- "h"
templateScenarioConfiguration$outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

test_that("`ScenarioConfiguration` active bindings are modified", {
  # Create a fresh temporary project for this test
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

  # Test that the active bindings work correctly
  expect_equal(templateScenarioConfiguration$scenarioName, "TestScenario")
  expect_equal(templateScenarioConfiguration$modelFile, "Aciclovir.pkml")
  expect_equal(templateScenarioConfiguration$individualId, "Indiv1")
  expect_equal(
    templateScenarioConfiguration$applicationProtocol,
    "IV 250mg 10min"
  )
  expect_equal(templateScenarioConfiguration$simulationTime, list(c(0, 24, 60)))
  expect_equal(templateScenarioConfiguration$simulationTimeUnit, "h")
  expect_equal(
    templateScenarioConfiguration$outputPaths,
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  )
})

test_that("`ScenarioConfiguration` can be created from excel file", {
  # Create a fresh temporary project for this test
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

  # Test that scenario configuration can be read from excel
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = c("TestScenario"),
    projectConfiguration = projectConfiguration
  )

  expect_equal(length(scenarioConfigurations), 1)
  expect_equal(scenarioConfigurations[[1]]$scenarioName, "TestScenario")
})

test_that("Parameter sheet names with parentheses are properly parsed", {
  # Create a fresh temporary project for this test
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

  # Test that scenario configuration can be read from excel
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = c("TestScenario", "TestScenario2"),
    projectConfiguration = projectConfiguration
  )

  expect_equal(
    names(scenarioConfigurations[["TestScenario"]]$paramSheets),
    "Global"
  )
  expect_equal(
    names(scenarioConfigurations[["TestScenario2"]]$paramSheets),
    c("Global", "Aciclovir", "Sheet, with comma")
  )
})

test_that("it throws an error when wrong scenario is defined", {
  scenarioNames <- "wrong"
  expect_error(
    readScenarioConfigurationFromExcel(
      scenarioNames = scenarioNames,
      projectConfiguration = projectConfiguration
    ),
    regexp = messages$scenarioConfigurationNameNotFoundWhenReading(scenarioNames[[
      1
    ]])
  )
})

test_that("It creates the correct scenario", {
  scenarioNames <- "TestScenario"

  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )
  expect_equal(names(scenarioConfigurations), scenarioNames)
  expect_equal(
    scenarioConfigurations[[scenarioNames]]$applicationProtocol,
    "Aciclovir_iv_250mg"
  )
  expect_equal(scenarioConfigurations[[scenarioNames]]$individualId, "Indiv1")
  expect_equal(
    scenarioConfigurations[[scenarioNames]]$modelFile,
    "Aciclovir.pkml"
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames]]$paramSheets,
    enum(enumValues = "Global")
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames]]$scenarioName,
    "TestScenario"
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames]]$simulateSteadyState,
    FALSE
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames]]$simulationTime,
    list(c(0, 24, 60))
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames]]$simulationTimeUnit,
    ospUnits$Time$h
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames]]$simulationRunOptions,
    NULL
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames]]$simulationType,
    "Individual"
  )
  expect_equal(scenarioConfigurations[[scenarioNames]]$steadyStateTime, 1000)
})

test_that("It creates multiple correct scenarios", {
  scenarioNames <- c("TestScenario", "TestScenario2")

  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )
  expect_equal(names(scenarioConfigurations), scenarioNames)
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$applicationProtocol,
    "Aciclovir_iv_250mg"
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$individualId,
    "Indiv1"
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$modelFile,
    "Aciclovir.pkml"
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$paramSheets,
    enum(enumValues = "Global")
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$scenarioName,
    "TestScenario"
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$simulateSteadyState,
    FALSE
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$simulationTime,
    list(c(0, 24, 60))
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$simulationTimeUnit,
    ospUnits$Time$h
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$simulationRunOptions,
    NULL
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$simulationType,
    "Individual"
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$steadyStateTime,
    1000
  )

  # Second scenario
  expect_equal(
    scenarioConfigurations[[scenarioNames[[2]]]]$applicationProtocol,
    "Aciclovir_iv_250mg"
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[2]]]]$individualId,
    "Indiv1"
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[2]]]]$modelFile,
    "Aciclovir.pkml"
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[2]]]]$paramSheets,
    enum(enumValues = c("Global", "Aciclovir", "Sheet, with comma"))
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[2]]]]$scenarioName,
    "TestScenario2"
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[2]]]]$simulateSteadyState,
    TRUE
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[2]]]]$simulationTime,
    list(c(0, 1, 60), c(1, 12, 20))
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[2]]]]$simulationTimeUnit,
    ospUnits$Time$h
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[2]]]]$simulationRunOptions,
    NULL
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[2]]]]$simulationType,
    "Individual"
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[2]]]]$steadyStateTime,
    500
  )
})

test_that("It does not fail on empty rows", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$configurationsFolder <- tempDir
  withr::with_tempfile(
    new = "Scenarios.xlsx",
    tmpdir = tempDir,
    code = {
      scenariosDfLocal <- as.data.frame(lapply(scenariosDf, rep, 2))
      scenariosDfLocal[3, ] <- scenariosDfLocal[2, ]
      scenariosDfLocal[2, ] <- scenariosDfLocal[4, ]
      scenariosDfLocal[3, ]$Scenario_name <- "TestScenario2"
      .writeExcel(
        data = list(
          "Scenarios" = scenariosDfLocal,
          "OutputPaths" = data.frame()
        ),
        path = file.path(tempDir, "Scenarios.xlsx"),
      )

      scenarioConfigs <- readScenarioConfigurationFromExcel(
        projectConfiguration = projectConfigurationLocal
      )
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
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$applicationProtocol,
    "Aciclovir_iv_250mg"
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$individualId,
    "Indiv1"
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$populationId,
    "TestPopulation"
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$modelFile,
    "Aciclovir.pkml"
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$paramSheets,
    enum(enumValues = "Global")
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$scenarioName,
    "PopulationScenario"
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$simulateSteadyState,
    FALSE
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$simulationTime,
    list(c(0, 12, 20))
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$simulationTimeUnit,
    ospUnits$Time$h
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$simulationRunOptions,
    NULL
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$simulationType,
    "Population"
  )
  expect_equal(
    scenarioConfigurations[[scenarioNames[[1]]]]$steadyStateTime,
    1000
  )
})

test_that("It creates all scenarios if no name is defined", {
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    projectConfiguration = projectConfiguration
  )
  expect_equal(
    names(scenarioConfigurations),
    c(
      "TestScenario",
      "TestScenario2",
      "PopulationScenario",
      "PopulationScenarioFromCSV",
      "TestScenario_missingParam"
    )
  )
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

  expect_error(
    createScenarios(scenarioConfigurations = scenarioConfigurations),
    regexp = messages$noPopulationIdForPopulationScenario("PopulationScenario"),
    fixed = TRUE
  )
})

test_that("It throws an error when reading wrong file structure for scenario configuration", {
  expectedColumns <- c(
    "Scenario_name",
    "IndividualId",
    "PopulationId",
    "ReadPopulationFromCSV",
    "ModelParameterSheets",
    "ApplicationProtocol",
    "SimulationTime",
    "SimulationTimeUnit",
    "SteadyState",
    "SteadyStateTime",
    "SteadyStateTimeUnit",
    "ModelFile",
    "OutputPathsIds"
  )
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$configurationsFolder <- tempDir
  withr::with_tempfile(
    new = "Scenarios.xlsx",
    tmpdir = tempDir,
    code = {
      scenariosDfLocal <- scenariosDf[3:12]
      .writeExcel(
        data = list(
          "Scenarios" = scenariosDfLocal
        ),
        path = file.path(tempDir, "Scenarios.xlsx"),
      )
      expect_error(
        readScenarioConfigurationFromExcel(
          projectConfiguration = projectConfigurationLocal
        ),
        regexp = messages$errorWrongXLSStructure(
          filePath = projectConfigurationLocal$scenariosFile,
          expectedColNames = expectedColumns
        ),
        fixed = TRUE
      )
    }
  )
})


# Test .parseSimulationTimeIntervals()
test_that("It parses time intervals correctly", {
  timeIntervalStringNull <- NULL
  timeIntervalStringValidOneInterval <- "0, 1, 1"
  timeIntervalStringValidTwoIntervals <- "0, 1, 1; 1, 2, 1"
  timeIntervalStringStartAfterEnd <- "2, 1, 1"
  timeIntervalStringResolutionZero <- "1, 2, 0"
  timeIntervalStringInvaldiNonNumeric <- "1, 2, a"
  timeIntervalStringNegative <- "-1, 1, 1"
  timeIntervalStringInvalid <- "0, 1"
  timeIntervalStringInvalidMultiple <- "0; 1; 1, 1; 2; 1"

  expect_equal(
    .parseSimulationTimeIntervals(timeIntervalStringNull),
    NULL
  )

  expect_equal(
    .parseSimulationTimeIntervals(timeIntervalStringValidOneInterval),
    list(c(0, 1, 1))
  )

  expect_equal(
    .parseSimulationTimeIntervals(timeIntervalStringValidTwoIntervals),
    list(
      c(0, 1, 1),
      c(1, 2, 1)
    )
  )

  expect_error(
    .parseSimulationTimeIntervals(timeIntervalStringStartAfterEnd),
    regexp = messages$stopWrongTimeIntervalString(
      timeIntervalStringStartAfterEnd
    ),
    fixed = TRUE
  )

  expect_error(
    .parseSimulationTimeIntervals(timeIntervalStringResolutionZero),
    messages$stopWrongTimeIntervalString(timeIntervalStringResolutionZero),
    fixed = TRUE
  )

  expect_error(
    .parseSimulationTimeIntervals(timeIntervalStringNegative),
    messages$stopWrongTimeIntervalString(timeIntervalStringNegative),
    fixed = TRUE
  )

  expect_error(
    .parseSimulationTimeIntervals(timeIntervalStringInvalidMultiple),
    messages$stopWrongTimeIntervalString(timeIntervalStringInvalidMultiple),
    fixed = TRUE
  )

  expect_error(
    .parseSimulationTimeIntervals(timeIntervalStringInvalid),
    messages$stopWrongTimeIntervalString(timeIntervalStringInvalid),
    fixed = TRUE
  )
})

test_that("It shows a meaningful error when steady-state is true but no unit is set", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$configurationsFolder <- tempDir
  withr::with_tempfile(
    new = "Scenarios.xlsx",
    tmpdir = tempDir,
    code = {
      scenariosDfLocal <- scenariosDf
      scenariosDfLocal$SteadyState <- TRUE
      scenariosDfLocal$SteadyStateTime <- 15
      .writeExcel(
        data = list(
          "Scenarios" = scenariosDfLocal,
          "OutputPaths" = data.frame()
        ),
        path = file.path(tempDir, "Scenarios.xlsx"),
      )

      expect_error(
        readScenarioConfigurationFromExcel(
          projectConfiguration = projectConfigurationLocal
        ),
        regexp = messages$missingSteadyStateTimeUnit("TestScenario"),
        fixed = TRUE
      )
    }
  )
})

# Tests for Excel sheet name sanitization
test_that(".sanitizeExcelSheetName handles valid names correctly", {
  expect_equal(.sanitizeExcelSheetName("ValidName", warn = FALSE), "ValidName")
  expect_equal(
    .sanitizeExcelSheetName("Valid_Name_123", warn = FALSE),
    "Valid_Name_123"
  )
  expect_equal(
    .sanitizeExcelSheetName("Name with spaces", warn = FALSE),
    "Name with spaces"
  )
})

test_that(".sanitizeExcelSheetName removes invalid characters", {
  expect_equal(.sanitizeExcelSheetName("Test/Name", warn = FALSE), "Test_Name")
  expect_equal(.sanitizeExcelSheetName("Test\\Name", warn = FALSE), "Test_Name")
  expect_equal(.sanitizeExcelSheetName("Test*Name", warn = FALSE), "Test_Name")
  expect_equal(
    .sanitizeExcelSheetName("Test[Name]", warn = FALSE),
    "Test_Name_"
  )
  expect_equal(
    .sanitizeExcelSheetName("Test:Name?", warn = FALSE),
    "Test_Name_"
  )
  expect_equal(
    .sanitizeExcelSheetName("Test/Name*With[All]:Invalid?", warn = FALSE),
    "Test_Name_With_All__Invalid_"
  )
})

test_that(".sanitizeExcelSheetName truncates long names", {
  longName <- "This_is_a_very_long_sheet_name_that_exceeds_31_characters_limit"
  result <- .sanitizeExcelSheetName(longName, warn = FALSE)
  expect_equal(nchar(result), 31)
  expect_equal(result, "This_is_a_very_long_sheet_name_")
})

test_that(".sanitizeExcelSheetName handles edge cases", {
  expect_equal(.sanitizeExcelSheetName("", warn = FALSE), "Sheet")
  expect_equal(.sanitizeExcelSheetName("   ", warn = FALSE), "Sheet")
  expect_equal(.sanitizeExcelSheetName(NULL, warn = FALSE), "Sheet")
  expect_equal(.sanitizeExcelSheetName(NA, warn = FALSE), "Sheet")
})

test_that(".sanitizeExcelSheetName trims whitespace", {
  expect_equal(
    .sanitizeExcelSheetName("  ValidName  ", warn = FALSE),
    "ValidName"
  )
  expect_equal(
    .sanitizeExcelSheetName("  Test Name  ", warn = FALSE),
    "Test Name"
  )
})

test_that(".sanitizeExcelSheetName warns when names are modified", {
  expect_warning(
    .sanitizeExcelSheetName("Test/Invalid", warn = TRUE),
    "Excel sheet name was sanitized"
  )
  expect_warning(
    .sanitizeExcelSheetName(
      "This_is_a_very_long_sheet_name_that_exceeds_31_characters",
      warn = TRUE
    ),
    "Excel sheet name was sanitized"
  )
  expect_silent(.sanitizeExcelSheetName("ValidName", warn = TRUE))
})

test_that(".sanitizeExcelSheetName handles combined issues", {
  complexName <- "  Invalid/Name*With[Special]:Characters?And_Very_Long_Name_That_Exceeds_Limit  "
  result <- .sanitizeExcelSheetName(complexName, warn = FALSE)
  expect_equal(nchar(result), 31)
  expect_true(!grepl("[/\\\\*\\[\\]:?]", result))
  expect_equal(result, "Invalid_Name_With_Special__Char")
})
