# Create a project configuration
projectConfiguration <- testProjectConfiguration()

# Helper function to create valid PI configuration sheets
createValidPISheets <- function() {
  list(
    PIConfiguration = data.frame(
      PITaskName = "Task1",
      Algorithm = "BOBYQA",
      CIMethod = "hessian",
      PrintEvaluationFeedback = TRUE,
      AutoEstimateCI = FALSE,
      SimulationRunOptions = NA,
      ObjectiveFunctionOptions = NA
    ),
    PIParameters = data.frame(
      PITaskName = "Task1",
      Scenario = "PITestScenario",
      `Container Path` = "Aciclovir",
      `Parameter Name` = "Lipophilicity",
      Value = -0.1,
      Units = "Log Units",
      MinValue = -2,
      MaxValue = 2,
      StartValue = -0.1,
      Group = NA,
      check.names = FALSE
    ),
    PIOutputMappings = data.frame(
      PITaskName = "Task1",
      Scenario = "PITestScenario",
      ObservedDataSheet = "ObservedData",
      DataSet = NA,
      Scaling = "log",
      xOffset = NA,
      yOffset = NA,
      Weight = NA
    ),
    AlgorithmOptions = data.frame(
      PITaskName = character(0),
      OptionName = character(0),
      OptionValue = character(0)
    ),
    CIOptions = data.frame(
      PITaskName = character(0),
      OptionName = character(0),
      OptionValue = character(0)
    )
  )
}

test_that("readPITaskConfigurationFromExcel creates PITaskConfiguration from excel file", {
  piTaskName <- "AciclovirLinear"
  expect_no_error(
    piTaskConfigurations <- readPITaskConfigurationFromExcel(
      piTaskNames = piTaskName,
      projectConfiguration = projectConfiguration
    )
  )

  piTaskConfiguration <- piTaskConfigurations[[1]]
  expect_true(is.list(piTaskConfigurations))
  expect_equal(names(piTaskConfigurations), piTaskName)
  expect_r6_class(piTaskConfiguration, "PITaskConfiguration")
  expect_equal(piTaskConfiguration$piTaskName, piTaskName)
})

# ERROR readPITaskConfigurationFromExcel(projectConfigurationLocal)

test_that("readPITaskConfigurationFromExcel creates all PI tasks if no name is defined", {
  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfiguration
  )
    
  piTaskNames <- c("AciclovirLinear", "AciclovirLog")
  expect_equal(names(piTaskConfigurations), piTaskNames)
})

test_that("readPITaskConfigurationFromExcel throws an error when wrong PI task name is defined", {
  piTaskName <- "WrongTaskName"
  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfiguration
  )

  expect_error(
    readPITaskConfigurationFromExcel(
      piTaskNames = piTaskName,
      projectConfiguration = projectConfiguration
    ),
    regexp = messages$errorPINotFound(
      "task",
      piTaskName,
      names(piTaskConfigurations)
    ),
    fixed = TRUE
  )
})

test_that("readPITaskConfigurationFromExcel creates correct PI task configuration", {
  piTaskName <- "AciclovirLinear"
  
  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    piTaskNames = piTaskName,
    projectConfiguration = projectConfiguration
  )
  piTaskConfiguration <- piTaskConfigurations[[piTaskName]]

  expect_equal(piTaskConfiguration$piTaskName, piTaskName)
  expect_equal(piTaskConfiguration$scenarioName, "PITestScenario")
  expect_equal(piTaskConfiguration$modelFile, "Aciclovir.pkml")

  expect_snapshot_value(piTaskConfiguration$piConfiguration, style = "deparse")
  expect_snapshot_value(piTaskConfiguration$piParameters, style = "deparse")
  expect_snapshot_value(piTaskConfiguration$piOutputMappings, style = "deparse")
})



test_that("readPITaskConfigurationFromExcel does not fail on empty rows in sheets", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  # Create test data with empty rows
  piConfigDf <- data.frame(
    PITaskName = c("Task1", NA, "Task2"),
    Algorithm = c("BOBYQA", NA, "DEoptim"),
    CIMethod = c("hessian", NA, "PL"),
    PrintEvaluationFeedback = c(TRUE, NA, FALSE),
    AutoEstimateCI = c(FALSE, NA, TRUE),
    SimulationRunOptions = c(NA, NA, NA),
    ObjectiveFunctionOptions = c(NA, NA, NA)
  )

  piParamsDf <- data.frame(
    PITaskName = c("Task1", NA, "Task2"),
    Scenario = c("PITestScenario", NA, "PITestScenario"),
    `Container Path` = c("Aciclovir", NA, "Aciclovir"),
    `Parameter Name` = c("Lipophilicity", NA, "Lipophilicity"),
    Value = c(-0.1, NA, -0.2),
    Units = c("Log Units", NA, "Log Units"),
    MinValue = c(-2, NA, -3),
    MaxValue = c(2, NA, 1),
    StartValue = c(-0.1, NA, -0.2),
    Group = c(NA, NA, NA),
    check.names = FALSE
  )

  piOutputDf <- data.frame(
    PITaskName = c("Task1", NA, "Task2"),
    Scenario = c("PITestScenario", NA, "PITestScenario"),
    ObservedDataSheet = c("ObservedData", NA, "ObservedData"),
    DataSet = c(NA, NA, NA),
    Scaling = c("log", NA, "lin"),
    xOffset = c(NA, NA, NA),
    yOffset = c(NA, NA, NA),
    Weight = c(NA, NA, NA)
  )

  algOptsDf <- data.frame(
    PITaskName = character(0),
    OptionName = character(0),
    OptionValue = character(0)
  )

  ciOptsDf <- data.frame(
    PITaskName = character(0),
    OptionName = character(0),
    OptionValue = character(0)
  )

  .writeExcel(
    data = list(
      "PIConfiguration" = piConfigDf,
      "PIParameters" = piParamsDf,
      "PIOutputMappings" = piOutputDf,
      "AlgorithmOptions" = algOptsDf,
      "CIOptions" = ciOptsDf
    ),
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  piConfigs <- readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfigurationLocal
  )
  expect_equal(names(piConfigs), c("Task1", "Task2"))
})

test_that("readPITaskConfigurationFromExcel validates PIConfiguration sheet structure", {
  testSheet <- "Sheet: PIConfiguration"
  expectedColumns <- c(
    "PITaskName",
    "Algorithm",
    "CIMethod",
    "PrintEvaluationFeedback",
    "AutoEstimateCI",
    "SimulationRunOptions",
    "ObjectiveFunctionOptions"
  )

  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()
  sheets$PIConfiguration <- data.frame(
    PITaskName = "Task1",
    WrongColumn = "BOBYQA"
  )

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  expect_error(
    readPITaskConfigurationFromExcel(
      projectConfiguration = projectConfigurationLocal
    ),
    regexp = messages$errorWrongXLSStructure(
      filePath = projectConfigurationLocal$parameterIdentificationFile,
      expectedColNames = expectedColumns,
      optionalMessage = testSheet
    ),
    fixed = TRUE
  ) 
})

test_that("readPITaskConfigurationFromExcel validates PIParameters sheet structure", {
  testSheet <- "Sheet: PIParameters"
  expectedColumns <- c(
    "PITaskName",
    "Scenario",
    "Container Path",
    "Parameter Name",
    "Value",
    "Units",
    "MinValue",
    "MaxValue",
    "StartValue",
    "Group"
  )

  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()
  sheets$PIParameters <- data.frame(
    PITaskName = "Task1",
    WrongColumn = "Value"
  )

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  expect_error(
    readPITaskConfigurationFromExcel(
      projectConfiguration = projectConfigurationLocal
    ),
    regexp = messages$errorWrongXLSStructure(
      filePath = projectConfigurationLocal$parameterIdentificationFile,
      expectedColNames = expectedColumns,
      optionalMessage = testSheet
    ),
    fixed = TRUE
  )
})

test_that("readPITaskConfigurationFromExcel validates PIOutputMappings sheet structure", {
  testSheet <- "Sheet: PIOutputMappings"
  expectedColumns <- c(
    "PITaskName",
    "Scenario",
    "ObservedDataSheet",
    "DataSet",
    "Scaling",
    "xOffset",
    "yOffset",
    "Weight"
  )

  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()
  sheets$PIOutputMappings <- data.frame(
    PITaskName = "Task1",
    WrongColumn = "log"
  )

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  expect_error(
    readPITaskConfigurationFromExcel(
      projectConfiguration = projectConfigurationLocal
    ),
    regexp = messages$errorWrongXLSStructure(
      filePath = projectConfigurationLocal$parameterIdentificationFile,
      expectedColNames = expectedColumns,
      optionalMessage = testSheet
    ),
    fixed = TRUE
  )
})

test_that("readPITaskConfigurationFromExcel validates AlgorithmOptions sheet structure", {
  testSheet <- "Sheet: AlgorithmOptions"
  expectedColumns <- c("PITaskName", "OptionName", "OptionValue")

  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()
  # Replace AlgorithmOptions with wrong structure
  sheets$AlgorithmOptions <- data.frame(
    PITaskName = "Task1",
    WrongColumn = "100"
  )

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  expect_error(
    readPITaskConfigurationFromExcel(
      projectConfiguration = projectConfigurationLocal
    ),
    regexp = messages$errorWrongXLSStructure(
      filePath = projectConfigurationLocal$parameterIdentificationFile,
      expectedColNames = expectedColumns,
      optionalMessage = testSheet
    ),
    fixed = TRUE
  )
})

test_that("readPITaskConfigurationFromExcel validates CIOptions sheet structure", {
  testSheet <- "Sheet: CIOptions"
  expectedColumns <- c("PITaskName", "OptionName", "OptionValue")

  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()
  sheets$CIOptions <- data.frame(
    PITaskName = "Task1",
    WrongColumn = "bootstrap"
  )

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  expect_error(
    readPITaskConfigurationFromExcel(
      projectConfiguration = projectConfigurationLocal
    ),
    regexp = messages$errorWrongXLSStructure(
      filePath = projectConfigurationLocal$parameterIdentificationFile,
      expectedColNames = expectedColumns,
      optionalMessage = testSheet
    ),
    fixed = TRUE
  )
})

test_that("readPITaskConfigurationFromExcel throws an error when missing required sheets", {
  testMissingSheet <- "PIConfiguration"
  expectedSheets <- c(
    "PIConfiguration",
    "PIParameters",
    "PIOutputMappings",
    "AlgorithmOptions",
    "CIOptions"
  )
  
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()
  sheets[testMissingSheet] <- NULL

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  expect_error(
    readPITaskConfigurationFromExcel(
      projectConfiguration = projectConfigurationLocal
    ),
    regexp = messages$messagePISheet(
      "missingSheets",
      testMissingSheet,
      filePath = projectConfigurationLocal$parameterIdentificationFile
    ),
    fixed = TRUE
  )  
})

test_that("readPITaskConfigurationFromExcel validates that referenced scenarios exist", {
  scenarioName <- "NonExistentScenario"
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    NULL,
    projectConfigurationLocal
  )

  sheets <- createValidPISheets()
  sheets$PIParameters$Scenario <- scenarioName
  sheets$PIOutputMappings$Scenario <- scenarioName

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  expect_error(
    readPITaskConfigurationFromExcel(
      projectConfiguration = projectConfigurationLocal
    ),
    regexp = messages$errorPINotFound(
      "scenario",
      scenarioName,
      names(scenarioConfigurations)
    ),
    fixed = TRUE
  )
})

test_that("readPITaskConfigurationFromExcel handles multiple parameter rows correctly", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()
  # Add second parameter row for same task
  sheets$PIParameters <- rbind(
    sheets$PIParameters,
    data.frame(
      PITaskName = "Task1",
      Scenarios = "PITestScenario",
      `Container Path` = "Neighborhoods|Kidney",
      `Parameter Name` = "TSspec",
      Value = 0.5,
      Units = "1/min",
      MinValue = 0,
      MaxValue = 10,
      StartValue = 0.5,
      Group = "1",
      check.names = FALSE
    )
  )

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfigurationLocal
  )

  params <- piTaskConfigurations$Task1$piParameters

  # Check that we have a list of 2 parameter definitions
  expect_equal(length(params), 2)
  expect_true(is.list(params[[1]]))
  expect_true(is.list(params[[2]]))

  # Check first parameter
  expect_equal(params[[1]]$`Container Path`, "Aciclovir")
  expect_equal(params[[1]]$`Parameter Name`, "Lipophilicity")
  expect_equal(params[[1]]$MinValue, -2)
  expect_equal(params[[1]]$MaxValue, 2)

  # Check second parameter
  expect_equal(params[[2]]$`Container Path`, "Neighborhoods|Kidney")
  expect_equal(params[[2]]$`Parameter Name`, "TSspec")
  expect_equal(params[[2]]$MinValue, 0)
  expect_equal(params[[2]]$MaxValue, 10)
})
