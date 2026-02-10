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
      Scenarios = "PITestScenario",
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
      Scenarios = "PITestScenario",
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
    )
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

  # Start with valid sheets and add Task2 with NA row in between
  sheets <- createValidPISheets()

  # Create Task2 rows (different from Task1)
  task2Config <- sheets$PIConfiguration
  task2Config$PITaskName <- "Task2"
  task2Config$Algorithm <- "DEoptim"
  task2Config$CIMethod <- "PL"
  task2Config$PrintEvaluationFeedback <- FALSE
  task2Config$AutoEstimateCI <- TRUE

  task2Params <- sheets$PIParameters
  task2Params$PITaskName <- "Task2"
  task2Params$Value <- -0.2
  task2Params$MinValue <- -3
  task2Params$MaxValue <- 1
  task2Params$StartValue <- -0.2

  task2Output <- sheets$PIOutputMappings
  task2Output$PITaskName <- "Task2"
  task2Output$ObservedDataSheet <- "ObservedData"
  task2Output$DataSet <- NA
  task2Output$Scaling <- "lin"

  # Add NA rows between Task1 and Task2
  sheets$PIConfiguration <- rbind(sheets$PIConfiguration, NA, task2Config)
  sheets$PIParameters <- rbind(sheets$PIParameters, NA, task2Params)
  sheets$PIOutputMappings <- rbind(sheets$PIOutputMappings, NA, task2Output)

  .writeExcel(
    data = sheets,
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
    "Scenarios",
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
    "Scenarios",
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
    )
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
  sheets$PIParameters$Scenarios <- scenarioName
  sheets$PIOutputMappings$Scenarios <- scenarioName

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  expect_error(
    readPITaskConfigurationFromExcel(
      projectConfiguration = projectConfigurationLocal
    )
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
