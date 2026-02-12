projectConfiguration <- testProjectConfiguration()

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
  piTaskName <- "AciclovirSimple"
  expect_no_error(
    piTaskConfigurations <- readPITaskConfigurationFromExcel(
      piTaskNames = piTaskName,
      projectConfiguration = projectConfiguration
    )
  )

  piTaskConfiguration <- piTaskConfigurations[[1]]
  expect_true(is.list(piTaskConfigurations))
  expect_equal(names(piTaskConfigurations), piTaskName)
  expect_true(isOfType(piTaskConfiguration, "PITaskConfiguration"))
  expect_equal(piTaskConfiguration$taskName, piTaskName)
})

test_that("readPITaskConfigurationFromExcel creates correct PITaskConfiguration", {
  piTaskName <- "AciclovirSimple"
  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    piTaskNames = piTaskName,
    projectConfiguration = projectConfiguration
  )

  expect_true(is.list(piTaskConfigurations))
  expect_equal(names(piTaskConfigurations), piTaskName)

  piTaskConfiguration <- piTaskConfigurations[[piTaskName]]
  expect_true(isOfType(piTaskConfiguration, "PITaskConfiguration"))
  expect_equal(piTaskConfiguration$taskName, piTaskName)

  expect_true(is.list(piTaskConfiguration$scenarioConfiguration))
  expect_equal(
    names(piTaskConfiguration$scenarioConfiguration),
    "PITestScenario"
  )
  expect_equal(
    piTaskConfiguration$scenarioConfiguration[[1]]$scenarioName,
    "PITestScenario"
  )
  expect_equal(
    piTaskConfiguration$scenarioConfiguration[[1]]$modelFile,
    "Aciclovir.pkml"
  )
})

test_that("readPITaskConfigurationFromExcel creates all PI tasks if no name is defined", {
  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfiguration
  )

  piTaskNames <- c("AciclovirSimple", "AciclovirMultiScenario")
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

test_that("readPITaskConfigurationFromExcel does not fail on empty rows in sheets", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()

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

  expect_equal(length(params), 2)
  expect_true(is.list(params[[1]]))
  expect_true(is.list(params[[2]]))

  expect_equal(params[[1]]$`Container Path`, "Aciclovir")
  expect_equal(params[[1]]$`Parameter Name`, "Lipophilicity")
  expect_equal(params[[1]]$MinValue, -2)
  expect_equal(params[[1]]$MaxValue, 2)

  expect_equal(params[[2]]$`Container Path`, "Neighborhoods|Kidney")
  expect_equal(params[[2]]$`Parameter Name`, "TSspec")
  expect_equal(params[[2]]$MinValue, 0)
  expect_equal(params[[2]]$MaxValue, 10)
})

test_that("readPITaskConfigurationFromExcel throws error when task is missing in one of the sheets", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()

  sheets$PIConfiguration <- rbind(
    sheets$PIConfiguration,
    data.frame(
      PITaskName = "Task2",
      Algorithm = "BOBYQA",
      CIMethod = "hessian",
      PrintEvaluationFeedback = TRUE,
      AutoEstimateCI = FALSE,
      SimulationRunOptions = NA,
      ObjectiveFunctionOptions = NA
    )
  )

  sheets$PIParameters <- rbind(
    sheets$PIParameters,
    data.frame(
      PITaskName = "Task2",
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
    )
  )

  .writeExcel(data = sheets, path = projectConfigurationLocal$parameterIdentificationFile)

  expect_error(
    readPITaskConfigurationFromExcel(
      projectConfiguration = projectConfigurationLocal
    ),
    messages$errorPITaskMissingInSheet("Task2", "PIOutputMappings"),
    fixed = TRUE
  )
})

test_that("readPITaskConfigurationFromExcel handles NA values correctly in optional fields", {
  piTaskName <- "AciclovirSimple"
  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    piTaskNames = piTaskName,
    projectConfiguration = projectConfiguration
  )

  outputMapping <- piTaskConfigurations[[piTaskName]]$piOutputMappings
  piConfiguration <- piTaskConfigurations[[piTaskName]]$piConfiguration

  expect_true(is.na(outputMapping[[1]]$xOffset))
  expect_true(is.na(outputMapping[[1]]$Weight))
  expect_true(is.na(piConfiguration$SimulationRunOptions))
  expect_true(is.na(piConfiguration$ObjectiveFunctionOptions))
})

test_that("readPITaskConfigurationFromExcel reads sheets with correct types", {
  piTaskName <- "AciclovirSimple"
  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    piTaskNames = piTaskName,
    projectConfiguration = projectConfiguration
  )

  piParameter <- piTaskConfigurations[[piTaskName]]$piParameters[[1]]
  outputMapping <- piTaskConfigurations[[piTaskName]]$piOutputMappings[[1]]
  piConfiguration <- piTaskConfigurations[[piTaskName]]$piConfiguration
  algorithmOptions <- piTaskConfigurations[[
    piTaskName
  ]]$piConfiguration$algorithmOptions
  ciOptions <- piTaskConfigurations[[piTaskName]]$piConfiguration$ciOptions

  sheetColumnTypes <- list(
    piParameters = lapply(piParameter, class),
    piOutputMappings = lapply(outputMapping, class),
    piConfiguration = lapply(piConfiguration, class),
    algortihmOptions = lapply(algorithmOptions, class),
    ciOptions = lapply(ciOptions, class)
  )

  expect_snapshot(sheetColumnTypes)
})

test_that("readPITaskConfigurationFromExcel converts option values from strings to numeric", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()
  sheets$AlgorithmOptions <- data.frame(
    PITaskName = "Task1",
    OptionName = c("maxeval", "ftol_rel"),
    OptionValue = c("1000", "0.001")
  )
  sheets$CIOptions <- data.frame(
    PITaskName = "Task1",
    OptionName = c("alpha", "max_iter"),
    OptionValue = c("0.05", "100")
  )

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfigurationLocal
  )

  algOptions <- piTaskConfigurations[[1]]$piConfiguration$algorithmOptions
  expect_true(is.numeric(algOptions$maxeval))
  expect_equal(algOptions$maxeval, 1000)
  expect_true(is.numeric(algOptions$ftol_rel))
  expect_equal(algOptions$ftol_rel, 0.001)

  ciOpts <- piTaskConfigurations[[1]]$piConfiguration$ciOptions
  expect_true(is.numeric(ciOpts$alpha))
  expect_equal(ciOpts$alpha, 0.05)
  expect_true(is.numeric(ciOpts$max_iter))
  expect_equal(ciOpts$max_iter, 100)
})
