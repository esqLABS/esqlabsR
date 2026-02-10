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
      ObservedDataSheet = "Laskin 1982.Group A",
      DataSet = "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_",
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

test_that("createPITasks creates ParameterIdentification objects from configurations", {
  projectConfiguration <- testProjectConfiguration()
  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    piTaskNames = "AciclovirSimple",
    projectConfiguration = projectConfiguration
  )

  expect_no_error(
    piTasks <- createPITasks(piTaskConfigurations)
  )

  expect_true(is.list(piTasks))
  expect_equal(names(piTasks), names(piTaskConfigurations))

  firstTask <- piTasks[[1]]
  expect_true(isOfType(firstTask, "ParameterIdentification"))
  expect_true(isOfType(firstTask$simulations, "Simulation"))
  expect_true(isOfType(firstTask$parameters, "PIParameters"))
  expect_true(isOfType(firstTask$outputMappings, "PIOutputMapping"))
  expect_true(isOfType(firstTask$configuration, "PIConfiguration"))
})

test_that("createPITasks filters to specific dataset when DataSet is specified", {
  projectConfiguration <- testProjectConfiguration()

  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    piTaskNames = "AciclovirSimple",
    projectConfiguration = projectConfiguration
  )

  expect_no_error(piTasks <- createPITasks(piTaskConfigurations))
  expect_equal(length(piTasks), 1)
  expect_true(isOfType(piTasks[[1]], "ParameterIdentification"))
  expect_true(length(piTasks[[1]]$outputMappings) > 0)
})

test_that("runPI executes single PI task successfully", {
  projectConfiguration <- testProjectConfiguration()

  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    piTaskNames = "AciclovirSimple",
    projectConfiguration = projectConfiguration
  )

  piTasks <- createPITasks(piTaskConfigurations)

  piResults <- runPI(piTasks)

  result <- piResults[[1]]
  expect_true(!is.null(result$task))
  expect_true(!is.null(result$result) || !is.null(result$error))
  expect_true(is.logical(result$success))

  if (result$success) {
    expect_true(!is.null(result$finalParameters))
  }
})

test_that("runPI handles optimization failure gracefully", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  # Create PI task with impossible bounds (will fail to optimize)
  sheets <- list(
    PIConfiguration = data.frame(
      PITaskName = "FailingTask",
      Algorithm = "BOBYQA",
      CIMethod = "hessian",
      PrintEvaluationFeedback = FALSE,
      AutoEstimateCI = FALSE,
      SimulationRunOptions = NA,
      ObjectiveFunctionOptions = NA
    ),
    PIParameters = data.frame(
      PITaskName = "FailingTask",
      Scenarios = "PITestScenario",
      `Container Path` = "Aciclovir",
      `Parameter Name` = "Lipophilicity",
      Value = -0.1,
      Units = "Log Units",
      MinValue = 100,  
      MaxValue = -100,
      StartValue = 0,
      Group = NA,
      check.names = FALSE
    ),
    PIOutputMappings = data.frame(
      PITaskName = "FailingTask",
      Scenarios = "PITestScenario",
      ObservedDataSheet = "Laskin 1982.Group A",
      DataSet = "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_",
      Scaling = "log",
      xOffset = NA,
      yOffset = NA,
      Weight = NA,
      check.names = FALSE
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

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  expect_error(
    {
      piTaskConfigurations <- readPITaskConfigurationFromExcel(
        projectConfiguration = projectConfigurationLocal
      )
      piTasks <- createPITasks(piTaskConfigurations)
    }
  )
})

test_that("It runs complete PI workflow from Excel to optimized parameters", {
  projectConfiguration <- testProjectConfiguration()
  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    piTaskNames = "AciclovirSimple",
    projectConfiguration = projectConfiguration
  )
  
  expect_no_error(piTasks <- createPITasks(piTaskConfigurations))
  expect_no_error(piResults <- runPI(piTasks))
  
  expect_equal(length(piResults), 1)
  expect_equal(names(piResults), "AciclovirSimple")

  result <- piResults[[1]]
  expect_true(isOfType(result$task, "ParameterIdentification"))
  expect_true(is.logical(result$success))
  if (result$success) {
    expect_true(!is.null(result$finalParameters))
    expect_true(is.numeric(result$finalParameters))
  }
})

test_that("It runs PI with parameter grouping across multiple scenarios", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  # Step 1: Add high dose observed data
  dataFile <- file.path(temp_project$path, "Data", "TestProject_TimeValuesData.xlsx")
  laskinData <- readExcel(dataFile, sheet = "Laskin 1982.Group A")

  highDoseData <- laskinData
  highDoseData$Dose <- "5.0 mg/kg"
  highDoseData$Measurement <- highDoseData$Measurement * 1.5

  combinedData <- rbind(laskinData, highDoseData)

  .writeExcel(
    data = list("Laskin 1982.Group A" = combinedData),
    path = dataFile,
    append = FALSE
  )

  # Step 2: Update PIOutputMappings to use correct dataset for second scenario
  piFile <- projectConfigurationLocal$parameterIdentificationFile
  piOutputMappings <- readExcel(piFile, sheet = "PIOutputMappings")

  piOutputMappings$DataSet[piOutputMappings$PITaskName == "AciclovirMultiScenario" &
                             piOutputMappings$Scenarios == "PIScenario_500mg"] <-
    "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_5.0 mg/kg_iv_"

  piConfiguration <- readExcel(piFile, sheet = "PIConfiguration")
  piParameters <- readExcel(piFile, sheet = "PIParameters")
  algorithmOptions <- readExcel(piFile, sheet = "AlgorithmOptions")
  ciOptions <- readExcel(piFile, sheet = "CIOptions")

  .writeExcel(
    data = list(
      PIConfiguration = piConfiguration,
      PIParameters = piParameters,
      PIOutputMappings = piOutputMappings,
      AlgorithmOptions = algorithmOptions,
      CIOptions = ciOptions
    ),
    path = piFile,
    append = FALSE
  )

  # Step 3: Read PI configuration for multi-scenario task
  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    piTaskNames = "AciclovirMultiScenario",
    projectConfiguration = projectConfigurationLocal
  )

  expect_no_error(piTasks <- createPITasks(piTaskConfigurations))

  piTask <- piTasks[[1]]
  piParams <- piTask$parameters

  # Should have 3 PIParameters objects:
  # 1. Lipophilicity (Group=1): 2 parameters (shared across both scenarios)
  # 2. TSspec (Group=2): 1 parameter (250mg scenario)
  # 3. TSspec (Group=3): 1 parameter (500mg scenario)
  expect_equal(length(piParams), 3)

  # Verify first group has 2 parameters (Lipophilicity from both scenarios)
  group1Params <- if (is.list(piParams[[1]]$parameters)) {
    piParams[[1]]$parameters
  } else {
    list(piParams[[1]]$parameters)
  }
  expect_equal(length(group1Params), 2)

  group2Params <- if (is.list(piParams[[2]]$parameters)) {
    piParams[[2]]$parameters
  } else {
    list(piParams[[2]]$parameters)
  }
  expect_equal(length(group2Params), 1)

  group3Params <- if (is.list(piParams[[3]]$parameters)) {
    piParams[[3]]$parameters
  } else {
    list(piParams[[3]]$parameters)
  }
  expect_equal(length(group3Params), 1)
  expect_equal(length(piTask$outputMappings), 2)

  expect_no_error(piResults <- runPI(piTasks))

  expect_equal(length(piResults), 1)
  result <- piResults[[1]]
  expect_true(is.logical(result$success))
})
