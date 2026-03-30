projectConfiguration <- testProjectConfiguration()

test_that("createPITasks creates ParameterIdentification objects from configurations", {
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

test_that("createPITasks creates PIParameters with correct bounds", {
  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    piTaskNames = "AciclovirSimple",
    projectConfiguration = projectConfiguration
  )

  piTasks <- createPITasks(piTaskConfigurations)
  firstTask <- piTasks[[1]]
  expect_true(length(firstTask$parameters) > 0)
  firstParam <- firstTask$parameters[[1]]
  expect_true(!is.null(firstParam$minValue))
  expect_true(!is.null(firstParam$maxValue))
  expect_true(!is.null(firstParam$startValue))
  expect_true(firstParam$minValue <= firstParam$startValue)
  expect_true(firstParam$startValue <= firstParam$maxValue)
})

test_that("createPITasks reuses scenarios across multiple tasks", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()
  sheets$PIConfiguration <- rbind(
    sheets$PIConfiguration,
    sheets$PIConfiguration
  )
  sheets$PIConfiguration$PITaskName <- c("Task1", "Task2")
  sheets$PIParameters <- rbind(sheets$PIParameters, sheets$PIParameters)
  sheets$PIParameters$PITaskName <- c("Task1", "Task2")
  sheets$PIOutputMappings <- rbind(
    sheets$PIOutputMappings,
    sheets$PIOutputMappings
  )
  sheets$PIOutputMappings$PITaskName <- c("Task1", "Task2")

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfigurationLocal
  )
  piTasks <- createPITasks(piTaskConfigurations)

  expect_equal(length(piTasks), 2)
  expect_equal(names(piTasks), c("Task1", "Task2"))
  expect_true(length(piTasks$Task1$simulations) > 0)
  expect_true(length(piTasks$Task2$simulations) > 0)

  expect_identical(
    piTasks$Task1$simulations[[1]]$simulation,
    piTasks$Task2$simulations[[1]]$simulation
  )
})

test_that("createPITasks requires DataSet column to be specified", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()
  sheets$PIOutputMappings$DataSet <- NA

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfigurationLocal
  )

  expect_error(
    createPITasks(piTaskConfigurations),
    regexp = messages$errorPIColumnRequired("DataSet", "PIOutputMappings")
  )
})

test_that("createPITasks throws error when DataSet name doesn't exist", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()
  sheets$PIOutputMappings$DataSet <- "NonExistentDataSet"

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfigurationLocal
  )
  observedData <- loadObservedData(
    projectConfigurationLocal,
    sheets = "Laskin 1982.Group A"
  )

  expect_error(
    createPITasks(piTaskConfigurations),
    regexp = messages$errorPIDatasetNotFound(
      "NonExistentDataSet",
      names(observedData)
    )
  )
})

test_that("createPITasks throws error when parameter bounds are invalid", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()
  sheets$PIParameters$MinValue <- 2
  sheets$PIParameters$MaxValue <- 0

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfigurationLocal
  )

  expect_error(
    createPITasks(piTaskConfigurations),
    regexp = messages$errorPIInvalidBounds(
      "Aciclovir|Lipophilicity",
      min = 2,
      start = -0.1,
      max = 0
    ),
    fixed = TRUE
  )
})

test_that("Same parameter across scenarios with different bounds in same group fails", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  # Create configuration with same parameter in 2 scenarios with different bounds
  sheets <- createValidPISheets()
  sheets$PIParameters <- data.frame(
    PITaskName = c("Task1", "Task1"),
    Scenarios = c("PITestScenario", "PIScenario_500mg"),
    `Container Path` = c("Aciclovir", "Aciclovir"),
    `Parameter Name` = c("Lipophilicity", "Lipophilicity"),
    Value = c(-0.1, -0.1),
    Units = c("Log Units", "Log Units"),
    MinValue = c(-2, -5),
    MaxValue = c(2, 5),
    StartValue = c(-0.1, -0.1),
    Group = c(1, 1),
    check.names = FALSE
  )

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfigurationLocal
  )

  expect_error(
    createPITasks(piTaskConfigurations),
    messages$errorPIGroupBoundsMismatch("1", "Aciclovir\\|Lipophilicity")
  )
})

test_that("createPITasks throws error when parameter not found in simulation", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()
  sheets$PIParameters$`Container Path` <- "InvalidPath"
  sheets$PIParameters$`Parameter Name` <- "InvalidParameter"

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfigurationLocal
  )

  expect_error(
    createPITasks(piTaskConfigurations),
    regexp = "InvalidPath\\|InvalidParameter"
  )
})

test_that("createPITasks handles all Group values as NA correctly", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()
  sheets$PIParameters <- rbind(
    sheets$PIParameters,
    data.frame(
      PITaskName = "Task1",
      Scenarios = "PITestScenario",
      `Container Path` = "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS",
      `Parameter Name` = "TSspec",
      Value = NA,
      Units = NA,
      MinValue = 0,
      MaxValue = 10,
      StartValue = 5,
      Group = NA,
      check.names = FALSE
    )
  )
  sheets$PIParameters$Group[1] <- NA

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )
  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfigurationLocal
  )

  expect_no_error(piTasks <- createPITasks(piTaskConfigurations))
  piTask <- piTasks[[1]]
  piParams <- piTask$parameters
  expect_equal(length(piParams), 2)
  expect_equal(length(piParams[[1]]$parameters), 1)
  expect_equal(length(piParams[[2]]$parameters), 1)
})

test_that("createPITasks creates grouped PIParameters across multiple scenarios", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  # Add high dose observed data
  dataFile <- file.path(
    temp_project$path,
    "Data",
    "TestProject_TimeValuesData.xlsx"
  )
  obsData <- readExcel(dataFile, sheet = "Laskin 1982.Group A")

  highDoseData <- obsData
  highDoseData$Dose <- "5.0 mg/kg"
  highDoseData$Measurement <- highDoseData$Measurement * 1.5
  combinedData <- rbind(obsData, highDoseData)

  .writeExcel(
    data = list("Laskin 1982.Group A" = combinedData),
    path = dataFile,
    append = FALSE
  )

  # Update PIOutputMappings to use correct dataset for second scenario
  piFile <- projectConfigurationLocal$parameterIdentificationFile
  piOutputMappings <- readExcel(piFile, sheet = "PIOutputMappings")

  # Update second row to use high-dose dataset
  piOutputMappings$DataSet[
    piOutputMappings$PITaskName == "AciclovirMultiScenario" &
      piOutputMappings$Scenarios == "PIScenario_500mg"
  ] <-
    "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_5.0 mg/kg_iv_"

  # Write back all PI sheets
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

  # Create PI tasks
  expect_no_error(piTasks <- createPITasks(piTaskConfigurations))

  # Verify parameter grouping structure
  piTask <- piTasks[[1]]
  piParams <- piTask$parameters

  expect_equal(length(piParams), 3)
  expect_equal(length(piParams[[1]]$parameters), 2)
  expect_equal(length(piParams[[2]]$parameters), 1)
  expect_equal(piParams[[1]]$parameters[[1]]$name, "Lipophilicity")
  expect_equal(piParams[[1]]$parameters[[2]]$name, "Lipophilicity")
  expect_equal(piParams[[2]]$parameters[[1]]$name, "TSspec")

  # Verify output mappings structure (2 scenarios)
  expect_equal(length(piTask$outputMappings), 2)
  expect_true(
    grepl("2.5 mg", names(piTask$outputMappings[[1]]$observedDataSets))
  )
  expect_true(
    grepl("5.0 mg", names(piTask$outputMappings[[2]]$observedDataSets))
  )
})

test_that("createPITasks throws error when no scenarios are configured", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  # Create PI configuration with empty scenario configuration
  piTaskConfig <- PITaskConfiguration$new(
    taskName = "TestTask",
    projectConfiguration = projectConfigurationLocal,
    scenarioConfiguration = list(),
    piDefinitions = list(
      piConfiguration = list(),
      piParameters = list(),
      piOutputMappings = list()
    )
  )

  expect_error(
    createPITasks(list(TestTask = piTaskConfig)),
    regexp = messages$errorPINoScenariosConfigured(),
    fixed = TRUE
  )
})

test_that("readPITaskConfigurationFromExcel throws error when scenario not found in PIParameters", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()
  sheets$PIParameters$Scenarios <- "NonExistentScenario"

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  expect_error(
    readPITaskConfigurationFromExcel(
      projectConfiguration = projectConfigurationLocal
    ),
    regexp = "NonExistentScenario"
  )
})

test_that("readPITaskConfigurationFromExcel throws error when scenario not found in PIOutputMappings", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()
  sheets$PIOutputMappings$Scenarios <- "NonExistentScenario"

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  expect_error(
    readPITaskConfigurationFromExcel(
      projectConfiguration = projectConfigurationLocal
    ),
    regexp = "NonExistentScenario"
  )
})

test_that("createPITasks applies scalar Weight to PIOutputMapping dataWeights", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()
  sheets$PIOutputMappings$Weight <- "2"

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfigurationLocal
  )
  piTasks <- createPITasks(piTaskConfigurations)

  outputMapping <- piTasks$Task1$outputMappings[[1]]
  dataSetName <- names(outputMapping$observedDataSets)[[1]]

  expect_true(all(outputMapping$dataWeights[[dataSetName]] == 2))
})

test_that("createPITasks applies xOffset/yOffset/xFactor/yFactor to PIOutputMapping dataTransformations", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()
  sheets$PIOutputMappings$xOffset <- 0.5
  sheets$PIOutputMappings$yOffset <- 1.0
  sheets$PIOutputMappings$xFactor <- 2.0
  sheets$PIOutputMappings$yFactor <- 0.5

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfigurationLocal
  )
  piTasks <- createPITasks(piTaskConfigurations)

  outputMapping <- piTasks$Task1$outputMappings[[1]]
  dataSetName <- names(outputMapping$observedDataSets)[[1]]
  transformations <- outputMapping$dataTransformations

  expect_equal(unname(transformations$xOffsets[2]), 0.5)
  expect_equal(unname(transformations$yOffsets[2]), 1.0)
  expect_equal(unname(transformations$xFactors[2]), 2.0)
  expect_equal(unname(transformations$yFactors[2]), 0.5)
})

test_that("createPITasks applies simulationRunOptions from PIConfiguration columns", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()

  # Both NA: simulationRunOptions stays NULL
  .writeExcel(data = sheets, path = projectConfigurationLocal$parameterIdentificationFile)
  piTasks <- createPITasks(readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfigurationLocal
  ))
  expect_null(piTasks$Task1$configuration$simulationRunOptions)

  # Only numberOfCores set
  sheets$PIConfiguration$numberOfCores <- 2
  .writeExcel(data = sheets, path = projectConfigurationLocal$parameterIdentificationFile)
  piTasks <- createPITasks(readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfigurationLocal
  ))
  opts <- piTasks$Task1$configuration$simulationRunOptions
  expect_false(is.null(opts))
  expect_equal(opts$numberOfCores, 2L)

  # Only checkForNegativeValues set
  sheets$PIConfiguration$numberOfCores <- NA_real_
  sheets$PIConfiguration$checkForNegativeValues <- FALSE
  .writeExcel(data = sheets, path = projectConfigurationLocal$parameterIdentificationFile)
  piTasks <- createPITasks(readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfigurationLocal
  ))
  opts <- piTasks$Task1$configuration$simulationRunOptions
  expect_false(is.null(opts))
  expect_false(opts$checkForNegativeValues)

  # Both set
  sheets$PIConfiguration$numberOfCores <- 4
  .writeExcel(data = sheets, path = projectConfigurationLocal$parameterIdentificationFile)
  piTasks <- createPITasks(readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfigurationLocal
  ))
  opts <- piTasks$Task1$configuration$simulationRunOptions
  expect_equal(opts$numberOfCores, 4L)
  expect_false(opts$checkForNegativeValues)
})

test_that("createPITasks applies objectiveFunctionOptions from PIConfiguration columns", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()

  # All NA: defaults are preserved
  .writeExcel(data = sheets, path = projectConfigurationLocal$parameterIdentificationFile)
  piTasks <- createPITasks(readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfigurationLocal
  ))
  ofo <- piTasks$Task1$configuration$objectiveFunctionOptions
  expect_equal(ofo$objectiveFunctionType, "lsq")
  expect_equal(ofo$residualWeightingMethod, "none")
  expect_equal(ofo$robustMethod, "none")

  # Set individual fields
  sheets$PIConfiguration$ObjectiveFunctionType <- "m3"
  sheets$PIConfiguration$ResidualWeightingMethod <- "std"
  .writeExcel(data = sheets, path = projectConfigurationLocal$parameterIdentificationFile)
  piTasks <- createPITasks(readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfigurationLocal
  ))
  ofo <- piTasks$Task1$configuration$objectiveFunctionOptions
  expect_equal(ofo$objectiveFunctionType, "m3")
  expect_equal(ofo$residualWeightingMethod, "std")
  expect_equal(ofo$robustMethod, "none")

  # Numeric options
  sheets$PIConfiguration$ObjectiveFunctionType <- NA
  sheets$PIConfiguration$ResidualWeightingMethod <- NA
  sheets$PIConfiguration$LinScaleCV <- 0.3
  sheets$PIConfiguration$LogScaleSD <- 0.1
  .writeExcel(data = sheets, path = projectConfigurationLocal$parameterIdentificationFile)
  piTasks <- createPITasks(readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfigurationLocal
  ))
  ofo <- piTasks$Task1$configuration$objectiveFunctionOptions
  expect_equal(ofo$linScaleCV, 0.3)
  expect_equal(ofo$logScaleSD, 0.1)
})

test_that("runPI executes single PI task successfully", {
  piTaskName <- "AciclovirSimple"
  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    piTaskNames = piTaskName,
    projectConfiguration = projectConfiguration
  )
  piTasks <- createPITasks(piTaskConfigurations)
  invisible(capture.output(suppressMessages(piResults <- runPI(piTasks))))

  expect_true(is.list(piResults))
  expect_equal(length(piResults), 1)
  expect_equal(names(piResults), piTaskName)

  result <- piResults[[piTaskName]]
  expect_true(isOfType(result$task, "ParameterIdentification"))
  expect_true(isOfType(result$result, "PIResult"))
  expect_null(result$error)
})

test_that("runPI returns failure result with warning when optimization fails", {
  temp_project <- with_temp_project()
  projectConfigurationLocal <- temp_project$config

  sheets <- createValidPISheets()

  .writeExcel(
    data = sheets,
    path = projectConfigurationLocal$parameterIdentificationFile
  )

  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    projectConfiguration = projectConfigurationLocal
  )

  piTasks <- createPITasks(piTaskConfigurations)

  # Create mock ParameterIdentification that fails during run
  PISimFailureTester <- R6::R6Class(
    classname = "PISimFailureTester",
    inherit = ospsuite.parameteridentification::ParameterIdentification,
    cloneable = FALSE,
    public = list(
      run = function() {
        stop("Simulated optimization failure")
      }
    )
  )

  # Replace the PI task with our failing mock
  mockTask <- PISimFailureTester$new(
    simulations = piTasks[[1]]$simulations,
    parameters = piTasks[[1]]$parameters,
    outputMappings = piTasks[[1]]$outputMappings,
    configuration = piTasks[[1]]$configuration
  )
  piTasks[[1]] <- mockTask

  # Should warn but not error
  expect_warning(
    piResults <- runPI(piTasks),
    messages$warningPIOptimizationFailed(
      "Task1",
      "Simulated optimization failure"
    ),
    fixed = TRUE
  )

  result <- piResults[[1]]
  expect_null(result$result)
  expect_equal(result$error, "Simulated optimization failure")
})

test_that("createPITasks uses OutputPath from row instead of all scenario output paths", {
  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    piTaskNames = "AciclovirSimple",
    projectConfiguration = projectConfiguration
  )

  piTasks <- createPITasks(piTaskConfigurations)
  firstTask <- piTasks[[1]]

  # Should have exactly 1 output mapping (1 row = 1 mapping)
  expect_equal(length(firstTask$outputMappings), 1)
})

test_that("createPITasks overwrites scenario output paths with PI-specified paths", {
  piTaskConfigurations <- readPITaskConfigurationFromExcel(
    piTaskNames = "AciclovirSimple",
    projectConfiguration = projectConfiguration
  )

  piTasks <- createPITasks(piTaskConfigurations)

  # Get the scenario from the task and check its output paths were updated
  scenarioConfig <- piTaskConfigurations[["AciclovirSimple"]]$scenarioConfiguration[["PITestScenario"]]
  # The scenario originally has output paths from Scenarios.xlsx
  # After createPITasks, the simulation output selections should match PI-specified paths
  simulation <- piTasks[[1]]$simulations[[1]]
  outputSelections <- sapply(simulation$outputSelections$allOutputs, function(x) x$path)
  expect_equal(
    outputSelections,
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  )
})
