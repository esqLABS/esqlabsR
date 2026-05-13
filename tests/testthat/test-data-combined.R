withr::local_options(lifecycle_verbosity = "quiet")

# Define which scenarios to run
scenarioNames <- c("TestScenario", "PopulationScenario")
outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

# Run scenarios from the JSON-based project
.testProjectForDC <- testProject()
simulatedScenarios <- runScenarios(
  .testProjectForDC,
  scenarioNames = scenarioNames
)

observedData <- loadObservedData(.testProjectForDC)

# Create a proper data frame with paths for all entries
dataCombinedDf <- data.frame(list(
  "DataCombinedName" = c(
    "AciclovirPVB",
    "AciclovirPVB",
    "DC_missingPath",
    "DC_missingPath"
  ),
  "dataType" = c("simulated", "observed", "simulated", "observed"),
  "label" = c(
    "Aciclovir simulated",
    "Aciclovir observed",
    "Aciclovir simulated",
    "Aciclovir observed"
  ),
  "scenario" = c(scenarioNames[1], NA, scenarioNames[1], NA),
  "path" = c(outputPaths, NA, outputPaths, NA),
  "dataSet" = c(NA, names(observedData), NA, names(observedData)),
  "group" = c(
    "Aciclovir PVB",
    "Aciclovir PVB",
    "Aciclovir PVB",
    "Aciclovir PVB"
  ),
  "xOffsets" = c(NA, NA, NA, NA),
  "xOffsetsUnits" = c(NA, NA, NA, NA),
  "yOffsets" = c(NA, NA, NA, NA),
  "yOffsetsUnits" = c(NA, NA, NA, NA),
  "xScaleFactors" = c(NA, NA, NA, NA),
  "yScaleFactors" = c(NA, NA, NA, NA)
))

test_that("It returns correct names of data combined when a path is not specified for one simulated scenario", {
  # Create a specific data frame with a missing path for testing
  df_missing_path <- dataCombinedDf
  df_missing_path$path[3] <- NA

  expect_error(
    .validateDataCombinedFromExcel(df_missing_path, list(), observedData),
    "No output path is defined"
  )
})

test_that("It errors when label is missing", {
  df_missing_label <- dataCombinedDf
  df_missing_label$label[1] <- NA
  expect_error(
    .validateDataCombinedFromExcel(df_missing_label, list(), observedData),
    regexp = messages$missingLabel()
  )
})

test_that("It errors when dataType is missing", {
  df_missing_dataType <- dataCombinedDf
  df_missing_dataType$dataType[1] <- NA
  expect_error(
    .validateDataCombinedFromExcel(df_missing_dataType, list(), observedData),
    regexp = messages$missingDataType()
  )
})

test_that("It errors when scenario is missing for simulated dataType", {
  df_missing_scenario <- dataCombinedDf
  df_missing_scenario$scenario[1] <- NA
  expect_error(
    .validateDataCombinedFromExcel(df_missing_scenario, list(), observedData),
    regexp = messages$missingScenarioName()
  )
})

test_that("It errors when dataSet is missing for observed dataType", {
  df_missing_dataSet <- dataCombinedDf
  df_missing_dataSet$dataSet[2] <- NA
  expect_error(
    .validateDataCombinedFromExcel(df_missing_dataSet, list(), observedData),
    "No data set is defined"
  )
})

test_that("It warns when scenario is not found in simulatedScenarios", {
  df_invalid_scenario <- dataCombinedDf
  df_invalid_scenario$scenario[1] <- "NonExistentScenario"

  # First test with stopIfNotFound = TRUE
  expect_error(
    .validateDataCombinedFromExcel(
      df_invalid_scenario,
      list(),
      observedData,
      stopIfNotFound = TRUE
    ),
    "The following scenarios are not present"
  )

  # Then test with stopIfNotFound = FALSE
  expect_warning(
    .validateDataCombinedFromExcel(
      df_invalid_scenario,
      list(),
      observedData,
      stopIfNotFound = FALSE
    ),
    "The following scenarios are not present"
  )
})

test_that("It warns when dataSet is not found in observedData", {
  # Create mock simulatedScenarios to avoid the scenario not found error
  mock_scenario <- list()
  mock_scenario[[scenarioNames[1]]] <- list(
    results = list(allQuantityPaths = outputPaths)
  )

  df_invalid_dataSet <- dataCombinedDf
  df_invalid_dataSet$dataSet[2] <- "NonExistentDataSet"

  # First test with stopIfNotFound = TRUE
  expect_error(
    .validateDataCombinedFromExcel(
      df_invalid_dataSet,
      mock_scenario,
      list(),
      stopIfNotFound = TRUE
    ),
    regexp = "The following data sets are not present in `observedData`"
  )

  # Then test with stopIfNotFound = FALSE
  expect_warning(
    .validateDataCombinedFromExcel(
      df_invalid_dataSet,
      mock_scenario,
      list(),
      stopIfNotFound = FALSE
    ),
    regexp = "The following data sets are not present in `observedData`"
  )
})


# createDataCombined(project, ...) tests ----

test_that("createDataCombined errors on non-Project input", {
  expect_error(createDataCombined("not a project"), "expected <Project>")
})

test_that("createDataCombined returns empty list when no names given", {
  project <- testProject()
  expect_identical(createDataCombined(project), list())
})

test_that("createDataCombined errors when requested name not in project", {
  project <- testProject()
  # TestProject has plots = NULL, so any requested name is missing
  expect_error(
    createDataCombined(project, dataCombinedNames = "Nonexistent"),
    "The following DataCombined names are not defined"
  )
})

test_that("createDataCombined builds DataCombined for Example project", {
  project <- exampleProject()
  simulated <- runScenarios(project, scenarioNames = "Aciclovir_iv")
  dcName <- names(project$plots$dataCombined)[[1]]

  result <- createDataCombined(
    project,
    dataCombinedNames = dcName,
    simulatedScenarios = simulated
  )

  expect_named(result, dcName)
  expect_s3_class(result[[dcName]], "DataCombined")
  df <- result[[dcName]]$toDataFrame()
  expect_setequal(unique(df$dataType), c("simulated", "observed"))
})

test_that("createDataCombined returns empty DataCombined when spec has no entries", {
  project <- Project$new()
  project$schemaVersion <- "2.0"
  project$scenarios <- list()
  project$modelParameterSets <- list()
  project$individualParameterSets <- list()
  project$applicationParameterSets <- list()
  project$individuals <- list()
  project$populations <- list()
  project$applications <- list()
  project$observedData <- list()
  project$outputPaths <- list()
  project$plots <- list(
    dataCombined = list(
      EmptyDC = list(name = "EmptyDC", simulated = list(), observed = list())
    )
  )

  result <- createDataCombined(project, dataCombinedNames = "EmptyDC")

  expect_named(result, "EmptyDC")
  expect_s3_class(result$EmptyDC, "DataCombined")
  expect_null(result$EmptyDC$toDataFrame())
})
