projectConfiguration <- testProjectConfiguration()

# Define which scenarios to run
scenarioNames <- c("TestScenario", "PopulationScenario")
outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

# Create `ScenarioConfiguration` objects from excel files
scenarioConfigurations <- readScenarioConfigurationFromExcel(
  scenarioNames = scenarioNames,
  projectConfiguration = projectConfiguration
)

# Set output paths for each scenario
for (scenarioConfiguration in scenarioConfigurations) {
  scenarioConfiguration$outputPaths <- outputPaths
}

# Run scenarios
scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations)

simulatedScenarios <- runScenarios(
  scenarios = scenarios
)

importerConfiguration <- ospsuite::loadDataImporterConfiguration(
  configurationFilePath = projectConfiguration$dataImporterConfigurationFile
)

# Load observed data
dataSheets <- "Laskin 1982.Group A"
observedData <- esqlabsR::loadObservedData(
  projectConfiguration = projectConfiguration,
  sheets = dataSheets,
  importerConfiguration = importerConfiguration
)

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
    regexp = messages$stopNoPathProvided("DC_missingPath")
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
    regexp = messages$stopNoDataSetProvided("AciclovirPVB")
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
    regexp = "The following scenarios are not present in `simulatedScenarios`"
  )

  # Then test with stopIfNotFound = FALSE
  expect_warning(
    .validateDataCombinedFromExcel(
      df_invalid_scenario,
      list(),
      observedData,
      stopIfNotFound = FALSE
    ),
    regexp = "The following scenarios are not present in `simulatedScenarios`"
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
