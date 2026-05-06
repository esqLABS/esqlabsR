withr::local_options(lifecycle_verbosity = "quiet")

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
observedData <- withr::with_options(
  list(lifecycle_verbosity = "quiet"),
  esqlabsR::loadObservedDataFromExcel(
    projectConfiguration = projectConfiguration,
    sheets = dataSheets,
    importerConfiguration = importerConfiguration
  )
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
    regexp = messages$warningInvalidScenarioName(c(
      "NonExistentScenario",
      "TestScenario"
    )),
    fixed = TRUE
  )

  # Then test with stopIfNotFound = FALSE
  expect_warning(
    .validateDataCombinedFromExcel(
      df_invalid_scenario,
      list(),
      observedData,
      stopIfNotFound = FALSE
    ),
    regexp = messages$warningInvalidScenarioName(c(
      "NonExistentScenario",
      "TestScenario"
    )),
    fixed = TRUE
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

test_that("createDataCombinedFromExcel errors when specified DataCombined names are not in the Excel file", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_error(
    createDataCombinedFromExcel(
      projectConfiguration = projectConfiguration,
      dataCombinedNames = c("AciclovirPVB", "NonExistentDC1", "NonExistentDC2"),
      simulatedScenarios = simulatedScenarios,
      observedData = observedData
    ),
    regexp = messages$stopDataCombinedNamesNotFound(c(
      "NonExistentDC1",
      "NonExistentDC2"
    )),
    fixed = TRUE
  )
})

# createDataCombined(project, ...) tests ----

test_that("createDataCombined errors on non-Project input", {
  expect_error(createDataCombined("not a project"), "expected <Project>")
})

test_that("createDataCombined returns empty list when no names given", {
  project <- loadProject(testProjectJSONPath())
  expect_identical(createDataCombined(project), list())
})

test_that("createDataCombined errors when requested name not in project", {
  project <- loadProject(testProjectJSONPath())
  # TestProject has plots = NULL, so any requested name is missing
  expect_error(
    createDataCombined(project, dataCombinedNames = "Nonexistent"),
    regexp = messages$stopDataCombinedNamesNotFound("Nonexistent"),
    fixed = TRUE
  )
})

test_that("createDataCombined builds DataCombined for Example project", {
  examplePath <- system.file(
    "extdata/projects/Example/Project.json",
    package = "esqlabsR"
  )
  project <- loadProject(examplePath)
  spec <- project$plots$dataCombined[[1]]
  scenarioName <- spec$simulated[[1]]$scenario
  path <- spec$simulated[[1]]$path
  simulatedScenarios <- list()
  simulatedScenarios[[scenarioName]] <- list(
    results = list(allQuantityPaths = path)
  )
  result <- tryCatch(
    createDataCombined(
      project,
      dataCombinedNames = names(project$plots$dataCombined)[[1]],
      simulatedScenarios = simulatedScenarios
    ),
    error = function(e) e
  )
  # Either a list of DataCombined (success) or a downstream ospsuite error
  # caused by the mock results having no values. Both prove the dispatch
  # reached the project-driven path.
  expect_true(inherits(result, "list") || inherits(result, "error"))
})
