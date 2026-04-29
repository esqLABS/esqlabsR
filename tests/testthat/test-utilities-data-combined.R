project <- testProject()

scenarioNames <- c("TestScenario", "PopulationScenario")
outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

simulatedScenarios <- runScenarios(
  project,
  scenarioNames = scenarioNames
)

observedDataForSetup <- loadObservedData(project)

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
  "dataSet" = c(
    NA,
    names(observedDataForSetup)[1],
    NA,
    names(observedDataForSetup)[1]
  ),
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
    .validateDataCombined(df_missing_path, list(), observedDataForSetup),
    regexp = messages$stopNoPathProvided("DC_missingPath")
  )
})

test_that("It errors when label is missing", {
  df_missing_label <- dataCombinedDf
  df_missing_label$label[1] <- NA
  expect_error(
    .validateDataCombined(df_missing_label, list(), observedDataForSetup),
    regexp = messages$missingLabel()
  )
})

test_that("It errors when dataType is missing", {
  df_missing_dataType <- dataCombinedDf
  df_missing_dataType$dataType[1] <- NA
  expect_error(
    .validateDataCombined(df_missing_dataType, list(), observedDataForSetup),
    regexp = messages$missingDataType()
  )
})

test_that("It errors when scenario is missing for simulated dataType", {
  df_missing_scenario <- dataCombinedDf
  df_missing_scenario$scenario[1] <- NA
  expect_error(
    .validateDataCombined(df_missing_scenario, list(), observedDataForSetup),
    regexp = messages$missingScenarioName()
  )
})

test_that("It errors when dataSet is missing for observed dataType", {
  df_missing_dataSet <- dataCombinedDf
  df_missing_dataSet$dataSet[2] <- NA
  expect_error(
    .validateDataCombined(df_missing_dataSet, list(), observedDataForSetup),
    regexp = messages$stopNoDataSetProvided("AciclovirPVB")
  )
})

test_that("It warns when scenario is not found in simulatedScenarios", {
  df_invalid_scenario <- dataCombinedDf
  df_invalid_scenario$scenario[1] <- "NonExistentScenario"

  # First test with stopIfNotFound = TRUE
  expect_error(
    .validateDataCombined(
      df_invalid_scenario,
      list(),
      observedDataForSetup,
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
    .validateDataCombined(
      df_invalid_scenario,
      list(),
      observedDataForSetup,
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
    .validateDataCombined(
      df_invalid_dataSet,
      mock_scenario,
      list(),
      stopIfNotFound = TRUE
    ),
    regexp = "The following data sets are not present in `observedData`"
  )

  # Then test with stopIfNotFound = FALSE
  expect_warning(
    .validateDataCombined(
      df_invalid_dataSet,
      mock_scenario,
      list(),
      stopIfNotFound = FALSE
    ),
    regexp = "The following data sets are not present in `observedData`"
  )
})

test_that("createDataCombined loads observed data automatically from Project", {
  dcList <- createDataCombined(
    project = project,
    dataCombinedNames = "AciclovirPVB",
    simulatedScenarios = simulatedScenarios
  )
  expect_true("AciclovirPVB" %in% names(dcList))
  expect_true(inherits(dcList$AciclovirPVB, "DataCombined"))
})

test_that("createDataCombined errors when specified DataCombined names are not found", {
  expect_error(
    createDataCombined(
      project = project,
      dataCombinedNames = c("AciclovirPVB", "NonExistentDC1", "NonExistentDC2"),
      simulatedScenarios = simulatedScenarios
    ),
    regexp = messages$stopDataCombinedNamesNotFound(c(
      "NonExistentDC1",
      "NonExistentDC2"
    )),
    fixed = TRUE
  )
})

# addDataCombined / removeDataCombined ----

test_that("addDataCombined appends rows and marks the project modified", {
  pc <- testProject()
  pc$.markSaved()
  beforeRows <- nrow(pc$plots$dataCombined)
  beforeNames <- unique(pc$plots$dataCombined$DataCombinedName)

  addDataCombined(
    project = pc,
    name = "NewDC",
    simulated = list(
      list(label = "sim1", scenario = "TestScenario", path = outputPaths)
    ),
    observed = list(
      list(label = "obs1", dataSet = names(observedDataForSetup)[[1]])
    )
  )

  expect_equal(nrow(pc$plots$dataCombined), beforeRows + 2L)
  expect_true("NewDC" %in% pc$plots$dataCombined$DataCombinedName)
  expect_setequal(
    pc$plots$dataCombined$dataType[
      pc$plots$dataCombined$DataCombinedName == "NewDC"
    ],
    c("simulated", "observed")
  )
  expect_true(pc$modified)
  expect_false(any(beforeNames == "NewDC"))
})

test_that("addDataCombined errors when the DataCombined name already exists", {
  pc <- testProject()
  expect_error(
    addDataCombined(
      project = pc,
      name = "AciclovirPVB",
      simulated = list(
        list(label = "sim1", scenario = "TestScenario", path = outputPaths)
      )
    ),
    regexp = "already exists"
  )
})

test_that("addDataCombined errors when simulated entry misses required fields", {
  pc <- testProject()
  expect_error(
    addDataCombined(
      project = pc,
      name = "BadDC",
      simulated = list(list(label = "x", scenario = "TestScenario"))
    ),
    regexp = "path"
  )
})

test_that("addDataCombined errors when observed entry misses required fields", {
  pc <- testProject()
  expect_error(
    addDataCombined(
      project = pc,
      name = "BadDC2",
      observed = list(list(label = "x"))
    ),
    regexp = "dataSet"
  )
})

test_that("addDataCombined R6 method delegates to standalone", {
  pc1 <- testProject()
  pc2 <- testProject()
  sim <- list(
    list(label = "s", scenario = "TestScenario", path = outputPaths)
  )
  addDataCombined(pc1, name = "DelegateDC", simulated = sim)
  pc2$addDataCombined(name = "DelegateDC", simulated = sim)
  expect_equal(
    nrow(pc1$plots$dataCombined),
    nrow(pc2$plots$dataCombined)
  )
})

test_that("addDataCombined returns project invisibly", {
  pc <- testProject()
  out <- withVisible(
    addDataCombined(
      project = pc,
      name = "InvisDC",
      simulated = list(
        list(label = "s", scenario = "TestScenario", path = outputPaths)
      )
    )
  )
  expect_false(out$visible)
  expect_identical(out$value, pc)
})

test_that("addDataCombined survives a JSON round-trip", {
  pc <- testProject()
  addDataCombined(
    project = pc,
    name = "RoundTripDC",
    simulated = list(
      list(
        label = "s1",
        scenario = "TestScenario",
        path = outputPaths,
        group = "g",
        xOffsets = 2
      )
    ),
    observed = list(
      list(label = "o1", dataSet = names(observedDataForSetup)[[1]])
    )
  )

  tmp <- withr::local_tempfile(fileext = ".json")
  saveProject(pc, path = tmp)
  reloaded <- loadProject(tmp)

  expect_true(
    "RoundTripDC" %in% reloaded$plots$dataCombined$DataCombinedName
  )
  rtRows <- reloaded$plots$dataCombined[
    reloaded$plots$dataCombined$DataCombinedName == "RoundTripDC",
  ]
  expect_setequal(rtRows$dataType, c("simulated", "observed"))
  expect_setequal(rtRows$label, c("s1", "o1"))
})

test_that("removeDataCombined drops all rows for the named DataCombined", {
  pc <- testProject()
  pc$.markSaved()
  beforeRows <- nrow(pc$plots$dataCombined)
  removedRows <- sum(pc$plots$dataCombined$DataCombinedName == "AciclovirPop")
  expect_gt(removedRows, 0L)

  suppressWarnings(removePlot(pc, "P4"))
  removeDataCombined(pc, "AciclovirPop")

  expect_equal(nrow(pc$plots$dataCombined), beforeRows - removedRows)
  expect_false("AciclovirPop" %in% pc$plots$dataCombined$DataCombinedName)
  expect_true(pc$modified)
})

test_that("removeDataCombined warns and is a no-op for unknown name", {
  pc <- testProject()
  pc$.markSaved()
  expect_warning(
    removeDataCombined(pc, "NoSuchDC_ZZZ"),
    regexp = "not found"
  )
  expect_false(pc$modified)
})

test_that("removeDataCombined warns when referenced by plotConfiguration", {
  pc <- testProject()
  expect_warning(
    removeDataCombined(pc, "AciclovirPVB"),
    regexp = "referenced"
  )
})
