# The .validateDataCombinedFromExcel() blocks below share one base fixture: an
# observed-data load plus a 4-row DataCombined-from-Excel data frame. Each block
# builds its own copy inside the block (no file-scope shared mutable state), so
# the blocks stay isolated and order-independent. `local_options` is not needed
# here: setup.R already sets `lifecycle_verbosity = "quiet"` for the suite.
.dataCombinedFromExcelFixture <- function() {
  scenarioNames <- c("testscenario", "populationscenario")
  outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  observedData <- loadObservedData(testProject())
  df <- data.frame(list(
    "dataCombinedName" = c(
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
  list(
    scenarioNames = scenarioNames,
    outputPaths = outputPaths,
    observedData = observedData,
    df = df
  )
}

test_that("It returns correct names of data combined when a path is not specified for one simulated scenario", {
  fx <- .dataCombinedFromExcelFixture()
  df_missing_path <- fx$df
  df_missing_path$path[3] <- NA

  expect_error(
    .validateDataCombinedFromExcel(df_missing_path, list(), fx$observedData),
    "No output path is defined"
  )
})

test_that("It errors when label is missing", {
  fx <- .dataCombinedFromExcelFixture()
  df_missing_label <- fx$df
  df_missing_label$label[1] <- NA
  expect_error(
    .validateDataCombinedFromExcel(df_missing_label, list(), fx$observedData),
    regexp = messages$missingLabel()
  )
})

test_that("It errors when dataType is missing", {
  fx <- .dataCombinedFromExcelFixture()
  df_missing_dataType <- fx$df
  df_missing_dataType$dataType[1] <- NA
  expect_error(
    .validateDataCombinedFromExcel(
      df_missing_dataType,
      list(),
      fx$observedData
    ),
    regexp = messages$missingDataType()
  )
})

test_that("It errors when scenario is missing for simulated dataType", {
  fx <- .dataCombinedFromExcelFixture()
  df_missing_scenario <- fx$df
  df_missing_scenario$scenario[1] <- NA
  expect_error(
    .validateDataCombinedFromExcel(
      df_missing_scenario,
      list(),
      fx$observedData
    ),
    regexp = messages$missingScenarioName()
  )
})

test_that("It errors when dataSet is missing for observed dataType", {
  fx <- .dataCombinedFromExcelFixture()
  df_missing_dataSet <- fx$df
  df_missing_dataSet$dataSet[2] <- NA
  expect_error(
    .validateDataCombinedFromExcel(df_missing_dataSet, list(), fx$observedData),
    "No data set is defined"
  )
})

test_that("It warns when scenario is not found in scenarioResults", {
  fx <- .dataCombinedFromExcelFixture()
  df_invalid_scenario <- fx$df
  df_invalid_scenario$scenario[1] <- "NonExistentScenario"

  # First test with stopIfNotFound = TRUE
  expect_error(
    .validateDataCombinedFromExcel(
      df_invalid_scenario,
      list(),
      fx$observedData,
      stopIfNotFound = TRUE
    ),
    "The following scenarios are not present"
  )

  # Then test with stopIfNotFound = FALSE
  expect_warning(
    .validateDataCombinedFromExcel(
      df_invalid_scenario,
      list(),
      fx$observedData,
      stopIfNotFound = FALSE
    ),
    "The following scenarios are not present"
  )
})

test_that("It warns when dataSet is not found in observedData", {
  fx <- .dataCombinedFromExcelFixture()
  # Mock scenarioResults to get past the scenario-not-found error.
  mock_scenario <- list()
  mock_scenario[[fx$scenarioNames[1]]] <- list(
    results = list(allQuantityPaths = fx$outputPaths)
  )

  df_invalid_dataSet <- fx$df
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
    createDataCombined(project, dataCombined = "Nonexistent"),
    "The following DataCombined names are not defined"
  )
})

test_that("createDataCombined errors when a requested plotGrids name is unknown", {
  project <- exampleProject()
  # An unknown plot grid name must abort rather than being silently dropped
  # (the intersection would otherwise yield an incomplete or empty result).
  expect_snapshot(
    error = TRUE,
    createDataCombined(project, plotGrids = "DoesNotExist")
  )
})

test_that("createDataCombined builds DataCombined for Example project", {
  project <- exampleProject()
  simulated <- runScenarios(project, scenarios = "aciclovir_iv")
  dcName <- names(project$definitions$dataCombined)[[1]]

  result <- createDataCombined(
    project,
    dataCombined = dcName,
    scenarioResults = simulated
  )

  expect_named(result, dcName)
  expect_s3_class(result[[dcName]], "DataCombined")
  df <- result[[dcName]]$toDataFrame()
  expect_setequal(unique(df$dataType), c("simulated", "observed"))
})

test_that("createDataCombined errors when dataCombined is not a string", {
  project <- testProject()
  # The leading call-context in the validator message is context-dependent,
  # so match only the stable type-mismatch portion.
  expect_error(
    createDataCombined(project, dataCombined = 123),
    "is of type <numeric>, but expected <character>"
  )
})

test_that("createDataCombined applies declared offsets and scale factors", {
  project <- testProject()
  path <- project$definitions$outputPaths$aciclovir_pvb
  addDataCombined(
    project,
    "dc_plain",
    simulated = list(list(
      label = "sim",
      scenario = "testscenario",
      path = path,
      group = "g"
    ))
  )
  addDataCombined(
    project,
    "dc_offset",
    simulated = list(list(
      label = "sim",
      scenario = "testscenario",
      path = path,
      group = "g",
      xOffsets = 1,
      xOffsetsUnits = "h",
      yScaleFactors = 2
    ))
  )
  simulated <- runScenarios(project, scenarios = "testscenario")

  result <- createDataCombined(
    project,
    dataCombined = c("dc_plain", "dc_offset"),
    scenarioResults = simulated
  )
  plain <- result$dc_plain$toDataFrame()
  offset <- result$dc_offset$toDataFrame()

  # 1 h x-offset shifts time by 60 (base unit minutes); yScaleFactor doubles y.
  expect_equal(min(offset$xValues), min(plain$xValues) + 60)
  expect_equal(offset$yValues, plain$yValues * 2)
})

test_that("createDataCombined(stopIfNotFound = FALSE) drops a wrong-path entry with offsets", {
  project <- testProject()
  addDataCombined(
    project,
    "dc_wrong",
    simulated = list(list(
      label = "sim",
      scenario = "testscenario",
      path = "Organism|NotAReal|Path",
      group = "g",
      xOffsets = 1,
      xOffsetsUnits = "h"
    ))
  )
  simulated <- runScenarios(project, scenarios = "testscenario")

  expect_warning(
    result <- createDataCombined(
      project,
      dataCombined = "dc_wrong",
      scenarioResults = simulated,
      stopIfNotFound = FALSE
    ),
    "has not been simulated"
  )
  expect_s3_class(result$dc_wrong, "DataCombined")
  # The skipped row must not reach the transform block.
  expect_null(result$dc_wrong$toDataFrame())
})

test_that("createDataCombined reports a failed scenario run distinctly", {
  project <- testProject()
  path <- project$definitions$outputPaths$aciclovir_pvb
  addDataCombined(
    project,
    "dc_failed",
    simulated = list(list(
      label = "sim",
      scenario = "testscenario",
      path = path,
      group = "g"
    ))
  )
  # A failed run is present in scenarioResults but carries results = NULL. The
  # key must match the scenario casing so the lookup resolves and the code
  # reaches the results = NULL failed-run branch (not the missing-scenario one).
  failedRun <- list(
    testscenario = list(
      simulation = NULL,
      results = NULL,
      outputValues = NULL,
      population = NULL
    )
  )

  expect_snapshot(
    error = TRUE,
    createDataCombined(
      project,
      dataCombined = "dc_failed",
      scenarioResults = failedRun
    )
  )
})

test_that("createDataCombined returns empty DataCombined when spec has no entries", {
  project <- .fakeProject(
    dataCombined = list(
      EmptyDC = list(name = "EmptyDC", simulated = list(), observed = list())
    )
  )

  result <- createDataCombined(project, dataCombined = "EmptyDC")

  expect_named(result, "EmptyDC")
  expect_s3_class(result$EmptyDC, "DataCombined")
  expect_null(result$EmptyDC$toDataFrame())
})
