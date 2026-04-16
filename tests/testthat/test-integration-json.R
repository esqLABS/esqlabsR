# Full JSON workflow integration tests ----

test_that("JSON workflow: load project, run scenario, get results", {
  pc <- testProjectConfiguration()

  results <- runScenarios(pc, scenarioNames = "TestScenario")

  expect_true("TestScenario" %in% names(results))
  expect_false(is.null(results$TestScenario$results))
  expect_false(is.null(results$TestScenario$outputValues))
})

test_that("JSON workflow: run multiple scenarios", {
  pc <- testProjectConfiguration()

  results <- runScenarios(
    pc,
    scenarioNames = c("TestScenario", "TestScenario2")
  )

  expect_equal(sort(names(results)), c("TestScenario", "TestScenario2"))
  expect_false(is.null(results$TestScenario$results))
  expect_false(is.null(results$TestScenario2$results))
})

test_that("JSON workflow: createDataCombined produces DataCombined objects", {
  pc <- testProjectConfiguration()

  results <- runScenarios(pc, scenarioNames = "TestScenario")

  dcList <- createDataCombined(
    projectConfiguration = pc,
    dataCombinedNames = "AciclovirPVB",
    simulatedScenarios = results
  )

  expect_true("AciclovirPVB" %in% names(dcList))
  expect_true(inherits(dcList$AciclovirPVB, "DataCombined"))
})

test_that("JSON workflow: createPlots produces plot output", {
  pc <- testProjectConfiguration()

  results <- runScenarios(pc, scenarioNames = "TestScenario")

  plotOutput <- createPlots(
    projectConfiguration = pc,
    plotGridNames = "Aciclovir",
    simulatedScenarios = results
  )

  expect_false(is.null(plotOutput))
})

test_that("JSON workflow: round-trip JSON -> Excel -> JSON preserves data", {
  testProject <- local_test_project()

  # Load from JSON
  pc1 <- ProjectConfiguration$new(testProject$snapshot_path)

  # Export to Excel
  exportDir <- withr::local_tempdir("roundtrip_export")
  exportProjectConfigurationToExcel(pc1, outputDir = exportDir, silent = TRUE)

  # Import back to JSON
  excelPath <- file.path(exportDir, "ProjectConfiguration.xlsx")
  importProjectConfigurationFromExcel(excelPath, outputDir = exportDir, silent = TRUE)

  # Load the round-tripped JSON
  pc2 <- ProjectConfiguration$new(file.path(exportDir, "ProjectConfiguration.json"))

  # Compare key data structures
  expect_equal(names(pc1$scenarios), names(pc2$scenarios))
  # modelParameters may gain species parameter groups during import,
  # so check that original groups are preserved (superset is OK)
  expect_true(all(names(pc1$modelParameters) %in% names(pc2$modelParameters)))
  expect_equal(length(pc1$individuals), length(pc2$individuals))
  expect_equal(length(pc1$populations), length(pc2$populations))
})

test_that("JSON workflow: round-trip preserves scenario outputPaths", {
  testProject <- local_test_project()

  pc1 <- ProjectConfiguration$new(testProject$snapshot_path)

  # Pick a scenario that has outputPaths set via outputPathIds
  scenarioName <- "TestScenario2"
  originalOutputPaths <- pc1$scenarios[[scenarioName]]$outputPaths

  # Export to Excel
  exportDir <- withr::local_tempdir("roundtrip_outputpaths")
  exportProjectConfigurationToExcel(pc1, outputDir = exportDir, silent = TRUE)

  # Import back to JSON
  excelPath <- file.path(exportDir, "ProjectConfiguration.xlsx")
  importProjectConfigurationFromExcel(excelPath, outputDir = exportDir, silent = TRUE)

  # Load the round-tripped JSON
  pc2 <- ProjectConfiguration$new(file.path(exportDir, "ProjectConfiguration.json"))

  # The outputPaths should be preserved through round-trip
  roundTrippedOutputPaths <- pc2$scenarios[[scenarioName]]$outputPaths
  expect_equal(roundTrippedOutputPaths, originalOutputPaths)
})
