# Full JSON workflow integration tests ----

test_that("JSON workflow: load project, run scenario, get results", {
  project <- testProject()

  results <- runScenarios(project, scenarioNames = "TestScenario")

  expect_true("TestScenario" %in% names(results))
  expect_false(is.null(results$TestScenario$results))
  expect_false(is.null(results$TestScenario$outputValues))
})

test_that("JSON workflow: run multiple scenarios", {
  project <- testProject()

  results <- runScenarios(
    project,
    scenarioNames = c("TestScenario", "TestScenario2")
  )

  expect_equal(sort(names(results)), c("TestScenario", "TestScenario2"))
  expect_false(is.null(results$TestScenario$results))
  expect_false(is.null(results$TestScenario2$results))
})

test_that("JSON workflow: createDataCombined produces DataCombined objects", {
  project <- testProject()

  results <- runScenarios(project, scenarioNames = "TestScenario")

  dcList <- createDataCombined(
    project = project,
    dataCombinedNames = "AciclovirPVB",
    simulatedScenarios = results
  )

  expect_true("AciclovirPVB" %in% names(dcList))
  expect_true(inherits(dcList$AciclovirPVB, "DataCombined"))
})

test_that("JSON workflow: createPlots produces plot output", {
  project <- testProject()

  results <- runScenarios(project, scenarioNames = "TestScenario")

  plotOutput <- createPlots(
    project = project,
    plotGridNames = "Aciclovir",
    simulatedScenarios = results
  )

  expect_false(is.null(plotOutput))
})

test_that("JSON workflow: round-trip JSON -> Excel -> JSON preserves data", {
  testProject <- local_test_project()

  # Load from JSON
  pc1 <- Project$new(testProject$snapshot_path)

  # Export to Excel
  exportDir <- withr::local_tempdir("roundtrip_export")
  exportProjectToExcel(pc1, outputDir = exportDir, silent = TRUE)

  # Import back to JSON
  excelPath <- file.path(exportDir, "Project.xlsx")
  importProjectFromExcel(excelPath, outputDir = exportDir, silent = TRUE)

  # Load the round-tripped JSON
  pc2 <- Project$new(file.path(exportDir, "Project.json"))

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

  pc1 <- Project$new(testProject$snapshot_path)

  # Pick a scenario that has outputPaths set via outputPathIds
  scenarioName <- "TestScenario2"
  originalOutputPaths <- pc1$scenarios[[scenarioName]]$outputPaths

  # Export to Excel
  exportDir <- withr::local_tempdir("roundtrip_outputpaths")
  exportProjectToExcel(pc1, outputDir = exportDir, silent = TRUE)

  # Import back to JSON
  excelPath <- file.path(exportDir, "Project.xlsx")
  importProjectFromExcel(excelPath, outputDir = exportDir, silent = TRUE)

  # Load the round-tripped JSON
  pc2 <- Project$new(file.path(exportDir, "Project.json"))

  # The outputPaths should be preserved through round-trip
  roundTrippedOutputPaths <- pc2$scenarios[[scenarioName]]$outputPaths
  expect_equal(roundTrippedOutputPaths, originalOutputPaths)
})
