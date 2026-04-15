# ---- importProjectConfigurationFromExcel tests ----

test_that("importProjectConfigurationFromExcel creates v2.0 JSON from Excel files", {
  paths <- local_test_project()

  outputDir <- withr::local_tempdir("test_import")

  jsonPath <- importProjectConfigurationFromExcel(
    paths$project_config_path,
    outputDir,
    silent = TRUE
  )

  expect_true(file.exists(jsonPath))

  jsonData <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  expect_equal(jsonData$schemaVersion, "2.0")
  expect_true("projectConfiguration" %in% names(jsonData))
  expect_true("scenarios" %in% names(jsonData))
  expect_true("modelParameters" %in% names(jsonData))
  expect_true("individuals" %in% names(jsonData))
  expect_true("populations" %in% names(jsonData))
  expect_true("applications" %in% names(jsonData))
  expect_true("outputPaths" %in% names(jsonData))
  expect_true("plots" %in% names(jsonData))

  # Check v2.0 schema structure
  expect_true(is.list(jsonData$scenarios))
  expect_true(length(jsonData$scenarios) > 0)
  expect_true(!is.null(jsonData$scenarios[[1]]$name))

  # modelParameters should be named lists of arrays
  expect_true(length(jsonData$modelParameters) > 0)
  firstSheet <- jsonData$modelParameters[[1]]
  expect_true(is.list(firstSheet))
  expect_true(!is.null(firstSheet[[1]]$containerPath))
})

test_that("importProjectConfigurationFromExcel creates JSON in source dir by default", {
  paths <- local_test_project()

  jsonPath <- importProjectConfigurationFromExcel(
    paths$project_config_path,
    silent = TRUE
  )

  expectedPath <- sub("\\.xlsx$", ".json", paths$project_config_path)
  expect_equal(normalizePath(jsonPath), normalizePath(expectedPath))
  expect_true(file.exists(jsonPath))
})

test_that("importProjectConfigurationFromExcel JSON can be loaded by ProjectConfiguration", {
  paths <- local_test_project()

  outputDir <- withr::local_tempdir("test_loadable")

  jsonPath <- importProjectConfigurationFromExcel(
    paths$project_config_path,
    outputDir,
    silent = TRUE
  )

  expect_no_error({
    pc <- loadProject(jsonPath)
  })

  expect_s3_class(pc, "ProjectConfiguration")
  expect_true(length(pc$scenarioConfigurations) > 0)
  expect_true(length(pc$modelParameters) > 0)
  expect_true(length(pc$individuals) > 0)
})

# ---- exportProjectConfigurationToExcel tests ----

test_that("exportProjectConfigurationToExcel creates Excel files from ProjectConfiguration", {
  pc <- testProjectConfigurationJSON()

  outputDir <- withr::local_tempdir("test_export")

  projConfigPath <- exportProjectConfigurationToExcel(
    pc,
    outputDir = outputDir,
    silent = TRUE
  )

  # Check that ProjectConfiguration.xlsx was created
  expect_true(file.exists(projConfigPath))

  # Check configurations directory was created
  configDir <- file.path(outputDir, "Configurations")
  expect_true(dir.exists(configDir))

  # Check key Excel files were created
  expect_true(file.exists(file.path(configDir, "ModelParameters.xlsx")))
  expect_true(file.exists(file.path(configDir, "Scenarios.xlsx")))
  expect_true(file.exists(file.path(configDir, "Individuals.xlsx")))
  expect_true(file.exists(file.path(configDir, "Applications.xlsx")))
})

test_that("exportProjectConfigurationToExcel preserves model parameters", {
  pc <- testProjectConfigurationJSON()

  outputDir <- withr::local_tempdir("test_export_params")
  exportProjectConfigurationToExcel(pc, outputDir = outputDir, silent = TRUE)

  # Read back the ModelParameters.xlsx
  paramsFile <- file.path(outputDir, "Configurations", "ModelParameters.xlsx")
  sheets <- readxl::excel_sheets(paramsFile)
  expect_true("Global" %in% sheets)

  globalDf <- readExcel(paramsFile, sheet = "Global")
  expect_true("Container Path" %in% names(globalDf))
  expect_true("Parameter Name" %in% names(globalDf))
  expect_true("Value" %in% names(globalDf))
})

# ---- projectConfigurationStatus tests ----

test_that("projectConfigurationStatus correctly handles missing JSON file", {
  test_proj <- local_test_project()

  json_path <- file.path(test_proj$dir, "NonExistent.json")
  expect_error(
    projectConfigurationStatus(test_proj$project_config_path, json_path),
    "JSON file does not exist"
  )
})

test_that("projectConfigurationStatus detects in-sync state after fresh import", {
  test_proj <- local_test_project()

  # Import fresh JSON from Excel
  importProjectConfigurationFromExcel(
    test_proj$project_config_path,
    silent = TRUE
  )

  jsonPath <- sub("\\.xlsx$", ".json", test_proj$project_config_path)
  expect_true(file.exists(jsonPath))

  status_result <- projectConfigurationStatus(
    test_proj$project_config_path,
    jsonPath,
    silent = TRUE
  )
  expect_true(status_result$in_sync)
})

# ---- Deprecated wrapper tests ----

test_that("snapshotProjectConfiguration calls importProjectConfigurationFromExcel with deprecation", {
  paths <- local_test_project()
  outputDir <- withr::local_tempdir("test_deprecated_snapshot")

  lifecycle::expect_deprecated(
    snapshotProjectConfiguration(
      paths$project_config_path,
      outputDir,
      silent = TRUE
    )
  )
})

test_that("restoreProjectConfiguration calls exportProjectConfigurationToExcel with deprecation", {
  paths <- local_test_project()

  lifecycle::expect_deprecated(
    restoreProjectConfiguration(
      paths$snapshot_path,
      outputDir = withr::local_tempdir("test_deprecated_restore"),
      silent = TRUE
    )
  )
})
