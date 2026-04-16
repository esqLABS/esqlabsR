# ---- importProjectFromExcel tests ----

test_that("importProjectFromExcel creates v2.0 JSON from Excel files", {
  paths <- local_test_project()

  outputDir <- withr::local_tempdir("test_import")

  jsonPath <- importProjectFromExcel(
    paths$project_config_path,
    outputDir,
    silent = TRUE
  )

  expect_true(file.exists(jsonPath))

  jsonData <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  expect_equal(jsonData$schemaVersion, "2.0")
  expect_true("filePaths" %in% names(jsonData))
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

test_that("importProjectFromExcel creates JSON in source dir by default", {
  paths <- local_test_project()

  jsonPath <- importProjectFromExcel(
    paths$project_config_path,
    silent = TRUE
  )

  expectedPath <- sub("\\.xlsx$", ".json", paths$project_config_path)
  expect_equal(normalizePath(jsonPath), normalizePath(expectedPath))
  expect_true(file.exists(jsonPath))
})

test_that("importProjectFromExcel JSON can be loaded by Project", {
  paths <- local_test_project()

  outputDir <- withr::local_tempdir("test_loadable")

  jsonPath <- importProjectFromExcel(
    paths$project_config_path,
    outputDir,
    silent = TRUE
  )

  expect_no_error({
    pc <- loadProject(jsonPath)
  })

  expect_s3_class(pc, "Project")
  expect_true(length(pc$scenarios) > 0)
  expect_true(length(pc$modelParameters) > 0)
  expect_true(length(pc$individuals) > 0)
})

test_that("importProjectFromExcel includes species parameters in modelParameters", {
  temp_project <- local_test_project()
  jsonPath <- importProjectFromExcel(
    temp_project$project_config_path,
    silent = TRUE
  )
  jsonData <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)

  # SpeciesParameters.xlsx should have species sheets (Rat, Rabbit, Monkey, etc.)
  # which should now appear in modelParameters
  speciesFile <- system.file("extdata", "SpeciesParameters.xlsx", package = "esqlabsR")
  speciesSheets <- readxl::excel_sheets(speciesFile)

  # At least one species sheet should be merged into modelParameters
  speciesInModel <- intersect(speciesSheets, names(jsonData$modelParameters))
  expect_true(length(speciesInModel) > 0)

  # Verify the species sheet has the standard parameter structure
  for (sheetName in speciesInModel) {
    sheet <- jsonData$modelParameters[[sheetName]]
    expect_true(length(sheet) > 0)
    first <- sheet[[1]]
    expect_true(all(c("containerPath", "parameterName", "value") %in% names(first)))
  }
})

# ---- exportProjectToExcel tests ----

test_that("exportProjectToExcel creates Excel files from Project", {
  pc <- testProject()

  outputDir <- withr::local_tempdir("test_export")

  projConfigPath <- exportProjectToExcel(
    pc,
    outputDir = outputDir,
    silent = TRUE
  )

  # Check that Project.xlsx was created
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

test_that("exportProjectToExcel works with plain-data individuals and scenarios", {
  pc <- testProject()
  tempDir <- withr::local_tempdir()
  exportProjectToExcel(pc, outputDir = tempDir, silent = TRUE)

  # Check that key files were created
  expect_true(file.exists(file.path(tempDir, "Configurations", "Scenarios.xlsx")))
  expect_true(file.exists(file.path(tempDir, "Configurations", "Individuals.xlsx")))
})

test_that("exportProjectToExcel preserves model parameters", {
  pc <- testProject()

  outputDir <- withr::local_tempdir("test_export_params")
  exportProjectToExcel(pc, outputDir = outputDir, silent = TRUE)

  # Read back the ModelParameters.xlsx
  paramsFile <- file.path(outputDir, "Configurations", "ModelParameters.xlsx")
  sheets <- readxl::excel_sheets(paramsFile)
  expect_true("Global" %in% sheets)

  globalDf <- readExcel(paramsFile, sheet = "Global")
  expect_true("Container Path" %in% names(globalDf))
  expect_true("Parameter Name" %in% names(globalDf))
  expect_true("Value" %in% names(globalDf))
})

# ---- projectStatus tests ----

test_that("projectStatus correctly handles missing JSON file", {
  test_proj <- local_test_project()

  json_path <- file.path(test_proj$dir, "NonExistent.json")
  expect_error(
    projectStatus(test_proj$project_config_path, json_path),
    "JSON file does not exist"
  )
})

test_that("projectStatus detects in-sync state after fresh import", {
  test_proj <- local_test_project()

  # Import fresh JSON from Excel
  importProjectFromExcel(
    test_proj$project_config_path,
    silent = TRUE
  )

  jsonPath <- sub("\\.xlsx$", ".json", test_proj$project_config_path)
  expect_true(file.exists(jsonPath))

  status_result <- projectStatus(
    test_proj$project_config_path,
    jsonPath,
    silent = TRUE
  )
  expect_true(status_result$in_sync)
})

test_that("projectStatus detects out-of-sync state when JSON is modified", {
  test_proj <- local_test_project()

  # Import fresh JSON from Excel to ensure a baseline in-sync state
  importProjectFromExcel(
    test_proj$project_config_path,
    silent = TRUE
  )

  jsonPath <- sub("\\.xlsx$", ".json", test_proj$project_config_path)
  expect_true(file.exists(jsonPath))

  # Modify the JSON to make it out of sync with Excel
  jsonData <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  original_name <- jsonData$scenarios[[1]]$name
  jsonData$scenarios[[1]]$name <- paste0(original_name, "_MODIFIED")
  writeLines(
    jsonlite::toJSON(jsonData, auto_unbox = TRUE, pretty = TRUE),
    jsonPath
  )

  status_result <- projectStatus(
    test_proj$project_config_path,
    jsonPath,
    silent = TRUE
  )
  expect_false(status_result$in_sync)
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
