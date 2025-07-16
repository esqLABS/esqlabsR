test_that("snapshotProjectConfiguration exports project configuration to JSON with correct filename", {
  # Set up test project using our fixture
  paths <- local_test_project()

  # Create output directory that will be automatically cleaned up
  outputDir <- withr::local_tempdir("test_export")

  # Get the expected JSON filename based on source Excel file
  excelFilename <- basename(paths$project_config_path)
  expectedJsonFilename <- sub("\\.xlsx$", ".json", excelFilename)
  expectedJsonPath <- file.path(outputDir, expectedJsonFilename)

  # Export to JSON with explicit outputDir
  configData <- snapshotProjectConfiguration(
    paths$project_config_path,
    outputDir
  )

  # Check JSON content with snapshot
  expect_snapshot(configData)

  # Check that JSON file with expected name exists
  expect_true(file.exists(expectedJsonPath))

  # Check JSON content in file with snapshot
  jsonContent <- jsonlite::fromJSON(expectedJsonPath)
  expect_snapshot(jsonContent)
})

test_that("restoreProjectConfiguration creates Excel files from JSON with correct filename", {
  # Set up test project using our fixture
  paths <- local_test_project()

  # Create output directories that will be automatically cleaned up
  exportDir <- withr::local_tempdir("test_export")
  importDir <- withr::local_tempdir("test_import")

  # Export to JSON with explicit outputDir
  excelFilename <- basename(paths$project_config_path)
  jsonFilename <- sub("\\.xlsx$", ".json", excelFilename)

  configData <- snapshotProjectConfiguration(
    paths$project_config_path,
    exportDir
  )

  # Get the actual JSON path
  jsonPath <- file.path(exportDir, jsonFilename)

  # Check JSON file exists
  expect_true(file.exists(jsonPath))

  # Import from JSON with explicit outputDir
  projectConfig <- restoreProjectConfiguration(jsonPath, importDir)

  # Check that a ProjectConfiguration object is returned
  expect_s3_class(projectConfig, "ProjectConfiguration")

  # Expected Excel filename in importDir should match the original Excel filename
  expectedExcelPath <- file.path(importDir, excelFilename)
  expect_true(file.exists(expectedExcelPath))

  # Check configurations directory was created
  configDir <- file.path(importDir, "Configurations")
  expect_true(dir.exists(configDir))

  # Check at least some Excel files were created
  excelFiles <- list.files(configDir, pattern = "\\.xlsx$")
  expect_true(length(excelFiles) > 0)
})

test_that("Excel files in Configurations folder preserve content after JSON roundtrip", {
  # Set up test project using our fixture
  paths <- local_test_project()

  # Create output directories for export and import
  exportDir <- withr::local_tempdir("test_excel_content_export")
  importDir <- withr::local_tempdir("test_excel_content_import")

  # Get the file mappings to test
  fileMapping <- list(
    modelParameters = "ModelParameters.xlsx",
    individuals = "Individuals.xlsx",
    populations = "Populations.xlsx",
    scenarios = "Scenarios.xlsx",
    applications = "Applications.xlsx",
    plots = "Plots.xlsx"
  )

  # Export to JSON with explicit outputDir
  excelFilename <- basename(paths$project_config_path)
  jsonFilename <- sub("\\.xlsx$", ".json", excelFilename)

  configData <- snapshotProjectConfiguration(
    paths$project_config_path,
    exportDir
  )

  # Get the actual JSON path
  jsonPath <- file.path(exportDir, jsonFilename)

  # Import from JSON with explicit outputDir
  restoreProjectConfiguration(jsonPath, importDir)

  # Configurations directory in the import directory
  configDir <- file.path(importDir, "Configurations")
  expect_true(dir.exists(configDir))

  # Verify sheet names are preserved in each Excel file
  for (name in names(fileMapping)) {
    # Get paths to original and regenerated files
    originalPath <- file.path(paths$configurations_dir, fileMapping[[name]])
    regeneratedPath <- file.path(
      configDir,
      fileMapping[[name]]
    )

    # Skip if original file doesn't exist
    if (!file.exists(originalPath)) {
      next
    }

    # Verify the regenerated file exists
    expect_true(
      file.exists(regeneratedPath),
      info = paste("Regenerated file not found:", regeneratedPath)
    )

    # Get sheet names from both files
    originalSheets <- readxl::excel_sheets(originalPath)
    regeneratedSheets <- readxl::excel_sheets(regeneratedPath)

    # Check that both files have the same sheets
    expect_identical(
      originalSheets,
      regeneratedSheets,
      info = paste("Sheet names don't match for:", fileMapping[[name]])
    )

    # Check basic data structure is preserved
    for (sheet in originalSheets) {
      # Read sheets from both files
      originalData <- readExcel(originalPath, sheet)
      regeneratedData <- readExcel(regeneratedPath, sheet)

      # Check that both files have the same data
      expect_identical(originalData, regeneratedData)
    }
  }
})

test_that("ProjectConfiguration can be created from regenerated JSON files", {
  # Set up test project using our fixture
  paths <- local_test_project()

  # Get the original Excel filename
  excelFilename <- basename(paths$project_config_path)

  # Create output directories for export and import
  exportDir <- withr::local_tempdir("test_config_export")
  importDir <- withr::local_tempdir("test_config_import")

  # Export to JSON with explicit outputDir
  jsonFilename <- sub("\\.xlsx$", ".json", excelFilename)
  configData <- snapshotProjectConfiguration(
    paths$project_config_path,
    exportDir
  )
  jsonPath <- file.path(exportDir, jsonFilename)

  # Import from JSON with explicit outputDir
  restoreProjectConfiguration(jsonPath, importDir)

  # Path to regenerated ProjectConfiguration.xlsx (in root directory, not in Configurations)
  regeneratedProjectConfigPath <- file.path(
    importDir,
    excelFilename
  )

  # Verify the file was created successfully
  expect_true(file.exists(regeneratedProjectConfigPath))

  # Change working directory to outputDir to use relative paths
  oldWd <- getwd()
  setwd(importDir)
  on.exit(setwd(oldWd), add = TRUE)

  # Try to create a ProjectConfiguration object with a relative path
  relativePath <- excelFilename
  expect_no_error({
    projectConfig <- createProjectConfiguration(relativePath)
  })

  # Verify the created object is of the correct class
  projectConfig <- createProjectConfiguration(relativePath)
  expect_s3_class(projectConfig, "ProjectConfiguration")
})

test_that("ProjectConfiguration.xlsx data is preserved in JSON round-trip", {
  # Set up test project using our fixture
  paths <- local_test_project()

  # Get the original Excel filename
  excelFilename <- basename(paths$project_config_path)

  # Create output directories for export and import
  exportDir <- withr::local_tempdir("test_data_export")
  importDir <- withr::local_tempdir("test_data_import")

  # Export to JSON with explicit outputDir
  jsonFilename <- sub("\\.xlsx$", ".json", excelFilename)
  configData <- snapshotProjectConfiguration(
    paths$project_config_path,
    exportDir
  )
  jsonPath <- file.path(exportDir, jsonFilename)

  # Import from JSON with explicit outputDir
  regeneratedConfig <- restoreProjectConfiguration(jsonPath, importDir)

  # Check that a ProjectConfiguration object was returned
  expect_s3_class(regeneratedConfig, "ProjectConfiguration")

  # Check that ProjectConfiguration.xlsx was created with the same name as original
  expectedExcelPath <- file.path(importDir, excelFilename)
  expect_true(file.exists(expectedExcelPath))

  # Read the original ProjectConfiguration.xlsx
  originalDf <- readExcel(paths$project_config_path)

  # Read the regenerated ProjectConfiguration.xlsx to compare directly
  regeneratedDf <- readExcel(expectedExcelPath)

  # Check that both data frames are identical
  expect_identical(originalDf, regeneratedDf)
})

test_that("projectConfigurationStatus() correctly handles missing JSON file", {
  # Create a temporary test project
  test_proj <- local_test_project()

  # Path to non-existent JSON file
  json_path <- file.path(test_proj$dir, "NonExistent.json")

  # Check status - should throw an error
  expect_error(
    projectConfigurationStatus(test_proj$project_config_path, json_path),
    "JSON file does not exist"
  )
})

test_that("projectConfigurationStatus() automatically finds JSON file when not specified", {
  # Create a temporary test project
  test_proj <- local_test_project()

  # Take a snapshot of the current configuration using default name
  snapshotProjectConfiguration(test_proj$project_config_path)

  # Default JSON path should be in the same directory as the Excel file
  expected_json_path <- sub("\\.xlsx$", ".json", test_proj$project_config_path)
  expect_true(file.exists(expected_json_path))

  # Check status without specifying JSON path - should find it automatically
  status_result <- projectConfigurationStatus(test_proj$project_config_path)
  expect_true(status_result$in_sync)
})


test_that("projectConfigurationStatus() correctly identifies in-sync files", {
  # Create a temporary test project
  test_proj <- local_test_project()

  # Check status - should be in sync
  status_result <- projectConfigurationStatus(
    test_proj$project_config_path,
    test_proj$snapshot_path
  )
  expect_true(
    status_result$in_sync,
    info = r"{If some changes we made to one of the project configuration excel files, the following test will fail
because the ProjectConfiguration Snapshot was not updated. Update the snapshot by running the lines below:

  snapshotProjectConfiguration(
    projectConfig = "inst/extdata/examples/TestProject/ProjectConfiguration.xlsx",
    outputDir = "inst/extdata/examples/TestProject"
  )
}"
  )
})

test_that("projectConfigurationStatus() detects sheet-level changes in Excel files", {
  # Create a temporary test project
  test_proj <- local_test_project()

  # Path to Plots.xlsx in the configurations directory
  plots_path <- file.path(test_proj$configurations_dir, "Plots.xlsx")

  # Get existing sheets in Plots.xlsx
  existing_sheets <- readxl::excel_sheets(plots_path)

  # Create a new sheet with test data
  new_sheet_data <- data.frame(
    TestID = c("test1", "test2"),
    TestName = c("Test One", "Test Two"),
    TestValue = c(1, 2)
  )

  # Load existing data into a list
  sheet_list <- list()
  for (sheet in existing_sheets) {
    sheet_list[[sheet]] <- readExcel(plots_path, sheet)
  }

  # Add new sheet to the list
  sheet_list[["NewTestSheet"]] <- new_sheet_data

  # Write all sheets back to the file
  .writeExcel(sheet_list, plots_path)

  # Verify the new sheet was added
  updated_sheets <- readxl::excel_sheets(plots_path)
  expect_true("NewTestSheet" %in% updated_sheets)

  # Check status - should detect sheet-level changes
  status_result <- projectConfigurationStatus(
    test_proj$project_config_path,
    test_proj$snapshot_path
  )
  expect_false(status_result$in_sync)

  expect_snapshot(status_result$details)
})

test_that("projectConfigurationStatus() detects data-level changes in Excel sheets", {
  # Create a temporary test project
  test_proj <- local_test_project()

  # Path to Scenarios.xlsx in the configurations directory
  scenarios_path <- createProjectConfiguration(
    test_proj$project_config_path
  )$scenariosFile

  # Get existing sheets in Scenarios.xlsx
  existing_sheets <- readxl::excel_sheets(scenarios_path)

  new_data <- list()

  for (i in seq_along(existing_sheets)) {
    sheet_data <- readExcel(scenarios_path, existing_sheets[i])

    if (i == 1) {
      # copy last row to a new row
      sheet_data[nrow(sheet_data) + 1, ] <- sheet_data[nrow(sheet_data), ]
    }
    new_data[[existing_sheets[i]]] <- sheet_data
  }

  .writeExcel(new_data, scenarios_path)

  # Check status - should detect data-level changes
  status_result <- projectConfigurationStatus(
    test_proj$project_config_path,
    test_proj$snapshot_path
  )
  expect_false(status_result$in_sync)

  expect_snapshot(status_result$details)
})

test_that("projectConfigurationStatus() handles simultaneous sheet, and data changes", {
  # Create a temporary test project
  test_proj <- local_test_project()

  # 1. Make data change
  scenarios_path <- createProjectConfiguration(
    test_proj$project_config_path
  )$scenariosFile

  # Get existing sheets in Scenarios.xlsx
  existing_sheets <- readxl::excel_sheets(scenarios_path)

  new_data <- list()

  for (i in seq_along(existing_sheets)) {
    sheet_data <- readExcel(scenarios_path, existing_sheets[i])

    if (i == 1) {
      # copy last row to a new row
      sheet_data[nrow(sheet_data) + 1, ] <- sheet_data[nrow(sheet_data), ]
    }
    new_data[[existing_sheets[i]]] <- sheet_data
  }

  .writeExcel(new_data, scenarios_path)

  # 2. Add a new sheet to an existing Excel file
  plots_path <- createProjectConfiguration(
    test_proj$project_config_path
  )$plotsFile
  if (file.exists(plots_path)) {
    # Create a new sheet
    new_sheet_data <- data.frame(
      TestID = c("test1", "test2"),
      TestName = c("Test One", "Test Two")
    )

    # Write to the file as a new sheet
    existing_sheets <- readxl::excel_sheets(plots_path)
    sheet_list <- list()
    for (sheet in existing_sheets) {
      sheet_list[[sheet]] <- readExcel(plots_path, sheet)
    }
    sheet_list[["CombinedTestSheet"]] <- new_sheet_data
    .writeExcel(sheet_list, plots_path)
  }

  # Check status - should detect all types of changes
  status_result <- projectConfigurationStatus(
    test_proj$project_config_path,
    test_proj$snapshot_path
  )
  expect_false(status_result$in_sync)

  expect_snapshot(status_result$details)
})

# Tests for modified flag behavior in JSON utilities context
test_that("snapshotProjectConfiguration works with unmodified ProjectConfiguration", {
  # Create a test project
  test_proj <- local_test_project()

  # Load ProjectConfiguration (should be unmodified)
  projectConfig <- createProjectConfiguration(test_proj$project_config_path)
  expect_false(projectConfig$modified)

  # Create output directory
  outputDir <- withr::local_tempdir("test_snapshot_unmodified")

  # Snapshot should work without issues
  expect_no_error({
    configData <- snapshotProjectConfiguration(projectConfig, outputDir)
  })

  # Original config should still be unmodified
  expect_false(projectConfig$modified)

  # JSON file should be created
  excelFilename <- basename(projectConfig$projectConfigurationFilePath)
  jsonFilename <- sub("\\.xlsx$", ".json", excelFilename)
  jsonPath <- file.path(outputDir, jsonFilename)
  expect_true(file.exists(jsonPath))
})

test_that("snapshotProjectConfiguration works with modified ProjectConfiguration", {
  # Create a test project
  test_proj <- local_test_project()

  # Load and modify ProjectConfiguration
  projectConfig <- createProjectConfiguration(test_proj$project_config_path)
  suppressWarnings(projectConfig$modelFolder <- "modified/model/folder")
  expect_true(projectConfig$modified)

  # Create output directory
  outputDir <- withr::local_tempdir("test_snapshot_modified")

  # Snapshot should work even with modified config
  expect_no_error({
    configData <- snapshotProjectConfiguration(projectConfig, outputDir)
  })

  # Original config should still be modified (snapshot doesn't change this)
  expect_true(projectConfig$modified)

  # JSON file should be created
  excelFilename <- basename(projectConfig$projectConfigurationFilePath)
  jsonFilename <- sub("\\.xlsx$", ".json", excelFilename)
  jsonPath <- file.path(outputDir, jsonFilename)
  expect_true(file.exists(jsonPath))
})

test_that("restoreProjectConfiguration returns ProjectConfiguration with modified=FALSE", {
  # Create a test project and snapshot
  test_proj <- local_test_project()

  # Create output directories
  exportDir <- withr::local_tempdir("test_restore_export")
  importDir <- withr::local_tempdir("test_restore_import")

  # Export to JSON
  excelFilename <- basename(test_proj$project_config_path)
  jsonFilename <- sub("\\.xlsx$", ".json", excelFilename)
  snapshotProjectConfiguration(test_proj$project_config_path, exportDir)
  jsonPath <- file.path(exportDir, jsonFilename)

  # Restore from JSON
  restoredConfig <- restoreProjectConfiguration(jsonPath, importDir)

  # Restored config should have modified=FALSE (freshly loaded from file)
  expect_false(restoredConfig$modified)
  expect_s3_class(restoredConfig, "ProjectConfiguration")
})

test_that("projectConfigurationStatus warns about modified ProjectConfiguration", {
  # Create a test project
  test_proj <- local_test_project()

  # Load and modify ProjectConfiguration
  projectConfig <- createProjectConfiguration(test_proj$project_config_path)
  suppressWarnings(projectConfig$modelFolder <- "modified/model/folder")
  expect_true(projectConfig$modified)

  # Check status with modified config - should show warning message
  expect_warning(
    status_result <- projectConfigurationStatus(
      projectConfig,
      test_proj$snapshot_path
    ),
    "The ProjectConfiguration object has been modified"
  )

  # Status should still work and return results
  expect_true(is.logical(status_result$in_sync))
  expect_true(is.list(status_result$details))
  expect_true(status_result$unsaved_changes)
})

test_that("projectConfigurationStatus does not warn about unmodified ProjectConfiguration", {
  # Create a test project
  test_proj <- local_test_project()

  # Load ProjectConfiguration without modifying
  projectConfig <- createProjectConfiguration(test_proj$project_config_path)
  expect_false(projectConfig$modified)

  # Create a JSON snapshot
  snapshotProjectConfiguration(test_proj$project_config_path)
  jsonPath <- sub("\\.xlsx$", ".json", test_proj$project_config_path)

  # Check status - should not show warning about modification
  expect_no_warning({
    status_result <- projectConfigurationStatus(projectConfig, jsonPath)
  })

  # Status should work normally
  expect_true(status_result$in_sync)
})

test_that("modifying ProjectConfiguration after snapshotting affects status check", {
  # Create a test project
  test_proj <- local_test_project()

  # Load ProjectConfiguration
  projectConfig <- createProjectConfiguration(test_proj$project_config_path)
  expect_false(projectConfig$modified)

  # Take snapshot of unmodified config
  snapshotProjectConfiguration(projectConfig)
  jsonPath <- sub("\\.xlsx$", ".json", test_proj$project_config_path)

  # Verify initial status is in sync
  status1 <- projectConfigurationStatus(projectConfig, jsonPath)
  expect_true(status1$in_sync)

  # Modify the configuration
  suppressWarnings(projectConfig$modelFolder <- "modified/model/folder")
  expect_true(projectConfig$modified)

  # Status check should now warn about modification

  expect_warning(status2 <- projectConfigurationStatus(projectConfig, jsonPath))
  # The warning is about the object being modified, not necessarily about sync status
  expect_true(status2$in_sync)
  expect_true(status2$unsaved_changes)
})

test_that("JSON roundtrip preserves modified flag behavior", {
  # Create a test project
  test_proj <- local_test_project()

  # Create export/import directories
  exportDir <- withr::local_tempdir("test_roundtrip_export")
  importDir <- withr::local_tempdir("test_roundtrip_import")

  # Load original ProjectConfiguration
  originalConfig <- createProjectConfiguration(test_proj$project_config_path)
  expect_false(originalConfig$modified)

  # Export to JSON
  excelFilename <- basename(test_proj$project_config_path)
  jsonFilename <- sub("\\.xlsx$", ".json", excelFilename)
  snapshotProjectConfiguration(originalConfig, exportDir)
  jsonPath <- file.path(exportDir, jsonFilename)

  # Restore from JSON
  restoredConfig <- restoreProjectConfiguration(jsonPath, importDir)
  expect_false(restoredConfig$modified)

  # Modify the restored config
  suppressWarnings(restoredConfig$dataFolder <- "new/data/folder")
  expect_true(restoredConfig$modified)

  # Save the modified config
  tempFile <- withr::local_tempfile(fileext = ".xlsx")
  restoredConfig$save(tempFile)
  expect_false(restoredConfig$modified)

  # Load from saved file
  finalConfig <- createProjectConfiguration(tempFile)
  expect_false(finalConfig$modified)
})

test_that("snapshotProjectConfiguration with path string handles modified flag correctly", {
  # Create a test project
  test_proj <- local_test_project()

  # Create output directory
  outputDir <- withr::local_tempdir("test_snapshot_path")

  # Snapshot using path string (not ProjectConfiguration object)
  expect_no_error({
    configData <- snapshotProjectConfiguration(
      test_proj$project_config_path,
      outputDir
    )
  })

  # Load the original file to check it wasn't affected
  projectConfig <- createProjectConfiguration(test_proj$project_config_path)
  expect_false(projectConfig$modified)

  # JSON file should be created
  excelFilename <- basename(test_proj$project_config_path)
  jsonFilename <- sub("\\.xlsx$", ".json", excelFilename)
  jsonPath <- file.path(outputDir, jsonFilename)
  expect_true(file.exists(jsonPath))
})

test_that("restoreProjectConfiguration aborts if user declines overwrite when files are not in sync", {
  # Set up test project and export/import dirs
  paths <- local_test_project()
  exportDir <- withr::local_tempdir("test_export_abort")
  importDir <- withr::local_tempdir("test_import_abort")

  # Export to JSON
  excelFilename <- basename(paths$project_config_path)
  jsonFilename <- sub("\\.xlsx$", ".json", excelFilename)
  snapshotProjectConfiguration(paths$project_config_path, exportDir)
  jsonPath <- file.path(exportDir, jsonFilename)

  # Create an out-of-sync Excel file in importDir
  file.copy(paths$project_config_path, file.path(importDir, excelFilename))
  # Modify the Excel file to make it out of sync
  df <- readExcel(file.path(importDir, excelFilename))
  df$Value[1] <- "MODIFIED_VALUE" # change a value
  .writeExcel(df, file.path(importDir, excelFilename))

  expect_warning(
    restoreProjectConfiguration(jsonPath, importDir)
  )
})
