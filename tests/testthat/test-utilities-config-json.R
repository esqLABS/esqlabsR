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
