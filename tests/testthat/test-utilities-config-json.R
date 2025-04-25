test_that("snapshotProjectConfiguration exports project configuration to JSON with correct filename", {
  # Set up test project using our fixture
  paths <- local_test_project()

  # Create output directory that will be automatically cleaned up
  output_dir <- withr::local_tempdir("test_export")

  # Get the expected JSON filename based on source Excel file
  excel_filename <- basename(paths$project_config_path)
  expected_json_filename <- sub("\\.xlsx$", ".json", excel_filename)
  expected_json_path <- file.path(output_dir, expected_json_filename)

  # Export to JSON
  config_data <- snapshotProjectConfiguration(
    paths$project_config_path,
    output_dir
  )

  # Check JSON content with snapshot
  expect_snapshot(config_data)

  # Check that JSON file with expected name exists
  expect_true(file.exists(expected_json_path))

  # Check JSON content in file with snapshot
  json_content <- jsonlite::fromJSON(expected_json_path)
  expect_snapshot(json_content)
})

test_that("restoreProjectConfiguration creates Excel files from JSON with correct filename", {
  # Set up test project using our fixture
  paths <- local_test_project()

  # Create output directories that will be automatically cleaned up
  export_dir <- withr::local_tempdir("test_export")
  import_dir <- withr::local_tempdir("test_import")

  # Export to JSON
  excel_filename <- basename(paths$project_config_path)
  json_filename <- sub("\\.xlsx$", ".json", excel_filename)

  config_data <- snapshotProjectConfiguration(
    paths$project_config_path,
    export_dir
  )

  # Get the actual JSON path
  json_path <- file.path(export_dir, json_filename)

  # Check JSON file exists
  expect_true(file.exists(json_path))

  # Import from JSON
  project_config <- restoreProjectConfiguration(json_path, import_dir)

  # Check that a ProjectConfiguration object is returned
  expect_s3_class(project_config, "ProjectConfiguration")

  # Expected Excel filename in import_dir should match the original Excel filename
  expected_excel_path <- file.path(import_dir, excel_filename)
  expect_true(file.exists(expected_excel_path))

  # Check configurations directory was created
  config_dir <- file.path(import_dir, "Configurations")
  expect_true(dir.exists(config_dir))

  # Check at least some Excel files were created
  excel_files <- list.files(config_dir, pattern = "\\.xlsx$")
  expect_true(length(excel_files) > 0)
})

test_that("Excel files in Configurations folder preserve content after JSON roundtrip", {
  # Set up test project using our fixture
  paths <- local_test_project()

  # Create output directories for export and import
  export_dir <- withr::local_tempdir("test_excel_content_export")
  import_dir <- withr::local_tempdir("test_excel_content_import")

  # Get the file mappings to test
  file_mapping <- list(
    modelParameters = "ModelParameters.xlsx",
    individuals = "Individuals.xlsx",
    populations = "Populations.xlsx",
    scenarios = "Scenarios.xlsx",
    applications = "Applications.xlsx",
    plots = "Plots.xlsx"
  )

  # Export to JSON
  excel_filename <- basename(paths$project_config_path)
  json_filename <- sub("\\.xlsx$", ".json", excel_filename)

  config_data <- snapshotProjectConfiguration(
    paths$project_config_path,
    export_dir
  )

  # Get the actual JSON path
  json_path <- file.path(export_dir, json_filename)

  # Import from JSON
  restoreProjectConfiguration(json_path, import_dir)

  # Configurations directory in the import directory
  config_dir <- file.path(import_dir, "Configurations")
  expect_true(dir.exists(config_dir))

  # Verify sheet names are preserved in each Excel file
  for (name in names(file_mapping)) {
    # Get paths to original and regenerated files
    original_path <- file.path(paths$configurations_dir, file_mapping[[name]])
    regenerated_path <- file.path(
      config_dir,
      file_mapping[[name]]
    )

    # Skip if original file doesn't exist
    if (!file.exists(original_path)) {
      next
    }

    # Verify the regenerated file exists
    expect_true(
      file.exists(regenerated_path),
      info = paste("Regenerated file not found:", regenerated_path)
    )

    # Get sheet names from both files
    original_sheets <- readxl::excel_sheets(original_path)
    regenerated_sheets <- readxl::excel_sheets(regenerated_path)

    # Check that both files have the same sheets
    expect_identical(
      original_sheets,
      regenerated_sheets,
      info = paste("Sheet names don't match for:", file_mapping[[name]])
    )

    # Check basic data structure is preserved
    for (sheet in original_sheets) {
      # Read sheets from both files
      original_data <- readExcel(original_path, sheet)
      regenerated_data <- readExcel(regenerated_path, sheet)

      # Check that both files have the same data
      expect_identical(original_data, regenerated_data)
    }
  }
})

test_that("ProjectConfiguration can be created from regenerated JSON files", {
  # Set up test project using our fixture
  paths <- local_test_project()

  # Get the original Excel filename
  excel_filename <- basename(paths$project_config_path)

  # Create output directories for export and import
  export_dir <- withr::local_tempdir("test_config_export")
  import_dir <- withr::local_tempdir("test_config_import")

  # Export to JSON
  json_filename <- sub("\\.xlsx$", ".json", excel_filename)
  config_data <- snapshotProjectConfiguration(
    paths$project_config_path,
    export_dir
  )
  json_path <- file.path(export_dir, json_filename)

  # Import from JSON
  restoreProjectConfiguration(json_path, import_dir)

  # Path to regenerated ProjectConfiguration.xlsx (in root directory, not in Configurations)
  regenerated_project_config_path <- file.path(
    import_dir,
    excel_filename
  )

  # Verify the file was created successfully
  expect_true(file.exists(regenerated_project_config_path))

  # Change working directory to output_dir to use relative paths
  old_wd <- getwd()
  setwd(import_dir)
  on.exit(setwd(old_wd), add = TRUE)

  # Try to create a ProjectConfiguration object with a relative path
  relative_path <- excel_filename
  expect_no_error({
    project_config <- createProjectConfiguration(relative_path)
  })

  # Verify the created object is of the correct class
  project_config <- createProjectConfiguration(relative_path)
  expect_s3_class(project_config, "ProjectConfiguration")
})

test_that("ProjectConfiguration.xlsx data is preserved in JSON round-trip", {
  # Set up test project using our fixture
  paths <- local_test_project()

  # Get the original Excel filename
  excel_filename <- basename(paths$project_config_path)

  # Create output directories for export and import
  export_dir <- withr::local_tempdir("test_data_export")
  import_dir <- withr::local_tempdir("test_data_import")

  # Export to JSON
  json_filename <- sub("\\.xlsx$", ".json", excel_filename)
  config_data <- snapshotProjectConfiguration(
    paths$project_config_path,
    export_dir
  )
  json_path <- file.path(export_dir, json_filename)

  # Import from JSON
  regenerated_config <- restoreProjectConfiguration(json_path, import_dir)

  # Check that a ProjectConfiguration object was returned
  expect_s3_class(regenerated_config, "ProjectConfiguration")

  # Check that ProjectConfiguration.xlsx was created with the same name as original
  expected_excel_path <- file.path(import_dir, excel_filename)
  expect_true(file.exists(expected_excel_path))

  # Read the original ProjectConfiguration.xlsx
  original_df <- readExcel(paths$project_config_path)

  # Read the regenerated ProjectConfiguration.xlsx to compare directly
  regenerated_df <- readExcel(expected_excel_path)

  # Check that both data frames are identical
  expect_identical(original_df, regenerated_df)
})
