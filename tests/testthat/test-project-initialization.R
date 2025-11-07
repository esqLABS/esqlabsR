test_that("isProjectInitialized correctly identifies project directories", {
  # Create temporary directory for testing
  tempDir <- withr::local_tempdir(pattern = "test_project_check")

  # Should return FALSE for empty directory
  expect_false(isProjectInitialized(tempDir))

  # Should return TRUE when ProjectConfiguration.xlsx exists
  initProject(destination = tempDir, overwrite = TRUE)
  expect_true(isProjectInitialized(tempDir))

  # Clean up and test with Configurations folder only
  unlink(file.path(tempDir, "ProjectConfiguration.xlsx"))
  expect_true(isProjectInitialized(tempDir))
})

test_that("isProjectInitialized handles non-existent directories", {
  # Should return FALSE for non-existent directory
  expect_false(isProjectInitialized("non_existent_directory"))
})

test_that("initProject with overwrite = TRUE doesn't ask for permission", {
  # Create temporary project using helper function
  temp_project <- with_temp_project()

  # The project should already be initialized
  expect_true(isProjectInitialized(temp_project$path))

  # Initialize again with overwrite = TRUE - should not ask for permission
  initProject(destination = temp_project$path, overwrite = TRUE)
  expect_true(isProjectInitialized(temp_project$path))
})

test_that("initProject creates proper project structure", {
  # Create temporary project using helper function
  temp_project <- with_temp_project()

  # Check that project structure was created
  expect_true(file.exists(file.path(
    temp_project$path,
    "ProjectConfiguration.xlsx"
  )))
  expect_true(dir.exists(file.path(temp_project$path, "Configurations")))
  expect_true(dir.exists(file.path(temp_project$path, "Models")))
  expect_true(dir.exists(file.path(temp_project$path, "Data")))
  expect_true(dir.exists(file.path(temp_project$path, "Results")))

  # Check that configuration files exist
  expect_true(file.exists(file.path(
    temp_project$path,
    "Configurations",
    "ModelParameters.xlsx"
  )))
  expect_true(file.exists(file.path(
    temp_project$path,
    "Configurations",
    "Individuals.xlsx"
  )))
  expect_true(file.exists(file.path(
    temp_project$path,
    "Configurations",
    "Applications.xlsx"
  )))
  expect_true(file.exists(file.path(
    temp_project$path,
    "Configurations",
    "Scenarios.xlsx"
  )))
  expect_true(file.exists(file.path(
    temp_project$path,
    "Configurations",
    "Plots.xlsx"
  )))
  expect_true(file.exists(file.path(
    temp_project$path,
    "Configurations",
    "Populations.xlsx"
  )))
})
