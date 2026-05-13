test_that("isProjectInitialized correctly identifies project directories", {
  tempDir <- withr::local_tempdir(pattern = "test_project_check")

  expect_false(isProjectInitialized(tempDir))

  initProject(destination = tempDir, overwrite = TRUE)
  expect_true(isProjectInitialized(tempDir))

  unlink(file.path(tempDir, "Project.json"))
  expect_true(isProjectInitialized(tempDir))
})

test_that("isProjectInitialized handles non-existent directories", {
  # Should return FALSE for non-existent directory
  expect_false(isProjectInitialized("non_existent_directory"))
})

test_that("initProject with overwrite = TRUE doesn't ask for permission", {
  temp_project <- with_temp_project()

  expect_true(isProjectInitialized(temp_project$path))

  initProject(
    destination = temp_project$path,
    type = "example",
    overwrite = TRUE
  )
  expect_true(isProjectInitialized(temp_project$path))
})

test_that("initProject creates proper project structure", {
  temp_project <- with_temp_project()

  expect_true(file.exists(file.path(temp_project$path, "Project.json")))
  expect_true(file.exists(file.path(temp_project$path, "Project.xlsx")))
  expect_true(dir.exists(file.path(temp_project$path, "Configurations")))
  expect_true(dir.exists(file.path(temp_project$path, "Models")))
  expect_true(dir.exists(file.path(temp_project$path, "Data")))
  expect_true(dir.exists(file.path(temp_project$path, "Results")))
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
  expect_s3_class(temp_project$project, "Project")
})
