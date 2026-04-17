test_that("isProjectInitialized correctly identifies project directories", {
  tempDir <- withr::local_tempdir(pattern = "test_project_check")

  # Should return FALSE for empty directory
  expect_false(isProjectInitialized(tempDir))

  # Should return TRUE when Project.xlsx exists
  initProject(destination = tempDir, overwrite = TRUE)
  expect_true(isProjectInitialized(tempDir))

  # Clean up and test with Configurations folder only
  unlink(file.path(tempDir, "Project.xlsx"))
  expect_true(isProjectInitialized(tempDir))
})

test_that("isProjectInitialized handles non-existent directories", {
  expect_false(isProjectInitialized("non_existent_directory"))
})

test_that("initProject with overwrite = TRUE doesn't ask for permission", {
  temp_dir <- withr::local_tempdir("test_init_overwrite")

  initProject(destination = temp_dir, overwrite = TRUE)
  expect_true(isProjectInitialized(temp_dir))

  # Initialize again with overwrite = TRUE — should not ask for permission
  initProject(destination = temp_dir, overwrite = TRUE)
  expect_true(isProjectInitialized(temp_dir))
})

test_that("initProject creates proper project structure", {
  temp_dir <- withr::local_tempdir("test_init_structure")

  initProject(destination = temp_dir, overwrite = TRUE)

  # JSON should exist (copied from Blank project template)
  expect_true(file.exists(file.path(temp_dir, "Project.json")))

  # Excel files should be generated from JSON
  expect_true(file.exists(file.path(temp_dir, "Project.xlsx")))
  expect_true(dir.exists(file.path(temp_dir, "Configurations")))

  # Directory structure should exist
  expect_true(dir.exists(file.path(temp_dir, "Models", "Simulations")))
  expect_true(dir.exists(file.path(temp_dir, "Data")))
  expect_true(dir.exists(file.path(temp_dir, "Results", "Figures")))
  expect_true(dir.exists(file.path(temp_dir, "Results", "SimulationResults")))
  expect_true(dir.exists(file.path(temp_dir, "Populations")))
})

test_that("initProject creates project from Blank template with no scenarios", {
  temp_dir <- withr::local_tempdir("test_init_blank")

  initProject(destination = temp_dir, overwrite = TRUE)

  # The generated project should be loadable
  pc <- loadProject(file.path(temp_dir, "Project.json"))
  expect_s3_class(pc, "Project")

  # Blank project should have no scenarios
  expect_equal(length(pc$scenarios), 0)
})

test_that("exampleProjectPath points to Example project", {
  path <- exampleProjectPath()
  expect_true(grepl("Example", path))
  expect_true(file.exists(path))
})
