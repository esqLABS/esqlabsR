test_that("Project$new() creates an empty in-memory project", {
  project <- Project$new()
  expect_s3_class(project, "Project")
  expect_null(project$projectFilePath)
  expect_null(project$projectDirPath)
  expect_false(project$modified)
  expect_false(project$validatedSinceMutation)
})

test_that("Project$new(path) loads a v2.0 JSON file", {
  project <- Project$new(
    testthat::test_path("data", "TestProject", "Project.json")
  )
  expect_s3_class(project, "Project")
  expect_equal(project$schemaVersion, "2.0")
  expect_false(project$modified)
})

test_that("Excel-bridge file fields can be set and clear modified flag accordingly", {
  project <- Project$new()
  expect_false(project$modified)
  project$modelParamsFile <- "X.xlsx"
  expect_true(project$modified)
})

test_that("asList round-trips with .projectToJson", {
  project <- testProject()
  expect_identical(project$asList, esqlabsR:::.projectToJson(project))
})

test_that("ProjectConfiguration() wrapper emits lifecycle warning and returns Project", {
  withr::local_options(lifecycle_verbosity = "warning")
  expect_warning(
    project <- ProjectConfiguration(
      testthat::test_path("data", "TestProject", "Project.json")
    ),
    class = "lifecycle_warning_deprecated"
  )
  expect_s3_class(project, "Project")
})
