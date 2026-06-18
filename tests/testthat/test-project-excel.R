test_that("snapshotProjectConfiguration warns with lifecycle_warning_deprecated", {
  withr::local_options(lifecycle_verbosity = "warning")
  expect_warning(
    tryCatch(
      snapshotProjectConfiguration(
        "nonexistent.xlsx",
        outputDir = withr::local_tempdir(),
        silent = TRUE
      ),
      error = function(e) NULL
    ),
    class = "lifecycle_warning_deprecated"
  )
})

test_that("restoreProjectConfiguration warns with lifecycle_warning_deprecated", {
  withr::local_options(lifecycle_verbosity = "warning")
  expect_warning(
    tryCatch(
      restoreProjectConfiguration(
        "nonexistent.json",
        outputDir = withr::local_tempdir(),
        silent = TRUE
      ),
      error = function(e) NULL
    ),
    class = "lifecycle_warning_deprecated"
  )
})

test_that("projectConfigurationStatus warns with lifecycle_warning_deprecated", {
  withr::local_options(lifecycle_verbosity = "warning")
  expect_warning(
    tryCatch(
      projectConfigurationStatus("nonexistent.xlsx"),
      error = function(e) NULL
    ),
    class = "lifecycle_warning_deprecated"
  )
})
