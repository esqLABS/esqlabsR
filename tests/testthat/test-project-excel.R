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

test_that("exportProjectToExcel writes OutputPaths sheet with atomic columns", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(exampleProjectPath()), work_dir, recursive = TRUE)
  project <- loadProject(file.path(work_dir, "Example", "Project.json"))

  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)

  scenariosFile <- file.path(excel_out, "Configurations", "Scenarios.xlsx")
  outputPathsDf <- readxl::read_excel(scenariosFile, sheet = "OutputPaths")
  expect_named(outputPathsDf, c("OutputPathId", "OutputPath"))
})

test_that("Project round-trips through Excel preserving outputPaths", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(exampleProjectPath()), work_dir, recursive = TRUE)
  project <- loadProject(file.path(work_dir, "Example", "Project.json"))

  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)
  reimportedJson <- importProjectFromExcel(
    file.path(excel_out, "Project.xlsx"),
    silent = TRUE
  )
  reimported <- loadProject(reimportedJson)

  expect_equal(
    unlist(reimported$outputPaths),
    unlist(project$outputPaths)
  )
})

test_that("Project round-trips through Excel preserving modelParameterSets", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(exampleProjectPath()), work_dir, recursive = TRUE)
  project <- loadProject(file.path(work_dir, "Example", "Project.json"))

  expect_gt(length(project$modelParameterSets), 0)

  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)
  reimportedJson <- importProjectFromExcel(
    file.path(excel_out, "Project.xlsx"),
    silent = TRUE
  )
  reimported <- loadProject(reimportedJson)

  toJson <- function(x) {
    jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA)
  }
  expect_equal(
    toJson(reimported$modelParameterSets),
    toJson(project$modelParameterSets)
  )
})
