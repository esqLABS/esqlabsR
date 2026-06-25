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

test_that("Project round-trips initialConditions (path/value/unit) through Excel", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(exampleProjectPath()), work_dir, recursive = TRUE)
  project <- loadProject(file.path(work_dir, "Example", "Project.json"))
  project$initialConditions <- list(
    ICSet = list(
      list(path = "Organism|Liver|A", value = 0.5, unit = "µmol"),
      list(path = "Organism|Liver|B", value = 1.0, unit = "µmol")
    )
  )

  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)
  reimportedJson <- importProjectFromExcel(
    file.path(excel_out, "Project.xlsx"),
    silent = TRUE
  )
  reimported <- loadProject(reimportedJson)

  roundTripped <- reimported$initialConditions[["ICSet"]]
  expect_equal(
    vapply(roundTripped, \(e) e$path, character(1)),
    c("Organism|Liver|A", "Organism|Liver|B")
  )
  expect_equal(
    vapply(roundTripped, \(e) as.numeric(e$value), numeric(1)),
    c(0.5, 1.0)
  )
  expect_equal(
    vapply(roundTripped, \(e) e$unit, character(1)),
    c("µmol", "µmol")
  )
})

test_that("Import yields no initialConditions when the Excel side-car is absent", {
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

  expect_length(reimported$initialConditions, 0)
})
