# importProjectFromExcel ----

test_that("importProjectFromExcel creates v2.0 JSON from Excel files", {
  paths <- local_test_project()

  outputDir <- withr::local_tempdir("test_import")

  jsonPath <- importProjectFromExcel(
    paths$project_config_path,
    outputDir,
    silent = TRUE
  )

  expect_true(file.exists(jsonPath))

  jsonData <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  expect_equal(jsonData$schemaVersion, "2.0")
  expect_true("filePaths" %in% names(jsonData))
  expect_true("scenarios" %in% names(jsonData))
  expect_true("modelParameters" %in% names(jsonData))
  expect_true("individuals" %in% names(jsonData))
  expect_true("populations" %in% names(jsonData))
  expect_true("applications" %in% names(jsonData))
  expect_true("outputPaths" %in% names(jsonData))
  expect_true("plots" %in% names(jsonData))

  # Check v2.0 schema structure
  expect_true(is.list(jsonData$scenarios))
  expect_true(length(jsonData$scenarios) > 0)
  expect_true(!is.null(jsonData$scenarios[[1]]$name))

  # modelParameters should be named lists of arrays
  expect_true(length(jsonData$modelParameters) > 0)
  firstSheet <- jsonData$modelParameters[[1]]
  expect_true(is.list(firstSheet))
  expect_true(!is.null(firstSheet[[1]]$containerPath))
})

test_that("importProjectFromExcel creates JSON in source dir by default", {
  paths <- local_test_project()

  jsonPath <- importProjectFromExcel(
    paths$project_config_path,
    silent = TRUE
  )

  expectedPath <- sub("\\.xlsx$", ".json", paths$project_config_path)
  expect_equal(normalizePath(jsonPath), normalizePath(expectedPath))
  expect_true(file.exists(jsonPath))
})

test_that("importProjectFromExcel JSON can be loaded by Project", {
  paths <- local_test_project()

  outputDir <- withr::local_tempdir("test_loadable")

  jsonPath <- importProjectFromExcel(
    paths$project_config_path,
    outputDir,
    silent = TRUE
  )

  expect_no_error({
    project <- loadProject(jsonPath)
  })

  expect_s3_class(project, "Project")
  expect_true(length(project$scenarios) > 0)
  expect_true(length(project$modelParameters) > 0)
  expect_true(length(project$individuals) > 0)
})

test_that("importProjectFromExcel does not merge bundled species parameters into modelParameters", {
  temp_project <- local_test_project()
  jsonPath <- importProjectFromExcel(
    temp_project$project_config_path,
    silent = TRUE
  )
  jsonData <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)

  speciesFile <- system.file(
    "extdata",
    "SpeciesParameters.xlsx",
    package = "esqlabsR"
  )
  speciesSheets <- readxl::excel_sheets(speciesFile)

  # No bundled species sheet should appear in modelParameters
  speciesInModel <- intersect(speciesSheets, names(jsonData$modelParameters))
  expect_length(speciesInModel, 0)
})

test_that("importProjectFromExcel handles PK-Sim exported population CSV files with metadata comment rows", {
  test_proj <- local_test_project()

  # Create PopulationsCSV directory if needed
  pop_csv_dir <- file.path(test_proj$configurations_dir, "PopulationsCSV")
  if (!dir.exists(pop_csv_dir)) {
    dir.create(pop_csv_dir, recursive = TRUE)
  }

  # Add a PK-Sim format CSV (with # metadata comment rows)
  pksim_csv_path <- file.path(pop_csv_dir, "PKSimPopulation.csv")

  # Create PK-Sim format CSV with metadata comment rows
  pksim_csv_lines <- c(
    "#Project: TestProject_V1",
    "#PK-Sim version: 12.1.222",
    "IndividualId,Gender,Organism|Weight",
    "1,Male,70",
    "2,Female,60"
  )
  writeLines(pksim_csv_lines, pksim_csv_path)

  # Import should succeed without error
  outputDir <- withr::local_tempdir("test_pksim_csv")
  expect_no_error({
    jsonPath <- importProjectFromExcel(
      test_proj$project_config_path,
      outputDir,
      silent = TRUE
    )
  })
})

# exportProjectToExcel ----

test_that("exportProjectToExcel creates Excel files from Project", {
  project <- testProject()

  outputDir <- withr::local_tempdir("test_export")

  projConfigPath <- exportProjectToExcel(
    project,
    outputDir = outputDir,
    silent = TRUE
  )

  # Check that Project.xlsx was created
  expect_true(file.exists(projConfigPath))

  # Check configurations directory was created
  configDir <- file.path(outputDir, "Configurations")
  expect_true(dir.exists(configDir))

  # Check key Excel files were created
  expect_true(file.exists(file.path(configDir, "ModelParameters.xlsx")))
  expect_true(file.exists(file.path(configDir, "Scenarios.xlsx")))
  expect_true(file.exists(file.path(configDir, "Individuals.xlsx")))
  expect_true(file.exists(file.path(configDir, "Applications.xlsx")))
})

test_that("exportProjectToExcel works with plain-data individuals and scenarios", {
  project <- testProject()
  tempDir <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = tempDir, silent = TRUE)

  # Check that key files were created
  expect_true(file.exists(file.path(
    tempDir,
    "Configurations",
    "Scenarios.xlsx"
  )))
  expect_true(file.exists(file.path(
    tempDir,
    "Configurations",
    "Individuals.xlsx"
  )))
})

test_that("exportProjectToExcel preserves model parameters", {
  project <- testProject()

  outputDir <- withr::local_tempdir("test_export_params")
  exportProjectToExcel(project, outputDir = outputDir, silent = TRUE)

  # Read back the ModelParameters.xlsx
  paramsFile <- file.path(outputDir, "Configurations", "ModelParameters.xlsx")
  sheets <- readxl::excel_sheets(paramsFile)
  expect_true("Global" %in% sheets)

  globalDf <- readExcel(paramsFile, sheet = "Global")
  expect_true("Container Path" %in% names(globalDf))
  expect_true("Parameter Name" %in% names(globalDf))
  expect_true("Value" %in% names(globalDf))
})

# projectStatus ----

test_that("projectStatus correctly handles missing JSON file", {
  test_proj <- local_test_project()

  json_path <- file.path(test_proj$dir, "NonExistent.json")
  expect_error(
    projectStatus(test_proj$project_config_path, json_path),
    "JSON file does not exist"
  )
})

test_that("projectStatus detects in-sync state after fresh import", {
  test_proj <- local_test_project()

  # Import fresh JSON from Excel
  importProjectFromExcel(
    test_proj$project_config_path,
    silent = TRUE
  )

  jsonPath <- sub("\\.xlsx$", ".json", test_proj$project_config_path)
  expect_true(file.exists(jsonPath))

  status_result <- projectStatus(
    test_proj$project_config_path,
    jsonPath,
    silent = TRUE
  )
  expect_true(status_result$in_sync)
})

test_that("projectStatus detects out-of-sync state when JSON is modified", {
  test_proj <- local_test_project()

  # Import fresh JSON from Excel to ensure a baseline in-sync state
  importProjectFromExcel(
    test_proj$project_config_path,
    silent = TRUE
  )

  jsonPath <- sub("\\.xlsx$", ".json", test_proj$project_config_path)
  expect_true(file.exists(jsonPath))

  # Modify the JSON to make it out of sync with Excel
  jsonData <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  original_name <- jsonData$scenarios[[1]]$name
  jsonData$scenarios[[1]]$name <- paste0(original_name, "_MODIFIED")
  writeLines(
    jsonlite::toJSON(jsonData, auto_unbox = TRUE, pretty = TRUE),
    jsonPath
  )

  status_result <- projectStatus(
    test_proj$project_config_path,
    jsonPath,
    silent = TRUE
  )
  expect_false(status_result$in_sync)
})

# Deprecated wrappers ----

test_that("snapshotProjectConfiguration emits deprecation warning", {
  paths <- local_test_project()
  outputDir <- withr::local_tempdir("test_deprecated_snapshot")

  lifecycle::expect_deprecated(
    snapshotProjectConfiguration(
      paths$project_config_path,
      outputDir,
      silent = TRUE
    )
  )
})

test_that("restoreProjectConfiguration emits deprecation warning", {
  paths <- local_test_project()

  lifecycle::expect_deprecated(
    restoreProjectConfiguration(
      paths$snapshot_path,
      outputDir = withr::local_tempdir("test_deprecated_restore"),
      silent = TRUE
    )
  )
})

test_that("createProjectConfiguration emits deprecation warning", {
  project <- testProject()
  lifecycle::expect_deprecated(
    createProjectConfiguration(project$projectFilePath)
  )
})

# Internal helpers ----

test_that(".parseCommaListToArray handles values containing apostrophes", {
  expect_equal(
    .parseCommaListToArray("5'-Reductase, CYP3A4"),
    c("5'-Reductase", "CYP3A4")
  )
})

test_that(".parseCommaListToArray returns NULL on empty/NA input", {
  expect_null(.parseCommaListToArray(NULL))
  expect_null(.parseCommaListToArray(NA))
  expect_null(.parseCommaListToArray(""))
})

test_that(".parseCommaListToArray trims whitespace", {
  expect_equal(.parseCommaListToArray(" a , b ,c"), c("a", "b", "c"))
})

test_that(".naToNull leaves multi-element vectors unchanged", {
  expect_equal(.naToNull(c("a", "b")), c("a", "b"))
  expect_equal(.naToNull(c(1, NA)), c(1, NA))
})

test_that(".naToNull returns NULL for scalar NA", {
  expect_null(.naToNull(NA))
  expect_null(.naToNull(NA_character_))
  expect_null(.naToNull(NA_real_))
})
