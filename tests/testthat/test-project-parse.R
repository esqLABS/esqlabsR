# Tests for the v2.0 Project.json parser. These tests cover only the parser
# and the internal `Project` class — the parser is not wired into runScenarios,
# validators, or plotting on this branch. Tests use `:::` because both are
# intentionally unexported.

example_project_json_path <- function() {
  system.file(
    "extdata",
    "projects",
    "Example",
    "Project.json",
    package = "esqlabsR",
    mustWork = TRUE
  )
}

test_that(".loadProjectJson() returns an internal Project from the bundled example", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())

  expect_s3_class(project, "Project")
  expect_s3_class(project, "R6")
  expect_identical(project$schemaVersion, "2.0")
  expect_identical(project$esqlabsRVersion, "6.0.0")
})

test_that(".loadProjectJson() captures jsonPath and projectDirPath", {
  path <- example_project_json_path()
  project <- esqlabsR:::.loadProjectJson(path)

  expect_identical(
    normalizePath(project$jsonPath, winslash = "/"),
    normalizePath(path, winslash = "/")
  )
  expect_identical(project$projectDirPath, dirname(project$jsonPath))
})

test_that(".loadProjectJson() exposes filePaths verbatim", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())

  expect_type(project$filePaths, "list")
  expect_identical(project$filePaths$modelFolder, "Models/Simulations/")
  expect_identical(project$filePaths$configurationsFolder, "Configurations/")
  expect_identical(project$filePaths$dataFolder, "Data/")
})

test_that(".loadProjectJson() preserves outputPaths as a named list", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())

  expect_type(project$outputPaths, "list")
  expect_named(
    project$outputPaths,
    c("Aciclovir_PVB", "Aciclovir_fat_cell"),
    ignore.order = TRUE
  )
  expect_identical(
    project$outputPaths$Aciclovir_PVB,
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  )
})

test_that(".loadProjectJson() preserves scenarios as a list of named lists", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())

  expect_type(project$scenarios, "list")
  expect_length(project$scenarios, 3L)

  first <- project$scenarios[[1L]]
  expect_identical(first$name, "Aciclovir_iv")
  expect_identical(first$individualId, "Adult_male")
  expect_null(first$populationId)
  expect_identical(first$modelParameters, list("Global", "Aciclovir"))
  expect_identical(first$outputPathIds, list("Aciclovir_PVB"))
})

test_that(".loadProjectJson() preserves modelParameters as a named list of sets", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())

  expect_named(
    project$modelParameters,
    c("Global", "Aciclovir"),
    ignore.order = TRUE
  )
  expect_length(project$modelParameters$Global, 1L)
  expect_identical(
    project$modelParameters$Global[[1L]]$parameterName,
    "EHC continuous fraction"
  )
})

test_that(".loadProjectJson() preserves individuals with inline parameters", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())

  expect_length(project$individuals, 1L)
  ind <- project$individuals[[1L]]
  expect_identical(ind$individualId, "Adult_male")
  expect_identical(ind$gender, "MALE")
  expect_length(ind$parameters, 1L)
  expect_identical(ind$parameters[[1L]]$parameterName, "GFR")
})

test_that(".loadProjectJson() preserves populations as a list of named lists", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())

  expect_length(project$populations, 1L)
  pop <- project$populations[[1L]]
  expect_identical(pop$populationId, "European_adults")
  expect_identical(pop$numberOfIndividuals, 50L)
})

test_that(".loadProjectJson() preserves applications as a named list keyed by protocol name", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())

  expect_named(project$applications, "Aciclovir_iv_250mg")
  expect_length(project$applications$Aciclovir_iv_250mg$parameters, 1L)
})

test_that(".loadProjectJson() preserves the observedData section", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())

  expect_length(project$observedData, 1L)
  source <- project$observedData[[1L]]
  expect_identical(source$type, "excel")
  expect_identical(source$file, "Aciclovir_TimeValuesData.xlsx")
  expect_identical(source$sheets, list("Laskin 1982.Group A"))
})

test_that(".loadProjectJson() preserves the plots section as a nested list", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())

  expect_type(project$plots, "list")
  expect_named(
    project$plots,
    c("dataCombined", "plotConfiguration", "plotGrids"),
    ignore.order = TRUE
  )
  expect_length(project$plots$dataCombined, 1L)
  expect_length(project$plots$plotConfiguration, 1L)
  expect_length(project$plots$plotGrids, 1L)

  dc <- project$plots$dataCombined[[1L]]
  expect_identical(dc$name, "Aciclovir_individual")
  expect_length(dc$simulated, 1L)
  expect_length(dc$observed, 1L)
})

test_that(".loadProjectJson() rejects a missing schemaVersion", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(list(esqlabsRVersion = "6.0.0"), tmp, auto_unbox = TRUE)

  expect_error(
    esqlabsR:::.loadProjectJson(tmp),
    "Unsupported schemaVersion"
  )
})

test_that(".loadProjectJson() rejects a non-2.0 schemaVersion", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(schemaVersion = "1.0", esqlabsRVersion = "5.0.0"),
    tmp,
    auto_unbox = TRUE
  )

  expect_error(
    esqlabsR:::.loadProjectJson(tmp),
    "Unsupported schemaVersion"
  )
})

test_that(".loadProjectJson() rejects a missing file", {
  expect_error(
    esqlabsR:::.loadProjectJson(tempfile(fileext = ".json")),
    "does not exist"
  )
})

test_that(".loadProjectJson() rejects a non-string path", {
  expect_error(
    esqlabsR:::.loadProjectJson(NULL),
    "must be a single non-NA string"
  )
  expect_error(
    esqlabsR:::.loadProjectJson(c("a.json", "b.json")),
    "must be a single non-NA string"
  )
})

test_that(".loadProjectJson() defaults missing optional sections to empty lists", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(schemaVersion = "2.0", esqlabsRVersion = "6.0.0"),
    tmp,
    auto_unbox = TRUE
  )

  project <- esqlabsR:::.loadProjectJson(tmp)

  expect_identical(project$filePaths, list())
  expect_identical(project$outputPaths, list())
  expect_identical(project$scenarios, list())
  expect_identical(project$modelParameters, list())
  expect_identical(project$individuals, list())
  expect_identical(project$populations, list())
  expect_identical(project$applications, list())
  expect_identical(project$observedData, list())
  expect_null(project$plots)
})

test_that("Project active fields are read-only", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())

  expect_error(project$schemaVersion <- "9.9", "read-only")
  expect_error(project$jsonPath <- "/tmp/elsewhere.json", "read-only")
  expect_error(project$scenarios <- list(), "read-only")
})

test_that("Project$print() summarises section counts", {
  project <- esqlabsR:::.loadProjectJson(example_project_json_path())

  expect_output(print(project), "<Project>")
  expect_output(print(project), "schema 2.0")
  expect_output(print(project), "scenarios:\\s+3")
  expect_output(print(project), "individuals:\\s+1")
  expect_output(print(project), "populations:\\s+1")
})
