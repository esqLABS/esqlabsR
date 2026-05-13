# Tests for the v2.0 Project.json parser. These tests cover only the parser
# and the internal `Project` class — the parser is not wired into runScenarios,
# validators, or plotting on this branch. Tests use `:::` because both are
# intentionally unexported.

test_that("loadProject() returns an internal Project from the bundled example", {
  project <- exampleProject()

  expect_s3_class(project, "Project")
  expect_s3_class(project, "R6")
  expect_identical(project$schemaVersion, "2.0")
  expect_identical(project$esqlabsRVersion, "6.0.0")
})

test_that("loadProject() captures jsonPath and projectDirPath", {
  path <- exampleProjectPath()
  project <- loadProject(path)

  expect_identical(
    normalizePath(project$jsonPath, winslash = "/"),
    normalizePath(path, winslash = "/")
  )
  expect_identical(project$projectDirPath, dirname(project$jsonPath))
})

test_that("loadProject() exposes filePaths verbatim", {
  project <- exampleProject()

  expect_type(project$filePaths, "list")
  expect_identical(project$filePaths$modelFolder, "Models/Simulations/")
  expect_identical(project$filePaths$configurationsFolder, "Configurations/")
  expect_identical(project$filePaths$dataFolder, "Data/")
})

test_that("loadProject() preserves outputPaths as a named list", {
  project <- exampleProject()

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

test_that("loadProject() parses scenarios into Scenario objects keyed by name", {
  project <- exampleProject()

  expect_type(project$scenarios, "list")
  expect_length(project$scenarios, 3L)
  expect_named(
    project$scenarios,
    c("Aciclovir_iv", "Aciclovir_iv_population", "Aciclovir_iv_steadystate")
  )

  first <- project$scenarios[["Aciclovir_iv"]]
  expect_s3_class(first, "Scenario")
  expect_identical(first$scenarioName, "Aciclovir_iv")
  expect_identical(first$individualId, "Adult_male")
  expect_null(first$populationId)
  expect_identical(first$modelParameterSets, c("Global", "Aciclovir"))
})

test_that("loadProject() preserves modelParameterSets as a named list of sets", {
  project <- exampleProject()

  expect_named(
    project$modelParameterSets,
    c("Global", "Aciclovir"),
    ignore.order = TRUE
  )
  expect_length(project$modelParameterSets$Global, 1L)
  expect_identical(
    project$modelParameterSets$Global[[1L]]$parameterName,
    "EHC continuous fraction"
  )
})

test_that("loadProject() preserves individualParameterSets as a named list of sets", {
  project <- exampleProject()

  expect_named(project$individualParameterSets, "Adult_male_default")
  expect_length(project$individualParameterSets$Adult_male_default, 1L)
  expect_identical(
    project$individualParameterSets$Adult_male_default[[1L]]$parameterName,
    "GFR"
  )
})

test_that("loadProject() preserves applicationParameterSets as a named list of sets", {
  project <- exampleProject()

  expect_named(project$applicationParameterSets, "Aciclovir_iv_250mg_default")
  expect_length(
    project$applicationParameterSets$Aciclovir_iv_250mg_default,
    1L
  )
  expect_identical(
    project$applicationParameterSets$Aciclovir_iv_250mg_default[[
      1L
    ]]$parameterName,
    "Dose"
  )
})

test_that("loadProject() preserves individuals as a named list keyed by individualId", {
  project <- exampleProject()

  expect_named(project$individuals, "Adult_male")
  ind <- project$individuals[["Adult_male"]]
  expect_s3_class(ind, "Individual")
  expect_identical(ind$gender, "MALE")
  expect_identical(ind$parameterSets, "Adult_male_default")
})

test_that("loadProject() preserves populations as a named list keyed by populationId", {
  project <- exampleProject()

  expect_named(project$populations, "European_adults")
  pop <- project$populations[["European_adults"]]
  expect_s3_class(pop, "Population")
  expect_identical(pop$numberOfIndividuals, 50)
})

test_that("loadProject() preserves applications as a named list keyed by protocol name", {
  project <- exampleProject()

  expect_named(project$applications, "Aciclovir_iv_250mg")
  app <- project$applications[["Aciclovir_iv_250mg"]]
  expect_s3_class(app, "Application")
  expect_identical(app$parameterSets, "Aciclovir_iv_250mg_default")
})

test_that("loadProject() preserves the observedData section", {
  project <- exampleProject()

  expect_length(project$observedData, 1L)
  source <- project$observedData[[1L]]
  expect_identical(source$type, "excel")
  expect_identical(source$file, "Aciclovir_TimeValuesData.xlsx")
  expect_identical(source$sheets, list("Laskin 1982.Group A"))
})

test_that("loadProject() parses plots into the asymmetric in-memory shape", {
  project <- exampleProject()

  expect_type(project$plots, "list")
  expect_named(
    project$plots,
    c("dataCombined", "plotConfiguration", "plotGrids"),
    ignore.order = TRUE
  )
  # dataCombined: named list keyed by name (no `name` field on entries)
  expect_named(project$plots$dataCombined, "Aciclovir_individual")
  dc <- project$plots$dataCombined$Aciclovir_individual
  expect_named(dc, c("simulated", "observed"), ignore.order = TRUE)
  expect_length(dc$simulated, 1L)
  expect_length(dc$observed, 1L)
  # plotConfiguration / plotGrids: data.frames
  expect_s3_class(project$plots$plotConfiguration, "data.frame")
  expect_s3_class(project$plots$plotGrids, "data.frame")
  expect_equal(nrow(project$plots$plotConfiguration), 1L)
  expect_equal(nrow(project$plots$plotGrids), 1L)
  expect_true("plotID" %in% names(project$plots$plotConfiguration))
  expect_true("name" %in% names(project$plots$plotGrids))
})

test_that(".parseNestedDataCombined re-keys by name and drops the name field", {
  raw <- list(
    list(name = "DC1", simulated = list(list(label = "a")), observed = list()),
    list(name = "DC2", simulated = list(), observed = list(list(label = "b")))
  )
  parsed <- esqlabsR:::.parseNestedDataCombined(raw)
  expect_named(parsed, c("DC1", "DC2"))
  expect_named(parsed$DC1, c("simulated", "observed"))
  expect_equal(parsed$DC1$simulated[[1]]$label, "a")
  expect_equal(parsed$DC2$observed[[1]]$label, "b")
})

test_that(".parseNestedDataCombined returns empty list for NULL or empty input", {
  expect_identical(esqlabsR:::.parseNestedDataCombined(NULL), list())
  expect_identical(esqlabsR:::.parseNestedDataCombined(list()), list())
})

test_that(".listOfListsToDataFrame pads missing fields with NA", {
  raw <- list(
    list(plotID = "P1", plotType = "individual", title = "T1"),
    list(plotID = "P2", plotType = "population")
  )
  df <- esqlabsR:::.listOfListsToDataFrame(raw)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2)
  expect_setequal(names(df), c("plotID", "plotType", "title"))
  expect_true(is.na(df$title[df$plotID == "P2"]))
})

test_that(".listOfListsToDataFrame returns empty data.frame for NULL or empty", {
  expect_equal(nrow(esqlabsR:::.listOfListsToDataFrame(NULL)), 0L)
  expect_equal(nrow(esqlabsR:::.listOfListsToDataFrame(list())), 0L)
})

test_that("loadProject() rejects a missing schemaVersion", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(list(esqlabsRVersion = "6.0.0"), tmp, auto_unbox = TRUE)

  expect_error(
    loadProject(tmp),
    "Unsupported schemaVersion"
  )
})

test_that("loadProject() rejects a non-2.0 schemaVersion", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(schemaVersion = "1.0", esqlabsRVersion = "5.0.0"),
    tmp,
    auto_unbox = TRUE
  )

  expect_error(
    loadProject(tmp),
    "Unsupported schemaVersion"
  )
})

test_that("loadProject() rejects a missing file", {
  expect_error(
    loadProject(tempfile(fileext = ".json")),
    "(does not exist|not found)"
  )
})

test_that("loadProject() rejects a non-string path", {
  expect_error(
    loadProject(NULL),
    "must be a single non-empty, non-NA string"
  )
  expect_error(
    loadProject(c("a.json", "b.json")),
    "must be a single non-empty, non-NA string"
  )
  expect_error(
    loadProject(""),
    "must be a single non-empty, non-NA string"
  )
})

test_that("loadProject() defaults missing optional sections to empty lists", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(schemaVersion = "2.0", esqlabsRVersion = "6.0.0"),
    tmp,
    auto_unbox = TRUE
  )

  project <- loadProject(tmp)

  expect_identical(project$filePaths, structure(list(), names = character(0L)))
  expect_identical(project$outputPaths, list())
  expect_identical(project$scenarios, list())
  expect_identical(project$modelParameterSets, list())
  expect_identical(project$individualParameterSets, list())
  expect_identical(project$applicationParameterSets, list())
  expect_identical(project$individuals, list())
  expect_identical(project$populations, list())
  expect_identical(
    project$applications,
    structure(list(), names = character(0L))
  )
  expect_identical(project$observedData, list())
  expect_null(project$plots)
})

test_that("Project lifecycle fields are read-only", {
  project <- exampleProject()

  expect_error(project$validatedSinceMutation <- TRUE, "readonly")
  expect_error(project$modified <- TRUE, "readonly")
})

test_that("Project$print() summarises section counts", {
  project <- exampleProject()

  expect_output(print(project), "<Project>")
  expect_output(print(project), "schema 2.0")
  expect_output(print(project), "scenarios:\\s+3")
  expect_output(print(project), "individuals:\\s+1")
  expect_output(print(project), "populations:\\s+1")
})
