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
test_that("project$modelFolder resolves a relative path against projectDirPath", {
  project <- loadProject(
    system.file(
      "extdata",
      "projects",
      "Example",
      "Project.json",
      package = "esqlabsR",
      mustWork = TRUE
    )
  )
  expect_equal(
    project$modelFolder,
    fs::path_abs(file.path(project$projectDirPath, "Models/Simulations"))
  )
})

test_that("project$configurationsFolder resolves a relative path against projectDirPath", {
  project <- loadProject(
    system.file(
      "extdata",
      "projects",
      "Example",
      "Project.json",
      package = "esqlabsR",
      mustWork = TRUE
    )
  )
  expect_equal(
    project$configurationsFolder,
    fs::path_abs(file.path(project$projectDirPath, "Configurations"))
  )
})

test_that("project$populationsFolder resolves relative to projectDirPath", {
  project <- loadProject(
    system.file(
      "extdata",
      "projects",
      "Example",
      "Project.json",
      package = "esqlabsR",
      mustWork = TRUE
    )
  )
  expect_equal(
    project$populationsFolder,
    fs::path_abs(file.path(project$projectDirPath, "Populations"))
  )
})

test_that("project$dataFolder resolves relative to projectDirPath", {
  project <- loadProject(
    system.file(
      "extdata",
      "projects",
      "Example",
      "Project.json",
      package = "esqlabsR",
      mustWork = TRUE
    )
  )
  expect_equal(
    project$dataFolder,
    fs::path_abs(file.path(project$projectDirPath, "Data"))
  )
})

test_that("project$dataFolder is NULL when filePaths.dataFolder is unset", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(
      schemaVersion = "2.0",
      filePaths = structure(list(), names = character(0)),
      outputPaths = structure(list(), names = character(0)),
      scenarios = list()
    ),
    tmp,
    auto_unbox = TRUE,
    null = "null"
  )
  project <- loadProject(tmp)
  expect_null(project$dataFolder)
})

test_that("project path fields are writable after the merger", {
  project <- loadProject(
    system.file(
      "extdata",
      "projects",
      "Example",
      "Project.json",
      package = "esqlabsR",
      mustWork = TRUE
    )
  )
  expect_false(project$modified)
  project$modelFolder <- "AnotherModels"
  expect_true(project$modified)
})

test_that(".clean_path expands env vars (other than PATH) and resolves to absolute", {
  project <- loadProject(
    system.file(
      "extdata",
      "projects",
      "Example",
      "Project.json",
      package = "esqlabsR",
      mustWork = TRUE
    )
  )
  withr::local_envvar(MY_TEST_ROOT = tempdir())
  resolved <- project$.__enclos_env__$private$.clean_path(
    "$MY_TEST_ROOT/sub",
    parent = NULL
  )
  expect_equal(
    resolved,
    fs::path_abs(file.path(tempdir(), "sub"))
  )
})

test_that(".clean_path returns NULL on NULL/NA/zero-length input", {
  project <- loadProject(
    system.file(
      "extdata",
      "projects",
      "Example",
      "Project.json",
      package = "esqlabsR",
      mustWork = TRUE
    )
  )
  cp <- project$.__enclos_env__$private$.clean_path
  expect_null(cp(NULL, parent = NULL))
  expect_null(cp(NA_character_, parent = NULL))
  expect_null(cp(character(0), parent = NULL))
})


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
  # plotConfiguration is a 1-row, 15-column data frame: report 1 plot,
  # not the column count.
  expect_output(
    print(project),
    "1 dataCombined / 1 plot\\(s\\) / 1 grid\\(s\\)"
  )
})

test_that(".markSaved clears modified but leaves validatedSinceMutation set", {
  project <- testProject()
  # A mutation marks the project modified and clears the validation flag.
  project$.markModified()
  expect_true(project$modified)

  # Validating sets the flag; a subsequent save must not clear it.
  project$.markValidated()
  project$.markSaved()

  expect_false(project$modified)
  # Saving is not a mutation, so a project validated before the save stays
  # validated after it; runScenarios()/createPlots() need not re-validate.
  expect_true(project$validatedSinceMutation)
})

test_that("a fresh project starts unmodified and unvalidated", {
  project <- testProject()
  expect_false(project$modified)
  expect_false(project$validatedSinceMutation)
})

test_that("a direct write to a section field invalidates the project", {
  project <- testProject()
  project$.markValidated()
  expect_false(project$modified)
  expect_true(project$validatedSinceMutation)

  project$scenarios[["New"]] <- Scenario(
    scenarioName = "New",
    modelFile = "m.pkml"
  )

  expect_true(project$modified)
  expect_false(project$validatedSinceMutation)
})

test_that("the documented c() attach idiom invalidates the project", {
  project <- testProject()
  scenarios <- list(Attached = Scenario(scenarioName = "Attached"))

  project$scenarios <- c(project$scenarios, scenarios)

  expect_true(project$modified)
  expect_s3_class(project$scenarios[["Attached"]], "Scenario")
})

test_that("nested subscript-assignment on a section entry invalidates the project", {
  project <- testProject()
  project$individuals[["Indiv1"]]$weight <- 81

  expect_true(project$modified)
  expect_identical(project$individuals[["Indiv1"]]$weight, 81)
})

test_that("an extracted Scenario is a copy and cannot mutate the project silently", {
  project <- testProject()
  sc <- project$scenarios[["TestScenario"]]
  sc$modelFile <- "HIJACKED.pkml"

  expect_false(project$modified)
  expect_false(
    identical(project$scenarios[["TestScenario"]]$modelFile, "HIJACKED.pkml")
  )
})

test_that("jsonPath is read-only and aliases projectFilePath", {
  project <- testProject()
  expect_identical(project$jsonPath, project$projectFilePath)
  expect_snapshot(error = TRUE, project$jsonPath <- "elsewhere.json")
})

test_that("sync() reports a direct section-field write as unsaved changes", {
  tmp <- withr::local_tempfile(fileext = ".json")
  saveProject(testProject(), tmp)
  project <- loadProject(tmp)

  project$scenarios[["TestScenario"]]$modelFile <- "Changed.pkml"

  status <- project$sync(silent = TRUE)
  expect_true(status$unsaved_changes)
  expect_false(status$json_modified)
  expect_false(status$in_sync)
})

# Clone safety ----

test_that("mutating a clone's scenario leaves the source untouched", {
  project <- testProject()
  clone <- project$clone()

  clone$scenarios[["TestScenario"]]$modelFile <- "OnlyOnClone.pkml"

  expect_identical(
    clone$scenarios[["TestScenario"]]$modelFile,
    "OnlyOnClone.pkml"
  )
  expect_false(
    identical(project$scenarios[["TestScenario"]]$modelFile, "OnlyOnClone.pkml")
  )
})

test_that("adding a scenario to a clone leaves the source untouched", {
  project <- testProject()
  clone <- project$clone()
  before <- length(project$scenarios)

  clone$scenarios[["Fresh"]] <- Scenario(scenarioName = "Fresh")

  expect_length(project$scenarios, before)
  expect_false("Fresh" %in% names(project$scenarios))
})

test_that("mutating a clone's nested individual entry leaves the source untouched", {
  project <- testProject()
  clone <- project$clone()

  clone$individuals[["Indiv1"]]$weight <- 99

  expect_identical(clone$individuals[["Indiv1"]]$weight, 99)
  expect_false(identical(project$individuals[["Indiv1"]]$weight, 99))
})

test_that("clone modified/validated flags are independent of the source", {
  project <- testProject()
  project$.markValidated()
  clone <- project$clone()

  clone$scenarios[["TestScenario"]]$modelFile <- "Changed.pkml"

  expect_true(clone$modified)
  expect_false(clone$validatedSinceMutation)
  expect_false(project$modified)
  expect_true(project$validatedSinceMutation)
})
