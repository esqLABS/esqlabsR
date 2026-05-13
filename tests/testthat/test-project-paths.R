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
    parent = NULL,
    must_work = FALSE
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

test_that("exampleProjectPath returns an existing Project.json", {
  path <- exampleProjectPath()
  expect_type(path, "character")
  expect_match(path, "Project\\.json$")
  expect_true(file.exists(path))
})
