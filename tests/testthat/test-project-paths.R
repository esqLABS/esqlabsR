test_that("project$modelFolder resolves a relative path against projectDirPath", {
  project <- esqlabsR:::.loadProjectJson(
    system.file(
      "extdata", "projects", "Example", "Project.json",
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
  project <- esqlabsR:::.loadProjectJson(
    system.file(
      "extdata", "projects", "Example", "Project.json",
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
  project <- esqlabsR:::.loadProjectJson(
    system.file(
      "extdata", "projects", "Example", "Project.json",
      package = "esqlabsR",
      mustWork = TRUE
    )
  )
  expect_equal(
    project$populationsFolder,
    fs::path_abs(file.path(project$projectDirPath, "Populations"))
  )
})

test_that("project active path fields are read-only", {
  project <- esqlabsR:::.loadProjectJson(
    system.file(
      "extdata", "projects", "Example", "Project.json",
      package = "esqlabsR",
      mustWork = TRUE
    )
  )
  expect_error(
    project$modelFolder <- "x",
    regexp = "modelFolder.*read-only"
  )
  expect_error(
    project$configurationsFolder <- "x",
    regexp = "configurationsFolder.*read-only"
  )
  expect_error(
    project$populationsFolder <- "x",
    regexp = "populationsFolder.*read-only"
  )
})

test_that(".clean_path expands env vars (other than PATH) and resolves to absolute", {
  project <- esqlabsR:::.loadProjectJson(
    system.file(
      "extdata", "projects", "Example", "Project.json",
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
  project <- esqlabsR:::.loadProjectJson(
    system.file(
      "extdata", "projects", "Example", "Project.json",
      package = "esqlabsR",
      mustWork = TRUE
    )
  )
  cp <- project$.__enclos_env__$private$.clean_path
  expect_null(cp(NULL, parent = NULL))
  expect_null(cp(NA_character_, parent = NULL))
  expect_null(cp(character(0), parent = NULL))
})
