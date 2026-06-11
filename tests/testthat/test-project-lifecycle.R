test_that("loadProject() returns a Project from a valid Project.json", {
  project <- testProject()
  expect_s3_class(project, "Project")
  expect_equal(project$schemaVersion, "2.0")
  expect_equal(length(project$scenarios), 4)
})

test_that("loadProject() errors when the file does not exist", {
  expect_error(
    loadProject(file.path(tempdir(), "does_not_exist.json")),
    regexp = "(does not exist|not found)"
  )
})

test_that("loadProject() errors on an unsupported schemaVersion", {
  badPath <- withr::local_tempfile(fileext = ".json")
  writeLines(
    '{"schemaVersion": "1.0", "filePaths": {}}',
    badPath
  )
  expect_error(
    loadProject(badPath),
    regexp = "Unsupported schemaVersion"
  )
})

test_that("loadProject() warns on a dangling cross-reference", {
  path <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(
      schemaVersion = "2.0",
      esqlabsRVersion = "6.0.0",
      filePaths = structure(list(), names = character(0)),
      observedData = list(),
      outputPaths = list(O1 = "Organism|A|Concentration in container"),
      scenarios = list(list(
        name = "S",
        individualId = "Ghost",
        modelFile = "m.pkml",
        outputPathIds = list("O1")
      )),
      individuals = structure(list(), names = character(0)),
      populations = structure(list(), names = character(0)),
      applications = structure(list(), names = character(0))
    ),
    path,
    auto_unbox = TRUE,
    null = "null"
  )

  expect_warning(
    loadProject(path),
    "unresolved cross-reference.*Ghost"
  )
})

test_that("loadProject() loads a clean project without a cross-reference warning", {
  expect_no_warning(testProject())
})

test_that("saveProject writes a Project to disk and clears modified flag", {
  project <- testProject()
  project$modelFolder <- "AnotherModels"
  expect_true(project$modified)

  tmp <- withr::local_tempfile(fileext = ".json")
  saveProject(project, tmp)
  expect_true(file.exists(tmp))
  expect_false(project$modified)
})

test_that("saveProject defaults to project$jsonPath when path is NULL", {
  tmp_src <- withr::local_tempfile(fileext = ".json")
  project <- testProject()
  saveProject(project, tmp_src)
  reloaded <- loadProject(tmp_src)
  reloaded$modelFolder <- "Models2"
  saveProject(reloaded)
  expect_false(reloaded$modified)
})

test_that("saveProject errors when project has no jsonPath and path is NULL", {
  project <- Project$new()
  expect_snapshot(saveProject(project), error = TRUE)
})

test_that("saveProject to a new path rebinds jsonPath, projectFilePath, and projectDirPath", {
  project <- testProject()
  newPath <- withr::local_tempfile(fileext = ".json")

  saveProject(project, newPath)

  expect_identical(project$jsonPath, fs::path_abs(newPath))
  expect_identical(project$projectFilePath, fs::path_abs(newPath))
  expect_identical(project$projectDirPath, dirname(fs::path_abs(newPath)))
})

test_that("a bare saveProject after a save-as writes to the new location", {
  project <- testProject()
  newPath <- withr::local_tempfile(fileext = ".json")
  saveProject(project, newPath)

  project$modelFolder <- "Models2"
  expect_true(project$modified)

  saveProject(project)
  expect_false(project$modified)
  expect_identical(
    loadProject(newPath)$modelFolder,
    fs::path_abs(
      file.path(dirname(fs::path_abs(newPath)), "Models2")
    )
  )
})

test_that("saveProject surfaces a missing parent directory with file context", {
  project <- testProject()
  badPath <- file.path(withr::local_tempdir(), "does-not-exist", "Project.json")
  # Path is environment-specific, so match the message rather than snapshot it.
  expect_error(
    saveProject(project, badPath),
    "Parent directory does not exist"
  )
})

test_that("saveProject errors on non-Project input", {
  expect_error(saveProject("not a project"), "Project")
})

test_that("exampleProject() succeeds", {
  path <- exampleProjectPath()
  expect_true(file.exists(path))
  project <- loadProject(path)
  expect_s3_class(project, "Project")
  expect_equal(project$schemaVersion, "2.0")
})
test_that("isProjectInitialized correctly identifies project directories", {
  tempDir <- withr::local_tempdir(pattern = "test_project_check")

  expect_false(isProjectInitialized(tempDir))

  initProject(destination = tempDir, overwrite = TRUE)
  expect_true(isProjectInitialized(tempDir))

  unlink(file.path(tempDir, "Project.json"))
  expect_true(isProjectInitialized(tempDir))
})

test_that("isProjectInitialized handles non-existent directories", {
  # Should return FALSE for non-existent directory
  expect_false(isProjectInitialized("non_existent_directory"))
})

test_that("isProjectInitialized does not false-positive on a dir whose path contains 'Project'", {
  parent <- withr::local_tempdir()
  # The directory's own path contains "Project"; an unrelated .xlsx inside
  # it must not be mistaken for a project config file.
  dir <- file.path(parent, "MyProjectFolder")
  dir.create(dir)
  writeLines("x", file.path(dir, "data.xlsx"))

  expect_false(isProjectInitialized(dir))
})

test_that("initProject(type = 'minimal', createExcel = FALSE) creates the JSON skeleton", {
  dir <- withr::local_tempdir()
  initProject(destination = dir, type = "minimal", createExcel = FALSE)

  expect_true(file.exists(file.path(dir, "Project.json")))
  expect_false(file.exists(file.path(dir, "Project.xlsx")))
  expect_true(dir.exists(file.path(dir, "Models", "Simulations")))
  expect_true(dir.exists(file.path(dir, "Results", "Figures")))
})

test_that("initProject(createExcel = FALSE) over an existing project does not write Excel", {
  dir <- withr::local_tempdir()
  initProject(destination = dir, type = "minimal", createExcel = FALSE)

  initProject(
    destination = dir,
    type = "minimal",
    createExcel = FALSE,
    overwrite = TRUE
  )
  expect_false(file.exists(file.path(dir, "Project.xlsx")))
})

test_that("initProject aborts non-interactively when a project exists and overwrite = FALSE", {
  dir <- withr::local_tempdir()
  initProject(destination = dir, type = "minimal", createExcel = FALSE)

  # Tests run non-interactively, so the prompt path must abort with guidance.
  expect_snapshot(
    error = TRUE,
    initProject(destination = dir, type = "minimal", createExcel = FALSE)
  )
})

test_that("initProject aborts when the user declines the overwrite prompt", {
  dir <- withr::local_tempdir()
  initProject(destination = dir, type = "minimal", createExcel = FALSE)

  local_mocked_bindings(
    .isInteractive = function() TRUE,
    .confirmOverwrite = function() FALSE
  )
  expect_snapshot(
    error = TRUE,
    initProject(destination = dir, type = "minimal", createExcel = FALSE)
  )
})

test_that("initProject proceeds when the user accepts the overwrite prompt", {
  dir <- withr::local_tempdir()
  initProject(destination = dir, type = "minimal", createExcel = FALSE)

  local_mocked_bindings(
    .isInteractive = function() TRUE,
    .confirmOverwrite = function() TRUE
  )
  expect_invisible(
    initProject(destination = dir, type = "minimal", createExcel = FALSE)
  )
  expect_true(isProjectInitialized(dir))
})

test_that("initProject with overwrite = TRUE doesn't ask for permission", {
  temp_project <- with_temp_project()

  expect_true(isProjectInitialized(temp_project$path))

  initProject(
    destination = temp_project$path,
    type = "example",
    overwrite = TRUE
  )
  expect_true(isProjectInitialized(temp_project$path))
})

test_that("initProject creates proper project structure", {
  temp_project <- with_temp_project()

  expect_true(file.exists(file.path(temp_project$path, "Project.json")))
  expect_true(file.exists(file.path(temp_project$path, "Project.xlsx")))
  expect_true(dir.exists(file.path(temp_project$path, "Configurations")))
  expect_true(dir.exists(file.path(temp_project$path, "Models")))
  expect_true(dir.exists(file.path(temp_project$path, "Data")))
  expect_true(dir.exists(file.path(temp_project$path, "Results")))
  expect_true(file.exists(file.path(
    temp_project$path,
    "Configurations",
    "ModelParameters.xlsx"
  )))
  expect_true(file.exists(file.path(
    temp_project$path,
    "Configurations",
    "Individuals.xlsx"
  )))
  expect_true(file.exists(file.path(
    temp_project$path,
    "Configurations",
    "Scenarios.xlsx"
  )))
  expect_true(file.exists(file.path(
    temp_project$path,
    "Configurations",
    "Plots.xlsx"
  )))
  expect_true(file.exists(file.path(
    temp_project$path,
    "Configurations",
    "Populations.xlsx"
  )))
  expect_s3_class(temp_project$project, "Project")
})

test_that("exampleProjectPath returns an existing Project.json", {
  path <- exampleProjectPath()
  expect_type(path, "character")
  expect_match(path, "Project\\.json$")
  expect_true(file.exists(path))
})


test_that("a mutation after validateProject() forces .ensureValid to re-validate", {
  project <- testProject()
  # Force the cache flag without having to run a full validation
  # (validateProject() depends on dataFolder existing in the test
  # fixture, which is a separate concern).
  project$.markValidated()
  expect_true(project$validatedSinceMutation)

  # A mutation must clear the cache so .ensureValid re-runs the
  # validators on the new shape; otherwise downstream callers
  # (runScenarios, createPlots) would skip on a now-invalid project.
  addOutputPath(project, "X", "Organism|A|Concentration in container")
  expect_false(project$validatedSinceMutation)

  # .ensureValid short-circuits only when the flag is TRUE; re-mark
  # validated, mutate again, and confirm the flag is cleared a second
  # time (i.e. every successful mutator goes through .markModified).
  project$.markValidated()
  removeOutputPath(project, "X")
  expect_false(project$validatedSinceMutation)
})

test_that("mutated project survives a saveProject -> loadProject round-trip", {
  project <- testProject()

  addOutputPath(project, "RoundtripX", "Organism|A|Concentration in container")
  addIndividual(
    project,
    "Pediatric_male",
    species = "Human",
    population = "European_ICRP_2002",
    gender = "MALE",
    weight = 25,
    height = 125,
    age = 8
  )

  out <- withr::local_tempfile(fileext = ".json")
  esqlabsR:::.saveProjectJson(project, out)
  reloaded <- loadProject(out)

  expect_identical(
    reloaded$outputPaths$RoundtripX,
    project$outputPaths$RoundtripX
  )
  expect_named(reloaded$individuals, names(project$individuals))
  expect_identical(reloaded$individuals$Pediatric_male$weight, 25)
})
