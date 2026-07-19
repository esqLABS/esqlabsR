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
        individual = "Ghost",
        modelFile = "m.pkml",
        outputPaths = list("O1")
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

test_that("a bound project's container edit persists immediately", {
  tmp <- saveSnapshot(testProject(), local_projectPath())
  project <- loadProject(tmp)

  project$modelFolder <- "AnotherModels"

  # Container metadata is write-through: a reload sees the new value with no
  # separate save step.
  expect_identical(
    loadProject(tmp)$modelFolder,
    fs::path_abs(file.path(dirname(tmp), "AnotherModels"))
  )
})

test_that("a container edit on an inline-only project keeps its sections", {
  # An inline snapshot has every section inlined in the container file and no
  # `definitions/` tree on disk. A container-metadata edit must materialize the
  # still-inline sections before the container-only write empties their inline
  # copies, or the next load's inline fallback reads them all empty.
  tmp <- saveSnapshot(testProject(), local_projectPath())
  project <- loadProject(tmp)
  before <- project$scenarios

  project$name <- "Renamed"

  reloaded <- loadProject(tmp)
  expect_named(reloaded$scenarios, names(before))
  expect_length(reloaded$scenarios, length(before))
  expect_identical(
    reloaded$scenarios$testscenario$modelFile,
    before$testscenario$modelFile
  )
  # The container edit itself still takes effect on reload.
  expect_identical(reloaded$name, "Renamed")
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

test_that("initProject(type = 'minimal') scaffolds a definitions/ directory", {
  dir <- withr::local_tempdir()
  initProject(destination = dir, type = "minimal", createExcel = FALSE)

  expect_true(dir.exists(file.path(dir, "definitions")))
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

test_that("initProject(overwrite = TRUE) replaces, removing stale definition files but keeping unrelated user files", {
  dir <- withr::local_tempdir()
  initProject(destination = dir, type = "example", createExcel = FALSE)

  # A stale definition file the template does not ship (simulating an old project's
  # definition), and an unrelated user file that must survive the overwrite.
  staleDefinition <- file.path(dir, "definitions", "scenarios", "staledefinition.json")
  writeLines("{}", staleDefinition)
  userFile <- file.path(dir, "my_notes.txt")
  writeLines("keep me", userFile)
  expect_true(file.exists(staleDefinition))

  initProject(
    destination = dir,
    type = "example",
    createExcel = FALSE,
    overwrite = TRUE
  )

  # Overwrite means replace: the stale definition is gone, the unrelated user
  # file is untouched, and the fresh project scaffold is present.
  expect_false(file.exists(staleDefinition))
  expect_true(file.exists(userFile))
  expect_identical(readLines(userFile), "keep me")
  expect_true(file.exists(file.path(dir, "Project.json")))
  expect_true(dir.exists(file.path(dir, "definitions", "scenarios")))
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

test_that("mutated project survives a snapshot -> loadProject round-trip", {
  project <- testProject()

  addOutputPath(project, "roundtripx", "Organism|A|Concentration in container")
  addIndividual(
    project,
    "pediatric_male",
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
    reloaded$outputPaths$roundtripx,
    project$outputPaths$roundtripx
  )
  expect_named(reloaded$individuals, names(project$individuals))
  expect_identical(reloaded$individuals$pediatric_male$weight, 25)
})
