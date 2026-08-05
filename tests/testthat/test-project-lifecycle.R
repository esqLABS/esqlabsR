test_that("loadProject() returns a Project from a valid Project.json", {
  project <- testProject()
  expect_s3_class(project, "Project")
  expect_equal(project$info$schemaVersion, "2.0")
  expect_equal(length(project$definitions$scenarios), 4)
})

test_that("loadProject() errors when the file does not exist", {
  expect_error(
    loadProject(file.path(tempdir(), "does_not_exist.json")),
    regexp = "(does not exist|not found)"
  )
})

test_that("loadProject() opens a project folder, whatever its project file is called", {
  # A folder is the natural thing to hand `loadProject()`, and an Excel import
  # can name the container after the workbook it read, so neither the folder nor
  # a non-default name may need the caller to spell out `Project.json`.
  dir <- withr::local_tempdir()
  initProject(destination = dir, type = "minimal", createExcel = FALSE)

  expect_s3_class(loadProject(dir), "Project")

  file.rename(file.path(dir, "Project.json"), file.path(dir, "MyStudy.json"))
  project <- loadProject(dir)
  expect_equal(fs::path_file(project$info$projectFilePath), "MyStudy.json")

  # And with the folder as the working directory, a bare call finds it too.
  withr::local_dir(dir)
  expect_s3_class(loadProject(), "Project")
})

test_that("loadProject() names the mistake on a folder that holds no project", {
  # Not a snapshot: the message carries the folder's absolute path, which is a
  # machine-specific temp directory.
  dir <- withr::local_tempdir()
  expect_error(loadProject(dir), "No esqlabsR project found in the folder")
})

test_that("loadProject() asks which project to open when a folder holds several", {
  dir <- withr::local_tempdir()
  initProject(destination = dir, type = "minimal", createExcel = FALSE)
  file.copy(
    file.path(dir, "Project.json"),
    file.path(dir, "StudyA.json")
  )
  file.rename(file.path(dir, "Project.json"), file.path(dir, "StudyB.json"))

  expect_error(loadProject(dir), "2 project files")
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

test_that("a bound project's container edit stays in memory until saveProject()", {
  project <- testProject()
  tmp <- project$info$projectFilePath
  original <- project$paths$simulationsFolder

  project$paths$simulationsFolder <- "AnotherModels"

  # The edit is in memory only: a fresh load still reads the old value, and the
  # project is dirty.
  expect_identical(loadProject(tmp)$paths$simulationsFolder, original)
  expect_true(.isModified(project))

  # After an explicit save, a fresh load sees the new value.
  saveProject(project)
  expect_identical(
    loadProject(tmp)$paths$simulationsFolder,
    fs::path_abs(file.path(dirname(tmp), "AnotherModels"))
  )
  expect_false(.isModified(project))
})

test_that("a container edit followed by saveProject() keeps the sections", {
  # A container-metadata edit and save must not empty the section trees: the
  # full-tree reconciler `.writeProjectTree` (which `saveProject` drives)
  # rewrites every kind's tree plus the container.
  project <- testProject()
  tmp <- project$info$projectFilePath
  before <- project$definitions$scenarios

  project$info$name <- "Renamed"
  saveProject(project)

  reloaded <- loadProject(tmp)
  expect_named(reloaded$definitions$scenarios, names(before))
  expect_length(reloaded$definitions$scenarios, length(before))
  expect_identical(
    reloaded$definitions$scenarios$testscenario$modelFile,
    before$testscenario$modelFile
  )
  # The container edit takes effect on reload.
  expect_identical(reloaded$info$name, "Renamed")
})

# --- saveProject() ------------------------------------------------------------

test_that("saveProject() writes only the changed definition's file (write-if-different)", {
  project <- testProject()
  saveProject(project) # settle the tree (byte-stable) so mtimes are a baseline
  scenarioDir <- file.path(
    project$info$projectDirPath,
    "definitions",
    "scenarios"
  )

  # Edit exactly one scenario, leaving its siblings untouched.
  setScenario(project, "testscenario", modelFile = "AnotherModel.pkml")

  before <- file.info(list.files(scenarioDir, full.names = TRUE))$mtime
  names(before) <- list.files(scenarioDir)
  Sys.sleep(1.05) # coarse mtime resolution: separate the save in time

  saveProject(project)

  after <- file.info(list.files(scenarioDir, full.names = TRUE))$mtime
  names(after) <- list.files(scenarioDir)
  changed <- names(after)[after > before]
  expect_identical(changed, "testscenario.json")
})

test_that("saveProject() deletes an orphan and leaves other kinds untouched", {
  project <- testProject()
  saveProject(project)
  scenarioDir <- file.path(
    project$info$projectDirPath,
    "definitions",
    "scenarios"
  )
  outputPathDir <- file.path(
    project$info$projectDirPath,
    "definitions",
    "output-paths"
  )

  target <- file.path(scenarioDir, "testscenario.json")
  expect_true(file.exists(target))
  outputPathsBefore <- list.files(outputPathDir)

  removeScenario(project, "testscenario")
  saveProject(project)

  # The removed scenario's file is gone; files under another kind are untouched.
  expect_false(file.exists(target))
  expect_identical(list.files(outputPathDir), outputPathsBefore)
})

test_that("a clean saveProject() is a no-op with the up-to-date message", {
  project <- testProject()
  saveProject(project) # first save clears the dirty bit

  expect_snapshot(saveProject(project))
})

test_that("saveProject() on an unbound in-memory project aborts", {
  project <- Project$new()
  .setInfoField(project, "schemaVersion", "2.0")
  .setSection(project, "scenarios", list())

  expect_snapshot(saveProject(project), error = TRUE)
})

# A project loaded from a container whose name is not the canonical
# `Project.json` (e.g. `ProjectConfiguration.json` from a legacy Excel import,
# or a renamed container) must be saved back to that same file, not forked into
# a stray `Project.json`. Forking would leave the loaded container stale (silent
# data loss on the next `loadProject()` of it) and fool a status check.
test_that("saveProject() updates the loaded container, not a stray Project.json", {
  # Materialize a tree project, then rename its container to a non-default name.
  dir <- withr::local_tempdir("legacyname_")
  file.copy(
    list.files(testthat::test_path("data", "TestProject"), full.names = TRUE),
    dir,
    recursive = TRUE
  )
  file.rename(
    file.path(dir, "Project.json"),
    file.path(dir, "ProjectConfiguration.json")
  )

  project <- loadProject(file.path(dir, "ProjectConfiguration.json"))
  project$info$description <- "edited"
  saveProject(project)

  # The loaded container carries the edit, and no stray `Project.json` appears.
  reloaded <- loadProject(file.path(dir, "ProjectConfiguration.json"))
  expect_identical(reloaded$info$description, "edited")
  expect_false(file.exists(file.path(dir, "Project.json")))
})

test_that("saveProject() stamps the writing version into the file and the handle", {
  # A project loaded from a container an earlier version wrote reports that
  # version until it is saved; the save records the version that wrote the file,
  # and the handle adopts it, so memory and tree agree after a save.
  project <- testProject()
  .setInfoField(project, "esqlabsRVersion", "1.2.3")
  current <- as.character(utils::packageVersion("esqlabsR"))

  project$info$name <- "Renamed"
  saveProject(project)

  container <- jsonlite::fromJSON(project$info$projectFilePath)
  expect_identical(container$esqlabsRVersion, current)
  expect_identical(project$info$esqlabsRVersion, current)
})

# #1213 item 26: the scaffold template ships a fixed `esqlabsRVersion`, so a
# freshly initialized project claimed one version and the first `saveProject()`
# rewrote it to the running one, which read as the project downgrading itself.
# `initProject()` stamps the running version too, so all the writers agree.
test_that("initProject() stamps the writing version, so a later save does not change it", {
  destination <- withr::local_tempdir()
  initProject(
    destination,
    type = "minimal",
    createExcel = FALSE,
    overwrite = TRUE
  )
  current <- as.character(utils::packageVersion("esqlabsR"))

  project <- loadProject(file.path(destination, "Project.json"))
  expect_identical(project$info$esqlabsRVersion, current)

  addOutputPath(project, "op", "Organism|Liver|Volume")
  saveProject(project)
  container <- jsonlite::fromJSON(project$info$projectFilePath)
  expect_identical(container$esqlabsRVersion, current)
})

test_that("saveProject() never warns about a stale Excel side-car", {
  # Build a project with an Excel side-car that has drifted, then edit and save.
  temp_project <- with_temp_project()
  project <- temp_project$project

  addOutputPath(project, "singleaxis", "Organism|A|Concentration in container")

  # saveProject reconciles memory -> tree only; it emits no Excel warning even
  # though the workbook is now a stale export of the project.
  expect_no_warning(saveProject(project))
})

# --- reloadProject() ----------------------------------------------------------

test_that("reloadProject() discards in-memory edits and clears the dirty bit", {
  project <- testProject()

  addScenario(project, "willbediscarded", modelFile = "Aciclovir.pkml")
  expect_true("willbediscarded" %in% names(project$definitions$scenarios))
  expect_true(.isModified(project))

  reloadProject(project)

  expect_false("willbediscarded" %in% names(project$definitions$scenarios))
  expect_false(.isModified(project))
  expect_true(project$status$tree_in_sync)
})

test_that("a clean reloadProject() is silent", {
  project <- testProject()
  expect_no_message(reloadProject(project))
})

test_that("reloadProject() on an unbound in-memory project aborts", {
  project <- Project$new()
  .setInfoField(project, "schemaVersion", "2.0")

  expect_snapshot(reloadProject(project), error = TRUE)
})

test_that("exampleProject() succeeds", {
  path <- exampleProjectPath()
  expect_true(file.exists(path))
  project <- loadProject(path)
  expect_s3_class(project, "Project")
  expect_equal(project$info$schemaVersion, "2.0")
})
test_that("isProjectInitialized correctly identifies project directories", {
  tempDir <- withr::local_tempdir(pattern = "test_project_check")

  expect_false(isProjectInitialized(tempDir))

  initProject(destination = tempDir, overwrite = TRUE)
  expect_true(isProjectInitialized(tempDir))

  # The Excel files are an interchange format, not the project: with the
  # `Project.json` gone, the exported `Project.xlsx` and `Configurations/`
  # left behind do not make this a project any more.
  unlink(file.path(tempDir, "Project.json"))
  expect_false(isProjectInitialized(tempDir))
})

test_that("isProjectInitialized handles non-existent directories", {
  # Should return FALSE for non-existent directory
  expect_false(isProjectInitialized("non_existent_directory"))
})

test_that("isProjectInitialized finds a container that is not named Project.json", {
  # An Excel import names the container after the workbook it read, so
  # `MyStudy.xlsx` produces `MyStudy.json`. The declared schema version, not
  # the file name, makes it a project.
  dir <- withr::local_tempdir()
  initProject(destination = dir, type = "minimal", createExcel = FALSE)
  file.rename(file.path(dir, "Project.json"), file.path(dir, "MyStudy.json"))

  expect_true(isProjectInitialized(dir))
})

test_that("isProjectInitialized ignores a JSON file that is not a project container", {
  dir <- withr::local_tempdir()
  # A plausible neighbour: some other tool's JSON, and a pre-6.0.0 monolithic
  # snapshot, neither of which declares a project schema version.
  jsonlite::write_json(
    list(name = "not a project"),
    file.path(dir, "data.json")
  )
  writeLines("{ not json at all", file.path(dir, "broken.json"))

  expect_false(isProjectInitialized(dir))
})

test_that(".hasLegacyExcelProject does not false-positive on a dir whose path contains 'Project'", {
  parent <- withr::local_tempdir()
  # The directory's own path contains "Project"; an unrelated .xlsx inside
  # it must not be mistaken for a project config file.
  dir <- file.path(parent, "MyProjectFolder")
  dir.create(dir)
  writeLines("x", file.path(dir, "data.xlsx"))

  expect_false(.hasLegacyExcelProject(dir))
  expect_false(isProjectInitialized(dir))
})

test_that("initProject does not scaffold over an unmigrated legacy Excel project", {
  dir <- withr::local_tempdir()
  writeLines("x", file.path(dir, "Project.xlsx"))
  dir.create(file.path(dir, "Configurations"))

  # Not a project, since there is no `Project.json` ...
  expect_false(isProjectInitialized(dir))
  # ... but not a folder that is free to fill either.
  expect_snapshot(
    error = TRUE,
    initProject(destination = dir, type = "minimal", createExcel = FALSE)
  )
})

test_that("initProject(type = 'minimal', createExcel = FALSE) creates the JSON skeleton", {
  dir <- withr::local_tempdir()
  initProject(destination = dir, type = "minimal", createExcel = FALSE)

  expect_true(file.exists(file.path(dir, "Project.json")))
  expect_false(file.exists(file.path(dir, "Project.xlsx")))
  expect_true(dir.exists(file.path(dir, "Models", "Simulations")))
  expect_true(dir.exists(file.path(dir, "Results", "Figures")))
  # Snapshots folder ships even though the package does not load from it yet.
  expect_true(dir.exists(file.path(dir, "Models", "Snapshots")))
})

test_that("initProject creates a destination folder that does not exist yet", {
  # `initProject("myProject")` is the first call of the authoring workflow, so
  # the folder it is about to fill is created rather than demanded up front.
  parent <- withr::local_tempdir()
  dir <- file.path(parent, "nested", "myProject")

  initProject(destination = dir, type = "minimal", createExcel = FALSE)

  expect_true(dir.exists(dir))
  expect_true(file.exists(file.path(dir, "Project.json")))
})

test_that("initProject writes a README into each scaffold folder so it stays tracked", {
  dir <- withr::local_tempdir()
  initProject(destination = dir, type = "minimal", createExcel = FALSE)

  readmes <- file.path(
    dir,
    c(
      "Models/Simulations",
      "Models/Snapshots",
      "Data",
      "Populations",
      "Results/Figures",
      "Results/SimulationResults"
    ),
    "README.md"
  )
  expect_true(all(file.exists(readmes)))
  # The definitions tree holds authored content, not a placeholder.
  expect_false(file.exists(file.path(dir, "definitions", "README.md")))
})

test_that("initProject does not overwrite a README a user has edited", {
  # The scaffold README is a starting placeholder; a rerun (e.g.
  # `overwrite = TRUE` to refresh the template) must leave a user-edited
  # working-folder README untouched, matching the "working folders are left
  # untouched" invariant `.clearProjectArtifacts()` documents.
  dir <- withr::local_tempdir()
  initProject(destination = dir, type = "minimal", createExcel = FALSE)

  edited <- file.path(dir, "Data", "README.md")
  writeLines("My own notes about this project's data.", edited)

  initProject(
    destination = dir,
    type = "minimal",
    createExcel = FALSE,
    overwrite = TRUE
  )

  expect_identical(
    readLines(edited),
    "My own notes about this project's data."
  )
})

test_that("initProject(overwrite = TRUE) refuses a definitionsFolder pointing outside the project", {
  # `.clearProjectArtifacts()` reads `definitionsFolder` straight out of the
  # container and unlinks it recursively, so an escaping value in a downloaded
  # `Project.json` would delete a tree above the destination. The overwrite must
  # abort instead, leaving everything outside the project folder intact.
  parent <- withr::local_tempdir()
  dir <- file.path(parent, "project")
  dir.create(dir)
  initProject(destination = dir, type = "minimal", createExcel = FALSE)

  bystander <- file.path(parent, "unrelated.txt")
  writeLines("not part of any project", bystander)

  container <- file.path(dir, "Project.json")
  jsonData <- jsonlite::fromJSON(container, simplifyVector = FALSE)
  jsonData$definitionsFolder <- ".."
  jsonlite::write_json(jsonData, container, auto_unbox = TRUE, null = "null")

  expect_error(
    initProject(
      destination = dir,
      type = "minimal",
      createExcel = FALSE,
      overwrite = TRUE
    ),
    "single folder name"
  )
  expect_true(file.exists(bystander))
  expect_true(dir.exists(file.path(dir, "definitions")))
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
  staleDefinition <- file.path(
    dir,
    "definitions",
    "scenarios",
    "staledefinition.json"
  )
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

test_that("initProject(overwrite = TRUE) removes a container that is not named Project.json", {
  # The scaffold writes `Project.json`, so an overwritten project whose
  # container carried the name of the workbook it was imported from would
  # otherwise survive as a second, stale container beside it.
  dir <- withr::local_tempdir()
  initProject(destination = dir, type = "minimal", createExcel = FALSE)
  imported <- file.path(dir, "MyStudy.json")
  file.rename(file.path(dir, "Project.json"), imported)

  initProject(
    destination = dir,
    type = "minimal",
    createExcel = FALSE,
    overwrite = TRUE
  )

  expect_false(file.exists(imported))
  expect_true(file.exists(file.path(dir, "Project.json")))
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
  .markValidated(project)
  expect_true(.isValidated(project))

  # A mutation must clear the cache so .ensureValid re-runs the
  # validators on the new shape; otherwise downstream callers
  # (runScenarios, createPlots) would skip on a now-invalid project.
  addOutputPath(project, "x", "Organism|A|Concentration in container")
  expect_false(.isValidated(project))

  # .ensureValid short-circuits only when the flag is TRUE; re-mark
  # validated, mutate again, and confirm the flag is cleared a second
  # time (i.e. every successful mutator goes through .markModified).
  .markValidated(project)
  removeOutputPath(project, "x")
  expect_false(.isValidated(project))
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
  .saveProjectJson(project, out)
  reloaded <- loadProject(out)

  expect_identical(
    reloaded$definitions$outputPaths$roundtripx,
    project$definitions$outputPaths$roundtripx
  )
  expect_named(
    reloaded$definitions$individuals,
    names(project$definitions$individuals)
  )
  expect_identical(reloaded$definitions$individuals$pediatric_male$weight, 25)
})
