# Tests for the single-file snapshot artifact (R/entity-files.R):
# the `.esqlabsR` extension normalization on saveSnapshot(), and the
# two-argument loadSnapshot(file, dir) which reads a snapshot and writes a
# full definitions/<kind>/ tree project at `dir`, returning the Project bound
# to `dir`. Loading a snapshot IS materializing it; the surface is just save
# and load, with no separate explode-the-tree verb.

# --- saveSnapshot: the `.esqlabsR` extension ----------------------------------

test_that("saveSnapshot normalizes a no-extension path to .esqlabsR", {
  project <- testProject()
  dir <- withr::local_tempdir()
  out <- saveSnapshot(project, file.path(dir, "study1"))

  expect_identical(fs::path_ext(out), "esqlabsR")
  expect_true(file.exists(out))
  expect_false(file.exists(file.path(dir, "study1")))
})

test_that("saveSnapshot normalizes a .json path to .esqlabsR", {
  project <- testProject()
  dir <- withr::local_tempdir()
  out <- saveSnapshot(project, file.path(dir, "study1.json"))

  expect_identical(fs::path_ext(out), "esqlabsR")
  expect_true(file.exists(out))
  expect_false(file.exists(file.path(dir, "study1.json")))
})

test_that("saveSnapshot keeps an explicit .esqlabsR path verbatim", {
  project <- testProject()
  dir <- withr::local_tempdir()
  out <- saveSnapshot(project, file.path(dir, "study1.esqlabsR"))

  expect_identical(out, file.path(dir, "study1.esqlabsR"))
  expect_true(file.exists(out))
})

test_that("saveSnapshot honors a different explicit extension with a note", {
  project <- testProject()
  dir <- withr::local_tempdir()
  # The note carries the tempdir path, so match the canonical-form sentence
  # rather than snapshotting the whole message.
  expect_message(
    out <- saveSnapshot(project, file.path(dir, "study1.txt")),
    "canonical single-file snapshot extension"
  )

  expect_identical(fs::path_ext(out), "txt")
  expect_true(file.exists(out))
})

test_that("the .esqlabsR snapshot content is the inlined-JSON snapshot", {
  project <- testProject()
  dir <- withr::local_tempdir()
  out <- saveSnapshot(project, file.path(dir, "study1"))

  # Content is still JSON, with every section inlined and no tree alongside.
  raw <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  expect_length(raw$scenarios, length(project$scenarios))
  expect_false(dir.exists(file.path(dir, "definitions")))
})

test_that("saveSnapshot refuses the own container path", {
  project <- testProject()
  # Passing the container path (Project.json) is refused even though it would
  # normalize to Project.esqlabsR, because the intent is clearly the container.
  expect_snapshot(saveSnapshot(project, project$jsonPath), error = TRUE)
})

test_that("saveSnapshot writes Project.esqlabsR next to the container safely", {
  project <- testProject()
  # A no-extension stem of the container basename normalizes to a distinct
  # `.esqlabsR` file, so it does not clobber the authoritative Project.json.
  containerNoExt <- fs::path_ext_remove(project$jsonPath)
  out <- saveSnapshot(project, containerNoExt)

  expect_identical(out, fs::path_ext_set(containerNoExt, "esqlabsR"))
  expect_true(file.exists(out))
  # The authoritative container is untouched (still tree-shape: scenarios []).
  raw <- jsonlite::fromJSON(project$jsonPath, simplifyVector = FALSE)
  expect_length(raw$scenarios, 0L)
})

# --- loadSnapshot(file, dir): load IS the materialize -------------------------

test_that("loadSnapshot writes a tree and returns the Project bound to dir", {
  out <- saveSnapshot(testProject(), file.path(withr::local_tempdir(), "study"))
  dir <- withr::local_tempdir()

  project <- loadSnapshot(out, dir)

  # The full tree project is written at `dir`: container + per-kind tree.
  expect_true(file.exists(file.path(dir, "Project.json")))
  expect_true(dir.exists(file.path(dir, "definitions", "scenarios")))
  # The returned project is bound to `dir`.
  expect_identical(
    project$projectDirPath,
    dirname(fs::path_abs(
      file.path(dir, "Project.json")
    ))
  )
})

test_that("loadSnapshot's tree reloads via loadProject identically", {
  source <- exampleProject()
  out <- saveSnapshot(source, file.path(withr::local_tempdir(), "study"))
  dir <- withr::local_tempdir()

  project <- loadSnapshot(out, dir)
  reloaded <- loadProject(file.path(dir, "Project.json"))

  expect_named(
    reloaded$scenarios,
    names(project$scenarios),
    ignore.order = TRUE
  )
  expect_named(
    reloaded$individuals,
    names(project$individuals),
    ignore.order = TRUE
  )
  expect_named(
    reloaded$parameterSets,
    names(project$parameterSets),
    ignore.order = TRUE
  )
  expect_named(
    reloaded$dataCombined,
    names(project$dataCombined),
    ignore.order = TRUE
  )
})

test_that("a loadSnapshot project edits write-through to dir like any tree", {
  out <- saveSnapshot(testProject(), file.path(withr::local_tempdir(), "study"))
  dir <- withr::local_tempdir()
  project <- loadSnapshot(out, dir)
  before <- names(project$scenarios)
  scenarioDir <- file.path(dir, "definitions", "scenarios")

  addScenario(project, "freshone", modelFile = "Aciclovir.pkml")
  # Write-through lands the file under `dir`, and a reload sees it alongside
  # the materialized siblings.
  expect_true(file.exists(file.path(scenarioDir, "freshone.json")))
  reloaded <- loadProject(file.path(dir, "Project.json"))
  expect_named(
    reloaded$scenarios,
    c(before, "freshone"),
    ignore.order = TRUE
  )

  removeScenario(project, "freshone")
  expect_false(file.exists(file.path(scenarioDir, "freshone.json")))
})

test_that("loadSnapshot reads a .esqlabsR snapshot", {
  out <- saveSnapshot(testProject(), file.path(withr::local_tempdir(), "study"))
  expect_identical(fs::path_ext(out), "esqlabsR")

  project <- loadSnapshot(out, withr::local_tempdir())
  expect_named(
    project$scenarios,
    names(testProject()$scenarios),
    ignore.order = TRUE
  )
})

test_that("loadSnapshot still reads a plain inlined Project.json (back-compat)", {
  # importProjectFromExcel() writes a single inlined Project.json; loadSnapshot()
  # must still accept that legacy form as the snapshot to materialize.
  source <- testProject()
  legacyDir <- withr::local_tempdir()
  legacy <- file.path(legacyDir, "Project.json")
  .saveProjectJson(source, legacy, includeScenarios = TRUE)

  dir <- withr::local_tempdir()
  project <- loadSnapshot(legacy, dir)
  expect_true(dir.exists(file.path(dir, "definitions", "scenarios")))
  expect_named(
    project$scenarios,
    names(source$scenarios),
    ignore.order = TRUE
  )
})

test_that("loadSnapshot creates dir when absent", {
  out <- saveSnapshot(testProject(), file.path(withr::local_tempdir(), "study"))
  dir <- file.path(withr::local_tempdir(), "new", "nested")
  expect_false(dir.exists(dir))

  project <- loadSnapshot(out, dir)
  expect_true(dir.exists(file.path(dir, "definitions", "scenarios")))
  expect_identical(
    project$projectDirPath,
    dirname(fs::path_abs(
      file.path(dir, "Project.json")
    ))
  )
})

test_that("loadSnapshot refuses a dir that already holds a project", {
  out <- saveSnapshot(testProject(), file.path(withr::local_tempdir(), "study"))
  # `dir` already contains a tree project, so materializing into it would
  # clobber the existing work; refuse rather than silently overwrite. The
  # message carries the (per-run) tempdir path, so match the stable sentence.
  dir <- testProject()$projectDirPath

  expect_error(loadSnapshot(out, dir), "already contains an esqlabsR project")
})

test_that("snapshot -> loadSnapshot -> snapshot is a fixed point over .esqlabsR", {
  project <- testProject()

  out1 <- saveSnapshot(project, file.path(withr::local_tempdir(), "study"))
  expect_identical(fs::path_ext(out1), "esqlabsR")
  reloaded <- loadSnapshot(out1, withr::local_tempdir())

  out2 <- saveSnapshot(reloaded, file.path(withr::local_tempdir(), "study"))
  # Byte-stable git diffs are the stated design goal, so assert byte equality,
  # not just structural equality: the two snapshots must be identical line for
  # line.
  expect_identical(readLines(out1), readLines(out2))
})

test_that("snapshot preserves metadata and the filePaths/excel split", {
  project <- exampleProject()

  out <- saveSnapshot(project, file.path(withr::local_tempdir(), "study"))
  restored <- loadSnapshot(out, withr::local_tempdir())

  expect_identical(restored$name, "Example")
  expect_identical(restored$description, "Aciclovir IV PK example project")
  expect_identical(restored$definitionsFolder, "definitions")
  expect_named(
    restored$filePaths,
    c("modelFolder", "populationsFolder", "dataFolder", "outputFolder"),
    ignore.order = TRUE
  )
  expect_length(restored$excel, 7L)
})

test_that("loadSnapshot errors on a non-existent snapshot file", {
  # The message carries the (per-run) path, so match the stable sentence.
  dir <- withr::local_tempdir()
  expect_error(
    loadSnapshot(file.path(dir, "missing.esqlabsR"), withr::local_tempdir()),
    "File not found"
  )
})

# --- migration of a legacy inlined single-file project ------------------------

test_that("loadSnapshot migrates a legacy inlined Project.json end to end", {
  # The public migration path for a handed-over single-file project is
  # loadSnapshot(file, dir): it explodes the inlined snapshot into a tree.
  source <- exampleProject()
  legacyDir <- withr::local_tempdir()
  legacy <- file.path(legacyDir, "Project.json")
  .saveProjectJson(source, legacy, includeScenarios = TRUE)

  dir <- withr::local_tempdir()
  loadSnapshot(legacy, dir)
  migrated <- loadProject(file.path(dir, "Project.json"))

  # Section for section, the migrated tree project matches the original.
  expect_named(
    migrated$scenarios,
    names(source$scenarios),
    ignore.order = TRUE
  )
  expect_named(
    migrated$individuals,
    names(source$individuals),
    ignore.order = TRUE
  )
  expect_named(
    migrated$populations,
    names(source$populations),
    ignore.order = TRUE
  )
  expect_named(
    migrated$applications,
    names(source$applications),
    ignore.order = TRUE
  )
  expect_named(
    migrated$parameterSets,
    names(source$parameterSets),
    ignore.order = TRUE
  )
  expect_named(
    migrated$outputPaths,
    names(source$outputPaths),
    ignore.order = TRUE
  )
  expect_named(
    migrated$parameterIdentification,
    names(source$parameterIdentification),
    ignore.order = TRUE
  )
  expect_named(
    migrated$dataCombined,
    names(source$dataCombined),
    ignore.order = TRUE
  )
})

# A legacy single-file Project.json may carry non-canonical ids (mixed case),
# which the entity tree (keyed by canonical id) cannot store. loadSnapshot()
# must canonicalize on the way in, lossless across every section: definitions
# AND the references that point at them (a scenario id used by a plot's
# dataCombined row and by a PI task / output mapping) are lowercased together,
# so the migrated tree's foreign keys still resolve.
test_that("loadSnapshot migrates a non-canonical legacy Project.json losslessly", {
  source <- exampleProject()
  legacyDir <- withr::local_tempdir()
  legacy <- file.path(legacyDir, "Project.json")
  .saveProjectJson(source, legacy, includeScenarios = TRUE)

  # Make two ids non-canonical everywhere they appear (definition and every
  # reference), exactly as a hand-authored legacy file would: a scenario id
  # (referenced by a plot row and a PI task) and an output-path id (referenced
  # by a PI output mapping).
  txt <- readLines(legacy)
  txt <- gsub("\"aciclovir_iv\"", "\"Aciclovir_IV\"", txt, fixed = TRUE)
  txt <- gsub("\"aciclovir_pvb\"", "\"Aciclovir_PVB\"", txt, fixed = TRUE)
  writeLines(txt, legacy)

  dir <- withr::local_tempdir()
  # The migration must not abort on the non-canonical id at the tree writer.
  expect_no_error(loadSnapshot(legacy, dir))
  migrated <- loadProject(file.path(dir, "Project.json"))

  # The scenario and output-path definitions are filed under their canonical
  # lowercase ids.
  expect_true("aciclovir_iv" %in% names(migrated$scenarios))
  expect_true("aciclovir_pvb" %in% names(migrated$outputPaths))

  # The references that pointed at them are canonicalized too, so they resolve.
  piTask <- migrated$parameterIdentification[[1]]
  expect_identical(as.character(piTask$scenarios), "aciclovir_iv")
  expect_identical(piTask$outputMappings[[1]]$outputPathId, "aciclovir_pvb")

  simScenario <- migrated$dataCombined[[1]]$simulated[[1]]$scenario
  expect_identical(simScenario, "aciclovir_iv")
})
