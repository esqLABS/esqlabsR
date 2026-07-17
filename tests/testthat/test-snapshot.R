# Tests for the single-file snapshot artifact (R/definition-files.R):
# `snapshotProject(project, dir, name, overwrite)` writes a portable
# `.esqlabsR` freeze of the in-memory state, and
# `restoreProject(snapshot, dir, overwrite)` reads one and materializes a full
# definitions/<kind>/ tree project at `dir`, returning the Project bound to
# `dir`. Restoring a snapshot IS materializing it; there is no separate
# explode-the-tree verb.

# --- snapshotProject: the `.esqlabsR` extension -------------------------------

test_that("snapshotProject forces .esqlabsR for a no-extension name", {
  project <- testProject()
  dir <- withr::local_tempdir()
  out <- snapshotProject(project, dir = dir, name = "study1")

  expect_identical(fs::path_ext(out), "esqlabsR")
  expect_true(file.exists(out))
  expect_false(file.exists(file.path(dir, "study1")))
})

test_that("snapshotProject forces .esqlabsR for a .json name", {
  project <- testProject()
  dir <- withr::local_tempdir()
  out <- snapshotProject(project, dir = dir, name = "study1.json")

  expect_identical(fs::path_ext(out), "esqlabsR")
  expect_true(file.exists(out))
  expect_false(file.exists(file.path(dir, "study1.json")))
})

test_that("snapshotProject keeps an explicit .esqlabsR name verbatim", {
  project <- testProject()
  dir <- withr::local_tempdir()
  out <- snapshotProject(project, dir = dir, name = "study1.esqlabsR")

  expect_identical(out, file.path(dir, "study1.esqlabsR"))
  expect_true(file.exists(out))
})

test_that("snapshotProject forces .esqlabsR over any foreign extension", {
  project <- testProject()
  dir <- withr::local_tempdir()
  # A foreign extension is replaced, not honored: `exp.zip` -> `exp.esqlabsR`,
  # silently (no informational note).
  out <- expect_no_message(
    snapshotProject(project, dir = dir, name = "exp.zip")
  )

  expect_identical(fs::path_file(out), "exp.esqlabsR")
  expect_true(file.exists(out))
})

test_that("snapshotProject uses a timestamped default name from the project", {
  project <- testProject()
  dir <- withr::local_tempdir()
  out <- snapshotProject(project, dir = dir)

  # <projectName>-YYYY-MM-DD-HHMMSS.esqlabsR, colon-free and sortable. Match the
  # pattern, not the exact timestamp.
  expect_match(
    fs::path_file(out),
    paste0("^", project$name, "-\\d{4}-\\d{2}-\\d{2}-\\d{6}\\.esqlabsR$")
  )
  expect_true(file.exists(out))
})

test_that("snapshotProject default name falls back to 'project' when nameless", {
  project <- Project$new()
  project$schemaVersion <- "2.0"
  dir <- withr::local_tempdir()
  out <- snapshotProject(project, dir = dir)

  expect_match(
    fs::path_file(out),
    "^project-\\d{4}-\\d{2}-\\d{2}-\\d{6}\\.esqlabsR$"
  )
  expect_true(file.exists(out))
})

test_that("snapshotProject creates dir when absent", {
  project <- testProject()
  dir <- file.path(withr::local_tempdir(), "new", "nested")
  expect_false(dir.exists(dir))

  out <- snapshotProject(project, dir = dir, name = "study")
  expect_true(file.exists(out))
})

test_that("snapshotProject errors over an existing file unless overwrite", {
  project <- testProject()
  dir <- withr::local_tempdir()
  snapshotProject(project, dir = dir, name = "study")

  expect_snapshot(
    snapshotProject(project, dir = dir, name = "study"),
    error = TRUE,
    transform = .redactTmpDir
  )

  # `overwrite = TRUE` replaces it.
  out <- snapshotProject(project, dir = dir, name = "study", overwrite = TRUE)
  expect_true(file.exists(out))
})

test_that("the .esqlabsR snapshot content is the inlined-JSON snapshot", {
  project <- testProject()
  dir <- withr::local_tempdir()
  out <- snapshotProject(project, dir = dir, name = "study1")

  # Content is still JSON, with every section inlined and no tree alongside.
  raw <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  expect_length(raw$scenarios, length(project$scenarios))
  expect_false(dir.exists(file.path(dir, "definitions")))
})

test_that("snapshotProject freezes unsaved in-memory edits", {
  project <- testProject()
  addScenario(project, "freshone", modelFile = "Aciclovir.pkml")
  dir <- withr::local_tempdir()

  # The snapshot reflects memory (including the unsaved scenario), not disk.
  out <- snapshotProject(project, dir = dir, name = "study")
  raw <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  snapshotScenarioNames <- vapply(raw$scenarios, \(s) s$name, character(1L))
  expect_true("freshone" %in% snapshotScenarioNames)
})

# --- restoreProject(snapshot, dir): restore IS the materialize ----------------

test_that("restoreProject writes a tree and returns the Project bound to dir", {
  out <- snapshotProject(
    testProject(),
    dir = withr::local_tempdir(),
    name = "study"
  )
  dir <- withr::local_tempdir()

  project <- restoreProject(out, dir)

  # The full tree project is written at `dir`: container + per-kind tree.
  expect_true(file.exists(file.path(dir, "Project.json")))
  expect_true(dir.exists(file.path(dir, "definitions", "scenarios")))
  # The returned project is bound to `dir`, dirty bit clear.
  expect_identical(
    project$projectDirPath,
    dirname(fs::path_abs(
      file.path(dir, "Project.json")
    ))
  )
  expect_false(project$.isModified())
})

test_that("restoreProject's tree reloads via loadProject identically", {
  source <- exampleProject()
  out <- snapshotProject(source, dir = withr::local_tempdir(), name = "study")
  dir <- withr::local_tempdir()

  project <- restoreProject(out, dir)
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

test_that("a restored project saves edits to dir like any tree", {
  out <- snapshotProject(
    testProject(),
    dir = withr::local_tempdir(),
    name = "study"
  )
  dir <- withr::local_tempdir()
  project <- restoreProject(out, dir)
  before <- names(project$scenarios)
  scenarioDir <- file.path(dir, "definitions", "scenarios")

  addScenario(project, "freshone", modelFile = "Aciclovir.pkml")
  # The edit stays in memory until saveProject().
  expect_false(file.exists(file.path(scenarioDir, "freshone.json")))
  saveProject(project)
  expect_true(file.exists(file.path(scenarioDir, "freshone.json")))
  reloaded <- loadProject(file.path(dir, "Project.json"))
  expect_named(
    reloaded$scenarios,
    c(before, "freshone"),
    ignore.order = TRUE
  )

  removeScenario(project, "freshone")
  saveProject(project)
  expect_false(file.exists(file.path(scenarioDir, "freshone.json")))
})

test_that("restoreProject reads a .esqlabsR snapshot", {
  out <- snapshotProject(
    testProject(),
    dir = withr::local_tempdir(),
    name = "study"
  )
  expect_identical(fs::path_ext(out), "esqlabsR")

  project <- restoreProject(out, withr::local_tempdir())
  expect_named(
    project$scenarios,
    names(testProject()$scenarios),
    ignore.order = TRUE
  )
})

test_that("restoreProject still reads a plain inlined Project.json (back-compat)", {
  # importProjectFromExcel() writes a single inlined Project.json;
  # restoreProject() must still accept that legacy form as the snapshot to
  # materialize.
  source <- testProject()
  legacyDir <- withr::local_tempdir()
  legacy <- file.path(legacyDir, "Project.json")
  .saveProjectJson(source, legacy, includeScenarios = TRUE)

  dir <- withr::local_tempdir()
  project <- restoreProject(legacy, dir)
  expect_true(dir.exists(file.path(dir, "definitions", "scenarios")))
  expect_named(
    project$scenarios,
    names(source$scenarios),
    ignore.order = TRUE
  )
})

test_that("restoreProject creates dir when absent", {
  out <- snapshotProject(
    testProject(),
    dir = withr::local_tempdir(),
    name = "study"
  )
  dir <- file.path(withr::local_tempdir(), "new", "nested")
  expect_false(dir.exists(dir))

  project <- restoreProject(out, dir)
  expect_true(dir.exists(file.path(dir, "definitions", "scenarios")))
  expect_identical(
    project$projectDirPath,
    dirname(fs::path_abs(
      file.path(dir, "Project.json")
    ))
  )
})

test_that("restoreProject refuses a non-empty dir without overwrite", {
  out <- snapshotProject(
    testProject(),
    dir = withr::local_tempdir(),
    name = "study"
  )
  # `dir` already contains a tree project, so materializing into it would
  # clobber the existing work; refuse rather than silently overwrite.
  dir <- testProject()$projectDirPath

  expect_error(restoreProject(out, dir), "already contains an esqlabsR project")
})

test_that("restoreProject with overwrite = TRUE rolls back in place and warns", {
  # Build a working tree, snapshot it, then diverge it and roll back.
  source <- testProject()
  out <- snapshotProject(source, dir = withr::local_tempdir(), name = "study")
  dir <- withr::local_tempdir()
  restoreProject(out, dir)

  # An in-place rollback replaces the existing tree and warns about stale
  # handles on the overwrite action.
  expect_snapshot(
    rolledBack <- restoreProject(out, dir, overwrite = TRUE),
    transform = .redactTmpDir
  )
  expect_named(
    rolledBack$scenarios,
    names(source$scenarios),
    ignore.order = TRUE
  )
})

test_that("snapshot -> restore -> snapshot is a fixed point over .esqlabsR", {
  project <- testProject()

  out1 <- snapshotProject(project, dir = withr::local_tempdir(), name = "study")
  expect_identical(fs::path_ext(out1), "esqlabsR")
  reloaded <- restoreProject(out1, withr::local_tempdir())

  out2 <- snapshotProject(
    reloaded,
    dir = withr::local_tempdir(),
    name = "study"
  )
  # Byte-stable git diffs are the stated design goal, so assert byte equality,
  # not just structural equality: the two snapshots must be identical line for
  # line.
  expect_identical(readLines(out1), readLines(out2))
})

test_that("snapshot byte-identity fixed point holds for the plots trio", {
  # testProject() carries no plots, so the byte-identity test above never
  # exercises the three sections reshaped most by the refactor. exampleProject()
  # populates the data-combined / plots / plot-grids tree, so guard all three
  # kinds are on disk then assert a byte-stable snapshot round-trip over them.
  project <- exampleProject()
  treeDir <- file.path(project$projectDirPath, "definitions")
  for (kind in c("data-combined", "plots", "plot-grids")) {
    expect_gt(length(list.files(file.path(treeDir, kind))), 0L)
  }

  out1 <- snapshotProject(project, dir = withr::local_tempdir(), name = "study")
  reloaded <- restoreProject(out1, withr::local_tempdir())
  out2 <- snapshotProject(
    reloaded,
    dir = withr::local_tempdir(),
    name = "study"
  )
  # Byte-stable snapshot for a project whose plots trio is populated.
  expect_identical(readLines(out1), readLines(out2))
})

test_that("snapshot preserves metadata and the filePaths/excel split", {
  project <- exampleProject()

  out <- snapshotProject(project, dir = withr::local_tempdir(), name = "study")
  restored <- restoreProject(out, withr::local_tempdir())

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

test_that("restoreProject errors on a non-existent snapshot file", {
  # The message carries the (per-run) path, so match the stable sentence.
  dir <- withr::local_tempdir()
  expect_error(
    restoreProject(file.path(dir, "missing.esqlabsR"), withr::local_tempdir()),
    "File not found"
  )
})

# --- migration of a legacy inlined single-file project ------------------------

test_that("restoreProject migrates a legacy inlined Project.json end to end", {
  # The public migration path for a handed-over single-file project is
  # restoreProject(snapshot, dir): it explodes the inlined snapshot into a tree.
  source <- exampleProject()
  legacyDir <- withr::local_tempdir()
  legacy <- file.path(legacyDir, "Project.json")
  .saveProjectJson(source, legacy, includeScenarios = TRUE)

  dir <- withr::local_tempdir()
  restoreProject(legacy, dir)
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
# which the definition tree (keyed by canonical id) cannot store. restoreProject()
# must canonicalize on the way in, lossless across every section: definitions
# AND the references that point at them (a scenario id used by a plot's
# dataCombined row and by a PI task / output mapping) are lowercased together,
# so the migrated tree's foreign keys still resolve.
test_that("restoreProject migrates a non-canonical legacy Project.json losslessly", {
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
  expect_no_error(restoreProject(legacy, dir))
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
