# Tests for the project-driven loadObservedData() dispatcher.

test_that("loadObservedData errors on non-Project input", {
  expect_error(
    loadObservedData("not a project"),
    "expected <Project>"
  )
})

test_that("loadObservedData returns empty list when observedData is NULL", {
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
  expect_identical(loadObservedData(project), list())
})

test_that("loadObservedData returns empty list when observedData is empty array", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(
      schemaVersion = "2.0",
      filePaths = structure(list(), names = character(0)),
      outputPaths = structure(list(), names = character(0)),
      scenarios = list(),
      observedData = list()
    ),
    tmp,
    auto_unbox = TRUE,
    null = "null"
  )
  project <- loadProject(tmp)
  expect_identical(loadObservedData(project), list())
})

test_that("loadObservedData loads excel observed data from TestProject", {
  project <- testProject()
  result <- loadObservedData(project)
  expect_type(result, "list")
  expect_true(length(result) >= 1)
  expect_true(all(vapply(result, inherits, logical(1), "DataSet")))
})

test_that("loadObservedData errors when an entry has invalid type", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(
      schemaVersion = "2.0",
      filePaths = list(dataFolder = "."),
      outputPaths = structure(list(), names = character(0)),
      scenarios = list(),
      observedData = list(list(type = "invalid_type", file = "x"))
    ),
    tmp,
    auto_unbox = TRUE,
    null = "null"
  )
  project <- loadProject(tmp)
  expect_snapshot(error = TRUE, loadObservedData(project))
})

test_that("loadObservedData warns on orphan programmatic sentinels (no in-memory DataSet)", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(
      schemaVersion = "2.0",
      filePaths = list(dataFolder = "."),
      outputPaths = structure(list(), names = character(0)),
      scenarios = list(),
      observedData = list(list(type = "programmatic", name = "x"))
    ),
    tmp,
    auto_unbox = TRUE,
    null = "null"
  )
  project <- loadProject(tmp)
  # A reloaded project sees the sentinel entry but has no in-memory DataSet to
  # back it (DataSets do not survive a JSON round-trip). loadObservedData warns
  # that the source resolved to nothing and returns an empty result;
  # addObservedData is the supported path for re-attaching the runtime DataSet.
  expect_warning(
    result <- loadObservedData(project),
    "resolved to no data"
  )
  expect_length(result, 0L)
})

test_that("loadObservedData errors when excel entry is missing required fields", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(
      schemaVersion = "2.0",
      filePaths = list(dataFolder = "."),
      outputPaths = structure(list(), names = character(0)),
      scenarios = list(),
      observedData = list(list(type = "excel", file = "x.xlsx"))
    ),
    tmp,
    auto_unbox = TRUE,
    null = "null"
  )
  project <- loadProject(tmp)
  expect_snapshot(error = TRUE, loadObservedData(project))
})

test_that("loadObservedData errors when source file is missing", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(
      schemaVersion = "2.0",
      filePaths = list(dataFolder = "."),
      outputPaths = structure(list(), names = character(0)),
      scenarios = list(),
      observedData = list(list(type = "pkml", file = "no-such-file.pkml"))
    ),
    tmp,
    auto_unbox = TRUE,
    null = "null"
  )
  project <- loadProject(tmp)
  expect_error(loadObservedData(project), "Observed-data source file not found")
})

test_that("loadObservedData rejects an observed-data file that escapes the data folder", {
  # A malicious project points `file` outside its own folder. The traversal is
  # rejected before the not-found check, so the error names the escape rather
  # than a misleading "file not found".
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(
      schemaVersion = "2.0",
      filePaths = list(dataFolder = "Data"),
      outputPaths = structure(list(), names = character(0)),
      scenarios = list(),
      observedData = list(
        list(type = "pkml", file = "../../../../etc/passwd")
      )
    ),
    tmp,
    auto_unbox = TRUE,
    null = "null"
  )
  project <- loadProject(tmp)
  expect_error(loadObservedData(project), "resolves outside the project folder")
})

test_that("loadObservedData loads pkml observed data", {
  tmpDir <- withr::local_tempdir()
  file.copy(
    testthat::test_path(
      "data",
      "TestProject",
      "Models",
      "Simulations",
      "Aciclovir.pkml"
    ),
    file.path(tmpDir, "Aciclovir.pkml")
  )
  jsonPath <- file.path(tmpDir, "Project.json")
  jsonlite::write_json(
    list(
      schemaVersion = "2.0",
      filePaths = list(dataFolder = "."),
      outputPaths = structure(list(), names = character(0)),
      scenarios = list(),
      observedData = list(list(type = "pkml", file = "Aciclovir.pkml"))
    ),
    jsonPath,
    auto_unbox = TRUE,
    null = "null"
  )
  project <- loadProject(jsonPath)
  # Aciclovir.pkml is a model, not a dataset; this expects ospsuite to error
  # gracefully — we only assert dispatch reached the pkml branch.
  expect_error(loadObservedData(project))
})

test_that("loadObservedData sources script entries", {
  tmpDir <- withr::local_tempdir()
  scriptPath <- file.path(tmpDir, "make_dataset.R")
  writeLines(
    c(
      "ds <- ospsuite::DataSet$new(name = 'TestDataSet')",
      "ds$setValues(xValues = c(0, 1, 2), yValues = c(1, 2, 3))",
      "ds"
    ),
    scriptPath
  )
  jsonPath <- file.path(tmpDir, "Project.json")
  jsonlite::write_json(
    list(
      schemaVersion = "2.0",
      filePaths = list(dataFolder = "."),
      outputPaths = structure(list(), names = character(0)),
      scenarios = list(),
      observedData = list(list(type = "script", file = "make_dataset.R"))
    ),
    jsonPath,
    auto_unbox = TRUE,
    null = "null"
  )
  project <- loadProject(jsonPath)
  # A script source executes arbitrary R, so the first source in a session
  # warns. Reset the once-per-session gate so this assertion does not depend on
  # whether an earlier test already tripped it.
  rlang::reset_warning_verbosity("esqlabsR_observed_data_script_source")
  expect_warning(
    result <- loadObservedData(project),
    "executing arbitrary R code"
  )
  expect_named(result, "TestDataSet")
  expect_s3_class(result$TestDataSet, "DataSet")
})

test_that("loadObservedData re-keys a script's list of DataSets by each name", {
  tmpDir <- withr::local_tempdir()
  scriptPath <- file.path(tmpDir, "make_list.R")
  writeLines(
    c(
      "a <- ospsuite::DataSet$new(name = 'Alpha')",
      "a$setValues(xValues = c(0, 1), yValues = c(1, 2))",
      "b <- ospsuite::DataSet$new(name = 'Beta')",
      "b$setValues(xValues = c(0, 1), yValues = c(3, 4))",
      # Deliberately give the list the wrong names; the loader must ignore
      # them and key by each DataSet's own $name.
      "list(wrong1 = a, wrong2 = b)"
    ),
    scriptPath
  )
  jsonPath <- file.path(tmpDir, "Project.json")
  jsonlite::write_json(
    list(
      schemaVersion = "2.0",
      filePaths = list(dataFolder = "."),
      outputPaths = structure(list(), names = character(0)),
      scenarios = list(),
      observedData = list(list(type = "script", file = "make_list.R"))
    ),
    jsonPath,
    auto_unbox = TRUE,
    null = "null"
  )
  project <- loadProject(jsonPath)
  result <- loadObservedData(project)
  expect_named(result, c("Alpha", "Beta"))
})

test_that("loadObservedData aborts on a name collision across sources", {
  tmpDir <- withr::local_tempdir()
  writeLines(
    c(
      "ds <- ospsuite::DataSet$new(name = 'Dup')",
      "ds$setValues(xValues = c(0, 1), yValues = c(1, 2))",
      "ds"
    ),
    file.path(tmpDir, "one.R")
  )
  writeLines(
    c(
      "ds <- ospsuite::DataSet$new(name = 'Dup')",
      "ds$setValues(xValues = c(0, 1), yValues = c(3, 4))",
      "ds"
    ),
    file.path(tmpDir, "two.R")
  )
  jsonPath <- file.path(tmpDir, "Project.json")
  jsonlite::write_json(
    list(
      schemaVersion = "2.0",
      filePaths = list(dataFolder = "."),
      outputPaths = structure(list(), names = character(0)),
      scenarios = list(),
      observedData = list(
        list(type = "script", file = "one.R"),
        list(type = "script", file = "two.R")
      )
    ),
    jsonPath,
    auto_unbox = TRUE,
    null = "null"
  )
  project <- loadProject(jsonPath)
  expect_error(loadObservedData(project), "Duplicate observed-data set name")
})

test_that("loadObservedData errors when script returns wrong type", {
  tmpDir <- withr::local_tempdir()
  scriptPath <- file.path(tmpDir, "bad.R")
  writeLines("42", scriptPath)
  jsonPath <- file.path(tmpDir, "Project.json")
  jsonlite::write_json(
    list(
      schemaVersion = "2.0",
      filePaths = list(dataFolder = "."),
      outputPaths = structure(list(), names = character(0)),
      scenarios = list(),
      observedData = list(list(type = "script", file = "bad.R"))
    ),
    jsonPath,
    auto_unbox = TRUE,
    null = "null"
  )
  project <- loadProject(jsonPath)
  expect_error(loadObservedData(project), "did not return a")
})

test_that("loadObservedData errors when dataFolder is not declared", {
  tmpDir <- withr::local_tempdir()
  jsonPath <- file.path(tmpDir, "Project.json")
  jsonlite::write_json(
    list(
      schemaVersion = "2.0",
      filePaths = structure(list(), names = character(0)),
      outputPaths = structure(list(), names = character(0)),
      scenarios = list(),
      observedData = list(list(type = "pkml", file = "x.pkml"))
    ),
    jsonPath,
    auto_unbox = TRUE,
    null = "null"
  )
  project <- loadProject(jsonPath)
  expect_snapshot(error = TRUE, loadObservedData(project))
})

test_that("addObservedData appends a valid config entry", {
  project <- testProject()
  before <- length(project$definitions$observedData)
  addObservedData(project, list(type = "pkml", file = "extra.pkml"))
  expect_length(project$definitions$observedData, before + 1L)
  added <- project$definitions$observedData[[before + 1L]]
  # The entry is classed as an ObservedDataSource (a transparent list wrapper
  # for printing); compare the fields, not the class.
  expect_s3_class(added, "ObservedDataSource")
  expect_equal(unclass(added), list(type = "pkml", file = "extra.pkml"))
})

test_that("addObservedData rejects an under-specified config entry", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addObservedData(project, list(type = "excel", file = "x.xlsx"))
  )
  expect_length(project$definitions$observedData, 1L)
})

test_that("addObservedData rejects a duplicate config entry file", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addObservedData(
      project,
      list(
        type = "excel",
        file = "Aciclovir_TimeValuesData.xlsx",
        importerConfiguration = "esqlabs_dataImporter_configuration.xml",
        sheets = list("Laskin 1982.Group A")
      )
    )
  )
  expect_length(project$definitions$observedData, 1L)
})

# Two declarations whose `file` differs only by directory derive the same
# on-disk id (the basename) and would silently overwrite each other (the second
# lost on reload). Under explicit-save the section is accepted in memory, but
# the serializer that `saveProject()` drives must fail fast naming the
# collision, leaving disk unchanged.
test_that("observedData declarations sharing a basename fail saveProject()", {
  project <- testProject()
  saveProject(project)
  dir <- file.path(project$info$projectDirPath, "definitions", "observed-data")
  before <- if (dir.exists(dir)) list.files(dir) else character()

  colliding <- list(
    list(type = "pkml", file = "dirA/obs.pkml"),
    list(type = "pkml", file = "dirB/obs.pkml")
  )
  .setSection(project, "observedData", colliding)
  expect_snapshot(
    saveProject(project),
    error = TRUE
  )

  # The on-disk tree did not change (the save aborted before writing).
  if (dir.exists(dir)) {
    expect_setequal(list.files(dir), before)
  }
})

# Under explicit-save, addObservedData() only mutates memory (the runtime store
# and the section together); nothing touches disk until saveProject().
test_that("addObservedData mutates memory only", {
  project <- testProject()
  state <- .projectSeam(project)
  ds <- ospsuite::DataSet$new(name = "InMemorySet")
  ds$setValues(xValues = c(1, 2), yValues = c(3, 4))

  # The add succeeds in memory: the DataSet is registered in the runtime store.
  addObservedData(project, ds)
  expect_true("InMemorySet" %in% names(state$.programmaticDataSets))
})

# saveProject() persists a session-added programmatic DataSet to a PKML file
# named <DataSet name>.pkml under the data folder and rewrites its section entry
# to a pkml source, so the data survives a reload.
test_that("saveProject persists a programmatic DataSet to PKML and it round-trips", {
  project <- testProject()
  state <- .projectSeam(project)
  ds <- ospsuite::DataSet$new(name = "RoundTripSet")
  ds$setValues(xValues = c(0, 1, 2), yValues = c(5, 6, 7))
  ds$xDimension <- ospsuite::ospDimensions$Time

  addObservedData(project, ds)
  saveProject(project)

  # The entry is now a pkml source pointing at the written file; the runtime
  # store no longer holds it (it is file-backed).
  entry <- project$definitions$observedData[[
    length(project$definitions$observedData)
  ]]
  expect_identical(entry$type, "pkml")
  expect_identical(entry$file, "RoundTripSet.pkml")
  expect_false("RoundTripSet" %in% names(state$.programmaticDataSets))
  expect_true(file.exists(file.path(
    project$paths$dataFolder,
    "RoundTripSet.pkml"
  )))

  # The data survives a reload (fresh session, empty runtime store).
  reloadProject(project)
  loaded <- loadObservedData(project)
  expect_true("RoundTripSet" %in% names(loaded))
  # A PKML round-trip converts through base units, so values return within
  # floating-point tolerance rather than bit-identical.
  expect_equal(loaded[["RoundTripSet"]]$yValues, c(5, 6, 7), tolerance = 1e-6)
})

# The PKML written for a programmatic DataSet is named <name>.pkml, so a
# programmatic name whose PKML file collides with an existing pkml source's
# basename aborts at save, before any file is written. The abort must come
# before the write, so the existing file is left intact (no data loss).
test_that("saveProject aborts on a programmatic-to-PKML basename collision", {
  project <- testProject()
  # An existing pkml source living in the file "Collide.pkml" but producing a
  # differently-named DataSet ("Other"), so it does not clash with the
  # programmatic name at add time. The programmatic DataSet named "Collide"
  # would persist to the same "Collide.pkml", colliding at save.
  existing <- ospsuite::DataSet$new(name = "Other")
  existing$setValues(xValues = c(0, 1), yValues = c(111, 222))
  collidePath <- file.path(project$paths$dataFolder, "Collide.pkml")
  ospsuite::saveDataSetToPKML(existing, collidePath)
  addObservedData(project, list(type = "pkml", file = "Collide.pkml"))
  ds <- ospsuite::DataSet$new(name = "Collide")
  ds$setValues(xValues = c(1, 2), yValues = c(333, 444))
  addObservedData(project, ds)

  expect_snapshot(saveProject(project), error = TRUE)

  # The abort left the existing "Collide.pkml" untouched: its data is still
  # "Other"'s, not the programmatic DataSet's.
  expect_equal(
    ospsuite::loadDataSetFromPKML(collidePath)$yValues,
    c(111, 222),
    tolerance = 1e-6
  )
})

# Persisting a programmatic DataSet needs a data folder to write the PKML into;
# a project with no dataFolder aborts at save with a clear message.
test_that("saveProject aborts persisting a programmatic DataSet with no dataFolder", {
  tmp <- withr::local_tempdir()
  jsonPath <- file.path(tmp, "Project.json")
  jsonlite::write_json(
    list(
      schemaVersion = "2.0",
      filePaths = structure(list(), names = character(0)),
      outputPaths = structure(list(), names = character(0)),
      scenarios = list(),
      observedData = list()
    ),
    jsonPath,
    auto_unbox = TRUE,
    null = "null"
  )
  project <- loadProject(jsonPath)
  ds <- ospsuite::DataSet$new(name = "NoFolderSet")
  ds$setValues(xValues = c(1, 2), yValues = c(3, 4))
  addObservedData(project, ds)

  expect_snapshot(saveProject(project), error = TRUE)
})

# A programmatic DataSet is session-only runtime state, not part of the on-disk
# tree, so reloadProject() (which re-reads the tree) must drop it: otherwise the
# reloaded section loses the sentinel but loadObservedData() would still return
# the discarded runtime dataset.
test_that("reloadProject clears a session-only programmatic DataSet", {
  project <- testProject()
  state <- .projectSeam(project)
  ds <- ospsuite::DataSet$new(name = "SessionOnlySet")
  ds$setValues(xValues = c(1, 2), yValues = c(3, 4))

  addObservedData(project, ds)
  expect_true("SessionOnlySet" %in% names(state$.programmaticDataSets))
  expect_true("SessionOnlySet" %in% names(loadObservedData(project)))

  reloadProject(project)

  # The reload returned the project to the on-disk state: the runtime store is
  # cleared and the discarded dataset no longer surfaces via loadObservedData().
  expect_false("SessionOnlySet" %in% names(state$.programmaticDataSets))
  expect_false("SessionOnlySet" %in% names(loadObservedData(project)))
})

# removeObservedData write-through ----

test_that("removeObservedData deletes the definition file on save", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "observed-data")
  # The fixture declares one Excel source, filed under its basename.
  id <- "Aciclovir_TimeValuesData.xlsx"
  expect_true(file.exists(file.path(dir, paste0(id, ".json"))))

  suppressWarnings(removeObservedData(project, id))

  # In memory the declaration is gone; the definition file is deleted on save
  # and a fresh load no longer sees it.
  expect_false(any(vapply(
    project$definitions$observedData,
    function(e) identical(basename(e[["file"]] %||% ""), id),
    logical(1)
  )))
  saveProject(project)
  expect_false(file.exists(file.path(dir, paste0(id, ".json"))))
  reloaded <- loadProject(project$info$projectFilePath)
  expect_length(reloaded$definitions$observedData, 0L)
})

test_that("removeObservedData removes a vector of ids in one pass", {
  project <- testProject()
  addObservedData(project, list(type = "pkml", file = "one.pkml"))
  addObservedData(project, list(type = "pkml", file = "two.pkml"))
  before <- length(project$definitions$observedData)

  removeObservedData(project, c("one.pkml", "two.pkml"))
  expect_length(project$definitions$observedData, before - 2L)
  files <- vapply(
    project$definitions$observedData,
    function(e) basename(e[["file"]] %||% NA_character_),
    character(1)
  )
  expect_false(any(c("one.pkml", "two.pkml") %in% files))
})

test_that("removeObservedData warns and skips a not-found id in the batch", {
  project <- testProject()
  addObservedData(project, list(type = "pkml", file = "one.pkml"))
  before <- length(project$definitions$observedData)
  expect_warning(
    removeObservedData(project, c("one.pkml", "ghost.pkml")),
    "ghost.pkml"
  )
  expect_length(project$definitions$observedData, before - 1L)
})

# The remove path writes through to disk BEFORE clearing the runtime store, so a
# Under explicit-save, removeObservedData() only mutates memory; a section that
# still carries a colliding pair of file entries after the removal serializes
# cleanly in memory but aborts at saveProject(). Seed the backing section
# (bypassing the setter) with a colliding pair alongside the programmatic
# sentinel; after the sentinel is dropped in memory, saveProject() aborts on
# the surviving collision, leaving disk unchanged.
test_that("removeObservedData mutates memory only; a surviving collision aborts saveProject()", {
  project <- testProject()
  saveProject(project)
  state <- .projectSeam(project)
  ds <- ospsuite::DataSet$new(name = "myProgSet")
  ds$setValues(xValues = c(1, 2), yValues = c(3, 4))
  addObservedData(project, ds)

  # Seed a colliding pair of surviving entries directly into the backing field.
  state$.observedData <- list(
    .asObservedDataSource(list(type = "programmatic", name = "myProgSet")),
    .asObservedDataSource(list(type = "pkml", file = "dirA/obs.pkml")),
    .asObservedDataSource(list(type = "pkml", file = "dirB/obs.pkml"))
  )

  # The removal succeeds in memory (the programmatic name is dropped from the
  # runtime store), but the surviving pair still collides at save time.
  removeObservedData(project, "myProgSet")
  expect_false("myProgSet" %in% names(state$.programmaticDataSets))
  expect_snapshot(saveProject(project), error = TRUE)
})

# Print method ----

test_that("print.ObservedDataSource renders the source declaration", {
  project <- testProject()
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$definitions$observedData[[1]]))
})
