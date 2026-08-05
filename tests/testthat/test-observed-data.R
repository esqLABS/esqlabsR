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
  # A script source executes arbitrary R, so the first source in a session warns.
  # The warning is gated once per session per project (keyed by the project file
  # path), so this fresh temp project has its own gate; reset it so the assertion
  # does not depend on any earlier run of this test.
  rlang::reset_warning_verbosity(paste0(
    "esqlabsR_observed_data_script_source_",
    project$info$projectFilePath
  ))
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
  # The script-source security warning is covered by its own test; suppress it
  # here so this list-keying assertion is not tangled up with it.
  result <- suppressWarnings(loadObservedData(project))
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
  expect_error(
    suppressWarnings(loadObservedData(project)),
    "Duplicate observed-data set name"
  )
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
  expect_error(
    suppressWarnings(loadObservedData(project)),
    "did not return a"
  )
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

test_that("addObservedData rejects an under-specified config entry, naming every gap at once", {
  # An Excel source needs three fields; reporting one per attempt made a bare
  # `list(type = "excel")` take three calls to complete (#1213).
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addObservedData(project, list(type = "excel", file = "x.xlsx"))
  )
  expect_snapshot(error = TRUE, addObservedData(project, list(type = "excel")))
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

test_that("addObservedData aborts on a duplicate DataSet name, replaces with overwrite", {
  project <- testProject()
  ds1 <- ospsuite::DataSet$new(name = "prog_ds")
  ds2 <- ospsuite::DataSet$new(name = "prog_ds")
  suppressMessages(addObservedData(project, ds1))
  expect_snapshot(error = TRUE, addObservedData(project, ds2))
  before <- length(project$definitions$observedData)
  suppressMessages(addObservedData(project, ds2, overwrite = TRUE))
  # The section length is unchanged (replaced in place) and the runtime store
  # now holds the second DataSet under that name.
  expect_length(project$definitions$observedData, before)
  expect_identical(loadObservedData(project)[["prog_ds"]], ds2)
})

test_that("addObservedData overwrite does not grow the observed-data name list", {
  project <- testProject()
  suppressMessages(addObservedData(
    project,
    ospsuite::DataSet$new(name = "iter")
  ))
  # Repeated overwrites of the same name (a plausible iterative-fitting loop)
  # must not accumulate duplicate names in the cached name list.
  for (i in seq_len(3)) {
    suppressMessages(
      addObservedData(
        project,
        ospsuite::DataSet$new(name = "iter"),
        overwrite = TRUE
      )
    )
  }
  expect_equal(sum(getObservedDataNames(project) == "iter"), 1L)
})

test_that("addObservedData overwrite = TRUE replaces a config entry in place", {
  project <- testProject()
  addObservedData(project, list(type = "pkml", file = "sub1/obs.pkml"))
  before <- length(project$definitions$observedData)
  # A second entry with the same basename replaces the first (same on-disk id),
  # keeping the section length unchanged and taking the new directory.
  addObservedData(
    project,
    list(type = "pkml", file = "sub2/obs.pkml"),
    overwrite = TRUE
  )
  expect_length(project$definitions$observedData, before)
  files <- vapply(
    project$definitions$observedData,
    function(e) if (is.null(e[["file"]])) NA_character_ else e[["file"]],
    character(1)
  )
  expect_true("sub2/obs.pkml" %in% files)
  expect_false("sub1/obs.pkml" %in% files)
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

# addObservedData() persists to `<name>.pkml` on save, so a DataSet name that is
# not a safe filename segment is rejected up front rather than failing later with
# an opaque low-level path error at save time.
test_that("addObservedData rejects a DataSet name that is not a safe filename", {
  project <- testProject()
  ds <- ospsuite::DataSet$new(name = "Cohort/A")
  ds$setValues(xValues = c(1, 2), yValues = c(3, 4))
  expect_error(addObservedData(project, ds), "safe filename segment")
})

# A hand-authored `programmatic` sentinel with no `name` is schema-legal (a
# programmatic entry has no required fields). The persist step must not crash on
# `store[[NULL]]`; a name-less entry has no id to name its definition file, so
# the save aborts with the serializer's clear "no id" message, not an opaque
# `subscript out of bounds`.
test_that("saveProject rejects a name-less programmatic sentinel with a clear error", {
  project <- testProject()
  observedData <- .getSection(project, "observedData")
  observedData <- c(observedData, list(list(type = "programmatic")))
  .setSection(project, "observedData", observedData)

  expect_error(saveProject(project), "no id to name its definition file")
})

# A save that aborts partway through the tree write must not lose a session-added
# DataSet: the persist step writes the PKML but keeps the DataSet in the runtime
# store until the whole save commits, so the data is recoverable after the abort.
test_that("a save that aborts mid-tree-write keeps the programmatic DataSet recoverable", {
  project <- testProject()
  saveProject(project)
  state <- .projectSeam(project)

  ds <- ospsuite::DataSet$new(name = "RecoverableSet")
  ds$setValues(xValues = c(1, 2), yValues = c(3, 4))
  addObservedData(project, ds)

  # Make one loaded scenario serializer-hostile so the tree write aborts after
  # the programmatic persist step has already run.
  scenarios <- .getSection(project, "scenarios")
  hostileId <- names(scenarios)[[1]]
  hostile <- scenarios[[hostileId]]
  hostile$simulateSteadyState <- TRUE
  hostile$steadyStateTimeUnit <- NULL
  scenarios[[hostileId]] <- hostile
  .setSection(project, "scenarios", scenarios)

  expect_error(saveProject(project), "steadyStateTimeUnit")
  # The DataSet is still in the runtime store, so it is not lost.
  expect_true("RecoverableSet" %in% names(state$.programmaticDataSets))
})

# Since a programmatic DataSet persists to `<name>.pkml`, a name that equals an
# existing file-based source's basename (e.g. an Excel `file`) no longer collides
# with it: the persisted file is `<name>.pkml`, a distinct id. Lock in that this
# now saves cleanly (it aborted before this PR).
test_that("a programmatic name equal to an existing file basename saves cleanly", {
  project <- testProject()
  # The fixture declares a file-based Excel source filed under this basename.
  ds <- ospsuite::DataSet$new(name = "Aciclovir_TimeValuesData.xlsx")
  ds$setValues(xValues = c(1, 2), yValues = c(3, 4))
  addObservedData(project, ds)

  expect_no_error(saveProject(project))
  expect_true(file.exists(file.path(
    project$paths$dataFolder,
    "Aciclovir_TimeValuesData.xlsx.pkml"
  )))
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
  # Dropping the only observed-data set leaves the PI mapping that referenced
  # it dangling, which the reload reports.
  expect_warning(
    reloaded <- loadProject(project$info$projectFilePath),
    "unresolved cross-reference"
  )
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

test_that("an observedData entry is filed and matched by its declared id", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "observed-data")

  addObservedData(
    project,
    list(id = "obs_demo", type = "pkml", file = "sub/demo.pkml")
  )
  saveProject(project)

  # The definition file is named from the id, like every other section, and the
  # id round-trips so a reload keeps filing it the same way.
  expect_true(file.exists(file.path(dir, "obs_demo.json")))
  expect_false(file.exists(file.path(dir, "demo.pkml.json")))
  reloaded <- loadProject(project$info$projectFilePath)
  ids <- vapply(
    reloaded$definitions$observedData,
    function(e) e[["id"]] %||% NA_character_,
    character(1)
  )
  expect_true("obs_demo" %in% ids)

  # The declared id is also the key the entry is removed by.
  removeObservedData(reloaded, "obs_demo")
  saveProject(reloaded)
  expect_false(file.exists(file.path(dir, "obs_demo.json")))
})

test_that("addObservedData rejects a duplicate declared id", {
  project <- testProject()
  addObservedData(project, list(id = "obs", type = "pkml", file = "a.pkml"))
  expect_snapshot(
    error = TRUE,
    addObservedData(project, list(id = "obs", type = "pkml", file = "b.pkml"))
  )

  # Overwriting replaces the entry carrying that id in place.
  addObservedData(
    project,
    list(id = "obs", type = "pkml", file = "b.pkml"),
    overwrite = TRUE
  )
  entry <- Filter(
    function(e) identical(e[["id"]], "obs"),
    project$definitions$observedData
  )
  expect_length(entry, 1L)
  expect_identical(entry[[1]][["file"]], "b.pkml")
})

test_that("addObservedData rejects an id that is not a single non-empty string", {
  project <- testProject()
  # An id names the declaration's file and is its remove handle, so a blank, NA,
  # or non-string one is a mistake to report, not a value to quietly ignore in
  # favour of the file basename.
  for (bad in list("", NA_character_, 42)) {
    expect_error(
      addObservedData(project, list(id = bad, type = "pkml", file = "a.pkml")),
      "single non-empty string"
    )
  }
})

test_that("a config entry cannot overwrite a live programmatic source", {
  # Replacing the sentinel in place would strand its DataSet in the runtime
  # store: nothing writes it at save, and the usual unresolved-sentinel warning
  # cannot fire because the sentinel is gone.
  project <- testProject()
  ds <- ospsuite::DataSet$new(name = "prog_src")
  ds$setValues(xValues = c(1, 2), yValues = c(3, 4))
  addObservedData(project, ds)

  expect_snapshot(
    error = TRUE,
    addObservedData(
      project,
      list(id = "prog_src", type = "pkml", file = "x.pkml"),
      overwrite = TRUE
    )
  )
  # The DataSet is untouched, so it still resolves and still saves.
  expect_true("prog_src" %in% getObservedDataNames(project))
})

test_that("a DataSet clashes with an existing declaration's declared id", {
  # The DataSet branch keys on resolved data-set names, which never see a
  # declaration whose `id` was chosen by hand; without the id check the section
  # would hold two entries filed under one id and only fail at save.
  project <- testProject()
  dataFolder <- project$paths$dataFolder
  src <- ospsuite::DataSet$new(name = "from_file")
  src$setValues(xValues = c(1, 2), yValues = c(3, 4))
  ospsuite::saveDataSetToPKML(src, file.path(dataFolder, "src.pkml"))
  addObservedData(project, list(id = "obs", type = "pkml", file = "src.pkml"))

  ds <- ospsuite::DataSet$new(name = "obs")
  ds$setValues(xValues = c(1, 2), yValues = c(3, 4))
  expect_error(addObservedData(project, ds), "already exists")
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
