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
      observedData = list(list(id = "x", type = "invalid_type", file = "x"))
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
      observedData = list(list(id = "x", type = "programmatic"))
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
      observedData = list(list(id = "x", type = "excel", file = "x.xlsx"))
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
      observedData = list(
        list(id = "no_such_file", type = "pkml", file = "no-such-file.pkml")
      )
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
        list(id = "passwd", type = "pkml", file = "../../../../etc/passwd")
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
      observedData = list(
        list(id = "aciclovir", type = "pkml", file = "Aciclovir.pkml")
      )
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

test_that("loadObservedData aborts on a name collision across sources", {
  tmpDir <- withr::local_tempdir()
  # Two PKML files, each holding a DataSet named "Dup".
  for (i in 1:2) {
    ds <- ospsuite::DataSet$new(name = "Dup")
    ds$setValues(xValues = c(0, 1), yValues = c(i, i + 1))
    ospsuite::saveDataSetToPKML(
      ds,
      file.path(tmpDir, paste0("dup", i, ".pkml"))
    )
  }
  jsonPath <- file.path(tmpDir, "Project.json")
  jsonlite::write_json(
    list(
      schemaVersion = "2.0",
      filePaths = list(dataFolder = "."),
      outputPaths = structure(list(), names = character(0)),
      scenarios = list(),
      observedData = list(
        list(id = "dup1", type = "pkml", file = "dup1.pkml"),
        list(id = "dup2", type = "pkml", file = "dup2.pkml")
      )
    ),
    jsonPath,
    auto_unbox = TRUE,
    null = "null"
  )
  project <- loadProject(jsonPath)
  expect_error(
    loadObservedData(project),
    "Duplicate observed-data set name"
  )
})

test_that("loadObservedData aborts on two session DataSets renamed to one name", {
  project <- testProject()
  plasma <- ospsuite::DataSet$new(name = "Plasma cohort A")
  plasma$setValues(xValues = c(0, 1), yValues = c(1, 2))
  urine <- ospsuite::DataSet$new(name = "Urine cohort A")
  urine$setValues(xValues = c(0, 1), yValues = c(3, 4))
  suppressMessages({
    addObservedData(project, plasma)
    addObservedData(project, urine)
  })
  urine$name <- "Plasma cohort A"

  expect_error(
    suppressWarnings(loadObservedData(project)),
    "Duplicate observed-data set name"
  )
})

test_that("loadObservedData returns a session DataSet under its current name", {
  project <- testProject()
  plasma <- ospsuite::DataSet$new(name = "Plasma cohort A")
  plasma$setValues(xValues = c(0, 1), yValues = c(1, 2))
  suppressMessages(addObservedData(project, plasma))
  plasma$name <- "Plasma cohort A, corrected"

  expect_no_warning(loaded <- loadObservedData(project))
  expect_identical(
    names(loaded),
    c(testObservedDataId, "Plasma cohort A, corrected")
  )
  expect_identical(loaded[["Plasma cohort A, corrected"]], plasma)
})

test_that("addObservedData rejects a script entry", {
  project <- testProject()
  expect_snapshot(
    addObservedData(project, list(type = "script", file = "make_dataset.R")),
    error = TRUE
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
      observedData = list(list(id = "x", type = "pkml", file = "x.pkml"))
    ),
    jsonPath,
    auto_unbox = TRUE,
    null = "null"
  )
  project <- loadProject(jsonPath)
  expect_snapshot(error = TRUE, loadObservedData(project))
})

test_that(".observedDataIdFromFile takes the canonical file stem without a warning", {
  expect_no_warning(
    ids <- c(
      .observedDataIdFromFile("Aciclovir_TimeValuesData.xlsx"),
      .observedDataIdFromFile("sub/My Data, v2.xlsx")
    )
  )
  expect_identical(ids, c("aciclovir_timevaluesdata", "my_data__v2"))
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
  expect_equal(
    unclass(added),
    list(id = "extra", type = "pkml", file = "extra.pkml")
  )
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

# Adding a DataSet without reading the other sources ----

# Declare a `pkml` source in the project's data folder holding one DataSet
# named "Digitized plasma". Returns the path of its PKML file.
.addDigitizedPkmlSource <- function(project) {
  ds <- ospsuite::DataSet$new(name = "Digitized plasma")
  ds$setValues(xValues = c(0, 1, 2), yValues = c(0, 4.1, 2.3))
  filePath <- file.path(project$paths$dataFolder, "digitized_plasma.pkml")
  ospsuite::saveDataSetToPKML(ds, filePath)
  addObservedData(project, list(type = "pkml", file = "digitized_plasma.pkml"))
  filePath
}

test_that("addObservedData with a DataSet reads no other source's file", {
  project <- testProject()
  # A source whose PKML file is not exported yet. Adding it also clears the
  # data-set names found when the project was loaded.
  addObservedData(
    project,
    list(type = "pkml", file = "cohort_b_export.pkml")
  )
  # Without a data folder, the Excel source cannot be found either.
  project$paths$dataFolder <- NULL

  expect_snapshot(
    addObservedData(
      project,
      ospsuite::DataSet$new(name = "Scratch measurement")
    )
  )
})

test_that("addObservedData checks a DataSet name against the names already loaded", {
  project <- testProject()
  pkmlPath <- .addDigitizedPkmlSource(project)
  suppressWarnings(suppressMessages(getObservedDataNames(project)))
  # Reading the source again would now fail.
  unlink(pkmlPath)

  suppressMessages(addObservedData(
    project,
    ospsuite::DataSet$new(name = "Scratch measurement")
  ))
  expect_identical(
    getObservedDataNames(project),
    c(testObservedDataId, "Digitized plasma", "Scratch measurement")
  )
  expect_snapshot(
    error = TRUE,
    addObservedData(
      project,
      ospsuite::DataSet$new(name = "Digitized plasma"),
      id = "digitized_plasma_rerun"
    )
  )
})

test_that("addObservedData aborts on the name of a DataSet added in this session", {
  project <- testProject()
  suppressMessages(addObservedData(
    project,
    ospsuite::DataSet$new(name = "Scratch measurement")
  ))
  # Adding a source clears the data-set names found so far; the DataSets added
  # in this session are still known.
  addObservedData(
    project,
    list(type = "pkml", file = "cohort_b_export.pkml")
  )

  expect_snapshot(
    error = TRUE,
    addObservedData(
      project,
      ospsuite::DataSet$new(name = "Scratch measurement"),
      id = "scratch_rerun"
    )
  )
})

test_that("a DataSet name known under another id aborts, also with overwrite", {
  project <- testProject()
  plasma <- ospsuite::DataSet$new(name = "Plasma cohort A")
  plasma$setValues(xValues = c(0, 1), yValues = c(1, 2))
  suppressMessages(addObservedData(project, plasma, id = "pilot"))
  rerun <- ospsuite::DataSet$new(name = "Plasma cohort A")
  rerun$setValues(xValues = c(0, 1), yValues = c(3, 4))

  expect_snapshot(error = TRUE, {
    addObservedData(project, rerun, id = "main")
    addObservedData(project, rerun, id = "main", overwrite = TRUE)
  })
  expect_identical(
    names(project$definitions$observedData),
    c("aciclovir_timevaluesdata", "pilot")
  )
})

test_that("addObservedData refuses a DataSet saved earlier under its id", {
  # Saving turns the DataSet into the `pkml` source `scratch_measurement`.
  # Adding a source clears the data-set names, and this one's file does not
  # exist, so no read can fill them again: only that id shows the clash.
  project <- testProject()
  addObservedData(
    project,
    list(type = "pkml", file = "cohort_b_export.pkml")
  )
  scratch <- ospsuite::DataSet$new(name = "Scratch measurement")
  scratch$setValues(xValues = c(0, 1, 3), yValues = c(0, 6, 2))
  suppressMessages(addObservedData(project, scratch))
  suppressMessages(saveProject(project))

  expect_snapshot(error = TRUE, {
    addObservedData(project, scratch)
    addObservedData(project, scratch, overwrite = TRUE)
  })
})

test_that("addObservedData refuses a DataSet whose PKML file another source reads", {
  # Saving the DataSet would write `<id>.pkml` into the data folder, the data
  # file of a source filed under another id.
  project <- testProject()
  addObservedData(
    project,
    list(id = "cohort_b_lab", type = "pkml", file = "cohort_b.pkml")
  )

  expect_snapshot(error = TRUE, {
    addObservedData(project, ospsuite::DataSet$new(name = "Cohort B"))
    addObservedData(
      project,
      ospsuite::DataSet$new(name = "Cohort B"),
      overwrite = TRUE
    )
  })
})

test_that("addObservedData compares data files by where they lie", {
  project <- testProject()
  dataFolder <- project$paths$dataFolder
  # Other spellings of `<id>.pkml` in the data folder, read by sources filed
  # under other ids. On Windows and macOS a name differing only in case is the
  # same file.
  sameFile <- list(
    "Cohort C" = "./cohort_c.pkml",
    "Cohort D" = "sub/../cohort_d.pkml",
    "Cohort E" = file.path(dataFolder, "cohort_e.pkml"),
    "Cohort F" = "COHORT_F.PKML"
  )
  for (name in names(sameFile)) {
    addObservedData(
      project,
      list(type = "pkml", file = sameFile[[name]]),
      id = sub("cohort ", "lab_", tolower(name))
    )
  }
  # Different files: one in a subfolder, one unrelated, one outside the data
  # folder (which the next load reports, not the add).
  addObservedData(
    project,
    list(id = "lab_g", type = "pkml", file = "sub/cohort_g.pkml")
  )
  addObservedData(
    project,
    list(id = "infusion", type = "pkml", file = "Infusion study.pkml")
  )
  addObservedData(
    project,
    list(id = "shared", type = "pkml", file = "../shared/cohort_h.pkml")
  )

  for (name in names(sameFile)) {
    expect_error(
      addObservedData(project, ospsuite::DataSet$new(name = name)),
      "overwrite the data file of another source",
      info = name
    )
  }
  for (name in c("Cohort G", "Cohort H")) {
    expect_no_error(suppressMessages(
      addObservedData(project, ospsuite::DataSet$new(name = name))
    ))
  }
})

test_that("a DataSet name inside a PKML source not loaded yet aborts the next load", {
  # The add cannot see the name without reading the source, so it is accepted
  # and the next load reports the duplicate.
  project <- testProject()
  .addDigitizedPkmlSource(project)
  suppressMessages(addObservedData(
    project,
    ospsuite::DataSet$new(name = "Digitized plasma"),
    id = "digitized_plasma_rerun"
  ))

  expect_snapshot(
    error = TRUE,
    suppressWarnings(suppressMessages(loadObservedData(project)))
  )
})

test_that("addObservedData overwrite = TRUE replaces a config entry in place", {
  project <- testProject()
  addObservedData(project, list(type = "pkml", file = "sub1/obs.pkml"))
  before <- length(project$definitions$observedData)
  # A second entry with the same file name takes the same id and replaces the
  # first, keeping the section length unchanged and taking the new directory.
  addObservedData(
    project,
    list(type = "pkml", file = "sub2/obs.pkml"),
    overwrite = TRUE
  )
  expect_length(project$definitions$observedData, before)
  expect_identical(
    unclass(project$definitions$observedData[["obs"]]),
    list(id = "obs", type = "pkml", file = "sub2/obs.pkml")
  )
})

# Files with one name, in other folders or with another extension, give one id,
# so the second source needs an `id` of its own.
test_that("sources whose files share a name take one id, and the second add aborts", {
  project <- testProject()
  addObservedData(project, list(type = "pkml", file = "dirA/obs.pkml"))
  addObservedData(
    project,
    list(
      type = "excel",
      file = "data.xlsx",
      importerConfiguration = "esqlabs_dataImporter_configuration.xml",
      sheets = list("Laskin 1982.Group A")
    )
  )

  expect_snapshot(error = TRUE, {
    addObservedData(project, list(type = "pkml", file = "dirB/obs.pkml"))
    addObservedData(project, list(type = "pkml", file = "data.pkml"))
  })
  addObservedData(
    project,
    list(type = "pkml", file = "data.pkml"),
    id = "data_pkml"
  )
  expect_identical(
    names(project$definitions$observedData),
    c("aciclovir_timevaluesdata", "obs", "data", "data_pkml")
  )
})

# Under explicit-save, addObservedData() only mutates memory (the runtime store
# and the section together); nothing touches disk until saveProject().
test_that("addObservedData mutates memory only", {
  project <- testProject()
  state <- .projectSeam(project)
  ds <- ospsuite::DataSet$new(name = "InMemorySet")
  ds$setValues(xValues = c(1, 2), yValues = c(3, 4))

  # The add succeeds in memory: the DataSet is registered in the runtime store
  # under its source id.
  addObservedData(project, ds)
  expect_identical(state$.programmaticDataSets, list(inmemoryset = ds))
})

# saveProject() persists a session-added programmatic DataSet to a PKML file
# named <id>.pkml under the data folder and rewrites its section entry to a pkml
# source, so the data survives a reload under the DataSet's own name.
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
  expect_identical(
    unclass(project$definitions$observedData[["roundtripset"]]),
    list(id = "roundtripset", type = "pkml", file = "roundtripset.pkml")
  )
  expect_length(state$.programmaticDataSets, 0L)
  expect_true(file.exists(file.path(
    project$paths$dataFolder,
    "roundtripset.pkml"
  )))

  # The data survives a reload (fresh session, empty runtime store).
  project <- loadProject(project$info$projectFilePath)
  loaded <- loadObservedData(project)
  expect_true("RoundTripSet" %in% names(loaded))
  # A PKML round-trip converts through base units, so values return within
  # floating-point tolerance rather than bit-identical.
  expect_equal(loaded[["RoundTripSet"]]$yValues, c(5, 6, 7), tolerance = 1e-6)
})

# The PKML written for a programmatic DataSet is named <id>.pkml, so a DataSet
# whose PKML file is the data file of a pkml source added later aborts at save,
# before any file is written. The abort must come before the write, so the
# existing file is left intact (no data loss).
test_that("saveProject aborts on a programmatic-to-PKML file collision", {
  project <- testProject()
  # The DataSet "Collide" would persist to "collide.pkml", the file of a pkml
  # source producing a differently-named DataSet ("Other"). The pkml source is
  # added after the DataSet, so the add of the DataSet cannot see its file; the
  # save is the last place to catch the collision.
  ds <- ospsuite::DataSet$new(name = "Collide")
  ds$setValues(xValues = c(1, 2), yValues = c(333, 444))
  addObservedData(project, ds)
  existing <- ospsuite::DataSet$new(name = "Other")
  existing$setValues(xValues = c(0, 1), yValues = c(111, 222))
  collidePath <- file.path(project$paths$dataFolder, "collide.pkml")
  ospsuite::saveDataSetToPKML(existing, collidePath)
  addObservedData(
    project,
    list(type = "pkml", file = "collide.pkml"),
    id = "other"
  )

  expect_snapshot(saveProject(project), error = TRUE)

  # The abort left the existing "collide.pkml" untouched: its data is still
  # "Other"'s, not the programmatic DataSet's.
  expect_equal(
    ospsuite::loadDataSetFromPKML(collidePath)$yValues,
    c(111, 222),
    tolerance = 1e-6
  )
})

test_that("saveProject keeps the data file of a source filed under another id", {
  project <- testProject()
  # The DataSet is added before the source that reads "cohort_b.pkml", so the
  # add cannot see the clash; the save is the last place to catch it.
  replacement <- ospsuite::DataSet$new(name = "Cohort B")
  replacement$setValues(xValues = c(0, 1), yValues = c(999, 999))
  suppressMessages(addObservedData(project, replacement))
  original <- ospsuite::DataSet$new(name = "Cohort B lab")
  original$setValues(xValues = c(0, 1), yValues = c(111, 222))
  cohortPath <- file.path(project$paths$dataFolder, "cohort_b.pkml")
  ospsuite::saveDataSetToPKML(original, cohortPath)
  addObservedData(
    project,
    list(id = "cohort_b_lab", type = "pkml", file = "cohort_b.pkml")
  )

  expect_snapshot(saveProject(project), error = TRUE)
  expect_equal(
    ospsuite::loadDataSetFromPKML(cohortPath)$yValues,
    c(111, 222),
    tolerance = 1e-6
  )
})

test_that("saveProject compares data files by where they lie", {
  # Each source reads `<id>.pkml` from the data folder under another spelling.
  # The DataSet is added first, so only the save can catch it.
  spellings <- c(
    "./cohort_b.pkml",
    "sub/../cohort_b.pkml",
    "<dataFolder>/cohort_b.pkml",
    "COHORT_B.PKML"
  )
  for (spelling in spellings) {
    project <- testProject()
    dataFolder <- project$paths$dataFolder
    file <- sub("<dataFolder>", dataFolder, spelling, fixed = TRUE)
    original <- ospsuite::DataSet$new(name = "Cohort B lab")
    original$setValues(xValues = c(0, 1), yValues = c(111, 222))
    existingPath <- file.path(dataFolder, basename(file))
    ospsuite::saveDataSetToPKML(original, existingPath)
    checksum <- tools::md5sum(existingPath)
    replacement <- ospsuite::DataSet$new(name = "Cohort B")
    replacement$setValues(xValues = c(0, 1), yValues = c(999, 999))
    suppressMessages(addObservedData(project, replacement))
    addObservedData(
      project,
      list(id = "cohort_b_lab", type = "pkml", file = file)
    )

    expect_error(
      saveProject(project),
      "overwrite the data file of another source",
      info = spelling
    )
    expect_identical(tools::md5sum(existingPath), checksum, info = spelling)
  }
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

# The id of a DataSet is made a safe file name like every other id, so a unit in
# its name does not stop the save; the DataSet keeps its name in the PKML file.
test_that("a DataSet whose name is not a safe file name is saved under a safe id", {
  project <- testProject()
  ds <- ospsuite::DataSet$new(name = "Plasma 2.5 mg/kg")
  ds$setValues(xValues = c(1, 2), yValues = c(3, 4))
  expect_no_warning(suppressMessages(addObservedData(project, ds)))
  saveProject(project)

  pkmlPath <- file.path(project$paths$dataFolder, "plasma_2.5_mg_kg.pkml")
  expect_identical(
    unclass(project$definitions$observedData[["plasma_2.5_mg_kg"]]),
    list(
      id = "plasma_2.5_mg_kg",
      type = "pkml",
      file = "plasma_2.5_mg_kg.pkml"
    )
  )
  expect_identical(
    ospsuite::loadDataSetFromPKML(pkmlPath)$name,
    "Plasma 2.5 mg/kg"
  )
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
  hostile$steadyState <- TRUE
  hostile$steadyStateTimeUnit <- NULL
  scenarios[[hostileId]] <- hostile
  .setSection(project, "scenarios", scenarios)

  expect_error(saveProject(project), "steadyStateTimeUnit")
  # The DataSet is still in the runtime store, so it is not lost.
  expect_identical(state$.programmaticDataSets, list(recoverableset = ds))
})

test_that("removeObservedData drops the DataSet of a source a failed save already rewrote", {
  # The save wrote the PKML and rewrote the entry as a `pkml` source, then
  # aborted, so the DataSet is still held in the session.
  project <- testProject()
  state <- .projectSeam(project)
  ds <- ospsuite::DataSet$new(name = "RecoverableSet")
  ds$setValues(xValues = c(1, 2), yValues = c(3, 4))
  suppressMessages(addObservedData(project, ds))
  .persistProgrammaticObservedData(project, state)
  expect_identical(
    project$definitions$observedData[["recoverableset"]]$type,
    "pkml"
  )

  removeObservedData(project, "recoverableset")

  expect_length(state$.programmaticDataSets, 0L)
  expect_false(
    "RecoverableSet" %in%
      suppressWarnings(suppressMessages(getObservedDataNames(project)))
  )
})

test_that("saveProject refuses a DataSet that shares its id with another source before writing it", {
  # The section is set directly: adding the second source is refused.
  project <- testProject()
  cohort <- ospsuite::DataSet$new(name = "Cohort B")
  cohort$setValues(xValues = c(0, 1), yValues = c(1, 2))
  suppressMessages(addObservedData(project, cohort))
  .setSection(
    project,
    "observedData",
    c(
      .getSection(project, "observedData"),
      list(list(id = "cohort_b", type = "pkml", file = "other/cohort_b.pkml"))
    )
  )

  expect_snapshot(saveProject(project), error = TRUE)
  expect_false(
    file.exists(file.path(project$paths$dataFolder, "cohort_b.pkml"))
  )
})

# removeObservedData write-through ----

test_that("removeObservedData deletes the definition file on save", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "observed-data")
  # The fixture declares one Excel source.
  id <- "aciclovir_timevaluesdata"
  expect_true(file.exists(file.path(dir, paste0(id, ".json"))))

  suppressWarnings(removeObservedData(project, id))

  # In memory the declaration is gone; the definition file is deleted on save
  # and a fresh load no longer sees it.
  expect_length(project$definitions$observedData, 0L)
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

  removeObservedData(project, c("one", "two"))
  expect_identical(
    names(project$definitions$observedData),
    "aciclovir_timevaluesdata"
  )
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
  expect_false(file.exists(file.path(dir, "demo.json")))
  reloaded <- loadProject(project$info$projectFilePath)
  expect_true("obs_demo" %in% names(reloaded$definitions$observedData))

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
  expect_identical(
    unclass(project$definitions$observedData[["obs"]]),
    list(id = "obs", type = "pkml", file = "b.pkml")
  )
})

test_that("addObservedData canonicalizes a declared id", {
  # A declared id names the declaration's definition file, and the Excel
  # importer already files the same declaration under the canonicalized id, so
  # hand-authoring has to reach the same one.
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "observed-data")

  expect_warning(
    addObservedData(
      project,
      list(
        id = "TestProject_TimeValuesData",
        type = "pkml",
        file = "TimeValues.pkml"
      )
    ),
    "Canonicalized"
  )
  expect_true(
    "testproject_timevaluesdata" %in%
      names(project$definitions$observedData)
  )

  saveProject(project)
  expect_true("testproject_timevaluesdata.json" %in% list.files(dir))
  expect_false("TestProject_TimeValuesData.json" %in% list.files(dir))

  # The canonical id is the handle the removal matches on.
  removeObservedData(project, "testproject_timevaluesdata")
  saveProject(project)
  expect_false("testproject_timevaluesdata.json" %in% list.files(dir))
})

test_that("the id argument wins over the entry's id, which wins over the source", {
  project <- testProject()
  expect_snapshot({
    addObservedData(
      project,
      list(id = "laskin_entry", type = "pkml", file = "Laskin IV.pkml"),
      id = "laskin_argument"
    )
    addObservedData(
      project,
      list(id = "laskin_entry", type = "pkml", file = "Laskin IV.pkml")
    )
    addObservedData(project, list(type = "pkml", file = "sub/Cohort B.pkml"))
    addObservedData(
      project,
      ospsuite::DataSet$new(name = "Scratch measurement")
    )
    addObservedData(
      project,
      ospsuite::DataSet$new(name = "Pilot measurement"),
      id = "pilot"
    )
  })

  expect_identical(
    lapply(
      .unwrapDefinitionList(project$definitions$observedData),
      unclass
    )[-1],
    list(
      laskin_argument = list(
        id = "laskin_argument",
        type = "pkml",
        file = "Laskin IV.pkml"
      ),
      laskin_entry = list(
        id = "laskin_entry",
        type = "pkml",
        file = "Laskin IV.pkml"
      ),
      cohort_b = list(
        id = "cohort_b",
        type = "pkml",
        file = "sub/Cohort B.pkml"
      ),
      scratch_measurement = list(
        id = "scratch_measurement",
        type = "programmatic"
      ),
      pilot = list(id = "pilot", type = "programmatic")
    )
  )
})

test_that("an id you give warns once when it is canonicalized, a taken one never", {
  project <- testProject()
  expect_snapshot({
    addObservedData(
      project,
      list(type = "pkml", file = "laskin_iv.pkml"),
      id = "Laskin IV"
    )
    addObservedData(
      project,
      list(id = "Laskin PO", type = "pkml", file = "laskin_po.pkml")
    )
  })
  expect_no_warning(suppressMessages({
    addObservedData(project, list(type = "pkml", file = "sub/Cohort B.pkml"))
    addObservedData(
      project,
      ospsuite::DataSet$new(name = "Scratch measurement")
    )
  }))
  expect_identical(
    names(project$definitions$observedData),
    c(
      "aciclovir_timevaluesdata",
      "laskin_iv",
      "laskin_po",
      "cohort_b",
      "scratch_measurement"
    )
  )
})

test_that("a source read from the project and passed back replaces itself", {
  project <- testProject()
  scratch <- ospsuite::DataSet$new(name = "Scratch measurement")
  scratch$setValues(xValues = c(0, 1, 3), yValues = c(0, 6, 2))
  suppressMessages(addObservedData(project, scratch))
  addObservedData(project, list(type = "pkml", file = "Laskin IV.pkml"))

  laskin <- project$definitions$observedData[["laskin_iv"]]
  laskin$file <- "sub/Laskin IV.pkml"
  aciclovir <- project$definitions$observedData[["aciclovir_timevaluesdata"]]
  aciclovir$sheets <- list("Laskin 1982.Group A", "Laskin 1982.Group B")
  expect_no_warning({
    addObservedData(project, laskin, overwrite = TRUE)
    addObservedData(project, aciclovir, overwrite = TRUE)
  })

  expect_identical(
    lapply(.unwrapDefinitionList(project$definitions$observedData), unclass),
    list(
      aciclovir_timevaluesdata = list(
        id = "aciclovir_timevaluesdata",
        type = "excel",
        file = "Aciclovir_TimeValuesData.xlsx",
        importerConfiguration = "esqlabs_dataImporter_configuration.xml",
        sheets = list("Laskin 1982.Group A", "Laskin 1982.Group B")
      ),
      scratch_measurement = list(
        id = "scratch_measurement",
        type = "programmatic"
      ),
      laskin_iv = list(
        id = "laskin_iv",
        type = "pkml",
        file = "sub/Laskin IV.pkml"
      )
    )
  )
})

test_that("addObservedData rejects an id that is not a single non-empty string", {
  project <- testProject()
  # An id names the declaration's file and is its remove handle, so a blank, NA,
  # or non-string one is a mistake to report, not a value to quietly ignore in
  # favor of the file name.
  for (bad in list("", NA_character_, 42, c("laskin_iv", "laskin_po"))) {
    expect_error(
      addObservedData(project, list(type = "pkml", file = "a.pkml"), id = bad),
      "must be a single non-empty string"
    )
    expect_error(
      addObservedData(
        project,
        ospsuite::DataSet$new(name = "Scratch measurement"),
        id = bad
      ),
      "must be a single non-empty string"
    )
  }
  expect_snapshot(error = TRUE, {
    addObservedData(project, list(id = 2024, type = "pkml", file = "a.pkml"))
    addObservedData(project, list(type = "pkml", file = 2024))
  })
  expect_length(project$definitions$observedData, 1L)
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

# Neither PKML file exists: the clash is found without reading.
test_that("a DataSet clashes with the id of another source", {
  project <- testProject()
  addObservedData(
    project,
    list(id = "cohort_b", type = "pkml", file = "cohort_b_export.pkml")
  )

  expect_snapshot(error = TRUE, {
    addObservedData(project, ospsuite::DataSet$new(name = "Cohort B"))
    addObservedData(project, ospsuite::DataSet$new(name = "cohort b"))
    # A file source with the id is not replaced by a DataSet.
    addObservedData(
      project,
      ospsuite::DataSet$new(name = "Cohort B"),
      overwrite = TRUE
    )
  })
})

test_that("removeObservedData warns and skips a not-found id in the batch", {
  project <- testProject()
  addObservedData(project, list(type = "pkml", file = "one.pkml"))
  before <- length(project$definitions$observedData)
  expect_warning(
    removeObservedData(project, c("one", "ghost")),
    "ghost"
  )
  expect_length(project$definitions$observedData, before - 1L)
})

test_that("removeObservedData finds a source by the DataSet name its id was taken from", {
  project <- testProject()
  state <- .projectSeam(project)
  suppressMessages(addObservedData(
    project,
    ospsuite::DataSet$new(name = "Scratch measurement")
  ))

  expect_snapshot(removeObservedData(project, "Scratch measurement"))
  expect_identical(
    names(project$definitions$observedData),
    "aciclovir_timevaluesdata"
  )
  expect_length(state$.programmaticDataSets, 0L)
})

# Removing a source referenced by a dataCombined ----

# A dataCombined refers to a data set by its name, so the removal learns the
# names of the removed source's data sets: from the DataSet held in the session,
# or by reading the source's file.

# Add a DataSet named "MyObs" under the id "pilot", and a dataCombined "dc1"
# that refers to the data set `dataSet`.
.addPilotWithReference <- function(project, dataSet = "MyObs") {
  ds <- ospsuite::DataSet$new(name = "MyObs")
  ds$setValues(xValues = c(0, 1, 2), yValues = c(10, 20, 30))
  suppressMessages(addObservedData(project, ds, id = "pilot"))
  addDataCombined(
    project,
    id = "dc1",
    observed = list(list(label = "Pilot", dataSet = dataSet))
  )
  ds
}

test_that("removing a saved DataSet source warns about a dataCombined that names it", {
  project <- testProject()
  .addPilotWithReference(project)
  suppressMessages(saveProject(project))
  reloaded <- suppressWarnings(loadProject(project$info$projectFilePath))

  expect_snapshot(removeObservedData(reloaded, "pilot"))
})

test_that("a dataCombined naming the source id instead of a data set gives no warning", {
  project <- testProject()
  .addPilotWithReference(project, dataSet = "pilot")

  expect_no_warning(removeObservedData(project, "pilot"))
  expect_identical(
    names(project$definitions$observedData),
    "aciclovir_timevaluesdata"
  )
})

test_that("only the source holding the named data set warns on removal", {
  project <- testProject()
  # A PKML file source filed as `cohort_b` whose data set has another name.
  lab <- ospsuite::DataSet$new(name = "Cohort B lab")
  lab$setValues(xValues = c(0, 1), yValues = c(1, 2))
  ospsuite::saveDataSetToPKML(
    lab,
    file.path(project$paths$dataFolder, "cohort_b.pkml")
  )
  addObservedData(project, list(type = "pkml", file = "cohort_b.pkml"))
  session <- ospsuite::DataSet$new(name = "Cohort B")
  session$setValues(xValues = c(0, 1), yValues = c(3, 4))
  suppressMessages(addObservedData(project, session, id = "lab"))
  addDataCombined(
    project,
    id = "cohort_b_plot",
    observed = list(list(label = "Cohort B", dataSet = "Cohort B"))
  )

  expect_no_warning(removeObservedData(project, "cohort_b"))
  expect_snapshot(removeObservedData(project, "lab"))
})

test_that("removing an Excel source warns once about every dataCombined naming its data sets", {
  project <- testProject()
  for (dc in c("laskin_iv", "laskin_iv_log")) {
    addDataCombined(
      project,
      id = dc,
      observed = list(list(label = "Laskin", dataSet = testObservedDataId))
    )
  }
  addDataCombined(
    project,
    id = "pilot_only",
    observed = list(list(label = "Pilot", dataSet = "MyObs"))
  )

  expect_snapshot(removeObservedData(project, "aciclovir_timevaluesdata"))
})

test_that("a source that cannot be read is removed without a reference warning", {
  project <- testProject()
  addObservedData(project, list(type = "pkml", file = "cohort_b.pkml"))
  addDataCombined(
    project,
    id = "cohort_b_plot",
    observed = list(list(label = "Cohort B", dataSet = "Cohort B"))
  )

  expect_no_warning(expect_no_error(removeObservedData(project, "cohort_b")))
  expect_identical(
    names(project$definitions$observedData),
    "aciclovir_timevaluesdata"
  )
})

test_that("a removal reads no source when no dataCombined names a data set", {
  project <- testProject()
  addObservedData(project, list(type = "pkml", file = "cohort_b.pkml"))
  reads <- 0L
  local_mocked_bindings(
    .loadObservedPkml = function(entry, dataFolder) {
      reads <<- reads + 1L
      list()
    }
  )

  expect_no_warning(removeObservedData(project, "cohort_b"))
  expect_identical(reads, 0L)
})

test_that("a DataSet renamed after the add is matched by its current name", {
  project <- testProject()
  ds <- .addPilotWithReference(project, dataSet = "Renamed")
  ds$name <- "Renamed"

  expect_snapshot(removeObservedData(project, "pilot"))
})

test_that("removing two referenced sources in one call warns once per source", {
  project <- testProject()
  .addPilotWithReference(project)
  main <- ospsuite::DataSet$new(name = "Main study")
  main$setValues(xValues = c(0, 1), yValues = c(5, 6))
  suppressMessages(addObservedData(project, main, id = "main"))
  addDataCombined(
    project,
    id = "dc2",
    observed = list(list(label = "Main", dataSet = "Main study"))
  )

  expect_snapshot(removeObservedData(project, c("pilot", "main")))
})

test_that("a programmatic source without its DataSet gives no reference warning", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "observed-data")
  jsonlite::write_json(
    list(id = "scratch_measurement", type = "programmatic"),
    file.path(dir, "scratch_measurement.json"),
    auto_unbox = TRUE
  )
  project <- loadProject(project$info$projectFilePath)
  addDataCombined(
    project,
    id = "scratch_plot",
    observed = list(
      list(label = "Scratch", dataSet = "Scratch measurement"),
      list(label = "Scratch id", dataSet = "scratch_measurement")
    )
  )

  expect_no_warning(removeObservedData(project, "scratch_measurement"))
})

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
  state$.observedData <- .observedDataSectionById(list(
    list(id = "myprogset", type = "programmatic"),
    list(id = "obs", type = "pkml", file = "dirA/obs.pkml"),
    list(id = "obs", type = "pkml", file = "dirB/obs.pkml")
  ))

  # The removal succeeds in memory (the DataSet is dropped from the runtime
  # store), but the surviving pair still collides at save time.
  removeObservedData(project, "myprogset")
  expect_length(state$.programmaticDataSets, 0L)
  expect_snapshot(saveProject(project), error = TRUE)
})

# Stored ids ----

test_that("every observed-data source stores its id first and the section is keyed by it", {
  project <- testProject()
  addObservedData(project, list(type = "pkml", file = "sub/Cohort B.pkml"))
  suppressMessages(addObservedData(
    project,
    ospsuite::DataSet$new(name = "Scratch measurement")
  ))

  expect_identical(
    lapply(.unwrapDefinitionList(project$definitions$observedData), unclass),
    list(
      "aciclovir_timevaluesdata" = list(
        id = "aciclovir_timevaluesdata",
        type = "excel",
        file = "Aciclovir_TimeValuesData.xlsx",
        importerConfiguration = "esqlabs_dataImporter_configuration.xml",
        sheets = list("Laskin 1982.Group A")
      ),
      "cohort_b" = list(
        id = "cohort_b",
        type = "pkml",
        file = "sub/Cohort B.pkml"
      ),
      "scratch_measurement" = list(
        id = "scratch_measurement",
        type = "programmatic"
      )
    )
  )
  expect_s3_class(
    project$definitions$observedData[["cohort_b"]],
    "ObservedDataSource"
  )
  # Keyed by id, the section is still read-only.
  expect_snapshot(
    error = TRUE,
    project$definitions$observedData[["cohort_b"]]$file <- "other.pkml"
  )
})

test_that("saveProject writes the id first into every definition file and a DataSet keeps its id", {
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "observed-data")
  addObservedData(project, list(type = "pkml", file = "sub/Cohort B.pkml"))
  scratch <- ospsuite::DataSet$new(name = "Scratch measurement")
  scratch$setValues(xValues = c(0, 1, 3), yValues = c(0, 6, 2))
  suppressMessages(addObservedData(project, scratch))

  saveProject(project)

  files <- sort(list.files(dir))
  expect_identical(
    files,
    c(
      "aciclovir_timevaluesdata.json",
      "cohort_b.json",
      "scratch_measurement.json"
    )
  )
  expect_identical(
    lapply(file.path(dir, files), jsonlite::read_json),
    list(
      list(
        id = "aciclovir_timevaluesdata",
        type = "excel",
        file = "Aciclovir_TimeValuesData.xlsx",
        importerConfiguration = "esqlabs_dataImporter_configuration.xml",
        sheets = list("Laskin 1982.Group A")
      ),
      list(id = "cohort_b", type = "pkml", file = "sub/Cohort B.pkml"),
      list(
        id = "scratch_measurement",
        type = "pkml",
        file = "scratch_measurement.pkml"
      )
    )
  )

  # Removed by the same id after the save, and after a reload.
  reloaded <- loadProject(project$info$projectFilePath)
  for (p in list(project, reloaded)) {
    removeObservedData(p, "scratch_measurement")
    expect_identical(
      names(p$definitions$observedData),
      c("aciclovir_timevaluesdata", "cohort_b")
    )
  }
  saveProject(reloaded)
  expect_false(file.exists(file.path(dir, "scratch_measurement.json")))
})

test_that("an observed-data record without an id in a project file aborts the load", {
  for (record in list(
    list(type = "pkml", file = "cohort_b.pkml"),
    list(type = "programmatic", name = "Scratch measurement")
  )) {
    tmp <- withr::local_tempfile(fileext = ".json")
    jsonlite::write_json(
      list(
        schemaVersion = "2.0",
        filePaths = list(dataFolder = "."),
        outputPaths = structure(list(), names = character(0)),
        scenarios = list(),
        observedData = list(record)
      ),
      tmp,
      auto_unbox = TRUE,
      null = "null"
    )

    expect_snapshot(loadProject(tmp), error = TRUE)
  }
})

test_that("an inline project file stores observed data as an array of records with ids", {
  project <- testProject()
  suppressMessages(addObservedData(
    project,
    ospsuite::DataSet$new(name = "Scratch measurement")
  ))

  path <- file.path(withr::local_tempdir(), "Project.json")
  .saveProjectJson(project, path)
  written <- jsonlite::read_json(path)

  # An array: no names, each record carrying its own `id`.
  expect_identical(
    written$observedData,
    list(
      list(
        id = "aciclovir_timevaluesdata",
        type = "excel",
        file = "Aciclovir_TimeValuesData.xlsx",
        importerConfiguration = "esqlabs_dataImporter_configuration.xml",
        sheets = list("Laskin 1982.Group A")
      ),
      list(id = "scratch_measurement", type = "programmatic")
    )
  )
})

test_that("a DataSet replaces a programmatic source with no data only with overwrite", {
  # A source with no data, as a project file written before saving leaves it.
  project <- testProject()
  dir <- file.path(project$info$projectDirPath, "definitions", "observed-data")
  jsonlite::write_json(
    list(id = "scratch_measurement", type = "programmatic"),
    file.path(dir, "scratch_measurement.json"),
    auto_unbox = TRUE
  )
  project <- loadProject(project$info$projectFilePath)
  scratch <- ospsuite::DataSet$new(name = "Scratch measurement")
  scratch$setValues(xValues = c(0, 1, 3), yValues = c(0, 6, 2))

  expect_snapshot(error = TRUE, addObservedData(project, scratch))

  suppressMessages(addObservedData(project, scratch, overwrite = TRUE))
  expect_identical(
    names(project$definitions$observedData),
    c("aciclovir_timevaluesdata", "scratch_measurement")
  )
  saveProject(project)
  expect_identical(
    sort(list.files(dir)),
    c("aciclovir_timevaluesdata.json", "scratch_measurement.json")
  )
  expect_identical(
    jsonlite::read_json(file.path(dir, "scratch_measurement.json")),
    list(
      id = "scratch_measurement",
      type = "pkml",
      file = "scratch_measurement.pkml"
    )
  )
})

test_that("a DataSet saved again under its id rewrites its own PKML file", {
  project <- testProject()
  pkmlPath <- file.path(project$paths$dataFolder, "scratch_measurement.pkml")
  first <- ospsuite::DataSet$new(name = "Scratch measurement")
  first$setValues(xValues = c(0, 1, 3), yValues = c(0, 6, 2))
  suppressMessages(addObservedData(project, first))
  saveProject(project)

  # Replaced in the same session, and again after a reload.
  second <- ospsuite::DataSet$new(name = "Scratch measurement")
  second$setValues(xValues = c(0, 1, 3), yValues = c(0, 7, 3))
  removeObservedData(project, "scratch_measurement")
  suppressMessages(addObservedData(project, second))
  expect_no_error(saveProject(project))
  expect_equal(
    ospsuite::loadDataSetFromPKML(pkmlPath)$yValues,
    c(0, 7, 3),
    tolerance = 1e-6
  )

  reloaded <- loadProject(project$info$projectFilePath)
  third <- ospsuite::DataSet$new(name = "Scratch measurement")
  third$setValues(xValues = c(0, 1, 3), yValues = c(0, 8, 4))
  removeObservedData(reloaded, "scratch_measurement")
  suppressMessages(addObservedData(reloaded, third))
  expect_no_error(saveProject(reloaded))
  expect_equal(
    ospsuite::loadDataSetFromPKML(pkmlPath)$yValues,
    c(0, 8, 4),
    tolerance = 1e-6
  )
})

# Two DataSets whose names differ only in case take one id.
test_that("a DataSet whose name gives the id of another DataSet aborts", {
  project <- testProject()
  suppressMessages(addObservedData(
    project,
    ospsuite::DataSet$new(name = "Cohort")
  ))

  expect_snapshot(
    error = TRUE,
    addObservedData(project, ospsuite::DataSet$new(name = "cohort"))
  )
})

# Replacing a DataSet source must not leave two sources providing the new name,
# or another source's PKML file overwritten.
test_that("a DataSet overwrite is refused when another source provides its name", {
  project <- testProject()
  state <- .projectSeam(project)
  upper <- ospsuite::DataSet$new(name = "Cohort B")
  upper$setValues(xValues = c(0, 1), yValues = c(1, 2))
  suppressMessages(addObservedData(project, upper))
  # A PKML source holding a data set named "cohort b", known once loaded.
  exported <- ospsuite::DataSet$new(name = "cohort b")
  exported$setValues(xValues = c(0, 1), yValues = c(5, 6))
  ospsuite::saveDataSetToPKML(
    exported,
    file.path(project$paths$dataFolder, "cohort_b_export.pkml")
  )
  addObservedData(project, list(type = "pkml", file = "cohort_b_export.pkml"))
  getObservedDataNames(project)
  lower <- ospsuite::DataSet$new(name = "cohort b")
  lower$setValues(xValues = c(0, 1), yValues = c(3, 4))

  expect_snapshot(
    error = TRUE,
    addObservedData(project, lower, overwrite = TRUE)
  )
  expect_identical(
    names(project$definitions$observedData),
    c("aciclovir_timevaluesdata", "cohort_b", "cohort_b_export")
  )
  expect_identical(state$.programmaticDataSets, list(cohort_b = upper))
})

test_that("a DataSet overwrite replaces the session DataSet and its name", {
  project <- testProject()
  state <- .projectSeam(project)
  upper <- ospsuite::DataSet$new(name = "Cohort B")
  upper$setValues(xValues = c(0, 1), yValues = c(1, 2))
  suppressMessages(addObservedData(project, upper))
  getObservedDataNames(project)
  corrected <- ospsuite::DataSet$new(name = "Cohort B, corrected")
  corrected$setValues(xValues = c(0, 1), yValues = c(3, 4))

  suppressMessages(addObservedData(
    project,
    corrected,
    id = "cohort_b",
    overwrite = TRUE
  ))

  expect_identical(state$.programmaticDataSets, list(cohort_b = corrected))
  expect_identical(
    getObservedDataNames(project),
    c(testObservedDataId, "Cohort B, corrected")
  )
})

test_that("a DataSet overwrite is refused when another source reads its PKML file", {
  project <- testProject()
  state <- .projectSeam(project)
  upper <- ospsuite::DataSet$new(name = "Cohort B")
  upper$setValues(xValues = c(0, 1), yValues = c(1, 2))
  suppressMessages(addObservedData(project, upper))
  addObservedData(
    project,
    list(id = "cohort_b_lab", type = "pkml", file = "Cohort_B.pkml")
  )
  lower <- ospsuite::DataSet$new(name = "cohort b")
  lower$setValues(xValues = c(0, 1), yValues = c(3, 4))

  expect_snapshot(
    error = TRUE,
    addObservedData(project, lower, overwrite = TRUE)
  )
  expect_identical(
    names(project$definitions$observedData),
    c("aciclovir_timevaluesdata", "cohort_b", "cohort_b_lab")
  )
  expect_identical(state$.programmaticDataSets, list(cohort_b = upper))
})

# Print method ----

test_that("print.ObservedDataSource renders the source declaration", {
  project <- testProject()
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$definitions$observedData[[1]]))
})

# The section is stored as an unnamed list, so printing it showed the count with
# no ids underneath (#1281). It lists the same ids the authoring functions take.

test_that("printing the observedData section lists the declaration ids", {
  project <- testProject()
  suppressMessages(addObservedData(
    project,
    ospsuite::DataSet$new(name = "simulated_plasma")
  ))
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$definitions$observedData))
})
