# Tests for the project-driven loadObservedData() dispatcher.

test_that("loadObservedData errors on non-Project input", {
  expect_error(
    loadObservedData("not a project"),
    "expected <Project>"
  )
})

test_that("loadObservedData returns empty list when observedDataSources is NULL", {
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

test_that("loadObservedData returns empty list when observedDataSources is empty array", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(
      schemaVersion = "2.0",
      filePaths = structure(list(), names = character(0)),
      outputPaths = structure(list(), names = character(0)),
      scenarios = list(),
      observedDataSources = list()
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
      observedDataSources = list(list(type = "invalid_type", file = "x"))
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
      observedDataSources = list(list(type = "programmatic", name = "x"))
    ),
    tmp,
    auto_unbox = TRUE,
    null = "null"
  )
  project <- loadProject(tmp)
  # A reloaded project sees the sentinel entry but has no in-memory DataSet to
  # back it (DataSets do not survive a JSON round-trip). loadObservedData warns
  # that the source resolved to nothing and returns an empty result;
  # addObservedDataSource is the supported path for re-attaching the runtime DataSet.
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
      observedDataSources = list(list(type = "excel", file = "x.xlsx"))
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
      observedDataSources = list(list(
        type = "pkml",
        file = "no-such-file.pkml"
      ))
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
      observedDataSources = list(
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
      observedDataSources = list(list(type = "pkml", file = "Aciclovir.pkml"))
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
      observedDataSources = list(
        list(type = "pkml", file = "dup1.pkml"),
        list(type = "pkml", file = "dup2.pkml")
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

test_that("addObservedDataSource rejects a script entry", {
  project <- testProject()
  expect_snapshot(
    addObservedDataSource(
      project,
      list(type = "script", file = "make_dataset.R")
    ),
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
      observedDataSources = list(list(type = "pkml", file = "x.pkml"))
    ),
    jsonPath,
    auto_unbox = TRUE,
    null = "null"
  )
  project <- loadProject(jsonPath)
  expect_snapshot(error = TRUE, loadObservedData(project))
})

test_that("addObservedDataSource appends a valid config entry", {
  project <- testProject()
  before <- length(project$definitions$observedDataSources)
  addObservedDataSource(project, list(type = "pkml", file = "extra.pkml"))
  expect_length(project$definitions$observedDataSources, before + 1L)
  added <- project$definitions$observedDataSources[[before + 1L]]
  # The entry is classed as an ObservedDataSource (a transparent list wrapper
  # for printing); compare the fields, not the class.
  expect_s3_class(added, "ObservedDataSource")
  expect_equal(
    unclass(added),
    list(id = "extra.pkml", type = "pkml", file = "extra.pkml")
  )
})

test_that("addObservedDataSource rejects an under-specified config entry, naming every gap at once", {
  # An Excel source needs three fields; reporting one per attempt made a bare
  # `list(type = "excel")` take three calls to complete (#1213).
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addObservedDataSource(project, list(type = "excel", file = "x.xlsx"))
  )
  expect_snapshot(
    error = TRUE,
    addObservedDataSource(project, list(type = "excel"))
  )
  expect_length(project$definitions$observedDataSources, 1L)
})

test_that("addObservedDataSource rejects a duplicate config entry file", {
  project <- testProject()
  expect_snapshot(
    error = TRUE,
    addObservedDataSource(
      project,
      list(
        type = "excel",
        file = "Aciclovir_TimeValuesData.xlsx",
        importerConfiguration = "esqlabs_dataImporter_configuration.xml",
        sheets = list("Laskin 1982.Group A")
      )
    )
  )
  expect_length(project$definitions$observedDataSources, 1L)
})

test_that("addObservedDataSource aborts on a duplicate DataSet name, replaces with overwrite", {
  project <- testProject()
  ds1 <- ospsuite::DataSet$new(name = "prog_ds")
  ds2 <- ospsuite::DataSet$new(name = "prog_ds")
  suppressMessages(addObservedDataSource(project, ds1))
  expect_snapshot(error = TRUE, addObservedDataSource(project, ds2))
  before <- length(project$definitions$observedDataSources)
  suppressMessages(addObservedDataSource(project, ds2, overwrite = TRUE))
  # The section length is unchanged (replaced in place) and the runtime store
  # now holds the second DataSet under that name.
  expect_length(project$definitions$observedDataSources, before)
  expect_identical(loadObservedData(project)[["prog_ds"]], ds2)
})

test_that("addObservedDataSource overwrite does not grow the observed-data name list", {
  project <- testProject()
  suppressMessages(addObservedDataSource(
    project,
    ospsuite::DataSet$new(name = "iter")
  ))
  # Repeated overwrites of the same name (a plausible iterative-fitting loop)
  # must not accumulate duplicate names in the cached name list.
  for (i in seq_len(3)) {
    suppressMessages(
      addObservedDataSource(
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
  addObservedDataSource(
    project,
    list(type = "pkml", file = "digitized_plasma.pkml")
  )
  filePath
}

test_that("addObservedDataSource with a DataSet reads no other source's file", {
  project <- testProject()
  # A source whose PKML file is not exported yet. Adding it also clears the
  # data-set names found when the project was loaded.
  addObservedDataSource(
    project,
    list(type = "pkml", file = "cohort_b_export.pkml")
  )
  # Without a data folder, the Excel source cannot be found either.
  project$paths$dataFolder <- NULL

  expect_snapshot(
    addObservedDataSource(
      project,
      ospsuite::DataSet$new(name = "Scratch measurement")
    )
  )
})

test_that("addObservedDataSource checks a DataSet name against the names already loaded", {
  project <- testProject()
  pkmlPath <- .addDigitizedPkmlSource(project)
  suppressWarnings(suppressMessages(getObservedDataNames(project)))
  # Reading the source again would now fail.
  unlink(pkmlPath)

  suppressMessages(addObservedDataSource(
    project,
    ospsuite::DataSet$new(name = "Scratch measurement")
  ))
  expect_identical(
    getObservedDataNames(project),
    c(testObservedDataId, "Digitized plasma", "Scratch measurement")
  )
  expect_snapshot(
    error = TRUE,
    addObservedDataSource(
      project,
      ospsuite::DataSet$new(name = "Digitized plasma")
    )
  )
})

test_that("addObservedDataSource aborts on the name of a DataSet added in this session", {
  project <- testProject()
  suppressMessages(addObservedDataSource(
    project,
    ospsuite::DataSet$new(name = "Scratch measurement")
  ))
  # Adding a source clears the data-set names found so far; the DataSets added
  # in this session are still known.
  addObservedDataSource(
    project,
    list(type = "pkml", file = "cohort_b_export.pkml")
  )

  expect_snapshot(
    error = TRUE,
    addObservedDataSource(
      project,
      ospsuite::DataSet$new(name = "Scratch measurement")
    )
  )
})

test_that("addObservedDataSource refuses a DataSet saved earlier under its name", {
  # Saving turns the DataSet into the source `Scratch measurement.pkml`.
  # Adding a source clears the data-set names, and this one's file does not
  # exist, so no read can fill them again: only that id shows the clash.
  # Without it, the add would succeed and the next load and save fail.
  project <- testProject()
  addObservedDataSource(
    project,
    list(type = "pkml", file = "cohort_b_export.pkml")
  )
  scratch <- ospsuite::DataSet$new(name = "Scratch measurement")
  scratch$setValues(xValues = c(0, 1, 3), yValues = c(0, 6, 2))
  suppressMessages(addObservedDataSource(project, scratch))
  suppressMessages(saveProject(project))

  expect_snapshot(error = TRUE, {
    addObservedDataSource(project, scratch)
    addObservedDataSource(project, scratch, overwrite = TRUE)
  })
})

test_that("addObservedDataSource refuses a DataSet whose PKML file another source reads", {
  # Saving the DataSet would write `<name>.pkml` into the data folder, the data
  # file of a source filed under its own declared id.
  project <- testProject()
  addObservedDataSource(
    project,
    list(id = "cohort_b", type = "pkml", file = "Cohort B.pkml")
  )

  expect_snapshot(error = TRUE, {
    addObservedDataSource(project, ospsuite::DataSet$new(name = "Cohort B"))
    addObservedDataSource(
      project,
      ospsuite::DataSet$new(name = "Cohort B"),
      overwrite = TRUE
    )
  })
})

test_that("addObservedDataSource compares data files by where they lie", {
  project <- testProject()
  dataFolder <- project$paths$dataFolder
  # Other spellings of `<name>.pkml` in the data folder. On Windows and macOS a
  # name differing only in case is the same file.
  sameFile <- list(
    "Cohort C" = "./Cohort C.pkml",
    "Cohort D" = "sub/../Cohort D.pkml",
    "Cohort E" = file.path(dataFolder, "Cohort E.pkml"),
    "Cohort F" = "cohort f.PKML"
  )
  for (name in names(sameFile)) {
    addObservedDataSource(
      project,
      list(
        id = sub(" ", "_", tolower(name)),
        type = "pkml",
        file = sameFile[[name]]
      )
    )
  }
  # Different files: one in a subfolder, one unrelated, one outside the data
  # folder (which the next load reports, not the add).
  addObservedDataSource(
    project,
    list(id = "cohort_g", type = "pkml", file = "sub/Cohort G.pkml")
  )
  addObservedDataSource(
    project,
    list(id = "infusion", type = "pkml", file = "Infusion study.pkml")
  )
  addObservedDataSource(
    project,
    list(id = "shared", type = "pkml", file = "../shared/Cohort H.pkml")
  )

  for (name in names(sameFile)) {
    expect_error(
      addObservedDataSource(project, ospsuite::DataSet$new(name = name)),
      "already exists",
      info = name
    )
  }
  for (name in c("Cohort G", "Cohort H")) {
    expect_no_error(suppressMessages(
      addObservedDataSource(project, ospsuite::DataSet$new(name = name))
    ))
  }
})

test_that("a DataSet name inside a PKML source not loaded yet aborts the next load", {
  project <- testProject()
  .addDigitizedPkmlSource(project)
  suppressMessages(addObservedDataSource(
    project,
    ospsuite::DataSet$new(name = "Digitized plasma")
  ))

  expect_snapshot(
    error = TRUE,
    suppressWarnings(suppressMessages(loadObservedData(project)))
  )
})

test_that("overwrite = TRUE accepts a DataSet name inside a PKML source not loaded yet", {
  # The add cannot see the name without reading the source, so it is accepted
  # and the next load reports the duplicate.
  project <- testProject()
  .addDigitizedPkmlSource(project)
  suppressMessages(addObservedDataSource(
    project,
    ospsuite::DataSet$new(name = "Digitized plasma"),
    overwrite = TRUE
  ))

  expect_error(
    suppressWarnings(suppressMessages(loadObservedData(project))),
    "Duplicate observed-data set name"
  )
})

# Loading selected sources ----

digitizedPlasmaDataSet <- function() {
  ds <- ospsuite::DataSet$new(name = "Digitized plasma")
  ds$setValues(xValues = c(0, 0.5, 1, 2), yValues = c(0, 3.8, 4.1, 2.3))
  ds
}

# A project whose full load fails: next to the Excel source it declares a PKML
# source holding a data set named "Digitized plasma", a PKML source whose file
# was never exported, and a DataSet added in the session under that same name.
.projectWithBrokenSources <- function(envir = parent.frame()) {
  project <- testProject(envir)
  .addDigitizedPkmlSource(project)
  addObservedDataSource(
    project,
    list(type = "pkml", file = "cohort_b_export.pkml")
  )
  suppressMessages(addObservedDataSource(project, digitizedPlasmaDataSet()))
  project
}

test_that("loadObservedData loads only the selected sources", {
  project <- .projectWithBrokenSources()

  expect_no_warning(
    result <- loadObservedData(
      project,
      sources = c("Digitized plasma", "Aciclovir_TimeValuesData.xlsx")
    )
  )

  # The file-based sources in the order the project declares them, then the
  # DataSets added in this session. The Excel sheet is read here with ospsuite
  # directly.
  dataFolder <- project$paths$dataFolder
  excelDataSets <- ospsuite::loadDataSetsFromExcel(
    xlsFilePath = file.path(dataFolder, "Aciclovir_TimeValuesData.xlsx"),
    importerConfigurationOrPath = file.path(
      dataFolder,
      "esqlabs_dataImporter_configuration.xml"
    ),
    sheets = "Laskin 1982.Group A"
  )
  expected <- c(
    excelDataSets,
    list("Digitized plasma" = digitizedPlasmaDataSet())
  )
  expect_identical(names(expected), c(testObservedDataId, "Digitized plasma"))
  expect_named(result, names(expected))
  expect_equal(
    ospsuite::dataSetToDataFrame(result),
    ospsuite::dataSetToDataFrame(expected)
  )
})

test_that("loadObservedData reads a selected PKML source and no other", {
  project <- .projectWithBrokenSources()

  result <- loadObservedData(project, sources = "digitized_plasma.pkml")

  # Only the data set the PKML file holds: the DataSet of the same name added in
  # this session is not added, which would abort the load on the duplicate name.
  expect_named(result, "Digitized plasma")
  # PKML stores single precision.
  expect_equal(
    result[["Digitized plasma"]]$yValues,
    c(0, 4.1, 2.3),
    tolerance = 1e-6
  )
})

test_that("loadObservedData with no sources selected returns an empty list", {
  project <- .projectWithBrokenSources()

  expect_identical(loadObservedData(project, sources = character(0)), list())
})

test_that("loadObservedData aborts on an unknown source id, listing the ids", {
  project <- .projectWithBrokenSources()

  expect_snapshot(
    error = TRUE,
    loadObservedData(
      project,
      sources = c("Aciclovir_TimeValuesData.xlsx", "laskin_po.pkml")
    )
  )

  emptyProject <- testProject()
  removeObservedDataSource(emptyProject, "Aciclovir_TimeValuesData.xlsx")
  expect_snapshot(
    error = TRUE,
    emptyProject$loadObservedData(sources = "laskin_po.pkml")
  )
})

test_that("loadObservedData rejects sources that are not source ids", {
  project <- testProject()
  expect_snapshot(error = TRUE, {
    loadObservedData(project, sources = NA_character_)
  })
})

test_that("loadObservedData finds a source by its id as given or as stored", {
  project <- testProject()
  ospsuite::saveDataSetToPKML(
    digitizedPlasmaDataSet(),
    file.path(project$paths$dataFolder, "laskin_1982_iv.pkml")
  )
  suppressWarnings(addObservedDataSource(
    project,
    list(id = "Laskin_IV", type = "pkml", file = "laskin_1982_iv.pkml")
  ))
  expected <- ospsuite::dataSetToDataFrame(digitizedPlasmaDataSet())

  for (id in c("Laskin_IV", "laskin_iv")) {
    expect_equal(
      ospsuite::dataSetToDataFrame(loadObservedData(project, sources = id)),
      expected,
      info = id
    )
  }
  # Both spellings name one source, which is loaded once.
  expect_equal(
    ospsuite::dataSetToDataFrame(
      loadObservedData(project, sources = c("Laskin_IV", "laskin_iv"))
    ),
    expected
  )
})

test_that("loadObservedData returns selected sources in the order the project declares them", {
  project <- testProject()
  cohortB <- ospsuite::DataSet$new(name = "Cohort B plasma")
  cohortB$setValues(xValues = c(0, 1, 4), yValues = c(0, 2.9, 1.2))
  ospsuite::saveDataSetToPKML(
    digitizedPlasmaDataSet(),
    file.path(project$paths$dataFolder, "laskin_1982_iv.pkml")
  )
  ospsuite::saveDataSetToPKML(
    cohortB,
    file.path(project$paths$dataFolder, "cohort_b.pkml")
  )
  addObservedDataSource(
    project,
    list(type = "pkml", file = "laskin_1982_iv.pkml")
  )
  addObservedDataSource(project, list(type = "pkml", file = "cohort_b.pkml"))

  result <- loadObservedData(
    project,
    sources = c("cohort_b.pkml", "laskin_1982_iv.pkml")
  )
  expect_named(result, c("Digitized plasma", "Cohort B plasma"))
})

test_that("loadObservedData of selected sources leaves the data-set names alone", {
  project <- testProject()
  seam <- .projectSeam(project)
  # Adding a source clears the data-set names found so far.
  addObservedDataSource(
    project,
    list(type = "pkml", file = "cohort_b_export.pkml")
  )
  expect_null(seam$.observedDataNamesCache)
  loadObservedData(project, sources = "Aciclovir_TimeValuesData.xlsx")
  expect_null(seam$.observedDataNamesCache)

  project <- testProject()
  seam <- .projectSeam(project)
  suppressMessages(addObservedDataSource(project, digitizedPlasmaDataSet()))
  allNames <- getObservedDataNames(project)
  expect_identical(allNames, c(testObservedDataId, "Digitized plasma"))
  loadObservedData(project, sources = "Digitized plasma")
  expect_identical(seam$.observedDataNamesCache, allNames)
})

test_that("loadObservedData warns only about a selected programmatic source with no data", {
  project <- testProject()
  suppressMessages(addObservedDataSource(project, digitizedPlasmaDataSet()))
  suppressMessages(addObservedDataSource(
    project,
    ospsuite::DataSet$new(name = "Lost cohort")
  ))
  # A reload loses the DataSet of a source that was never saved.
  seam <- .projectSeam(project)
  seam$.programmaticDataSets[["Lost cohort"]] <- NULL
  rlang::reset_warning_verbosity(paste0(
    "esqlabsR_observed_data_unresolved_programmatic_",
    .projectWarningKey(project)
  ))

  expect_no_warning(
    result <- loadObservedData(project, sources = "Digitized plasma")
  )
  expect_equal(
    ospsuite::dataSetToDataFrame(result),
    ospsuite::dataSetToDataFrame(digitizedPlasmaDataSet())
  )

  expect_snapshot(result <- loadObservedData(project, sources = "Lost cohort"))
  expect_identical(result, list())
})

test_that("a warning about selected sources leaves the warning of a full load", {
  project <- testProject()
  for (name in c("Lost cohort A", "Lost cohort B")) {
    suppressMessages(addObservedDataSource(
      project,
      ospsuite::DataSet$new(name = name)
    ))
  }
  # A reload loses the DataSets of sources that were never saved.
  seam <- .projectSeam(project)
  seam$.programmaticDataSets <- list()
  rlang::reset_warning_verbosity(paste0(
    "esqlabsR_observed_data_unresolved_programmatic_",
    .projectWarningKey(project)
  ))

  expect_warning(
    loadObservedData(project, sources = "Lost cohort A"),
    "Lost cohort A"
  )
  # The full load, as createDataCombined() runs it, still names every source
  # that resolved to no data.
  expect_snapshot(result <- loadObservedData(project))
  expect_named(result, testObservedDataId)
})

test_that("addObservedDataSource overwrite = TRUE replaces a config entry in place", {
  project <- testProject()
  addObservedDataSource(project, list(type = "pkml", file = "sub1/obs.pkml"))
  before <- length(project$definitions$observedDataSources)
  # A second entry with the same basename replaces the first (same on-disk id),
  # keeping the section length unchanged and taking the new directory.
  addObservedDataSource(
    project,
    list(type = "pkml", file = "sub2/obs.pkml"),
    overwrite = TRUE
  )
  expect_length(project$definitions$observedDataSources, before)
  files <- vapply(
    project$definitions$observedDataSources,
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
test_that("observed-data sources sharing a basename fail saveProject()", {
  project <- testProject()
  saveProject(project)
  dir <- file.path(
    project$info$projectDirPath,
    "definitions",
    "observed-data-sources"
  )
  before <- if (dir.exists(dir)) list.files(dir) else character()

  colliding <- list(
    list(type = "pkml", file = "dirA/obs.pkml"),
    list(type = "pkml", file = "dirB/obs.pkml")
  )
  .setSection(project, "observedDataSources", colliding)
  expect_snapshot(
    saveProject(project),
    error = TRUE
  )

  # The on-disk tree did not change (the save aborted before writing).
  if (dir.exists(dir)) {
    expect_setequal(list.files(dir), before)
  }
})

# Under explicit-save, addObservedDataSource() only mutates memory (the runtime store
# and the section together); nothing touches disk until saveProject().
test_that("addObservedDataSource mutates memory only", {
  project <- testProject()
  state <- .projectSeam(project)
  ds <- ospsuite::DataSet$new(name = "InMemorySet")
  ds$setValues(xValues = c(1, 2), yValues = c(3, 4))

  # The add succeeds in memory: the DataSet is registered in the runtime store.
  addObservedDataSource(project, ds)
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

  addObservedDataSource(project, ds)
  saveProject(project)

  # The entry is now a pkml source pointing at the written file; the runtime
  # store no longer holds it (it is file-backed).
  entry <- project$definitions$observedDataSources[[
    length(project$definitions$observedDataSources)
  ]]
  expect_identical(entry$type, "pkml")
  expect_identical(entry$file, "RoundTripSet.pkml")
  expect_false("RoundTripSet" %in% names(state$.programmaticDataSets))
  expect_true(file.exists(file.path(
    project$paths$dataFolder,
    "RoundTripSet.pkml"
  )))

  # The data survives a reload (fresh session, empty runtime store).
  project <- loadProject(project$info$projectFilePath)
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
  # The programmatic DataSet named "Collide" would persist to "Collide.pkml",
  # the file of a pkml source producing a differently-named DataSet ("Other").
  # The pkml source is added after the DataSet, so the add of the DataSet
  # cannot see its id; the save is the last place to catch the collision.
  ds <- ospsuite::DataSet$new(name = "Collide")
  ds$setValues(xValues = c(1, 2), yValues = c(333, 444))
  addObservedDataSource(project, ds)
  existing <- ospsuite::DataSet$new(name = "Other")
  existing$setValues(xValues = c(0, 1), yValues = c(111, 222))
  collidePath <- file.path(project$paths$dataFolder, "Collide.pkml")
  ospsuite::saveDataSetToPKML(existing, collidePath)
  addObservedDataSource(project, list(type = "pkml", file = "Collide.pkml"))

  expect_snapshot(saveProject(project), error = TRUE)

  # The abort left the existing "Collide.pkml" untouched: its data is still
  # "Other"'s, not the programmatic DataSet's.
  expect_equal(
    ospsuite::loadDataSetFromPKML(collidePath)$yValues,
    c(111, 222),
    tolerance = 1e-6
  )
})

test_that("saveProject keeps the data file of a source filed under a declared id", {
  project <- testProject()
  # The DataSet is added before the source that reads "Cohort B.pkml", so the
  # add cannot see the clash; the save is the last place to catch it.
  replacement <- ospsuite::DataSet$new(name = "Cohort B")
  replacement$setValues(xValues = c(0, 1), yValues = c(999, 999))
  suppressMessages(addObservedDataSource(project, replacement))
  original <- ospsuite::DataSet$new(name = "Cohort B")
  original$setValues(xValues = c(0, 1), yValues = c(111, 222))
  cohortPath <- file.path(project$paths$dataFolder, "Cohort B.pkml")
  ospsuite::saveDataSetToPKML(original, cohortPath)
  addObservedDataSource(
    project,
    list(id = "cohort_b", type = "pkml", file = "Cohort B.pkml")
  )

  expect_snapshot(saveProject(project), error = TRUE)
  expect_equal(
    ospsuite::loadDataSetFromPKML(cohortPath)$yValues,
    c(111, 222),
    tolerance = 1e-6
  )
})

test_that("saveProject compares data files by where they lie", {
  # Each source reads `<name>.pkml` from the data folder under another
  # spelling. The DataSet is added first, so only the save can catch it.
  spellings <- c(
    "./Cohort B.pkml",
    "sub/../Cohort B.pkml",
    "<dataFolder>/Cohort B.pkml",
    "cohort b.PKML"
  )
  for (spelling in spellings) {
    project <- testProject()
    dataFolder <- project$paths$dataFolder
    file <- sub("<dataFolder>", dataFolder, spelling, fixed = TRUE)
    original <- ospsuite::DataSet$new(name = "Cohort B")
    original$setValues(xValues = c(0, 1), yValues = c(111, 222))
    existingPath <- file.path(dataFolder, basename(file))
    ospsuite::saveDataSetToPKML(original, existingPath)
    checksum <- tools::md5sum(existingPath)
    replacement <- ospsuite::DataSet$new(name = "Cohort B")
    replacement$setValues(xValues = c(0, 1), yValues = c(999, 999))
    suppressMessages(addObservedDataSource(project, replacement))
    addObservedDataSource(
      project,
      list(id = "cohort_b", type = "pkml", file = file)
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
      observedDataSources = list()
    ),
    jsonPath,
    auto_unbox = TRUE,
    null = "null"
  )
  project <- loadProject(jsonPath)
  ds <- ospsuite::DataSet$new(name = "NoFolderSet")
  ds$setValues(xValues = c(1, 2), yValues = c(3, 4))
  addObservedDataSource(project, ds)

  expect_snapshot(saveProject(project), error = TRUE)
})

# addObservedDataSource() persists to `<name>.pkml` on save, so a DataSet name that is
# not a safe filename segment is rejected up front rather than failing later with
# an opaque low-level path error at save time.
test_that("addObservedDataSource rejects a DataSet name that is not a safe filename", {
  project <- testProject()
  ds <- ospsuite::DataSet$new(name = "Cohort/A")
  ds$setValues(xValues = c(1, 2), yValues = c(3, 4))
  expect_error(addObservedDataSource(project, ds), "safe filename segment")
})

# A hand-authored `programmatic` sentinel with no `id` is schema-legal (a
# programmatic entry has no required fields). The persist step must not crash on
# it; an id-less entry has nothing to name its definition file, so the save
# aborts with the serializer's clear "no id" message, not an opaque
# `subscript out of bounds`.
test_that("saveProject rejects an id-less programmatic sentinel with a clear error", {
  project <- testProject()
  observedData <- .getSection(project, "observedDataSources")
  observedData <- c(observedData, list(list(type = "programmatic")))
  .setSection(project, "observedDataSources", observedData)

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
  addObservedDataSource(project, ds)

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
  expect_true("RecoverableSet" %in% names(state$.programmaticDataSets))
})

test_that("removeObservedDataSource drops the DataSet of a source a failed save already rewrote", {
  # The save wrote the PKML and rewrote the entry as a `pkml` source, then
  # aborted, so the DataSet is still held in the session.
  project <- testProject()
  state <- .projectSeam(project)
  ds <- ospsuite::DataSet$new(name = "RecoverableSet")
  ds$setValues(xValues = c(1, 2), yValues = c(3, 4))
  suppressMessages(addObservedDataSource(project, ds))
  .persistProgrammaticObservedData(project, state)
  expect_identical(
    project$definitions$observedDataSources[["RecoverableSet"]]$type,
    "pkml"
  )

  removeObservedDataSource(project, "RecoverableSet")

  expect_false("RecoverableSet" %in% names(state$.programmaticDataSets))
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
  suppressMessages(addObservedDataSource(project, cohort))
  .setSection(
    project,
    "observedDataSources",
    c(
      .getSection(project, "observedDataSources"),
      list(list(type = "pkml", file = "other/Cohort B"))
    )
  )

  expect_snapshot(saveProject(project), error = TRUE)
  expect_false(file.exists(
    file.path(project$paths$dataFolder, "Cohort B.pkml")
  ))
})

# removeObservedDataSource write-through ----

test_that("removeObservedDataSource deletes the definition file on save", {
  project <- testProject()
  dir <- file.path(
    project$info$projectDirPath,
    "definitions",
    "observed-data-sources"
  )
  # The fixture declares one Excel source, filed under its basename.
  id <- "Aciclovir_TimeValuesData.xlsx"
  expect_true(file.exists(file.path(dir, paste0(id, ".json"))))

  suppressWarnings(removeObservedDataSource(project, id))

  # In memory the declaration is gone; the definition file is deleted on save
  # and a fresh load no longer sees it.
  expect_false(any(vapply(
    project$definitions$observedDataSources,
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
  expect_length(reloaded$definitions$observedDataSources, 0L)
})

test_that("removeObservedDataSource removes a vector of ids in one pass", {
  project <- testProject()
  addObservedDataSource(project, list(type = "pkml", file = "one.pkml"))
  addObservedDataSource(project, list(type = "pkml", file = "two.pkml"))
  before <- length(project$definitions$observedDataSources)

  removeObservedDataSource(project, c("one.pkml", "two.pkml"))
  expect_length(project$definitions$observedDataSources, before - 2L)
  files <- vapply(
    project$definitions$observedDataSources,
    function(e) basename(e[["file"]] %||% NA_character_),
    character(1)
  )
  expect_false(any(c("one.pkml", "two.pkml") %in% files))
})

test_that("an observed-data source is filed and matched by its declared id", {
  project <- testProject()
  dir <- file.path(
    project$info$projectDirPath,
    "definitions",
    "observed-data-sources"
  )

  addObservedDataSource(
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
    reloaded$definitions$observedDataSources,
    function(e) e[["id"]] %||% NA_character_,
    character(1)
  )
  expect_true("obs_demo" %in% ids)

  # The declared id is also the key the entry is removed by.
  removeObservedDataSource(reloaded, "obs_demo")
  saveProject(reloaded)
  expect_false(file.exists(file.path(dir, "obs_demo.json")))
})

test_that("addObservedDataSource rejects a duplicate declared id", {
  project <- testProject()
  addObservedDataSource(
    project,
    list(id = "obs", type = "pkml", file = "a.pkml")
  )
  expect_snapshot(
    error = TRUE,
    addObservedDataSource(
      project,
      list(id = "obs", type = "pkml", file = "b.pkml")
    )
  )

  # Overwriting replaces the entry carrying that id in place.
  addObservedDataSource(
    project,
    list(id = "obs", type = "pkml", file = "b.pkml"),
    overwrite = TRUE
  )
  entry <- Filter(
    function(e) identical(e[["id"]], "obs"),
    project$definitions$observedDataSources
  )
  expect_length(entry, 1L)
  expect_identical(entry[[1]][["file"]], "b.pkml")
})

test_that("addObservedDataSource canonicalizes a declared id", {
  # A declared id names the declaration's definition file, and the Excel
  # importer already files the same declaration under the canonicalized id, so
  # hand-authoring has to reach the same one. Two ids differing only in case
  # would otherwise be two declarations mapping to one file on a
  # case-insensitive filesystem.
  project <- testProject()
  dir <- file.path(
    project$info$projectDirPath,
    "definitions",
    "observed-data-sources"
  )

  expect_warning(
    addObservedDataSource(
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
      names(project$definitions$observedDataSources)
  )

  saveProject(project)
  expect_true("testproject_timevaluesdata.json" %in% list.files(dir))
  expect_false("TestProject_TimeValuesData.json" %in% list.files(dir))

  # The canonical id is the handle the removal matches on.
  removeObservedDataSource(project, "testproject_timevaluesdata")
  saveProject(project)
  expect_false("testproject_timevaluesdata.json" %in% list.files(dir))
})

test_that("addObservedDataSource leaves a derived id and a DataSet name as they are", {
  # Only a declared id is canonicalized. A derived id is the source's own
  # string: the `file` basename is one segment of a real path, and a
  # programmatic entry's id is the `DataSet` name that `dataCombined` entries
  # and PI output mappings reference.
  project <- testProject()
  ds <- ospsuite::DataSet$new(name = "MyProgSet")
  ds$setValues(xValues = c(1, 2), yValues = c(3, 4))
  # The DataSet goes in first: it resolves every declared source to check its
  # name, which a config entry naming a file that is not there would abort.
  suppressMessages(addObservedDataSource(project, ds))
  addObservedDataSource(project, list(type = "pkml", file = "sub/MyObs.pkml"))

  expect_setequal(
    names(project$definitions$observedDataSources),
    c("Aciclovir_TimeValuesData.xlsx", "MyObs.pkml", "MyProgSet")
  )
})

test_that("removeObservedDataSource finds every id kind by the spelling it was added with", {
  # Three kinds of id share this one section. A declared id is stored the way
  # `addObservedDataSource()` canonicalized it; a derived one (a `file` basename, a
  # `DataSet` name) is the source's own string and carries casing no
  # canonicalization produces. Each has to be removable by the id its caller
  # typed, which is why the match takes a verbatim pass and a canonical one.
  project <- testProject()
  ds <- ospsuite::DataSet$new(name = "MyProgSet")
  ds$setValues(xValues = c(1, 2), yValues = c(3, 4))
  # The DataSet goes in first: it resolves every declared source to check its
  # name, which a config entry naming a file that is not there would abort.
  suppressMessages(addObservedDataSource(project, ds))
  addObservedDataSource(project, list(type = "pkml", file = "sub/MyObs.pkml"))
  suppressWarnings(addObservedDataSource(
    project,
    list(id = "MyDeclared", type = "pkml", file = "TimeValues.pkml")
  ))

  # A genuine miss still warns, naming the id as typed rather than the
  # canonical form the second pass tried.
  expect_warning(removeObservedDataSource(project, "NotThere"), "NotThere")

  # A declared id answers to the spelling that was authored, not only to the
  # canonical one that spelling was rewritten to.
  removeObservedDataSource(project, "MyDeclared")
  expect_false(
    "mydeclared" %in% names(project$definitions$observedDataSources)
  )

  # A derived id and a programmatic `DataSet` name are matched verbatim, so
  # canonicalizing the search key unconditionally would strand both.
  removeObservedDataSource(project, c("MyObs.pkml", "MyProgSet"))
  expect_setequal(
    names(project$definitions$observedDataSources),
    "Aciclovir_TimeValuesData.xlsx"
  )
  expect_false("MyProgSet" %in% getObservedDataNames(project))
})

test_that("addObservedDataSource rejects an id that is not a single non-empty string", {
  project <- testProject()
  # An id names the declaration's file and is its remove handle, so a blank, NA,
  # or non-string one is a mistake to report, not a value to quietly ignore in
  # favour of the file basename.
  for (bad in list("", NA_character_, 42)) {
    expect_error(
      addObservedDataSource(
        project,
        list(id = bad, type = "pkml", file = "a.pkml")
      ),
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
  addObservedDataSource(project, ds)

  expect_snapshot(
    error = TRUE,
    addObservedDataSource(
      project,
      list(id = "prog_src", type = "pkml", file = "x.pkml"),
      overwrite = TRUE
    )
  )
  # The DataSet is untouched, so it still resolves and still saves.
  expect_true("prog_src" %in% getObservedDataNames(project))
})

# A DataSet keeps its name as its id, so a name equal to the id of another
# source, given or taken from its file basename, would share that source's
# definition file. Neither PKML file exists: the clash is found without reading.
test_that("a DataSet clashes with the id of another source", {
  project <- testProject()
  addObservedDataSource(
    project,
    list(id = "cohort_b", type = "pkml", file = "cohort_b_export.pkml")
  )

  expect_snapshot(error = TRUE, {
    addObservedDataSource(project, ospsuite::DataSet$new(name = "cohort_b"))
    # The fixture's Excel source takes its id from its file basename.
    addObservedDataSource(
      project,
      ospsuite::DataSet$new(name = "Aciclovir_TimeValuesData.xlsx")
    )
  })
})

test_that("removeObservedDataSource warns and skips a not-found id in the batch", {
  project <- testProject()
  addObservedDataSource(project, list(type = "pkml", file = "one.pkml"))
  before <- length(project$definitions$observedDataSources)
  expect_warning(
    removeObservedDataSource(project, c("one.pkml", "ghost.pkml")),
    "ghost.pkml"
  )
  expect_length(project$definitions$observedDataSources, before - 1L)
})

# The remove path writes through to disk BEFORE clearing the runtime store, so a
# Under explicit-save, removeObservedDataSource() only mutates memory; a section that
# still carries a colliding pair of file entries after the removal serializes
# cleanly in memory but aborts at saveProject(). Seed the backing section
# (bypassing the setter) with a colliding pair alongside the programmatic
# sentinel; after the sentinel is dropped in memory, saveProject() aborts on
# the surviving collision, leaving disk unchanged.
test_that("removeObservedDataSource mutates memory only; a surviving collision aborts saveProject()", {
  project <- testProject()
  saveProject(project)
  state <- .projectSeam(project)
  ds <- ospsuite::DataSet$new(name = "myProgSet")
  ds$setValues(xValues = c(1, 2), yValues = c(3, 4))
  addObservedDataSource(project, ds)

  # Seed a colliding pair of surviving entries directly into the backing field.
  state$.observedDataSources <- .observedDataSectionById(list(
    list(id = "myProgSet", type = "programmatic"),
    list(type = "pkml", file = "dirA/obs.pkml"),
    list(type = "pkml", file = "dirB/obs.pkml")
  ))

  # The removal succeeds in memory (the programmatic name is dropped from the
  # runtime store), but the surviving pair still collides at save time.
  removeObservedDataSource(project, "myProgSet")
  expect_false("myProgSet" %in% names(state$.programmaticDataSets))
  expect_snapshot(saveProject(project), error = TRUE)
})

# Stored ids ----

test_that("every observed-data source stores its id first and the section is keyed by it", {
  project <- testProject()
  addObservedDataSource(
    project,
    list(type = "pkml", file = "sub/Cohort B.pkml")
  )
  suppressMessages(addObservedDataSource(
    project,
    ospsuite::DataSet$new(name = "Scratch measurement")
  ))

  expect_identical(
    lapply(
      .unwrapDefinitionList(project$definitions$observedDataSources),
      unclass
    ),
    list(
      "Aciclovir_TimeValuesData.xlsx" = list(
        id = "Aciclovir_TimeValuesData.xlsx",
        type = "excel",
        file = "Aciclovir_TimeValuesData.xlsx",
        importerConfiguration = "esqlabs_dataImporter_configuration.xml",
        sheets = list("Laskin 1982.Group A")
      ),
      "Cohort B.pkml" = list(
        id = "Cohort B.pkml",
        type = "pkml",
        file = "sub/Cohort B.pkml"
      ),
      "Scratch measurement" = list(
        id = "Scratch measurement",
        type = "programmatic"
      )
    )
  )
  expect_s3_class(
    project$definitions$observedDataSources[["Cohort B.pkml"]],
    "ObservedDataSource"
  )
  # Keyed by id, the section is still read-only.
  expect_snapshot(
    error = TRUE,
    project$definitions$observedDataSources[[
      "Cohort B.pkml"
    ]]$file <- "other.pkml"
  )
})

test_that("saveProject writes the id first into every definition file and a DataSet keeps its name as id", {
  project <- testProject()
  dir <- file.path(
    project$info$projectDirPath,
    "definitions",
    "observed-data-sources"
  )
  addObservedDataSource(
    project,
    list(type = "pkml", file = "sub/Cohort B.pkml")
  )
  scratch <- ospsuite::DataSet$new(name = "Scratch measurement")
  scratch$setValues(xValues = c(0, 1, 3), yValues = c(0, 6, 2))
  suppressMessages(addObservedDataSource(project, scratch))

  saveProject(project)

  files <- sort(list.files(dir))
  expect_identical(
    files,
    c(
      "Aciclovir_TimeValuesData.xlsx.json",
      "Cohort B.pkml.json",
      "Scratch measurement.json"
    )
  )
  expect_identical(
    lapply(file.path(dir, files), jsonlite::read_json),
    list(
      list(
        id = "Aciclovir_TimeValuesData.xlsx",
        type = "excel",
        file = "Aciclovir_TimeValuesData.xlsx",
        importerConfiguration = "esqlabs_dataImporter_configuration.xml",
        sheets = list("Laskin 1982.Group A")
      ),
      list(id = "Cohort B.pkml", type = "pkml", file = "sub/Cohort B.pkml"),
      list(
        id = "Scratch measurement",
        type = "pkml",
        file = "Scratch measurement.pkml"
      )
    )
  )

  # Removed by the DataSet name after the save, and after a reload.
  reloaded <- loadProject(project$info$projectFilePath)
  for (p in list(project, reloaded)) {
    removeObservedDataSource(p, "Scratch measurement")
    expect_identical(
      names(p$definitions$observedDataSources),
      c("Aciclovir_TimeValuesData.xlsx", "Cohort B.pkml")
    )
  }
  saveProject(reloaded)
  expect_false(file.exists(file.path(dir, "Scratch measurement.json")))
})

test_that("a definition file that stores no id, or a programmatic name, still loads", {
  # Written by hand, or saved before every file stored its id.
  project <- testProject()
  dir <- file.path(
    project$info$projectDirPath,
    "definitions",
    "observed-data-sources"
  )
  jsonlite::write_json(
    list(type = "pkml", file = "Cohort B.pkml"),
    file.path(dir, "Cohort B.pkml.json"),
    auto_unbox = TRUE
  )
  jsonlite::write_json(
    list(type = "programmatic", name = "Scratch measurement"),
    file.path(dir, "Scratch measurement.json"),
    auto_unbox = TRUE
  )

  reloaded <- loadProject(project$info$projectFilePath)
  sources <- .unwrapDefinitionList(reloaded$definitions$observedDataSources)
  expect_identical(
    lapply(sources[c("Cohort B.pkml", "Scratch measurement")], unclass),
    list(
      "Cohort B.pkml" = list(
        id = "Cohort B.pkml",
        type = "pkml",
        file = "Cohort B.pkml"
      ),
      "Scratch measurement" = list(
        id = "Scratch measurement",
        type = "programmatic"
      )
    )
  )

  # A save after a change stores the id in both files, and no `name`.
  .markModified(reloaded)
  saveProject(reloaded)
  expect_identical(
    jsonlite::read_json(file.path(dir, "Cohort B.pkml.json")),
    list(id = "Cohort B.pkml", type = "pkml", file = "Cohort B.pkml")
  )
  expect_identical(
    jsonlite::read_json(file.path(dir, "Scratch measurement.json")),
    list(id = "Scratch measurement", type = "programmatic")
  )
})

test_that("a programmatic name in a project snapshot loads as its id", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(
      schemaVersion = "2.0",
      filePaths = list(dataFolder = "."),
      outputPaths = structure(list(), names = character(0)),
      scenarios = list(),
      observedDataSources = list(
        list(type = "programmatic", name = "Scratch measurement")
      )
    ),
    tmp,
    auto_unbox = TRUE,
    null = "null"
  )

  project <- loadProject(tmp)

  expect_identical(
    lapply(
      .unwrapDefinitionList(project$definitions$observedDataSources),
      unclass
    ),
    list(
      "Scratch measurement" = list(
        id = "Scratch measurement",
        type = "programmatic"
      )
    )
  )
})

test_that("snapshotProject writes observed data as an array of records with ids", {
  project <- testProject()
  suppressMessages(addObservedDataSource(
    project,
    ospsuite::DataSet$new(name = "Scratch measurement")
  ))

  path <- snapshotProject(project, dir = withr::local_tempdir(), name = "obs")
  written <- jsonlite::read_json(path)

  # An array: no names, each record carrying its own `id`.
  expect_identical(
    written$observedDataSources,
    list(
      list(
        id = "Aciclovir_TimeValuesData.xlsx",
        type = "excel",
        file = "Aciclovir_TimeValuesData.xlsx",
        importerConfiguration = "esqlabs_dataImporter_configuration.xml",
        sheets = list("Laskin 1982.Group A")
      ),
      list(id = "Scratch measurement", type = "programmatic")
    )
  )
})

test_that("a source passed back keeps the id it is stored under", {
  # The id names an existing source, so it is that source's id, not one the
  # user typed: it is kept as is even when it is not the spelling a typed id
  # would be canonicalized to, or no longer the basename of its file.
  project <- testProject()
  dir <- file.path(
    project$info$projectDirPath,
    "definitions",
    "observed-data-sources"
  )
  jsonlite::write_json(
    list(id = "Laskin IV", type = "pkml", file = "laskin_1982_iv.pkml"),
    file.path(dir, "Laskin IV.json"),
    auto_unbox = TRUE
  )
  project <- loadProject(project$info$projectFilePath)
  scratch <- ospsuite::DataSet$new(name = "Scratch measurement")
  scratch$setValues(xValues = c(0, 1, 3), yValues = c(0, 6, 2))
  suppressMessages(addObservedDataSource(project, scratch))
  saveProject(project)

  sources <- project$definitions$observedDataSources
  aciclovir <- sources[["Aciclovir_TimeValuesData.xlsx"]]
  aciclovir$file <- "Aciclovir_TimeValuesData_v2.xlsx"
  expect_no_warning({
    addObservedDataSource(project, sources[["Laskin IV"]], overwrite = TRUE)
    addObservedDataSource(
      project,
      sources[["Scratch measurement"]],
      overwrite = TRUE
    )
    addObservedDataSource(project, aciclovir, overwrite = TRUE)
  })

  expect_identical(
    names(project$definitions$observedDataSources),
    c("Aciclovir_TimeValuesData.xlsx", "Laskin IV", "Scratch measurement")
  )
  expect_identical(
    project$definitions$observedDataSources[[
      "Aciclovir_TimeValuesData.xlsx"
    ]]$file,
    "Aciclovir_TimeValuesData_v2.xlsx"
  )
  expect_no_error(saveProject(project))
  expect_identical(
    sort(list.files(dir)),
    c(
      "Aciclovir_TimeValuesData.xlsx.json",
      "Laskin IV.json",
      "Scratch measurement.json"
    )
  )
})

test_that("a DataSet replaces a programmatic source with no data only with overwrite", {
  # A source with no data, as a snapshot taken before saving leaves it.
  project <- testProject()
  dir <- file.path(
    project$info$projectDirPath,
    "definitions",
    "observed-data-sources"
  )
  jsonlite::write_json(
    list(id = "Scratch measurement", type = "programmatic"),
    file.path(dir, "Scratch measurement.json"),
    auto_unbox = TRUE
  )
  project <- loadProject(project$info$projectFilePath)
  scratch <- ospsuite::DataSet$new(name = "Scratch measurement")
  scratch$setValues(xValues = c(0, 1, 3), yValues = c(0, 6, 2))

  expect_snapshot(error = TRUE, addObservedDataSource(project, scratch))

  suppressMessages(addObservedDataSource(project, scratch, overwrite = TRUE))
  expect_identical(
    names(project$definitions$observedDataSources),
    c("Aciclovir_TimeValuesData.xlsx", "Scratch measurement")
  )
  saveProject(project)
  expect_identical(
    sort(list.files(dir)),
    c("Aciclovir_TimeValuesData.xlsx.json", "Scratch measurement.json")
  )
  expect_identical(
    jsonlite::read_json(file.path(dir, "Scratch measurement.json")),
    list(
      id = "Scratch measurement",
      type = "pkml",
      file = "Scratch measurement.pkml"
    )
  )
})

test_that("a DataSet saved again under its name rewrites its own PKML file", {
  project <- testProject()
  pkmlPath <- file.path(project$paths$dataFolder, "Scratch measurement.pkml")
  first <- ospsuite::DataSet$new(name = "Scratch measurement")
  first$setValues(xValues = c(0, 1, 3), yValues = c(0, 6, 2))
  suppressMessages(addObservedDataSource(project, first))
  saveProject(project)

  # Replaced in the same session, and again after a reload.
  second <- ospsuite::DataSet$new(name = "Scratch measurement")
  second$setValues(xValues = c(0, 1, 3), yValues = c(0, 7, 3))
  removeObservedDataSource(project, "Scratch measurement")
  suppressMessages(addObservedDataSource(project, second))
  expect_no_error(saveProject(project))
  expect_equal(
    ospsuite::loadDataSetFromPKML(pkmlPath)$yValues,
    c(0, 7, 3),
    tolerance = 1e-6
  )

  reloaded <- loadProject(project$info$projectFilePath)
  third <- ospsuite::DataSet$new(name = "Scratch measurement")
  third$setValues(xValues = c(0, 1, 3), yValues = c(0, 8, 4))
  removeObservedDataSource(reloaded, "Scratch measurement")
  suppressMessages(addObservedDataSource(reloaded, third))
  expect_no_error(saveProject(reloaded))
  expect_equal(
    ospsuite::loadDataSetFromPKML(pkmlPath)$yValues,
    c(0, 8, 4),
    tolerance = 1e-6
  )
})

# `Cohort.json` and `cohort.json` are one file on Windows and macOS, so ids that
# differ only in case are the same id, refused where the user adds the second.
test_that("addObservedDataSource refuses an id that differs from another only in case", {
  project <- testProject()
  addObservedDataSource(project, list(type = "pkml", file = "Laskin 1982.pkml"))
  addObservedDataSource(
    project,
    list(id = "cohort", type = "pkml", file = "cohort_export.pkml")
  )
  suppressMessages(addObservedDataSource(
    project,
    ospsuite::DataSet$new(name = "Cohort B")
  ))

  expect_snapshot(error = TRUE, {
    addObservedDataSource(
      project,
      list(type = "pkml", file = "sub/laskin 1982.pkml")
    )
    addObservedDataSource(project, ospsuite::DataSet$new(name = "Cohort"))
    addObservedDataSource(project, ospsuite::DataSet$new(name = "cohort b"))
  })
  expect_identical(
    names(project$definitions$observedDataSources),
    c("Aciclovir_TimeValuesData.xlsx", "Laskin 1982.pkml", "cohort", "Cohort B")
  )
})

# Replacing a source whose id is spelled in another case drops the replaced
# source's data-set name from the names the project knows, not only its entry.
test_that("overwriting a DataSet source spelled in another case replaces its data-set name", {
  project <- testProject()
  dir <- file.path(
    project$info$projectDirPath,
    "definitions",
    "observed-data-sources"
  )
  state <- .projectSeam(project)
  upper <- ospsuite::DataSet$new(name = "Cohort B")
  upper$setValues(xValues = c(0, 1), yValues = c(1, 2))
  lower <- ospsuite::DataSet$new(name = "cohort b")
  lower$setValues(xValues = c(0, 1), yValues = c(3, 4))
  suppressMessages(addObservedDataSource(project, upper))
  getObservedDataNames(project)

  suppressMessages(addObservedDataSource(project, lower, overwrite = TRUE))

  expect_identical(
    names(project$definitions$observedDataSources),
    c("Aciclovir_TimeValuesData.xlsx", "cohort b")
  )
  expect_identical(names(state$.programmaticDataSets), "cohort b")
  expect_identical(
    getObservedDataNames(project),
    c(testObservedDataId, "cohort b")
  )
  saveProject(project)
  expect_identical(
    sort(list.files(dir)),
    c("Aciclovir_TimeValuesData.xlsx.json", "cohort b.json")
  )
})

test_that("overwriting a file source spelled in another case replaces its data-set name", {
  project <- testProject()
  dir <- file.path(
    project$info$projectDirPath,
    "definitions",
    "observed-data-sources"
  )
  dataFolder <- project$paths$dataFolder
  for (name in c("Cohort", "COHORT")) {
    ds <- ospsuite::DataSet$new(name = name)
    ds$setValues(xValues = c(0, 1), yValues = c(1, 2))
    ospsuite::saveDataSetToPKML(
      ds,
      file.path(dataFolder, paste0(name, ".pkml"))
    )
  }
  addObservedDataSource(project, list(type = "pkml", file = "Cohort.pkml"))
  getObservedDataNames(project)

  addObservedDataSource(
    project,
    list(id = "COHORT.pkml", type = "pkml", file = "COHORT.pkml"),
    overwrite = TRUE
  )

  expect_identical(
    names(project$definitions$observedDataSources),
    c("Aciclovir_TimeValuesData.xlsx", "COHORT.pkml")
  )
  expect_identical(
    getObservedDataNames(project),
    c(testObservedDataId, "COHORT")
  )
  saveProject(project)
  expect_identical(
    sort(list.files(dir)),
    c("Aciclovir_TimeValuesData.xlsx.json", "COHORT.pkml.json")
  )
})

# Replacing a programmatic source spelled in another case must not leave two
# sources providing the new name, or another source's PKML file overwritten.
test_that("a DataSet overwrite is refused when another source provides its name", {
  project <- testProject()
  state <- .projectSeam(project)
  upper <- ospsuite::DataSet$new(name = "Cohort B")
  upper$setValues(xValues = c(0, 1), yValues = c(1, 2))
  suppressMessages(addObservedDataSource(project, upper))
  # A PKML source holding a data set named "cohort b", known once loaded.
  exported <- ospsuite::DataSet$new(name = "cohort b")
  exported$setValues(xValues = c(0, 1), yValues = c(5, 6))
  ospsuite::saveDataSetToPKML(
    exported,
    file.path(project$paths$dataFolder, "cohort_b_export.pkml")
  )
  addObservedDataSource(
    project,
    list(type = "pkml", file = "cohort_b_export.pkml")
  )
  getObservedDataNames(project)
  lower <- ospsuite::DataSet$new(name = "cohort b")
  lower$setValues(xValues = c(0, 1), yValues = c(3, 4))

  expect_snapshot(
    error = TRUE,
    addObservedDataSource(project, lower, overwrite = TRUE)
  )
  expect_identical(
    names(project$definitions$observedDataSources),
    c("Aciclovir_TimeValuesData.xlsx", "Cohort B", "cohort_b_export.pkml")
  )
  expect_identical(names(state$.programmaticDataSets), "Cohort B")
})

test_that("a DataSet overwrite is refused when another source reads its PKML file", {
  project <- testProject()
  state <- .projectSeam(project)
  upper <- ospsuite::DataSet$new(name = "Cohort B")
  upper$setValues(xValues = c(0, 1), yValues = c(1, 2))
  suppressMessages(addObservedDataSource(project, upper))
  addObservedDataSource(
    project,
    list(id = "cohort_b_lab", type = "pkml", file = "cohort b.pkml")
  )
  lower <- ospsuite::DataSet$new(name = "cohort b")
  lower$setValues(xValues = c(0, 1), yValues = c(3, 4))

  expect_snapshot(
    error = TRUE,
    addObservedDataSource(project, lower, overwrite = TRUE)
  )
  expect_identical(
    names(project$definitions$observedDataSources),
    c("Aciclovir_TimeValuesData.xlsx", "Cohort B", "cohort_b_lab")
  )
  expect_identical(names(state$.programmaticDataSets), "Cohort B")
})

# Print method ----

test_that("print.ObservedDataSource renders the source declaration", {
  project <- testProject()
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$definitions$observedDataSources[[1]]))
})

# The section is stored as an unnamed list, so printing it showed the count with
# no ids underneath (#1281). It lists the same ids the authoring functions take.

test_that("printing the observedDataSources section lists the declaration ids", {
  project <- testProject()
  suppressMessages(addObservedDataSource(
    project,
    ospsuite::DataSet$new(name = "simulated_plasma")
  ))
  withr::local_options(cli.unicode = FALSE)
  local_reproducible_output()
  expect_snapshot(print(project$definitions$observedDataSources))
})

# Projects saved with the earlier section names ----

# A project saved by an earlier development version of esqlabsR stored its
# observed-data sources under the key `observedData` and in the folder
# `observed-data`. Read under the current names it would show no sources at all,
# so the load stops and names what to rename. The earlier names are held in
# variables so a rename that matches the earlier spelling leaves them alone.
earlierKey <- "observedData"
earlierFolder <- "observed-data"

test_that("loading a project that keeps its sources in the earlier folder aborts", {
  dir <- .copyTestProjectDir()
  definitionsDir <- file.path(dir, "definitions")
  file.rename(
    file.path(definitionsDir, "observed-data-sources"),
    file.path(definitionsDir, earlierFolder)
  )

  expect_snapshot(loadProject(file.path(dir, "Project.json")), error = TRUE)
})

test_that("loading a Project.json with the earlier observed-data key aborts", {
  dir <- .copyTestProjectDir()
  path <- file.path(dir, "Project.json")
  container <- jsonlite::read_json(path)
  names(container)[names(container) == "observedDataSources"] <- earlierKey
  jsonlite::write_json(container, path, auto_unbox = TRUE, null = "null")

  expect_snapshot(loadProject(path), error = TRUE)
})

# A project saved by an earlier build usually carries both: the folder holding
# its source files and the key, written as an empty list.
test_that("loading a project with the earlier folder and key aborts naming both", {
  dir <- .copyTestProjectDir()
  definitionsDir <- file.path(dir, "definitions")
  file.rename(
    file.path(definitionsDir, "observed-data-sources"),
    file.path(definitionsDir, earlierFolder)
  )
  path <- file.path(dir, "Project.json")
  container <- jsonlite::read_json(path)
  names(container)[names(container) == "observedDataSources"] <- earlierKey
  jsonlite::write_json(container, path, auto_unbox = TRUE, null = "null")

  expect_snapshot(loadProject(path), error = TRUE)
})

# A project opened with the current names and saved again gains the current
# folder and key next to the earlier ones, so there is nothing to rename to.
test_that("loading a project with both the earlier and the current names aborts", {
  dir <- .copyTestProjectDir()
  definitionsDir <- file.path(dir, "definitions")
  dir.create(file.path(definitionsDir, earlierFolder))
  file.copy(
    list.files(
      file.path(definitionsDir, "observed-data-sources"),
      full.names = TRUE
    ),
    file.path(definitionsDir, earlierFolder)
  )
  path <- file.path(dir, "Project.json")
  container <- jsonlite::read_json(path)
  container[[earlierKey]] <- list()
  jsonlite::write_json(container, path, auto_unbox = TRUE, null = "null")

  expect_snapshot(loadProject(path), error = TRUE)
})

# The current folder is read in place of any source listed in the project file,
# so renaming the earlier key would leave its sources unread and drop them on
# the next save. They have to become definition files in the folder instead.
test_that("loading a project whose earlier key lists sources next to the current folder aborts", {
  dir <- .copyTestProjectDir()
  path <- file.path(dir, "Project.json")
  container <- jsonlite::read_json(path)
  container[["observedDataSources"]] <- NULL
  container[[earlierKey]] <- list(list(
    id = "laskin_po",
    type = "pkml",
    file = "Laskin 1982 po.pkml"
  ))
  jsonlite::write_json(container, path, auto_unbox = TRUE, null = "null")

  expect_snapshot(loadProject(path), error = TRUE)
})

test_that("restoring a snapshot with the earlier observed-data key aborts", {
  snapshot <- snapshotProject(
    testProject(),
    dir = withr::local_tempdir(),
    name = "aciclovir"
  )
  json <- jsonlite::read_json(snapshot)
  names(json)[names(json) == "observedDataSources"] <- earlierKey
  jsonlite::write_json(json, snapshot, auto_unbox = TRUE, null = "null")
  dir <- file.path(withr::local_tempdir(), "restored")

  expect_snapshot(restoreProject(snapshot, dir), error = TRUE)
  expect_false(dir.exists(dir))
})
