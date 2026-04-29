# loadObservedData ----

test_that("loadObservedData returns empty list when observedData is NULL", {
  project <- testProject()
  project$observedData <- NULL
  result <- loadObservedData(project)
  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("loadObservedData loads Excel data using project defaults", {
  project <- testProject()
  result <- loadObservedData(project)
  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_true(all(sapply(result, function(ds) inherits(ds, "DataSet"))))
})

test_that("loadObservedData loads Excel data with explicit file overrides", {
  project <- testProject()
  project$observedData <- list(list(
    type = "excel",
    file = "TestProject_TimeValuesData.xlsx",
    importerConfiguration = "esqlabs_dataImporter_configuration.xml",
    sheets = list("Laskin 1982.Group A")
  ))
  result <- loadObservedData(project)
  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_true(all(sapply(result, function(ds) inherits(ds, "DataSet"))))
})

test_that("loadObservedData merges datasets from multiple entries", {
  project <- testProject()
  project$observedData <- list(
    list(
      type = "excel",
      file = "TestProject_TimeValuesData.xlsx",
      importerConfiguration = "esqlabs_dataImporter_configuration.xml",
      sheets = list("Laskin 1982.Group A")
    ),
    list(
      type = "excel",
      file = "TestProject_TimeValuesData.xlsx",
      importerConfiguration = "esqlabs_dataImporter_configuration.xml",
      sheets = list("Laskin 1982.Group A")
    )
  )
  result <- loadObservedData(project)
  expect_type(result, "list")
  expect_true(length(result) >= 1)
})

test_that("loadObservedData returns named list of DataSet objects for Example project", {
  project <- testProject()
  dataSets <- loadObservedData(project)

  expect_type(dataSets, "list")
  expect_true(length(dataSets) > 0)
  expect_true(all(nzchar(names(dataSets))))
  expect_true(all(vapply(
    dataSets,
    function(ds) inherits(ds, "DataSet"),
    logical(1)
  )))
})

test_that("loadObservedData returns empty list when project declares no observed data", {
  project <- testProject()
  project$observedData <- NULL

  expect_identical(loadObservedData(project), list())
})

test_that("loadObservedData errors on non-Project input", {
  expect_error(
    loadObservedData("not a project"),
    regexp = "Project"
  )
})

test_that("loadObservedData loads PKML data", {
  project <- testProject()
  project$observedData <- list(list(
    type = "pkml",
    file = "ObsDataAciclovir_1.pkml"
  ))
  result <- loadObservedData(project)
  expect_type(result, "list")
  expect_length(result, 1)
  expect_true(inherits(result[[1]], "DataSet"))
  expect_equal(names(result), result[[1]]$name)
})

test_that("loadObservedData loads mixed Excel and PKML data", {
  project <- testProject()
  project$observedData <- list(
    list(
      type = "excel",
      file = "TestProject_TimeValuesData.xlsx",
      importerConfiguration = "esqlabs_dataImporter_configuration.xml",
      sheets = list("Laskin 1982.Group A")
    ),
    list(type = "pkml", file = "ObsDataAciclovir_1.pkml")
  )
  result <- loadObservedData(project)
  expect_type(result, "list")
  expect_true(length(result) >= 2)
  expect_true(all(sapply(result, function(ds) inherits(ds, "DataSet"))))
})

test_that("loadObservedData loads from JSON with explicit file and importerConfiguration", {
  project <- testProject()
  result <- loadObservedData(project)
  expect_type(result, "list")
  expect_length(result, 2)
  expect_true(all(sapply(result, function(ds) inherits(ds, "DataSet"))))
  excelName <- "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
  pkmlName <- "Vergin 1995.Iv"
  expect_true(excelName %in% names(result))
  expect_true(pkmlName %in% names(result))
})

test_that("Project$observedData preserves file and importerConfiguration from JSON", {
  project <- testProject()
  expect_length(project$observedData, 2)
  excelEntry <- project$observedData[[1]]
  expect_equal(excelEntry$type, "excel")
  expect_equal(excelEntry$file, "TestProject_TimeValuesData.xlsx")
  expect_equal(
    excelEntry$importerConfiguration,
    "esqlabs_dataImporter_configuration.xml"
  )
  expect_equal(excelEntry$sheets, list("Laskin 1982.Group A"))
  pkmlEntry <- project$observedData[[2]]
  expect_equal(pkmlEntry$type, "pkml")
  expect_equal(pkmlEntry$file, "ObsDataAciclovir_1.pkml")
})

test_that("Project load errors if Excel observedData entry missing file", {
  project <- testProject()
  pc_path <- project$jsonPath
  json_data <- jsonlite::fromJSON(pc_path, simplifyVector = FALSE)
  json_data$observedData <- list(list(
    type = "excel",
    importerConfiguration = "esqlabs_dataImporter_configuration.xml",
    sheets = list("Sheet1")
  ))
  tmp_json <- tempfile(fileext = ".json")
  jsonlite::write_json(json_data, tmp_json, auto_unbox = TRUE, null = "null")
  expect_error(
    loadProject(tmp_json),
    regexp = "file"
  )
})

test_that("Project load errors if Excel observedData entry missing importerConfiguration", {
  project <- testProject()
  pc_path <- project$jsonPath
  json_data <- jsonlite::fromJSON(pc_path, simplifyVector = FALSE)
  json_data$observedData <- list(list(
    type = "excel",
    file = "TestProject_TimeValuesData.xlsx",
    sheets = list("Sheet1")
  ))
  tmp_json <- tempfile(fileext = ".json")
  jsonlite::write_json(json_data, tmp_json, auto_unbox = TRUE, null = "null")
  expect_error(
    loadProject(tmp_json),
    regexp = "importerConfiguration"
  )
})

test_that("Project load errors if script observedData entry missing file", {
  project <- testProject()
  pc_path <- project$jsonPath
  json_data <- jsonlite::fromJSON(pc_path, simplifyVector = FALSE)
  json_data$observedData <- list(list(type = "script"))
  tmp_json <- tempfile(fileext = ".json")
  jsonlite::write_json(json_data, tmp_json, auto_unbox = TRUE, null = "null")
  expect_error(
    loadProject(tmp_json),
    regexp = "file"
  )
})

test_that("loadObservedData loads script returning single DataSet", {
  project <- testProject()
  project$observedData <- list(list(
    type = "script",
    file = "scripts/test_script_single.R"
  ))
  result <- loadObservedData(project)
  expect_type(result, "list")
  expect_length(result, 1)
  expect_true(inherits(result[[1]], "DataSet"))
  expect_equal(names(result), "ScriptGenerated")
})

test_that("loadObservedData loads script returning list of DataSets", {
  project <- testProject()
  project$observedData <- list(list(
    type = "script",
    file = "scripts/test_script_list.R"
  ))
  result <- loadObservedData(project)
  expect_type(result, "list")
  expect_length(result, 2)
  expect_true(all(sapply(result, inherits, "DataSet")))
  expect_setequal(names(result), c("ScriptDataSet1", "ScriptDataSet2"))
})

test_that("loadObservedData errors if script file not found", {
  project <- testProject()
  project$observedData <- list(list(
    type = "script",
    file = "scripts/nonexistent.R"
  ))
  expect_error(
    loadObservedData(project),
    regexp = "Script file not found"
  )
})

test_that("loadObservedData errors if script returns wrong type", {
  project <- testProject()
  project$observedData <- list(list(
    type = "script",
    file = "scripts/test_script_bad.R"
  ))
  expect_error(
    loadObservedData(project),
    regexp = "must return DataSet"
  )
})

# Public CRUD: observed data ----

test_that("addObservedData with DataSet adds entry and stores DataSet by name", {
  project <- testProject()
  ds <- ospsuite::DataSet$new(name = "ProgrammaticData")
  ds$setValues(xValues = 1:3, yValues = 1:3)
  initialCount <- length(project$observedData)
  expect_message(
    project$addObservedData(ds),
    regexp = "reproducibility"
  )
  expect_equal(length(project$observedData), initialCount + 1)
  newEntry <- project$observedData[[length(project$observedData)]]
  expect_equal(newEntry$type, "programmatic")
  expect_equal(newEntry$name, "ProgrammaticData")
  expect_equal(project$.getProgrammaticDataSets()[["ProgrammaticData"]], ds)
})

test_that("addObservedData with config list adds entry directly", {
  project <- testProject()
  initialCount <- length(project$observedData)
  config <- list(
    type = "script",
    file = "scripts/generate_data.R"
  )
  project$addObservedData(config)
  expect_equal(length(project$observedData), initialCount + 1)
  newEntry <- project$observedData[[length(project$observedData)]]
  expect_equal(newEntry$type, "script")
  expect_equal(newEntry$file, "scripts/generate_data.R")
})

test_that("addObservedData with config list validates type", {
  project <- testProject()
  expect_error(
    project$addObservedData(list(type = "invalid", file = "test.xlsx")),
    regexp = "Invalid type"
  )
  expect_error(
    project$addObservedData(list(file = "test.xlsx")),
    regexp = "must include.*type"
  )
})

test_that("loadObservedData merges programmatic and JSON-declared DataSets", {
  project <- testProject()
  ds <- ospsuite::DataSet$new(name = "ProgrammaticData")
  ds$setValues(xValues = 1:3, yValues = 1:3)
  suppressMessages(project$addObservedData(ds))
  result <- loadObservedData(project)
  expect_true("ProgrammaticData" %in% names(result))
  expect_equal(result[["ProgrammaticData"]], ds)
  expect_true(length(result) > 1)
})

test_that("addObservedData errors on duplicate DataSet name", {
  project <- testProject()
  ds1 <- ospsuite::DataSet$new(name = "SameName")
  ds1$setValues(xValues = 1:3, yValues = 1:3)
  ds2 <- ospsuite::DataSet$new(name = "SameName")
  ds2$setValues(xValues = 1:3, yValues = 4:6)
  suppressMessages(project$addObservedData(ds1))
  expect_error(
    suppressMessages(project$addObservedData(ds2)),
    regexp = "already exists"
  )
})

test_that("getObservedDataNames returns all DataSet names", {
  project <- testProject()
  names_before <- getObservedDataNames(project)
  expect_true(length(names_before) >= 1)

  ds <- ospsuite::DataSet$new(name = "NewData")
  ds$setValues(xValues = 1:3, yValues = 1:3)
  suppressMessages(project$addObservedData(ds))

  names_after <- getObservedDataNames(project)
  expect_true("NewData" %in% names_after)
  expect_equal(length(names_after), length(names_before) + 1)
})

test_that("addObservedData errors when name conflicts with existing loaded data", {
  project <- testProject()
  existing_name <- getObservedDataNames(project)[[1]]
  ds <- ospsuite::DataSet$new(name = existing_name)
  ds$setValues(xValues = 1:3, yValues = 1:3)
  expect_error(
    suppressMessages(project$addObservedData(ds)),
    regexp = "already exists"
  )
})

test_that("standalone addObservedData (DataSet) matches R6 method behavior", {
  pc1 <- testProject()
  pc2 <- testProject()
  ds1 <- ospsuite::DataSet$new(name = "StandaloneDS")
  ds1$setValues(xValues = 1:3, yValues = 1:3)
  ds2 <- ospsuite::DataSet$new(name = "StandaloneDS")
  ds2$setValues(xValues = 1:3, yValues = 1:3)

  suppressMessages(addObservedData(pc1, ds1))
  suppressMessages(pc2$addObservedData(ds2))

  expect_equal(length(pc1$observedData), length(pc2$observedData))
  expect_equal(
    names(pc1$.getProgrammaticDataSets()),
    names(pc2$.getProgrammaticDataSets())
  )
  expect_equal(
    pc1$.getProgrammaticDataSets()[["StandaloneDS"]]$name,
    pc2$.getProgrammaticDataSets()[["StandaloneDS"]]$name
  )
  expect_equal(
    pc1$.getProgrammaticDataSets()[["StandaloneDS"]]$xValues,
    pc2$.getProgrammaticDataSets()[["StandaloneDS"]]$xValues
  )
  expect_equal(
    pc1$.getProgrammaticDataSets()[["StandaloneDS"]]$yValues,
    pc2$.getProgrammaticDataSets()[["StandaloneDS"]]$yValues
  )
})

test_that("standalone addObservedData (config list) appends entry", {
  project <- testProject()
  initial <- length(project$observedData)
  addObservedData(project, list(type = "script", file = "scripts/x.R"))
  expect_equal(length(project$observedData), initial + 1)
  expect_true(project$modified)
})

test_that("standalone addObservedData returns project invisibly", {
  project <- testProject()
  out <- withVisible(
    addObservedData(project, list(type = "script", file = "scripts/y.R"))
  )
  expect_false(out$visible)
  expect_identical(out$value, project)
})

test_that("removeObservedData removes programmatic DataSet by name", {
  project <- testProject()
  ds <- ospsuite::DataSet$new(name = "ToRemove")
  ds$setValues(xValues = 1:3, yValues = 1:3)
  suppressMessages(addObservedData(project, ds))
  project$.markSaved()
  before <- length(project$observedData)
  removeObservedData(project, "ToRemove")
  expect_equal(length(project$observedData), before - 1)
  expect_false("ToRemove" %in% names(project$.getProgrammaticDataSets()))
  expect_true(project$modified)
})

test_that("removeObservedData removes config entry by file basename", {
  project <- testProject()
  addObservedData(project, list(type = "script", file = "scripts/delete-me.R"))
  before <- length(project$observedData)
  removeObservedData(project, "delete-me.R")
  expect_equal(length(project$observedData), before - 1)
})

test_that("removeObservedData warns on missing name", {
  project <- testProject()
  expect_warning(
    removeObservedData(project, "NoSuchDataSet_ZZZ"),
    regexp = "not found"
  )
})

test_that("removeObservedData removes the correct DataSet when multiple programmatic exist", {
  project <- testProject()
  dsA <- ospsuite::DataSet$new(name = "DS_A")
  dsA$setValues(xValues = 1:3, yValues = c(10, 20, 30))
  dsB <- ospsuite::DataSet$new(name = "DS_B")
  dsB$setValues(xValues = 1:3, yValues = c(100, 200, 300))

  suppressMessages(addObservedData(project, dsA))
  suppressMessages(addObservedData(project, dsB))

  # Remove A; B must still be reachable via loadObservedData
  removeObservedData(project, "DS_A")
  loaded <- loadObservedData(project)
  expect_false("DS_A" %in% names(loaded))
  expect_true("DS_B" %in% names(loaded))
  expect_equal(loaded[["DS_B"]]$yValues, c(100, 200, 300), tolerance = 1e-4)
})

test_that("addObservedData annotates programmatic sentinel with the DataSet name", {
  project <- testProject()
  ds <- ospsuite::DataSet$new(name = "Annotated")
  ds$setValues(xValues = 1:3, yValues = 1:3)
  suppressMessages(addObservedData(project, ds))

  # The programmatic sentinel in observedData should record the DataSet
  # name so the entry is identifiable when serialised or removed.
  programmaticEntries <- Filter(
    function(e) identical(e$type, "programmatic"),
    project$observedData
  )
  names_recorded <- vapply(
    programmaticEntries,
    function(e) e$name %||% NA_character_,
    character(1)
  )
  expect_true("Annotated" %in% names_recorded)
})

test_that("removeObservedData removes the named programmatic sentinel, not the first one", {
  project <- testProject()
  dsA <- ospsuite::DataSet$new(name = "DS_A")
  dsA$setValues(xValues = 1:3, yValues = c(10, 20, 30))
  dsB <- ospsuite::DataSet$new(name = "DS_B")
  dsB$setValues(xValues = 1:3, yValues = c(100, 200, 300))

  suppressMessages(addObservedData(project, dsA))
  suppressMessages(addObservedData(project, dsB))

  removeObservedData(project, "DS_B")

  programmaticEntries <- Filter(
    function(e) identical(e$type, "programmatic"),
    project$observedData
  )
  names_recorded <- vapply(
    programmaticEntries,
    function(e) e$name %||% NA_character_,
    character(1)
  )
  expect_true("DS_A" %in% names_recorded)
  expect_false("DS_B" %in% names_recorded)
})
