test_that("It converts a single positive number", {
  string <- "21"
  expect_equal(stringToNum(string), 21)
})

test_that("It converts a single negative number", {
  string <- "-21"
  expect_equal(stringToNum(string), -21)
})

test_that("It converts a vector of numbers", {
  string <- c("21", "-21")
  expect_equal(stringToNum(string), c(21, -21))
})

test_that("It converts a non numerics to NA", {
  string <- c("21", "one", "-21")
  expect_equal(stringToNum(string), c(21, NA, -21))
})

test_that("It converts a LLOQ values", {
  string <- c("21", "one", "<5", "-21", " < - 5 ", "<s")
  # "LLOQ/2"
  expect_equal(
    stringToNum(string, lloqMode = LLOQMode$`LLOQ/2`),
    c(21, NA, 2.5, -21, -2.5, NA)
  )
  # LLOQ
  expect_equal(
    stringToNum(string, lloqMode = LLOQMode$LLOQ),
    c(21, NA, 5, -21, -5, NA)
  )
  # ZERO
  expect_equal(
    stringToNum(string, lloqMode = LLOQMode$ZERO),
    c(21, NA, 0, -21, 0, NA)
  )
  # IGNORE
  expect_equal(
    stringToNum(string, lloqMode = LLOQMode$ignore),
    c(21, NA, NA, -21, NA, NA)
  )
})

test_that("It converts a ULOQ values", {
  string <- c("21", "one", ">5", "-21", " > - 5 ", "<s")
  # ULOQ
  expect_equal(
    stringToNum(string, uloqMode = ULOQMode$ULOQ),
    c(21, NA, 5, -21, -5, NA)
  )
  # IGNORE
  expect_equal(
    stringToNum(string, uloqMode = ULOQMode$ignore),
    c(21, NA, NA, -21, NA, NA)
  )
})


dataSet1 <- ospsuite::DataSet$new(name = "data1")
dataSet2 <- ospsuite::DataSet$new(name = "data2")


test_that("It returns an empty DataSet if calculating for empty DataSets", {
  meanDataSet <- calculateMeanDataSet(dataSet1)
  expect_equal(meanDataSet$xValues, numeric())
  expect_equal(meanDataSet$yValues, numeric())
})

dataSet1$setValues(xValues = 1:5, yValues = 1:5)
dataSet2$setValues(xValues = 2:6, yValues = 4:8)

test_that("It can calculate the mean data set for a single data set", {
  dataSet1$addMetaData(name = "meta1", value = "a")
  meanDataSet <- calculateMeanDataSet(dataSet1)
  expect_equal(meanDataSet$xValues, dataSet1$xValues)
  expect_equal(meanDataSet$yValues, dataSet1$yValues)
  expect_equal(meanDataSet$yErrorValues, rep(NaN, 5))
  expect_equal(meanDataSet$name, "Mean")
  expect_equal(meanDataSet$metaData, list(meta1 = "a", `Subject ID` = "mean"))
})

test_that("It can calculate the arithmetic mean and standard deviation (default)", {
  meanDataSet <- calculateMeanDataSet(list(dataSet1, dataSet2))
  expect_equal(meanDataSet$xValues, 1:6)
  expect_equal(meanDataSet$yValues, c(1, 3:6, 8), tolerance = 1e-06)
  expect_equal(
    meanDataSet$yErrorValues,
    c(NaN, rep(sqrt(2), 4), NaN),
    tolerance = 1e-06
  )
})

test_that("It can calculate the geometric mean and standard deviation", {
  meanDataSet <- calculateMeanDataSet(
    list(dataSet1, dataSet2),
    method = "geometric"
  )
  expect_equal(meanDataSet$xValues, 1:6)
  expect_equal(
    meanDataSet$yValues,
    c(
      1,
      geomean(c(2, 4)),
      geomean(c(3, 5)),
      geomean(c(4, 6)),
      geomean(c(5, 7)),
      8
    )
  )
  expect_equal(
    meanDataSet$yErrorValues,
    c(
      NaN,
      geosd(c(2, 4)),
      geosd(c(3, 5)),
      geosd(c(4, 6)),
      geosd(c(5, 7)),
      NaN
    ),
    tolerance = 1e-06
  )
})

test_that("It can convert values to outputXunit and outputYunit", {
  # input units are "h" and "mg/l"
  meanDataSet <- calculateMeanDataSet(
    list(dataSet1, dataSet2),
    outputXunit = "min",
    outputYunit = "µg/l"
  )
  expect_equal(meanDataSet$xValues, seq(60, 360, 60))
  expect_equal(meanDataSet$yValues, c(1, 3:6, 8) * 1000, tolerance = 1e-06)
  expect_equal(
    meanDataSet$yErrorValues,
    c(NaN, rep(sqrt(2e06), 4), NaN),
    tolerance = 1e-06
  )
})

test_that("It can convert values to xUnit and yUnit of first data set, with given molWeights", {
  dataSet1$yDimension <- ospsuite::ospDimensions$`Concentration (molar)`
  dataSet1$yUnit <- ospsuite::ospUnits$`Concentration [molar]`$`mmol/l`
  dataSet1$molWeight <- 2
  dataSet2$molWeight <- 4
  dataSet2$setValues(xValues = seq(120, 360, 60), yValues = 4:8)
  dataSet2$xUnit <- "min"
  meanDataSet <- calculateMeanDataSet(
    list(dataSet1, dataSet2),
    outputMolWeight = 5
  )
  expect_equal(meanDataSet$molWeight, 5)
  expect_equal(meanDataSet$xUnit, "h")
  expect_equal(meanDataSet$xValues, 1:6)
  expect_equal(meanDataSet$yDimension, "Concentration (molar)")
  expect_equal(meanDataSet$yUnit, "mmol/l")
  # dataSet2 yValues in mmol/l: 1.00 1.25 1.50 1.75 2.00
  # dataSet1 yValues in mg/l: 1 2 3 4 5
  expect_equal(meanDataSet$yValues, c(1, 1.5, 2.125, 2.75, 3.375, 2))
  expect_equal(
    meanDataSet$yErrorValues,
    c(
      NaN,
      sd(c(1, 2)),
      sd(c(1.25, 3)),
      sd(c(1.5, 4)),
      sd(c(1.75, 5)),
      NaN
    ),
    tolerance = 1e-06
  )
})

test_that("It throws an error when xValues can not be converted to same xUnit", {
  dataSet1 <- ospsuite::DataSet$new(name = "data1")
  dataSet2 <- ospsuite::DataSet$new(name = "data2")
  dataSet1$setValues(xValues = 1:5, yValues = 1:5)
  dataSet2$setValues(xValues = 2:6, yValues = 4:8)
  dataSet2$xDimension <- ospsuite::ospDimensions$Flow
  expect_error(
    calculateMeanDataSet(list(dataSet1, dataSet2)),
    "Unit 'l/min' is not defined in dimension 'Time'"
  )
})

test_that("It throws an error when yValues can not be converted to same yUnit", {
  dataSet1 <- ospsuite::DataSet$new(name = "data1")
  dataSet2 <- ospsuite::DataSet$new(name = "data2")
  dataSet1$setValues(xValues = 1:5, yValues = 1:5)
  dataSet2$setValues(xValues = 2:6, yValues = 4:8)
  dataSet1$yDimension <- ospsuite::ospDimensions$Flow
  expect_error(
    calculateMeanDataSet(list(dataSet1, dataSet2)),
    "Unit 'mg/l' is not defined in dimension 'Flow'"
  )
})

test_that("Only meta data entries that are equal in all inital data sets are set in the mean data set", {
  dataSet1 <- ospsuite::DataSet$new(name = "data1")
  dataSet2 <- ospsuite::DataSet$new(name = "data2")
  dataSet1$setValues(xValues = 1:5, yValues = 1:5)
  dataSet2$setValues(xValues = 2:6, yValues = 4:8)
  dataSet1$addMetaData(name = "meta1", value = "a")
  dataSet2$addMetaData(name = "meta1", value = "a")
  dataSet1$addMetaData(name = "meta2", value = "b")
  dataSet2$addMetaData(name = "meta2", value = "c")
  dataSet1$addMetaData(name = "meta3", value = "d")
  dataSet1$addMetaData(name = "meta4", value = "d")
  meanDataSet <- calculateMeanDataSet(list(dataSet1, dataSet2))
  expect_equal(meanDataSet$metaData, list(meta1 = "a", `Subject ID` = "mean"))
})

test_that("It throws an error when molWeights of data sets are different and no outputMolWeight is given", {
  dataSet1$molWeight <- 1
  dataSet2$molWeight <- 2
  expect_error(
    calculateMeanDataSet(list(dataSet1, dataSet2)),
    regexp = messages$errorOutputMolWeightNeeded(),
    fixed = TRUE
  )
})

test_that("It can handle the lloqMode argument", {
  dataSet1 <- ospsuite::DataSet$new(name = "data1")
  dataSet2 <- ospsuite::DataSet$new(name = "data2")
  dataSet1$setValues(xValues = 1:5, yValues = 1:5)
  dataSet2$setValues(xValues = 2:6, yValues = 4:8)
  dataSet2$LLOQ <- 6
  # LLOQ/2 --> no difference
  meanDataSet <- calculateMeanDataSet(
    list(dataSet1, dataSet2),
    lloqMode = "LLOQ/2"
  )
  expect_equal(meanDataSet$xValues, 1:6)
  expect_equal(meanDataSet$yValues, c(1, 3:6, 8), tolerance = 1e-06)
  expect_equal(
    meanDataSet$yErrorValues,
    c(NaN, rep(sqrt(2), 4), NaN),
    tolerance = 1e-06
  )
  # LLOQ
  meanDataSet <- calculateMeanDataSet(
    list(dataSet1, dataSet2),
    lloqMode = "LLOQ"
  )
  expect_equal(meanDataSet$xValues, 1:6)
  expect_equal(meanDataSet$yValues, c(1, 4, 4.5, 5, 6, 8), tolerance = 1e-06)
  expect_equal(
    meanDataSet$yErrorValues,
    c(
      NaN,
      sd(c(2, 6)),
      sd(c(3, 6)),
      sd(c(4, 6)),
      sd(c(5, 7)),
      NaN
    ),
    tolerance = 1e-06
  )
  # ZERO
  meanDataSet <- calculateMeanDataSet(
    list(dataSet1, dataSet2),
    lloqMode = "ZERO"
  )
  expect_equal(meanDataSet$xValues, 1:6)
  expect_equal(meanDataSet$yValues, c(1, 1, 1.5, 5, 6, 8), tolerance = 1e-06)
  expect_equal(
    meanDataSet$yErrorValues,
    c(
      NaN,
      sd(c(2, 0)),
      sd(c(3, 0)),
      sd(c(4, 6)),
      sd(c(5, 7)),
      NaN
    ),
    tolerance = 1e-06
  )
  # ignore
  meanDataSet <- calculateMeanDataSet(
    list(dataSet1, dataSet2),
    lloqMode = "ignore"
  )
  expect_equal(meanDataSet$xValues, 1:6)
  expect_equal(meanDataSet$yValues, c(1, 2, 3, 5, 6, 8), tolerance = 1e-06)
  expect_equal(
    meanDataSet$yErrorValues,
    c(NaN, NaN, NaN, sd(c(4, 6)), sd(c(5, 7)), NaN),
    tolerance = 1e-06
  )
})

test_that("It sets the LLOQ if it is given for any of the original data sets", {
  dataSet1 <- ospsuite::DataSet$new(name = "data1")
  dataSet2 <- ospsuite::DataSet$new(name = "data2")
  dataSet1$setValues(xValues = 1:5, yValues = 1:5)
  dataSet2$setValues(xValues = 2:5, yValues = 4:7)
  meanDataSet <- calculateMeanDataSet(list(dataSet1, dataSet2))
  expect_null(meanDataSet$LLOQ)

  dataSet2$LLOQ <- 2
  meanDataSet <- calculateMeanDataSet(list(dataSet1, dataSet2))
  expect_equal(meanDataSet$LLOQ, 2)

  dataSet1$LLOQ <- 1
  meanDataSet <- calculateMeanDataSet(list(dataSet1, dataSet2))
  expect_equal(meanDataSet$LLOQ, 1.5, tolerance = 1e-06)
})

test_that("loadObservedData returns empty list when observedData is NULL", {
  pc <- testProject()
  pc$observedData <- NULL
  result <- loadObservedData(pc)
  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("loadObservedData loads Excel data using project defaults", {
  pc <- testProject()
  result <- loadObservedData(pc)
  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_true(all(sapply(result, function(ds) inherits(ds, "DataSet"))))
})

test_that("loadObservedData loads Excel data with explicit file overrides", {
  pc <- testProject()
  pc$observedData <- list(list(
    type = "excel",
    file = "TestProject_TimeValuesData.xlsx",
    importerConfiguration = "esqlabs_dataImporter_configuration.xml",
    sheets = list("Laskin 1982.Group A")
  ))
  result <- loadObservedData(pc)
  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_true(all(sapply(result, function(ds) inherits(ds, "DataSet"))))
})

test_that("loadObservedData merges datasets from multiple entries", {
  pc <- testProject()
  pc$observedData <- list(
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
  result <- loadObservedData(pc)
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
  pc <- testProject()
  pc$observedData <- list(list(
    type = "pkml",
    file = "ObsDataAciclovir_1.pkml"
  ))
  result <- loadObservedData(pc)
  expect_type(result, "list")
  expect_length(result, 1)
  expect_true(inherits(result[[1]], "DataSet"))
  expect_equal(names(result), result[[1]]$name)
})

test_that("loadObservedData loads mixed Excel and PKML data", {
  pc <- testProject()
  pc$observedData <- list(
    list(
      type = "excel",
      file = "TestProject_TimeValuesData.xlsx",
      importerConfiguration = "esqlabs_dataImporter_configuration.xml",
      sheets = list("Laskin 1982.Group A")
    ),
    list(type = "pkml", file = "ObsDataAciclovir_1.pkml")
  )
  result <- loadObservedData(pc)
  expect_type(result, "list")
  expect_true(length(result) >= 2)
  expect_true(all(sapply(result, function(ds) inherits(ds, "DataSet"))))
})

test_that("loadObservedData loads from JSON with explicit file and importerConfiguration", {
  pc <- testProject()
  result <- loadObservedData(pc)
  expect_type(result, "list")
  expect_length(result, 2)
  expect_true(all(sapply(result, function(ds) inherits(ds, "DataSet"))))
  excelName <- "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_"
  pkmlName <- "Vergin 1995.Iv"
  expect_true(excelName %in% names(result))
  expect_true(pkmlName %in% names(result))
})

test_that("Project$observedData preserves file and importerConfiguration from JSON", {
  pc <- testProject()
  expect_length(pc$observedData, 2)
  excelEntry <- pc$observedData[[1]]
  expect_equal(excelEntry$type, "excel")
  expect_equal(excelEntry$file, "TestProject_TimeValuesData.xlsx")
  expect_equal(
    excelEntry$importerConfiguration,
    "esqlabs_dataImporter_configuration.xml"
  )
  expect_equal(excelEntry$sheets, list("Laskin 1982.Group A"))
  pkmlEntry <- pc$observedData[[2]]
  expect_equal(pkmlEntry$type, "pkml")
  expect_equal(pkmlEntry$file, "ObsDataAciclovir_1.pkml")
})

test_that("Project load errors if Excel observedData entry missing file", {
  pc <- testProject()
  pc_path <- pc$jsonPath
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
  pc <- testProject()
  pc_path <- pc$jsonPath
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
  pc <- testProject()
  pc_path <- pc$jsonPath
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
  pc <- testProject()
  pc$observedData <- list(list(
    type = "script",
    file = "scripts/test_script_single.R"
  ))
  result <- loadObservedData(pc)
  expect_type(result, "list")
  expect_length(result, 1)
  expect_true(inherits(result[[1]], "DataSet"))
  expect_equal(names(result), "ScriptGenerated")
})

test_that("loadObservedData loads script returning list of DataSets", {
  pc <- testProject()
  pc$observedData <- list(list(
    type = "script",
    file = "scripts/test_script_list.R"
  ))
  result <- loadObservedData(pc)
  expect_type(result, "list")
  expect_length(result, 2)
  expect_true(all(sapply(result, inherits, "DataSet")))
  expect_setequal(names(result), c("ScriptDataSet1", "ScriptDataSet2"))
})

test_that("loadObservedData errors if script file not found", {
  pc <- testProject()
  pc$observedData <- list(list(
    type = "script",
    file = "scripts/nonexistent.R"
  ))
  expect_error(
    loadObservedData(pc),
    regexp = "Script file not found"
  )
})

test_that("loadObservedData errors if script returns wrong type", {
  pc <- testProject()
  pc$observedData <- list(list(
    type = "script",
    file = "scripts/test_script_bad.R"
  ))
  expect_error(
    loadObservedData(pc),
    regexp = "must return DataSet"
  )
})

test_that("addObservedData with DataSet adds entry and stores DataSet by name", {
  pc <- testProject()
  ds <- ospsuite::DataSet$new(name = "ProgrammaticData")
  ds$setValues(xValues = 1:3, yValues = 1:3)
  initialCount <- length(pc$observedData)
  expect_message(
    pc$addObservedData(ds),
    regexp = "reproducibility"
  )
  expect_equal(length(pc$observedData), initialCount + 1)
  newEntry <- pc$observedData[[length(pc$observedData)]]
  expect_equal(newEntry$type, "programmatic")
  expect_null(newEntry$name)
  expect_equal(pc$.getProgrammaticDataSets()[["ProgrammaticData"]], ds)
})

test_that("addObservedData with config list adds entry directly", {
  pc <- testProject()
  initialCount <- length(pc$observedData)
  config <- list(
    type = "script",
    file = "scripts/generate_data.R"
  )
  pc$addObservedData(config)
  expect_equal(length(pc$observedData), initialCount + 1)
  newEntry <- pc$observedData[[length(pc$observedData)]]
  expect_equal(newEntry$type, "script")
  expect_equal(newEntry$file, "scripts/generate_data.R")
})

test_that("addObservedData with config list validates type", {
  pc <- testProject()
  expect_error(
    pc$addObservedData(list(type = "invalid", file = "test.xlsx")),
    regexp = "Invalid type"
  )
  expect_error(
    pc$addObservedData(list(file = "test.xlsx")),
    regexp = "must include.*type"
  )
})

test_that("loadObservedData merges programmatic and JSON-declared DataSets", {
  pc <- testProject()
  ds <- ospsuite::DataSet$new(name = "ProgrammaticData")
  ds$setValues(xValues = 1:3, yValues = 1:3)
  suppressMessages(pc$addObservedData(ds))
  result <- loadObservedData(pc)
  expect_true("ProgrammaticData" %in% names(result))
  expect_equal(result[["ProgrammaticData"]], ds)
  expect_true(length(result) > 1)
})


test_that("addObservedData errors on duplicate DataSet name", {
  pc <- testProject()
  ds1 <- ospsuite::DataSet$new(name = "SameName")
  ds1$setValues(xValues = 1:3, yValues = 1:3)
  ds2 <- ospsuite::DataSet$new(name = "SameName")
  ds2$setValues(xValues = 1:3, yValues = 4:6)
  suppressMessages(pc$addObservedData(ds1))
  expect_error(
    suppressMessages(pc$addObservedData(ds2)),
    regexp = "already exists"
  )
})

test_that("getObservedDataNames returns all DataSet names", {
  pc <- testProject()
  names_before <- getObservedDataNames(pc)
  expect_true(length(names_before) >= 1)

  ds <- ospsuite::DataSet$new(name = "NewData")
  ds$setValues(xValues = 1:3, yValues = 1:3)
  suppressMessages(pc$addObservedData(ds))

  names_after <- getObservedDataNames(pc)
  expect_true("NewData" %in% names_after)
  expect_equal(length(names_after), length(names_before) + 1)
})

test_that("addObservedData errors when name conflicts with existing loaded data", {
  pc <- testProject()
  existing_name <- getObservedDataNames(pc)[[1]]
  ds <- ospsuite::DataSet$new(name = existing_name)
  ds$setValues(xValues = 1:3, yValues = 1:3)
  expect_error(
    suppressMessages(pc$addObservedData(ds)),
    regexp = "already exists"
  )
})
