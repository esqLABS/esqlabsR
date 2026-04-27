test_that("addApplication adds an application object with empty parameters", {
  pc <- testProject()
  initial <- length(pc$applications)
  addApplication(pc, "Oral_10mg")
  expect_equal(length(pc$applications), initial + 1)
  expect_true(inherits(pc$applications[["Oral_10mg"]], "Application"))
  expect_null(pc$applications[["Oral_10mg"]]$parameters)
  expect_true(pc$modified)
})

test_that("addApplication errors on duplicate id", {
  pc <- testProject()
  existing <- names(pc$applications)[[1]]
  expect_error(
    addApplication(pc, existing),
    regexp = "already exists"
  )
})

test_that("addApplicationParameter appends to an application", {
  pc <- testProject()
  addApplication(pc, "OralX")
  addApplicationParameter(
    pc,
    "OralX",
    containerPath = "Events|Oral|Schema",
    parameterName = "Dose",
    value = 250,
    units = "mg"
  )
  pset <- pc$applications[["OralX"]]$parameters
  expect_equal(pset$paths, "Events|Oral|Schema|Dose")
  expect_equal(pset$values, 250)
  expect_equal(pset$units, "mg")
  expect_true(inherits(pc$applications[["OralX"]], "Application"))
})

test_that("addApplicationParameter errors on unknown application id", {
  pc <- testProject()
  expect_error(
    addApplicationParameter(
      pc,
      "NoSuchApp_QQ",
      containerPath = "a",
      parameterName = "x",
      value = 1,
      units = "mg"
    ),
    regexp = "not found"
  )
})

test_that("removeApplication removes the application", {
  pc <- testProject()
  addApplication(pc, "TmpApp")
  pc$modified <- FALSE
  removeApplication(pc, "TmpApp")
  expect_false("TmpApp" %in% names(pc$applications))
  expect_true(pc$modified)
})

test_that("removeApplicationParameter drops the entry", {
  pc <- testProject()
  addApplication(pc, "AppRm")
  addApplicationParameter(
    pc,
    "AppRm",
    containerPath = "Events|Oral|Schema",
    parameterName = "Dose",
    value = 250,
    units = "mg"
  )
  removeApplicationParameter(
    pc,
    "AppRm",
    containerPath = "Events|Oral|Schema",
    parameterName = "Dose"
  )
  expect_null(pc$applications[["AppRm"]]$parameters)
})

test_that("application round-trips with parameters", {
  pc <- Project$new()
  pc$modelFolder <- tempdir()
  addApplication(pc, "OralRT")
  addApplicationParameter(
    pc,
    "OralRT",
    containerPath = "Events|Oral|Schema",
    parameterName = "Dose",
    value = 100,
    units = "mg"
  )
  tmp <- tempfile(fileext = ".json")
  saveProject(pc, tmp)
  reloaded <- loadProject(tmp)
  pset <- reloaded$applications[["OralRT"]]$parameters
  expect_equal(pset$paths, "Events|Oral|Schema|Dose")
  expect_equal(pset$values, 100)
  expect_equal(pset$units, "mg")
  expect_true(inherits(reloaded$applications[["OralRT"]], "Application"))
})

test_that("loaded JSON with applications object shape parses correctly", {
  json <- '{
    "schemaVersion": "2.0",
    "esqlabsRVersion": "6.0.0",
    "applications": {
      "OralFromJson": {
        "parameters": [
          {"containerPath": "Events|Oral|Schema", "parameterName": "Dose",
           "value": 50, "units": "mg"}
        ]
      }
    }
  }'
  tmp <- tempfile(fileext = ".json")
  writeLines(json, tmp)
  pc <- loadProject(tmp)
  pset <- pc$applications[["OralFromJson"]]$parameters
  expect_equal(pset$paths, "Events|Oral|Schema|Dose")
  expect_equal(pset$values, 50)
})

test_that("project$addApplication and addApplicationParameter delegate", {
  pc1 <- testProject()
  pc2 <- testProject()
  addApplication(pc1, "DM")
  pc2$addApplication("DM")
  addApplicationParameter(
    pc1,
    "DM",
    containerPath = "a",
    parameterName = "x",
    value = 1,
    units = "mg"
  )
  pc2$addApplicationParameter(
    "DM",
    containerPath = "a",
    parameterName = "x",
    value = 1,
    units = "mg"
  )
  expect_equal(
    pc1$applications[["DM"]]$parameters,
    pc2$applications[["DM"]]$parameters
  )
})
