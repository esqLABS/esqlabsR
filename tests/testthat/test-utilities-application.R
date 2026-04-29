test_that("addApplication adds an application object with empty parameters", {
  project <- testProject()
  initial <- length(project$applications)
  addApplication(project, "Oral_10mg")
  expect_equal(length(project$applications), initial + 1)
  expect_true(inherits(project$applications[["Oral_10mg"]], "Application"))
  expect_null(project$applications[["Oral_10mg"]]$parameters)
  expect_true(project$modified)
})

test_that("addApplication errors on duplicate id", {
  project <- testProject()
  existing <- names(project$applications)[[1]]
  expect_error(
    addApplication(project, existing),
    regexp = "already exists"
  )
})

test_that("addApplicationParameter appends to an application", {
  project <- testProject()
  addApplication(project, "OralX")
  addApplicationParameter(
    project,
    "OralX",
    containerPath = "Events|Oral|Schema",
    parameterName = "Dose",
    value = 250,
    units = "mg"
  )
  pset <- project$applications[["OralX"]]$parameters
  expect_equal(pset$paths, "Events|Oral|Schema|Dose")
  expect_equal(pset$values, 250)
  expect_equal(pset$units, "mg")
  expect_true(inherits(project$applications[["OralX"]], "Application"))
})

test_that("addApplicationParameter errors on unknown application id", {
  project <- testProject()
  expect_error(
    addApplicationParameter(
      project,
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
  project <- testProject()
  addApplication(project, "TmpApp")
  project$.markSaved()
  removeApplication(project, "TmpApp")
  expect_false("TmpApp" %in% names(project$applications))
  expect_true(project$modified)
})

test_that("removeApplicationParameter drops the entry", {
  project <- testProject()
  addApplication(project, "AppRm")
  addApplicationParameter(
    project,
    "AppRm",
    containerPath = "Events|Oral|Schema",
    parameterName = "Dose",
    value = 250,
    units = "mg"
  )
  removeApplicationParameter(
    project,
    "AppRm",
    containerPath = "Events|Oral|Schema",
    parameterName = "Dose"
  )
  expect_null(project$applications[["AppRm"]]$parameters)
})

test_that("application round-trips with parameters", {
  project <- Project$new()
  project$modelFolder <- tempdir()
  addApplication(project, "OralRT")
  addApplicationParameter(
    project,
    "OralRT",
    containerPath = "Events|Oral|Schema",
    parameterName = "Dose",
    value = 100,
    units = "mg"
  )
  tmp <- tempfile(fileext = ".json")
  saveProject(project, tmp)
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
  project <- loadProject(tmp)
  pset <- project$applications[["OralFromJson"]]$parameters
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
