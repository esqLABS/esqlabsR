test_that("loadProject() returns a Project from a valid Project.json", {
  project <- loadProject(testProjectJSONPath())
  expect_s3_class(project, "Project")
  expect_equal(project$schemaVersion, "2.0")
  expect_equal(length(project$scenarios), 4)
})

test_that("loadProject() errors when the file does not exist", {
  expect_error(
    loadProject(file.path(tempdir(), "does_not_exist.json")),
    regexp = "(does not exist|not found)"
  )
})

test_that("loadProject() errors on an unsupported schemaVersion", {
  badPath <- withr::local_tempfile(fileext = ".json")
  writeLines(
    '{"schemaVersion": "1.0", "filePaths": {}}',
    badPath
  )
  expect_error(
    loadProject(badPath),
    regexp = "Unsupported schemaVersion"
  )
})

test_that("saveProject writes a Project to disk and clears modified flag", {
  project <- loadProject(testProjectJSONPath())
  project$modelFolder <- "AnotherModels"
  expect_true(project$modified)

  tmp <- withr::local_tempfile(fileext = ".json")
  saveProject(project, tmp)
  expect_true(file.exists(tmp))
  expect_false(project$modified)
})

test_that("saveProject defaults to project$jsonPath when path is NULL", {
  tmp_src <- withr::local_tempfile(fileext = ".json")
  project <- loadProject(testProjectJSONPath())
  saveProject(project, tmp_src)
  reloaded <- loadProject(tmp_src)
  reloaded$modelFolder <- "Models2"
  saveProject(reloaded)
  expect_false(reloaded$modified)
})

test_that("saveProject errors when project has no jsonPath and path is NULL", {
  project <- Project$new()
  expect_snapshot(saveProject(project), error = TRUE)
})

test_that("saveProject errors on non-Project input", {
  expect_snapshot(saveProject("not a project"), error = TRUE)
})

test_that("loadProject(exampleProjectPath()) succeeds", {
  path <- exampleProjectPath()
  expect_true(file.exists(path))
  project <- loadProject(path)
  expect_s3_class(project, "Project")
  expect_equal(project$schemaVersion, "2.0")
})
