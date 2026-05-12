test_that("loadProject() returns a Project from a valid Project.json", {
  project <- loadProject(testProjectJSONPath())
  expect_s3_class(project, "Project")
  expect_equal(project$schemaVersion, "2.0")
  expect_equal(length(project$scenarios), 4)
})

test_that("loadProject() errors when the file does not exist", {
  expect_error(
    loadProject(file.path(tempdir(), "does_not_exist.json")),
    regexp = "does not exist"
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
