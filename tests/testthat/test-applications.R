test_that("addApplication + removeApplication round-trip leaves the section unchanged", {
  project <- testProject()
  before <- project$applications

  addApplication(project, "NewApp")
  expect_true("NewApp" %in% names(project$applications))

  removeApplication(project, "NewApp")
  expect_identical(project$applications, before)
})

test_that("removeApplication warns on missing key and is a no-op", {
  project <- testProject()
  expect_warning(removeApplication(project, "Ghost"), "not found")
})
