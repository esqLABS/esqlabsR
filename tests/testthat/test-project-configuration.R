test_that("`createProjectConfiguration()` works as expected with project template", {
  myConfig <- testProjectConfiguration()
  expect_true(isOfType(myConfig, "ProjectConfiguration"))
})

test_that("A warning is (not) displayed if path/file does not exist", {
  myConfig <- testProjectConfiguration()
  expect_no_warning(myConfig$outputFolder <- "this/directory/does/not/exist")
  expect_warning(myConfig$dataFolder <- "this/directory/does/not/exist")
})


test_that("`createDefaultProjectConfiguration()` is deprecated", {
  expect_warning(createDefaultProjectConfiguration(path = example_ProjectConfiguration()))
})



test_that("Project Configuration can be created from V5 project configuration file but raises a warning", {
  expect_warning(createProjectConfiguration(test_path("..", "data", "ProjectConfiguration-V5.xlsx")))
})


test_that("Project Configuration can be exported and reimported", {
  myConfig <- testProjectConfiguration()

  temp <- file_temp(pattern = "exportedProjectConfiguration", ext = ".xlsx")
  myConfig$save(path = temp)

  expect_no_error(
    createProjectConfiguration(path = temp)
  )
})
