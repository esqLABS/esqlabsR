test_that("`createDefaultProjectConfiguration()` works as expected", {
  myConfig <- createProjectConfiguration(path = example_ProjectConfiguration())
  expect_true(isOfType(myConfig, "ProjectConfiguration"))

  expect_snapshot(myConfig)
})

test_that("`createDefaultProjectConfiguration()` with specified path works as expected", {
  myConfig <- createProjectConfiguration(test_ProjectConfiguration())
  expect_true(isOfType(myConfig, "ProjectConfiguration"))
})

test_that("A warning is (not) displayed if path/file does not exist", {
  myConfig <- createProjectConfiguration(test_ProjectConfiguration())
  expect_no_warning(myConfig$outputFolder <- "this/directory/does/not/exist")
  expect_warning(myConfig$dataFolder <- "this/directory/does/not/exist")
})


test_that("`createDefaultProjectConfiguration()` is deprecated", {
  expect_warning(createDefaultProjectConfiguration(path = example_ProjectConfiguration()))
})
