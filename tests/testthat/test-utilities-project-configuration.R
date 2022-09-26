test_that("`createDefaultProjectConfiguration()` works as expected", {
  # current wd to restore at the end of the test
  oldWd <- getwd()

  setwd(file.path(oldWd, "..", "data", "TestProject", "Code"))
  myConfig <- createDefaultProjectConfiguration()
  expect_true(isOfType(myConfig, "ProjectConfiguration"))

  setwd(oldWd)
})

test_that("`createDefaultProjectConfiguration()` with specified path works as expected", {
  path <- file.path(getwd(), "..", "data", "TestProject", "Code", "ProjectConfiguration.xlsx")
  myConfig <- createDefaultProjectConfiguration(path)
  expect_true(isOfType(myConfig, "ProjectConfiguration"))
})
