test_that("`createDefaultProjectConfiguration()` works as expected", {
  # current wd to restore at the end of the test
  oldWd <- getwd()

  setwd(esqlabsR:::esqlabsR_example("TestProject/Code"))
  myConfig <- createDefaultProjectConfiguration()
  expect_true(isOfType(myConfig, "ProjectConfiguration"))

  setwd(oldWd)
})

test_that("`createDefaultProjectConfiguration()` with specified path works as expected", {
  myConfig <- createDefaultProjectConfiguration(test_ProjectConfiguration())
  expect_true(isOfType(myConfig, "ProjectConfiguration"))
})
