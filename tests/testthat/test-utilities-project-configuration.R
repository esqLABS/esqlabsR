test_that("`createDefaultProjectConfiguration()` works as expected", {
  path <- file.path(getwd(), "..", "data", "ProjectConfiguration.xlsx")
  myConfig <- createDefaultProjectConfiguration(path)
  expect_true(isOfType(myConfig, "ProjectConfiguration"))
})
