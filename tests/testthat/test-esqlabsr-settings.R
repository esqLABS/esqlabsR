test_that("Names for settings are as expected", {
  expect_snapshot(esqlabsRSettingNames)
})

test_that("Check that values for package environment bindings are correct", {
  expect_error(
    getEsqlabsRSetting("xyz"),
    messages$errorPackageSettingNotFound("xyz", esqlabsEnv)
  )

  expect_equal(getEsqlabsRSetting("packageName"), "esqlabsR")
})
