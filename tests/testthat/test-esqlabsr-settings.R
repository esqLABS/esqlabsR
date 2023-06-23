test_that("Names for settings are as expected", {
  expect_equal(
    esqlabsRSettingNames,
    list(
      packageVersion = "packageVersion",
      packageName = "packageName"
    )
  )
})

test_that("Check that values for package environment bindings are correct", {
  expect_error(
    getEsqlabsRSetting("xyz"),
    messages$errorPackageSettingNotFound("xyz", esqlabsEnv)
  )

  expect_equal(getEsqlabsRSetting("packageName"), "esqlabsR")
  expect_type(getEsqlabsRSetting("packageVersion"), "character")
})
