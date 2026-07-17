test_that("Names for settings are as expected", {
  expect_equal(
    esqlabsRSettingNames,
    list(
      packageVersion = "packageVersion",
      packageName = "packageName",
      colorPalette = "colorPalette"
    )
  )
})

test_that("Check that values for package environment bindings are correct", {
  expect_snapshot(getEsqlabsRSetting("xyz"), error = TRUE)

  expect_equal(getEsqlabsRSetting("packageName"), "esqlabsR")
  expect_type(getEsqlabsRSetting("packageVersion"), "character")
})
