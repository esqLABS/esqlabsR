test_that("Names for settings are as expected", {
  expect_snapshot(esqlabsRSettingNames)
})

test_that("Check that values for package environment bindings are correct", {
  expect_error(
    getEsqlabsRSetting("xyz"),
    "No global setting with the name 'xyz' exists."
  )

  expect_equal(getEsqlabsRSetting("packageName"), "esqlabsR")
})
