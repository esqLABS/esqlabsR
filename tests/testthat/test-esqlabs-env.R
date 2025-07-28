test_that("esqlabsRSettingNames function returns correct enum", {
  # Check that esqlabsRSettingNames returns an enum
  expect_true(is.list(esqlabsRSettingNames))
  expect_true(all(
    c("packageName", "packageVersion") %in% names(esqlabsRSettingNames)
  ))
})

test_that("getEsqlabsRSetting returns correct settings", {
  # Check packageName
  expect_equal(getEsqlabsRSetting("packageName"), "esqlabsR")

  # Check packageVersion
  expect_true(is.character(getEsqlabsRSetting("packageVersion")))

  # Check error for non-existent setting
  # The error message contains the list of available settings which may change over time
  # so we just check that an error is thrown without checking the specific message
  expect_error(getEsqlabsRSetting("nonExistentSetting"))
})

test_that(".getEsqlabsColors returns the expected colors", {
  # Access the internal function
  colors <- esqlabsR:::.getEsqlabsColors()

  # Test that it returns expected format
  expect_true(is.character(colors))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))

  # Test that it includes the first fixed colors
  firstColors <- esqlabsR:::esqlabsColors(3)
  expect_true(all(firstColors %in% colors))

  # The number of colors might change, so instead of testing the exact number,
  # we'll check that it's more than the minimum expected
  expect_true(length(colors) >= 52)
})
