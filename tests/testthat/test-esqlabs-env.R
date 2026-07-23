test_that("esqlabsRSettingNames lists the expected settings", {
  expect_equal(
    esqlabsRSettingNames,
    list(
      packageVersion = "packageVersion",
      packageName = "packageName",
      colorPalette = "colorPalette"
    )
  )
})

test_that("getEsqlabsRSetting returns correct settings", {
  expect_equal(getEsqlabsRSetting("packageName"), "esqlabsR")
  expect_type(getEsqlabsRSetting("packageVersion"), "character")

  expect_snapshot(getEsqlabsRSetting("nonExistentSetting"), error = TRUE)
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
