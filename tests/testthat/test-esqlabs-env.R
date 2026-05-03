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
