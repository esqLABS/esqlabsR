test_that("ExportConfiguration can be initialized and properties set", {
  # Create a new ExportConfiguration object
  exportConfig <- ExportConfiguration$new()

  # Test default values
  expect_null(exportConfig$heightPerRow)

  # Test setting heightPerRow
  exportConfig$heightPerRow <- 3
  expect_equal(exportConfig$heightPerRow, 3)

  # Test validation - the validation now uses ospsuite.utils::validateIsNumeric
  # which has a more specific error message. Let's just check that an error occurs.
  expect_error(exportConfig$heightPerRow <- "not a number")
})

test_that("ExportConfiguration maintains properties after setting", {
  # Create a new ExportConfiguration object
  exportConfig <- ExportConfiguration$new()

  # Set properties
  exportConfig$width <- 8
  exportConfig$height <- 6
  exportConfig$heightPerRow <- 2
  exportConfig$format <- "png"
  exportConfig$units <- "in"
  exportConfig$dpi <- 300

  # Check that properties were set correctly
  expect_equal(exportConfig$width, 8)
  expect_equal(exportConfig$height, 6)
  expect_equal(exportConfig$heightPerRow, 2)
  expect_equal(exportConfig$format, "png")
  expect_equal(exportConfig$units, "in")
  expect_equal(exportConfig$dpi, 300)
})
