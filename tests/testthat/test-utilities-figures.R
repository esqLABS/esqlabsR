# esqlabsColors ----------------------------------------------------------

test_that("esqlabsColors input validation works as expected", {
  expect_error(
    esqlabsColors(-1),
    regexp = messages$nrOfColorsShouldBePositive(-1),
    fixed = TRUE
  )
})

test_that("esqlabsColors works with empty argument vector", {
  expect_length(esqlabsColors(0), 0)
})

test_that("esqlabsColors returns two colors", {
  expect_length(esqlabsColors(2), 2)
})

test_that("esqlabsColors returns three colors", {
  expect_length(esqlabsColors(3), 3)
})

test_that("esqlabsColors returns ten colors", {
  expect_length(esqlabsColors(10), 10)
})

test_that("esqlabsColors returns ten colors", {
  expect_length(esqlabsColors(10), 10)
})

# col2hsv -----------------------------------------------------------------

test_that("col2hsv returns expected HSV values for a given R color name", {
  expect_equal(
    col2hsv("yellow"),
    structure(
      c(0.166666666666667, 1, 1),
      .Dim = c(3L, 1L),
      .Dimnames = list(c("h", "s", "v"), NULL)
    )
  )

  expect_equal(
    col2hsv("white"),
    structure(
      c(0, 0, 1),
      .Dim = c(3L, 1L),
      .Dimnames = list(c("h", "s", "v"), NULL)
    )
  )
})

# createEsqlabsPlotConfiguration ------------------------------------------

test_that("createEsqlabsPlotConfiguration() creates object with chosen defaults", {
  myPC <- createEsqlabsPlotConfiguration()
  expect_type(myPC, "list")
  expect_equal(myPC$titleSize, 10)
})

test_that("createEsqlabsPlotGridConfiguration() creates object with chosen defaults", {
  myPGC <- createEsqlabsPlotGridConfiguration()
  expect_true(isOfType(myPGC, "PlotGridConfiguration"))
  expect_equal(myPGC$tagLevels, "a")
})

# single observed and simulated datasets
oneObsSimDC <- readRDS(getTestDataFilePath("oneObsSimDC"))

test_that(".parseExcelMultiValueField numeric conversion path is covered", {
  # Direct test to ensure numeric conversion code path is covered
  result <- esqlabsR:::.parseExcelMultiValueField(
    value = "72.5, 80.5",
    fieldName = "test",
    plotID = "P1",
    expectedLength = 2,
    expectedType = "numeric"
  )
  expect_equal(result, c(72.5, 80.5))
  expect_true(is.numeric(result))

  # Test space-separated numeric values trigger correct error
  expect_error(
    esqlabsR:::.parseExcelMultiValueField(
      value = "72 80",
      fieldName = "test",
      plotID = "P1",
      expectedLength = 2,
      expectedType = "numeric"
    ),
    regexp = "Invalid format.*Expected.*Values separated by commas",
    fixed = FALSE
  )
})
