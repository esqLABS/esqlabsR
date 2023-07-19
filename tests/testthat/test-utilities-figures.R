## context("esqLABS_colors")

test_that("esqLABS_colors input validation works as expected", {
  expect_error(esqLABS_colors(-1), messages$nrOfColorsShouldBePositive(-1))
})

test_that("esqLABS_colors wprks with empty argument vector", {
  expect_length(esqLABS_colors(0), 0)
})

test_that("esqLABS_colors returns two colors", {
  expect_length(esqLABS_colors(2), 2)
})

test_that("esqLABS_colors returns three colors", {
  expect_length(esqLABS_colors(3), 3)
})

test_that("esqLABS_colors returns ten colors", {
  expect_length(esqLABS_colors(10), 10)
})

test_that("esqLABS_colors returns ten colors", {
  expect_length(esqLABS_colors(10), 10)
})

## context("col2hsv")

test_that("col2hsv returns expected HSV values for a given R color name", {
  expect_equal(
    col2hsv("yellow"),
    structure(c(0.166666666666667, 1, 1),
      .Dim = c(3L, 1L),
      .Dimnames = list(c("h", "s", "v"), NULL)
    )
  )

  expect_equal(
    col2hsv("white"),
    structure(c(0, 0, 1),
      .Dim = c(3L, 1L),
      .Dimnames = list(c("h", "s", "v"), NULL)
    )
  )
})



## context("createEsqlabsPlotConfiguration")

test_that("createEsqlabsPlotConfiguration() creates object with chosen defaults", {
  myPC <- createEsqlabsPlotConfiguration()
  expect_true(isOfType(myPC, "DefaultPlotConfiguration"))
  expect_equal(myPC$titleSize, 12)
})

## context("createEsqlabsPlotGridConfiguration")

test_that("createEsqlabsPlotGridConfiguration() creates object with chosen defaults", {
  myPGC <- createEsqlabsPlotGridConfiguration()
  expect_true(isOfType(myPGC, "PlotGridConfiguration"))
  expect_equal(myPGC$tagLevels, "a")
})

## context("createEsqlabsExportConfiguration")

test_that("createEsqlabsExportConfiguration() creates object with chosen defaults", {
  myProjConfig <- ProjectConfiguration$new()
  myEC <- createEsqlabsExportConfiguration(myProjConfig)
  expect_true(isOfType(myEC, "ExportConfiguration"))
  expect_equal(myEC$units, "cm")
})
