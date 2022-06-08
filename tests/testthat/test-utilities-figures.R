## context("esqLABS_colors")
test_that("esqLABS_colors returns two colors", {
  expect_equal(length(esqLABS_colors(2)), 2)
})

test_that("esqLABS_colors returns ten colors", {
  expect_equal(length(esqLABS_colors(10)), 10)
})

## context("createEsqlabsPlotConfiguration")

test_that("createEsqlabsPlotConfiguration() creates object with chosen defaults", {
  myPC <- createEsqlabsPlotConfiguration()
  expect_s3_class(myPC, "DefaultPlotConfiguration")
  expect_equal(myPC$titleSize, 8)
})

## context("createEsqlabsPlotGridConfiguration")

test_that("createEsqlabsPlotGridConfiguration() creates object with chosen defaults", {
  myPGC <- createEsqlabsPlotGridConfiguration()
  expect_s3_class(myPGC, "PlotGridConfiguration")
  expect_equal(myPGC$tagLevels, "a")
})

## context("createEsqlabsExportConfiguration")

test_that("createEsqlabsExportConfiguration() creates object with chosen defaults", {
  myProjConfig <- ProjectConfiguration$new()
  myEC <- createEsqlabsExportConfiguration(myProjConfig)
  expect_s3_class(myEC, "ExportConfiguration")
  expect_equal(myEC$units, "cm")
})
