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
  expect_true(isOfType(myPC, "DefaultPlotConfiguration"))
  expect_equal(myPC$titleSize, 10)
})

test_that("createEsqlabsPlotGridConfiguration() creates object with chosen defaults", {
  myPGC <- createEsqlabsPlotGridConfiguration()
  expect_true(isOfType(myPGC, "PlotGridConfiguration"))
  expect_equal(myPGC$tagLevels, "a")
})

test_that("esqlabsPlotConfiguration fields match DefaultPlotConfiguration", {
  defaultConfig <- ospsuite::DefaultPlotConfiguration$new()
  esqlabsConfig <- createEsqlabsPlotConfiguration()

  defaultFields <- names(defaultConfig)
  esqlabsFields <- names(esqlabsConfig)

  missingFields <- setdiff(defaultFields, esqlabsFields)
  expect_true(
    length(missingFields) == 0,
    info = paste("Missing fields:", paste(missingFields, collapse = ", "))
  )

  esqlabsConfig$linesColor <- NULL
  esqlabsConfig$legendPosition <- NULL

  for (field in defaultFields) {
    expect_equal(
      class(esqlabsConfig[[field]]),
      class(defaultConfig[[field]]),
      info = paste("Field", field, "has different types")
    )
  }
})

test_that("createEsqlabsPlotConfiguration() works with ospsuite::plotIndividualTimeProfile", {
  oneObsSimDC <- readRDS(getTestDataFilePath("oneObsSimDC"))
  esqlabsConfig <- createEsqlabsPlotConfiguration()

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "time profile - esqlabsPlotConfiguration",
    fig = plotIndividualTimeProfile(oneObsSimDC, esqlabsConfig)
  )
})

test_that(".updatePlotConfiguration is NA-safe when a field comparison yields NA", {
  plotConfiguration <- createEsqlabsPlotConfiguration()
  plotConfiguration$titleSize <- NA_real_

  result <- .updatePlotConfiguration(
    plotConfiguration,
    list(titleSize = 99)
  )

  expect_identical(result$titleSize, NA_real_)
})
