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

  withr::local_seed(123)
  vdiffr::expect_doppelganger(
    title = "time profile - esqlabsPlotConfiguration",
    fig = plotIndividualTimeProfile(oneObsSimDC, esqlabsConfig)
  )
})

test_that(".updatePlotConfiguration keeps a user-customized field over the override", {
  # The field differs from the default (the user set it), so the override is
  # NOT applied even though the current value carries an NA. A plain `==`
  # comparison also yields NA here, so this case passed before and after.
  plotConfiguration <- createEsqlabsPlotConfiguration()
  plotConfiguration$titleSize <- NA_real_

  result <- .updatePlotConfiguration(
    plotConfiguration,
    list(titleSize = 99)
  )

  expect_identical(result$titleSize, NA_real_)
})

test_that(".updatePlotConfiguration applies the override when a default value carries an NA", {
  # Regression for the NA-unsafe default comparison: when the current value
  # still equals the default AND that value contains an NA, `all(x == x)`
  # returns NA and `isTRUE(NA)` is FALSE, so a legit override was silently
  # skipped. Build a config whose default `titleMargin` carries a matching NA
  # (capture the real default first to avoid recursing into the mock).
  realConfig <- createEsqlabsPlotConfiguration()
  withNA <- realConfig$titleMargin
  withNA[[2]] <- NA_real_
  naDefaultConfig <- realConfig
  naDefaultConfig$titleMargin <- withNA

  local_mocked_bindings(
    createEsqlabsPlotConfiguration = function(...) naDefaultConfig
  )
  # Current value equals the (NA-carrying) default, so the override must apply.
  plotConfiguration <- naDefaultConfig
  result <- .updatePlotConfiguration(
    plotConfiguration,
    list(titleMargin = c(5, 5, 5, 5))
  )

  expect_identical(result$titleMargin, c(5, 5, 5, 5))
})

test_that(".calculateLimits widens a zero-width range to a finite interval", {
  # All-equal nonzero input on a linear axis.
  linNonzero <- esqlabsR:::.calculateLimits(c(5, 5, 5))
  expect_true(all(is.finite(linNonzero)))
  expect_gt(linNonzero[[2]], linNonzero[[1]])

  # All-zero input on a linear axis previously collapsed to c(0, 0).
  linZero <- esqlabsR:::.calculateLimits(c(0, 0, 0))
  expect_true(all(is.finite(linZero)))
  expect_gt(linZero[[2]], linZero[[1]])

  # All-equal positive input on a log axis stays strictly positive.
  logNonzero <- esqlabsR:::.calculateLimits(c(5, 5, 5), scaling = "log")
  expect_true(all(is.finite(logNonzero)))
  expect_gt(logNonzero[[1]], 0)
  expect_gt(logNonzero[[2]], logNonzero[[1]])
})
