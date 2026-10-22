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

test_that("createEsqlabsPlotConfiguration() returns the house settings as a named list", {
  expect_equal(
    createEsqlabsPlotConfiguration(),
    list(
      titleSize = 10,
      xLabelSize = 9,
      yLabelSize = 9,
      xAxisLabelTicksSize = 8,
      yAxisLabelTicksSize = 8,
      legendKeysSize = 6,
      xLabelMargin = c(10, 0, 0, 0),
      yLabelMargin = c(0, 0, 10, 0),
      linesSize = 0.5,
      linesAlpha = 0.75,
      pointsSize = 1.75,
      pointsShape = ospsuite.plots::ospShapeNames,
      errorbarsSize = 0.65,
      errorbarsCapSize = 2.75,
      legendPosition = "top",
      legendJustification = "left",
      yAxisLabelTicksAngle = 0,
      pointsColor = .getEsqlabsColors(),
      ribbonsFill = .getEsqlabsColors(),
      linesColor = .getEsqlabsColors()
    )
  )
})

test_that("createEsqlabsPlotGridConfiguration() creates object with chosen defaults", {
  myPGC <- createEsqlabsPlotGridConfiguration()
  expect_true(isOfType(myPGC, "PlotGridConfiguration"))
  expect_equal(
    list(myPGC$tagLevels, myPGC$tagSize, myPGC$titleSize),
    list("a", 11, 12)
  )
  expect_equal(myPGC$titleHorizontalJustification, 0.5)
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
  # skipped. Build a config whose default `xLabelMargin` carries a matching NA
  # (capture the real default first to avoid recursing into the mock).
  realConfig <- createEsqlabsPlotConfiguration()
  withNA <- realConfig$xLabelMargin
  withNA[[2]] <- NA_real_
  naDefaultConfig <- realConfig
  naDefaultConfig$xLabelMargin <- withNA

  local_mocked_bindings(
    createEsqlabsPlotConfiguration = function(...) naDefaultConfig
  )
  # Current value equals the (NA-carrying) default, so the override must apply.
  plotConfiguration <- naDefaultConfig
  result <- .updatePlotConfiguration(
    plotConfiguration,
    list(xLabelMargin = c(5, 5, 5, 5))
  )

  expect_identical(result$xLabelMargin, c(5, 5, 5, 5))
})

test_that(".updatePlotConfiguration applies the override to an unset field", {
  # A list cannot hold `NULL`, so a field the user removed or never set is
  # absent, and the function-specific setting fills it.
  plotConfiguration <- createEsqlabsPlotConfiguration()
  plotConfiguration$legendPosition <- NULL

  result <- .updatePlotConfiguration(
    plotConfiguration,
    list(legendPosition = "bottom", legendTitle = "Parameter")
  )

  expect_identical(
    result[c("legendPosition", "legendTitle")],
    list(legendPosition = "bottom", legendTitle = "Parameter")
  )
})

test_that(".applyPlotConfiguration rejects a configuration that is not a named list", {
  expect_snapshot(
    error = TRUE,
    .applyPlotConfiguration(defaultPlotConfiguration = c(titleSize = 10))
  )
})

test_that(".calculateLimits widens a zero-width range to a finite interval", {
  # All-equal nonzero input on a linear axis.
  linNonzero <- .calculateLimits(c(5, 5, 5))
  expect_true(all(is.finite(linNonzero)))
  expect_gt(linNonzero[[2]], linNonzero[[1]])

  # All-zero input on a linear axis previously collapsed to c(0, 0).
  linZero <- .calculateLimits(c(0, 0, 0))
  expect_true(all(is.finite(linZero)))
  expect_gt(linZero[[2]], linZero[[1]])

  # All-equal positive input on a log axis stays strictly positive.
  logNonzero <- .calculateLimits(c(5, 5, 5), scaling = "log")
  expect_true(all(is.finite(logNonzero)))
  expect_gt(logNonzero[[1]], 0)
  expect_gt(logNonzero[[2]], logNonzero[[1]])
})
