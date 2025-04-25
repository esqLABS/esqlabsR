test_that("`executeInParallel()` works as expected", {
  skip_on_os("mac")

  chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")

  # Limiting the number of cores so the test does not fail when building
  # https://stackoverflow.com/questions/50571325/r-cran-check-fail-when-using-parallel-functions
  if (nzchar(chk) && chk == "TRUE") {
    # use 2 cores in CRAN/Travis/AppVeyor
    num_workers <- 2L
  } else {
    # use all cores in devtools::test()
    num_workers <- ospsuite::getOSPSuiteSetting("numberOfCores")
  }

  v1 <- 1:4
  v2 <- 8:100

  x <- executeInParallel(
    "mean",
    list(v1, v2),
    outputNames = c("res1", "res2"),
    nrOfCores = num_workers
  )

  expect_equal(x[["res1"]], mean(v1), tolerance = 0.01)
  expect_equal(x[["res2"]], mean(v2), tolerance = 0.01)

  expect_error(executeInParallel("log", list(letters[1:4], LETTERS[1:4])))
})

test_that("`executeInParallel()` works with custom functions", {
  skip_on_os("mac")

  # Define a custom function that uses an exported value
  squareAndAdd <- function(x, y) {
    return(x^2 + y)
  }

  valueToExport <- 10

  result <- executeInParallel(
    squareAndAdd,
    list(1, 2, 3, 4),
    exports = "valueToExport",
    y = valueToExport,
    nrOfCores = 2
  )

  expect_equal(result, list(11, 14, 19, 26))

  # Test validation of outputNames length
  expect_error(
    executeInParallel(
      "mean",
      list(1:4, 5:8),
      outputNames = c("one"),
      nrOfCores = 2
    ),
    "must have the same length"
  )
})
