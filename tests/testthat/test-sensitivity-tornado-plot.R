# Single output path ------------------------------------------------------

simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
parameterPaths <- c(
  "Aciclovir|Lipophilicity",
  "Events|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
  "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR-Aciclovir|GFR fraction"
)
variationRange <- c(0.1, 2, 10) # 1.0 is deliberately left out for testing

# `loadSimulation()` initializes a PK-Sim native session; running it at file
# source time (as `test_dir()` sources every test file up front) bleeds native
# state across files. Defer it behind memoized accessors so the native load and
# the time-consuming baseline `sensitivityCalculation()` happen inside a
# `test_that()` block on first use, computed once and cached for the rest of the
# file.
sensFixture <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      simulation <- loadSimulation(simPath)
      set.seed(123)
      results <- sensitivityCalculation(
        simulation = simulation,
        outputPaths = outputPaths,
        parameterPaths = parameterPaths,
        variationRange = variationRange
      )
      cache <<- list(simulation = simulation, results = results)
    }
    cache
  }
})

sensFixtureMultiple <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      simulation <- loadSimulation(simPath)
      outputPathsMultiple <- c(
        "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
        "Organism|Age",
        "Organism|ArterialBlood|Plasma|Aciclovir"
      )
      parameterPathsMultiple <- c(
        "Aciclovir|Lipophilicity",
        "Events|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
        "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR-Aciclovir|GFR fraction"
      )
      resultsMultiple <- sensitivityCalculation(
        simulation = simulation,
        outputPaths = outputPathsMultiple,
        parameterPaths = parameterPathsMultiple,
        variationRange = c(0.1, 10)
      )
      cache <<- list(resultsMultiple = resultsMultiple)
    }
    cache
  }
})


# Validate plotting arguments ---------------------------------------------

test_that("sensitivityTornadoPlot fails with incorrect input", {
  expect_error(
    sensitivityTornadoPlot("x"),
    regexp = messages$errorWrongType(
      "sensitivityCalculation",
      "character",
      "SensitivityCalculation"
    ),
    fixed = TRUE
  )
})

test_that("sensitivityTornadoPlot fails with invalid parameterFactor", {
  results <- sensFixture()$results
  expect_error(
    sensitivityTornadoPlot(results, parameterFactor = 0),
    "parameterFactor.*out of the allowed range"
  )
})

test_that("sensitivityTornadoPlot fails with invalid xAxisZoomRange", {
  results <- sensFixture()$results
  xAxisZoomRange <- 100
  expect_error(
    sensitivityTornadoPlot(results, xAxisZoomRange = xAxisZoomRange),
    messages$errorWrongLength(xAxisZoomRange, 2),
    fixed = TRUE
  )
})

test_that("sensitivityTornadoPlot errors if parameterFactor is missing in sensitivity calculation results", {
  results <- sensFixture()$results
  expect_error(
    sensitivityTornadoPlot(results, parameterFactor = 0.2),
    "are not included in the sensitivity analysis results"
  )
})

# Default plot ------------------------------------------------------------

test_that("sensitivityTornadoPlot creates default plot", {
  results <- sensFixture()$results
  set.seed(123)
  p <- sensitivityTornadoPlot(results)

  set.seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "sensitivityTornadoPlot works as expected",
      fig = p
    )
  )
})

test_that("sensitivityTornadoPlot creates default plot with custom parameter path labels", {
  simulation <- sensFixture()$simulation
  # Work on a labelled local copy so the file-scope `parameterPaths` is never
  # mutated for other tests in this file.
  namedParameterPaths <- parameterPaths
  names(namedParameterPaths) <- c("Lipophilicity", "Dose", "GFR fraction")

  resultsLab <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = namedParameterPaths,
    variationRange = variationRange
  )

  p <- sensitivityTornadoPlot(resultsLab)

  vdiffr::expect_doppelganger(
    title = "sensitivityTornadoPlot works with user parameter path names",
    fig = suppressWarnings(p)
  )

  n <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  pb <- ggplot2::ggplot_build(p[[n]][[1]])

  expect_setequal(
    names(namedParameterPaths),
    pb$layout$panel_params[[1]]$y$get_labels()
  )
})

# Default plot with custom PK parameter -----------------------------------

test_that("sensitivityTornadoPlot works with custom PK parameter", {
  simulation <- sensFixture()$simulation
  customFun <- list("y_max" = function(y) max(y, na.rm = TRUE))

  resultsCustomPK <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    customOutputFunctions = customFun,
    variationRange = variationRange
  )

  set.seed(123)
  p <- sensitivityTornadoPlot(resultsCustomPK)

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "sensitivityTornadoPlot custom PK Parameter",
    fig = suppressWarnings(p)
  )
})


# Default plot with x-axis zoom -------------------------------------------

test_that("sensitivityTornadoPlot applies x-axis zoom range correctly", {
  results <- sensFixture()$results
  set.seed(123)
  p <- sensitivityTornadoPlot(results, xAxisZoomRange = c(-100, 100))

  set.seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "sensitivityTornadoPlot zoomed",
      fig = p
    )
  )
})


# Multiple output paths ---------------------------------------------------

test_that("sensitivityTornadoPlot handles multiple output paths", {
  resultsMultiple <- sensFixtureMultiple()$resultsMultiple
  set.seed(123)
  plotsMultiple <- sensitivityTornadoPlot(resultsMultiple)

  set.seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "multiple output path tornado",
      fig = plotsMultiple
    )
  )
})

# Filter data to be plotted -----------------------------------------------

outputPathsFilter <- "Organism|ArterialBlood|Plasma|Aciclovir"
parameterPathsFilter <- "Aciclovir|Lipophilicity"
pkParametersFilter <- c("AUC_inf", "C_max")

test_that("sensitivityTornadoPlot plots are as expected with filters", {
  resultsMultiple <- sensFixtureMultiple()$resultsMultiple
  set.seed(123)
  plotFiltered <- sensitivityTornadoPlot(
    resultsMultiple,
    outputPaths = outputPathsFilter,
    parameterPaths = parameterPathsFilter,
    pkParameters = pkParametersFilter
  )

  set.seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "filtered tornado",
      fig = plotFiltered
    )
  )
})

# .splitParameterName ----------------------------------------------------

test_that(".splitParameterName inserts a line break after the third pipe", {
  split <- esqlabsR:::.splitParameterName

  # NULL passes through unchanged, regardless of `equalLines`.
  expect_null(split(NULL))
  expect_null(split(NULL, equalLines = TRUE))

  # Fewer than three pipes: unchanged by default, a trailing "\n" appended
  # only when `equalLines = TRUE` (to keep multi-line labels vertically even).
  expect_equal(split("a"), "a")
  expect_equal(split("a", equalLines = TRUE), "a\n")
  expect_equal(split("a|b|c"), "a|b|c")
  expect_equal(split("a|b|c", equalLines = TRUE), "a|b|c\n")

  # Three or more pipes: a newline is inserted after the third pipe. This
  # branch wins over `equalLines`, so both calls give the same result.
  expect_equal(split("a|b|c|d"), "a|b|c|\nd")
  expect_equal(split("a|b|c|d", equalLines = TRUE), "a|b|c|\nd")
  expect_equal(split("a|b|c|d|e"), "a|b|c|\nd|e")
})
