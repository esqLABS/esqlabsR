# The numeric-snapshot blocks below capture axis ranges whose formatting depends
# on print/precision options. Each such block sets them locally with
# `withr::local_options()` (see `.localSnapshotOptions()`), so the state is
# scoped to the test rather than leaked at file scope.
.localSnapshotOptions <- function(.local_envir = parent.frame()) {
  withr::local_options(
    tibble.width = Inf,
    pillar.min_title_chars = Inf,
    pillar.sigfig = 4,
    digits = 4,
    scipen = 999,
    .local_envir = .local_envir
  )
}

# Single output path ------------------------------------------------------

simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
parameterPaths <- c(
  "Aciclovir|Lipophilicity",
  "Events|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
  "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR-Aciclovir|GFR fraction"
)
variationRange <- c(0.1, 2, 20) # 1.0 is deliberately left out for testing

# `loadSimulation()` initializes a PK-Sim native session; running it at file
# source time (as `test_dir()` sources every test file up front) bleeds native
# state across files. Defer it behind memoized accessors so the native load and
# the baseline `sensitivityCalculation()` happen inside a `test_that()` block on
# first use, computed once and cached for the rest of the file.
sensFixture <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      simulation <- loadSimulation(simPath)
      withr::local_seed(123)
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
        variationRange = c(1, 5, 10)
      )
      cache <<- list(resultsMultiple = resultsMultiple)
    }
    cache
  }
})


# Validate plotting arguments ---------------------------------------------

test_that("sensitivitySpiderPlot fails with invalid input", {
  expect_error(
    sensitivitySpiderPlot("x"),
    regexp = messages$errorWrongType(
      "sensitivityCalculation",
      "character",
      "SensitivityCalculation"
    ),
    fixed = TRUE
  )
})

# Default plot ------------------------------------------------------------

test_that("sensitivitySpiderPlot creates expected default plot", {
  .localSnapshotOptions()
  results <- sensFixture()$results
  withr::local_seed(123)
  p <- sensitivitySpiderPlot(results)

  withr::local_seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "sensitivitySpiderPlot works as expected",
      fig = p
    )
  )
})

test_that("sensitivitySpiderPlot legend labels are correctly applied", {
  .localSnapshotOptions()
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

  p <- sensitivitySpiderPlot(resultsLab)

  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "sensitivitySpiderPlot works with user parameter path names",
      fig = p
    )
  )

  n <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  pb <- ggplot2::ggplot_build(p[[n]][[1]])

  expect_equal(
    levels(as.factor(names(namedParameterPaths))),
    as.character(pb$plot$scales$get_scales("colour")$get_labels())
  )
})

# Default plot with custom PK parameter -----------------------------------

test_that("sensitivitySpiderPlot handles custom PK parameters", {
  .localSnapshotOptions()
  simulation <- sensFixture()$simulation
  customFun <- list("minmax" = function(y) max(y) / min(y[y != 0]))

  resultsCustomPK <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    customOutputFunctions = customFun,
    variationRange = variationRange
  )

  withr::local_seed(123)
  p <- sensitivitySpiderPlot(resultsCustomPK)

  withr::local_seed(123)
  vdiffr::expect_doppelganger(
    title = "sensitivitySpiderPlot custom PK Parameter",
    fig = suppressWarnings(p)
  )
})

# Parameterized plots -----------------------------------------------------

n <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

test_that("sensitivitySpiderPlot applies free y-axis scaling correctly", {
  .localSnapshotOptions()
  results <- sensFixture()$results
  withr::local_seed(123)
  p <- sensitivitySpiderPlot(results, yAxisFacetScales = "free")
  pbs <- purrr::map(seq_along(p[[n]]), ~ ggplot2::ggplot_build(p[[n]][[.x]]))
  plotParams <- purrr::map(pbs, ~ .x$layout$panel_params[[1]]$y.range)

  expect_snapshot(unlist(plotParams))
})

test_that("sensitivitySpiderPlot correctly applies absolute y-axis values correctly", {
  .localSnapshotOptions()
  results <- sensFixture()$results
  withr::local_seed(123)
  p <- sensitivitySpiderPlot(results, yAxisType = "absolute")

  withr::local_seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "sensitivitySpiderPlot works as expected with absolute y-values",
      fig = p
    )
  )

  pbs <- purrr::map(seq_along(p[[n]]), ~ ggplot2::ggplot_build(p[[n]][[.x]]))
  plotParams <- purrr::map(pbs, ~ .x$layout$panel_params[[1]]$y.range)

  expect_snapshot(unlist(plotParams))
})

test_that("sensitivitySpiderPlot applies absolute x-axis values correctly", {
  .localSnapshotOptions()
  results <- sensFixture()$results
  withr::local_seed(123)
  p <- sensitivitySpiderPlot(
    results,
    xAxisType = "absolute",
    # select parameter paths with non-negative values
    parameterPaths = parameterPaths[2:3]
  )

  withr::local_seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "sensitivitySpiderPlot works as expected with absolute x-values",
      fig = p
    )
  )
})

test_that("sensitivitySpiderPlot applies absolute x- and y-axis values correctly", {
  .localSnapshotOptions()
  results <- sensFixture()$results
  withr::local_seed(123)
  p1 <- sensitivitySpiderPlot(
    results,
    xAxisType = "absolute",
    yAxisType = "absolute",
    xAxisScale = "log",
    yAxisScale = "lin"
  ) # default scales
  p2 <- sensitivitySpiderPlot(
    results,
    xAxisType = "absolute",
    yAxisType = "absolute",
    xAxisScale = "lin",
    yAxisScale = "log"
  )

  expect_snapshot(extractAxisRange(p1))
  expect_snapshot(extractAxisRange(p2))
})

test_that("sensitivitySpiderPlot applies free scaling with absolute y-values", {
  .localSnapshotOptions()
  results <- sensFixture()$results
  withr::local_seed(123)
  p <- sensitivitySpiderPlot(
    results,
    yAxisType = "absolute",
    yAxisFacetScales = "free"
  )
  pbs <- purrr::map(seq_along(p[[n]]), ~ ggplot2::ggplot_build(p[[n]][[.x]]))
  plotParams <- list(
    unlist(
      plotParams <- purrr::map(pbs, ~ .x$layout$panel_params[[1]]$y.range)
    ),
    unlist(plotParams <- purrr::map(pbs, ~ .x$plot$labels$y))
  )

  expect_snapshot(unlist(plotParams))
})

# Plot configuration ------------------------------------------------------

n <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

test_that("sensitivitySpiderPlot uses defaultPlotConfiguration scales", {
  results <- sensFixture()$results
  myPlotConfiguration <- createEsqlabsPlotConfiguration()
  myPlotConfiguration$xAxisScale <- "lin"
  myPlotConfiguration$yAxisScale <- "log"

  p <- sensitivitySpiderPlot(
    results,
    defaultPlotConfiguration = myPlotConfiguration
  )
  pb <- ggplot2::ggplot_build(p[[n]][[1]])

  expect_equal(pb$layout$panel_scales_x[[1]]$trans$name, "identity")
  expect_equal(pb$layout$panel_scales_y[[1]]$trans$name, "log-10")
})

test_that("sensitivitySpiderPlot signature overrides defaultPlotConfiguration", {
  results <- sensFixture()$results
  myPlotConfiguration <- createEsqlabsPlotConfiguration()
  myPlotConfiguration$xAxisScale <- "lin" # to be overridden
  myPlotConfiguration$yAxisScale <- "log" # to be overridden

  p <- sensitivitySpiderPlot(
    results,
    defaultPlotConfiguration = myPlotConfiguration,
    xAxisScale = "log",
    yAxisScale = "lin"
  )
  pb <- ggplot2::ggplot_build(p[[n]][[1]])

  expect_equal(pb$layout$panel_scales_x[[1]]$trans$name, "log-10")
  expect_equal(pb$layout$panel_scales_y[[1]]$trans$name, "identity")
})

# Multiple output paths ---------------------------------------------------

test_that("sensitivitySpiderPlot handles multiple output paths correctly", {
  .localSnapshotOptions()
  resultsMultiple <- sensFixtureMultiple()$resultsMultiple
  withr::local_seed(123)
  plotsMultiple <- sensitivitySpiderPlot(resultsMultiple)

  withr::local_seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "multiple output path spiders",
      fig = plotsMultiple
    )
  )
})

# Filter data to be plotted -----------------------------------------------

outputPathsFilter <- "Organism|ArterialBlood|Plasma|Aciclovir"
parameterPathsFilter <- "Aciclovir|Lipophilicity"
pkParametersFilter <- c("C_max", "t_max")

test_that("sensitivitySpiderPlot plots as expected with filters", {
  .localSnapshotOptions()
  resultsMultiple <- sensFixtureMultiple()$resultsMultiple
  withr::local_seed(123)
  plotFiltered <- sensitivitySpiderPlot(
    resultsMultiple,
    outputPaths = outputPathsFilter,
    parameterPaths = parameterPathsFilter,
    pkParameters = pkParametersFilter
  )

  withr::local_seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "filtered spider",
      fig = plotFiltered
    )
  )
})
