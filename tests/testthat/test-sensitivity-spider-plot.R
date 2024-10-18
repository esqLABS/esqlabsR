# Save old options
old_opts <- options()

options(
  tibble.width = Inf,
  pillar.min_title_chars = Inf,
  pillar.sigfig = 4,
  digits = 4,
  scipen = 999
)

# Single output path ------------------------------------------------------

simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
simulation <- loadSimulation(simPath)
outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
parameterPaths <- c(
  "Aciclovir|Lipophilicity",
  "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
  "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR fraction"
)
variationRange <- c(0.1, 2, 20) # 1.0 is deliberately left out for testing

set.seed(123)
results <- sensitivityCalculation(
  simulation = simulation,
  outputPaths = outputPaths,
  parameterPaths = parameterPaths,
  variationRange = variationRange
)

# Validate plotting arguments ---------------------------------------------

test_that("sensitivitySpiderPlot fails with invalid input", {
  expect_error(
    sensitivitySpiderPlot("x"),
    "argument 'sensitivityCalculation' is of type 'character', but expected 'SensitivityCalculation'"
  )
})

# Default plot ------------------------------------------------------------

test_that("sensitivitySpiderPlot creates expected default plot", {
  set.seed(123)
  p <- sensitivitySpiderPlot(results)

  set.seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "sensitivitySpiderPlot works as expected",
      fig = p
    )
  )
})

# Default plot with custom PK parameter -----------------------------------

test_that("sensitivitySpiderPlot handles custom PK parameters", {
  customFun <- list("minmax" = function(y) max(y) / min(y[y != 0]))

  resultsCustomPK <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    customOutputFunctions = customFun,
    variationRange = variationRange
  )

  set.seed(123)
  p <- sensitivitySpiderPlot(resultsCustomPK)

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "sensitivitySpiderPlot custom PK Parameter",
    fig = suppressWarnings(p)
  )
})

# Parameterized plots -----------------------------------------------------

n <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

test_that("sensitivitySpiderPlot applies free y-axis scaling correctly", {
  set.seed(123)
  p <- sensitivitySpiderPlot(results, yAxisFacetScales = "free")
  pbs <- purrr::map(seq_along(p[[n]]), ~ ggplot2::ggplot_build(p[[n]][[.x]]))
  plotParams <- purrr::map(pbs, ~ .x$layout$panel_params[[1]]$y.range)

  expect_snapshot(unlist(plotParams))
})

test_that("sensitivitySpiderPlot correctly applies absolute y-axis values correctly", {
  set.seed(123)
  p <- sensitivitySpiderPlot(results, yAxisType = "absolute")

  set.seed(123)
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
  set.seed(123)
  p <- sensitivitySpiderPlot(results,
    xAxisType = "absolute",
    # select parameter paths with non-negative values
    parameterPaths = parameterPaths[2:3]
  )

  set.seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "sensitivitySpiderPlot works as expected with absolute x-values",
      fig = p
    )
  )
})

test_that("sensitivitySpiderPlot applies absolute x- and y-axis values correctly", {
  set.seed(123)
  p1 <- sensitivitySpiderPlot(results,
    xAxisType = "absolute", yAxisType = "absolute",
    xAxisScale = "log", yAxisScale = "lin"
  ) # default scales
  p2 <- sensitivitySpiderPlot(results,
    xAxisType = "absolute", yAxisType = "absolute",
    xAxisScale = "lin", yAxisScale = "log"
  )

  expect_snapshot(extractAxisRange(p1))
  expect_snapshot(extractAxisRange(p2))
})

test_that("sensitivitySpiderPlot applies free scaling with absolute y-values", {
  set.seed(123)
  p <- sensitivitySpiderPlot(results, yAxisType = "absolute", yAxisFacetScales = "free")
  pbs <- purrr::map(seq_along(p[[n]]), ~ ggplot2::ggplot_build(p[[n]][[.x]]))
  plotParams <- list(
    unlist(plotParams <- purrr::map(pbs, ~ .x$layout$panel_params[[1]]$y.range)),
    unlist(plotParams <- purrr::map(pbs, ~ .x$plot$labels$y))
  )

  expect_snapshot(unlist(plotParams))
})

# Plot configuration ------------------------------------------------------

n <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

test_that("sensitivitySpiderPlot uses defaultPlotConfiguration scales", {
  myPlotConfiguration <- createEsqlabsPlotConfiguration()
  myPlotConfiguration$xAxisScale <- "lin"
  myPlotConfiguration$yAxisScale <- "log"

  p <- sensitivitySpiderPlot(results, defaultPlotConfiguration = myPlotConfiguration)
  pb <- ggplot2::ggplot_build(p[[n]][[1]])

  expect_equal(pb$layout$panel_scales_x[[1]]$trans$name, "identity")
  expect_equal(pb$layout$panel_scales_y[[1]]$trans$name, "log-10")
})

test_that("sensitivitySpiderPlot signature overrides defaultPlotConfiguration", {
  myPlotConfiguration <- createEsqlabsPlotConfiguration()
  myPlotConfiguration$xAxisScale <- "lin" # to be overridden
  myPlotConfiguration$yAxisScale <- "log" # to be overridden

  p <- sensitivitySpiderPlot(results,
    defaultPlotConfiguration = myPlotConfiguration,
    xAxisScale = "log", yAxisScale = "lin"
  )
  pb <- ggplot2::ggplot_build(p[[n]][[1]])

  expect_equal(pb$layout$panel_scales_x[[1]]$trans$name, "log-10")
  expect_equal(pb$layout$panel_scales_y[[1]]$trans$name, "identity")
})

# Multiple output paths ---------------------------------------------------

simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
simulation <- loadSimulation(simPath)
outputPaths <- c(
  "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  "Organism|Age",
  "Organism|ArterialBlood|Plasma|Aciclovir"
)
parameterPaths <- c(
  "Aciclovir|Lipophilicity",
  "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
  "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR fraction"
)

resultsMultiple <- sensitivityCalculation(
  simulation = simulation,
  outputPaths = outputPaths,
  parameterPaths = parameterPaths,
  variationRange = c(1, 5, 10)
)

test_that("sensitivitySpiderPlot handles multiple output paths correctly", {
  set.seed(123)
  plotsMultiple <- sensitivitySpiderPlot(resultsMultiple)

  set.seed(123)
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
  set.seed(123)
  plotFiltered <- sensitivitySpiderPlot(
    resultsMultiple,
    outputPaths = outputPathsFilter,
    parameterPaths = parameterPathsFilter,
    pkParameters = pkParametersFilter
  )

  set.seed(123)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "filtered spider",
      fig = plotFiltered
    )
  )
})

# Restore old options
on.exit(options(old_opts), add = TRUE)
