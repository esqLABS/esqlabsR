# save old options
old <- options()

options(
  tibble.width = Inf,
  pillar.min_title_chars = Inf,
  pillar.sigfig = 4,
  digits = 4,
  scipen = 999
)

# single output path -------------------------------------

# run time-consuming simulations just once
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

# validating plotting arguments -------------------------

test_that("sensitivitySpiderPlot fails with incorrect input objects", {
  expect_error(
    sensitivitySpiderPlot("x"),
    "argument 'sensitivityCalculation' is of type 'character', but expected 'SensitivityCalculation'"
  )
})

# default plots ---------------------------------------

test_that("sensitivitySpiderPlot default plots are as expected", {
  set.seed(123)
  p <- sensitivitySpiderPlot(results)

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "sensitivitySpiderPlot works as expected",
    fig = suppressWarnings(p)
  )
})

# parameterized plots ---------------------------------

n <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

test_that("sensitivitySpiderPlot correctly applies free y-axis facets scaling", {
  set.seed(123)
  p <- sensitivitySpiderPlot(results, yAxisFacetScales = "free")
  pbs <- purrr::map(seq_along(p[[n]]), ~ ggplot2::ggplot_build(p[[n]][[.x]]))
  plotParams <- purrr::map(pbs, ~ .x$layout$panel_params[[1]]$y.range)

  expect_snapshot(unlist(plotParams))
})

test_that("sensitivitySpiderPlot correctly applies absolute y-axis values", {
  set.seed(123)
  p <- sensitivitySpiderPlot(results, yAxisType = "absolute")

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "sensitivitySpiderPlot works as expected with absolute y-values",
    fig = suppressWarnings(p)
  )

  pbs <- purrr::map(seq_along(p[[n]]), ~ ggplot2::ggplot_build(p[[n]][[.x]]))
  plotParams <- purrr::map(pbs, ~ .x$layout$panel_params[[1]]$y.range)

  expect_snapshot(unlist(plotParams))
})

test_that("sensitivitySpiderPlot correctly applies free scaling with absolute y-values", {
  set.seed(123)
  p <- sensitivitySpiderPlot(results, yAxisType = "absolute", yAxisFacetScales = "free")
  pbs <- purrr::map(seq_along(p[[n]]), ~ ggplot2::ggplot_build(p[[n]][[.x]]))
  plotParams <- list(
    unlist(plotParams <- purrr::map(pbs, ~ .x$layout$panel_params[[1]]$y.range)),
    unlist(plotParams <- purrr::map(pbs, ~ .x$plot$labels$y))
  )

  expect_snapshot(unlist(plotParams))
})

# multiple output paths -------------------------------------

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

results_multiple <- sensitivityCalculation(
  simulation = simulation,
  outputPaths = outputPaths,
  parameterPaths = parameterPaths,
  variationRange = c(1, 5, 10)
)

test_that("sensitivitySpiderPlot plots are as expected for multiple output paths", {
  set.seed(123)
  plots_multiple <- sensitivitySpiderPlot(results_multiple)

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple output path spiders",
    fig = suppressWarnings(plots_multiple)
  )
})

# filter data to be plotted -------------------------------------

outputPathsFilter <- "Organism|ArterialBlood|Plasma|Aciclovir"
parameterPathsFilter <- "Aciclovir|Lipophilicity"
pkParametersFilter <- c("C_max", "t_max")

test_that("sensitivitySpiderPlot plots are as expected with filters", {
  set.seed(123)
  spider_plot_filtered <- sensitivitySpiderPlot(
    results_multiple,
    outputPaths = outputPathsFilter,
    parameterPaths = parameterPathsFilter,
    pkParameters = pkParametersFilter
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "filtered spider",
    fig = suppressWarnings(spider_plot_filtered)
  )
})

# restore old options
options(old)
