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
variationRange <- c(0.1, 2, 10) # 1.0 is deliberately left out for testing

set.seed(123)
results <- sensitivityCalculation(
  simulation = simulation,
  outputPaths = outputPaths,
  parameterPaths = parameterPaths,
  variationRange = variationRange
)

# validating plotting arguments -------------------------

test_that("sensitivityTornadoPlot fails with incorrect input objects", {
  expect_error(
    sensitivityTornadoPlot("x"),
    "argument 'sensitivityCalculation' is of type 'character', but expected 'SensitivityCalculation'"
  )
})

test_that("sensitivityTornadoPlot fails with incorrect parameterFactor input", {
  expect_error(
    sensitivityTornadoPlot(results, parameterFactor = 0),
    "parameterFactor error"
  )
})

test_that("sensitivityTornadoPlot errors if parameterFactor is missing in sensitivityCalculation", {
  expect_error(
    sensitivityTornadoPlot(results, parameterFactor = 0.2),
    "values of 0.2 and 5 are not included in the sensitivity analysis results"
  )
})

# default plots ---------------------------------------

test_that("sensitivityTornadoPlot default plots are as expected", {
  set.seed(123)
  p <- sensitivityTornadoPlot(results)

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "sensitivityTornadoPlot works as expected",
    fig = suppressWarnings(p)
  )
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

resultsMultiple <- sensitivityCalculation(
  simulation = simulation,
  outputPaths = outputPaths,
  parameterPaths = parameterPaths,
  variationRange = c(0.1, 10)
)

test_that("sensitivityTornadoPlot plots are as expected for multiple output paths", {
  set.seed(123)
  plotsMultiple <- sensitivityTornadoPlot(resultsMultiple)

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple output path tornado",
    fig = suppressWarnings(plotsMultiple)
  )
})

# filter data to be plotted -------------------------------------

outputPathsFilter <- "Organism|ArterialBlood|Plasma|Aciclovir"
parameterPathsFilter <- "Aciclovir|Lipophilicity"
pkParametersFilter <- c("AUC_inf", "C_max")

test_that("sensitivityTornadoPlot plots are as expected with filters", {
  set.seed(123)
  tornadoPlotFiltered <- sensitivityTornadoPlot(
    resultsMultiple,
    outputPaths = outputPathsFilter,
    parameterPaths = parameterPathsFilter,
    pkParameters = pkParametersFilter
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "filtered tornado",
    fig = suppressWarnings(tornadoPlotFiltered)
  )
})

# restore old options
options(old)
