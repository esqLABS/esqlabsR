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
  expect_error(
    sensitivityTornadoPlot(results, parameterFactor = 0),
    "parameterFactor error"
  )
})

test_that("sensitivityTornadoPlot fails with invalid xAxisZoomRange", {
  xAxisZoomRange <- 100
  expect_error(
    sensitivityTornadoPlot(results, xAxisZoomRange = xAxisZoomRange),
    messages$errorWrongLength(xAxisZoomRange, 2),
    fixed = TRUE
  )
})

test_that("sensitivityTornadoPlot errors if parameterFactor is missing in sensitivity calculation results", {
  expect_error(
    sensitivityTornadoPlot(results, parameterFactor = 0.2),
    glue::glue(
      "'parameterFactor' values of 0.2 and 5 are not included in the \\
      sensitivity analysis results. Current values: 0.1, 1, 2, 10. \\
      Please rerun the sensitivity analysis with the required values."
    )
  )
})

# Default plot ------------------------------------------------------------

test_that("sensitivityTornadoPlot creates default plot", {
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
  names(parameterPaths) <- c("Lipophilicity", "Dose", "GFR fraction")

  resultsLab <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
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
    names(parameterPaths),
    pb$layout$panel_params[[1]]$y$get_labels()
  )
})

# Default plot with custom PK parameter -----------------------------------

test_that("sensitivityTornadoPlot works with custom PK parameter", {
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

test_that("sensitivityTornadoPlot handles multiple output paths", {
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

# Restore old options
on.exit(options(old_opts), add = TRUE)
