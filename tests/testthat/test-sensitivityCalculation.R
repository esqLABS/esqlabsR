test_that("Check sensitivityCalculation dataframes and plots are as expected", {

  # the dataframes and plots are checked in the same test to reduce test time
  # otherwise time-consuming simulations will need to be run each time

  simPath <- system.file("extdata", "Aciclovir.pkml", package = "esqlabsR")
  simulation <- loadSimulation(simPath)
  outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  parameterPaths <- c(
    "Aciclovir|Lipophilicity",
    "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
    "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR fraction"
  )

  set.seed(123)
  results <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths
  )

  expect_equal(
    names(results$tsData),
    c(
      "OutputPath", "ParameterFactor", "ParameterPath", "ParameterValue",
      "Time", "Concentration", "Unit", "Dimension", "TimeUnit", "TimeDimension",
      "molWeight"
    )
  )

  set.seed(123)
  expect_snapshot(str(results$tsData))

  set.seed(123)
  expect_snapshot(summary(results$tsData))

  expect_equal(
    names(results$pkData),
    c(
      "OutputPath", "ParameterPath", "ParameterFactor", "ParameterValue",
      "PKParameter", "PKParameterValue", "Unit", "PercentChangePK",
      "SensitivityPKParameter"
    )
  )

  set.seed(123)
  expect_snapshot(str(results$pkData))

  set.seed(123)
  expect_snapshot(summary(results$pkData))

  expect_equal(
    unique(results$tsData$OutputPath),
    unique(results$pkData$OutputPath)
  )

  expect_equal(
    unique(results$tsData$OutputPath),
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  )

  expect_equal(
    unique(results$tsData$ParameterPath),
    unique(results$pkData$ParameterPath)
  )

  expect_snapshot(unique(results$tsData$ParameterPath))

  # plotting fails with incorrect input objects

  expect_error(
    sensitivityTimeProfiles("x"),
    "argument 'sensitivityAnalysis' is of type 'character', but expected 'SensitivityAnalysis'"
  )

  expect_error(
    sensitivitySpiderPlot("x"),
    "argument 'sensitivityAnalysis' is of type 'character', but expected 'SensitivityAnalysis'"
  )


  # visual regression tests -------------

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "sensitivityTimeProfiles works as expected",
    fig = suppressWarnings(sensitivityTimeProfiles(results))
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "sensitivitySpiderPlot works as expected",
    fig = sensitivitySpiderPlot(results)
  )
})
