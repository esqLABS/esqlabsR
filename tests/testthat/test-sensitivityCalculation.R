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
  df_list <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths
  )

  expect_equal(
    names(df_list$tsData),
    c("OutputPath", "ParameterFactor", "ParameterPath", "ParameterValue",
      "Time", "Concentration", "Unit", "Dimension", "TimeUnit", "TimeDimension",
      "molWeight")
  )

  set.seed(123)
  expect_snapshot(str(df_list$tsData))

  set.seed(123)
  expect_snapshot(summary(df_list$tsData))

  expect_equal(
    names(df_list$pkData),
    c("OutputPath", "ParameterPath", "ParameterFactor", "ParameterValue",
      "PKParameter", "PKParameterValue", "Unit", "PercentChangePK",
      "SensitivityPKParameter")
  )

  set.seed(123)
  expect_snapshot(str(df_list$pkData))

  set.seed(123)
  expect_snapshot(summary(df_list$pkData))

  # TODO: turn on after plots are finalized
  # set.seed(123)
  # vdiffr::expect_doppelganger(
  #   title = "sensitivityTimeProfiles works as expected",
  #   fig = sensitivityTimeProfiles(df_list$tsData)
  # )
  #
  # set.seed(123)
  # vdiffr::expect_doppelganger(
  #   title = "sensitivitySpiderPlot works as expected",
  #   fig = sensitivitySpiderPlot(df_list$pkData)
  # )
})
