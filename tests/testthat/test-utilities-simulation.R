## context("initializeSimulation")

test_that("`initializeSimulation()` loads a simulation at the minimum", {
  simulation <- loadSimulation(system.file("extdata", "simple.pkml", package = "ospsuite"))
  initializeSimulation(simulation, steadyStateTime = TRUE)
  simulationResults <- runSimulation(simulation)
  expect_s3_class(simulationResults, "SimulationResults")
})

## context("compareSimulationParameters")

test_that("`compareSimulationParameters()` produces no differences with identical simulations", {
  simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
  sim1 <- loadSimulation(simPath)
  sim2 <- loadSimulation(simPath)

  res <- compareSimulationParameters(sim1, sim2)
  expect_equal(
    res,
    list(In1NotIn2 = list(), In2NotIn1 = list(), Different = list())
  )
})
