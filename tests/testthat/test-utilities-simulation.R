## context("initializeSimulation")

test_that("`initializeSimulation()` loads a simulation at the minimum", {
  simulation <- loadSimulation(system.file("extdata", "simple.pkml", package = "ospsuite"))
  initializeSimulation(simulation, steadyStateTime = TRUE)
  simulationResults <- runSimulation(simulation)
  expect_s3_class(simulationResults, "SimulationResults")
})
