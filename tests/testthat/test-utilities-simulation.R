## context("initializeSimulation")

test_that("`initializeSimulation()` loads a simulation at the minimum", {
  simulation <- loadSimulation(system.file("extdata", "simple.pkml", package = "ospsuite"))
  initializeSimulation(simulation)
  simulationResults <- runSimulation(simulation)
  expect_true(isOfType(simulationResults, "SimulationResults"))
})

test_that("`initializeSimulation()` does not fail when additionalParams is empty", {
  simulation <- loadSimulation(system.file("extdata", "simple.pkml", package = "ospsuite"))

  dataFolder <- getTestDataFilePath("")
  paramsXLSpath <- file.path(dataFolder, "Parameters.xlsx")
  sheets <- c("EmptySheet")
  params <- readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheets)

  initializeSimulation(simulation, additionalParams = params)
  simulationResults <- runSimulation(simulation)
  expect_true(isOfType(simulationResults, "SimulationResults"))
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
# getAllApplicationParameters

simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
simulation <- loadSimulation(simPath)

test_that("It returns application parameters when no molecules are defined", {
  applicationParams <- getAllApplicationParameters(simulation = simulation)

  expect_length(applicationParams, 5)
})

test_that("It returns application parameters when a molecule are defined", {
  molecule <- "Aciclovir"
  applicationParams <- getAllApplicationParameters(
    simulation = simulation,
    moleculeNames = molecule
  )

  expect_length(applicationParams, 5)
})

test_that("It returns an empty list when a molecule is defined that is not in the model", {
  molecule <- "Foo"
  applicationParams <- getAllApplicationParameters(
    simulation = simulation,
    moleculeNames = molecule
  )

  expect_equal(applicationParams, list())
})
