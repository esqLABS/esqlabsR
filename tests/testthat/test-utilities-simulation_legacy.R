# ## context("initializeSimulation")
#
# test_that("`initializeSimulation()` loads a simulation at the minimum", {
#   simulation <- ospsuite::loadSimulation(system.file("extdata", "simple.pkml", package = "ospsuite"))
#   initializeSimulation(simulation)
#   simulationResults <- runSimulations(simulation)
#   expect_true(isOfType(simulationResults, "SimulationResults"))
# })
#
# test_that("`initializeSimulation()` does not fail when additionalParams is empty", {
#   simulation <- loadSimulation(system.file("extdata", "simple.pkml", package = "ospsuite"))
#
#   dataFolder <- getTestDataFilePath("")
#   paramsXLSpath <- file.path(dataFolder, "Parameters.xlsx")
#   sheets <- c("EmptySheet")
#   params <- readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheets)
#
#   initializeSimulation(simulation, additionalParams = params)
#   simulationResults <- runSimulations(simulation)
#   expect_true(isOfType(simulationResults, "SimulationResults"))
# })
#
# ## context("compareSimulationParameters")
#
# test_that("`compareSimulations()` produces no differences with identical simulations", {
#   simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#   sim1 <- loadSimulation(simPath)
#   sim2 <- loadSimulation(simPath)
#
#   res <- compareSimulations(sim1, sim2)
#   enmptyNamedList <- list()
#   names(enmptyNamedList) <- vector()
#   expect_equal(
#     res,
#     list(Parameters = list(In1NotIn2 = NULL, In2NotIn1 = NULL, Different = enmptyNamedList))
#   )
# })
#
# test_that("`compareSimulations()` lists differencies on parameter correctly", {
#   sim1 <- loadSimulation(testthat::test_path("../data/simple.pkml"))
#   sim2 <- loadSimulation(testthat::test_path("../data/simple2.pkml"))
#
#   res <- compareSimulations(sim1, sim2)
#   in1notIn2Paths <- c("Organism|RHSParameter")
#   in2notIn1Paths <- c("Organism|in2NotIn1")
#   differentPaths <- c("Organism|Q")
#
#   expect_equal(res$Parameters$In1NotIn2[[1]]$path, getAllParametersMatching(in1notIn2Paths, sim1)[[1]]$path)
#   expect_equal(res$Parameters$In2NotIn1[[1]]$path, getAllParametersMatching(in2notIn1Paths, sim2)[[1]]$path)
#
#   expect_equal(res$Parameters$Different[[1]]$simulation1$value, getAllParametersMatching(differentPaths, sim1)[[1]]$value)
#   expect_equal(res$Parameters$Different[[1]]$simulation2$value, getAllParametersMatching(differentPaths, sim2)[[1]]$value)
# })
# # getAllApplicationParameters
#
# simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
# simulation <- loadSimulation(simPath)
#
# test_that("It returns application parameters when no molecules are defined", {
#   applicationParams <- getAllApplicationParameters(simulation = simulation)
#
#   expect_length(applicationParams, 5)
# })
#
# test_that("It returns application parameters when a molecule are defined", {
#   molecule <- "Aciclovir"
#   applicationParams <- getAllApplicationParameters(
#     simulation = simulation,
#     moleculeNames = molecule
#   )
#
#   expect_length(applicationParams, 5)
# })
#
# test_that("It returns an empty list when a molecule is defined that is not in the model", {
#   molecule <- "Foo"
#   applicationParams <- getAllApplicationParameters(
#     simulation = simulation,
#     moleculeNames = molecule
#   )
#
#   expect_equal(applicationParams, list())
# })
