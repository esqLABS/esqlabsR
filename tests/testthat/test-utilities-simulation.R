test_that("`initializeSimulation()` loads a simulation at the minimum", {
  simulation <- loadSimulation(system.file(
    "extdata",
    "simple.pkml",
    package = "ospsuite"
  ))
  initializeSimulation(simulation)
  simulationResults <- runSimulations(simulation)
  expect_true(isOfType(simulationResults, "SimulationResults"))
})

test_that("`initializeSimulation()` does not fail when additionalParams is empty", {
  simulation <- loadSimulation(system.file(
    "extdata",
    "simple.pkml",
    package = "ospsuite"
  ))

  dataFolder <- getTestDataFilePath("")
  paramsXLSpath <- file.path(dataFolder, "Parameters.xlsx")
  sheets <- c("EmptySheet")
  params <- readParametersFromXLS(
    paramsXLSpath = paramsXLSpath,
    sheets = sheets
  )

  initializeSimulation(simulation, additionalParams = params)
  simulationResults <- runSimulations(simulation)
  expect_true(isOfType(simulationResults, "SimulationResults"))
})

test_that("`initializeSimulation()` applies additional initial conditions", {
  simulation <- loadSimulation(system.file(
    "extdata",
    "simple.pkml",
    package = "ospsuite"
  ))

  moleculePath <- "Organism|Liver|A"
  initialConditions <- list(
    paths = moleculePath,
    values = 5,
    units = "µmol"
  )

  initializeSimulation(
    simulation,
    additionalInitialConditions = initialConditions
  )

  molecule <- ospsuite::getAllMoleculesMatching(moleculePath, simulation)[[1]]
  expect_equal(molecule$value, 5)
})

test_that("`initializeSimulation()` does not fail when additionalInitialConditions is empty", {
  simulation <- loadSimulation(system.file(
    "extdata",
    "simple.pkml",
    package = "ospsuite"
  ))

  emptyInitialConditions <- list(
    paths = character(0),
    values = numeric(0),
    units = character(0)
  )

  initializeSimulation(
    simulation,
    additionalInitialConditions = emptyInitialConditions
  )
  simulationResults <- runSimulations(simulation)
  expect_true(isOfType(simulationResults, "SimulationResults"))
})

test_that("`initializeSimulation()` errors on malformed additionalInitialConditions", {
  simulation <- loadSimulation(system.file(
    "extdata",
    "simple.pkml",
    package = "ospsuite"
  ))

  expect_error(
    initializeSimulation(
      simulation,
      # missing the required 'units' element
      additionalInitialConditions = list(
        paths = "Organism|Liver|A",
        values = 5
      )
    ),
    regexp = messages$wrongParametersStructure("additionalInitialConditions"),
    fixed = TRUE
  )
})

test_that("`initializeSimulation()` honours stopIfParameterNotFound for initial conditions", {
  badInitialConditions <- list(
    paths = "Organism|NonExistentContainer|X",
    values = 5,
    units = "µmol"
  )

  # With stopIfParameterNotFound = FALSE a missing molecule path is tolerated
  simulation <- loadSimulation(system.file(
    "extdata",
    "simple.pkml",
    package = "ospsuite"
  ))
  expect_no_error(
    suppressWarnings(initializeSimulation(
      simulation,
      additionalInitialConditions = badInitialConditions,
      stopIfParameterNotFound = FALSE
    ))
  )

  # With the default (TRUE) the missing path raises an error
  simulation <- loadSimulation(system.file(
    "extdata",
    "simple.pkml",
    package = "ospsuite"
  ))
  expect_error(
    initializeSimulation(
      simulation,
      additionalInitialConditions = badInitialConditions,
      stopIfParameterNotFound = TRUE
    ),
    regexp = "no entity exists for path \"Organism|NonExistentContainer|X\"",
    fixed = TRUE
  )
})


test_that("`compareSimulations()` produces no differences with identical simulations", {
  simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
  sim1 <- loadSimulation(simPath)
  sim2 <- loadSimulation(simPath)

  res <- compareSimulations(sim1, sim2)
  enmptyNamedList <- list()
  names(enmptyNamedList) <- vector()
  expect_equal(
    res,
    list(
      Parameters = list(
        In1NotIn2 = NULL,
        In2NotIn1 = NULL,
        Different = enmptyNamedList
      )
    )
  )
})

test_that("`compareSimulations()` lists differencies on parameter correctly", {
  sim1 <- loadSimulation(testthat::test_path("../data/simple.pkml"))
  sim2 <- loadSimulation(testthat::test_path("../data/simple2.pkml"))

  res <- compareSimulations(sim1, sim2)
  in1notIn2Paths <- c("Organism|RHSParameter")
  in2notIn1Paths <- c("Organism|in2NotIn1")
  differentPaths <- c("Organism|Q")

  expect_equal(
    res$Parameters$In1NotIn2[[1]]$path,
    getAllParametersMatching(in1notIn2Paths, sim1)[[1]]$path
  )
  expect_equal(
    res$Parameters$In2NotIn1[[1]]$path,
    getAllParametersMatching(in2notIn1Paths, sim2)[[1]]$path
  )

  expect_equal(
    res$Parameters$Different[[1]]$simulation1$value,
    getAllParametersMatching(differentPaths, sim1)[[1]]$value
  )
  expect_equal(
    res$Parameters$Different[[1]]$simulation2$value,
    getAllParametersMatching(differentPaths, sim2)[[1]]$value
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
