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

test_that("`initializeSimulation()` applies additionalInitialConditions", {
  simulation <- loadSimulation(system.file(
    "extdata",
    "simple.pkml",
    package = "ospsuite"
  ))
  initialConditions <- list(
    paths = "Organism|A",
    values = 42,
    units = "µmol"
  )
  initializeSimulation(
    simulation,
    additionalInitialConditions = initialConditions
  )
  applied <- ospsuite::getQuantityValuesByPath(
    quantityPaths = "Organism|A",
    simulation = simulation,
    units = "µmol"
  )
  expect_equal(applied, 42)
})

test_that("`initializeSimulation()` does not fail when additionalInitialConditions is empty", {
  simulation <- loadSimulation(system.file(
    "extdata",
    "simple.pkml",
    package = "ospsuite"
  ))
  emptyIC <- list(paths = NULL, values = NULL, units = NULL)
  initializeSimulation(simulation, additionalInitialConditions = emptyIC)
  simulationResults <- runSimulations(simulation)
  expect_true(isOfType(simulationResults, "SimulationResults"))
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
  sim1 <- loadSimulation(getTestDataFilePath("simple.pkml"))
  sim2 <- loadSimulation(getTestDataFilePath("simple2.pkml"))

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

test_that("It returns application parameters when no molecules are defined", {
  simulation <- loadSimulation(simPath)
  applicationParams <- getAllApplicationParameters(simulation = simulation)

  expect_length(applicationParams, 5)
})

test_that("It returns application parameters when a molecule are defined", {
  simulation <- loadSimulation(simPath)
  molecule <- "Aciclovir"
  applicationParams <- getAllApplicationParameters(
    simulation = simulation,
    moleculeNames = molecule
  )

  expect_length(applicationParams, 5)
})

test_that("It returns an empty list when a molecule is defined that is not in the model", {
  simulation <- loadSimulation(simPath)
  molecule <- "Foo"
  applicationParams <- getAllApplicationParameters(
    simulation = simulation,
    moleculeNames = molecule
  )

  expect_equal(applicationParams, list())
})

# Species scaling ----

# Human model of the test project, re-exported with PK-Sim 13
.loadHumanAciclovirSimulation <- function() {
  loadSimulation(
    testthat::test_path(
      "data",
      "TestProject",
      "Models",
      "Simulations",
      "Aciclovir.pkml"
    ),
    loadFromCache = FALSE
  )
}

test_that("`applyIndividualParameters()` scales a human model to a rat", {
  simulation <- .loadHumanAciclovirSimulation()
  ratCharacteristics <- createIndividualCharacteristics(species = Species$Rat)

  expect_no_warning(applyIndividualParameters(ratCharacteristics, simulation))

  # Expected values are those of a rat individual created in PK-Sim 13. They
  # cover species constants that `createIndividual()` does not return, the
  # colon bile salt concentration that is new in PK-Sim 13, lumen geometry
  # that is a formula in the human model, and the derived body weight.
  expectedValues <- c(
    "Organism|Liver|EHC continuous fraction" = 1,
    "Organism|Lumen|Stomach|Basal pH in fasted state" = 3.9,
    "Organism|Liver|Vf (neutral lipid)-PT" = 0.0138,
    "Organism|Kidney|Fraction vascular" = 0.105,
    "Organism|Bone|Allometric scale factor" = 0.75,
    "Organism|Muscle|Vf (water)-PT" = 0.756,
    "Organism|Lumen|Duodenum|Length" = 1,
    "Organism|Lumen|ColonAscendens|Bile Salt concentration" = 5000,
    "Organism|Liver|Volume" = 0.0103,
    "Organism|Weight" = 0.227777
  )
  values <- vapply(
    names(expectedValues),
    function(path) getParameter(path, simulation)$value,
    FUN.VALUE = numeric(1)
  )
  expect_equal(values, expectedValues, tolerance = 1e-6)
})

test_that("`applyIndividualParameters()` follows the body weight of a rat", {
  simulation <- .loadHumanAciclovirSimulation()
  ratCharacteristics <- createIndividualCharacteristics(
    species = Species$Rat,
    weight = 0.4
  )

  applyIndividualParameters(ratCharacteristics, simulation)

  # Organ volumes of a 0.4 kg rat created in PK-Sim 13; species constants do
  # not depend on the weight.
  expectedValues <- c(
    "Organism|Weight" = 0.4,
    "Organism|Liver|Volume" = 0.0180878666414959,
    "Organism|Kidney|Volume" = 0.0040390381820816,
    "Organism|Liver|Vf (neutral lipid)-PT" = 0.0138
  )
  values <- vapply(
    names(expectedValues),
    function(path) getParameter(path, simulation)$value,
    FUN.VALUE = numeric(1)
  )
  expect_equal(values, expectedValues, tolerance = 1e-6)
})

test_that("`applyIndividualParameters()` leaves the species constants of a human model", {
  simulation <- .loadHumanAciclovirSimulation()
  humanCharacteristics <- createIndividualCharacteristics(
    species = Species$Human,
    population = HumanPopulation$European_ICRP_2002,
    gender = Gender$Male,
    weight = 73,
    height = 176,
    age = 30
  )

  applyIndividualParameters(humanCharacteristics, simulation)

  expect_equal(
    getParameter("Organism|Liver|Vf (neutral lipid)-PT", simulation)$value,
    0.0348
  )
  expect_equal(
    getParameter("Organism|Liver|EHC continuous fraction", simulation)$value,
    0
  )
  # Lumen geometry stays a formula of the body height
  expect_true(getParameter("Organism|Lumen|Duodenum|Length", simulation)$isFormula)
})

test_that("`initializeSimulation()` scales a human model to a mouse and runs it", {
  simulation <- .loadHumanAciclovirSimulation()
  mouseCharacteristics <- createIndividualCharacteristics(species = Species$Mouse)

  expect_no_warning(
    initializeSimulation(
      simulation,
      individualCharacteristics = mouseCharacteristics
    )
  )

  # Values of a mouse individual created in PK-Sim 13
  expect_equal(
    getParameter("Organism|Lumen|Stomach|Basal pH in fasted state", simulation)$value,
    4.04
  )
  expect_equal(
    getParameter("Organism|Liver|Specific blood flow rate", simulation)$value,
    0.269230769230769,
    tolerance = 1e-6
  )
  simulationResults <- runSimulations(simulation)[[1]]
  expect_true(isOfType(simulationResults, "SimulationResults"))
})

test_that("`applyIndividualParameters()` scales between two non-human species", {
  # A model already scaled to a rat and then scaled to a mouse must equal the
  # human model scaled to a mouse directly.
  simulationViaRat <- .loadHumanAciclovirSimulation()
  applyIndividualParameters(
    createIndividualCharacteristics(species = Species$Rat),
    simulationViaRat
  )
  applyIndividualParameters(
    createIndividualCharacteristics(species = Species$Mouse),
    simulationViaRat
  )
  simulationDirect <- .loadHumanAciclovirSimulation()
  applyIndividualParameters(
    createIndividualCharacteristics(species = Species$Mouse),
    simulationDirect
  )

  organismValues <- function(simulation) {
    paths <- getAllParameterPathsIn(simulation)
    parameters <- getAllParametersMatching(
      paths[startsWith(paths, "Organism|")],
      simulation
    )
    values <- vapply(parameters, function(parameter) parameter$value, numeric(1))
    names(values) <- vapply(parameters, function(parameter) parameter$path, character(1))
    values[order(names(values))]
  }
  expect_equal(organismValues(simulationViaRat), organismValues(simulationDirect))
})

# ospsuite's test snapshot holds a mouse project. Loading it takes about ten
# seconds, so the simulation is loaded once for this file.
.mouseSimulation <- local({
  simulation <- NULL
  function() {
    if (is.null(simulation)) {
      simulation <<- ospsuite::loadSimulationsFromSnapshot(
        system.file("extdata", "test_snapshot.json", package = "ospsuite")
      )[[1]]
    }
    simulation
  }
})

.humanCharacteristics <- function() {
  createIndividualCharacteristics(
    species = Species$Human,
    population = HumanPopulation$European_ICRP_2002,
    gender = Gender$Male,
    weight = 73,
    height = 176,
    age = 30
  )
}

test_that("`applyIndividualParameters()` refuses a human individual on a non-human model", {
  mouseSimulation <- .mouseSimulation()
  weightBefore <- getParameter("Organism|Weight", mouseSimulation)$value

  expect_error(
    applyIndividualParameters(.humanCharacteristics(), mouseSimulation),
    "Mouse"
  )
  # The model is untouched
  expect_equal(getParameter("Organism|Weight", mouseSimulation)$value, weightBefore)

  # Another non-human species is still applied
  expect_no_error(
    applyIndividualParameters(
      createIndividualCharacteristics(species = Species$Rat),
      mouseSimulation
    )
  )
  expect_equal(
    getParameter("Organism|Lumen|Stomach|Basal pH in fasted state", mouseSimulation)$value,
    3.9
  )
})

test_that("`.simulationSpecies()` reads the stored individual, then the parameters", {
  humanSimulation <- .loadHumanAciclovirSimulation()
  expect_identical(.simulationSpecies(humanSimulation), Species$Human)
  expect_identical(.simulationSpecies(.mouseSimulation()), Species$Mouse)

  # A MoBi model without a PK-Sim organism cannot be told
  mobiSimulation <- loadSimulation(
    system.file("extdata", "simple.pkml", package = "ospsuite"),
    loadFromCache = FALSE
  )
  expect_null(.simulationSpecies(mobiSimulation))

  # Without a stored individual, as in exports older than OSP version 12, the
  # human-only parameters decide
  local_mocked_bindings(.storedIndividualSpecies = function(simulation) NULL)
  expect_identical(.simulationSpecies(humanSimulation), Species$Human)
  expect_identical(.simulationSpecies(.mouseSimulation()), NA_character_)
  expect_error(
    applyIndividualParameters(.humanCharacteristics(), .mouseSimulation()),
    "not built for a human individual"
  )
  expect_no_error(applyIndividualParameters(.humanCharacteristics(), humanSimulation))
})
