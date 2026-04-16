defaultOutputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

test_that("runScenarios stops with an error if a parameter is not present", {
  pc <- testProject()
  expect_error(runScenarios(pc, scenarioNames = "TestScenario_missingParam"))
})

test_that("It runs one scenario without specifying output paths", {
  pc <- testProject()

  simulatedScenarios <- runScenarios(
    project = pc,
    scenarioNames = "TestScenario"
  )

  expect_equal(names(simulatedScenarios), "TestScenario")
  expect_equal(
    simulatedScenarios[["TestScenario"]]$results$allQuantityPaths[[1]],
    defaultOutputPath
  )
})

test_that("It runs one scenario with specifying output paths", {
  pc <- testProject()
  OutputPaths <- enum(list(
    Aciclovir_PVB = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    Aciclovir_bone_pls = "Organism|Bone|Plasma|Aciclovir|Concentration"
  ))

  # Modify output paths on the scenario before running
  pc$scenarios[["TestScenario"]]$outputPaths <- enumValues(OutputPaths)

  simulatedScenarios <- runScenarios(
    project = pc,
    scenarioNames = "TestScenario"
  )

  expect_equal(names(simulatedScenarios), "TestScenario")
  expect_equal(
    simulatedScenarios[["TestScenario"]]$results$allQuantityPaths,
    enumValues(OutputPaths)
  )
})

test_that("It runs two scenarios", {
  pc <- testProject()
  scenarioNames <- c("TestScenario", "TestScenario2")

  # Disable steady-state for second scenario
  pc$scenarios[["TestScenario2"]]$simulateSteadyState <- FALSE
  # Prevent warning because Indiv not found by replacing with existing IndividualId
  pc$scenarios[["TestScenario2"]]$individualId <- "Indiv1"

  simulatedScenarios <- runScenarios(
    project = pc,
    scenarioNames = scenarioNames
  )

  expect_equal(names(simulatedScenarios), scenarioNames)
  expect_equal(
    simulatedScenarios[["TestScenario"]]$results$allQuantityPaths,
    defaultOutputPath
  )

  expect_equal(
    simulatedScenarios[["TestScenario2"]]$results$allQuantityPaths,
    c(
      "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      "Organism|Fat|Intracellular|Aciclovir|Concentration in container"
    )
  )
})

test_that("It runs population and individual scenarios", {
  pc <- testProject()
  scenarioNames <- c("TestScenario", "PopulationScenario")

  simulatedScenarios <- runScenarios(
    project = pc,
    scenarioNames = scenarioNames
  )

  expect_equal(names(simulatedScenarios), scenarioNames)
  # Check that the first scenario is individual simulation
  expect_equal(
    length(simulatedScenarios[["TestScenario"]]$results$allIndividualIds),
    1
  )
  # Check that the second scenario is population simulation
  expect_equal(
    length(simulatedScenarios[["PopulationScenario"]]$results$allIndividualIds),
    2
  )
})

# exportScenarioResults / importScenarioResults ----

test_that("exportScenarioResults saves results to csv and pkml", {
  pc <- testProject()

  simulatedScenarios <- runScenarios(
    project = pc,
    scenarioNames = "TestScenario"
  )

  tempdir <- tempdir()
  withr::with_tempdir(
    code = {
      outputFolder <- exportScenarioResults(
        simulatedScenariosResults = simulatedScenarios,
        project = pc,
        outputFolder = tempdir
      )
      expect_true(file.exists(file.path(tempdir, "TestScenario.pkml")))
      expect_true(file.exists(file.path(tempdir, "TestScenario.csv")))
      expect_equal(outputFolder, tempdir)
    },
    tmpdir = tempdir
  )
})

test_that("importScenarioResults loads results from csv and pkml", {
  pc <- testProject()

  simulatedScenarios <- runScenarios(
    project = pc,
    scenarioNames = "TestScenario"
  )

  tempdir <- tempdir()
  withr::with_tempdir(
    code = {
      exportScenarioResults(
        simulatedScenariosResults = simulatedScenarios,
        project = pc,
        outputFolder = tempdir
      )

      loaded <- importScenarioResults(
        scenarioNames = "TestScenario",
        resultsFolder = tempdir
      )

      expect_equal(names(loaded), "TestScenario")
      expect_false(is.null(loaded$TestScenario$simulation))
      expect_false(is.null(loaded$TestScenario$results))
      expect_false(is.null(loaded$TestScenario$outputValues))
    },
    tmpdir = tempdir
  )
})

test_that("importScenarioResults throws an error when files don't exist", {
  nonExistentFolder <- file.path(tempdir(), "non-existent-folder")

  expect_error(
    importScenarioResults(
      scenarioNames = "TestScenario",
      resultsFolder = nonExistentFolder
    )
  )
})

test_that("export/import handles scenario names with forbidden characters", {
  pc <- testProject()

  simulatedScenarios <- runScenarios(
    project = pc,
    scenarioNames = "TestScenario"
  )
  # Rename simulatedScenarios to include a slash
  names(simulatedScenarios) <- "TestScenario/with/slash"

  tempdir <- tempdir()
  withr::with_tempdir(
    code = {
      # Save results using temp folder
      outputFolder <- exportScenarioResults(
        simulatedScenariosResults = simulatedScenarios,
        project = pc,
        outputFolder = tempdir
      )
      # Check that the results are saved
      expect_true(file.exists(file.path(
        tempdir,
        "TestScenario_with_slash.pkml"
      )))
      expect_true(file.exists(file.path(
        tempdir,
        "TestScenario_with_slash.csv"
      )))

      # Check that the corrected output folder path is returned
      expect_equal(outputFolder, tempdir)

      # Load results using temp folder
      simulatedScenarioResults <- importScenarioResults(
        scenarioNames = "TestScenario/with/slash",
        resultsFolder = tempdir
      )

      # Check that the results are loaded with correct names
      expect_equal(names(simulatedScenarioResults), "TestScenario/with/slash")
    },
    tmpdir = tempdir
  )
})

test_that("customParams in runScenarios overrides default parameters", {
  pc <- testProject()
  dosePath <- "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose"

  # Default run — dose should be 250 from the JSON
  results <- runScenarios(pc, scenarioNames = "TestScenario")
  sim <- results$TestScenario$simulation
  doseParam <- ospsuite::getParameter(dosePath, sim)
  expect_equal(
    doseParam$value,
    ospsuite::toBaseUnit(doseParam, 250, "mg")
  )

  # Run with custom parameter overriding dose to 300
  results2 <- runScenarios(
    pc,
    scenarioNames = "TestScenario",
    customParams = list(
      paths = dosePath,
      values = 300,
      units = "mg"
    )
  )
  sim2 <- results2$TestScenario$simulation
  doseParam2 <- ospsuite::getParameter(dosePath, sim2)
  expect_equal(
    doseParam2$value,
    ospsuite::toBaseUnit(doseParam2, 300, "mg")
  )
})

test_that("deprecated loadScenarioResults still works", {
  nonExistentFolder <- file.path(tempdir(), "non-existent-folder")

  lifecycle::expect_deprecated(
    expect_error(
      loadScenarioResults(
        scenarioNames = "TestScenario",
        resultsFolder = nonExistentFolder
      )
    )
  )
})

test_that("It correctly runs when only one scenario name is provided", {
  pc <- testProject()

  simulatedScenarios <- runScenarios(
    project = pc,
    scenarioNames = "TestScenario"
  )

  expect_equal(names(simulatedScenarios), "TestScenario")
  expect_equal(
    simulatedScenarios[["TestScenario"]]$results$allQuantityPaths[[1]],
    defaultOutputPath
  )
})

# JSON integration ----

test_that("runScenarios runs a scenario from JSON Project", {
  pc <- testProject()
  results <- runScenarios(pc, scenarioNames = "TestScenario")
  expect_true("TestScenario" %in% names(results))
  expect_false(is.null(results$TestScenario$results))
  expect_false(is.null(results$TestScenario$outputValues))
})

test_that("runScenarios errors on unknown scenario name", {
  pc <- testProject()
  expect_error(
    runScenarios(pc, scenarioNames = "NonExistent"),
    "NonExistent"
  )
})

test_that("runScenarios filters to specified scenarioNames", {
  pc <- testProject()
  results <- runScenarios(pc, scenarioNames = "TestScenario")
  expect_length(results, 1)
  expect_equal(names(results), "TestScenario")
})

# .executeScenario tests ----

test_that(".executeScenario initializes and runs an individual scenario", {
  pc <- testProject()
  scenario <- pc$scenarios[["TestScenario"]]
  cache <- new.env(parent = emptyenv())
  cache$individuals <- list()
  cache$populations <- list()

  result <- esqlabsR:::.executeScenario(
    scenario = scenario,
    pc = pc,
    customParams = NULL,
    cache = cache,
    simulationRunOptions = NULL
  )

  expect_true(!is.null(result$simulation))
  expect_true(!is.null(result$results))
  expect_true(!is.null(result$outputValues))
  expect_null(result$population)
  expect_equal(result$simulation$name, "TestScenario")
})

test_that(".executeScenario caches IndividualCharacteristics", {
  pc <- testProject()
  scenario <- pc$scenarios[["TestScenario"]]
  cache <- new.env(parent = emptyenv())
  cache$individuals <- list()
  cache$populations <- list()

  esqlabsR:::.executeScenario(
    scenario = scenario,
    pc = pc,
    customParams = NULL,
    cache = cache,
    simulationRunOptions = NULL
  )

  # IndividualCharacteristics should now be cached
  expect_true(scenario$individualId %in% names(cache$individuals))
  expect_s3_class(
    cache$individuals[[scenario$individualId]],
    "IndividualCharacteristics"
  )
})

test_that(".executeScenario reuses cached IndividualCharacteristics", {
  pc <- testProject()
  scenario <- pc$scenarios[["TestScenario"]]
  cache <- new.env(parent = emptyenv())

  # Pre-populate cache
  indivData <- pc$individuals[[scenario$individualId]]
  indivChar <- ospsuite::createIndividualCharacteristics(
    species = indivData$species,
    population = indivData$population,
    gender = indivData$gender,
    weight = indivData$weight,
    height = indivData$height,
    age = indivData$age
  )
  cache$individuals <- list()
  cache$individuals[[scenario$individualId]] <- indivChar
  cache$populations <- list()

  result <- esqlabsR:::.executeScenario(
    scenario = scenario,
    pc = pc,
    customParams = NULL,
    cache = cache,
    simulationRunOptions = NULL
  )

  # Should have used the cached one
  expect_identical(
    cache$individuals[[scenario$individualId]],
    indivChar
  )
})

# addScenario ----

test_that("addScenario errors on duplicate scenario name", {
  pc <- testProject()
  existing_name <- names(pc$scenarios)[[1]]
  expect_error(
    addScenario(pc, scenarioName = existing_name, modelFile = "model.pkml"),
    "already exists"
  )
})

test_that("addScenario errors on invalid individualId", {
  pc <- testProject()
  expect_error(
    addScenario(
      pc,
      scenarioName = "NewScenario",
      modelFile = "model.pkml",
      individualId = "NonExistent"
    ),
    "individualId.*NonExistent.*not found"
  )
})

test_that("addScenario collects all validation errors in one message", {
  pc <- testProject()
  expect_error(
    addScenario(
      pc,
      scenarioName = "NewScenario",
      modelFile = "model.pkml",
      individualId = "BadIndiv",
      populationId = "BadPop",
      applicationProtocol = "BadApp"
    ),
    "BadIndiv.*BadPop.*BadApp"
  )
})

test_that("addScenario errors on empty scenarioName", {
  pc <- testProject()
  expect_error(
    addScenario(pc, scenarioName = "", modelFile = "model.pkml"),
    "non-empty string"
  )
})

# Happy path ----

test_that("addScenario adds a valid scenario with correct fields", {
  pc <- testProject()
  original_count <- length(pc$scenarios)

  addScenario(
    pc,
    scenarioName = "ProgrammaticScenario",
    modelFile = "Aciclovir.pkml",
    individualId = "Indiv1"
  )

  expect_length(pc$scenarios, original_count + 1)
  expect_true("ProgrammaticScenario" %in% names(pc$scenarios))

  sc <- pc$scenarios[["ProgrammaticScenario"]]
  expect_s3_class(sc, "Scenario")
  expect_equal(sc$scenarioName, "ProgrammaticScenario")
  expect_equal(sc$modelFile, "Aciclovir.pkml")
  expect_equal(sc$individualId, "Indiv1")
  expect_equal(sc$simulationType, "Individual")
})

test_that("addScenario with populationId sets simulationType to Population", {
  pc <- testProject()
  pop_name <- names(pc$populations)[[1]]

  addScenario(
    pc,
    scenarioName = "PopScenario",
    modelFile = "Aciclovir.pkml",
    populationId = pop_name
  )

  sc <- pc$scenarios[["PopScenario"]]
  expect_equal(sc$simulationType, "Population")
  expect_equal(sc$populationId, pop_name)
})

test_that("addScenario parses simulationTime string into list of vectors", {
  pc <- testProject()

  addScenario(
    pc,
    scenarioName = "TimeScenario",
    modelFile = "Aciclovir.pkml",
    simulationTime = "0, 100, 1",
    simulationTimeUnit = "h"
  )

  sc <- pc$scenarios[["TimeScenario"]]
  expect_equal(sc$simulationTime, list(c(0, 100, 1)))
  expect_equal(sc$simulationTimeUnit, "h")
})

test_that("addScenario resolves outputPathIds to output path strings", {
  pc <- testProject()
  path_ids <- names(pc$outputPaths)

  addScenario(
    pc,
    scenarioName = "OutputScenario",
    modelFile = "Aciclovir.pkml",
    outputPathIds = path_ids
  )

  sc <- pc$scenarios[["OutputScenario"]]
  expect_equal(sc$outputPaths, unname(pc$outputPaths[path_ids]))
})

test_that("addScenario sets modified flag to TRUE", {
  pc <- testProject()
  expect_false(pc$modified)

  addScenario(
    pc,
    scenarioName = "ModifiedScenario",
    modelFile = "Aciclovir.pkml"
  )

  expect_true(pc$modified)
})

test_that("pc$addScenario() delegates to standalone addScenario()", {
  pc <- testProject()

  pc$addScenario(
    scenarioName = "MethodScenario",
    modelFile = "Aciclovir.pkml",
    individualId = "Indiv1"
  )

  expect_true("MethodScenario" %in% names(pc$scenarios))
  sc <- pc$scenarios[["MethodScenario"]]
  expect_equal(sc$individualId, "Indiv1")
})

test_that("addScenario populates all optional fields correctly", {
  pc <- testProject()
  param_group <- names(pc$modelParameters)[[1]]

  addScenario(
    pc,
    scenarioName = "FullScenario",
    modelFile = "Aciclovir.pkml",
    individualId = "Indiv1",
    applicationProtocol = names(pc$applications)[[1]],
    parameterGroups = param_group,
    simulationTime = "0, 50, 1; 50, 100, 2",
    simulationTimeUnit = "min",
    steadyState = TRUE,
    steadyStateTime = 500,
    overwriteFormulasInSS = TRUE,
    readPopulationFromCSV = FALSE
  )

  sc <- pc$scenarios[["FullScenario"]]
  expect_equal(sc$applicationProtocol, names(pc$applications)[[1]])
  expect_equal(sc$parameterGroups, param_group)
  expect_equal(sc$simulationTime, list(c(0, 50, 1), c(50, 100, 2)))
  expect_equal(sc$simulationTimeUnit, "min")
  expect_true(sc$simulateSteadyState)
  expect_equal(sc$steadyStateTime, 500)
  expect_true(sc$overwriteFormulasInSS)
  expect_false(sc$readPopulationFromCSV)
})

test_that("addScenario returns project invisibly", {
  pc <- testProject()
  result <- withVisible(addScenario(
    pc,
    scenarioName = "InvisibleScenario",
    modelFile = "Aciclovir.pkml"
  ))
  expect_false(result$visible)
  expect_identical(result$value, pc)
})

# Guard-clause regression tests ----

test_that(".prepareScenario errors when simulationTimeUnit is NULL but simulationTime is set", {
  pc <- testProject()
  scenario <- pc$scenarios[["TestScenario"]]
  # simulationTime is already set from JSON; remove the unit

  scenario$simulationTimeUnit <- NULL

  cache <- new.env(parent = emptyenv())
  cache$individuals <- list()
  cache$populations <- list()

  expect_error(
    esqlabsR:::.prepareScenario(
      scenario = scenario,
      pc = pc,
      customParams = NULL,
      cache = cache,
      simulationRunOptions = NULL
    ),
    "simulation time.*unit|simulationTimeUnit|time unit"
  )
})

test_that(".prepareScenario errors when populationId references a non-existent population", {
  pc <- testProject()
  scenario <- pc$scenarios[["PopulationScenario"]]$clone()
  scenario$populationId <- "NonExistentPopulation"

  cache <- new.env(parent = emptyenv())
  cache$individuals <- list()
  cache$populations <- list()

  expect_error(
    esqlabsR:::.prepareScenario(
      scenario = scenario,
      pc = pc,
      customParams = NULL,
      cache = cache,
      simulationRunOptions = NULL
    ),
    "NonExistentPopulation.*not found"
  )
})

test_that(".prepareScenario does not crash on is.na(NULL) when applicationProtocol is NULL", {
  pc <- testProject()
  scenario <- pc$scenarios[["TestScenario"]]$clone()
  # Default Scenario has applicationProtocol = NULL; force it explicitly
  scenario$applicationProtocol <- NULL

  cache <- new.env(parent = emptyenv())
  cache$individuals <- list()
  cache$populations <- list()

  # Should not error with "argument is of length zero" from is.na(NULL).
  # It may still succeed or fail for another reason, but the old crash
  # from if(is.na(NULL)) must not happen.
  result <- tryCatch(
    esqlabsR:::.prepareScenario(
      scenario = scenario,
      pc = pc,
      customParams = NULL,
      cache = cache,
      simulationRunOptions = NULL
    ),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    expect_no_match(conditionMessage(result), "argument is of length zero")
  } else {
    # If no error, the guard was handled gracefully
    expect_true(!is.null(result$simulation))
  }
})
