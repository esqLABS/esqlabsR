defaultOutputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

test_that("runScenarios stops with an error if a parameter is not present", {
  project <- testProject()
  expect_error(runScenarios(
    project,
    scenarioNames = "TestScenario_missingParam"
  ))
})

test_that("It runs one scenario without specifying output paths", {
  project <- testProject()

  simulatedScenarios <- runScenarios(
    project = project,
    scenarioNames = "TestScenario"
  )

  expect_equal(names(simulatedScenarios), "TestScenario")
  expect_equal(
    simulatedScenarios[["TestScenario"]]$results$allQuantityPaths[[1]],
    defaultOutputPath
  )
})

test_that("It runs one scenario with specifying output paths", {
  project <- testProject()
  OutputPaths <- enum(list(
    Aciclovir_PVB = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    Aciclovir_bone_pls = "Organism|Bone|Plasma|Aciclovir|Concentration"
  ))

  # Modify output paths on the scenario before running
  project$scenarios[["TestScenario"]]$outputPaths <- enumValues(OutputPaths)

  simulatedScenarios <- runScenarios(
    project = project,
    scenarioNames = "TestScenario"
  )

  expect_equal(names(simulatedScenarios), "TestScenario")
  expect_equal(
    simulatedScenarios[["TestScenario"]]$results$allQuantityPaths,
    enumValues(OutputPaths)
  )
})

test_that("It runs two scenarios", {
  project <- testProject()
  scenarioNames <- c("TestScenario", "TestScenario2")

  # Disable steady-state for second scenario
  project$scenarios[["TestScenario2"]]$simulateSteadyState <- FALSE
  # Prevent warning because Indiv not found by replacing with existing IndividualId
  project$scenarios[["TestScenario2"]]$individualId <- "Indiv1"

  simulatedScenarios <- runScenarios(
    project = project,
    scenarioNames = scenarioNames
  )

  expect_equal(names(simulatedScenarios), scenarioNames)
  expect_true(
    defaultOutputPath %in%
      simulatedScenarios[["TestScenario"]]$results$allQuantityPaths
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
  project <- testProject()
  scenarioNames <- c("TestScenario", "PopulationScenario")

  simulatedScenarios <- runScenarios(
    project = project,
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
  project <- testProject()

  simulatedScenarios <- runScenarios(
    project = project,
    scenarioNames = "TestScenario"
  )

  tempdir <- tempdir()
  withr::with_tempdir(
    code = {
      outputFolder <- exportScenarioResults(
        simulatedScenariosResults = simulatedScenarios,
        project = project,
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
  project <- testProject()

  simulatedScenarios <- runScenarios(
    project = project,
    scenarioNames = "TestScenario"
  )

  tempdir <- tempdir()
  withr::with_tempdir(
    code = {
      exportScenarioResults(
        simulatedScenariosResults = simulatedScenarios,
        project = project,
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
  project <- testProject()

  simulatedScenarios <- runScenarios(
    project = project,
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
        project = project,
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
  project <- testProject()
  dosePath <- "Events|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose"

  # Default run — dose should be 250 from the JSON
  results <- runScenarios(project, scenarioNames = "TestScenario")
  sim <- results$TestScenario$simulation
  doseParam <- ospsuite::getParameter(dosePath, sim)
  expect_equal(
    doseParam$value,
    ospsuite::toBaseUnit(doseParam, 250, "mg")
  )

  # Run with custom parameter overriding dose to 300
  results2 <- runScenarios(
    project,
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
  project <- testProject()

  simulatedScenarios <- runScenarios(
    project = project,
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
  project <- testProject()
  results <- runScenarios(project, scenarioNames = "TestScenario")
  expect_true("TestScenario" %in% names(results))
  expect_false(is.null(results$TestScenario$results))
  expect_false(is.null(results$TestScenario$outputValues))
})

test_that("runScenarios errors on unknown scenario name", {
  project <- testProject()
  expect_error(
    runScenarios(project, scenarioNames = "NonExistent"),
    "NonExistent"
  )
})

test_that("runScenarios filters to specified scenarioNames", {
  project <- testProject()
  results <- runScenarios(project, scenarioNames = "TestScenario")
  expect_length(results, 1)
  expect_equal(names(results), "TestScenario")
})


# Guard-clause regression tests ----

test_that(".prepareScenario errors when simulationTimeUnit is NULL but simulationTime is set", {
  project <- testProject()
  scenario <- project$scenarios[["TestScenario"]]
  # simulationTime is already set from JSON; remove the unit

  scenario$simulationTimeUnit <- NULL

  cache <- new.env(parent = emptyenv())
  cache$individuals <- list()
  cache$populations <- list()

  expect_error(
    esqlabsR:::.prepareScenario(
      scenario = scenario,
      project = project,
      customParams = NULL,
      cache = cache,
      simulationRunOptions = NULL
    ),
    "simulation time.*unit|simulationTimeUnit|time unit"
  )
})

test_that(".prepareScenario errors when populationId references a non-existent population", {
  project <- testProject()
  scenario <- project$scenarios[["PopulationScenario"]]$clone()
  scenario$populationId <- "NonExistentPopulation"

  cache <- new.env(parent = emptyenv())
  cache$individuals <- list()
  cache$populations <- list()

  expect_error(
    esqlabsR:::.prepareScenario(
      scenario = scenario,
      project = project,
      customParams = NULL,
      cache = cache,
      simulationRunOptions = NULL
    ),
    "NonExistentPopulation.*not found"
  )
})

test_that(".prepareScenario does not crash on is.na(NULL) when applicationProtocol is NULL", {
  project <- testProject()
  scenario <- project$scenarios[["TestScenario"]]$clone()
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
      project = project,
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


# createScenariosFromPKML ------------------------------------------------

# ==============================================================================
# SECTION 1: Core Functionality Tests
# ==============================================================================

test_that("Basic scenario creation from single PKML file", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Create scenarios from PKML
  scenarioConfigurations <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPath,
    project = project
  )

  expect_length(scenarioConfigurations, 1)
  scenario <- scenarioConfigurations[[1]]
  expect_equal(scenario$scenarioName, "Vergin 1995 IV")
  expect_equal(as.character(scenario$modelFile), "Aciclovir.pkml")
  expect_equal(scenario$applicationProtocol, "Vergin 1995 IV")
  expect_equal(scenario$simulationTimeUnit, "h")
  expect_false(scenario$simulateSteadyState)
  expect_false(scenario$readPopulationFromCSV)
  expect_true(length(scenario$outputPaths) > 0)
  expect_true(!is.null(scenario$simulationTime))
})

test_that("Scenario creation with custom names", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  scenarioConfigurations <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPath,
    project = project,
    scenarioNames = "CustomScenario"
  )

  expect_length(scenarioConfigurations, 1)
  expect_equal(names(scenarioConfigurations), "CustomScenario")
  expect_equal(
    scenarioConfigurations[["CustomScenario"]]$scenarioName,
    "CustomScenario"
  )
})

test_that("Time unit extraction from PKML file", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  scenarioConfigurations <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPath,
    project = project
  )

  expect_equal(scenarioConfigurations[[1]]$simulationTimeUnit, "h")
})

test_that("Custom time unit overrides PKML time unit", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  scenarioConfigurations <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPath,
    project = project,
    simulationTimeUnit = "min"
  )

  expect_equal(scenarioConfigurations[[1]]$simulationTimeUnit, "min")
})

# ==============================================================================
# SECTION 2: Vectorization and Recycling Tests
# ==============================================================================

test_that("Case 1: Single PKML, no other arguments (original behavior)", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  scenarioConfigurations <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPath,
    project = project
  )

  expect_length(scenarioConfigurations, 1)
  expect_equal(names(scenarioConfigurations), "Vergin 1995 IV")
})

test_that("Case 2: Single PKML with single-value arguments", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  scenarioConfigurations <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPath,
    project = project,
    scenarioNames = "CustomScenario",
    individualId = "Individual001",
    steadyState = TRUE,
    readPopulationFromCSV = FALSE
  )

  expect_length(scenarioConfigurations, 1)
  expect_equal(names(scenarioConfigurations), "CustomScenario")

  scenario <- scenarioConfigurations[["CustomScenario"]]
  expect_equal(scenario$individualId, "Individual001")
  expect_true(scenario$simulateSteadyState)
  expect_false(scenario$readPopulationFromCSV)
})

test_that("Case 3: Single PKML with vector arguments (PKML recycled)", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  scenarioConfigurations <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPath,
    project = project,
    scenarioNames = c("LowDose", "MediumDose", "HighDose"),
    individualId = c("Patient1", "Patient2", "Patient3"),
    applicationProtocols = c("Protocol1", "Protocol2", "Protocol3"),
    steadyState = c(FALSE, TRUE, TRUE),
    readPopulationFromCSV = c(FALSE, FALSE, TRUE)
  )

  expect_length(scenarioConfigurations, 3)
  expect_equal(
    names(scenarioConfigurations),
    c("LowDose", "MediumDose", "HighDose")
  )

  # All scenarios should use the same model file
  for (scenario in scenarioConfigurations) {
    expect_equal(as.character(scenario$modelFile), "Aciclovir.pkml")
  }

  # But have different settings
  expect_equal(scenarioConfigurations[["LowDose"]]$individualId, "Patient1")
  expect_equal(scenarioConfigurations[["MediumDose"]]$individualId, "Patient2")
  expect_equal(scenarioConfigurations[["HighDose"]]$individualId, "Patient3")
})

test_that("Case 4: Multiple PKMLs with single-value arguments (arguments recycled)", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")
  pkmlPaths <- rep(pkmlPath, 3)

  scenarioConfigurations <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPaths,
    project = project,
    scenarioNames = c("Model1", "Model2", "Model3"),
    individualId = "SharedIndividual",
    applicationProtocols = "SharedProtocol",
    steadyState = TRUE,
    readPopulationFromCSV = FALSE
  )

  expect_length(scenarioConfigurations, 3)
  expect_equal(names(scenarioConfigurations), c("Model1", "Model2", "Model3"))

  # All scenarios should have the same recycled settings
  for (scenario in scenarioConfigurations) {
    expect_equal(scenario$individualId, "SharedIndividual")
    expect_equal(scenario$applicationProtocol, "SharedProtocol")
    expect_true(scenario$simulateSteadyState)
    expect_false(scenario$readPopulationFromCSV)
  }
})

test_that("Case 5: Multiple PKMLs with vector arguments", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")
  pkmlPaths <- rep(pkmlPath, 3)

  scenarioConfigurations <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPaths,
    project = project,
    scenarioNames = c("Pediatric", "Adult", "Elderly"),
    individualId = c("Child001", "Adult001", "Elder001"),
    applicationProtocols = c("PedProtocol", "AdultProtocol", "ElderProtocol"),
    steadyState = c(FALSE, TRUE, TRUE),
    readPopulationFromCSV = c(FALSE, TRUE, FALSE)
  )

  expect_length(scenarioConfigurations, 3)
  expect_equal(
    names(scenarioConfigurations),
    c("Pediatric", "Adult", "Elderly")
  )

  # Check each scenario has different settings
  expect_equal(scenarioConfigurations[["Pediatric"]]$individualId, "Child001")
  expect_equal(scenarioConfigurations[["Adult"]]$individualId, "Adult001")
  expect_equal(scenarioConfigurations[["Elderly"]]$individualId, "Elder001")

  expect_false(scenarioConfigurations[["Pediatric"]]$simulateSteadyState)
  expect_true(scenarioConfigurations[["Adult"]]$simulateSteadyState)
  expect_true(scenarioConfigurations[["Elderly"]]$simulateSteadyState)
})

test_that("Vector recycling works for single values", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")
  pkmlPaths <- rep(pkmlPath, 3)

  scenarioConfigurations <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPaths,
    project = project,
    scenarioNames = c("Scenario1", "Scenario2", "Scenario3"),
    individualId = "TestIndividual",
    steadyState = TRUE,
    steadyStateTime = 500,
    steadyStateTimeUnit = "min",
    readPopulationFromCSV = FALSE
  )

  expect_length(scenarioConfigurations, 3)

  # Check that all scenarios got the recycled values
  for (i in 1:3) {
    scenario <- scenarioConfigurations[[i]]
    expect_equal(scenario$individualId, "TestIndividual")
    expect_true(scenario$simulateSteadyState)
    expect_false(scenario$readPopulationFromCSV)
    expected_time <- ospsuite::toBaseUnit(
      quantityOrDimension = ospsuite::ospDimensions$Time,
      values = 500,
      unit = "min"
    )
    expect_equal(scenario$steadyStateTime, expected_time)
  }
})

test_that("Mixed NULL and vector arguments work correctly", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")
  pkmlPaths <- rep(pkmlPath, 2)

  scenarioConfigurations <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPaths,
    project = project,
    scenarioNames = c("Test1", "Test2"),
    individualId = NULL,
    populationId = c("Pop1", "Pop2"),
    applicationProtocols = "SharedProtocol",
    modelParameters = NULL,
    steadyState = c(TRUE, FALSE)
  )

  expect_length(scenarioConfigurations, 2)

  scenario1 <- scenarioConfigurations[["Test1"]]
  expect_null(scenario1$individualId)
  expect_equal(scenario1$populationId, "Pop1")
  expect_equal(scenario1$applicationProtocol, "SharedProtocol")
  expect_true(scenario1$simulateSteadyState)

  scenario2 <- scenarioConfigurations[["Test2"]]
  expect_null(scenario2$individualId)
  expect_equal(scenario2$populationId, "Pop2")
  expect_equal(scenario2$applicationProtocol, "SharedProtocol")
  expect_false(scenario2$simulateSteadyState)
})

# ==============================================================================
# SECTION 3: Parameter Handling Tests
# ==============================================================================

test_that("Custom parameters are applied correctly", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  scenarioConfigurations <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPath,
    project = project,
    individualId = "TestIndividual",
    modelParameters = "Global,Custom",
    steadyState = TRUE,
    steadyStateTime = 500,
    steadyStateTimeUnit = "min"
  )

  scenario <- scenarioConfigurations[[1]]
  expect_equal(scenario$individualId, "TestIndividual")
  expect_equal(scenario$modelParameters, c("Global", "Custom"))
  expect_true(scenario$simulateSteadyState)
  expected_time <- ospsuite::toBaseUnit(
    quantityOrDimension = ospsuite::ospDimensions$Time,
    values = 500,
    unit = "min"
  )
  expect_equal(scenario$steadyStateTime, expected_time)
})

test_that("Comma-separated modelParameters and outputPaths are handled correctly", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")
  pkmlPaths <- rep(pkmlPath, 2)

  scenarioConfigurations <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPaths,
    project = project,
    scenarioNames = c("Scenario1", "Scenario2"),
    modelParameters = c("Global,Custom", "Global,Alternative"),
    outputPaths = c("Path1,Path2", "Path3,Path4,Path5")
  )

  expect_length(scenarioConfigurations, 2)

  scenario1 <- scenarioConfigurations[["Scenario1"]]
  expect_equal(scenario1$modelParameters, c("Global", "Custom"))
  expect_equal(scenario1$outputPaths, c("Path1", "Path2"))

  scenario2 <- scenarioConfigurations[["Scenario2"]]
  expect_equal(scenario2$modelParameters, c("Global", "Alternative"))
  expect_equal(scenario2$outputPaths, c("Path3", "Path4", "Path5"))
})

test_that("Named outputPaths vectors are handled correctly", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")
  pkmlPaths <- rep(pkmlPath, 2)

  # Test with named vectors for outputPaths
  namedOutputPaths <- list(
    c(
      "plasma" = "Organism|VenousBlood|Plasma|compound|Concentration in container",
      "liver" = "Organism|Liver|Intracellular|compound|Concentration"
    ),
    c("kidney" = "Organism|Kidney|Intracellular|compound|Concentration")
  )

  scenarioConfigurations <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPaths,
    project = project,
    scenarioNames = c("Scenario1", "Scenario2"),
    outputPaths = namedOutputPaths
  )

  expect_length(scenarioConfigurations, 2)

  scenario1 <- scenarioConfigurations[["Scenario1"]]
  expectedPaths1 <- c(
    "plasma" = "Organism|VenousBlood|Plasma|compound|Concentration in container",
    "liver" = "Organism|Liver|Intracellular|compound|Concentration"
  )
  expect_equal(scenario1$outputPaths, expectedPaths1)
  expect_equal(names(scenario1$outputPaths), c("plasma", "liver"))

  scenario2 <- scenarioConfigurations[["Scenario2"]]
  expectedPaths2 <- c(
    "kidney" = "Organism|Kidney|Intracellular|compound|Concentration"
  )
  expect_equal(scenario2$outputPaths, expectedPaths2)
  expect_equal(names(scenario2$outputPaths), "kidney")
})

test_that("Single named outputPath vector is recycled correctly", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")
  pkmlPaths <- rep(pkmlPath, 2)

  # Test recycling a single named vector
  singleNamedOutputPath <- c(
    "plasma" = "Organism|VenousBlood|Plasma|compound|Concentration in container"
  )

  scenarioConfigurations <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPaths,
    project = project,
    scenarioNames = c("Scenario1", "Scenario2"),
    outputPaths = singleNamedOutputPath
  )

  expect_length(scenarioConfigurations, 2)

  # Both scenarios should have the same named output path
  scenario1 <- scenarioConfigurations[["Scenario1"]]
  scenario2 <- scenarioConfigurations[["Scenario2"]]

  expect_equal(scenario1$outputPaths, singleNamedOutputPath)
  expect_equal(scenario2$outputPaths, singleNamedOutputPath)
  expect_equal(names(scenario1$outputPaths), "plasma")
  expect_equal(names(scenario2$outputPaths), "plasma")
})

test_that("Vectorized simulation time parameters work correctly", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")
  pkmlPaths <- rep(pkmlPath, 2)

  scenarioConfigurations <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPaths,
    project = project,
    scenarioNames = c("ShortSim", "LongSim"),
    simulationTime = c("0, 24, 100", "0, 168, 200"),
    simulationTimeUnit = c("h", "h")
  )

  expect_length(scenarioConfigurations, 2)

  scenario1 <- scenarioConfigurations[["ShortSim"]]
  expected_intervals_1 <- list(c(0, 24, 100))
  expect_equal(scenario1$simulationTime, expected_intervals_1)
  expect_equal(scenario1$simulationTimeUnit, "h")

  scenario2 <- scenarioConfigurations[["LongSim"]]
  expected_intervals_2 <- list(c(0, 168, 200))
  expect_equal(scenario2$simulationTime, expected_intervals_2)
  expect_equal(scenario2$simulationTimeUnit, "h")
})

test_that("NULL defaults for steady state parameters work correctly", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Default behavior
  scenarioConfigurations1 <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPath,
    project = project
  )

  scenario1 <- scenarioConfigurations1[[1]]
  expect_false(scenario1$simulateSteadyState)

  # Steady state enabled but no time provided
  scenarioConfigurations2 <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPath,
    project = project,
    scenarioNames = "TestSteadyStateNullTime",
    steadyState = TRUE
  )

  scenario2 <- scenarioConfigurations2[[1]]
  expect_true(scenario2$simulateSteadyState)
})

test_that("Application protocol names are handled correctly", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Default protocol name should be scenario name
  scenarioConfigurations1 <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPath,
    project = project,
    scenarioNames = "MyCustomScenario"
  )

  expect_equal(
    scenarioConfigurations1[["MyCustomScenario"]]$applicationProtocol,
    "MyCustomScenario"
  )

  # Explicit protocol name should be used
  scenarioConfigurations2 <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPath,
    project = project,
    scenarioNames = "TestExplicitProtocol",
    applicationProtocols = "CustomProtocolName"
  )

  expect_equal(
    scenarioConfigurations2[["TestExplicitProtocol"]]$applicationProtocol,
    "CustomProtocolName"
  )
})

test_that("Complex scenario configuration with all parameters", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Test scenario with all possible parameters set
  scenarioConfigurations <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPath,
    project = project,
    scenarioNames = "ComplexScenario",
    individualId = "Individual123",
    populationId = "Population456",
    applicationProtocols = "ComplexProtocol",
    modelParameters = "Global,Custom,Advanced",
    outputPaths = "Path1,Path2,Path3",
    simulationTime = "0,24,100;24,48,50",
    simulationTimeUnit = "h",
    steadyState = TRUE,
    steadyStateTime = 1500,
    steadyStateTimeUnit = "min",
    readPopulationFromCSV = TRUE
  )

  scenario <- scenarioConfigurations[["ComplexScenario"]]
  expect_equal(scenario$scenarioName, "ComplexScenario")
  expect_equal(scenario$individualId, "Individual123")
  expect_equal(scenario$populationId, "Population456")
  expect_equal(scenario$simulationType, "Population")
  expect_equal(scenario$applicationProtocol, "ComplexProtocol")
  expect_equal(
    scenario$modelParameters,
    c("Global", "Custom", "Advanced")
  )
  expect_equal(scenario$outputPaths, c("Path1", "Path2", "Path3"))
  expect_equal(scenario$simulationTime, list(c(0, 24, 100), c(24, 48, 50)))
  expect_equal(scenario$simulationTimeUnit, "h")
  expect_true(scenario$simulateSteadyState)
  expect_true(scenario$readPopulationFromCSV)
  # Steady state time should be converted to base units
  expect_true(!is.null(scenario$steadyStateTime))
})

test_that("Empty parameter sheets and output paths handling", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Test with empty parameter sheets
  scenarioConfigurations <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPath,
    project = project,
    scenarioNames = "EmptyParamsTest",
    modelParameters = "",
    outputPaths = ""
  )

  scenario <- scenarioConfigurations[["EmptyParamsTest"]]
  expect_equal(scenario$scenarioName, "EmptyParamsTest")
  # Empty param sheets should not add any sheets
  expect_null(scenario$modelParameters)
})

test_that("PKML file with custom simulation time intervals", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Test with custom simulation time that overrides PKML
  scenarioConfigurations <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPath,
    project = project,
    scenarioNames = "CustomTimeTest",
    simulationTime = "0,12,50;12,24,25",
    simulationTimeUnit = "h"
  )

  scenario <- scenarioConfigurations[["CustomTimeTest"]]
  expect_equal(scenario$simulationTime, list(c(0, 12, 50), c(12, 24, 25)))
  expect_equal(scenario$simulationTimeUnit, "h")
})

test_that(".parseSimulationTimeIntervals internal function works correctly", {
  # Test NULL input
  result <- esqlabsR:::.parseSimulationTimeIntervals(NULL)
  expect_null(result)

  # Test single interval
  result <- esqlabsR:::.parseSimulationTimeIntervals("0,24,100")
  expect_equal(result, list(c(0, 24, 100)))

  # Test multiple intervals
  result <- esqlabsR:::.parseSimulationTimeIntervals("0,24,100;24,48,50")
  expect_equal(result, list(c(0, 24, 100), c(24, 48, 50)))

  # Test error conditions
  expect_error(
    esqlabsR:::.parseSimulationTimeIntervals("0,24"),
    "time interval string"
  )

  expect_error(
    esqlabsR:::.parseSimulationTimeIntervals("0,24,0"),
    "time interval string"
  )

  expect_error(
    esqlabsR:::.parseSimulationTimeIntervals("-1,24,100"),
    "time interval string"
  )

  expect_error(
    esqlabsR:::.parseSimulationTimeIntervals("24,0,100"),
    "time interval string"
  )
})

# ==============================================================================
# SECTION 4: Error Handling and Validation Tests
# ==============================================================================

test_that("Non-existent PKML file throws error", {
  temp_project <- with_temp_project()
  project <- temp_project$config

  expect_error(
    createScenariosFromPKML(
      pkmlFilePaths = "non_existent_file.pkml",
      project = project
    ),
    "File not found"
  )
})

test_that("Invalid vector lengths throw errors", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")
  pkmlPaths <- rep(pkmlPath, 3) # 3 scenarios

  # Test with wrong length for individualId (pkmlPaths=3, individualId=2 -> inconsistent lengths)
  expect_error(
    createScenariosFromPKML(
      pkmlFilePaths = pkmlPaths,
      project = project,
      individualId = c("Ind1", "Ind2") # Length 2, but pkmlPaths is length 3
    ),
    "Inconsistent vector argument lengths"
  )

  # Test with wrong length for steadyState (pkmlPaths=3, steadyState=2 -> inconsistent lengths)
  expect_error(
    createScenariosFromPKML(
      pkmlFilePaths = pkmlPaths,
      project = project,
      steadyState = c(TRUE, FALSE) # Length 2, but pkmlPaths is length 3
    ),
    "Inconsistent vector argument lengths"
  )

  # Test inconsistent vector lengths (both > 1 but different lengths)
  expect_error(
    createScenariosFromPKML(
      pkmlFilePaths = pkmlPath, # Length 1
      project = project,
      scenarioNames = c("S1", "S2"), # Length 2
      individualId = c("I1", "I2", "I3") # Length 3 - inconsistent!
    ),
    "Inconsistent vector argument lengths.*All vector arguments with length > 1 must have the same length"
  )
})

test_that("Duplicate scenario names are handled correctly", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Test with duplicate custom scenario names
  expect_warning(
    scenarioConfigurations <- createScenariosFromPKML(
      pkmlFilePaths = c(pkmlPath, pkmlPath, pkmlPath),
      project = project,
      scenarioNames = c("DuplicateName", "DuplicateName", "UniqueName")
    ),
    "Duplicate scenario names found and made unique by adding indices"
  )

  # Check that names were made unique
  expect_equal(
    names(scenarioConfigurations),
    c("DuplicateName", "DuplicateName_2", "UniqueName")
  )
})

# readScenarioConfigurationFromExcel was removed — scenarios are now loaded
# directly from JSON via Project$new(). No replacement test needed here.

test_that("Scenario objects are returned with correct type", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  scenarioConfigurations <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPath,
    project = project
  )

  expect_length(scenarioConfigurations, 1)
  expect_true(isOfType(scenarioConfigurations[[1]], "Scenario"))
})

# ==============================================================================
# SECTION 6: Edge Cases and Regression Tests
# ==============================================================================

test_that("Function has no side effects on Excel files", {
  temp_project <- with_temp_project()
  project <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Record initial state
  applicationsFile <- project$applicationsFile
  initialExists <- file.exists(applicationsFile)
  initialSheets <- character(0)
  if (initialExists) {
    initialSheets <- readxl::excel_sheets(applicationsFile)
  }

  # Create scenarios (should have no side effects)
  scenarioConfigurations <- createScenariosFromPKML(
    pkmlFilePaths = pkmlPath,
    project = project,
    scenarioNames = "TestNoSideEffects"
  )

  # Check that scenario was created
  expect_length(scenarioConfigurations, 1)
  expect_equal(names(scenarioConfigurations), "TestNoSideEffects")

  # Check that Applications file state is unchanged
  finalExists <- file.exists(applicationsFile)
  expect_equal(finalExists, initialExists)

  if (finalExists) {
    finalSheets <- readxl::excel_sheets(applicationsFile)
    expect_equal(finalSheets, initialSheets)
    expect_false("TestNoSideEffects" %in% finalSheets)
  }
})

test_that("Helper functions work correctly", {
  # Test .sanitizeExcelSheetName
  expect_equal(esqlabsR:::.sanitizeExcelSheetName(NULL, warn = FALSE), "Sheet")
  expect_equal(esqlabsR:::.sanitizeExcelSheetName(NA, warn = FALSE), "Sheet")
  expect_equal(esqlabsR:::.sanitizeExcelSheetName("", warn = FALSE), "Sheet")
  expect_equal(esqlabsR:::.sanitizeExcelSheetName("   ", warn = FALSE), "Sheet")
  expect_equal(
    esqlabsR:::.sanitizeExcelSheetName("ValidName", warn = FALSE),
    "ValidName"
  )

  # Test with invalid characters
  expect_warning(
    result <- esqlabsR:::.sanitizeExcelSheetName(
      "Invalid/Name[*]:?\\",
      warn = TRUE
    ),
    "Excel sheet name was sanitized"
  )
  expect_equal(result, "Invalid_Name______")

  # Test name that's too long
  longName <- paste(rep("a", 35), collapse = "")
  expect_warning(
    result <- esqlabsR:::.sanitizeExcelSheetName(longName, warn = TRUE),
    "Excel sheet name was sanitized"
  )
  expect_equal(nchar(result), 31)
})

# .mergeScenarioParameters layer order ----------------------------------

# Layer merge order in .prepareScenario:
#   1. modelParameters (iterated in scenario$modelParameters order)
#   2. species defaults (from bundled SpeciesParameters.xlsx)
#   3. individual inline parameters (project$individuals[[id]]$parameters)
#   4. application inline parameters (project$applications[[id]]$parameters)
#   5. customParams (caller-supplied)
# Last write wins, silent.

# All tests construct a minimal in-memory Project where every layer sets the
# *same* parameter path to a different value and assert which value survives.

# Helper: build a minimal project with one model parameter set, one individual,
# one application, one scenario referencing all three. PARAM is the single
# parameter path used across layers.
PARAM_PATH <- "Organism|Liver|Volume"
PARAM_CONTAINER <- "Organism|Liver"
PARAM_NAME <- "Volume"

.makeOrderTestProject <- function(
  modelParamsValues = NULL, # named list: setId -> value
  individualValue = NULL, # numeric scalar or NULL
  applicationValue = NULL, # numeric scalar or NULL
  scenarioModelParamIds = character() # ordered ids referenced by scenario
) {
  project <- Project$new()
  project$modelFolder <- tempdir()

  # Build model parameter sets
  for (setId in names(modelParamsValues)) {
    addModelParameter(
      project,
      id = setId,
      containerPath = PARAM_CONTAINER,
      parameterName = PARAM_NAME,
      value = modelParamsValues[[setId]],
      units = "L"
    )
  }

  # Add individual (with optional parameter)
  addIndividual(project, "I1", species = "Human")
  if (!is.null(individualValue)) {
    addIndividualParameter(
      project,
      "I1",
      containerPath = PARAM_CONTAINER,
      parameterName = PARAM_NAME,
      value = individualValue,
      units = "L"
    )
  }

  # Add application (with optional parameter)
  addApplication(project, "App1")
  if (!is.null(applicationValue)) {
    addApplicationParameter(
      project,
      "App1",
      containerPath = PARAM_CONTAINER,
      parameterName = PARAM_NAME,
      value = applicationValue,
      units = "L"
    )
  }

  # Add scenario referencing the model parameter sets in the requested order
  addScenario(
    project,
    "S1",
    "model.pkml",
    individualId = "I1",
    applicationProtocol = "App1",
    modelParameters = scenarioModelParamIds
  )

  project
}

# Helper: extract the merged value for PARAM_PATH from .prepareScenario,
# bypassing the simulation load (which needs a real pkml). We patch
# ospsuite::loadSimulation to return a mock so we can call .prepareScenario
# end-to-end and read the merged params off the simulation state.
#
# Simpler approach: extract just the merge-loop portion. We introduce a
# helper .mergeScenarioParameters in Task 2 that returns the merged structure
# without loading the simulation; we test that helper directly.

.mergedValueFor <- function(project, customParams = NULL) {
  scenario <- project$scenarios[["S1"]]
  merged <- .mergeScenarioParameters(scenario, project, customParams)
  idx <- which(merged$paths == PARAM_PATH)
  if (length(idx) == 0) NA_real_ else merged$values[[idx]]
}

test_that("within modelParameters, later set overrides earlier", {
  project <- .makeOrderTestProject(
    modelParamsValues = list(SetA = 10, SetB = 20),
    scenarioModelParamIds = c("SetA", "SetB")
  )
  expect_equal(.mergedValueFor(project), 20)

  pc2 <- .makeOrderTestProject(
    modelParamsValues = list(SetA = 10, SetB = 20),
    scenarioModelParamIds = c("SetB", "SetA")
  )
  expect_equal(.mergedValueFor(pc2), 10)
})

test_that("individual inline parameters override modelParameters layer", {
  project <- .makeOrderTestProject(
    modelParamsValues = list(Set = 10),
    individualValue = 99,
    scenarioModelParamIds = "Set"
  )
  expect_equal(.mergedValueFor(project), 99)
})

test_that("application inline parameters override individual inline parameters", {
  project <- .makeOrderTestProject(
    modelParamsValues = list(Set = 10),
    individualValue = 99,
    applicationValue = 7,
    scenarioModelParamIds = "Set"
  )
  expect_equal(.mergedValueFor(project), 7)
})

test_that("customParams override application inline parameters", {
  project <- .makeOrderTestProject(
    modelParamsValues = list(Set = 10),
    individualValue = 99,
    applicationValue = 7,
    scenarioModelParamIds = "Set"
  )
  custom <- list(paths = PARAM_PATH, values = 1, units = "L")
  expect_equal(.mergedValueFor(project, customParams = custom), 1)
})

test_that("layer that doesn't set the path doesn't disturb other layers", {
  # Only individual sets the path; everything else absent.
  project <- .makeOrderTestProject(individualValue = 42)
  expect_equal(.mergedValueFor(project), 42)
})

test_that("merge produces correct path|value|units triplets", {
  project <- .makeOrderTestProject(
    modelParamsValues = list(Set = 10),
    scenarioModelParamIds = "Set"
  )
  scenario <- project$scenarios[["S1"]]
  merged <- .mergeScenarioParameters(scenario, project, customParams = NULL)
  expect_setequal(c("paths", "values", "units"), names(merged))
  expect_equal(length(merged$paths), length(merged$values))
  expect_equal(length(merged$paths), length(merged$units))
})
