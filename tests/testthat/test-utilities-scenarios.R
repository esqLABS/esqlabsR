defaultOutputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

test_that("runScenarios stops with an error if a parameter is not present", {
  pc <- testProjectConfigurationJSON()
  expect_error(runScenarios(pc, scenarioNames = "TestScenario_missingParam"))
})

test_that("It runs one scenario without specifying output paths", {
  pc <- testProjectConfigurationJSON()

  simulatedScenarios <- runScenarios(
    projectConfiguration = pc,
    scenarioNames = "TestScenario"
  )

  expect_equal(names(simulatedScenarios), "TestScenario")
  expect_equal(
    simulatedScenarios[["TestScenario"]]$results$allQuantityPaths,
    defaultOutputPath
  )
})

test_that("It runs one scenario with specifying output paths", {
  pc <- testProjectConfigurationJSON()
  OutputPaths <- enum(list(
    Aciclovir_PVB = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    Aciclovir_bone_pls = "Organism|Bone|Plasma|Aciclovir|Concentration"
  ))

  # Modify output paths on the scenario configuration before running
  pc$scenarioConfigurations[["TestScenario"]]$outputPaths <- enumValues(OutputPaths)

  simulatedScenarios <- runScenarios(
    projectConfiguration = pc,
    scenarioNames = "TestScenario"
  )

  expect_equal(names(simulatedScenarios), "TestScenario")
  expect_equal(
    simulatedScenarios[["TestScenario"]]$results$allQuantityPaths,
    enumValues(OutputPaths)
  )
})

test_that("It runs two scenarios", {
  pc <- testProjectConfigurationJSON()
  scenarioNames <- c("TestScenario", "TestScenario2")

  # Disable steady-state for second config
  pc$scenarioConfigurations[["TestScenario2"]]$simulateSteadyState <- FALSE
  # Prevent warning because Indiv not found by replacing with existing IndividualId
  pc$scenarioConfigurations[["TestScenario2"]]$individualId <- "Indiv1"

  simulatedScenarios <- runScenarios(
    projectConfiguration = pc,
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
  pc <- testProjectConfigurationJSON()
  scenarioNames <- c("TestScenario", "PopulationScenario")

  simulatedScenarios <- runScenarios(
    projectConfiguration = pc,
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


test_that("It saves and loads scenario results for scenario names with forbidden characters", {
  pc <- testProjectConfigurationJSON()

  simulatedScenarios <- runScenarios(
    projectConfiguration = pc,
    scenarioNames = "TestScenario"
  )
  # Rename simulatedScenarios to include a slash
  names(simulatedScenarios) <- "TestScenario/with/slash"

  tempdir <- tempdir()
  withr::with_tempdir(
    code = {
      # Save results using temp folder
      outputFolder <- saveScenarioResults(
        simulatedScenariosResults = simulatedScenarios,
        projectConfiguration = pc,
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

      # Check that the correted output folder path is returned
      expect_equal(outputFolder, tempdir)

      # Load results using temp folder
      simulatedScenarioResults <- loadScenarioResults(
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
  pc <- testProjectConfigurationJSON()
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

test_that("loadScenarioResults throws an error when files don't exist", {
  # Use a non-existent folder to trigger an error
  nonExistentFolder <- file.path(tempdir(), "non-existent-folder")

  expect_error(
    loadScenarioResults(
      scenarioNames = "TestScenario",
      resultsFolder = nonExistentFolder
    )
  )
})

test_that("It correctly runs when only one scenario name is provided", {
  pc <- testProjectConfigurationJSON()

  simulatedScenarios <- runScenarios(
    projectConfiguration = pc,
    scenarioNames = "TestScenario"
  )

  expect_equal(names(simulatedScenarios), "TestScenario")
  expect_equal(
    simulatedScenarios[["TestScenario"]]$results$allQuantityPaths,
    defaultOutputPath
  )
})

# JSON integration ----

test_that("runScenarios runs a scenario from JSON ProjectConfiguration", {
  pc <- testProjectConfigurationJSON()
  results <- runScenarios(pc, scenarioNames = "TestScenario")
  expect_true("TestScenario" %in% names(results))
  expect_false(is.null(results$TestScenario$results))
  expect_false(is.null(results$TestScenario$outputValues))
})

test_that("runScenarios errors on unknown scenario name", {
  pc <- testProjectConfigurationJSON()
  expect_error(
    runScenarios(pc, scenarioNames = "NonExistent"),
    "NonExistent"
  )
})

test_that("runScenarios runs all scenarios when scenarioNames is NULL", {
  pc <- testProjectConfigurationJSON()
  # Only run individual scenarios to keep test fast
  # Filter to just TestScenario to avoid population scenarios being slow
  results <- runScenarios(pc, scenarioNames = "TestScenario")
  expect_length(results, 1)
})
