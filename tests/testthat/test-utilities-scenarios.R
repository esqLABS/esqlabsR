##  context("runScenarios")
# Create a project configuration
projectConfiguration <- createDefaultProjectConfiguration(test_ProjectConfiguration())
defaultOutputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

test_that("It stops with an error if the excel file defines a parameter that is
          not present", {
  # Define which scenarios to run
  scenarioNames <- c("TestScenario_missingParam")
  # Create `ScenarioConfiguration` objects from excel files
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )
  expect_error(createScenarios(scenarioConfigurations = scenarioConfigurations, stopIfParameterNotFound = TRUE))
})

test_that("It runs one scenario without specifying output paths", {
  # Define which scenarios to run
  scenarioNames <- c("TestScenario_missingParam")
  # Create `ScenarioConfiguration` objects from excel files
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )
  scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations, stopIfParameterNotFound = FALSE)

  simulatedScenarios <- runScenarios(
    scenarios = scenarios
  )

  expect_equal(names(simulatedScenarios), scenarioNames)
  expect_equal(simulatedScenarios[[scenarioNames[[1]]]]$results$allQuantityPaths, defaultOutputPath)
})

test_that("It runs one scenario with specifying output paths", {
  OutputPaths <- enum(list(
    Aciclovir_PVB = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    Aciclovir_bone_pls = "Organism|Bone|Plasma|Aciclovir|Concentration"
  ))

  # Define which scenarios to run
  scenarioNames <- c("TestScenario")
  # Create `ScenarioConfiguration` objects from excel files
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )

  for (scenarioConfiguration in scenarioConfigurations) {
    scenarioConfiguration$outputPaths <- enumValues(OutputPaths)
  }

  scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations)

  simulatedScenarios <- runScenarios(
    scenarios = scenarios
  )

  expect_equal(names(simulatedScenarios), scenarioNames)
  expect_equal(simulatedScenarios[[scenarioNames[[1]]]]$results$allQuantityPaths, enumValues(OutputPaths))
})

test_that("It runs two scenarios", {
  # Define which scenarios to run
  scenarioNames <- c(
    "TestScenario",
    "TestScenario2"
  )
  # Create `ScenarioConfiguration` objects from excel files
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )
  # Disable steady-state for second config
  scenarioConfigurations[[2]]$simulateSteadyState <- FALSE

  scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations)

  simulatedScenarios <- runScenarios(
    scenarios = scenarios
  )

  expect_equal(names(simulatedScenarios), scenarioNames)
  expect_equal(simulatedScenarios[[scenarioNames[[1]]]]$results$allQuantityPaths, defaultOutputPath)

  expect_equal(simulatedScenarios[[scenarioNames[[2]]]]$results$allQuantityPaths, c(
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    "Organism|Fat|Intracellular|Aciclovir|Concentration in container"
  ))
})

test_that("It runs population and individual scenarios", {
  # Define which scenarios to run
  scenarioNames <- c(
    "TestScenario",
    "PopulationScenario"
  )
  # Create `ScenarioConfiguration` objects from excel files
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )
  scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations)
  simulatedScenarios <- runScenarios(
    scenarios = scenarios
  )

  expect_equal(names(simulatedScenarios), scenarioNames)
  # Check that the first scenario is individual simulation
  expect_equal(length(simulatedScenarios[[scenarioNames[[1]]]]$results$allIndividualIds), 1)
  # Check that the second scenario is population simulation
  expect_equal(length(simulatedScenarios[[scenarioNames[[2]]]]$results$allIndividualIds), 2)
})


test_that("It saves and loads scenario results for scenario names with forbidden characters", {
  # Define which scenarios to run
  scenarioNames <- c("TestScenario")
  # Create `ScenarioConfiguration` objects from excel files
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )
  scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations)

  simulatedScenarios <- runScenarios(
    scenarios = scenarios
  )
  # Rename simulatedScenarios to include a slash
  names(simulatedScenarios) <- "TestScenario/with/slash"

  tempdir <- tempdir()
  withr::with_tempdir(
    code = {
      # Save results using temp folder
      outputFolder <- saveScenarioResults(
        simulatedScenariosResults = simulatedScenarios,
        projectConfiguration = projectConfiguration,
        outputFolder = tempdir
      )
      # Check that the results are saved
      expect_true(file.exists(file.path(tempdir, "TestScenario_with_slash.pkml")))
      expect_true(file.exists(file.path(tempdir, "TestScenario_with_slash.csv")))

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
