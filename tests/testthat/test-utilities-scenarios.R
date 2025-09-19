defaultOutputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

test_that("It stops with an error if the excel file defines a parameter that is
          not present", {
  # Create a fresh temporary project for this test
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  # Define which scenarios to run
  scenarioNames <- c("TestScenario_missingParam")
  # Create `ScenarioConfiguration` objects from excel files
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )
  expect_error(createScenarios(
    scenarioConfigurations = scenarioConfigurations,
    stopIfParameterNotFound = TRUE
  ))
})

test_that("All working scenarios in testProject can be created without errors", {
  # Create a fresh temporary project for this test
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  # Define which scenarios to run
  scenarioNames <- c(
    "TestScenario",
    "TestScenario2",
    "PopulationScenario",
    "PopulationScenarioFromCSV"
  )
  # Create `ScenarioConfiguration` objects from excel files
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )

  expect_no_error(createScenarios(
    scenarioConfigurations = scenarioConfigurations,
    stopIfParameterNotFound = FALSE
  ))
})

test_that("It runs one scenario without specifying output paths", {
  # Create a fresh temporary project for this test
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  # Define which scenarios to run
  scenarioNames <- c("TestScenario_missingParam")
  # Create `ScenarioConfiguration` objects from excel files
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )
  scenarios <- createScenarios(
    scenarioConfigurations = scenarioConfigurations,
    stopIfParameterNotFound = FALSE
  )

  simulatedScenarios <- runScenarios(
    scenarios = scenarios
  )

  expect_equal(names(simulatedScenarios), scenarioNames)
  expect_equal(
    simulatedScenarios[[scenarioNames[[1]]]]$results$allQuantityPaths,
    defaultOutputPath
  )
})

test_that("It runs one scenario with specifying output paths", {
  # Create a fresh temporary project for this test
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
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
  expect_equal(
    simulatedScenarios[[scenarioNames[[1]]]]$results$allQuantityPaths,
    enumValues(OutputPaths)
  )
})

test_that("It runs two scenarios", {
  # Create a fresh temporary project for this test
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
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
  # Prevent warning because Indiv not found by replacing with existing IndividualId
  scenarioConfigurations[[2]]$individualId <- "Indiv1"

  scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations)

  simulatedScenarios <- runScenarios(
    scenarios = scenarios
  )

  expect_equal(names(simulatedScenarios), scenarioNames)
  expect_equal(
    simulatedScenarios[[scenarioNames[[1]]]]$results$allQuantityPaths,
    defaultOutputPath
  )

  expect_equal(
    simulatedScenarios[[scenarioNames[[2]]]]$results$allQuantityPaths,
    c(
      "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      "Organism|Fat|Intracellular|Aciclovir|Concentration in container"
    )
  )
})

test_that("It runs population and individual scenarios", {
  # Create a fresh temporary project for this test
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
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
  expect_equal(
    length(simulatedScenarios[[scenarioNames[[1]]]]$results$allIndividualIds),
    1
  )
  # Check that the second scenario is population simulation
  expect_equal(
    length(simulatedScenarios[[scenarioNames[[2]]]]$results$allIndividualIds),
    2
  )
})


test_that("It saves and loads scenario results for scenario names with forbidden characters", {
  # Create a fresh temporary project for this test
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
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


test_that("The hierarchy of parametrization is correct", {
  # Create a fresh temporary project for this test
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  # Define which scenarios to run
  scenarioNames <- c("TestScenario")
  # Create `ScenarioConfiguration` objects from excel files without custom parameters
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )
  scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations)

  # Check that the hierarchy of parametrization is correct
  idx <- which(
    scenarios[[1]]$finalCustomParams$paths ==
      "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose"
  )
  expect_equal(scenarios[[1]]$finalCustomParams$values[[idx]], 250)

  # Create `ScenarioConfiguration` objects from excel files with custom parameter
  scenarios <- createScenarios(
    scenarioConfigurations = scenarioConfigurations,
    customParams = list(
      paths = "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
      values = 300,
      units = "mg"
    )
  )

  # Check that the custom parameter overrides the default
  idx <- which(
    scenarios[[1]]$finalCustomParams$paths ==
      "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose"
  )
  expect_equal(scenarios[[1]]$finalCustomParams$values[[idx]], 300)
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

test_that("It correctly runs when only one scenario (not a list) is provided", {
  # Create a fresh temporary project for this test
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

  # Define which scenarios to run
  scenarioNames <- c("TestScenario_missingParam")
  # Create `ScenarioConfiguration` objects from excel files
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )
  scenarios <- createScenarios(
    scenarioConfigurations = scenarioConfigurations,
    stopIfParameterNotFound = FALSE
  )

  simulatedScenarios <- runScenarios(
    scenarios = scenarios[[1]]
  )

  expect_equal(names(simulatedScenarios), scenarioNames)
  expect_equal(
    simulatedScenarios[[scenarioNames[[1]]]]$results$allQuantityPaths,
    defaultOutputPath
  )
})
