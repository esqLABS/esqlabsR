# ==============================================================================
# SECTION 1: Core Functionality Tests
# ==============================================================================

test_that("Basic scenario creation from single PKML file", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Create scenarios from PKML
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration
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
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
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
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration
  )

  expect_equal(scenarioConfigurations[[1]]$simulationTimeUnit, "h")
})

test_that("Custom time unit overrides PKML time unit", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    simulationTimeUnit = "min"
  )

  expect_equal(scenarioConfigurations[[1]]$simulationTimeUnit, "min")
})

# ==============================================================================
# SECTION 2: Vectorization and Recycling Tests
# ==============================================================================

test_that("Case 1: Single PKML, no other arguments (original behavior)", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration
  )

  expect_length(scenarioConfigurations, 1)
  expect_equal(names(scenarioConfigurations), "Vergin 1995 IV")
})

test_that("Case 2: Single PKML with single-value arguments", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
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
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
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
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")
  pkmlPaths <- rep(pkmlPath, 3)

  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPaths,
    projectConfiguration = projectConfiguration,
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
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")
  pkmlPaths <- rep(pkmlPath, 3)

  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPaths,
    projectConfiguration = projectConfiguration,
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
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")
  pkmlPaths <- rep(pkmlPath, 3)

  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPaths,
    projectConfiguration = projectConfiguration,
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
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")
  pkmlPaths <- rep(pkmlPath, 2)

  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPaths,
    projectConfiguration = projectConfiguration,
    scenarioNames = c("Test1", "Test2"),
    individualId = NULL,
    populationId = c("Pop1", "Pop2"),
    applicationProtocols = "SharedProtocol",
    paramSheets = NULL,
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
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    individualId = "TestIndividual",
    paramSheets = "Global,Custom",
    steadyState = TRUE,
    steadyStateTime = 500,
    steadyStateTimeUnit = "min"
  )

  scenario <- scenarioConfigurations[[1]]
  expect_equal(scenario$individualId, "TestIndividual")
  expect_equal(enumKeys(scenario$paramSheets), c("Global", "Custom"))
  expect_true(scenario$simulateSteadyState)
  expected_time <- ospsuite::toBaseUnit(
    quantityOrDimension = ospsuite::ospDimensions$Time,
    values = 500,
    unit = "min"
  )
  expect_equal(scenario$steadyStateTime, expected_time)
})

test_that("Comma-separated paramSheets and outputPaths are handled correctly", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")
  pkmlPaths <- rep(pkmlPath, 2)

  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPaths,
    projectConfiguration = projectConfiguration,
    scenarioNames = c("Scenario1", "Scenario2"),
    paramSheets = c("Global,Custom", "Global,Alternative"),
    outputPaths = c("Path1,Path2", "Path3,Path4,Path5")
  )

  expect_length(scenarioConfigurations, 2)

  scenario1 <- scenarioConfigurations[["Scenario1"]]
  expect_equal(enumKeys(scenario1$paramSheets), c("Global", "Custom"))
  expect_equal(scenario1$outputPaths, c("Path1", "Path2"))

  scenario2 <- scenarioConfigurations[["Scenario2"]]
  expect_equal(enumKeys(scenario2$paramSheets), c("Global", "Alternative"))
  expect_equal(scenario2$outputPaths, c("Path3", "Path4", "Path5"))
})

test_that("Named outputPaths vectors are handled correctly", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
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

  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPaths,
    projectConfiguration = projectConfiguration,
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
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")
  pkmlPaths <- rep(pkmlPath, 2)

  # Test recycling a single named vector
  singleNamedOutputPath <- c(
    "plasma" = "Organism|VenousBlood|Plasma|compound|Concentration in container"
  )

  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPaths,
    projectConfiguration = projectConfiguration,
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

test_that("Named outputPaths are exported to Excel with correct OutputPathIds", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Create scenario with named output paths
  namedOutputPaths <- c(
    "plasma" = "Organism|VenousBlood|Plasma|compound|Concentration in container",
    "liver" = "Organism|Liver|Intracellular|compound|Concentration"
  )

  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    scenarioNames = "TestScenario",
    outputPaths = namedOutputPaths
  )

  # Add to Excel
  addScenarioConfigurationsToExcel(
    scenarioConfigurations = scenarioConfigurations,
    projectConfiguration = projectConfiguration,
    appendToExisting = FALSE
  )

  # Read back the OutputPaths sheet
  outputPaths <- readExcel(
    path = projectConfiguration$scenariosFile,
    sheet = "OutputPaths"
  )

  # Check that the names are used as OutputPathId
  expect_true("plasma" %in% outputPaths$OutputPathId)
  expect_true("liver" %in% outputPaths$OutputPathId)

  # Check that the paths match
  plasmaRow <- outputPaths[outputPaths$OutputPathId == "plasma", ]
  expect_equal(
    plasmaRow$OutputPath,
    "Organism|VenousBlood|Plasma|compound|Concentration in container"
  )

  liverRow <- outputPaths[outputPaths$OutputPathId == "liver", ]
  expect_equal(
    liverRow$OutputPath,
    "Organism|Liver|Intracellular|compound|Concentration"
  )
})

test_that("Vectorized simulation time parameters work correctly", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")
  pkmlPaths <- rep(pkmlPath, 2)

  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPaths,
    projectConfiguration = projectConfiguration,
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
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Default behavior
  scenarioConfigurations1 <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration
  )

  scenario1 <- scenarioConfigurations1[[1]]
  expect_false(scenario1$simulateSteadyState)

  # Steady state enabled but no time provided
  scenarioConfigurations2 <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    scenarioNames = "TestSteadyStateNullTime",
    steadyState = TRUE
  )

  scenario2 <- scenarioConfigurations2[[1]]
  expect_true(scenario2$simulateSteadyState)
})

test_that("Application protocol names are handled correctly", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Default protocol name should be scenario name
  scenarioConfigurations1 <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    scenarioNames = "MyCustomScenario"
  )

  expect_equal(
    scenarioConfigurations1[["MyCustomScenario"]]$applicationProtocol,
    "MyCustomScenario"
  )

  # Explicit protocol name should be used
  scenarioConfigurations2 <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
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
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Test scenario with all possible parameters set
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    scenarioNames = "ComplexScenario",
    individualId = "Individual123",
    populationId = "Population456",
    applicationProtocols = "ComplexProtocol",
    paramSheets = "Global,Custom,Advanced",
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
    enumKeys(scenario$paramSheets),
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
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Test with empty parameter sheets
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    scenarioNames = "EmptyParamsTest",
    paramSheets = "",
    outputPaths = ""
  )

  scenario <- scenarioConfigurations[["EmptyParamsTest"]]
  expect_equal(scenario$scenarioName, "EmptyParamsTest")
  # Empty param sheets should not add any sheets
  expect_equal(length(enumKeys(scenario$paramSheets)), 0)
})

test_that("PKML file with custom simulation time intervals", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Test with custom simulation time that overrides PKML
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
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
  projectConfiguration <- temp_project$config

  expect_error(
    createScenarioConfigurationsFromPKML(
      pkmlFilePaths = "non_existent_file.pkml",
      projectConfiguration = projectConfiguration
    ),
    "File not found"
  )
})

test_that("Invalid vector lengths throw errors", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")
  pkmlPaths <- rep(pkmlPath, 3) # 3 scenarios

  # Test with wrong length for individualId (pkmlPaths=3, individualId=2 -> inconsistent lengths)
  expect_error(
    createScenarioConfigurationsFromPKML(
      pkmlFilePaths = pkmlPaths,
      projectConfiguration = projectConfiguration,
      individualId = c("Ind1", "Ind2") # Length 2, but pkmlPaths is length 3
    ),
    "Inconsistent vector argument lengths"
  )

  # Test with wrong length for steadyState (pkmlPaths=3, steadyState=2 -> inconsistent lengths)
  expect_error(
    createScenarioConfigurationsFromPKML(
      pkmlFilePaths = pkmlPaths,
      projectConfiguration = projectConfiguration,
      steadyState = c(TRUE, FALSE) # Length 2, but pkmlPaths is length 3
    ),
    "Inconsistent vector argument lengths"
  )

  # Test inconsistent vector lengths (both > 1 but different lengths)
  expect_error(
    createScenarioConfigurationsFromPKML(
      pkmlFilePaths = pkmlPath, # Length 1
      projectConfiguration = projectConfiguration,
      scenarioNames = c("S1", "S2"), # Length 2
      individualId = c("I1", "I2", "I3") # Length 3 - inconsistent!
    ),
    "Inconsistent vector argument lengths.*All vector arguments with length > 1 must have the same length"
  )
})

test_that("Duplicate scenario names are handled correctly", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Test with duplicate custom scenario names
  expect_warning(
    scenarioConfigurations <- createScenarioConfigurationsFromPKML(
      pkmlFilePaths = c(pkmlPath, pkmlPath, pkmlPath),
      projectConfiguration = projectConfiguration,
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

test_that("readScenarioConfigurationFromExcel error conditions", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

  # Test with non-existent scenarios file
  expect_error(
    readScenarioConfigurationFromExcel(
      scenarioNames = "NonExistent",
      projectConfiguration = projectConfiguration
    ),
    "Scenario.*not specified"
  )

  # Create an invalid Excel file structure
  invalidData <- data.frame(
    WrongColumn1 = "test",
    WrongColumn2 = "test2"
  )
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Scenarios")
  openxlsx::writeData(wb, "Scenarios", invalidData)
  openxlsx::saveWorkbook(
    wb,
    projectConfiguration$scenariosFile,
    overwrite = TRUE
  )

  expect_error(
    readScenarioConfigurationFromExcel(
      projectConfiguration = projectConfiguration
    ),
    "wrong structure"
  )
})

test_that("addScenarioConfigurationsToExcel validation errors", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

  # Test with invalid scenarioConfigurations (not a named list)
  expect_error(
    addScenarioConfigurationsToExcel(
      scenarioConfigurations = list("invalid"),
      projectConfiguration = projectConfiguration
    ),
    "scenarioConfigurations must be a named list"
  )

  # Test with invalid scenarioConfigurations (wrong object type)
  expect_error(
    addScenarioConfigurationsToExcel(
      scenarioConfigurations = list(TestScenario = "not a scenario config"),
      projectConfiguration = projectConfiguration
    ),
    "type.*character.*expected.*ScenarioConfiguration"
  )
})

test_that("Internal validation functions work correctly", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

  # Create a valid scenario configuration
  validScenario <- ScenarioConfiguration$new(projectConfiguration)
  validScenario$scenarioName <- "ValidScenario"

  # Test valid scenario configurations
  expect_silent(esqlabsR:::.validateScenarioConfigurations(list(validScenario)))

  # Test population scenario without population ID
  populationScenario <- ScenarioConfiguration$new(projectConfiguration)
  populationScenario$scenarioName <- "PopulationScenario"
  populationScenario$simulationType <- "Population"
  populationScenario$populationId <- NULL

  expect_error(
    esqlabsR:::.validateScenarioConfigurations(list(populationScenario)),
    "population.*id"
  )

  # Test population scenario with population ID
  populationScenario$populationId <- "TestPopulation"
  expect_silent(esqlabsR:::.validateScenarioConfigurations(list(
    populationScenario
  )))
})

# ==============================================================================
# SECTION 5: Excel Integration Tests
# ==============================================================================

test_that("Full workflow: createScenarioConfigurationsFromPKML + addScenarioConfigurationsToExcel", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Create scenarios
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration
  )

  # Add to Excel
  addScenarioConfigurationsToExcel(
    scenarioConfigurations = scenarioConfigurations,
    projectConfiguration = projectConfiguration
  )

  # Verify by reading back
  scenarioName <- "Vergin 1995 IV"
  scenarios <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioName,
    projectConfiguration = projectConfiguration
  )

  expect_length(scenarios, 1)
  scenario <- scenarios[[1]]
  expect_equal(scenario$scenarioName, scenarioName)
  expect_equal(as.character(scenario$modelFile), "Aciclovir.pkml")
  expect_true(
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)" %in%
      scenario$outputPaths
  )
})

test_that("Excel append functionality works correctly", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # First, create some existing scenarios
  existingScenarios <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = c(pkmlPath, pkmlPath),
    projectConfiguration = projectConfiguration,
    scenarioNames = c("ExistingScenario1", "ExistingScenario2")
  )
  addScenarioConfigurationsToExcel(
    scenarioConfigurations = existingScenarios,
    projectConfiguration = projectConfiguration
  )

  # Then add new scenarios
  newScenarios <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = c(pkmlPath, pkmlPath),
    projectConfiguration = projectConfiguration,
    scenarioNames = c("NewScenario1", "NewScenario2")
  )
  addScenarioConfigurationsToExcel(
    scenarioConfigurations = newScenarios,
    projectConfiguration = projectConfiguration,
    appendToExisting = TRUE
  )

  # Verify all scenarios exist
  allScenarios <- readScenarioConfigurationFromExcel(
    scenarioNames = c(
      "ExistingScenario1",
      "ExistingScenario2",
      "NewScenario1",
      "NewScenario2"
    ),
    projectConfiguration = projectConfiguration
  )
  expect_length(allScenarios, 4)
})

test_that("Duplicate scenario names in Excel throw error", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # First add a scenario
  firstScenarios <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    scenarioNames = "DuplicateScenario"
  )
  addScenarioConfigurationsToExcel(
    scenarioConfigurations = firstScenarios,
    projectConfiguration = projectConfiguration
  )

  # Try to add another scenario with the same name
  duplicateScenarios <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    scenarioNames = "DuplicateScenario"
  )

  expect_error(
    addScenarioConfigurationsToExcel(
      scenarioConfigurations = duplicateScenarios,
      projectConfiguration = projectConfiguration
    ),
    "Duplicate scenario names found"
  )
})

test_that("Application protocol sheets are created correctly", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Create scenarios
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration
  )

  # Add to Excel (this should create the protocol sheet)
  addScenarioConfigurationsToExcel(
    scenarioConfigurations = scenarioConfigurations,
    projectConfiguration = projectConfiguration
  )

  # Check that the protocol sheet was created
  scenarioName <- "Vergin 1995 IV"
  protocolName <- scenarioName

  expect_true(
    protocolName %in%
      readxl::excel_sheets(projectConfiguration$applicationsFile)
  )
})

test_that("Existing application sheets are not overwritten", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")
  applicationsFile <- projectConfiguration$applicationsFile

  # Create initial data
  initialData <- data.frame(
    "Container Path" = "Test|Container",
    "Parameter Name" = "TestParam",
    "Value" = 999,
    "Units" = "mg",
    check.names = FALSE
  )

  # Write to Excel with specific sheet name
  protocolName <- "TestNoOverwrite"
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, protocolName)
  openxlsx::writeData(wb, protocolName, initialData)
  openxlsx::saveWorkbook(wb, applicationsFile, overwrite = TRUE)

  # Verify initial data
  initialParams <- readxl::read_excel(applicationsFile, sheet = protocolName)
  expect_equal(nrow(initialParams), 1)
  expect_equal(initialParams$`Parameter Name`[1], "TestParam")

  # Create scenarios with the same protocol name
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    scenarioNames = protocolName
  )

  # Add scenarios to Excel
  addScenarioConfigurationsToExcel(
    scenarioConfigurations = scenarioConfigurations,
    projectConfiguration = projectConfiguration
  )

  # Check that the existing sheet was not overwritten
  finalParams <- readxl::read_excel(applicationsFile, sheet = protocolName)
  expect_equal(nrow(finalParams), 1)
  expect_equal(finalParams$`Parameter Name`[1], "TestParam")
  expect_equal(finalParams$Value[1], 999)
})

test_that("Scenarios can be created without PKML file existing for applications", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Create scenarios
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    scenarioNames = "TestMissingPKML"
  )

  # Manually remove the PKML file to test the error path
  tempPkml <- file.path(temp_project$config$modelFolder, "temp.pkml")
  file.copy(pkmlPath, tempPkml)
  scenarioConfigurations[[1]]$modelFile <- "temp.pkml"
  file.remove(tempPkml)

  # This should throw an error when trying to add to Excel
  expect_error(
    addScenarioConfigurationsToExcel(
      scenarioConfigurations = scenarioConfigurations,
      projectConfiguration = projectConfiguration
    ),
    "PKML.*file cannot be find"
  )
})

test_that("Empty applications file creation works correctly", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Create scenarios
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    scenarioNames = "TestEmptyFile"
  )

  # Remove applications file to test creation
  applicationsFile <- projectConfiguration$applicationsFile
  if (file.exists(applicationsFile)) {
    file.remove(applicationsFile)
  }

  # This should create the applications file
  addScenarioConfigurationsToExcel(
    scenarioConfigurations = scenarioConfigurations,
    projectConfiguration = projectConfiguration
  )

  expect_true(file.exists(applicationsFile))
  sheets <- readxl::excel_sheets(applicationsFile)
  expect_true("TestEmptyFile" %in% sheets)
})

# ==============================================================================
# SECTION 6: Edge Cases and Regression Tests
# ==============================================================================

test_that("Function has no side effects on Excel files", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Record initial state
  applicationsFile <- projectConfiguration$applicationsFile
  initialExists <- file.exists(applicationsFile)
  initialSheets <- character(0)
  if (initialExists) {
    initialSheets <- readxl::excel_sheets(applicationsFile)
  }

  # Create scenarios (should have no side effects)
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
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

test_that("Created scenarios are runnable", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Create and add scenarios
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration
  )

  addScenarioConfigurationsToExcel(
    scenarioConfigurations = scenarioConfigurations,
    projectConfiguration = projectConfiguration
  )

  # Read back and run
  scenarioName <- "Vergin 1995 IV"
  scenarios <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioName,
    projectConfiguration = projectConfiguration
  )

  # Test that the scenario is runnable
  runnableScenarios <- createScenarios(scenarioConfigurations = scenarios)
  simulatedScenarios <- runScenarios(scenarios = runnableScenarios)

  expect_length(simulatedScenarios, 1)
  expect_equal(names(simulatedScenarios), scenarioName)
  expect_true(!is.null(simulatedScenarios[[scenarioName]]$results))
  expect_true(
    length(simulatedScenarios[[scenarioName]]$results$allQuantityPaths) > 0
  )
})

test_that("Both Applications and Events parameters are extracted", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Load simulation to check what parameters exist
  simulation <- ospsuite::loadSimulation(pkmlPath)

  applicationsParams <- ospsuite::getAllParametersMatching(
    "Applications|**",
    simulation
  )
  eventsParams <- ospsuite::getAllParametersMatching("Events|**", simulation)

  # Skip test if no parameters found
  skip_if(
    length(applicationsParams) == 0 && length(eventsParams) == 0,
    "No application/event parameters found in test PKML"
  )

  # Create and add scenarios
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    scenarioNames = "TestBothNodes"
  )

  addScenarioConfigurationsToExcel(
    scenarioConfigurations = scenarioConfigurations,
    projectConfiguration = projectConfiguration
  )

  # Check that parameters were written
  applicationsFile <- projectConfiguration$applicationsFile
  expect_true(file.exists(applicationsFile))

  sheets <- readxl::excel_sheets(applicationsFile)
  expect_true("TestBothNodes" %in% sheets)

  params <- readxl::read_excel(applicationsFile, sheet = "TestBothNodes")
  expect_true(nrow(params) > 0)
})

test_that("Constant parameters are filtered correctly", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Load simulation and check parameter types
  simulation <- ospsuite::loadSimulation(pkmlPath)
  allParams <- c(
    ospsuite::getAllParametersMatching("Applications|**", simulation),
    ospsuite::getAllParametersMatching("Events|**", simulation)
  )

  # Find constant parameters
  constantParams <- Filter(function(p) p$isConstant, allParams)

  # Skip test if no parameters found
  skip_if(
    length(allParams) == 0,
    "No application/event parameters found in test PKML"
  )

  # Create scenarios
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    scenarioNames = "TestConstantFilter"
  )

  # Add scenarios to Excel
  addScenarioConfigurationsToExcel(
    scenarioConfigurations = scenarioConfigurations,
    projectConfiguration = projectConfiguration
  )

  # Read the written parameters
  applicationsFile <- projectConfiguration$applicationsFile
  if (
    file.exists(applicationsFile) &&
      "TestConstantFilter" %in% readxl::excel_sheets(applicationsFile)
  ) {
    params <- readxl::read_excel(applicationsFile, sheet = "TestConstantFilter")

    # If there are constant parameters and they were written
    if (length(constantParams) > 0 && nrow(params) > 0) {
      # All written parameters should be from constant parameters
      writtenPaths <- paste(
        params$`Container Path`,
        params$`Parameter Name`,
        sep = "|"
      )
      constantPaths <- sapply(constantParams, function(p) p$path)

      # Check that written parameters are a subset of constant parameters
      expect_true(all(writtenPaths %in% constantPaths))
    }
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

  # Test .formatSimulationTimeForExcel
  expect_equal(esqlabsR:::.formatSimulationTimeForExcel(NULL), "")
  expect_equal(
    esqlabsR:::.formatSimulationTimeForExcel(list(c(0, 24, 100))),
    "0, 24, 100"
  )
  expect_equal(
    esqlabsR:::.formatSimulationTimeForExcel(list(
      c(0, 24, 100),
      c(24, 48, 50)
    )),
    "0, 24, 100; 24, 48, 50"
  )
})

test_that("Deprecated setApplications function works", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  simulation <- ospsuite::loadSimulation(pkmlPath)
  scenarioConfiguration <- ScenarioConfiguration$new(projectConfiguration)
  scenarioConfiguration$applicationProtocol <- "TestProtocol"

  # Test deprecated function with warning
  expect_warning(
    setApplications(simulation, scenarioConfiguration),
    "setApplications.*deprecated"
  )
})
