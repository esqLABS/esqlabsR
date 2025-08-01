# Test for createScenarioConfigurationsFromPKML and addScenarioConfigurationsToExcel functions

# Basic functionality tests
test_that("createScenarioConfigurationsFromPKML and addScenarioConfigurationsToExcel create scenarios from PKML files", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(
    temp_project$config$modelFolder,
    "Aciclovir.pkml"
  )

  # Create scenarios from PKML
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration
  )

  # Add scenarios to configuration
  addScenarioConfigurationsToExcel(
    scenarioConfigurations = scenarioConfigurations,
    projectConfiguration = projectConfiguration
  )

  scenarioName <- "Vergin 1995 IV"
  scenarios <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioName,
    projectConfiguration = projectConfiguration
  )
  expect_length(scenarios, 1)
  scenario <- scenarios[[1]]
  expect_equal(scenario$scenarioName, scenarioName)
  expect_equal(scenario$modelFile, "Aciclovir.pkml")
  expect_equal(scenario$applicationProtocol, scenarioName)
  # The Aciclovir.pkml file has displayUnit="h" (hours) in its output schema
  expect_equal(scenario$simulationTimeUnit, "h")
  expect_false(scenario$simulateSteadyState)
  expect_false(scenario$readPopulationFromCSV)
  expect_true(length(scenario$outputPaths) > 0)
  expect_true(
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)" %in%
      scenario$outputPaths
  )
  expect_true(!is.null(scenario$simulationTime))
})

test_that("createScenarioConfigurationsFromPKML extracts time unit from PKML file", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

  # Create scenarios from PKML
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = file.path(
      temp_project$config$modelFolder,
      "Aciclovir.pkml"
    ),
    projectConfiguration = projectConfiguration
  )

  # Add scenarios to configuration
  addScenarioConfigurationsToExcel(
    scenarioConfigurations = scenarioConfigurations,
    projectConfiguration = projectConfiguration
  )

  # Load scenarios and verify time unit was extracted
  scenarioName <- "Vergin 1995 IV"
  scenarios <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioName,
    projectConfiguration = projectConfiguration
  )
  expect_length(scenarios, 1)
  expect_equal(scenarios[[1]]$simulationTimeUnit, "h")
})

test_that("createScenarioConfigurationsFromPKML uses provided time unit when specified", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

  # Create scenarios from PKML with custom time unit
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = file.path(
      temp_project$config$modelFolder,
      "Aciclovir.pkml"
    ),
    projectConfiguration = projectConfiguration,
    simulationTimeUnit = "min"
  )

  # Add scenarios to configuration
  addScenarioConfigurationsToExcel(
    scenarioConfigurations = scenarioConfigurations,
    projectConfiguration = projectConfiguration
  )

  # Load scenarios and verify custom time unit was used
  scenarioName <- "Vergin 1995 IV"
  scenarios <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioName,
    projectConfiguration = projectConfiguration
  )
  expect_length(scenarios, 1)
  expect_equal(scenarios[[1]]$simulationTimeUnit, "min")
})

test_that("createScenarioConfigurationsFromPKML works with custom scenario names", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

  # Create scenarios from PKML with custom name
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = file.path(
      temp_project$config$modelFolder,
      "Aciclovir.pkml"
    ),
    projectConfiguration = projectConfiguration,
    scenarioNames = "CustomScenario"
  )

  # Add scenarios to configuration
  addScenarioConfigurationsToExcel(
    scenarioConfigurations = scenarioConfigurations,
    projectConfiguration = projectConfiguration
  )

  scenarios <- readScenarioConfigurationFromExcel(
    scenarioNames = "CustomScenario",
    projectConfiguration = projectConfiguration
  )
  expect_length(scenarios, 1)
  expect_equal(names(scenarios), "CustomScenario")
  expect_equal(scenarios[[1]]$scenarioName, "CustomScenario")
})

test_that("createScenarioConfigurationsFromPKML works with multiple PKML files", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

  # Create scenarios from multiple PKML files
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = c(
      file.path(temp_project$config$modelFolder, "Aciclovir.pkml"),
      file.path(temp_project$config$modelFolder, "Aciclovir.pkml")
    ),
    projectConfiguration = projectConfiguration,
    scenarioNames = c("Scenario1", "Scenario2")
  )

  # Add scenarios to configuration
  addScenarioConfigurationsToExcel(
    scenarioConfigurations = scenarioConfigurations,
    projectConfiguration = projectConfiguration
  )

  scenarios <- readScenarioConfigurationFromExcel(
    scenarioNames = c("Scenario1", "Scenario2"),
    projectConfiguration = projectConfiguration
  )
  expect_length(scenarios, 2)
  expect_equal(names(scenarios), c("Scenario1", "Scenario2"))
  expect_equal(scenarios[[1]]$scenarioName, "Scenario1")
  expect_equal(scenarios[[2]]$scenarioName, "Scenario2")
})

test_that("createScenarioConfigurationsFromPKML works with custom parameters", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

  # Create scenarios from PKML with custom parameters
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = file.path(
      temp_project$config$modelFolder,
      "Aciclovir.pkml"
    ),
    projectConfiguration = projectConfiguration,
    individualId = "TestIndividual",
    paramSheets = c("Global", "Custom"),
    steadyState = TRUE,
    steadyStateTime = 500,
    steadyStateTimeUnit = "min"
  )

  # Add scenarios to configuration
  addScenarioConfigurationsToExcel(
    scenarioConfigurations = scenarioConfigurations,
    projectConfiguration = projectConfiguration
  )

  scenarioName <- "Vergin 1995 IV"
  scenarios <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioName,
    projectConfiguration = projectConfiguration
  )
  scenario <- scenarios[[1]]
  expect_equal(scenario$individualId, "TestIndividual")
  expect_equal(enumKeys(scenario$paramSheets), c("Global", "Custom"))
  expect_true(scenario$simulateSteadyState)
  expect_equal(
    scenario$steadyStateTime,
    ospsuite::toBaseUnit(
      quantityOrDimension = ospsuite::ospDimensions$Time,
      values = 500,
      unit = "min"
    )
  )
})

test_that("createScenarioConfigurationsFromPKML uses NULL defaults for steady state parameters", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

  # Create scenarios with default parameters (steadyState = FALSE, NULL times)
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = file.path(
      temp_project$config$modelFolder,
      "Aciclovir.pkml"
    ),
    projectConfiguration = projectConfiguration
  )

  # Check that steady state is disabled by default
  scenarioName <- names(scenarioConfigurations)[1]
  scenario <- scenarioConfigurations[[1]]
  expect_false(scenario$simulateSteadyState)

  # Test steady state enabled but no time provided (should still work)
  scenarioConfigurations2 <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = file.path(
      temp_project$config$modelFolder,
      "Aciclovir.pkml"
    ),
    projectConfiguration = projectConfiguration,
    scenarioNames = "TestSteadyStateNullTime",
    steadyState = TRUE
    # steadyStateTime and steadyStateTimeUnit are NULL by default
  )

  scenario2 <- scenarioConfigurations2[[1]]
  expect_true(scenario2$simulateSteadyState)
  # steadyStateTime should remain NULL when not provided
})

test_that("addScenarioConfigurationsToExcel appends by default", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

  # First, create some existing scenarios
  existingScenarios <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = c(
      file.path(temp_project$config$modelFolder, "Aciclovir.pkml"),
      file.path(temp_project$config$modelFolder, "Aciclovir.pkml")
    ),
    projectConfiguration = projectConfiguration,
    scenarioNames = c("ExistingScenario1", "ExistingScenario2")
  )
  addScenarioConfigurationsToExcel(
    scenarioConfigurations = existingScenarios,
    projectConfiguration = projectConfiguration
  )
  original_scenarios_names <- names(existingScenarios)

  # Then add new scenarios
  newScenarios <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = c(
      file.path(temp_project$config$modelFolder, "Aciclovir.pkml"),
      file.path(temp_project$config$modelFolder, "Aciclovir.pkml")
    ),
    projectConfiguration = projectConfiguration,
    scenarioNames = c("Scenario1", "Scenario2")
  )
  addScenarioConfigurationsToExcel(
    scenarioConfigurations = newScenarios,
    projectConfiguration = projectConfiguration,
    appendToExisting = TRUE
  )

  # Verify all scenarios exist
  allScenarios <- readScenarioConfigurationFromExcel(
    scenarioNames = c(original_scenarios_names, "Scenario1", "Scenario2"),
    projectConfiguration = projectConfiguration
  )
  expect_length(allScenarios, 4)
  expect_equal(
    names(allScenarios),
    c(original_scenarios_names, "Scenario1", "Scenario2")
  )
})

test_that("createScenarioConfigurationsFromPKML throws error for non-existent file", {
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

test_that("addScenarioConfigurationsToExcel throws error for duplicate scenario names", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # First add a scenario
  firstScenarios <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    scenarioNames = "Scenario1"
  )
  addScenarioConfigurationsToExcel(
    scenarioConfigurations = firstScenarios,
    projectConfiguration = projectConfiguration
  )

  # Try to add another scenario with the same name
  duplicateScenarios <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    scenarioNames = "Scenario1"
  )

  expect_error(
    addScenarioConfigurationsToExcel(
      scenarioConfigurations = duplicateScenarios,
      projectConfiguration = projectConfiguration
    ),
    "Duplicate scenario names found"
  )
})

test_that("createScenarioConfigurationsFromPKML throws error for wrong scenario names length", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  expect_error(
    createScenarioConfigurationsFromPKML(
      pkmlFilePaths = pkmlPath,
      projectConfiguration = projectConfiguration,
      scenarioNames = c("Scenario1", "Scenario2")
    ),
    "Invalid argument lengths"
  )
})

test_that("createScenarioConfigurationsFromPKML throws error for wrong application protocols length", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  expect_error(
    createScenarioConfigurationsFromPKML(
      pkmlFilePaths = pkmlPath,
      projectConfiguration = projectConfiguration,
      applicationProtocols = c("Protocol1", "Protocol2")
    ),
    "Invalid applicationProtocols length"
  )
})

test_that("createScenarioConfigurationsFromPKML handles duplicate scenario names by adding indices", {
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
    c("DuplicateName_1", "DuplicateName_2", "UniqueName")
  )
  expect_equal(
    scenarioConfigurations[["DuplicateName_1"]]$scenarioName,
    "DuplicateName_1"
  )
  expect_equal(
    scenarioConfigurations[["DuplicateName_2"]]$scenarioName,
    "DuplicateName_2"
  )
  expect_equal(
    scenarioConfigurations[["UniqueName"]]$scenarioName,
    "UniqueName"
  )

  # Test with multiple different duplicate names
  expect_warning(
    scenarioConfigurations2 <- createScenarioConfigurationsFromPKML(
      pkmlFilePaths = c(pkmlPath, pkmlPath, pkmlPath, pkmlPath),
      projectConfiguration = projectConfiguration,
      scenarioNames = c("Name1", "Name1", "Name2", "Name2")
    ),
    "Duplicate scenario names found and made unique by adding indices"
  )

  # Check that all names were made unique
  expect_equal(
    names(scenarioConfigurations2),
    c("Name1_1", "Name1_2", "Name2_1", "Name2_2")
  )
})

test_that("addScenarioConfigurationsToExcel creates protocol sheet if missing and scenario is runnable", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Create scenarios from PKML
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration
  )

  # Add scenarios to configuration
  addScenarioConfigurationsToExcel(
    scenarioConfigurations = scenarioConfigurations,
    projectConfiguration = projectConfiguration
  )

  # Protocol name should now be the scenario name, not extracted from PKML
  scenarioName <- "Vergin 1995 IV"
  protocolName <- scenarioName

  # Check that the new protocol sheet was created
  expect_true(
    protocolName %in%
      readxl::excel_sheets(
        projectConfiguration$applicationsFile
      )
  )

  # Read headers to verify sheet structure
  headerFirst <- readxl::read_excel(
    projectConfiguration$applicationsFile,
    sheet = readxl::excel_sheets(projectConfiguration$applicationsFile)[1],
    n_max = 0
  )
  headerNew <- readxl::read_excel(
    projectConfiguration$applicationsFile,
    sheet = protocolName,
    n_max = 0
  )

  expect_true(
    protocolName %in%
      readxl::excel_sheets(
        projectConfiguration$applicationsFile
      )
  )
  expect_equal(headerFirst, headerNew)

  # Test that the scenario is runnable
  scenarioName <- "Vergin 1995 IV"
  scenarios <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioName,
    projectConfiguration = projectConfiguration
  )
  expect_length(scenarios, 1)
  expect_equal(names(scenarios), scenarioName)

  # Run the scenario to ensure it's properly configured
  runableScenarios <- createScenarios(scenarioConfigurations = scenarios)
  simulatedScenarios <- runScenarios(scenarios = runableScenarios)
  expect_length(simulatedScenarios, 1)
  expect_equal(names(simulatedScenarios), scenarioName)
  expect_true(!is.null(simulatedScenarios[[scenarioName]]$results))
  expect_true(
    length(simulatedScenarios[[scenarioName]]$results$allQuantityPaths) > 0
  )
})

test_that("createScenarioConfigurationsFromPKML and addScenarioConfigurationsToExcel create runnable scenarios", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Create scenarios from PKML
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration
  )

  # Add scenarios to configuration
  addScenarioConfigurationsToExcel(
    scenarioConfigurations = scenarioConfigurations,
    projectConfiguration = projectConfiguration
  )

  # Read back the scenarios and verify they can be run
  scenarioName <- "Vergin 1995 IV"
  scenarios <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioName,
    projectConfiguration = projectConfiguration
  )
  expect_length(scenarios, 1)
  expect_equal(names(scenarios), scenarioName)

  # Create runnable scenarios and simulate
  runableScenarios <- createScenarios(scenarioConfigurations = scenarios)
  simulatedScenarios <- runScenarios(scenarios = runableScenarios)
  expect_length(simulatedScenarios, 1)
  expect_equal(names(simulatedScenarios), scenarioName)
  expect_true(!is.null(simulatedScenarios[[scenarioName]]$results))
  expect_true(
    length(simulatedScenarios[[scenarioName]]$results$allQuantityPaths) > 0
  )
})

# Tests for new protocol extraction functionality
test_that("createScenarioConfigurationsFromPKML defaults protocol name to scenario name", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Create scenarios with custom scenario name
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    scenarioNames = "MyCustomScenario"
  )

  # Check that protocol name matches scenario name
  scenario <- scenarioConfigurations[["MyCustomScenario"]]
  expect_equal(scenario$applicationProtocol, "MyCustomScenario")
})

test_that("createScenarioConfigurationsFromPKML has no side effects on Excel files", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Record initial state of Applications file
  applicationsFile <- projectConfiguration$applicationsFile
  initialExists <- file.exists(applicationsFile)
  initialSheets <- character(0)
  if (initialExists) {
    initialSheets <- readxl::excel_sheets(applicationsFile)
  }

  # Create scenarios from PKML
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
    # Specifically check that our scenario sheet was NOT created
    expect_false("TestNoSideEffects" %in% finalSheets)
  }
})

test_that("addScenarioConfigurationsToExcel extracts and writes application parameters to Excel", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Create scenarios from PKML (should have no side effects)
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    scenarioNames = "TestParameterExtraction"
  )

  # Check that the scenario configuration was created
  expect_length(scenarioConfigurations, 1)
  scenario <- scenarioConfigurations[["TestParameterExtraction"]]
  expect_equal(scenario$applicationProtocol, "TestParameterExtraction")

  # At this point, no Excel files should be created or modified
  applicationsFile <- projectConfiguration$applicationsFile
  if (file.exists(applicationsFile)) {
    sheets <- readxl::excel_sheets(applicationsFile)
    expect_false("TestParameterExtraction" %in% sheets)
  }

  # Now add scenarios to Excel - this should extract and write parameters
  addScenarioConfigurationsToExcel(
    scenarioConfigurations = scenarioConfigurations,
    projectConfiguration = projectConfiguration
  )

  # Check that Applications.xlsx file has the new sheet with parameters
  expect_true(file.exists(applicationsFile))

  sheets <- readxl::excel_sheets(applicationsFile)
  expect_true("TestParameterExtraction" %in% sheets)

  # Read the parameters from the new sheet
  params <- readxl::read_excel(
    applicationsFile,
    sheet = "TestParameterExtraction"
  )

  # Check that it has the correct structure
  expected_columns <- c("Container Path", "Parameter Name", "Value", "Units")
  expect_true(all(expected_columns %in% colnames(params)))

  # Check that we have some parameters (should have constant application parameters)
  expect_true(nrow(params) > 0)

  # Check that excluded parameters like "Volume" and "Application rate" are not present
  excluded_params <- c("Volume", "Application rate")
  expect_false(any(params$`Parameter Name` %in% excluded_params))
})

test_that("addScenarioConfigurationsToExcel extracts both Applications and Events parameters", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Load simulation to check what parameters exist
  simulation <- ospsuite::loadSimulation(pkmlPath)

  # Get parameters from both Applications and Events nodes
  applicationsParams <- ospsuite::getAllParametersMatching(
    "Applications|**",
    simulation
  )
  eventsParams <- ospsuite::getAllParametersMatching("Events|**", simulation)

  # Check that we have some parameters in at least one of the locations
  expect_true(length(applicationsParams) > 0 || length(eventsParams) > 0)

  # Create scenarios from PKML
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    scenarioNames = "TestBothNodes"
  )

  # Add scenarios to Excel - this should extract and write parameters
  addScenarioConfigurationsToExcel(
    scenarioConfigurations = scenarioConfigurations,
    projectConfiguration = projectConfiguration
  )

  # Check that parameters were written to Excel
  applicationsFile <- projectConfiguration$applicationsFile
  expect_true(file.exists(applicationsFile))

  sheets <- readxl::excel_sheets(applicationsFile)
  expect_true("TestBothNodes" %in% sheets)

  # Read the parameters
  params <- readxl::read_excel(applicationsFile, sheet = "TestBothNodes")
  expect_true(nrow(params) > 0)
})

test_that("createScenarioConfigurationsFromPKML respects provided applicationProtocols parameter", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Create scenarios with explicit application protocol
  scenarioConfigurations <- createScenarioConfigurationsFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    scenarioNames = "TestExplicitProtocol",
    applicationProtocols = "CustomProtocolName"
  )

  # Check that the explicit protocol name was used
  scenario <- scenarioConfigurations[["TestExplicitProtocol"]]
  expect_equal(scenario$applicationProtocol, "CustomProtocolName")
})

test_that("addScenarioConfigurationsToExcel doesn't overwrite existing application sheets", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # First, manually create a sheet with some data
  applicationsFile <- projectConfiguration$applicationsFile

  # Create initial data
  initialData <- data.frame(
    "Container Path" = "Test|Container",
    "Parameter Name" = "TestParam",
    "Value" = 999,
    "Units" = "mg",
    check.names = FALSE
  )

  # Write to Excel with a specific sheet name
  protocolName <- "TestNoOverwrite"
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, protocolName)
  openxlsx::writeData(wb, protocolName, initialData)
  openxlsx::saveWorkbook(wb, applicationsFile, overwrite = TRUE)

  # Verify initial data
  initialParams <- readxl::read_excel(applicationsFile, sheet = protocolName)
  expect_equal(nrow(initialParams), 1)
  expect_equal(initialParams$`Parameter Name`[1], "TestParam")

  # Now create scenarios with the same protocol name
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

test_that("addScenarioConfigurationsToExcel filters constant parameters correctly", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(temp_project$config$modelFolder, "Aciclovir.pkml")

  # Load simulation and check parameter types
  simulation <- ospsuite::loadSimulation(pkmlPath)
  allParams <- c(
    ospsuite::getAllParametersMatching("Applications|**", simulation),
    ospsuite::getAllParametersMatching("Events|**", simulation)
  )

  # Find constant and non-constant parameters
  constantParams <- Filter(function(p) p$isConstant, allParams)
  nonConstantParams <- Filter(function(p) !p$isConstant, allParams)

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
