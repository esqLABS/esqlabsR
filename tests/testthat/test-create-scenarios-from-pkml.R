# Test for addScenarioFromPKML function

# Basic functionality tests
test_that("addScenarioFromPKML creates scenarios from PKML files", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(
    temp_project$config$modelFolder,
    "Aciclovir.pkml"
  )

  addScenarioFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration
  )

  scenarioName <- paste0(
    "Scenario",
    tools::file_path_sans_ext(basename(pkmlPath))
  )
  scenarios <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioName,
    projectConfiguration = projectConfiguration
  )
  expect_length(scenarios, 1)
  scenario <- scenarios[[1]]
  expect_equal(scenario$scenarioName, scenarioName)
  expect_equal(scenario$modelFile, "Aciclovir.pkml")
  expect_equal(scenario$applicationProtocol, "IV 250mg 10min")
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

test_that("addScenarioFromPKML extracts time unit from PKML file", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

  addScenarioFromPKML(
    pkmlFilePaths = file.path(
      temp_project$config$modelFolder,
      "Aciclovir.pkml"
    ),
    projectConfiguration = projectConfiguration
  )

  # Load scenarios and verify time unit was extracted

  scenarios <- readScenarioConfigurationFromExcel(
    scenarioNames = paste0(
      "Scenario",
      tools::file_path_sans_ext(basename("Aciclovir.pkml"))
    ),
    projectConfiguration = projectConfiguration
  )
  expect_equal(length(scenarios), 1)

  # The Aciclovir.pkml file has displayUnit="h" (hours) in its output schema
  expect_equal(scenarios[[1]]$simulationTimeUnit, "h")
})

test_that("addScenarioFromPKML uses provided time unit when specified", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config

  addScenarioFromPKML(
    pkmlFilePaths = file.path(
      temp_project$config$modelFolder,
      "Aciclovir.pkml"
    ),
    projectConfiguration = projectConfiguration,
    simulationTimeUnit = "min"
  )

  # Load scenarios and verify provided time unit was used
  scenarioName <- paste0(
    "Scenario",
    tools::file_path_sans_ext(basename("Aciclovir.pkml"))
  )
  scenarios <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioName,
    projectConfiguration = projectConfiguration
  )
  expect_equal(length(scenarios), 1)

  # Should use the provided time unit, not extract from PKML
  expect_equal(scenarios[[1]]$simulationTimeUnit, "min")
})

# Customization tests
test_that("addScenarioFromPKML works with custom scenario names", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(
    temp_project$config$modelFolder,
    "Aciclovir.pkml"
  )

  addScenarioFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    scenarioNames = "CustomScenario"
  )

  scenarios <- readScenarioConfigurationFromExcel(
    scenarioNames = "CustomScenario",
    projectConfiguration = projectConfiguration
  )
  expect_length(scenarios, 1)
  expect_equal(names(scenarios), "CustomScenario")
  expect_equal(scenarios[[1]]$scenarioName, "CustomScenario")
})

test_that("addScenarioFromPKML works with multiple PKML files", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath1 <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  pkmlPath2 <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")

  addScenarioFromPKML(
    pkmlFilePaths = c(pkmlPath1, pkmlPath2),
    projectConfiguration = projectConfiguration,
    scenarioNames = c("Scenario1", "Scenario2")
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

test_that("addScenarioFromPKML works with custom parameters", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(
    temp_project$config$modelFolder,
    "Aciclovir.pkml"
  )

  addScenarioFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    individualId = "TestIndividual",
    paramSheets = c("Global", "Custom"),
    steadyState = TRUE,
    steadyStateTime = 500
  )

  scenarioName <- paste0(
    "Scenario",
    tools::file_path_sans_ext(basename(pkmlPath))
  )
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

# Appending behavior tests
test_that("addScenarioFromPKML appends by default", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(
    temp_project$config$modelFolder,
    "Aciclovir.pkml"
  )

  original_scenarios_names <- names(readScenarioConfigurationFromExcel(
    projectConfiguration = projectConfiguration
  ))

  addScenarioFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    scenarioNames = "Scenario1"
  )
  addScenarioFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    scenarioNames = "Scenario2"
  )

  allScenarios <- readScenarioConfigurationFromExcel(
    projectConfiguration = projectConfiguration
  )
  expect_length(allScenarios, length(original_scenarios_names) + 2)
  expect_equal(
    names(allScenarios),
    c(original_scenarios_names, "Scenario1", "Scenario2")
  )
})

# Error handling tests
test_that("addScenarioFromPKML throws error for non-existent file", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  expect_error(
    addScenarioFromPKML(
      pkmlFilePaths = "non_existent_file.pkml",
      projectConfiguration = projectConfiguration
    ),
    "File not found"
  )
})

test_that("addScenarioFromPKML throws error for duplicate scenario names", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")

  addScenarioFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration,
    scenarioNames = "Scenario1"
  )
  expect_error(
    addScenarioFromPKML(
      pkmlFilePaths = pkmlPath,
      projectConfiguration = projectConfiguration,
      scenarioNames = "Scenario1"
    ),
    "Duplicate scenario names found"
  )
})

test_that("addScenarioFromPKML throws error for wrong scenario names length", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  expect_error(
    addScenarioFromPKML(
      pkmlFilePaths = pkmlPath,
      projectConfiguration = projectConfiguration,
      scenarioNames = c("Scenario1", "Scenario2")
    ),
    "scenarioNames must have the same length as pkmlFilePaths"
  )
})

test_that("addScenarioFromPKML throws error for wrong application protocols length", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  expect_error(
    addScenarioFromPKML(
      pkmlFilePaths = pkmlPath,
      projectConfiguration = projectConfiguration,
      applicationProtocols = c("Protocol1", "Protocol2")
    ),
    "applicationProtocols must have length 1 or same length as pkmlFilePaths"
  )
})

# Advanced functionality tests
test_that("addScenarioFromPKML creates protocol sheet if missing and scenario is runnable", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(
    temp_project$config$modelFolder,
    "Aciclovir.pkml"
  )

  # Remove all protocol sheets except the first to simulate missing protocol
  applicationsFile <- projectConfiguration$applicationsFile
  sheets <- readxl::excel_sheets(applicationsFile)
  firstSheet <- sheets[1]
  wb <- openxlsx::loadWorkbook(applicationsFile)
  for (sheet in sheets[-1]) {
    openxlsx::removeWorksheet(wb, sheet)
  }
  openxlsx::saveWorkbook(wb, applicationsFile, overwrite = TRUE)

  # Add scenario from PKML (should create new protocol sheet)
  addScenarioFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration
  )

  # Extract protocol name from PKML
  sim <- ospsuite::loadSimulation(pkmlPath)
  simTree <- ospsuite::getSimulationTree(sim)
  protocolName <- names(simTree$Applications)[1]

  # Check that the new protocol sheet was created
  newSheets <- readxl::excel_sheets(applicationsFile)
  expect_true(protocolName %in% newSheets)

  # Check that the new sheet has the same header as the first sheet
  headerFirst <- names(readxl::read_excel(
    applicationsFile,
    sheet = firstSheet,
    n_max = 0
  ))
  headerNew <- names(readxl::read_excel(
    applicationsFile,
    sheet = protocolName,
    n_max = 0
  ))
  expect_equal(headerFirst, headerNew)

  # Check that the scenario is runnable
  scenarioName <- paste0(
    "Scenario",
    tools::file_path_sans_ext(basename(pkmlPath))
  )
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioName,
    projectConfiguration = projectConfiguration
  )
  scenarios <- createScenarios(scenarioConfigurations)
  simulatedScenarios <- esqlabsR::runScenarios(scenarios)
  expect_length(simulatedScenarios, 1)
  expect_equal(names(simulatedScenarios), scenarioName)
  expect_true(!is.null(simulatedScenarios[[scenarioName]]$results))
  expect_true(
    length(simulatedScenarios[[scenarioName]]$results$allQuantityPaths) > 0
  )
})

test_that("addScenarioFromPKML creates runnable scenarios", {
  temp_project <- with_temp_project()
  projectConfiguration <- temp_project$config
  pkmlPath <- file.path(
    temp_project$config$modelFolder,
    "Aciclovir.pkml"
  )

  addScenarioFromPKML(
    pkmlFilePaths = pkmlPath,
    projectConfiguration = projectConfiguration
  )

  scenarioName <- paste0(
    "Scenario",
    tools::file_path_sans_ext(basename(pkmlPath))
  )
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames = scenarioName,
    projectConfiguration = projectConfiguration
  )
  scenarios <- createScenarios(scenarioConfigurations)

  # Verify scenarios were created successfully
  expect_length(scenarios, 1)
  expect_equal(names(scenarios), scenarioName)

  # Run the scenarios
  simulatedScenarios <- esqlabsR::runScenarios(scenarios)

  # Verify scenarios ran successfully
  expect_length(simulatedScenarios, 1)
  expect_equal(names(simulatedScenarios), scenarioName)
  expect_true(!is.null(simulatedScenarios[[scenarioName]]$results))
  expect_true(
    length(simulatedScenarios[[scenarioName]]$results$allQuantityPaths) > 0
  )
})
