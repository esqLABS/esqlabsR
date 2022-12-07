projectConfiguration <- createDefaultProjectConfiguration(file.path("..", "data", "ProjectConfiguration_forTests.xlsx"))

# Define which scenarios to run
scenarioNames <- c("TestScenario")
outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

# Create `ScenarioConfiguration` objects from excel files
scenarioConfigurations <- readScenarioConfigurationFromExcel(
  scenarioNames = scenarioNames,
  projectConfiguration = projectConfiguration
)

# Set output paths for each scenario
for (scenarioConfiguration in scenarioConfigurations) {
  scenarioConfiguration$outputPaths <- outputPaths
}

# Run scenarios
simulatedScenarios <- runScenarios(
  scenarioConfigurations = scenarioConfigurations,
  customParams = NULL, saveSimulationsToPKML = FALSE
)

########### Load observed data - data template v10########
# Which sheets to load
dataSheets <- "Laskin 1982.Group A"
observedData <- esqlabsR::loadObservedData(
  projectConfiguration = projectConfiguration,
  sheets = dataSheets
)

test_that("It returns NULL if no DataCombined are defined in the excel sheet", {
plots <- createPlotsFromExcel(simulatedScenarios = simulatedScenarios,
observedData = observedData,
projectConfiguration = projectConfiguration,
stopIfNotFound = TRUE)
expect_null(plots)
}
)
