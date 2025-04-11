projectConfiguration <- testProjectConfiguration()

# Define which scenarios to run
scenarioNames <- c("TestScenario", "PopulationScenario")
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
scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations)

simulatedScenarios <- runScenarios(
  scenarios = scenarios
)

importerConfiguration <- ospsuite::loadDataImporterConfiguration(
  configurationFilePath = projectConfiguration$dataImporterConfigurationFile
)

# Load observed data
dataSheets <- "Laskin 1982.Group A"
observedData <- esqlabsR::loadObservedData(
  projectConfiguration = projectConfiguration,
  sheets = dataSheets,
  importerConfiguration = importerConfiguration
)

dataCombinedDf <- data.frame(list(
  "DataCombinedName" = c("AciclovirPVB", "AciclovirPVB", "DC_missingPath", "DC_missingPath"),
  "dataType" = c("simulated", "observed", "simulated", "observed"),
  "label" = c("Aciclovir simulated", "Aciclovir observed", "Aciclovir simulated", "Aciclovir observed"),
  "scenario" = c(scenarioNames[1], NA, scenarioNames[1], NA),
  "path" = c(outputPaths, NA, NA, NA),
  "dataSet" = c(NA, names(observedData), NA, names(observedData)),
  "group" = c("Aciclovir PVB", "Aciclovir PVB"),
  "xOffsets" = c(NA, NA),
  "xOffsetsUnits" = c(NA, NA),
  "yOffsets" = c(NA, NA),
  "yOffsetsUnits" = c(NA, NA),
  "xScaleFactors" = c(NA, NA),
  "yScaleFactors" = c(NA, NA)
))

test_that("It returns correct names of data combined when a path is not specified for one simulated scenario", {
  expect_error(.validateDataCombinedFromExcel(dataCombinedDf, observedData), regexp = messages$stopNoPathProvided("DC_missingPath"))
})
