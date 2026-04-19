# This file is run before all tests (inside each worker when parallel)
options(ospsuite.plots.watermarkEnabled = FALSE)

# Shared sensitivity-calculation inputs ---------------------------------------
# These are the inputs used by four test files (test-sensitivity-calculation,
# test-sensitivity-spider-plot, test-sensitivity-time-profiles,
# test-utilities-sensitivity-calculation). Computing them once here saves
# three full runs per worker.
.testFixtures$aciclovirSaOutputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
.testFixtures$aciclovirSaParameterPaths <- c(
  "Aciclovir|Lipophilicity",
  "Events|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
  "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR-Aciclovir|GFR fraction"
)
.testFixtures$aciclovirSaVariationRange <- c(0.1, 2, 20)

set.seed(123)
.testFixtures$aciclovirSaResults <- sensitivityCalculation(
  simulation = aciclovirSim(),
  outputPaths = .testFixtures$aciclovirSaOutputPath,
  parameterPaths = .testFixtures$aciclovirSaParameterPaths,
  variationRange = .testFixtures$aciclovirSaVariationRange
)

# Shared TestProject scenarios ------------------------------------------------
# Pre-run TestScenario + PopulationScenario so plot/data-combined tests don't
# re-simulate them.
.testFixtures$testProjectConfiguration <- testProjectConfiguration()

.testScenarioConfigurations <- readScenarioConfigurationFromExcel(
  scenarioNames = c("TestScenario", "PopulationScenario"),
  projectConfiguration = .testFixtures$testProjectConfiguration
)
for (.sc in .testScenarioConfigurations) {
  .sc$outputPaths <- .testFixtures$aciclovirSaOutputPath
}
.testScenarios <- createScenarios(scenarioConfigurations = .testScenarioConfigurations)
.testFixtures$testSimulatedScenarios <- runScenarios(scenarios = .testScenarios)

# Shared observed data used by plotting + DataCombined tests
.testFixtures$aciclovirObsData <- esqlabsR::loadObservedData(
  projectConfiguration = .testFixtures$testProjectConfiguration,
  sheets = "Laskin 1982.Group A",
  importerConfiguration = ospsuite::loadDataImporterConfiguration(
    configurationFilePath = .testFixtures$testProjectConfiguration$dataImporterConfigurationFile
  )
)

# Reusable DataCombined fixtures ---------------------------------------------
source(testthat::test_path("create-data-combined-objects.R"), local = TRUE)
