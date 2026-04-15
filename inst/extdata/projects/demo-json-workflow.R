# =============================================================================
# esqlabsR: JSON-First Workflow Demo
# =============================================================================
#
# This script demonstrates the new JSON-driven workflow where the entire
# project — scenarios, observed data, plots — is described in a single
# ProjectConfiguration.json file.
#
# Key improvement: observed data sources are declared in the JSON.
# `createDataCombined()` and `createPlots()` load them automatically —
# no manual loading step, no parameter threading.

library(esqlabsR)

# ---------------------------------------------------------------------------
# 1. Load the project
# ---------------------------------------------------------------------------
# A single JSON file describes everything: folder paths, scenarios,
# individuals, populations, observed data sources, and plot definitions.

projectConfigPath <- system.file(
  "extdata", "projects", "Example", "ProjectConfiguration.json",
  package = "esqlabsR"
)

pc <- loadProject(projectConfigPath)
print(pc)

# The observed data section in the JSON looks like:
#
#   "observedData": [
#     { "type": "excel", "sheets": ["Laskin 1982.Group A"] }
#   ]
#
# It can also specify custom file/importer overrides or PKML sources:
#
#   { "type": "excel", "file": "OtherStudy.xlsx",
#     "importerConfiguration": "other_importer.xml",
#     "sheets": ["Sheet1", "Sheet2"] }
#
#   { "type": "pkml", "file": "pkml/SomeDataSet.pkml" }

# ---------------------------------------------------------------------------
# 2. Run scenarios
# ---------------------------------------------------------------------------
# Scenarios are defined in the JSON. Run one, some, or all of them.

simulatedScenarios <- runScenarios(pc, scenarioNames = "TestScenario")

# The result is a named list. Each entry contains:
#   $simulation    — the initialized Simulation object
#   $results       — SimulationResults
#   $outputValues  — extracted output values
cat("Simulated scenarios:", paste(names(simulatedScenarios), collapse = ", "), "\n")

# ---------------------------------------------------------------------------
# 3. Create DataCombined objects
# ---------------------------------------------------------------------------
# This is where the magic happens. The old workflow required:
#
#   importerConfig <- ospsuite::loadDataImporterConfiguration(...)
#   observedData   <- loadObservedData(pc, sheets = ..., importerConfiguration = ...)
#   dcList         <- createDataCombined(pc, ..., observedData = observedData)
#
# Now it's just:

dcList <- createDataCombined(
  projectConfiguration = pc,
  dataCombinedNames = "AciclovirPVB",
  simulatedScenarios = simulatedScenarios
)

cat("DataCombined objects:", paste(names(dcList), collapse = ", "), "\n")

# ---------------------------------------------------------------------------
# 4. Create plots
# ---------------------------------------------------------------------------
# Same simplification — no observedData parameter, and projectConfiguration
# is now the first argument.

plots <- createPlots(
  projectConfiguration = pc,
  plotGridNames = "Aciclovir",
  simulatedScenarios = simulatedScenarios
)

cat("Plot grids created:", paste(names(plots), collapse = ", "), "\n")

# Display the first plot
print(plots[[1]])

# ---------------------------------------------------------------------------
# 5. Create all plots at once
# ---------------------------------------------------------------------------
# Pass NULL (or omit) plotGridNames to create every plot grid defined in JSON.
# Use stopIfNotFound = FALSE to skip plots whose scenarios weren't simulated.

allPlots <- createPlots(
  projectConfiguration = pc,
  simulatedScenarios = simulatedScenarios,
  stopIfNotFound = FALSE
)

cat("All plot grids:", paste(names(allPlots), collapse = ", "), "\n")

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
# The full workflow is now 3 lines:
#
#   pc      <- loadProject("ProjectConfiguration.json")
#   results <- runScenarios(pc, scenarioNames = "TestScenario")
#   plots   <- createPlots(pc, simulatedScenarios = results)
#
# Everything else — observed data loading, data combination, plot
# configuration — is driven by the JSON.
