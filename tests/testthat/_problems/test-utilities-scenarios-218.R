# Extracted from test-utilities-scenarios.R:218

# prequel ----------------------------------------------------------------------
defaultOutputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

# test -------------------------------------------------------------------------
pc <- testProjectConfigurationJSON()
dosePath <- "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose"
results <- runScenarios(pc, scenarioNames = "TestScenario")
sim <- results$TestScenario$simulation
doseParam <- ospsuite::getParameter(dosePath, sim)
