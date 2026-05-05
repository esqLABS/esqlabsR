# ScenarioData has the documented field defaults

    list(applicationProtocol = NULL, individualId = NULL, modelFile = NULL, 
        modelParameterSets = NULL, outputPaths = NULL, overwriteFormulasInSS = FALSE, 
        populationId = NULL, readPopulationFromCSV = FALSE, scenarioName = NULL, 
        simulateSteadyState = FALSE, simulationTime = NULL, simulationTimeUnit = NULL, 
        simulationType = "Individual", steadyStateTime = 1000, steadyStateTimeUnit = NULL)

# .parseScenarios copies basic fields for an individual scenario

    list(applicationProtocol = "Aciclovir_iv_250mg", individualId = "Adult_male", 
        modelFile = "Aciclovir.pkml", modelParameterSets = c("Global", 
        "Aciclovir"), outputPaths = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)", 
        overwriteFormulasInSS = FALSE, populationId = NULL, readPopulationFromCSV = FALSE, 
        scenarioName = "Aciclovir_iv", simulateSteadyState = FALSE, 
        simulationTime = list(c(0, 24, 60)), simulationTimeUnit = "h", 
        simulationType = "Individual", steadyStateTime = 1000, steadyStateTimeUnit = NULL)

