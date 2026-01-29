# readPITaskConfigurationFromExcel creates correct PI task configuration

    list(PITaskName = "AciclovirLinear", Algorithm = "BOBYQA", CIMethod = "hessian", 
        PrintEvaluationFeedback = TRUE, AutoEstimateCI = FALSE, SimulationRunOptions = NA_character_, 
        ObjectiveFunctionOptions = NA_character_, algorithmOptions = list(
            maxeval = 1000, ftol_rel = 0), ciOptions = list(confLevel = 0.95))

---

    list(PITaskName = "AciclovirLinear", Scenario = "PITestScenario", 
        "Container Path" = "Aciclovir", "Parameter Name" = "Lipophilicity", 
        Value = -0.1, Units = "Log Units", MinValue = -10, MaxValue = 10, 
        StartValue = 1, Group = "1")

---

    list(PITaskName = "AciclovirLinear", Scenario = "PITestScenario", 
        ObservedDataSheet = "Laskin 1982.Group A", DataSet = "Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv_", 
        Scaling = "lin", xOffset = NA_real_, yOffset = NA_real_, 
        Weight = NA_real_)

