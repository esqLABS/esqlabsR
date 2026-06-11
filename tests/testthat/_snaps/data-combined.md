# createDataCombined reports a failed scenario run distinctly

    Code
      createDataCombined(project, dataCombinedNames = "DC_failed",
        simulatedScenarios = failedRun)
    Condition
      Error in `FUN()`:
      ! The DataCombined "DC_failed" references the output path 'Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)' of scenario <TestScenario>, but that scenario produced no results. Re-run the scenario and check that it completed successfully.

