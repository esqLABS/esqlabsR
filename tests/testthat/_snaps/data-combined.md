# createDataCombined errors when dataCombinedNames is not a string

    Code
      createDataCombined(project, dataCombinedNames = 123)
    Condition
      Error in `createDataCombined()`:
      ! The following DataCombined names are not defined in the Excel file: "123"

# createDataCombined reports a failed scenario run distinctly

    Code
      createDataCombined(project, dataCombinedNames = "DC_failed",
        simulatedScenarios = failedRun)
    Condition
      Error in `FUN()`:
      ! Output path 'Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)' is defined in the DataCombined "DC_failed" for scenario <TestScenario> but has not been simulated. Please check that the output path is specified for this scenario.

