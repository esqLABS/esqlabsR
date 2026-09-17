# createDataCombined errors when a requested plotGrids name is unknown

    Code
      createDataCombined(project, plotGrids = "DoesNotExist")
    Condition
      Error in `createDataCombined()`:
      ! The following plot grids are not defined in the project: "DoesNotExist"

# createDataCombined reports a failed scenario run distinctly

    Code
      createDataCombined(project, dataCombined = "dc_failed", scenarioResults = failedRun)
    Condition
      Error in `createDataCombined()`:
      ! The DataCombined "dc_failed" references the output path 'Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)' of scenario <testscenario>, but that scenario produced no results. Re-run the scenario and check that it completed successfully.

