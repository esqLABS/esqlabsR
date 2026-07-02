# createDataCombined reports a failed scenario run distinctly

    Code
      createDataCombined(project, dataCombined = "dc_failed", scenarioResults = failedRun)
    Condition
      Error in `.validateDataCombinedFromExcel()`:
      ! The following scenarios are not present in `scenarioResults`: "testscenario". Data cannot be added to `DataCombined` object.

