# createPlots aborts on unknown plotGridNames when stopIfNotFound

    Code
      createPlots(project, plotGridNames = "DoesNotExist", simulatedScenarios = simulated,
        validate = FALSE, stopIfNotFound = TRUE)
    Condition
      Error in `createPlots()`:
      ! The following plot grids are not defined in the project: "DoesNotExist"

