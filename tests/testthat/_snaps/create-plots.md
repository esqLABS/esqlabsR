# createPlots aborts on unknown plotGrids when stopIfNotFound

    Code
      createPlots(project, plotGrids = "DoesNotExist", scenarioResults = simulated,
        validate = FALSE, stopIfNotFound = TRUE)
    Condition
      Error in `createPlots()`:
      ! The following plot grids are not defined in the project: "DoesNotExist"

# createPlots(plots) aborts on an unknown plotId when stopIfNotFound

    Code
      createPlots(project, plots = "ghost_plot", scenarioResults = simulated,
        validate = FALSE, stopIfNotFound = TRUE)
    Condition
      Error in `createPlots()`:
      ! The following plots are not defined in the project: "ghost_plot"

