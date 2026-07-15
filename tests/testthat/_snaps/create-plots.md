# .assertPlotGridsBuildable aborts on a grid missing its plotGridId

    Code
      esqlabsR:::.assertPlotGridsBuildable(list(list(plotIds = "p1")), plotIDs = "p1")
    Condition
      Error in `esqlabsR:::.assertPlotGridsBuildable()`:
      ! Every plot grid must declare a `plotGridId`.

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

# createPlots aborts on an unknown plotType even when validate = FALSE

    Code
      suppressWarnings(createPlots(project, plots = "p_bad", scenarioResults = simulated,
        validate = FALSE))
    Condition
      Error in `.assertPlotConfigurationsBuildable()`:
      ! Invalid plotType "timeprofile" for plot "p_bad".
      i Must be one of: "individual", "population", "observedVsSimulated", "residualsVsSimulated", and "residualsVsTime".

