# .assertPlotGridsBuildable aborts on a grid missing its plotGridId

    Code
      .assertPlotGridsBuildable(list(list(plotIds = "p1")), plotIDs = "p1")
    Condition
      Error in `.assertPlotGridsBuildable()`:
      ! Every plot grid must declare a `plotGridId`.

# .assertPlotConfigurationsBuildable aborts on a plot missing its plotId

    Code
      .assertPlotConfigurationsBuildable(list(list(dataCombinedId = "dc", plotType = "individual")),
      dataCombinedNames = "dc")
    Condition
      Error in `.assertPlotConfigurationsBuildable()`:
      ! Every plot must declare a `plotId`.

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

# the build guard rejects field problems even when validate = FALSE

    Code
      .assertPlotConfigurationsBuildable(list(p1 = list(plotId = "p1",
        dataCombinedId = "dc", plotType = "observedVsSimulated", xAxisScale = "log",
        yAxisScale = "linear", pointSize = 2)), dataCombinedNames = "dc")
    Condition
      Error in `.assertPlotConfigurationsBuildable()`:
      ! Invalid plot configuration fields:
      x Plot "p1" has unknown field pointSize. See `vignette("reference-definition-fields")` for the plot fields.
      x Plot "p1" is an "observedVsSimulated" plot with xAxisScale "log" and yAxisScale "linear". Both axes of this plot type use one scale: set both fields to the same value, or set only one of them.

