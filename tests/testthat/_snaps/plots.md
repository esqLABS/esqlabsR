# addPlotGrid aborts when no plots are defined

    Code
      addPlotGrid(project, "G1", plotIDs = "MissingPlot")
    Condition
      Error in `addPlotGrid()`:
      ! no plots are defined; add plots before creating a plot grid.
      i use `addPlot()` to add plots referenced by `plotIDs`.

