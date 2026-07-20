# addPlotGrid aborts when no plots are defined

    Code
      addPlotGrid(project, "G1", plots = "MissingPlot")
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "G1" -> "g1"
      Error in `addPlotGrid()`:
      ! no plots are defined; add plots before creating a plot grid.
      i use `addPlot()` to add plots referenced by `plots`.

# addPlot aborts on a mismatched scalar-field length

    Code
      addPlot(project, id = c("p1x", "p2x", "p3x"), dataCombined = c("dc_a", "dc_b"),
      plotType = "individual")
    Condition
      Error in `.recycleScalarArg()`:
      ! `dataCombined` must be length 1 or length 3 (the number of ids).
      x It is length 2.

# print.Plot renders a single plot configuration

    Code
      print(project$definitions$plots[["p1"]])
    Output
      <Plot>
        * Plot Id: p1
        * DataCombined Id: aciclovir_individual
        * Plot Type: individual
        * Title: Aciclovir IV 250 mg

# print.PlotGrid renders a single plot grid

    Code
      print(project$definitions$plotGrids[["individual_diagnostics"]])
    Output
      <PlotGrid>
        * Plot Grid Id: individual_diagnostics
        * Plot Ids: p1
        * Title: Aciclovir — Individual Diagnostics

# print.DataCombined renders simulated and observed counts

    Code
      print(project$definitions$dataCombined[["aciclovir_individual"]])
    Output
      <DataCombined>
        * Simulated Entries: 1
        * Observed Entries: 1

