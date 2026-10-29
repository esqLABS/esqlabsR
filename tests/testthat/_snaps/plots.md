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

# addDataCombined aborts on an existing id, replaces it with overwrite

    Code
      addDataCombined(project, "dc", simulated = mk("second"))
    Condition
      Error in `addDataCombined()`:
      ! DataCombined "dc" already exists.
      i Pass `overwrite = TRUE` to replace it.

# addPlot aborts on an existing id, replaces it with overwrite

    Code
      addPlot(project, "p", "dc", "population", title = "second")
    Condition
      Error in `addPlot()`:
      ! plot "p" already exists.
      i Pass `overwrite = TRUE` to replace it.

# addPlotGrid aborts on an existing id, replaces it with overwrite

    Code
      addPlotGrid(project, "g", plots = "p", title = "second")
    Condition
      Error in `addPlotGrid()`:
      ! plot grid "g" already exists.
      i Pass `overwrite = TRUE` to replace it.

# addPlot aborts on a mismatched scalar-field length

    Code
      addPlot(project, id = c("p1x", "p2x", "p3x"), dataCombined = c("dc_a", "dc_b"),
      plotType = "individual")
    Condition
      Error in `.recycleScalarArg()`:
      ! `dataCombined` must be length 1 or length 3 (the number of ids).
      x It is length 2.

# addPlot rejects unknown fields and invalid values, writing nothing

    Code
      addPlot(project, id = c("p_style", "p_scale"), dataCombined = "dc_ok",
      plotType = "individual", lineColour = list("red", NULL), linesColor = list(NULL,
        "reddish"), yAxisScale = list(NULL, "lin"))
    Condition
      Error in `addPlot()`:
      ! Invalid plot configuration fields:
      x Plot "p_style" has unknown field lineColour. See `vignette("reference-definition-fields")` for the plot fields.
      x Plot "p_scale" has linesColor "reddish". Expected: color names or hexadecimal color codes.
      x Plot "p_scale" has yAxisScale "lin". Allowed values: "linear" and "log". Use "linear" instead of "lin".

# .validatePlots reports field problems as critical errors

    Code
      cat(msgs, sep = "\n")
    Output
      Plot "p_unknown" has unknown field lineColour. See `vignette("reference-definition-fields")` for the plot fields.
      Plot "p_lin" has xAxisScale "lin". Allowed values: "linear" and "log". Use "linear" instead of "lin".
      Plot "p_values" has linesColor "#4ABDCB, reddish". Expected: color names or hexadecimal color codes.
      Plot "p_values" has titleFontFace "heavy". Expected: one of: plain, bold, italic, bold.italic.
      Plot "p_values" has linesAlpha "2". Expected: numbers from 0 to 1.
      Plot "p_values" has xAxisLimits "0, 12, 24". Expected: 2 values.
      Plot "p_ovs" is an "observedVsSimulated" plot with xAxisScale "log" and yAxisScale "linear". Both axes of this plot type use one scale: set both fields to the same value, or set only one of them.
      Plot "p_lloq" has lloqDirection "vertical", which a plot of type "individual" cannot draw. Allowed: "horizontal".
      Plot "p_lloq_ovs" has lloqDirection "both" and lloqOnBothAxes FALSE, which disagree. "both" draws the LLOQ on both axes, "vertical" only on the observed axis: set one of the two fields.
      Plot "p_ticks" has 3 tick labels in xAxisTicksLabels but not the same number of ticks in xAxisTicks. Label texts need one tick each.

# .validatePlots warns about plot fields that have no effect

    Code
      cat(msgs, sep = "\n")
    Output
      Plot 'p_rvs' of plotType 'residualsVsSimulated' sets 'xUnit', which only applies to plotType 'individual', 'population', 'residualsVsTime' and is ignored.
      Plot 'p_rvs' of plotType 'residualsVsSimulated' sets 'ribbonsAlpha', which only applies to plotType 'population' and is ignored.
      Plot 'p_ind' sets 'watermark', 'watermarkSize', 'displayLLOQ', which the OSP Suite plot functions cannot draw yet. The settings have no effect.
      Plot 'p_ind' of plotType 'individual' sets 'pointsColor', which only applies to plotType 'observedVsSimulated', 'residualsVsSimulated', 'residualsVsTime' and is ignored. On a time profile, observed points take the color of their curve: use 'linesColor'.
      Plot 'p_ind' sets 'titleColor' but not 'title', so there is no text to style and the setting is ignored.
      Plot 'p_ovs' sets 'foldLinesLegendDiagonal', which the OSP Suite plot functions cannot draw yet. The setting has no effect.
      Plot 'p_ovs' of plotType 'observedVsSimulated' sets 'linesLinetype', which only applies to plotType 'individual', 'population', 'residualsVsSimulated', 'residualsVsTime' and is ignored.

# .validatePlots names the closest existing id for a dangling reference

    Code
      cat(vapply(result$critical_errors, function(e) e$message, character(1)), sep = "\n")
    Output
      plotConfiguration references unknown dataCombined: aciclovir_pbv (did you mean 'aciclovir_pvb'?)
      plotGrids references unknown plots: p2 (did you mean 'p1'?)

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
        * Title: Aciclovir - Individual Diagnostics

# print.DataCombined renders simulated and observed counts

    Code
      print(project$definitions$dataCombined[["aciclovir_individual"]])
    Output
      <DataCombined>
        * Simulated Entries: 1
        * Observed Entries: 1

