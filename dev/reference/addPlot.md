# Add a plot configuration to a Project

Add one or more entries to `plots` definitions (a keyed list, one entry
per plot). Errors if a `plotId` already exists, if a `dataCombined` is
not present in `dataCombined` definitions, or if a `plotType` is not one
of the supported types.

## Usage

``` r
addPlot(project, id, dataCombined, plotType, ...)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character vector of unique plot identifiers (the number of plots to
  add). Each is canonicalized to a safe, lowercase id (a warning names
  the result if it changed). Stored in the `plotId` field.

- dataCombined:

  Character, length 1 (recycled to all plots) or the same length as `id`
  (aligned by position). Each must reference an existing DataCombined id
  on the project.

- plotType:

  Character, length 1 (recycled) or the same length as `id`. Each one of
  `"individual"`, `"population"`, `"observedVsSimulated"`,
  `"residualsVsSimulated"`, `"residualsVsTime"`.

- ...:

  Optional plot-configuration fields, e.g. `title`, `subtitle`, `xUnit`,
  `yUnit`, `xAxisScale`, `yAxisScale`, `xValuesLimits`, `yValuesLimits`,
  `aggregation`, `quantiles`, `nsd`, `foldDistance`. A multi-value field
  (e.g. `quantiles = c(0.05, 0.5, 0.95)`) is applied whole to every plot
  and stored as a comma-separated string; to set a scalar field
  differently per plot, pass a list of the same length as `id`.

  Note the deliberate asymmetry with the positional scalar args above: a
  length-`N` *vector* passed to `dataCombined` or `plotType` aligns to
  the ids **by position** (one value per plot), whereas a length-`N`
  *atomic vector* passed as a `...` field is treated as one multi-value
  field and applied **whole** to every plot (collapsed to a
  comma-separated string), not split one-per-plot. So
  `title = c("A", "B")` gives every plot the single title `"A, B"`, not
  plot 1 `"A"` and plot 2 `"B"`. To vary a `...` field per plot, pass a
  length-`N` **list** (`title = list("A", "B")`).

  `...` also accepts `overwrite`, a logical scalar (default `FALSE`): a
  plot id that already exists aborts unless `overwrite = TRUE`, which
  replaces it (last-write-wins).

## Value

The `project` object, invisibly.

## Details

The id argument sets `N`, the number of definitions to act on, and
cannot itself be recycled: when any scalar-per-definition field has
length greater than 1, the id vector must have that same length. A
length-1 id with all-scalar fields is the ordinary single-definition
call.

Each scalar-per-definition field is either length 1 (recycled to all `N`
definitions) or length `N` (aligned to the ids by position). Any other
length is an error naming the field and the lengths.

A vector-valued-per-definition field (an individual's or application's
`parameterSets`, a scenario's `outputPaths` and `parameterSets`) is
applied whole to every definition, never split positionally. To give a
different multi-valued list per definition, pass a list of the same
length as the id vector (one vector per definition).

The call is all-or-nothing: every definition is validated first, and if
any fails the whole call aborts and writes nothing. On success all
definitions are folded into the section and persisted in a single
write-through.

Two families of authoring functions sit outside this id-sets-`N` rule.
[`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md)
and
[`removeParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeParameterEntry.md)
vectorize over parameter entries (parallel `containerPath` /
`parameterName` / `value` / `units` vectors) within a single named set,
a different axis than the id-sets-`N` rule described here.
[`renameScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/renameScenario.md),
[`duplicateScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/duplicateScenario.md),
[`addObservedData()`](https://esqlabs.github.io/esqlabsR/dev/reference/addObservedData.md),
[`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md),
and the per-task parameter-identification sub-definition helpers act on
a single definition per call.

## See also

Other plots:
[`addPlotGrid()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPlotGrid.md),
[`removePlot()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePlot.md),
[`removePlotGrid()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePlotGrid.md)
