# Add one or more plot grids to a Project

Add new entries to `plotGrids` definitions (a keyed list, one entry per
grid). Errors if a `plotGridId` already exists or if any of the supplied
`plots` are not present in `plots` definitions.

## Usage

``` r
addPlotGrid(project, id, plots, ...)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character vector of unique plot-grid ids (the number of grids to add).
  Each is canonicalized to a safe, lowercase id (a warning names the
  result if it changed). Stored in the `plotGridId` field.

- plots:

  The plot ids each grid includes (stored internally as a
  comma-separated string). A character vector is applied whole to every
  grid; to give a different set of plot ids per grid, pass a list of the
  same length as `id` (one character vector per grid).

- ...:

  Optional plot-grid fields, e.g. `title`, `subtitle`. A scalar field is
  recycled to every grid; to set one differently per grid, pass a list
  of the same length as `id`.

  A length-`N` *atomic vector* passed as a `...` field is treated as one
  multi-value field and applied **whole** to every grid (collapsed to a
  comma-separated string), not split one-per-grid. So
  `title = c("A", "B")` gives every grid the single title `"A, B"`, not
  grid 1 `"A"` and grid 2 `"B"`. To vary a `...` field per grid, pass a
  length-`N` **list** (`title = list("A", "B")`).

  `...` also accepts `overwrite`, a logical scalar (default `FALSE`): a
  plot-grid id that already exists aborts unless `overwrite = TRUE`,
  which replaces it (last-write-wins).

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
[`addPlot()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPlot.md),
[`removePlot()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePlot.md),
[`removePlotGrid()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePlotGrid.md)
