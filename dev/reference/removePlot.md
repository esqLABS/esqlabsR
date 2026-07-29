# Remove one or more plot configurations from a Project

Drop the entries with matching `plotId`s. Warns (and skips) any `id` not
found, and warns when a removed plot is still referenced by any
`plotGrids` entry. All removals are written through in a single pass.

## Usage

``` r
removePlot(project, id)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character vector of plot ids. Each is canonicalized the same way
  [`addPlot()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPlot.md)
  canonicalizes it.

## Value

The `project` object, invisibly.

## See also

Other plots:
[`addPlot()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPlot.md),
[`addPlotGrid()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPlotGrid.md),
[`removePlotGrid()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePlotGrid.md)
