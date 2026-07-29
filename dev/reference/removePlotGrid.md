# Remove one or more plot grids from a Project

Drop the entries with matching `plotGridId`s. Warns (and skips) any `id`
not present. All removals are written through in a single pass.

## Usage

``` r
removePlotGrid(project, id)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character vector of plot-grid ids. Each is canonicalized the same way
  [`addPlotGrid()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPlotGrid.md)
  canonicalizes it.

## Value

The `project` object, invisibly.

## See also

Other plots:
[`addPlot()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPlot.md),
[`addPlotGrid()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPlotGrid.md),
[`removePlot()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePlot.md)
