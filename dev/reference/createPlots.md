# Generate plots from a Project

**Returns plot grids, not standalone plots.** By default `createPlots()`
builds every plot grid declared in `plotGrids` definitions and no
standalone plots, and hands back a **named list of plot grids keyed by
Plot Grid name** (the `plots` argument opts individual standalone plots
into that same list).

Reads `plots` definitions and `plotGrids` definitions (both keyed lists,
one entry per plot / grid) to build the requested plot grids and,
optionally, standalone single plots. DataCombined objects are resolved
via
[`createDataCombined()`](https://esqlabs.github.io/esqlabsR/dev/reference/createDataCombined.md)
internally unless supplied via `dataCombinedList`.

With neither `plotGrids` nor `plots`, all plot grids declared in
`plotGrids` definitions are built (the default). The two arguments are
independent selectors: `plotGrids` selects plot grids (keyed by
`plotGridId` in the result), `plots` selects standalone single plots
(keyed by `plotId`). A `plotId` that is also part of a requested grid
still gets its own standalone entry.

## Usage

``` r
createPlots(
  project,
  plotGrids = NULL,
  plots = NULL,
  scenarioResults = NULL,
  dataCombinedList = NULL,
  stopIfNotFound = TRUE,
  validate = TRUE
)

createPlotsFromExcel(...)
```

## Arguments

- project:

  A `Project` (see
  [`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md)).

- plotGrids:

  Names of plot grids to build. If `NULL` (default) and `plots` is also
  `NULL`, all grids declared in `plotGrids` definitions are built.

- plots:

  Ids of standalone single plots to render directly (not laid out in a
  grid), each resolved against `plots` definitions. `NULL` (default)
  renders no standalone plots; standalone plots are opt-in.

- scenarioResults:

  Named list of Scenario Results from
  [`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md)
  (each entry has `simulation`, `results`, `outputValues`,
  `population`). Not the OSPS `SimulationResults`.

- dataCombinedList:

  Optional pre-built named list of `DataCombined` objects. If `NULL`,
  the function builds them via
  [`createDataCombined()`](https://esqlabs.github.io/esqlabsR/dev/reference/createDataCombined.md).

- stopIfNotFound:

  If `TRUE`, errors when a referenced DataCombined or simulated/observed
  entry cannot be resolved, or when a requested `plotGrids` / `plots` id
  is not defined in the project.

- validate:

  Logical. If `TRUE` (default), runs the relevant section validators via
  [`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md)
  before building the plots and aborts with a formatted summary on
  critical errors. Set to `FALSE` to skip the pre-flight check (e.g.
  when the caller has already validated the project).

## Value

A named list of **plot grids** keyed by Plot Grid name: one entry per
requested plot grid (keyed by its `plotGridId`), unioned with one entry
per requested standalone plot (keyed by its `plotId`) when `plots` is
given. Note the list holds plot grids, not standalone `Plot` objects,
unless standalone plots were explicitly requested via `plots`. An empty
list when the project has no plots to build.
