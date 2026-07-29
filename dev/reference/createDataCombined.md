# Generate DataCombined objects from a Project

Builds
[`ospsuite::DataCombined`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DataCombined.html)
objects from a JSON-driven
[Project](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md).
The project's `dataCombined` section declares the simulated/observed
entries; `loadObservedData(project)` resolves observed sources
internally. Either `dataCombined` or `plotGrids` (or both) selects which
DataCombined to build.

A simulated entry's `path` may be either a literal model quantity path
or an output-path id (a key of the project's `outputPaths` definitions).
An id is resolved to its literal path before the entry is built; any
value that is not a known id is used as a literal path.

## Usage

``` r
createDataCombined(
  project,
  dataCombined = NULL,
  plotGrids = NULL,
  scenarioResults = NULL,
  stopIfNotFound = TRUE,
  validate = TRUE
)

createDataCombinedFromExcel(...)
```

## Arguments

- project:

  A `Project` (see
  [`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md)).

- dataCombined:

  Names of the DataCombined entries to build. If any name is not
  declared in `dataCombined` definitions, an error is thrown.

- plotGrids:

  Names of plot grids whose DataCombined dependencies should be built.
  Combined with `dataCombined` if both are given.

- scenarioResults:

  A named list of Scenario Results (as returned by
  [`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md)).
  Not the OSPS `SimulationResults`.

- stopIfNotFound:

  If `TRUE` (default), the function errors when a referenced simulated
  path or observed dataSet cannot be resolved. If `FALSE`, a warning is
  emitted and the entry is skipped.

- validate:

  If `TRUE` (default), the `dataCombined` section is validated before
  any DataCombined is built, so a definition missing a required field
  aborts with a clear message instead of failing mid-build.

## Value

A named list of `DataCombined` objects, one per requested name. Empty
list when no names are requested.
