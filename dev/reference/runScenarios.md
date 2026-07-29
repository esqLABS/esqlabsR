# Run a set of scenarios from a `Project`.

Loads simulations, applies parameters, runs the simulations, and
collects results for one or more scenarios defined on a parsed
[Project](https://esqlabs.github.io/esqlabsR/dev/reference/Project.md).
The project must already have been loaded with
[`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md).

## Usage

``` r
runScenarios(
  project,
  scenarios = NULL,
  customParams = NULL,
  simulationRunOptions = NULL,
  validate = TRUE,
  stopIfParameterNotFound = TRUE,
  stopIfFails = TRUE
)
```

## Arguments

- project:

  A
  [Project](https://esqlabs.github.io/esqlabsR/dev/reference/Project.md)
  object loaded from a `Project.json` file.

- scenarios:

  Optional character vector of scenario names to run. `NULL` (default)
  runs all scenarios in the project. Each name is canonicalized (the
  same lowercasing and character-substitution ids get) and matched
  against the canonical ids scenarios were authored under, so the name
  you passed to
  [`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md)
  resolves; a name that canonicalizes to a different form warns, naming
  the id it resolved to.

- customParams:

  A list with vectors `paths`, `values`, and `units` — applied to every
  selected scenario as the final parameter layer.

- simulationRunOptions:

  Optional
  [ospsuite::SimulationRunOptions](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/SimulationRunOptions.html)
  for the simulation run. `NULL` (default) uses the package defaults.

- validate:

  Logical. If `TRUE` (default), runs the relevant section validators via
  [`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md)
  before simulating and aborts with a formatted summary on critical
  errors. Set to `FALSE` to skip the pre-flight check (e.g. when the
  caller has already validated the project).

- stopIfParameterNotFound:

  Logical. If `TRUE` (default), a `customParams` path that matches no
  parameter in a scenario's simulation aborts the run. Set to `FALSE` to
  skip such paths with a warning instead.

- stopIfFails:

  Logical. If `TRUE` (default), a scenario that fails to build (e.g. a
  missing model parameter path) or whose simulation produced no results
  aborts the run with an error. Set to `FALSE` to instead warn and leave
  that scenario's `outputValues` `NULL` while the other scenarios are
  still built, run, and returned.

## Value

A named list keyed by scenario name. Each entry is a list with
`simulation` (the initialized
[ospsuite::Simulation](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/Simulation.html)),
`results`
([ospsuite::SimulationResults](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/SimulationResults.html)),
`outputValues` (the computed output values, or `NULL` if simulation
failed), and `population` (an
[ospsuite::Population](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/Population.html)
for population scenarios, or `NULL` for individual scenarios). With
`stopIfFails = FALSE`, a scenario skipped at build time is still
returned, with `simulation`, `results`, and `outputValues` all `NULL`.

## Details

If a scenario fails, either at build time or because its simulation
produced no results, `runScenarios()` aborts by default
(`stopIfFails = TRUE`). Set `stopIfFails = FALSE` to instead produce a
warning, skip the failing scenario, and leave its `outputValues` `NULL`.

## See also

[`buildSimulations()`](https://esqlabs.github.io/esqlabsR/dev/reference/buildSimulations.md)
to obtain the parameterized simulations without running them.
