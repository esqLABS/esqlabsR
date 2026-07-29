# Build the simulations for a set of scenarios without running them

Loads and fully parameterizes (but does not run) the
[ospsuite::Simulation](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/Simulation.html)
(and, for a population scenario, the
[ospsuite::Population](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/Population.html))
for one or more scenarios defined on a parsed
[Project](https://esqlabs.github.io/esqlabsR/dev/reference/Project.md).
Use this to inspect or modify a simulation before running it yourself,
to save the configured simulation to PKML, or to hand it to another
OSP-suite routine. To simulate and collect results in one step, use
[`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md).

## Usage

``` r
buildSimulations(
  project,
  scenarios = NULL,
  customParams = NULL,
  simulationRunOptions = NULL,
  validate = TRUE,
  stopIfParameterNotFound = TRUE
)
```

## Arguments

- project:

  A
  [Project](https://esqlabs.github.io/esqlabsR/dev/reference/Project.md)
  object loaded from a `Project.json` file.

- scenarios:

  Optional character vector of scenario names to build. `NULL` (default)
  builds all scenarios in the project. Each name is canonicalized (the
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
  [ospsuite::SimulationRunOptions](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/SimulationRunOptions.html).
  Consulted only for a scenario with `simulateSteadyState` set (the
  steady-state pre-solve still runs); it is not applied to the returned
  simulations, since they are not run here. `NULL` (default) falls back
  to the project's `defaultSimulationRunOptions`.

- validate:

  Logical. If `TRUE` (default), runs the relevant section validators via
  [`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md)
  before building and aborts with a formatted summary on critical
  errors. Set to `FALSE` to skip the pre-flight check.

- stopIfParameterNotFound:

  Logical. If `TRUE` (default), a `customParams` path that matches no
  parameter in a scenario's simulation aborts the build. Set to `FALSE`
  to skip such paths with a warning instead.

## Value

A named list keyed by scenario name. Each entry is a list with
`simulation` (the initialized, not-yet-run
[ospsuite::Simulation](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/Simulation.html))
and `population` (an
[ospsuite::Population](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/Population.html)
for population scenarios, or `NULL` for individual scenarios). Pass the
result to
[`ospsuite::runSimulations()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/runSimulations.html),
or inspect and edit the `Simulation` first. This is not the shape
[`saveScenarioResults()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveScenarioResults.md)
expects; use
[`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md)
for that.

## See also

[`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md)
to build and run in one step.
