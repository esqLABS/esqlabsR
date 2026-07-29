# Create a Scenario

Builds a plain-data `Scenario` record holding the configuration fields
of a v2.0 `Project.json` scenario entry. It does not create or hold
ospsuite runtime objects; the runtime is built by
[`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md)
at execution time.

A `Scenario` is a named list with copy semantics: an entry extracted
from `scenarios` definitions is an independent copy. The section
accessor is read-only, so to apply a change you pass the record to an
authoring function
([`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md)
/
[`setScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/setScenario.md)),
which validates and writes it through to the project.

## Usage

``` r
Scenario(
  scenarioName = NULL,
  modelFile = NULL,
  applicationProtocol = NULL,
  individualId = NULL,
  populationId = NULL,
  outputPaths = NULL,
  simulationType = if (is.null(populationId)) "Individual" else "Population",
  readPopulationFromCSV = FALSE,
  simulateSteadyState = FALSE,
  simulationTime = NULL,
  simulationTimeUnit = NULL,
  steadyStateTime = 1000,
  steadyStateTimeUnit = NULL,
  overwriteFormulasInSS = FALSE,
  modelParameterSets = NULL,
  initialConditions = NULL
)
```

## Arguments

- scenarioName:

  Character. Name of the scenario.

- modelFile:

  Character. Name of the `.pkml` model file (relative to the model
  folder).

- applicationProtocol:

  Character or `NA`. Name of the application protocol; `NA` when absent.

- individualId:

  Character or `NULL`. ID referencing `individuals` definitions.

- populationId:

  Character or `NULL`. ID referencing `populations` definitions.

- outputPaths:

  Named character vector of literal output paths. Names are the ids
  referencing `outputPaths` definitions; values are the literal paths.
  `NULL` when the scenario declares no outputs. Round-trip serialization
  reads `names(outputPaths)` to rebuild the `outputPaths` id array, so
  the named-vector invariant must be preserved.

- simulationType:

  Character. `"Individual"` or `"Population"`. Defaults to
  `"Population"` when `populationId` is set, `"Individual"` otherwise.

- readPopulationFromCSV:

  Logical. If `TRUE`, load population from CSV.

- simulateSteadyState:

  Logical. If `TRUE`, run steady-state before the main simulation.

- simulationTime:

  The parsed time grid: a list of length-3 numeric vectors
  `c(start, end, resolution)`, one per interval. This is the stored
  form;
  [`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md)
  and
  [`setScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/setScenario.md)
  also take a single interval as `c(start, end, resolution)` or a
  `"start, end, resolution"` string and parse it to this shape.

- simulationTimeUnit:

  Character. Time unit for `simulationTime`.

- steadyStateTime:

  Numeric. Steady-state time **in base unit (minutes)**.

- steadyStateTimeUnit:

  Character. Original unit for `steadyStateTime`, preserved for
  round-trip serialization.

- overwriteFormulasInSS:

  Logical. Overwrite formula parameters during steady-state.

- modelParameterSets:

  Character vector. Parameter-set ids referencing `parameterSets`
  definitions.

- initialConditions:

  Character vector. Initial-condition set ids referencing
  `initialConditions` definitions.

## Value

A `Scenario` object: a named list carrying exactly the fields above.
