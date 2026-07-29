# Add one or more scenarios programmatically to a Project

Creates new `Scenario` records and adds them to `scenarios` definitions
after validating all references. The call vectorizes over a vector of
ids (see the recycling rule under Details). Scalar-per-definition fields
(`modelFile`, `individual`, `population`, `application`,
`simulationTime`, `simulationTimeUnit`, `steadyState`,
`steadyStateTime`, `steadyStateTimeUnit`, `overwriteFormulasInSS`,
`readPopulationFromCSV`) follow the recycle/align rule. The
vector-valued-per-definition fields `parameterSets` and `outputPaths`
are applied whole to every scenario; to give a different set per
scenario, pass a list of the same length as `id` (one character vector
per scenario). `initialConditions` follows the same whole-vector rule.

## Usage

``` r
addScenario(
  project,
  id,
  modelFile,
  individual = NULL,
  population = NULL,
  application = NULL,
  parameterSets = NULL,
  initialConditions = NULL,
  outputPaths = NULL,
  simulationTime = NULL,
  simulationTimeUnit = "h",
  steadyState = FALSE,
  steadyStateTime = 1000,
  steadyStateTimeUnit = "min",
  overwriteFormulasInSS = FALSE,
  readPopulationFromCSV = FALSE,
  overwrite = FALSE
)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character vector of ids (names) for the new scenarios (the number of
  scenarios to add). Each is canonicalized to a safe, lowercase,
  single-path-segment id (a warning names the result if it changed);
  each canonical id must not already exist in `scenarios` definitions.

- modelFile:

  Character. Name of the `.pkml` model file (relative to model folder).

- individual:

  Character or `NULL`. Id referencing `individuals` definitions.

- population:

  Character or `NULL`. Id referencing `populations` definitions.

- application:

  Character or `NULL`. Id of the application protocol referencing
  `applications` definitions.

- parameterSets:

  Character vector or `NULL`. Parameter-set ids referencing
  `parameterSets` definitions. Applied whole to every scenario.

- initialConditions:

  Character vector or `NULL`. Initial-condition set ids referencing
  `initialConditions` definitions. Applied whole to every scenario.

- outputPaths:

  Character vector or `NULL`. Output-path ids referencing `outputPaths`
  definitions. Applied whole to every scenario.

- simulationTime:

  The simulation time grid, or `NULL` (default) to keep the one the
  model file carries. One interval is a length-3 numeric vector
  `c(start, end, resolution)` or the same grid written as a string,
  `"start, end, resolution"`. Several intervals go in one string,
  `"start, end, resolution; start, end, resolution"`. To give a
  different grid per scenario, pass a list with one element (in either
  form) per id.

- simulationTimeUnit:

  Character. Time unit string. Default `"h"`.

- steadyState:

  Logical. Whether to simulate steady state. Default `FALSE`.

- steadyStateTime:

  Numeric. Steady-state time in `steadyStateTimeUnit`. Default `1000`.

- steadyStateTimeUnit:

  Character. Unit for `steadyStateTime`. Default `"min"`.

- overwriteFormulasInSS:

  Logical. Overwrite formulas during steady state. Default `FALSE`.

- readPopulationFromCSV:

  Logical. Load population from CSV. Default `FALSE`.

- overwrite:

  Logical. When `FALSE` (default), an id that already exists aborts.
  When `TRUE`, the existing scenario is replaced (last-write-wins).
  Distinct from `overwriteFormulasInSS`, which is a steady-state model
  option unrelated to duplicate handling.

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

Other scenario:
[`duplicateScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/duplicateScenario.md),
[`removeScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeScenario.md),
[`renameScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/renameScenario.md),
[`setScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/setScenario.md)
