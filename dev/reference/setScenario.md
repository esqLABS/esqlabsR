# Modify fields of an existing scenario

Changes one or more fields of the scenario identified by `id` and
persists the change the same way
[`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md)
does (write-through to the scenario definition). The section accessor
`project$definitions$scenarios` is read-only, so this is the way to
revise an existing scenario: read it if you need the current values
(`sc <- project$definitions$scenarios[[name]]`), then pass the changes
here (`setScenario(project, name, ...)`).

Only the arguments you pass are changed; every other field keeps its
current value (partial update). For an optional field, passing `NULL`
clears it (e.g. `individual = NULL` detaches the individual), whereas
omitting the argument leaves it untouched. The required `modelFile`
cannot be cleared.

References are validated exactly as in
[`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md):
every supplied foreign-key argument (`individual`, `population`,
`application`, `parameterSets`, `initialConditions`, `outputPaths`) must
resolve in the project, and the changed scenario must pass structural
validation before it is written, so an invalid change touches neither
memory nor disk. A dangling reference is rejected eagerly with an
immediate error, not deferred to
[`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md).

The call vectorizes over a vector of ids (see the recycling rule under
Details): a supplied scalar-per-definition field is recycled or aligned
across `id`, and the whole-vector fields `parameterSets` /
`initialConditions` / `outputPaths` are applied whole to every scenario.
A field left unsupplied is untouched on every scenario.

## Usage

``` r
setScenario(
  project,
  id,
  modelFile,
  individual,
  population,
  application,
  parameterSets,
  initialConditions,
  outputPaths,
  simulationTime,
  simulationTimeUnit,
  steadyState,
  steadyStateTime,
  steadyStateTimeUnit,
  overwriteFormulasInSS,
  readPopulationFromCSV
)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character vector. Ids of the scenarios to modify. Each is
  canonicalized the same way
  [`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md)
  canonicalizes it, and must already exist in `scenarios` definitions.

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

  Character time-unit string. Omitting the argument leaves the current
  value untouched (there is no default; this is a partial update).

- steadyState:

  Logical, whether to simulate steady state. Omitting the argument
  leaves the current value untouched (there is no default; this is a
  partial update).

- steadyStateTime:

  Numeric steady-state time in `steadyStateTimeUnit`. Omitting the
  argument leaves the current value untouched (there is no default; this
  is a partial update).

- steadyStateTimeUnit:

  Character unit for `steadyStateTime`. Omitting the argument leaves the
  current value untouched (there is no default; this is a partial
  update).

- overwriteFormulasInSS:

  Logical, whether to overwrite formulas during steady state. Omitting
  the argument leaves the current value untouched (there is no default;
  this is a partial update).

- readPopulationFromCSV:

  Logical, whether to load the population from CSV. Omitting the
  argument leaves the current value untouched (there is no default; this
  is a partial update).

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
[`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md),
[`duplicateScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/duplicateScenario.md),
[`removeScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeScenario.md),
[`renameScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/renameScenario.md)
