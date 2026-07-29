# Modify fields of an existing population

Changes one or more fields of the population identified by `id` and
persists the change immediately to the population definition
(write-through). The `populations` definitions accessor is read-only, so
this is the way to revise an existing population in place.

Only the arguments you pass via `...` are changed; every other field
keeps its current value (partial update). Validation matches
[`addPopulation()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPopulation.md):
the numeric range fields are coerced via
[`as.double()`](https://rdrr.io/r/base/double.html) and
`numberOfIndividuals` (if supplied) must be a positive number. The
required `species` field, if supplied, must be a non-empty string.

## Usage

``` r
setPopulation(project, id, ...)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character vector. Ids of the populations to modify. Each is
  canonicalized the same way
  [`addPopulation()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPopulation.md)
  canonicalizes it, and must already exist in `populations` definitions.

- ...:

  Named fields to change. Accepted: `species`, `numberOfIndividuals`,
  `proportionOfFemales`, `weightMin`, `weightMax`, `heightMin`,
  `heightMax`, `ageMin`, `ageMax`, `BMIMin`, `BMIMax`, `gender`,
  `weightUnit`, `heightUnit`, `ageUnit`, `BMIUnit`, `population`,
  `diseaseState`, `proteinOntogenies`. Scalar-per-definition fields
  recycle/align across `id`. Numeric fields are coerced via
  [`as.double()`](https://rdrr.io/r/base/double.html). Unknown fields
  trigger an error.

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

Other population:
[`addPopulation()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPopulation.md),
[`removePopulation()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePopulation.md)
