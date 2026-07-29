# Modify fields of an existing individual

Changes one or more fields of the individual identified by `id` and
persists the change immediately to the individual definition
(write-through). The `individuals` definitions accessor is read-only, so
this is the way to revise an existing individual in place.

Only the arguments you pass via `...` are changed; every other field
keeps its current value (partial update). Validation matches
[`addIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/addIndividual.md):
numeric fields (`weight`, `height`, `age`) are coerced via
[`as.double()`](https://rdrr.io/r/base/double.html), `gender` (if
supplied) must be a non-empty string, and `parameterSets` (if supplied)
must be a character vector of ids that resolve in `parameterSets`
definitions. The required `species` field, if supplied, must be a
non-empty string.

## Usage

``` r
setIndividual(project, id, ...)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character vector. Ids of the individuals to modify. Each is
  canonicalized the same way
  [`addIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/addIndividual.md)
  canonicalizes it, and must already exist in `individuals` definitions.

- ...:

  Named fields to change. Accepted: `species`, `population`, `gender`,
  `weight`, `height`, `age`, `proteinOntogenies`, `parameterSets`.
  Scalar-per-definition fields recycle/align across `id`;
  `parameterSets` is applied whole (or one vector per individual via a
  length-`id` list). Unknown fields trigger an error.

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

Other individual:
[`addIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/addIndividual.md),
[`removeIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeIndividual.md)
