# Add one or more individuals to a Project

Add individuals to `individuals` definitions, vectorizing over a vector
of ids (see the recycling rule under Details). Scalar-per-definition
fields (`species` and the `...` fields `population`, `gender`, `weight`,
`height`, `age`, `proteinOntogenies`) follow the recycle/align rule;
`parameterSets` is vector-valued-per-definition (applied whole to every
individual, or one vector per individual via a length-`id` list).

## Usage

``` r
addIndividual(project, id, species, ...)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character vector of unique ids for the individuals (the number of
  individuals to add). Each is canonicalized to a safe, lowercase id (a
  warning names the result if it changed).

- species:

  Character scalar (recycled) or the same length as `id`, species name.

- ...:

  Optional named fields: `population`, `gender`, `weight`, `height`,
  `age`, `proteinOntogenies`, `parameterSets`, and `overwrite`. `gender`
  defaults to `UNKNOWN` when omitted (the only valid PK-Sim gender for
  some animal species); when supplied it must be a valid `GenderInt`
  token. Numeric fields are coerced via
  [`as.double()`](https://rdrr.io/r/base/double.html). `parameterSets`
  is a character vector of ids referencing `parameterSets` definitions.
  `overwrite` is a logical scalar (default `FALSE`): an id that already
  exists aborts unless `overwrite = TRUE`, which replaces it
  (last-write-wins). Unknown fields trigger an error.

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
[`removeIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeIndividual.md),
[`setIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/setIndividual.md)
