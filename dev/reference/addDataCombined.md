# Add one or more DataCombined to a Project

Append new DataCombined entries (each with one or more simulated and/or
observed rows) to `dataCombined` definitions. Pass a vector of ids to
add several DataCombined in one call.

## Usage

``` r
addDataCombined(
  project,
  id,
  simulated = list(),
  observed = list(),
  overwrite = FALSE
)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character vector of unique DataCombined ids (the number of
  DataCombined to add). Each is canonicalized to a safe, lowercase id (a
  warning names the result if it changed).

- simulated:

  For a single DataCombined (`id` length 1), a list of named lists, each
  including `label`, `scenario`, and `path` (optional `group`,
  `xOffsets`, `xOffsetsUnits`, `yOffsets`, `yOffsetsUnits`,
  `xScaleFactors`, `yScaleFactors`). `path` may be either a literal
  model quantity path or an output-path id (a key of the project's
  `outputPaths` definitions); an id is resolved to its literal path when
  the DataCombined is built by
  [`createDataCombined()`](https://esqlabs.github.io/esqlabsR/dev/reference/createDataCombined.md).
  The `scenario` reference is canonicalized to match its scenario
  definition. To add several DataCombined in one call, pass a list of
  the same length as `id`, one such simulated list per DataCombined.

- observed:

  Like `simulated`, but each named list includes `label` and `dataSet`
  (optional fields as `simulated` minus `scenario` and `path`).

- overwrite:

  Logical scalar. When `FALSE` (default), an id that already exists
  aborts. When `TRUE`, the existing DataCombined is replaced
  (last-write-wins).

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

Other dataCombined:
[`removeDataCombined()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeDataCombined.md)
