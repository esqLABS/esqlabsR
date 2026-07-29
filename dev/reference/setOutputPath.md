# Change the literal path of one or more existing output paths

Updates the OSPS-notation path string bound to existing output-path ids
and persists the change immediately to the output-path definition
(write-through). The ids themselves are not changed (use
[`removeOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeOutputPath.md) +
[`addOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/addOutputPath.md)
to rename), so every scenario that records these output paths keeps
referencing them. The `outputPaths` definitions accessor is read-only,
so this is the way to change a path in place. The call vectorizes over a
vector of ids (see the recycling rule under Details); `path` is
scalar-per-definition (one path recycled to every id, or a length-`id`
vector aligned by position).

## Usage

``` r
setOutputPath(project, id, path)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character vector. The output-path ids to modify. Each must already
  exist in `outputPaths` definitions.

- path:

  Character vector of new non-empty OSPS-notation path strings, length 1
  (recycled) or the same length as `id`.

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

Other outputPath:
[`addOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/addOutputPath.md),
[`removeOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeOutputPath.md)
