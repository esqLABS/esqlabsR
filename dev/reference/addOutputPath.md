# Add one or more output paths to a Project

Add output paths to `outputPaths` definitions, vectorizing over a vector
of ids (see the recycling rule under Details). `path` is
scalar-per-definition: a single path is recycled to every id, or a
length-`id` vector aligns by position.

## Usage

``` r
addOutputPath(project, id, path, overwrite = FALSE)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character vector of output path ids (unique within the call and not
  already present in `outputPaths` definitions). Each is canonicalized.

- path:

  Character vector of output paths, length 1 (recycled) or the same
  length as `id`.

- overwrite:

  Logical scalar. When `FALSE` (default), an id that already exists
  aborts. When `TRUE`, the existing output path is replaced
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

Other outputPath:
[`removeOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeOutputPath.md),
[`setOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/setOutputPath.md)
