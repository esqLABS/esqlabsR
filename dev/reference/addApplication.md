# Add one or more application protocols to a Project

Add protocols to `applications` definitions, vectorizing over a vector
of ids (see the recycling rule under Details). `parameterSets` is
vector-valued-per-definition: it is applied whole to every protocol; to
give a different set per protocol, pass a list of the same length as
`id` (one character vector per protocol).

## Usage

``` r
addApplication(project, id, parameterSets = NULL, overwrite = FALSE)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character vector of unique protocol ids (the number of protocols to
  add). Each is canonicalized to a safe, lowercase id (a warning names
  the result if it changed).

- parameterSets:

  Optional character vector of set ids referencing `parameterSets`
  definitions, applied whole to every protocol. Defaults to `NULL`. Use
  a list of the same length as `id` for a per-protocol set.

- overwrite:

  Logical scalar. When `FALSE` (default), an id that already exists
  aborts. When `TRUE`, the existing protocol is replaced
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

Other application:
[`removeApplication()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeApplication.md),
[`setApplicationParameterSets()`](https://esqlabs.github.io/esqlabsR/dev/reference/setApplicationParameterSets.md)
