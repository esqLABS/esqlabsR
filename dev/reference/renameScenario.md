# Rename an existing scenario

Renames the scenario currently keyed `id` to `newId`, preserving its
configuration. The change is write-through: the scenario's old
definition is removed and a new one written under `newId`, the in-memory
key changes, and the record's stored name is updated to match the new
key so a reload round-trips (the name-equals-key invariant the project
relies on).

Both `id` and `newId` are canonicalized the same way
[`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md)
canonicalizes an id (lowercased, made a safe single-path-segment id,
with a warning when the value changed), so the same typed strings used
to create and reference a scenario resolve consistently.

## Usage

``` r
renameScenario(project, id, newId)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character. Id of the scenario to rename; must exist in `scenarios`
  definitions (after canonicalization).

- newId:

  Character. New id for the scenario; its canonical form must not
  already belong to a different scenario.

## Value

The `project` object, invisibly.

## See also

Other scenario:
[`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md),
[`duplicateScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/duplicateScenario.md),
[`removeScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeScenario.md),
[`setScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/setScenario.md)
