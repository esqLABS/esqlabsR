# Duplicate an existing scenario

Creates a deep copy of the scenario currently keyed `id` under `newId`,
leaving the original untouched. The copy is a new definition written
through to `newId` (the in-memory store and the on-disk project both
gain an independent scenario).

Both `id` and `newId` are canonicalized the same way
[`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md)
canonicalizes an id (lowercased, made a safe single-path-segment id,
with a warning when the value changed).

## Usage

``` r
duplicateScenario(project, id, newId)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character. Id of the scenario to copy; must exist in `scenarios`
  definitions (after canonicalization).

- newId:

  Character. Id for the new copy; its canonical form must not already
  belong to an existing scenario.

## Value

The `project` object, invisibly.

## See also

Other scenario:
[`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md),
[`removeScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeScenario.md),
[`renameScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/renameScenario.md),
[`setScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/setScenario.md)
