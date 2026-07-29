# Remove one or more scenarios from a Project

Remove one or more scenarios from a Project

## Usage

``` r
removeScenario(project, id)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character vector of scenario ids to remove in one write-through. Each
  is canonicalized the same way
  [`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md)
  canonicalizes it, so the same typed id removes what it created. A
  not-found id warns and is skipped.

## Value

The `project` object, invisibly.

## See also

Other scenario:
[`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md),
[`duplicateScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/duplicateScenario.md),
[`renameScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/renameScenario.md),
[`setScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/setScenario.md)
