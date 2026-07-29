# Remove one or more populations from a Project

Drop the populations with matching ids in one write-through. Warns (and
skips) any id not present, and warns when a removed population is still
referenced.

## Usage

``` r
removePopulation(project, id)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character vector of population ids to remove. Each is canonicalized
  the same way
  [`addPopulation()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPopulation.md)
  canonicalizes it.

## Value

The `project` object, invisibly.

## See also

Other population:
[`addPopulation()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPopulation.md),
[`setPopulation()`](https://esqlabs.github.io/esqlabsR/dev/reference/setPopulation.md)
