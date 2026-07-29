# Remove one or more individuals from a Project

Drop the individuals with matching ids in one write-through. Warns (and
skips) any id not present, and warns when a removed individual is still
referenced.

## Usage

``` r
removeIndividual(project, id)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character vector of individual ids to remove. Each is canonicalized
  the same way
  [`addIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/addIndividual.md)
  canonicalizes it.

## Value

The `project` object, invisibly.

## See also

Other individual:
[`addIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/addIndividual.md),
[`setIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/setIndividual.md)
