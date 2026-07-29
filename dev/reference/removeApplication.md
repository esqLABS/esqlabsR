# Remove one or more application protocols from a Project

Drop the protocols with matching ids in one write-through. Warns (and
skips) any id not present, and warns when a removed protocol is still
referenced.

## Usage

``` r
removeApplication(project, id)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character vector of application ids to remove. Each is canonicalized
  the same way
  [`addApplication()`](https://esqlabs.github.io/esqlabsR/dev/reference/addApplication.md)
  canonicalizes it.

## Value

The `project` object, invisibly.

## See also

Other application:
[`addApplication()`](https://esqlabs.github.io/esqlabsR/dev/reference/addApplication.md),
[`setApplicationParameterSets()`](https://esqlabs.github.io/esqlabsR/dev/reference/setApplicationParameterSets.md)
