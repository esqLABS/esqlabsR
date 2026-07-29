# Remove one or more DataCombined from a Project

Drop the named entries from `dataCombined` definitions. Warns (and
skips) any `id` not present, and warns about any plot entries that still
reference a removed id. All removals are written through in one pass.

## Usage

``` r
removeDataCombined(project, id)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character vector of DataCombined ids to remove. Each is canonicalized
  the same way
  [`addDataCombined()`](https://esqlabs.github.io/esqlabsR/dev/reference/addDataCombined.md)
  canonicalizes it.

## Value

The `project` object, invisibly.

## See also

Other dataCombined:
[`addDataCombined()`](https://esqlabs.github.io/esqlabsR/dev/reference/addDataCombined.md)
