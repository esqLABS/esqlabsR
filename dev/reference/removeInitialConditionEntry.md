# Remove one or many entries from a named initial-condition set

Removes molecule start-value entries from the named set. `path` accepts
a vector of length N to remove all N entries in a single call (and a
single write to disk); a scalar call (length-1 vector) removes one
entry. If every entry of the set is removed, the set itself is
auto-removed from `initialConditions` definitions. Warns if the set or
any named entry doesn't exist.

## Usage

``` r
removeInitialConditionEntry(project, id, path)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character scalar, set id. Canonicalized.

- path:

  Character vector of molecule paths (length N).

## Value

The `project` object, invisibly.

## See also

Other parameters:
[`addInitialConditionEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addInitialConditionEntry.md),
[`addInitialConditions()`](https://esqlabs.github.io/esqlabsR/dev/reference/addInitialConditions.md),
[`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md),
[`addParameterSet()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterSet.md),
[`readInitialConditionsFromXLS()`](https://esqlabs.github.io/esqlabsR/dev/reference/readInitialConditionsFromXLS.md),
[`removeInitialConditions()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeInitialConditions.md),
[`removeParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeParameterEntry.md),
[`removeParameterSet()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeParameterSet.md)
