# Remove one or many parameter entries from a named parameter set

Removes parameter entries from the named set. `containerPath` and
`parameterName` accept parallel vectors of equal length N to remove all
N entries in a single call (and a single write to disk); a scalar call
(length-1 vectors) removes one entry. If every entry of the set is
removed, the set itself is auto-removed from `parameterSets`
definitions. Warns if the set or any named entry doesn't exist.

## Usage

``` r
removeParameterEntry(project, id, containerPath, parameterName)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character scalar, set id. Canonicalized.

- containerPath:

  Character vector of container paths (length N).

- parameterName:

  Character vector of parameter names (length N).

## Value

The `project` object, invisibly.

## See also

Other parameters:
[`addInitialConditionEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addInitialConditionEntry.md),
[`addInitialConditions()`](https://esqlabs.github.io/esqlabsR/dev/reference/addInitialConditions.md),
[`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md),
[`addParameterSet()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterSet.md),
[`readInitialConditionsFromXLS()`](https://esqlabs.github.io/esqlabsR/dev/reference/readInitialConditionsFromXLS.md),
[`removeInitialConditionEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeInitialConditionEntry.md),
[`removeInitialConditions()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeInitialConditions.md),
[`removeParameterSet()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeParameterSet.md)
