# Add one or many parameter entries to a named parameter set

Adds parameter entries to the named set in `parameterSets` definitions.
`containerPath`, `parameterName`, `value`, and `units` accept parallel
vectors of equal length N to add all N entries in a single call (and a
single write to disk); a scalar call (length-1 vectors) adds one entry.
Building a large set with one vectorized call is far cheaper than a loop
of scalar calls, since each call rewrites the whole set file.

## Usage

``` r
addParameterEntry(
  project,
  id,
  containerPath,
  parameterName,
  value,
  units = NULL,
  overwrite = FALSE
)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character scalar, set id. Canonicalized; created if not present.

- containerPath:

  Character vector of container paths (length N).

- parameterName:

  Character vector of parameter names (length N).

- value:

  Numeric vector of values (length N).

- units:

  Character vector of units (length N). An entry in a base unit carries
  no unit: write it as `""` or `NA` (what an empty Units cell read from
  Excel gives you). `NULL` (the default) means no unit on any of the N
  entries.

- overwrite:

  Logical scalar. When `FALSE` (default), a duplicate
  `(containerPath, parameterName)` pair aborts. When `TRUE`, it
  overwrites the existing entry (last-write-wins).

## Value

The `project` object, invisibly.

## Details

Unlike the other `add*` functions, which abort on a missing parent, this
creates the parent set on demand if it does not yet exist (informing you
when it does). A duplicate `(containerPath, parameterName)` pair
(already in the set, or repeated within a single vectorized call) aborts
unless `overwrite = TRUE`, in which case the last value wins.

## See also

Other parameters:
[`addInitialConditionEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addInitialConditionEntry.md),
[`addInitialConditions()`](https://esqlabs.github.io/esqlabsR/dev/reference/addInitialConditions.md),
[`addParameterSet()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterSet.md),
[`readInitialConditionsFromXLS()`](https://esqlabs.github.io/esqlabsR/dev/reference/readInitialConditionsFromXLS.md),
[`removeInitialConditionEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeInitialConditionEntry.md),
[`removeInitialConditions()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeInitialConditions.md),
[`removeParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeParameterEntry.md),
[`removeParameterSet()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeParameterSet.md)
