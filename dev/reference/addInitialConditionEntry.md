# Add one or many entries to a named initial-condition set

Adds molecule start-value entries to the named set in
`initialConditions` definitions. `path`, `value`, and `unit` accept
parallel vectors of equal length N to add all N entries in a single call
(and a single write to disk); a scalar call (length-1 vectors) adds one
entry. Building a large set with one vectorized call is far cheaper than
a loop of scalar calls, since each call rewrites the whole set file.

## Usage

``` r
addInitialConditionEntry(project, id, path, value, unit, overwrite = FALSE)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character scalar, set id. Canonicalized; created if not present.

- path:

  Character vector of molecule paths (length N).

- value:

  Numeric vector of start values (length N).

- unit:

  Character vector of units (length N). A unit is mandatory for a
  molecule start value; a blank unit is rejected.

- overwrite:

  Logical scalar. When `FALSE` (default), a duplicate `path` aborts.
  When `TRUE`, it overwrites the existing entry (last-write-wins).

## Value

The `project` object, invisibly.

## Details

Unlike the other `add*` functions, which abort on a missing parent, this
creates the parent set on demand if it does not yet exist (informing you
when it does). A duplicate `path` (already in the set, or repeated
within a single vectorized call) aborts unless `overwrite = TRUE`, in
which case the last value wins.

## See also

Other parameters:
[`addInitialConditions()`](https://esqlabs.github.io/esqlabsR/dev/reference/addInitialConditions.md),
[`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md),
[`addParameterSet()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterSet.md),
[`readInitialConditionsFromXLS()`](https://esqlabs.github.io/esqlabsR/dev/reference/readInitialConditionsFromXLS.md),
[`removeInitialConditionEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeInitialConditionEntry.md),
[`removeInitialConditions()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeInitialConditions.md),
[`removeParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeParameterEntry.md),
[`removeParameterSet()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeParameterSet.md)
