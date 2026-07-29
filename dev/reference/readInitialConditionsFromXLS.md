# Read initial values (molecule start values) from a structured Excel file.

Each excel sheet must consist of columns `Container Path`,
`Molecule Name`, `Is Present`, `Value`, `Units`, `Scale Divisor`, and
`Neg. Values Allowed`. Units are mandatory for every present molecule; a
present row with a blank `Units` cell is an error.

## Usage

``` r
readInitialConditionsFromXLS(filePath, sheets = NULL)
```

## Arguments

- filePath:

  Path to the excel file

- sheets:

  Names of the excel sheets containing the information about the initial
  values. Multiple sheets can be processed. If no sheets are provided,
  the first one in the Excel file is used.

## Value

A single list combining all processed sheets, containing vectors `paths`
with the full molecule paths, `values` with the values, and `units` with
the units the values are in. When multiple sheets are read, their rows
are merged into this one structure; if the same molecule path occurs
more than once, the last occurrence wins (last sheet, then last row). A
duplicate path, whether within a single sheet or repeated across sheets,
triggers a warning before the earlier value is replaced.

## See also

Other parameters:
[`addInitialConditionEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addInitialConditionEntry.md),
[`addInitialConditions()`](https://esqlabs.github.io/esqlabsR/dev/reference/addInitialConditions.md),
[`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md),
[`addParameterSet()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterSet.md),
[`removeInitialConditionEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeInitialConditionEntry.md),
[`removeInitialConditions()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeInitialConditions.md),
[`removeParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeParameterEntry.md),
[`removeParameterSet()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeParameterSet.md)
