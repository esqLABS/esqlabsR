# Extend parameters structure with new entries

Extend parameters structure with new entries

## Usage

``` r
extendParameterStructure(parameters, newParameters)
```

## Arguments

- parameters:

  A list containing vectors `paths` with the full paths to the
  parameters, `values` the values of the parameters, and `units` with
  the units the values are in. This list will be extended.

- newParameters:

  A list containing vectors 'paths' with the full paths to the
  parameters, 'values' the values of the parameters, and 'units' with
  the units the values are in. Entries from this list will extend or
  overwrite the list `parameters`

## Value

Updated list of parameter paths, values, and units

## Details

This function adds new parameter entries from `newParameters` to
`parameters`. If an entry with the same path is already present in
`parameters`, its value and unit will be overwritten with the values
from `newParameters`.
