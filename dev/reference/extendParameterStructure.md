# Extend parameters structure with new entries

Extend parameters structure with new entries

## Usage

``` r
extendParameterStructure(parameters, newParameters)
```

## Arguments

- parameters:

  A parameter structure (a list with elements `paths`, `values`, and
  `units`) or `NULL`. If `NULL`, it is treated as an empty parameter
  structure.

- newParameters:

  A parameter structure (a list with elements `paths`, `values`, and
  `units`) or `NULL`. If `NULL`, it is treated as an empty parameter
  structure whose entries will be added to or overwrite those in
  `parameters`.

## Value

Updated list of parameter paths, values, and units

## Details

This function adds new parameter entries from `newParameters` to
`parameters`. If an entry with the same path is already present in
`parameters`, its value and unit will be overwritten with the values
from `newParameters`.
