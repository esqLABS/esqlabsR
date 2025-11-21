# Validate parameter list structure

Validate parameter list structure

## Usage

``` r
.validateParametersStructure(
  parameterStructure,
  argumentName = NULL,
  nullAllowed = FALSE
)
```

## Arguments

- parameterStructure:

  Object to be checked. Expected is a named list with names "paths",
  "values", and "units".

## Value

`TRUE` if validation succeeded (silently). Throws an error otherwise.
