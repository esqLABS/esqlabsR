# Recycle or validate vector arguments for scenario creation

Recycle or validate vector arguments for scenario creation

## Usage

``` r
.recycleOrValidateVector(arg, argName, nScenarios)
```

## Arguments

- arg:

  Vector argument to recycle or validate.

- argName:

  Character string name of the argument for error messages.

- nScenarios:

  Integer number of scenarios to create.

## Value

Vector with the correct length for all scenarios, or NULL if input was
NULL.

## Details

Handles vector recycling for scenario parameters. Single values are
recycled to all scenarios, vectors with the correct length are used
as-is, and invalid lengths throw an error.
