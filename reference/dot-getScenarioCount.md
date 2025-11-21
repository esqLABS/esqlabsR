# Get the number of scenarios to create based on vector arguments

Get the number of scenarios to create based on vector arguments

## Usage

``` r
.getScenarioCount(pkmlFilePaths, ...)
```

## Arguments

- pkmlFilePaths:

  Character vector of PKML file paths.

- ...:

  Other vector arguments to check for length consistency.

## Value

Integer number of scenarios to create.

## Details

Determines the number of scenarios to create based on the length of
vector arguments. All vector arguments with length \> 1 must have the
same length, which determines the final number of scenarios.
