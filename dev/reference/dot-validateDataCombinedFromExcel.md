# Validate and process the 'DataCombined' sheet

Validate and process the 'DataCombined' sheet

## Usage

``` r
.validateDataCombinedFromExcel(
  dfDataCombined,
  simulatedScenarios,
  observedData,
  stopIfNotFound
)
```

## Arguments

- dfDataCombined:

  Data frame created by reading the ' DataCombined' sheet

- simulatedScenarios:

  List of simulated scenarios as created by
  [`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md)

- observedData:

  Observed data objects

- stopIfNotFound:

  if `TRUE`, throw an error if a simulated result of an observed data
  are not found

## Value

Processed `dfDataCombined`
