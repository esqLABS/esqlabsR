# Generate DataCombined objects as defined in excel file

Generate DataCombined objects as defined in excel file

## Usage

``` r
createDataCombinedFromExcel(
  projectConfiguration,
  dataCombinedNames = NULL,
  plotGridNames = NULL,
  simulatedScenarios = NULL,
  observedData = NULL,
  stopIfNotFound = TRUE
)
```

## Arguments

- projectConfiguration:

  Object of class `ProjectConfiguration` that contains information about
  the output paths and the excel file where plots are defined.

- dataCombinedNames:

  Names of the DataCombined objects that will be created. If a
  DataCombined with a given name is not defined in the Excel file, an
  error is thrown. Can be used together with `plotGridNames`.

- plotGridNames:

  Names of the plot grid specified in the sheet `plotGrids`. Each data
  combined used by the specified plot grids will be created. Can be used
  together with `dataCombinedNames`.

- simulatedScenarios:

  A list of simulated scenarios as returned by
  [`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md)

- observedData:

  A list of `DataSet` objects

- stopIfNotFound:

  If TRUE (default), the function stops if any of the simulated results
  or observed data are not found. If FALSE a warning is printed.

## Value

A list of `DataCombined` objects, or an empty list if both
`dataCombinedNames` and `plotGridNames` are `NULL` or
`stopIfNotFound = TRUE` and the specified `DataCombined` could not be
created.
