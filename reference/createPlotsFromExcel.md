# Generate plots as defined in excel file `projectConfiguration$plotsFile`

Generate plots as defined in excel file `projectConfiguration$plotsFile`

## Usage

``` r
createPlotsFromExcel(
  plotGridNames = NULL,
  simulatedScenarios = NULL,
  observedData = NULL,
  dataCombinedList = NULL,
  projectConfiguration,
  outputFolder = NULL,
  stopIfNotFound = TRUE
)
```

## Arguments

- plotGridNames:

  Names of the plot grid specified in the sheet `plotGrids` for which
  the figures will be created. If `NULL` (default), all plot grids
  specified in the excel sheet will be created. If a plot grid with a
  given name does not exist, an error is thrown.

- simulatedScenarios:

  A list of simulated scenarios as returned by
  [`runScenarios()`](https://esqlabs.github.io/esqlabsR/reference/runScenarios.md).
  Can be `NULL` if no simulated data is required for the plots.

- observedData:

  A list of `DataSet` objects. Can be `NULL` if no observed data is
  required for the plots.

- dataCombinedList:

  A (named) list of `DataCombined` objects as input to create plots
  defined in the `plotGridNames` argument. Missing `DataCombined` will
  be created from the Excel file (default behavior). Defaults to `NULL`,
  in which case all `DataCombined` are created from Excel.

- projectConfiguration:

  Object of class `ProjectConfiguration` that contains information about
  the output paths and the excel file where plots are defined.

- outputFolder:

  Optional - path to the folder where the results will be stored. If
  `NULL` (default), `projectConfiguration$outputFolder` is used. Only
  relevant for plots specified for export in the `exportConfiguration`
  sheet.

- stopIfNotFound:

  If TRUE (default), the function stops if any of the simulated results
  or observed data are not found. If FALSE a warning is printed.

## Value

A list of `ggplot` objects
