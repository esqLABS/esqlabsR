# Add user defined variability on parameters to a population from an excel file.

Add user defined variability on parameters to a population from an excel
file.

## Usage

``` r
extendPopulationFromXLS(population, XLSpath, sheet = NULL)
```

## Arguments

- population:

  Object of type `Population`

- XLSpath:

  Path to the excel file that stores the information of parameters. The
  file must have the columns "Container Path", "Parameter Name", "Mean",
  "SD", "Units", and "Distribution". Mean and SD values must be in the
  base units of the parameters.

- sheet:

  Name or the index of the sheet in the excel file. If `NULL`, the first
  sheet in the file is used.

## Details

The method reads the information from the specified excel sheet(s) and
calls `extendPopulationByUserDefinedParams`
