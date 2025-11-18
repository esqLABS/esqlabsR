# Read parameter values from a structured Excel file. Each excel sheet must consist of columns 'Container Path', 'Parameter Name', 'Value', and 'Units'

Read parameter values from a structured Excel file. Each excel sheet
must consist of columns 'Container Path', 'Parameter Name', 'Value', and
'Units'

## Usage

``` r
readParametersFromXLS(paramsXLSpath, sheets = NULL)
```

## Arguments

- paramsXLSpath:

  Path to the excel file

- sheets:

  Names of the excel sheets containing the information about the
  parameters. Multiple sheets can be processed. If no sheets are
  provided, the first one in the Excel file is used.

## Value

A list containing vectors `paths` with the full paths to the parameters,
`values` the values of the parameters, and `units` with the units the
values are in.
