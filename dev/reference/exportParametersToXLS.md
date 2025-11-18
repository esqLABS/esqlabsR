# Export simulation parameters to excel

Creates an excel file with information from the passed parameters. The
excel sheet will contain columns "Container Path", "Parameter Name",
"Value", and "Units". The resulting file can be loaded in `MoBi` or in
`R` with the function
[`readParametersFromXLS()`](https://esqlabs.github.io/esqlabsR/dev/reference/readParametersFromXLS.md).

## Usage

``` r
exportParametersToXLS(parameters, paramsXLSpath, sheet = NULL, append = FALSE)
```

## Arguments

- parameters:

  A single or a list of `Parameter` objects

- paramsXLSpath:

  Path to the excel file

- sheet:

  (Optional) name of the excel sheet

- append:

  If TRUE, appends parameters to existing file. If the file exists but
  the sheet doesn't exist, creates a new sheet. If FALSE (default),
  overwrites the existing file.
