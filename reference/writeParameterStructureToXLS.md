# Write parameter structure to excel that can be loaded in MoBi

Write parameter structure to excel that can be loaded in MoBi

## Usage

``` r
writeParameterStructureToXLS(
  parameterStructure,
  paramsXLSpath,
  sheet = NULL,
  append = FALSE
)
```

## Arguments

- parameterStructure:

  A list containing vectors `paths` with the full paths to the
  parameters, `values` the values of the parameters, and `units` with
  the units the values are in.

- paramsXLSpath:

  Path to the excel file

- sheet:

  (Optional) name of the excel sheet

- append:

  If TRUE, the existing excel file/sheet will be appended with the new
  parameter structure. If FALSE (default), the existing file will be
  overwritten.

## Examples

``` r
if (FALSE) { # \dontrun{
params <- list(
  paths = c("Container1|Path1", "Container|Second|Third|Path2"),
  values = c(1, 2),
  units = c("", "µmol")
)

writeParameterStructureToXLS(params, "test.xlsx")
} # }
```
