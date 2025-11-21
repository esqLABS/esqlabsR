# Create an instance of `ExportConfiguration` R6 class

An instance of `ExportConfiguration` R6 class from `{tlf}` package is
needed for saving the plots and plot grids created using the
`{ospsuite}` package.

The default attributes of the class are chosen to reflect the corporate
standards adopted by esqLABS GmbH.

## Usage

``` r
createEsqlabsExportConfiguration(outputFolder)
```

## Arguments

- outputFolder:

  Path to the folder where the results will be stored.

## Value

An instance of `ExportConfiguration` R6 class.

## See also

Other create-plotting-configurations:
[`createEsqlabsPlotConfiguration()`](https://esqlabs.github.io/esqlabsR/reference/createEsqlabsPlotConfiguration.md),
[`createEsqlabsPlotGridConfiguration()`](https://esqlabs.github.io/esqlabsR/reference/createEsqlabsPlotGridConfiguration.md)

## Examples

``` r
myProjConfig <- ProjectConfiguration$new()
createEsqlabsExportConfiguration(myProjConfig$outputFolder)
#> Format: png
#> Width: 18 cm
#> Height: 9 cm
#> Resolution: 300 dots per inch
```
