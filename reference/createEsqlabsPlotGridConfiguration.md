# Create an instance of `PlotGridConfiguration` R6 class

An instance of `PlotGridConfiguration` R6 class from `{tlf}` package is
needed for creating a grid of multiple visualizations created using the
`{ospsuite}` package.

The default attributes of the class are chosen to reflect the corporate
standards adopted by esqLABS GmbH.

## Usage

``` r
createEsqlabsPlotGridConfiguration()
```

## Value

An instance of `PlotGridConfiguration` R6 class.

## See also

Other create-plotting-configurations:
[`createEsqlabsExportConfiguration()`](https://esqlabs.github.io/esqlabsR/reference/createEsqlabsExportConfiguration.md),
[`createEsqlabsPlotConfiguration()`](https://esqlabs.github.io/esqlabsR/reference/createEsqlabsPlotConfiguration.md)

## Examples

``` r
createEsqlabsPlotGridConfiguration()
#> Warning: ospsuite.utils::Printable was deprecated in ospsuite.utils 1.6.2.
#> ℹ Please use ospsuite.utils::ospPrint*() instead.
#> ℹ The deprecated feature was likely used in the ospsuite.utils package.
#>   Please report the issue at
#>   <https://github.com/open-systems-pharmacology/OSPSuite.RUtils/issues>.
#> PlotGridConfiguration: 
#>    Plot grid annotations: NULL 
#>      Title: NULL 
#>      Subtitle: NULL 
#>      Caption: NULL 
#>    Plot grid arrangement: NULL 
#>      Number of plots included: 0 
#>      Number of columns in the grid: NULL 
#>      Number of rows in the grid: NULL 
#>      Arranged in row-major order: NULL 
#>    Individual plot tags: NULL 
#>      Tag level format: a 
#>      Tag level prefix: NULL 
#>      Tag level suffix: NULL 
#>      Tag level separator: NULL 
```
