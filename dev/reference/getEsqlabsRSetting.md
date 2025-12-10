# Get the value of a global esqlabsR setting.

Get the value of a global esqlabsR setting.

## Usage

``` r
getEsqlabsRSetting(settingName)
```

## Arguments

- settingName:

  String name of the setting

## Value

Value of the setting stored in esqlabsEnv. If the setting does not
exist, an error is thrown.

## Examples

``` r
getEsqlabsRSetting("packageVersion")
#>      version 
#> "5.5.1.9003" 
getEsqlabsRSetting("packageName")
#> [1] "esqlabsR"
```
