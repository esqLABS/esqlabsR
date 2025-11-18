# Load data from pkml

Loads data sets that are exported as pkml. The files must be located in
the folder `projectConfiguration$dataFolder`, subfolder `pkml`. and be
named `projectConfiguration$dataFile`.

## Usage

``` r
loadObservedDataFromPKML(projectConfiguration, obsDataNames = NULL)
```

## Arguments

- projectConfiguration:

  Object of class `ProjectConfiguration` containing the necessary
  information.

- obsDataNames:

  String or a list of strings defining data sets to load If `NULL`
  (default), all data sets located in the folder are loaded. Must not
  contain the ".pkml" file extension.

## Value

A named list of `DataSet` objects, with names being the names of the
data sets.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create default project configuration
projectConfiguration <- createProjectConfiguration()
dataSets <- loadObservedData(projectConfiguration)
} # }
```
