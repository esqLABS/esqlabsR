# Load data from excel

Loads data sets from excel. The excel file containing the data must be
located in the folder `projectConfiguration$dataFolder` and be named
`projectConfiguration$dataFile`. Importer configuration file must be
located in the same folder and named
`projectConfiguration$dataImporterConfigurationFile`.

## Usage

``` r
loadObservedData(
  projectConfiguration,
  sheets = NULL,
  importerConfiguration = NULL
)
```

## Arguments

- projectConfiguration:

  Object of class `ProjectConfiguration` containing the necessary
  information.

- sheets:

  String or a list of strings defining which sheets to load. If `NULL`
  (default), all sheets within the file are loaded. This parameter takes
  precedence over any sheets defined in `importerConfiguration`: the
  function always sets `importerConfiguration$sheets` to `NULL` before
  calling
  [`ospsuite::loadDataSetsFromExcel()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/loadDataSetsFromExcel.html),
  so the passed `importerConfiguration` object is mutated as a side
  effect.

- importerConfiguration:

  `DataImporterConfiguration` object used to load the data. If `NULL`
  (default), default esqlabs importer configuration as defined in
  `projectConfiguration$dataImporterConfigurationFile` will be used.
  Note: the `sheets` property of this object is always set to `NULL` by
  this function (see the `sheets` parameter description).

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
