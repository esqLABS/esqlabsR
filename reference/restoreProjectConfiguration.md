# Import project configuration from JSON to Excel files

Creates Excel configuration files from a JSON configuration file. This
allows for recreating the project configuration from version-controlled
JSON.

## Usage

``` r
restoreProjectConfiguration(
  jsonPath = "ProjectConfiguration.json",
  outputDir = NULL
)
```

## Arguments

- jsonPath:

  Path to the JSON configuration file. Defaults to
  "ProjectConfiguration.json".

- outputDir:

  Directory where the Excel files will be created. If NULL (default),
  the Excel files will be created in the same directory as the source
  JSON file.

## Value

A ProjectConfiguration object initialized with the regenerated
ProjectConfiguration.xlsx

## See also

Other project configuration snapshots:
[`projectConfigurationStatus()`](https://esqlabs.github.io/esqlabsR/reference/projectConfigurationStatus.md),
[`snapshotProjectConfiguration()`](https://esqlabs.github.io/esqlabsR/reference/snapshotProjectConfiguration.md)
