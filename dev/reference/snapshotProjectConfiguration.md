# Export project configuration Excel files to JSON

Exports all Excel configuration files in the Configurations folder of an
esqlabsR project to a single JSON file. This allows for easier version
control and programmatic manipulation.

## Usage

``` r
snapshotProjectConfiguration(
  projectConfig = "ProjectConfiguration.xlsx",
  outputDir = NULL,
  ...
)
```

## Arguments

- projectConfig:

  A ProjectConfiguration object or path to ProjectConfiguration excel
  file. Defaults to "ProjectConfiguration.xlsx".

- outputDir:

  Directory where the JSON file will be saved. If NULL (default), the
  JSON file will be created in the same directory as the source Excel
  file.

- ...:

  Additional arguments.

## Value

Invisibly returns the exported configuration data structure

## See also

Other project configuration snapshots:
[`projectConfigurationStatus()`](https://esqlabs.github.io/esqlabsR/dev/reference/projectConfigurationStatus.md),
[`restoreProjectConfiguration()`](https://esqlabs.github.io/esqlabsR/dev/reference/restoreProjectConfiguration.md)
