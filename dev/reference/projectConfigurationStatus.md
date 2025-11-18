# Check if Excel configuration files are in sync with JSON snapshot

Compares Excel configuration files against their JSON snapshot to
determine if they are synchronized. The JSON snapshot is considered the
source of truth.

## Usage

``` r
projectConfigurationStatus(
  projectConfig = "ProjectConfiguration.xlsx",
  jsonPath = NULL,
  silent = FALSE
)
```

## Arguments

- projectConfig:

  A ProjectConfiguration object or path to ProjectConfiguration excel
  file. Defaults to "ProjectConfiguration.xlsx".

- jsonPath:

  Path to the JSON configuration file. If NULL (default), the function
  will look for a JSON file with the same name as the
  ProjectConfiguration file but with .json extension.

- silent:

  Logical indicating whether to suppress informational messages.
  Defaults to FALSE.

## Value

A list with components:

- in_sync:

  Logical indicating whether all files are synchronized

- details:

  A list with detailed comparison results for each file

- unsaved_changes:

  Logical indicating whether the ProjectConfiguration object has unsaved
  modifications

## See also

Other project configuration snapshots:
[`restoreProjectConfiguration()`](https://esqlabs.github.io/esqlabsR/dev/reference/restoreProjectConfiguration.md),
[`snapshotProjectConfiguration()`](https://esqlabs.github.io/esqlabsR/dev/reference/snapshotProjectConfiguration.md)
