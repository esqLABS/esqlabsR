# Write scenarios to Excel file

Write scenarios to Excel file

## Usage

``` r
.writeScenariosToExcel(
  scenarioConfigurations,
  projectConfiguration,
  appendToExisting
)
```

## Arguments

- scenarioConfigurations:

  List of `ScenarioConfiguration` objects to write.

- projectConfiguration:

  A `ProjectConfiguration` object containing file paths.

- appendToExisting:

  Logical. Whether to append to existing scenarios or overwrite.

## Details

Internal function that handles the actual writing of scenario
configurations to Excel files. Creates both the Scenarios and
OutputPaths sheets with proper data types and structure. Handles merging
with existing data when appending.
