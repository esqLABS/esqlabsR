# Add scenario configurations to project Excel files

Adds scenario configurations to the project's Scenarios.xlsx file and
ensures that required application protocol sheets exist in the
Applications.xlsx file. This function handles the Excel file operations
for adding scenarios to a project.

## Usage

``` r
addScenarioConfigurationsToExcel(
  scenarioConfigurations,
  projectConfiguration,
  appendToExisting = TRUE
)
```

## Arguments

- scenarioConfigurations:

  Named list of `ScenarioConfiguration` objects to add to the project.

- projectConfiguration:

  A `ProjectConfiguration` object holding base information.

- appendToExisting:

  Logical. Whether to append new scenarios to existing ones in the
  scenarios file. If `FALSE`, the ENTIRE scenarios file will be
  overwritten with only the new scenarios. If `TRUE` (default), new
  scenarios will be added to existing ones.

## Value

Invisibly returns the names of the added scenarios.

## Details

This function performs the following operations:

- Checks for duplicate scenario names if `appendToExisting` is `TRUE`.

- Creates missing application protocol sheets in Applications.xlsx by
  extracting parameters from PKML files (both Events and Applications
  parameters).

- Writes scenario configurations to the Scenarios.xlsx file with proper
  structure.

- Manages output paths and their IDs in the OutputPaths sheet.

The function ensures that the Excel files are properly structured with
the following sheets:

- **Scenarios sheet**: Contains scenario definitions with columns for
  scenario name, individual/population IDs, parameter sheets,
  application protocol, simulation time, steady state settings, model
  file, and output path IDs.

- **OutputPaths sheet**: Contains output path IDs and their
  corresponding paths. When named vectors are used for `outputPaths` in
  scenario configurations, the names will be used as OutputPathId
  values.

- **Applications.xlsx**: Contains application protocol sheets with
  parameter definitions.

The function handles parameter extraction from PKML files, excluding
default parameters like "Volume" and "Application rate", and only
includes constant parameters.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create scenario configurations from PKML
scenarios <- createScenarioConfigurationsFromPKML(
  pkmlFilePaths = "path/to/simulation.pkml",
  projectConfiguration = projectConfiguration
)

# Add scenarios to project Excel files
addScenarioConfigurationsToExcel(
  scenarioConfigurations = scenarios,
  projectConfiguration = projectConfiguration,
  appendToExisting = TRUE
)
} # }
```
