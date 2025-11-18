# Read scenario definition(s) from Excel file

Read scenario definition(s) from Excel file

## Usage

``` r
readScenarioConfigurationFromExcel(scenarioNames = NULL, projectConfiguration)
```

## Arguments

- scenarioNames:

  Character vector. Names of the scenarios that are defined in the Excel
  file. If `NULL` (default), all scenarios specified in the Excel file
  will be created.

- projectConfiguration:

  A `ProjectConfiguration` object holding base information.

## Value

A named list of `ScenarioConfiguration` objects with the names of the
list being scenario names.

## Details

Reads scenario definition from the Excel file defined in
`ProjectConfiguration` and creates `ScenarioConfiguration` objects with
new information. If a scenario that is specified in `scenarioNames` is
not found in the Excel file, an error is thrown.

The function expects the Excel file to have a "Scenarios" sheet with the
following columns: `Scenario_name`, `IndividualId`, `PopulationId`,
`ReadPopulationFromCSV`, `ModelParameterSheets`, `ApplicationProtocol`,
`SimulationTime`, `SimulationTimeUnit`, `SteadyState`,
`SteadyStateTime`, `SteadyStateTimeUnit`, `ModelFile`, `OutputPathsIds`.
It also expects an "OutputPaths" sheet with `OutputPathId` and
`OutputPath` columns.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create default ProjectConfiguration
projectConfiguration <- createProjectConfiguration()
scenarioName <- "MyScenario"
# Read scenario definition from Excel
scenarioConfiguration <- readScenarioConfigurationFromExcel(
  scenarioNames = scenarioName,
  projectConfiguration
)[[scenarioName]]
} # }
```
