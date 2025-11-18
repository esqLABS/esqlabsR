# Create scenario configurations from PKML files

Creates scenario configurations from PKML files by extracting available
information such as applications, output paths, and simulation time
settings. This function creates scenario configuration objects that can
be used with the esqlabsR workflow.

## Usage

``` r
createScenarioConfigurationsFromPKML(
  pkmlFilePaths,
  projectConfiguration,
  scenarioNames = NULL,
  individualId = NULL,
  populationId = NULL,
  applicationProtocols = NULL,
  paramSheets = NULL,
  outputPaths = NULL,
  simulationTime = NULL,
  simulationTimeUnit = NULL,
  steadyState = FALSE,
  steadyStateTime = NULL,
  steadyStateTimeUnit = NULL,
  readPopulationFromCSV = FALSE
)
```

## Arguments

- pkmlFilePaths:

  Character vector of paths to PKML files to create scenarios from. Can
  be a single string (recycled for all scenarios) or a vector with the
  same length as the number of scenarios being created (determined by
  the longest vector argument).

- projectConfiguration:

  A `ProjectConfiguration` object holding base information.

- scenarioNames:

  Character vector. Optional custom names for the scenarios. If `NULL`
  (default), scenario names will be extracted from the simulation names
  in the PKML files. If provided, must have the same length as
  `pkmlFilePaths`.

- individualId:

  Character vector. Optional individual IDs to use for scenarios. If
  `NULL` (default), no individual will be specified. Can be a single
  string (recycled for all scenarios) or a vector with the same length
  as `pkmlFilePaths`.

- populationId:

  Character vector. Optional population IDs to use for scenarios. If
  `NULL` (default), no population will be specified. If provided, sets
  simulation type to "Population". Can be a single string (recycled for
  all scenarios) or a vector with the same length as `pkmlFilePaths`.

- applicationProtocols:

  Character vector. Optional application protocol names to use for
  scenarios. If `NULL` (default), application protocols will be set to
  the scenario name. Can be a single string (recycled for all scenarios)
  or a vector with the same length as `pkmlFilePaths`.

- paramSheets:

  Character vector. Optional parameter sheet names to apply to
  scenarios. If `NULL` (default), no parameter sheets will be applied.
  Can be a single string (recycled for all scenarios) or a vector with
  the same length as `pkmlFilePaths`. If providing multiple sheets per
  scenario, separate them with commas in the string.

- outputPaths:

  Character vector or named vector. Optional output paths to use for
  scenarios. If `NULL` (default), output paths will be extracted from
  the PKML files' output selections. Can be a single string (recycled
  for all scenarios) or a vector with the same length as
  `pkmlFilePaths`. If providing multiple paths per scenario, separate
  them with commas in the string. Named vectors are supported where
  names serve as aliases for the paths, e.g., c("plasma" =
  "Organism\|VenousBlood\|Plasma\|AKB-9090\|Concentration in
  container").

- simulationTime:

  Character vector. Optional simulation time to use for scenarios as
  character strings containing one or multiple time intervals separated
  by a ';'. Each time interval is a triplet of values \<StartTime,
  EndTime, Resolution\>, where `Resolution` is the number of simulated
  points per time unit defined in the `simulationTimeUnit`. If `NULL`
  (default), simulation time will be extracted from the PKML files'
  output schema intervals. Can be a single string (recycled for all
  scenarios) or a vector with the same length as `pkmlFilePaths`.

- simulationTimeUnit:

  Character vector. Optional simulation time units. Only used when
  `simulationTime` is provided. If `NULL` (default), will be extracted
  from the PKML file's output schema intervals, or set to "min"
  (minutes) if not available. Can be a single string (recycled for all
  scenarios) or a vector with the same length as `pkmlFilePaths`.

- steadyState:

  Logical vector. Whether to simulate steady-state for each scenario.
  Default is `FALSE`. Can be a single logical value (recycled for all
  scenarios) or a vector with the same length as `pkmlFilePaths`.

- steadyStateTime:

  Numeric vector. Steady-state times. Only used when corresponding
  `steadyState` is `TRUE`. If `NULL` (default), no steady-state time
  will be set. Can be a single numeric value (recycled for all
  scenarios) or a vector with the same length as `pkmlFilePaths`.

- steadyStateTimeUnit:

  Character vector. Steady-state time units. Only used when
  `steadyState = TRUE` and `steadyStateTime` is provided. If `NULL`
  (default), "min" will be used. Can be a single string (recycled for
  all scenarios) or a vector with the same length as `pkmlFilePaths`.

- readPopulationFromCSV:

  Logical vector. Whether to read population from CSV for each scenario.
  Default is `FALSE`. Can be a single logical value (recycled for all
  scenarios) or a vector with the same length as `pkmlFilePaths`.

## Value

A named list of `ScenarioConfiguration` objects with the names being the
scenario names.

## Details

This function extracts the following information from PKML files:

- **Applications**: Application protocol names (defaults to scenario
  name).

- **Output paths**: All selected outputs for the simulation from
  `outputSelections$allOutputs`.

- **Simulation time**: Time intervals with start time, end time, and
  resolution from `outputSchema$intervals`.

- **Simulation time unit**: Time unit from the output schema intervals
  (e.g., "h" for hours).

### Vector Arguments and Recycling

All arguments support vectorization to create scenarios with different
parameter values:

- **Length 1**: The value is recycled (applied to all scenarios).

- **Length \> 1**: All vector arguments must have the same length, which
  determines the number of scenarios.

- **Mixed lengths**: An error is thrown if vector arguments have
  inconsistent lengths.

The number of scenarios created is determined by the longest vector
argument. All shorter vectors (including `pkmlFilePaths`) are recycled
to match this length.

This allows you to efficiently create multiple scenarios in several
ways:

- **Same PKML, different settings**: Use a single PKML file with vectors
  of different parameter values.

- **Different PKMLs, same settings**: Use multiple PKML files with
  single parameter values.

- **Different PKMLs, different settings**: Use vectors of both PKML
  files and parameter values.

The function handles duplicate scenario names by appending indices
(e.g., "Scenario_1", "Scenario_2"). It creates scenario configurations
but does not write them to Excel files. Use
[`addScenarioConfigurationsToExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenarioConfigurationsToExcel.md)
to add the scenarios to the project's Excel files.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create default project configuration
projectConfiguration <- createDefaultProjectConfiguration()

# Create scenarios from a single PKML file
pkmlPath <- "path/to/simulation.pkml"
scenarios <- createScenarioConfigurationsFromPKML(
  pkmlFilePaths = pkmlPath,
  projectConfiguration = projectConfiguration
)

# Add scenarios to Excel configuration
addScenarioConfigurationsToExcel(
  scenarioConfigurations = scenarios,
  projectConfiguration = projectConfiguration
)

# Create scenarios from multiple PKML files with custom names
pkmlPaths <- c("path/to/sim1.pkml", "path/to/sim2.pkml")
scenarios <- createScenarioConfigurationsFromPKML(
  pkmlFilePaths = pkmlPaths,
  projectConfiguration = projectConfiguration,
  scenarioNames = c("Scenario1", "Scenario2")
)

# Add multiple scenarios to configuration
addScenarioConfigurationsToExcel(
  scenarioConfigurations = scenarios,
  projectConfiguration = projectConfiguration
)

# Example of vector recycling - single value applied to all scenarios
scenarios <- createScenarioConfigurationsFromPKML(
  pkmlFilePaths = c("sim1.pkml", "sim2.pkml", "sim3.pkml"),
  projectConfiguration = projectConfiguration,
  individualId = "Individual_001", # Recycled to all scenarios
  steadyState = TRUE,              # Recycled to all scenarios
  steadyStateTime = 1000           # Recycled to all scenarios
)

# Example of vector arguments - different values per scenario
scenarios <- createScenarioConfigurationsFromPKML(
  pkmlFilePaths = c("pediatric.pkml", "adult.pkml", "elderly.pkml"),
  projectConfiguration = projectConfiguration,
  scenarioNames = c("Pediatric", "Adult", "Elderly"),
  individualId = c("Child_001", "Adult_001", "Elderly_001"),
  applicationProtocols = c("Pediatric_Dose", "Standard_Dose", "Reduced_Dose"),
  steadyState = c(FALSE, TRUE, TRUE),
  steadyStateTime = c(NA, 2000, 1500)
)

# Example of PKML recycling - same model, different settings
scenarios <- createScenarioConfigurationsFromPKML(
  pkmlFilePaths = "base_model.pkml",                    # Single PKML recycled
  projectConfiguration = projectConfiguration,
  scenarioNames = c("LowDose", "MediumDose", "HighDose"),
  individualId = c("Patient1", "Patient2", "Patient3"),
  applicationProtocols = c("Low_Protocol", "Med_Protocol", "High_Protocol"),
  steadyState = c(FALSE, TRUE, TRUE)                   # Different settings per scenario
)
} # }
```
