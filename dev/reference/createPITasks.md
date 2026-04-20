# Create Parameter Identification tasks

Creates ParameterIdentification objects from PI configurations. Each
PITaskConfiguration produces one ParameterIdentification object.

## Usage

``` r
createPITasks(
  piTaskConfigurations,
  observedData = NULL,
  stopIfParameterNotFound = TRUE
)
```

## Arguments

- piTaskConfigurations:

  Named list of `PITaskConfiguration` objects

- observedData:

  Named list of `DataSet` objects. When provided, datasets are looked up
  by the name in the `DataSet` column of `PIOutputMappings`. When `NULL`
  (default), observed data is loaded internally using the
  `ObservedDataSheet` column.

- stopIfParameterNotFound:

  Logical. If `TRUE` (default), an error is thrown when a parameter
  defined in the scenario configuration cannot be found in the
  simulation. Set to `FALSE` to continue silently when scenario
  parameters are absent from the model.

## Value

Named list of `ParameterIdentification` objects

## Examples

``` r
if (FALSE) { # \dontrun{
projectConfiguration <- createProjectConfiguration(
  exampleProjectConfigurationPath(),
  ignoreVersionCheck = TRUE
)
piTaskConfigurations <- readPITaskConfigurationFromExcel(
  projectConfiguration = projectConfiguration
)
piTasks <- createPITasks(piTaskConfigurations)
} # }
```
