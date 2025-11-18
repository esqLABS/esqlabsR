# Create `Scenario` objects from `ScenarioConfiguration` objects

Load simulation. Apply parameters from global XLS. Apply individual
physiology. Apply individual model parameters. Set simulation outputs.
Set simulation time. initializeSimulation(). Create population

## Usage

``` r
createScenarios(
  scenarioConfigurations,
  customParams = NULL,
  stopIfParameterNotFound = TRUE
)
```

## Arguments

- scenarioConfigurations:

  List of `ScenarioConfiguration` objects to be simulated. See
  `createScenarios()` for details.

- customParams:

  A list containing vectors 'paths' with the full paths to the
  parameters, 'values' the values of the parameters, and 'units' with
  the units the values are in. The values will be applied to all
  scenarios.

- stopIfParameterNotFound:

  Boolean. If `TRUE` (default) and a custom parameter is not found, an
  error is thrown. If `FALSE`, non-existing parameters are ignored.

## Value

Named list of `Scenario` objects.
