# Validate `ScenarioConfiguration` objects

Validate `ScenarioConfiguration` objects

## Usage

``` r
.validateScenarioConfigurations(scenarioConfigurations)
```

## Arguments

- scenarioConfigurations:

  List of `ScenarioConfiguration` objects to validate.

## Details

Validates that all scenario configurations are of the correct type and
that population scenarios have a defined population ID. Throws an error
if validation fails.
