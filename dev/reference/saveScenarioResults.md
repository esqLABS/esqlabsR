# Save results of scenario simulations to csv.

Save results of scenario simulations to csv.

## Usage

``` r
saveScenarioResults(
  simulatedScenariosResults,
  projectConfiguration,
  outputFolder = NULL,
  saveSimulationsToPKML = TRUE
)
```

## Arguments

- simulatedScenariosResults:

  Named list with `simulation`, `results`, `outputValues`, and
  `population` as produced by
  [`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md).

- projectConfiguration:

  An instance of `ProjectConfiguration`

- outputFolder:

  Optional - path to the folder where the results will be stored. If
  `NULL` (default), a sub-folder in
  `ProjectConfiguration$outputFolder/SimulationResults/<DateSuffix>`.

- saveSimulationsToPKML:

  If `TRUE` (default), simulations corresponding to the results are
  saved to PKML along with the results.

## Value

`outputFolder` or the created output folder path, if no `outputFolder`
was provided.

## Details

For each scenario, a separate csv file will be created. If the scenario
is a population simulation, a population is stored along with the
results with the file name suffix `_population`. Results can be read
with the
[`loadScenarioResults()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadScenarioResults.md)
function.

## Examples

``` r
if (FALSE) { # \dontrun{
projectConfiguration <- esqlabsR::createProjectConfiguration()
scenarioConfigurations <- readScenarioConfigurationFromExcel(
  projectConfiguration = projectConfiguration
)
scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations)
simulatedScenariosResults <- runScenarios(
  scenarios = scenarios
)
saveScenarioResults(simulatedScenariosResults, projectConfiguration)
} # }
```
