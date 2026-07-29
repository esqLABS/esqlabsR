# Save results of scenario simulations to csv.

Save results of scenario simulations to csv.

## Usage

``` r
saveScenarioResults(
  simulatedScenariosResults,
  project,
  outputFolder = NULL,
  saveSimulationsToPKML = TRUE
)
```

## Arguments

- simulatedScenariosResults:

  Named list with `simulation`, `results`, `outputValues`, and
  `population` as produced by
  [`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md).

- project:

  A `Project` object (loaded with
  [`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md))
  providing the `outputFolder` used to derive the default destination.

- outputFolder:

  Optional - path to the folder where the results will be stored. If
  `NULL` (default), a sub-folder in
  `project$paths$outputFolder/SimulationResults/<DateSuffix>`.

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
project <- loadProject("Project.json")
simulatedScenariosResults <- runScenarios(project)
saveScenarioResults(simulatedScenariosResults, project)
} # }
```
