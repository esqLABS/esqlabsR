# Load simulated scenarios from csv and pkml.

Load simulated scenarios from csv and pkml.

## Usage

``` r
loadScenarioResults(scenarios, resultsFolder, project = NULL)
```

## Arguments

- scenarios:

  Names of simulated scenarios

- resultsFolder:

  Path to the folder where simulation results as csv and the
  corresponding simulations as pkml are located.

- project:

  Optional `Project` object (loaded with
  [`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md))
  whose scenarios declare the output paths that were run. When supplied,
  the reloaded `outputValues` are restricted to each scenario's declared
  output paths, so the reloaded column set matches what
  [`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md)
  produced. When `NULL` (default), all output paths recorded in the csv
  are extracted.

## Value

A named list keyed by scenario name. Each entry mirrors the record
produced by
[`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md):
`simulation` (the initialized `Simulation` object with applied
parameters), `results` (the `SimulationResults` object reloaded from
csv), `outputValues` (the output values extracted for the scenario's
declared output paths when `project` is supplied, otherwise for all
recorded output paths), and `population` (the `Population` reloaded from
`<scenario>_population.csv` for population scenarios, or `NULL` for
individual scenarios).

## Details

This function requires simulation results AND the corresponding
simulation files being located in the same folder (`resultsFolder`) and
have the names of the scenarios.

## Examples

``` r
if (FALSE) { # \dontrun{
# First simulate scenarios and save the results
project <- loadProject("Project.json")
simulatedScenariosResults <- runScenarios(project)
resultsFolder <- saveScenarioResults(simulatedScenariosResults, project)

# Now load the results, restricting to each scenario's declared output paths
simulatedScenariosResults <- loadScenarioResults(
  scenarios = names(project$definitions$scenarios),
  resultsFolder = resultsFolder,
  project = project
)
} # }
```
