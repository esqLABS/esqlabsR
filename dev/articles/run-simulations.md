# Run simulations

Once a project defines one or more scenarios, you run them with a single
function,
[`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md).
A scenario is a named, runnable simulation specification (a model, an
optional subject, the dosing, the parameters, the time grid, and the
outputs to record); designing them is covered in
[`vignette("design-scenarios")`](https://esqlabs.github.io/esqlabsR/dev/articles/design-scenarios.md).
Here we take a project that already has scenarios and focus on running
them and handling the results.

This article uses the worked example project bundled with the package,
so every chunk runs as written. Load it with
[`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md):

``` r

project <- loadProject(exampleProjectPath())

project$definitions$scenarios
#> <DefinitionList>
#> scenarios (3 definitions):
#>   • aciclovir_iv
#>   • aciclovir_iv_population
#>   • aciclovir_iv_steadystate
```

The example defines three scenarios: an individual scenario, a
population scenario, and an individual scenario that runs to steady
state first.

## Running scenarios

[`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md)
takes the project and, optionally, the names of the scenarios to run.
The first call in a session initializes PK-Sim, which takes a moment;
subsequent calls are faster.

To run a single scenario, name it:

``` r

results <- runScenarios(project, scenarios = "aciclovir_iv")

names(results)
#> [1] "aciclovir_iv"
```

To run a subset, pass a vector of names:

``` r

results <- runScenarios(
  project,
  scenarios = c("aciclovir_iv", "aciclovir_iv_steadystate")
)
```

To run every scenario in the project, leave `scenarios` as its default
`NULL`:

``` r

results <- runScenarios(project)
```

In this article we keep the runnable example to the single individual
scenario above, because each scenario builds and simulates a full PBPK
model and that work adds up quickly.

## The Scenario Result record

[`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md)
returns a named list keyed by scenario name. Each entry is one Scenario
Result, a plain list with four fields:

``` r

scenarioResult <- results[["aciclovir_iv"]]
```

- `simulation`: the initialized OSPS `Simulation` object, with all
  scenario parameters applied.
- `results`: the OSPS `SimulationResults` object produced by running
  that simulation.
- `outputValues`: the recorded output paths extracted into a data frame,
  ready for analysis or plotting.
- `population`: the OSPS `Population` for a population scenario, or
  `NULL` for an individual scenario.

A common point of confusion is worth stating directly: a Scenario Result
is **not** the OSPS `SimulationResults`. The Scenario Result is the
larger record that bundles the simulation, the results, the extracted
output values, and the population. The OSPS `SimulationResults` is only
the inner `results` field:

``` r

class(scenarioResult$results)
#> [1] "SimulationResults" "DotNetWrapper"     "NetObject"        
#> [4] "R6"
```

The `outputValues` field splits into the simulated `data` and its
`metaData`:

``` r

head(scenarioResult$outputValues$data)
#>   IndividualId Time
#> 1            0    0
#> 2            0    1
#> 3            0    2
#> 4            0    3
#> 5            0    4
#> 6            0    5
#>   Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
#> 1                                                                  0.000000
#> 2                                                                  2.734434
#> 3                                                                  7.854866
#> 4                                                                 13.133364
#> 5                                                                 18.282331
#> 6                                                                 23.266077
```

## Individual versus population scenarios

The `population` field is what distinguishes the two kinds of scenario
at run time. For an individual scenario, no population is attached and
the field is `NULL`:

``` r

scenarioResult$population
#> NULL
```

For a population scenario, the same field instead holds the sampled
`Population`, and `outputValues` then carries one trajectory per virtual
individual rather than a single curve.

## Saving and reloading results

Simulating a model is the slow step, so it is good practice to save
results once and reload them for downstream work such as redrawing a
figure, rather than re-simulating each time.
[`saveScenarioResults()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveScenarioResults.md)
writes each Scenario Result to disk: the results as a `.csv`, the
simulation as a `.pkml`, and, for population scenarios, the population
as a `<scenario>_population.csv`.

By default the destination is derived from the project’s output folder.
You can also pass an explicit `outputFolder`; here we send the output to
a temporary directory so nothing is written into the project tree:

``` r

resultsFolder <- withr::local_tempdir()

saveScenarioResults(
  results,
  project,
  outputFolder = resultsFolder
)
#> [1] "/tmp/RtmpQly0Hj/file28f770a84d03"
```

[`loadScenarioResults()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadScenarioResults.md)
reads the saved files back, given the scenario names and the folder they
were written to. Passing the `project` restricts the reloaded output
values to each scenario’s declared output paths, so the reloaded column
set matches what
[`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md)
produced. The reload restores all four fields of each Scenario Result,
including `population` for population scenarios:

``` r

reloaded <- loadScenarioResults(
  scenarios = "aciclovir_iv",
  resultsFolder = resultsFolder,
  project = project
)
```

The reloaded record has the same shape as the one
[`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md)
returned, so any code that consumes a Scenario Result works on either.

## Run options and parallel execution

[`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md)
accepts a `simulationRunOptions` argument, an
[`ospsuite::SimulationRunOptions`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/SimulationRunOptions.html)
object, to control how the underlying simulations run. It governs solver
settings and whether scenarios are simulated in parallel. Leaving it at
its default `NULL` uses the package defaults, which is the right choice
for most analyses. Reach for an explicit `SimulationRunOptions` when you
need to tune the run, for example to enable parallel execution across
multiple cores when running many scenarios.

## Where to go next

Now that you have simulated scenarios and their results in hand, you can
move on to visualizing them, which is covered in
[`vignette("plot-results")`](https://esqlabs.github.io/esqlabsR/dev/articles/plot-results.md).
