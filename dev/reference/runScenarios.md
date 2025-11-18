# Run a set of scenarios.

Run a set of scenarios.

## Usage

``` r
runScenarios(scenarios, simulationRunOptions = NULL)
```

## Arguments

- scenarios:

  List of `Scenario` objects to be simulated.

- simulationRunOptions:

  Object of type `SimulationRunOptions` that will be passed to
  simulation runs. If `NULL`, default options are used.

## Value

A named list, where the names are scenario names, and the values are
lists with the entries `simulation` being the initialized `Simulation`
object with applied parameters, `results` being `SimulatioResults`
object produced by running the simulation, `outputValues` the output
values of the `SimulationResults`, and `population` the `Population`
object if the scenario is a population simulation.

## Details

If simulation of a scenario fails, a warning is produced, and the
`outputValues` for this scenario is `NULL`.
