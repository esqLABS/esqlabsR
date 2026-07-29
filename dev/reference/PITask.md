# Create a Parameter Identification task

Builds a plain-data `PITask` record bundling the scenarios, the
optimisation variables
([`PIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIParameter.md)
records), the output mappings
([`PIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIOutputMapping.md)
records), and the solver configuration of one Parameter Identification
run.

A task requires at least one parameter and one output mapping. Compose
the records first, then add the task to a project with
[`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md)
and run every task on the project with
[`runPI()`](https://esqlabs.github.io/esqlabsR/dev/reference/runPI.md).
To grow a task after it is added, use
[`addPIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIParameter.md)
/
[`addPIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIOutputMapping.md).

## Usage

``` r
PITask(id, scenarios, parameters, outputMappings, configuration = list())
```

## Arguments

- id:

  Character scalar. Identifier for the task.

- scenarios:

  Character vector of scenario ids the task runs against. Every scenario
  referenced by a parameter or output mapping must be in this set.

- parameters:

  List of
  [`PIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIParameter.md)
  records. May be empty to create a task that is seeded later with
  [`addPIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIParameter.md);
  a task must have at least one parameter to run, which
  [`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md)
  enforces.

- outputMappings:

  List of
  [`PIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIOutputMapping.md)
  records. May be empty to create a task that is seeded later with
  [`addPIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIOutputMapping.md);
  a task must have at least one output mapping to run, which
  [`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md)
  enforces.

- configuration:

  Named list of solver settings (e.g. `algorithm`, `ciMethod`,
  `objectiveFunction`, `simulationRunOptions`). Defaults to an empty
  list, leaving every runtime default in place.

## Value

A `PITask` object: a named list with copy semantics.

## See also

[`PIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIParameter.md),
[`PIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIOutputMapping.md),
[`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md),
[`runPI()`](https://esqlabs.github.io/esqlabsR/dev/reference/runPI.md).

Other parameterIdentification:
[`PIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIOutputMapping.md),
[`PIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIParameter.md),
[`addPIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIOutputMapping.md),
[`addPIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIParameter.md),
[`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md),
[`removePIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePIOutputMapping.md),
[`removePIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePIParameter.md),
[`removePITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePITask.md)

## Examples

``` r
PITask(
  id = "aciclovir_fit",
  scenarios = "aciclovir_iv",
  parameters = list(PIParameter(
    id = "lipophilicity",
    scenarios = "aciclovir_iv",
    path = "Aciclovir|Lipophilicity",
    minValue = -2,
    maxValue = 2,
    startValue = 0
  )),
  outputMappings = list(PIOutputMapping(
    id = "pvb",
    scenarios = "aciclovir_iv",
    outputPath = "aciclovir_pvb",
    observedData = "Laskin 1982.Group A"
  ))
)
#> <PITask>
#>   • Id: aciclovir_fit
#>   • Scenarios: aciclovir_iv
#>   • Number of Parameters: 1
#>   • Number of Output Mappings: 1
#>   • Algorithm: <empty string>
#>   • CI Method: <empty string>
```
