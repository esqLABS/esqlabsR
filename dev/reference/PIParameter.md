# Create a Parameter Identification parameter

Builds a plain-data `PIParameter` record describing one optimisation
variable of a Parameter Identification task: the model parameter to
estimate, the scenarios it is fitted across, and its search bounds and
start value.

A `PIParameter` is a building block for a
[`PITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/PITask.md);
pass a list of them as the `parameters` argument of
[`PITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/PITask.md)
or
[`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md),
or add one to an existing task with
[`addPIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIParameter.md).

## Usage

``` r
PIParameter(id, scenarios, path, units = NULL, minValue, maxValue, startValue)
```

## Arguments

- id:

  Character scalar. Identifier for this parameter, unique within its
  task. Used as a free label by the PI run, not as a definition-file id.

- scenarios:

  Character vector of scenario ids the parameter is estimated across.
  Listing several scenarios fits one shared value across all of them.
  The constructor does not check these against the task's own
  `scenarios`; any that are not a subset of the task's scenarios are
  reported later, by
  [`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md)
  and at run time.

- path:

  Character scalar. Full simulation path of the model parameter to
  estimate (OSPS notation).

- units:

  Optional character scalar. Display unit the bounds and start value are
  expressed in. `NULL` or `""` means the model default unit.

- minValue, maxValue, startValue:

  Numeric scalars. Search bounds and the starting value;
  `minValue <= startValue <= maxValue` must hold.

## Value

A `PIParameter` object: a named list with copy semantics.

## See also

[`PITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/PITask.md),
[`PIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIOutputMapping.md),
[`addPIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIParameter.md).

Other parameterIdentification:
[`PIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIOutputMapping.md),
[`PITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/PITask.md),
[`addPIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIOutputMapping.md),
[`addPIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIParameter.md),
[`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md),
[`removePIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePIOutputMapping.md),
[`removePIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePIParameter.md),
[`removePITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePITask.md)

## Examples

``` r
PIParameter(
  id = "lipophilicity",
  scenarios = "aciclovir_iv",
  path = "Aciclovir|Lipophilicity",
  minValue = -2,
  maxValue = 2,
  startValue = 0
)
#> <PIParameter>
#>   • Id: lipophilicity
#>   • Scenarios: aciclovir_iv
#>   • Path: Aciclovir|Lipophilicity
#>   • Units: <empty string>
#>   • Min / Start / Max: -2 / 0 / 2
```
