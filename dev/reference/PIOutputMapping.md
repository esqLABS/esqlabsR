# Create a Parameter Identification output mapping

Builds a plain-data `PIOutputMapping` record pairing one simulation
output with the observed dataset it is fitted against, plus the optional
per-mapping fitting metadata (scaling, axis offsets and factors,
residual weights).

A `PIOutputMapping` is a building block for a
[`PITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/PITask.md);
pass a list of them as the `outputMappings` argument of
[`PITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/PITask.md)
or
[`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md),
or add one to an existing task with
[`addPIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIOutputMapping.md).

## Usage

``` r
PIOutputMapping(
  id,
  scenarios,
  outputPath,
  observedData,
  scaling = NULL,
  xOffset = 0,
  yOffset = 0,
  xFactor = 1,
  yFactor = 1,
  weight = NULL
)
```

## Arguments

- id:

  Character scalar. Identifier for this mapping, unique within its task.

- scenarios:

  Character vector of scenario ids the mapping applies to. Must be a
  subset of the task's own `scenarios`.

- outputPath:

  Character scalar identifying an output path defined in `outputPaths`:
  either its id (a key of `outputPaths`) or the literal model path it
  maps to (its value). Both resolve to the same defined output path; it
  must already exist (add one with
  [`addOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/addOutputPath.md)).

- observedData:

  Character scalar. Id of the observed dataset to fit against.

- scaling:

  Optional character scalar. Residual scaling (e.g. `"lin"` or `"log"`);
  `NULL` uses the runtime default.

- xOffset, yOffset, xFactor, yFactor:

  Numeric scalars. Affine transform applied to the observed data before
  comparison. Defaults are the identity transform (`0` offsets, `1`
  factors).

- weight:

  Optional numeric scalar or vector. Residual weight(s) applied to the
  observed dataset; `NULL` leaves the data unweighted.

## Value

A `PIOutputMapping` object: a named list with copy semantics.

## See also

[`PITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/PITask.md),
[`PIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIParameter.md),
[`addPIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIOutputMapping.md).

Other parameterIdentification:
[`PIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIParameter.md),
[`PITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/PITask.md),
[`addPIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIOutputMapping.md),
[`addPIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIParameter.md),
[`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md),
[`removePIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePIOutputMapping.md),
[`removePIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePIParameter.md),
[`removePITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePITask.md)

## Examples

``` r
PIOutputMapping(
  id = "pvb",
  scenarios = "aciclovir_iv",
  outputPath = "aciclovir_pvb",
  observedData = "Laskin 1982.Group A",
  scaling = "log"
)
#> <PIOutputMapping>
#>   • Id: pvb
#>   • Scenarios: aciclovir_iv
#>   • Output Path Id: aciclovir_pvb
#>   • Observed Data Id: Laskin 1982.Group A
#>   • Scaling: log
#>   • Weight: <empty string>
```
