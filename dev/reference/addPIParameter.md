# Add a parameter to an existing PI task

Add a parameter to an existing PI task

## Usage

``` r
addPIParameter(
  project,
  task,
  path,
  scenarios,
  minValue,
  maxValue,
  startValue,
  units = NULL,
  id = NULL,
  overwrite = FALSE
)
```

## Arguments

- project:

  A `Project` object.

- task:

  Character scalar. Existing PI task id.

- path:

  Character scalar. Full simulation parameter path.

- scenarios:

  Character vector of scenario names; each must exist in `scenarios`
  definitions.

- minValue, maxValue, startValue:

  Numeric scalars.

- units:

  Optional character scalar.

- id:

  Optional character scalar; auto-generated as `<task>_param_<N>` when
  absent.

- overwrite:

  Logical scalar. When `FALSE` (default), an explicit `id` that already
  exists in the task aborts. When `TRUE`, the existing parameter is
  replaced (last-write-wins). Ignored for an auto-generated `id`, which
  never collides.

## Value

The `project` object, invisibly.

## See also

Other parameterIdentification:
[`PIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIOutputMapping.md),
[`PIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIParameter.md),
[`PITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/PITask.md),
[`addPIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIOutputMapping.md),
[`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md),
[`removePIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePIOutputMapping.md),
[`removePIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePIParameter.md),
[`removePITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePITask.md)
