# Add an output mapping to an existing PI task

Add an output mapping to an existing PI task

## Usage

``` r
addPIOutputMapping(
  project,
  task,
  outputPath,
  observedData,
  scenarios,
  scaling = NULL,
  xOffset = 0,
  yOffset = 0,
  xFactor = 1,
  yFactor = 1,
  weight = NULL,
  id = NULL,
  overwrite = FALSE
)
```

## Arguments

- project:

  A `Project` object.

- task:

  Character scalar. Existing PI task id.

- outputPath:

  Character scalar identifying a defined output path: either its id (a
  key in `names(project$definitions$outputPaths)`) or the literal model
  path it maps to. Both resolve to the same output path.

- observedData:

  Character scalar. Name of the observed dataset.

- scenarios:

  Character vector of scenario names.

- scaling, xOffset, yOffset, xFactor, yFactor, weight:

  Optional per-mapping fitting metadata. Defaults match
  [`PIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIOutputMapping.md).

- id:

  Optional character scalar; auto-generated as `<task>_mapping_<N>` when
  absent.

- overwrite:

  Logical scalar. When `FALSE` (default), an explicit `id` that already
  exists in the task aborts. When `TRUE`, the existing mapping is
  replaced (last-write-wins). Ignored for an auto-generated `id`, which
  never collides.

## Value

The `project` object, invisibly.

## See also

Other parameterIdentification:
[`PIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIOutputMapping.md),
[`PIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIParameter.md),
[`PITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/PITask.md),
[`addPIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIParameter.md),
[`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md),
[`removePIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePIOutputMapping.md),
[`removePIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePIParameter.md),
[`removePITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePITask.md)
