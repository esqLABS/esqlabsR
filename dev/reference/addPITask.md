# Add a Parameter Identification task to a Project

Add a Parameter Identification task to a Project

## Usage

``` r
addPITask(
  project,
  id,
  scenarios,
  parameters,
  outputMappings,
  configuration = list(),
  overwrite = FALSE
)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character scalar. New task id; must not collide with an existing task
  id.

- scenarios:

  Character vector of scenario names. Each must exist in
  `names(project$definitions$scenarios)`.

- parameters:

  List of `PIParameter` records. May be empty
  ([`list()`](https://rdrr.io/r/base/list.html)) to create a task seeded
  later with
  [`addPIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIParameter.md).

- outputMappings:

  List of `PIOutputMapping` records. May be empty
  ([`list()`](https://rdrr.io/r/base/list.html)) to create a task seeded
  later with
  [`addPIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIOutputMapping.md).
  Each `outputPath` must identify a defined output path, either by its
  id (a key in `names(project$definitions$outputPaths)`) or by its
  literal model path.

- configuration:

  Named list of solver settings; see the `configuration` argument of
  [`PITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/PITask.md)
  for the supported keys.

- overwrite:

  Logical scalar. When `FALSE` (default), an existing task id aborts.
  When `TRUE`, the existing task is replaced (last-write-wins).

## Value

The `project` object, invisibly.

## See also

Other parameterIdentification:
[`PIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIOutputMapping.md),
[`PIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIParameter.md),
[`PITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/PITask.md),
[`addPIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIOutputMapping.md),
[`addPIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIParameter.md),
[`removePIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePIOutputMapping.md),
[`removePIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePIParameter.md),
[`removePITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePITask.md)
