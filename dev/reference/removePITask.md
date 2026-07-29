# Remove one or more Parameter Identification tasks from a Project

Drop the tasks with matching ids in one write-through. Warns (and skips)
any id not present.

## Usage

``` r
removePITask(project, id)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character vector of task ids. Each is canonicalized the same way
  [`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md)
  canonicalizes it.

## Value

The `project` object, invisibly.

## Details

[`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md)
is not vectorized over ids: each task is composed of its own distinct
lists of `PIParameter` / `PIOutputMapping` records, so several tasks are
added with several calls. The per-task sub-definition helpers
([`addPIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIParameter.md)
/
[`addPIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIOutputMapping.md)
and their removals) act on one parent task identified by `task`, so they
likewise stay single-definition.

## See also

Other parameterIdentification:
[`PIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIOutputMapping.md),
[`PIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIParameter.md),
[`PITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/PITask.md),
[`addPIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIOutputMapping.md),
[`addPIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIParameter.md),
[`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md),
[`removePIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePIOutputMapping.md),
[`removePIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePIParameter.md)
