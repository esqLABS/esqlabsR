# Remove an output mapping from a PI task

Warns and is a no-op when the mapping id does not exist. If removing the
output mapping leaves the task with no parameters AND no output
mappings, the task is auto-removed from `parameterIdentification`
definitions and a warning is emitted.

## Usage

``` r
removePIOutputMapping(project, task, id)
```

## Arguments

- project:

  A `Project` object.

- task:

  Character scalar. Existing PI task id.

- id:

  Character scalar. Output mapping id to remove.

## Value

The `project` object, invisibly.

## See also

Other parameterIdentification:
[`PIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIOutputMapping.md),
[`PIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIParameter.md),
[`PITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/PITask.md),
[`addPIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIOutputMapping.md),
[`addPIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIParameter.md),
[`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md),
[`removePIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePIParameter.md),
[`removePITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePITask.md)
