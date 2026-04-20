# Run Parameter Identification tasks

Executes parameter identification for all PI tasks. Handles failures
gracefully - continues with other tasks if one fails.

## Usage

``` r
runPI(piTasks)
```

## Arguments

- piTasks:

  Named list of `ParameterIdentification` objects usually created using
  `createPITasks`

## Value

Named list of PI results. Each result contains:

- task: original `ParameterIdentification` object

- result: `PIResult` object from `task$run()`, or NULL if failed

- error: Error message string if failed, absent on success

## Examples

``` r
if (FALSE) { # \dontrun{
# After creating PI tasks
piResults <- runPI(piTasks)
} # }
```
