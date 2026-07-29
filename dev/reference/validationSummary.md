# validationSummary

Aggregates the per-section results from
[`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md)
into overall counts of critical errors and warnings, plus the names of
the sections that produced each.

## Usage

``` r
validationSummary(validationResults)
```

## Arguments

- validationResults:

  Named list of class `"ValidationResults"`, the output of
  [`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md).

## Value

A list with `total_critical_errors`, `total_warnings`,
`sections_with_errors`, and `sections_with_warnings`.

## See also

[`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md),
[`isAnyCriticalErrors()`](https://esqlabs.github.io/esqlabsR/dev/reference/isAnyCriticalErrors.md).

## Examples

``` r
if (FALSE) { # \dontrun{
project <- loadProject("Project.json")
results <- validateProject(project)
summary <- validationSummary(results)
summary$total_critical_errors
} # }
```
