# isAnyCriticalErrors

Reports whether any section of a validation run produced a critical
error, collapsing the per-section results from
[`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md)
into a single logical.

## Usage

``` r
isAnyCriticalErrors(validationResults)
```

## Arguments

- validationResults:

  Named list of class `"ValidationResults"`, the output of
  [`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md).

## Value

A single logical: `TRUE` if any section has critical errors, otherwise
`FALSE`.

## See also

[`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md),
[`validationSummary()`](https://esqlabs.github.io/esqlabsR/dev/reference/validationSummary.md).

## Examples

``` r
if (FALSE) { # \dontrun{
project <- loadProject("Project.json")
results <- validateProject(project)
if (isAnyCriticalErrors(results)) {
  print(validationSummary(results))
}
} # }
```
