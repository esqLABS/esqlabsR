# Validate a Project

Runs every section validator (and a cross-reference pass) against a
parsed `Project` and returns a named list of `validationResult` objects,
one per section, in canonical order. On a clean run (no section produced
critical errors) it marks the project validated, so subsequent
[`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md)
/
[`createPlots()`](https://esqlabs.github.io/esqlabsR/dev/reference/createPlots.md)
calls can skip a redundant validation pass until the next edit.

## Usage

``` r
validateProject(project)
```

## Arguments

- project:

  A `Project` object (typically produced by
  [`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md)).
  Path inputs are not accepted here; load the project first.

## Value

Named list of `validationResult` objects with class
`"ValidationResults"`. Order matches `.validationAdapters`, with
`crossReferences` last.

## See also

[`isAnyCriticalErrors()`](https://esqlabs.github.io/esqlabsR/dev/reference/isAnyCriticalErrors.md),
[`validationSummary()`](https://esqlabs.github.io/esqlabsR/dev/reference/validationSummary.md),
[`print.ValidationResults()`](https://esqlabs.github.io/esqlabsR/dev/reference/print.ValidationResults.md).

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
