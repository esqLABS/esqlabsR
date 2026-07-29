# Validate a project

Once you have authored scenarios and their supporting definitions, the
natural next step, before you run anything, is to check that the project
hangs together: that every reference resolves, no required field is
missing, and no id is duplicated.
[`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md)
does this and prints a readable report.

It assumes you know what a Project is and how to author one (see
[`vignette("projects")`](https://esqlabs.github.io/esqlabsR/dev/articles/projects.md)
and
[`vignette("design-scenarios")`](https://esqlabs.github.io/esqlabsR/dev/articles/design-scenarios.md)).

## Why and when to validate

[`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md)
and
[`createPlots()`](https://esqlabs.github.io/esqlabsR/dev/reference/createPlots.md)
both validate the relevant parts of the project automatically before
they run, so a broken project never silently produces wrong results; it
stops with the validation problem. So why validate explicitly?

Because an explicit
[`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md)
gives you the **full, readable report** for the whole project at once,
at the moment you choose, rather than the first blocking error a run
happens to hit. The natural moment is right after authoring and before
running: it is the cheap check that saves a slow simulation from failing
partway through.

Throughout, work on a writable copy of the bundled example:

``` r

project_dir <- withr::local_tempdir()
initProject(destination = project_dir, type = "example", createExcel = FALSE)
project <- loadProject(file.path(project_dir, "Project.json"))
```

## Two diagnostic levels

Validation has exactly **two** levels, not three:

- A **Critical Error** is a blocking problem: a duplicate id, a required
  field left empty, or a reference to a definition that does not exist.
  A section carrying one has no valid result (its `isValid()` method
  returns `FALSE`), and the project will not run scenarios or build
  plots until it is resolved.
- A **Warning** is a non-blocking note: surfaced so you see it, but it
  does not stop execution. A section with warnings and no critical
  errors is still valid (its `isValid()` method returns `TRUE`).

There is no separate “valid data” tier. A section that simply passed is
just a result with no critical errors. The quickest single question, “is
anything blocking?”, is answered by
[`isAnyCriticalErrors()`](https://esqlabs.github.io/esqlabsR/dev/reference/isAnyCriticalErrors.md).

## The clean case

The bundled example is well formed, so its report is short.
[`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md)
runs every section validator plus a cross-reference pass and returns a
structured object that prints a summary:

``` r

results <- validateProject(project)
results
#> Validation report: 0 critical errors, 0 warnings.
#> ✔ No issues found.
```

[`isAnyCriticalErrors()`](https://esqlabs.github.io/esqlabsR/dev/reference/isAnyCriticalErrors.md)
agrees:

``` r

isAnyCriticalErrors(results)
#> [1] FALSE
```

## A worked example: a dangling reference

The most common authoring slip is a dangling reference: a definition
that points at an id that does not exist, usually a small misspelling.
To see what the report looks like, suppose one scenario file in our copy
mistypes the individual id `adult_male` as `adlt_male`. Reloading the
project catches the dangling reference at load time, and
[`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md)
then reports it in full:

The report is grouped by definition type: a cross (`✖`) marks each
critical error, an exclamation mark (`!`) marks each warning, and the
sections with nothing to report collapse into a single tail line.

``` r

broken_results <- validateProject(broken)
broken_results
#> Validation report: 1 critical error, 0 warnings.
#> crossReferences
#>   ✖ [Invalid Reference] Scenario 'aciclovir_iv' references undefined individual 'adlt_male' (did you mean 'adult_male'?)
#> 10 sections OK.
```

Notice the message does not just say the reference is missing; it offers
a **“did you mean”** suggestion built from the ids that actually exist
(`did you mean 'adult_male'?`), the validator catching the most common
mistake, a small misspelling of an existing id.

## Read the result in code

The printed report is for you; the object
[`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md)
returns is for your code. It is a named list with one entry per
definition type, and each entry is a Validation Result you can index
into. So a script can gate on a specific section without parsing any
text:

``` r

names(broken_results)
#>  [1] "individuals"             "populations"            
#>  [3] "scenarios"               "outputPaths"            
#>  [5] "parameterSets"           "applications"           
#>  [7] "plots"                   "dataCombined"           
#>  [9] "observedData"            "parameterIdentification"
#> [11] "crossReferences"
broken_results$crossReferences$critical_errors[[1]]$message
#> [1] "Scenario 'aciclovir_iv' references undefined individual 'adlt_male' (did you mean 'adult_male'?)"
```

[`validationSummary()`](https://esqlabs.github.io/esqlabsR/dev/reference/validationSummary.md)
rolls the per-section results into aggregate counts and the names of the
sections that produced errors or warnings, which is handy for a one-line
status in a report or a log:

``` r

validationSummary(broken_results)
#> $total_critical_errors
#> [1] 1
#> 
#> $total_warnings
#> [1] 0
#> 
#> $sections_with_errors
#> [1] "crossReferences"
#> 
#> $sections_with_warnings
#> character(0)
```

## Resolve and re-validate

Fixing the problem is the same authoring you already know. Here the
scenario simply needs to point at the correct individual id;
[`setScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/setScenario.md)
updates it in memory (call
[`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md)
when you want the fix on disk):

``` r

setScenario(broken, "aciclovir_iv", individual = "adult_male")
```

Re-validate to confirm the project is clean again:

``` r

fixed_results <- validateProject(broken)
fixed_results
#> Validation report: 0 critical errors, 0 warnings.
#> ✔ No issues found.
isAnyCriticalErrors(fixed_results)
#> [1] FALSE
```

Both the report and
[`isAnyCriticalErrors()`](https://esqlabs.github.io/esqlabsR/dev/reference/isAnyCriticalErrors.md)
are clean, so the project is ready to run.

## Where to go next

With a clean project in hand, see
[`vignette("run-simulations")`](https://esqlabs.github.io/esqlabsR/dev/articles/run-simulations.md)
to execute the scenarios and
[`vignette("plot-results")`](https://esqlabs.github.io/esqlabsR/dev/articles/plot-results.md)
to turn the results into figures. To author or repair the definitions a
validation problem points at, see
[`vignette("design-scenarios")`](https://esqlabs.github.io/esqlabsR/dev/articles/design-scenarios.md).
