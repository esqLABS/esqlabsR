# Print a project validation report

Renders the named list of per-section `validationResult` objects that
[`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md)
returns into a human-readable summary, grouped by definition type (the
list keys: `scenarios`, `individuals`, and the rest). The structured
object itself is unchanged and stays indexable
(`results$scenarios$critical_errors`); only the console view differs.

## Usage

``` r
# S3 method for class 'ValidationResults'
print(x, ...)
```

## Arguments

- x:

  A `ValidationResults` object, the value of
  [`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md).

- ...:

  Ignored, present for S3 compatibility.

## Value

`x`, invisibly.

## Details

The summary opens with overall counts (the same aggregation as
[`validationSummary()`](https://esqlabs.github.io/esqlabsR/dev/reference/validationSummary.md)),
then lists each definition type that has at least one issue: a cross
marks each critical error, a `!` marks each warning, and the `category`
of each entry is shown as a sub-label. Definition types with no issues
are folded into a compact "N section?s OK" tail. A fully valid result
prints a single "no issues" line. Glyphs and styling come from `cli`, so
the output degrades gracefully to plain ASCII when unicode or colour is
unavailable.

## See also

[`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md),
[`validationSummary()`](https://esqlabs.github.io/esqlabsR/dev/reference/validationSummary.md),
[`isAnyCriticalErrors()`](https://esqlabs.github.io/esqlabsR/dev/reference/isAnyCriticalErrors.md).
