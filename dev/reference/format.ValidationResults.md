# Format a project validation report

Builds the character vector of lines that
[`print.ValidationResults()`](https://esqlabs.github.io/esqlabsR/dev/reference/print.ValidationResults.md)
writes to the console. Exposed as a `format` method so the rendered
report can be captured as a string. See
[`print.ValidationResults()`](https://esqlabs.github.io/esqlabsR/dev/reference/print.ValidationResults.md)
for the layout.

## Usage

``` r
# S3 method for class 'ValidationResults'
format(x, ...)
```

## Arguments

- x:

  A `ValidationResults` object, the value of
  [`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md).

- ...:

  Ignored, present for S3 compatibility.

## Value

A character vector, one element per line of the report.

## See also

[`print.ValidationResults()`](https://esqlabs.github.io/esqlabsR/dev/reference/print.ValidationResults.md),
[`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md).
