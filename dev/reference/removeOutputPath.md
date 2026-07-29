# Remove one or more output paths from a Project

Drop the output paths with matching ids in one write-through. Warns (and
skips) any id not present, and warns when a removed output path is still
referenced.

## Usage

``` r
removeOutputPath(project, id)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character vector of output-path ids to remove. Each is canonicalized
  the same way
  [`addOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/addOutputPath.md)
  canonicalizes it.

## Value

The `project` object, invisibly.

## See also

Other outputPath:
[`addOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/addOutputPath.md),
[`setOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/setOutputPath.md)
