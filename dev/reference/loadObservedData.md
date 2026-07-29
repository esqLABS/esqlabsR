# Load observed data declared in a Project

Reads the `observedData` declarations from a
[Project](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md)
and returns the corresponding
[`ospsuite::DataSet`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DataSet.html)
objects. Source types: `excel` (via importer configuration), `pkml`,
`script`, and `programmatic`. A `programmatic` declaration is a sentinel
for a `DataSet` added at runtime with
[`addObservedData()`](https://esqlabs.github.io/esqlabsR/dev/reference/addObservedData.md);
its data lives in the session until you save, and
[`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md)
persists it to a PKML file and rewrites the entry as a `pkml` source. A
`programmatic` sentinel read from disk with no matching in-session
`DataSet` (a hand-authored declaration, or a project opened before it
was ever saved) resolves to nothing, and the load warns you by name.

## Usage

``` r
loadObservedData(project)
```

## Arguments

- project:

  A `Project` object (see
  [`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md)).

## Value

A named list of
[`ospsuite::DataSet`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DataSet.html)
objects. Empty list when `observedData` definitions is empty or `NULL`.

## Security

A `script` observed-data source runs the R file it names, with
[`source()`](https://rdrr.io/r/base/source.html), on your machine when
the data is resolved. Any R code in that file executes, so treat a
project the same way you would treat a script someone sends you: only
load and resolve observed data from a project you trust. This applies to
`loadObservedData()` and to anything that resolves observed data for
you, such as
[`createDataCombined()`](https://esqlabs.github.io/esqlabsR/dev/reference/createDataCombined.md).

## Examples

``` r
if (FALSE) { # \dontrun{
project <- loadProject("path/to/Project.json")
dataSets <- loadObservedData(project)
} # }
```
