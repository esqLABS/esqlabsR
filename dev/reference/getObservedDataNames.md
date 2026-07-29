# Get names of all observed data in a Project

Returns the names of all DataSets that would be returned by
[`loadObservedData()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadObservedData.md).
On first call this loads the data to discover names; subsequent calls
return cached names until a mutation invalidates the cache.

## Usage

``` r
getObservedDataNames(project)
```

## Arguments

- project:

  A `Project` object (see
  [`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md)).

## Value

A character vector of DataSet names.

## See also

Other observedData:
[`addObservedData()`](https://esqlabs.github.io/esqlabsR/dev/reference/addObservedData.md),
[`removeObservedData()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeObservedData.md)
