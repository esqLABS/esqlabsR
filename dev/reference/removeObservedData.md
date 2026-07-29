# Remove one or more observed-data sources from a Project

Removes by the source's id: its `id` field when the declaration carries
one, else the DataSet name (for a `type = "programmatic"` entry that has
not been saved yet) or the `file` basename (for `type` `"excel"` /
`"pkml"` / `"script"` entries). Vectorizes over a vector of ids,
removing each in one in-memory update; persist with
[`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md).
Warns and skips any id with no matching entry.

## Usage

``` r
removeObservedData(project, id)
```

## Arguments

- project:

  A `Project` object.

- id:

  Character vector of ids. An observed-data id is the declaration's `id`
  field, or, when it declares none, comes from the data source itself
  (the `DataSet` name of an unsaved programmatic source, or a file
  basename for a file-based source). Either way it is matched verbatim,
  not canonicalized. A saved programmatic source that declares no `id`
  is matched by its `<name>.pkml` basename (see the note above).

## Value

The `project` object, invisibly.

## Details

Note a programmatic source changes its id once saved:
[`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md)
writes the `DataSet` to `<name>.pkml` and rewrites the entry as a `pkml`
source, so after a save you remove it by that file basename
(`"<name>.pkml"`), not by the original `DataSet` name.

Unlike the other authoring functions,
[`addObservedData()`](https://esqlabs.github.io/esqlabsR/dev/reference/addObservedData.md)
is not vectorized over ids: its second argument is a `DataSet` or a
configuration list, not an id, so it adds a single source per call. Add
several sources with several calls.

## See also

Other observedData:
[`addObservedData()`](https://esqlabs.github.io/esqlabsR/dev/reference/addObservedData.md),
[`getObservedDataNames()`](https://esqlabs.github.io/esqlabsR/dev/reference/getObservedDataNames.md)
