# Add observed data to a Project

Adds one observed-data declaration. `entry` is either an
[`ospsuite::DataSet`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DataSet.html)
object (which becomes a `type = "programmatic"` declaration keyed by
`dataSet$name`) or a list describing where the data is read from.

Such a list always carries a `type`, plus the fields that type needs:

|            |                                           |                 |
|------------|-------------------------------------------|-----------------|
| `type`     | required fields                           | optional fields |
| `"excel"`  | `file`, `importerConfiguration`, `sheets` | `id`            |
| `"pkml"`   | `file`                                    | `id`            |
| `"script"` | `file`                                    | `id`            |

`file` and `importerConfiguration` are paths *relative to the project's
data folder* (`project$paths$dataFolder`). A file sitting directly in
that folder is `"observed.xlsx"`, not `"Data/observed.xlsx"`; one in a
subfolder of it is `"subfolder/observed.xlsx"`. `sheets` lists the Excel
sheet names to import. A `script` source runs the R file it names (see
the Security section of
[`loadObservedData()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadObservedData.md)).

`id` names the declaration itself: it becomes the declaration's file
under the project's definitions folder
(`definitions/observed-data/<id>.json` in the default layout) and is the
id
[`removeObservedData()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeObservedData.md)
matches on. Left out, the `file` basename serves as both. It is not the
name the data is known by: each imported
[`ospsuite::DataSet`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DataSet.html)
carries the name its source gives it (the data-set name in an Excel
sheet, the name inside a PKML file), and that name, not the
declaration's id, is what a `dataCombined` entry references.

A `DataSet` you pass lives in the R session until you save. On
[`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md)
it is written to a PKML file named `<DataSet name>.pkml` under the
project's data folder and its entry becomes a `pkml` source, so the data
survives a reload. Saving therefore needs a data folder to be declared
in the project's file paths.

## Usage

``` r
addObservedData(project, entry, overwrite = FALSE)
```

## Arguments

- project:

  A `Project` object.

- entry:

  An
  [`ospsuite::DataSet`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DataSet.html)
  object, or a configuration list carrying a `type` (`"excel"`,
  `"pkml"`, or `"script"`) and that type's fields, as described above.

- overwrite:

  Logical scalar. When `FALSE` (default), a source whose id already
  exists (a `DataSet` name, or a configuration list's `id` or `file`
  basename) aborts. When `TRUE`, the existing source with that id is
  replaced (last-write-wins) as long as both are the same kind. A
  cross-kind replacement is refused in either direction, because it
  would either strand the session's `DataSet` or leave two sources
  resolving to one name: remove the existing source with
  [`removeObservedData()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeObservedData.md)
  first, then add the new one.

## Value

The `project` object, invisibly.

## See also

Other observedData:
[`getObservedDataNames()`](https://esqlabs.github.io/esqlabsR/dev/reference/getObservedDataNames.md),
[`removeObservedData()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeObservedData.md)
