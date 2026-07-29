# Export a Project to Excel files

Writes Excel configuration files from a `Project` object (typically
loaded from JSON). This is the reverse of
[`importProjectFromExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/importProjectFromExcel.md).

## Usage

``` r
exportProjectToExcel(
  project,
  outputDir = NULL,
  overwrite = FALSE,
  silent = FALSE
)
```

## Arguments

- project:

  A `Project` object.

- outputDir:

  Directory where the Excel files will be created. Defaults to the
  directory of the source JSON file.

- overwrite:

  Logical. Guards against silently overwriting existing Excel workbooks.
  With `overwrite = FALSE` (default), the export aborts when
  `Project.xlsx` or any `Configurations/` workbook already exists in
  `outputDir`, because the export replaces each workbook wholesale and
  would discard any hand-edits it carries. Pass `overwrite = TRUE` to
  replace the existing workbooks.

- silent:

  Logical. If `TRUE`, suppresses informational messages. Defaults to
  `FALSE`.

## Value

Invisibly returns the path to the created `Project.xlsx`.

## See also

Other projectPersistence:
[`exampleProjectPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/exampleProjectPath.md),
[`importProjectFromExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/importProjectFromExcel.md),
[`initProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/initProject.md),
[`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md),
[`projectStatus()`](https://esqlabs.github.io/esqlabsR/dev/reference/projectStatus.md),
[`reloadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/reloadProject.md),
[`restoreProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/restoreProject.md),
[`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md),
[`snapshotProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/snapshotProject.md)
