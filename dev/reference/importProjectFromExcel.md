# Import project configuration from Excel files

Reads all Excel configuration files in an esqlabsR project and converts
them to the JSON project format: a project file (named after the Excel
file, e.g. `Project.xlsx` becomes `Project.json`) plus one file per
definition in the `definitions/` folder. The result is a ready-to-use
project —
[`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md)
can open it directly. This is the migration path from Excel-based
projects to the JSON-primary workflow.

The `configurationsFolder` and the per-section workbook filenames are
read from the Excel file and must stay under the project folder: a value
that escapes it (a `../` climb or an absolute path) aborts naming the
field. A folder deliberately placed outside the project with the
`${VAR}` environment-variable form is still allowed.

## Usage

``` r
importProjectFromExcel(
  projectConfigPath = "Project.xlsx",
  outputDir = NULL,
  overwrite = FALSE,
  silent = FALSE,
  copyAssets = TRUE
)
```

## Arguments

- projectConfigPath:

  Path to the `Project.xlsx` file. Defaults to `"Project.xlsx"`.

- outputDir:

  Directory where the JSON project is created. If `NULL` (default), it
  is created in the same directory as the source Excel file.

- overwrite:

  Logical. Guards against silently replacing an existing JSON project.
  With `overwrite = FALSE` (default), the import aborts when a project
  file or a non-empty `definitions/` tree already exists in `outputDir`,
  because re-importing replaces the JSON project with the Excel state
  and deletes any definitions authored only on the JSON side. Pass
  `overwrite = TRUE` to replace the existing JSON project with the Excel
  state.

- silent:

  Logical. If `TRUE`, suppresses the import summary (the project
  written, its per-section definition counts, and the folders copied or
  missing). Defaults to `FALSE`.

- copyAssets:

  Logical. Whether to copy the input folders the project references
  (models, data, csv populations) into `outputDir`, which is what makes
  the imported project runnable where it was written. Defaults to
  `TRUE`. Set it to `FALSE` when only the definitions are wanted and the
  assets would be wasted work, as when the import feeds a throwaway
  comparison snapshot.

## Value

Invisibly returns the path to the created project file (the
`Project.json`).

## See also

Other projectPersistence:
[`exampleProjectPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/exampleProjectPath.md),
[`exportProjectToExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/exportProjectToExcel.md),
[`initProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/initProject.md),
[`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md),
[`projectStatus()`](https://esqlabs.github.io/esqlabsR/dev/reference/projectStatus.md),
[`reloadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/reloadProject.md),
[`restoreProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/restoreProject.md),
[`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md),
[`snapshotProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/snapshotProject.md)
