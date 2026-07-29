# Check a loaded project for unsaved changes and outdated Excel files

Prints a report of how the `Project` in your R session compares to the
files on disk, in two parts:

- project vs. saved files: whether the project carries changes that have
  not been saved with
  [`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md)
  yet. Reported as `NA` for a project that exists only in the R session,
  without a folder on disk.

- project vs. Excel: when the project has a `Project.xlsx` Excel file,
  whether that file still matches the current project (one-way: would
  exporting again change it). Reported as `NA` when there is no Excel
  file or it cannot be read.

`projectStatus()` only reports; it never changes any files. To save your
changes, call
[`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md);
to bring the Excel files up to date, call
[`exportProjectToExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/exportProjectToExcel.md)
or
[`importProjectFromExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/importProjectFromExcel.md).

## Usage

``` r
projectStatus(project, silent = FALSE)
```

## Arguments

- project:

  A `Project` object.

- silent:

  Logical. If `TRUE`, suppresses the printed report and only returns the
  structured result (the same shape as `project$status`). Defaults to
  `FALSE`.

## Value

Invisibly, a `list(tree_in_sync, excel_in_sync, details)` (see the
`status` field of
[Project](https://esqlabs.github.io/esqlabsR/dev/reference/Project.md)).

## See also

[`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md),
[`reloadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/reloadProject.md),
[`exportProjectToExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/exportProjectToExcel.md),
[`importProjectFromExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/importProjectFromExcel.md).

Other projectPersistence:
[`exampleProjectPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/exampleProjectPath.md),
[`exportProjectToExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/exportProjectToExcel.md),
[`importProjectFromExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/importProjectFromExcel.md),
[`initProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/initProject.md),
[`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md),
[`reloadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/reloadProject.md),
[`restoreProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/restoreProject.md),
[`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md),
[`snapshotProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/snapshotProject.md)

## Examples

``` r
if (FALSE) { # \dontrun{
project <- loadProject("Project.json")
projectStatus(project) # readable report
project$status # the same information as a structured list
} # }
```
