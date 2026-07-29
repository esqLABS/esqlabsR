# Save the whole project to a single shareable snapshot file

Write a `Project`, exactly as it is in your R session, to a single
self-contained `.esqlabsR` file. Unsaved changes are included, so the
snapshot captures the project as it is right now — which also makes it a
good way to set an experiment aside: snapshot it, then go back to the
saved state with
[`reloadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/reloadProject.md).
The file content is JSON; the `.esqlabsR` extension simply marks the
file as a portable snapshot, so it is not confused with the
`Project.json` file of a project folder.

Turn a snapshot back into a full project folder with
[`restoreProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/restoreProject.md);
nothing is lost in the round trip.

## Usage

``` r
snapshotProject(project, dir = ".", name = NULL, overwrite = FALSE)
```

## Arguments

- project:

  A `Project` object. It does not need a folder on disk; a project that
  exists only in the R session can be snapshotted too.

- dir:

  Target folder for the snapshot file (default `"."`). Created if it
  does not exist.

- name:

  File name for the snapshot, without extension: the `.esqlabsR`
  extension is always added, and any extension you include is replaced
  (`"exp.zip"` and `"study.json"` both become `exp.esqlabsR` /
  `study.esqlabsR`). When `NULL` (default), a timestamped name
  `<projectName>-YYYY-MM-DD-HHMMSS` is used (it sorts by date and is
  safe as a Windows file name); `<projectName>` falls back to
  `"project"` when the project has no name.

- overwrite:

  If `FALSE` (default), writing over an existing snapshot file aborts.
  Pass `TRUE` to replace it.

## Value

Invisibly, the path of the written snapshot file (always with the
`.esqlabsR` extension).

## See also

[`restoreProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/restoreProject.md),
[`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md),
[`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md).

Other projectPersistence:
[`exampleProjectPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/exampleProjectPath.md),
[`exportProjectToExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/exportProjectToExcel.md),
[`importProjectFromExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/importProjectFromExcel.md),
[`initProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/initProject.md),
[`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md),
[`projectStatus()`](https://esqlabs.github.io/esqlabsR/dev/reference/projectStatus.md),
[`reloadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/reloadProject.md),
[`restoreProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/restoreProject.md),
[`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md)

## Examples

``` r
# Create a temporary example project and snapshot it to a single file.
dir <- file.path(tempdir(), "snapshot-example")
dir.create(dir, showWarnings = FALSE)
initProject(dir, type = "example", createExcel = FALSE)
project <- loadProject(file.path(dir, "Project.json"))
snapshot <- snapshotProject(project, dir = tempdir(), name = "study")
snapshot # the path of the snapshot file
#> [1] "/tmp/RtmpnGTvlq/study.esqlabsR"
```
