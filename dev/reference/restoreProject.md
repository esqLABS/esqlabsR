# Recreate a project folder from a snapshot file

Read a snapshot file and recreate a full project folder from it at
`dir`: a `Project.json` file plus the `definitions/` folder with one
file per definition. Returns a freshly loaded `Project` that works from
`dir`.

You only need the file, not a loaded project. The typical use is
sharing: a colleague sends you a `.esqlabsR` file, and
`restoreProject("theirs.esqlabsR", "myproj")` recreates the whole
project folder from it. It is also the way back to an earlier state:
with `overwrite = TRUE` it rolls an existing project folder back to the
snapshot.

Snapshots are usually `.esqlabsR` files written by
[`snapshotProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/snapshotProject.md),
but a `Project.json` in which all sections are written out in the file
itself, rather than in a `definitions/` folder (a legacy single-file
project), is accepted too. A monolithic snapshot written by a previous
esqlabsR version (`snapshotProjectConfiguration()`) is also accepted and
upgraded to the current project format on read, through the Excel
bridge; observed data is not carried in such a snapshot, so add it with
[`addObservedData()`](https://esqlabs.github.io/esqlabsR/dev/reference/addObservedData.md)
if a plot or parameter identification needs it. The result is a normal
project:
[`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md)
opens it from `dir` with exactly the same content.

## Usage

``` r
restoreProject(snapshot, dir = ".", overwrite = FALSE)
```

## Arguments

- snapshot:

  Path to the snapshot file to read (a `.esqlabsR` file, a legacy
  single-file `Project.json`, or a monolithic snapshot written by a
  previous esqlabsR version). Must exist.

- dir:

  Folder in which the project is recreated (default `"."`). Created if
  it does not exist.

- overwrite:

  If `FALSE` (default), `restoreProject()` aborts when `dir` already
  contains any files — a project, unrelated files, anything; restore
  into a fresh folder only. If `TRUE`, the contents of `dir` are
  replaced with the snapshot. When `dir` held an esqlabsR project, a
  warning reminds you that a `Project` you loaded from `dir` earlier no
  longer matches the files: continue with the returned project, or
  refresh the old one with
  [`reloadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/reloadProject.md).
  The recommended form is
  `p <- restoreProject(snap, dir, overwrite = TRUE)`.

## Value

A freshly loaded `Project` working from `dir`, with no unsaved changes.

## See also

[`snapshotProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/snapshotProject.md),
[`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md),
[`reloadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/reloadProject.md).

Other projectPersistence:
[`exampleProjectPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/exampleProjectPath.md),
[`exportProjectToExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/exportProjectToExcel.md),
[`importProjectFromExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/importProjectFromExcel.md),
[`initProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/initProject.md),
[`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md),
[`projectStatus()`](https://esqlabs.github.io/esqlabsR/dev/reference/projectStatus.md),
[`reloadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/reloadProject.md),
[`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md),
[`snapshotProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/snapshotProject.md)

## Examples

``` r
# Write a snapshot, then recreate a project folder from it.
src <- file.path(tempdir(), "restore-src")
dir.create(src, showWarnings = FALSE)
initProject(src, type = "example", createExcel = FALSE)
snapshot <- snapshotProject(
  loadProject(file.path(src, "Project.json")),
  dir = tempdir(),
  name = "shared"
)
project <- restoreProject(snapshot, file.path(tempdir(), "restored"))
```
