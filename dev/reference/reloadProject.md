# Discard a project's unsaved changes and re-read it from disk

The undo of saving: discard every unsaved change and re-read the project
from its files on disk, in place. The `Project` stays the same object,
so every variable that points to it stays valid.

`reloadProject()` always re-reads the project's files and updates the
project in place, so it also picks up changes made to the files outside
the R session (for example after
[`restoreProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/restoreProject.md)
rolled the project back). It simply produces no announcement when there
was nothing to discard: unlike a clean
[`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md),
a clean reload prints nothing.

## Usage

``` r
reloadProject(project)
```

## Arguments

- project:

  A `Project` with a folder on disk. A project that exists only in the R
  session has nothing to reload from and aborts.

## Value

Invisibly, the `project`, with unsaved changes discarded.

## See also

[`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md),
[`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md),
[`snapshotProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/snapshotProject.md),
[`restoreProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/restoreProject.md),
[`projectStatus()`](https://esqlabs.github.io/esqlabsR/dev/reference/projectStatus.md).

Other projectPersistence:
[`exampleProjectPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/exampleProjectPath.md),
[`exportProjectToExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/exportProjectToExcel.md),
[`importProjectFromExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/importProjectFromExcel.md),
[`initProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/initProject.md),
[`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md),
[`projectStatus()`](https://esqlabs.github.io/esqlabsR/dev/reference/projectStatus.md),
[`restoreProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/restoreProject.md),
[`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md),
[`snapshotProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/snapshotProject.md)

## Examples

``` r
if (FALSE) { # \dontrun{
project <- loadProject("Project.json")
addScenario(project, "oops", modelFile = "model.pkml")
reloadProject(project) # discard the edit, back to disk
} # }
```
