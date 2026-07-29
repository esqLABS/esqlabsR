# Save the project to the disk

Write your changes to the project files on disk. Changes made in your R
session (e.g. with
[`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md),
[`setIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/setIndividual.md),
[`removeParameterSet()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeParameterSet.md))
only live in memory until you call `saveProject()`.

What happens when you save:

- Only files with actual changes are re-written, so `git diff` shows
  exactly the definitions you edited.

- If you removed something from the project (e.g. a scenario), its file
  in the `definitions/` folder is deleted. Files outside the
  `definitions/` folder are never touched.

- The `Project.json` file is updated.

If there is nothing to save, `saveProject()` simply reports that the
project is already up to date. Saving repeatedly is always safe.

Saving does not update the Excel files. If you also work with the Excel
configuration files, refresh them with
[`exportProjectToExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/exportProjectToExcel.md).
Use
[`projectStatus()`](https://esqlabs.github.io/esqlabsR/dev/reference/projectStatus.md)
to check whether project files on disk, the Excel files, and your R
session are in sync.

## Usage

``` r
saveProject(project)
```

## Arguments

- project:

  A `Project` loaded from disk with
  [`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md)
  (or restored with
  [`restoreProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/restoreProject.md)).
  A project created directly with `Project$new()` has no folder on disk
  to save to; use
  [`snapshotProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/snapshotProject.md)
  to write it to a single file, or create a project folder first with
  [`initProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/initProject.md).

## Value

Invisibly, the `project`.

## See also

[`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md),
[`reloadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/reloadProject.md),
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
[`reloadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/reloadProject.md),
[`restoreProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/restoreProject.md),
[`snapshotProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/snapshotProject.md)

## Examples

``` r
if (FALSE) { # \dontrun{
project <- loadProject("Project.json")
addOutputPath(project, "PVB", "Organism|PeripheralVenousBlood|...")
saveProject(project) # tree now mirrors memory
saveProject(project) # clean save: "Project is already up to date; ..."
} # }
```
