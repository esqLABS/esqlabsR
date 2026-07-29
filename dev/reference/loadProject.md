# Load a project from a JSON configuration file

Load a `Project` from a JSON file. This is the primary entry point for
working with esqlabsR projects.

On load, the project is checked for the most common reference mistakes
(for example a scenario referring to an individual or population that is
not defined). Such issues are reported as warnings so that obvious
configuration mistakes surface immediately, but loading still succeeds.
Use
[`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md)
for a full report.

## Usage

``` r
loadProject(path = "Project.json")
```

## Arguments

- path:

  Path to the `Project.json` file. Defaults to `Project.json` in the
  working directory.

## Value

Object of type `Project`

## Editing a loaded project

Changes you make to a loaded project — with
[`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md),
[`setIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/setIndividual.md),
[`removeParameterSet()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeParameterSet.md),
[`addOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/addOutputPath.md),
and the other add/set/remove functions — live only in your R session
until you save them; the files on disk stay as they are. Reading a
section directly (for example `scenarios` definitions) never changes the
project: a definition changes only through the add/set/remove functions.

Write your changes to the project files with
[`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md).
Discard unsaved changes and go back to what is saved on disk with
[`reloadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/reloadProject.md).
Save the current state of the whole project to a single file you can
archive or share with
[`snapshotProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/snapshotProject.md),
and recreate a project folder from such a file (or roll an existing one
back) with
[`restoreProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/restoreProject.md).
Check for unsaved changes and outdated Excel files with
[`projectStatus()`](https://esqlabs.github.io/esqlabsR/dev/reference/projectStatus.md).

## See also

[`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md),
[`reloadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/reloadProject.md),
[`snapshotProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/snapshotProject.md),
[`restoreProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/restoreProject.md),
[`projectStatus()`](https://esqlabs.github.io/esqlabsR/dev/reference/projectStatus.md).

Other projectPersistence:
[`exampleProjectPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/exampleProjectPath.md),
[`exportProjectToExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/exportProjectToExcel.md),
[`importProjectFromExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/importProjectFromExcel.md),
[`initProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/initProject.md),
[`projectStatus()`](https://esqlabs.github.io/esqlabsR/dev/reference/projectStatus.md),
[`reloadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/reloadProject.md),
[`restoreProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/restoreProject.md),
[`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md),
[`snapshotProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/snapshotProject.md)

## Examples

``` r
if (FALSE) { # \dontrun{
project <- loadProject("Project.json")
results <- runScenarios(project)

# Edits stay in memory until you save.
addOutputPath(project, "x", "Organism|A|Concentration in container")
saveProject(project)
} # }
```
