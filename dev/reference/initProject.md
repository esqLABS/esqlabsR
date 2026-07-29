# Initialize esqlabsR Project Folders and required Files

Creates a new JSON-based esqlabsR project in `destination`: a
`Project.json` file plus a `definitions/` folder holding one file per
definition, alongside the working folders (`Models/`, `Data/`,
`Populations/`, `Results/`). By default it also writes the optional
Excel configuration files from the JSON; set `createExcel = FALSE` for a
JSON-only project.

## Usage

``` r
initProject(
  destination = ".",
  type = c("minimal", "example"),
  createExcel = TRUE,
  overwrite = FALSE
)
```

## Arguments

- destination:

  A string defining the path where to initialize the project. Defaults
  to the current working directory. The folder is created if it does not
  exist yet.

- type:

  Type of project to create: `"minimal"` (default) creates an empty
  project with just the directory structure, `"example"` creates a
  project with example data, models, and configurations.

- createExcel:

  If `TRUE` (default), generates Excel configuration files from the
  JSON. Set to `FALSE` for a JSON-only workflow.

- overwrite:

  If TRUE, overwrites existing project without asking for permission. If
  FALSE and a project already exists, asks user for permission to
  overwrite.

## Value

Invisibly returns `destination`, the path the project was initialized
in.

## See also

Other projectPersistence:
[`exampleProjectPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/exampleProjectPath.md),
[`exportProjectToExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/exportProjectToExcel.md),
[`importProjectFromExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/importProjectFromExcel.md),
[`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md),
[`projectStatus()`](https://esqlabs.github.io/esqlabsR/dev/reference/projectStatus.md),
[`reloadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/reloadProject.md),
[`restoreProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/restoreProject.md),
[`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md),
[`snapshotProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/snapshotProject.md)
