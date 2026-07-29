# Run Parameter Identification tasks defined in a Project

Builds and runs every requested PI task in the Project. Build errors
(typos in parameter paths, unknown outputs, missing observed data, etc.)
propagate as hard errors so users can fix them immediately. Only the
optimisation step is wrapped in `tryCatch`: a numerical failure inside
`task$run()` degrades to `result = NULL, error = <message>` so the loop
continues with the remaining tasks.

## Usage

``` r
runPI(
  project,
  tasks = NULL,
  observedData = NULL,
  stopIfParameterNotFound = TRUE
)
```

## Arguments

- project:

  A `Project` object (see
  [`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md)).

- tasks:

  Optional character vector of task ids to run. When `NULL` (default),
  every task on the Project is run. The ids are canonicalized the same
  way
  [`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md)
  canonicalizes a task id, so a name typed as it was first passed to
  [`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md)
  still resolves.

- observedData:

  Optional named list of pre-loaded `DataSet` objects that overrides
  automatic resolution from `observedData` definitions.

- stopIfParameterNotFound:

  Logical. When `TRUE` (default), a parameter listed in a scenario's
  parameter sets but absent from the simulation aborts the build; when
  `FALSE`, it is skipped with a warning. Forwarded through
  `.prepareScenario()` to
  [`initializeSimulation()`](https://esqlabs.github.io/esqlabsR/dev/reference/initializeSimulation.md).

## Value

Named list of per-task results. Each entry is a list with `task` (the
runtime `ParameterIdentification` object), `result` (the `PIResult` from
`task$run()`, or `NULL` on optimisation failure), and optional `error`
(the optimiser's failure message, absent on success).
