# Project

An R6 class representing an esqlabsR project.

Changes you make to a loaded project — with
[`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md),
[`setIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/setIndividual.md),
[`removeParameterSet()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeParameterSet.md),
and the other add/set/remove functions — live only in your R session
until you write them to the project files with
[`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md).
This makes a loaded project a safe place to experiment: discard unsaved
changes with
[`reloadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/reloadProject.md),
or use
[`snapshotProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/snapshotProject.md)
/
[`restoreProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/restoreProject.md)
to set a save-point and return to it later.

A `Project` cannot be copied with `$clone()` — because nothing is
written to disk until you save, there is no need for a working copy: the
loaded project itself is one. Reading a definition (for example
`sc <- project$definitions$scenarios[["my_scenario"]]`) hands you an
independent copy, so changing `sc` does not change the project until you
re-submit it with the matching set function. The one exception is
observed data added as a `DataSet` object via
[`addObservedData()`](https://esqlabs.github.io/esqlabsR/dev/reference/addObservedData.md):
the `DataSet` is shared, so changes to that object are seen by the
project too.

The public authoring methods (`project$addScenario(...)`,
`project$addOutputPath(...)`, the whole `add*` / `set*` / `remove*`
family) mirror the exported free functions of the same name; the free
function is the primary entry point, carries the full per-argument
documentation, and forwards to the method. The method arguments are
summarised once below (roxygen2 documents R6 method arguments in the
class topic); for the authoritative, per-function argument descriptions
see the linked free function, e.g.
[`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md),
[`setScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/setScenario.md),
[`addOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/addOutputPath.md),
[`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md),
[`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md),
[`createScenariosFromPKML()`](https://esqlabs.github.io/esqlabsR/dev/reference/createScenariosFromPKML.md).

## Active bindings

- `info`:

  Project identity and metadata, as a writable field group. Read a field
  with `project$info$name`; write one with `project$info$name <- "..."`.
  Writable fields: `name` (human-readable project name), `description`
  (free-text description). Read-only fields: `schemaVersion` (schema
  version declared in the JSON, always `"2.0"` for projects this package
  loads), `esqlabsRVersion` (informational version string from the
  JSON), `projectFilePath` (absolute path to the JSON file the project
  was loaded from, `NULL` for an in-memory project), and
  `projectDirPath` (the directory containing that file, the base for
  resolving relative paths). Assigning a writable field sets the dirty
  bit; assigning a read-only field aborts. The two version fields are
  managed by the load/save machinery, not by users.

- `paths`:

  The project's working-folder paths, as a writable field group:
  `simulationsFolder` (pkml simulation files, `Models/Simulations`,
  sitting under `Models/` alongside the `Snapshots` folder for PK-Sim /
  MoBi snapshots), `dataFolder` (experimental data), `outputFolder`
  (results), `populationsFolder` (population CSVs loaded by
  [`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md)),
  and `definitionsFolder` (the folder holding the definition files,
  default `"definitions"`). Read a field with
  `project$paths$simulationsFolder` (returned resolved against
  `projectDirPath`); write one with
  `project$paths$simulationsFolder <- "Models"` (stored verbatim,
  resolved on the next read). Assigning any field sets the dirty bit.
  Changing `definitionsFolder` redirects where the next
  [`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md)
  writes the definition files; nothing moves on disk until that save.
  The Excel-bridge sheet-name fields live in the separate `excel` group.

- `excel`:

  The Excel import/export bridge sheet-name fields, as a writable field
  group: `configurationsFolder`, `modelParamsFile`, `individualsFile`,
  `populationsFile`, `scenariosFile`, `applicationsFile`, `plotsFile`,
  `parameterIdentificationFile`, `initialConditionsFile`. Read a field
  with `project$excel$modelParamsFile` (returned resolved against
  `configurationsFolder`, itself resolved against `projectDirPath`);
  write one with `project$excel$modelParamsFile <- "P.xlsx"` (stored
  verbatim). Assigning any field sets the dirty bit. Empty for a project
  created directly in the JSON format, without Excel files.

- `defaultSimulationRunOptions`:

  Named list of the project-level default simulation run options (the
  `defaultSimulationRunOptions` JSON field), or `NULL` when none are
  declared. Used by
  [`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md)
  as the default `simulationRunOptions` when the caller does not pass
  one. Recognized fields: `numberOfCores`, `checkForNegativeValues`,
  `showProgress`.

- `definitions`:

  The project's definition sections, as a read-only field group. Each
  section is one field: `outputPaths`, `scenarios`, `parameterSets`,
  `initialConditions`, `individuals`, `populations`, `applications`,
  `observedData`, `dataCombined`, `plots`, `plotGrids`,
  `parameterIdentification`. Read a section with
  `project$definitions$scenarios` (returned as a printable, read-only
  named list keyed by id). The group is read-only from the handle: every
  assignment form aborts. The only sanctioned way to change a section is
  an authoring function
  ([`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md)
  /
  [`setScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/setScenario.md)
  /
  [`removeScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeScenario.md)
  and their per-section siblings) or editing the definition's JSON file;
  those route through the internal write seam, which updates the
  in-memory backing field, sets the dirty bit, and clears the
  validation-cache flag so the next run/plot re-validates. Nothing
  touches the `definitions/<kind>/` tree until
  [`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md)
  reconciles it to memory.

- `asList`:

  Returns the current project as a list matching the JSON schema.
  Reflects any in-memory modifications. Read-only.

- `status`:

  Read-only sync report as a structured list: `tree_in_sync` (`FALSE`
  when the project carries unsaved changes, `NA` for a project that
  exists only in the R session, without a folder on disk),
  `excel_in_sync` (`TRUE`/`FALSE`, or `NA` when the project has no Excel
  file or it cannot be read), and `details` (the differences, empty when
  everything is in sync). The same information
  [`projectStatus()`](https://esqlabs.github.io/esqlabsR/dev/reference/projectStatus.md)
  prints. Read-only; assignment aborts.

## Methods

### Public methods

- [`Project$new()`](#method-Project-initialize)

- [`Project$addScenario()`](#method-Project-addScenario)

- [`Project$removeScenario()`](#method-Project-removeScenario)

- [`Project$setScenario()`](#method-Project-setScenario)

- [`Project$renameScenario()`](#method-Project-renameScenario)

- [`Project$duplicateScenario()`](#method-Project-duplicateScenario)

- [`Project$createScenariosFromPKML()`](#method-Project-createScenariosFromPKML)

- [`Project$addIndividual()`](#method-Project-addIndividual)

- [`Project$removeIndividual()`](#method-Project-removeIndividual)

- [`Project$setIndividual()`](#method-Project-setIndividual)

- [`Project$addPopulation()`](#method-Project-addPopulation)

- [`Project$removePopulation()`](#method-Project-removePopulation)

- [`Project$setPopulation()`](#method-Project-setPopulation)

- [`Project$getProgrammaticPopulation()`](#method-Project-getProgrammaticPopulation)

- [`Project$addApplication()`](#method-Project-addApplication)

- [`Project$removeApplication()`](#method-Project-removeApplication)

- [`Project$setApplicationParameterSets()`](#method-Project-setApplicationParameterSets)

- [`Project$addOutputPath()`](#method-Project-addOutputPath)

- [`Project$removeOutputPath()`](#method-Project-removeOutputPath)

- [`Project$setOutputPath()`](#method-Project-setOutputPath)

- [`Project$addParameterSet()`](#method-Project-addParameterSet)

- [`Project$removeParameterSet()`](#method-Project-removeParameterSet)

- [`Project$addParameterEntry()`](#method-Project-addParameterEntry)

- [`Project$removeParameterEntry()`](#method-Project-removeParameterEntry)

- [`Project$addInitialConditions()`](#method-Project-addInitialConditions)

- [`Project$removeInitialConditions()`](#method-Project-removeInitialConditions)

- [`Project$addInitialConditionEntry()`](#method-Project-addInitialConditionEntry)

- [`Project$removeInitialConditionEntry()`](#method-Project-removeInitialConditionEntry)

- [`Project$addPlot()`](#method-Project-addPlot)

- [`Project$removePlot()`](#method-Project-removePlot)

- [`Project$addPlotGrid()`](#method-Project-addPlotGrid)

- [`Project$removePlotGrid()`](#method-Project-removePlotGrid)

- [`Project$addDataCombined()`](#method-Project-addDataCombined)

- [`Project$removeDataCombined()`](#method-Project-removeDataCombined)

- [`Project$addObservedData()`](#method-Project-addObservedData)

- [`Project$removeObservedData()`](#method-Project-removeObservedData)

- [`Project$loadObservedData()`](#method-Project-loadObservedData)

- [`Project$getObservedDataNames()`](#method-Project-getObservedDataNames)

- [`Project$addPITask()`](#method-Project-addPITask)

- [`Project$removePITask()`](#method-Project-removePITask)

- [`Project$addPIParameter()`](#method-Project-addPIParameter)

- [`Project$removePIParameter()`](#method-Project-removePIParameter)

- [`Project$addPIOutputMapping()`](#method-Project-addPIOutputMapping)

- [`Project$removePIOutputMapping()`](#method-Project-removePIOutputMapping)

- [`Project$save()`](#method-Project-save)

- [`Project$reload()`](#method-Project-reload)

- [`Project$validate()`](#method-Project-validate)

- [`Project$ensureValid()`](#method-Project-ensureValid)

- [`Project$rawFilePaths()`](#method-Project-rawFilePaths)

- [`Project$rawExcel()`](#method-Project-rawExcel)

- [`Project$isModified()`](#method-Project-isModified)

- [`Project$print()`](#method-Project-print)

------------------------------------------------------------------------

### `Project$new()`

Construct a `Project` from a JSON file path, or create an empty
in-memory project when called with no arguments.

#### Usage

    Project$new(projectFilePath = character())

#### Arguments

- `projectFilePath`:

  A string representing the path to the project JSON file.

------------------------------------------------------------------------

### `Project$addScenario()`

Add scenarios. See
[`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md),
the primary entry point.

#### Usage

    Project$addScenario(
      id,
      modelFile,
      individual = NULL,
      population = NULL,
      application = NULL,
      parameterSets = NULL,
      initialConditions = NULL,
      outputPaths = NULL,
      simulationTime = NULL,
      simulationTimeUnit = "h",
      steadyState = FALSE,
      steadyStateTime = 1000,
      steadyStateTimeUnit = "min",
      overwriteFormulasInSS = FALSE,
      readPopulationFromCSV = FALSE,
      overwrite = FALSE
    )

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

- `modelFile`:

  Character name of the `.pkml` model file. See
  [`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md).

- `individual, population, application`:

  Character id (or `NULL`) of the individual / population / application
  a scenario references. See
  [`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md).

- `parameterSets, initialConditions, outputPaths`:

  Character vectors (or `NULL`) of definition ids a scenario references,
  or the target of an `add*`/`set*`/`remove*` call. See
  [`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md).

- `simulationTime, simulationTimeUnit`:

  Simulation time specification and its unit. See
  [`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md).

- `steadyState, steadyStateTime, steadyStateTimeUnit`:

  Steady-state flag, time, and unit. See
  [`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md).

- `overwriteFormulasInSS, readPopulationFromCSV`:

  Logical scenario options. See
  [`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md).

- `overwrite`:

  Logical scalar controlling duplicate-collision behaviour of the `add*`
  authoring methods. See
  [`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md),
  [`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md).

------------------------------------------------------------------------

### `Project$removeScenario()`

Remove scenarios. See
[`removeScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeScenario.md).

#### Usage

    Project$removeScenario(id)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

------------------------------------------------------------------------

### `Project$setScenario()`

Modify fields of existing scenarios. See
[`setScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/setScenario.md).
The `...` carries only the fields to change (partial update); a field
passed as `NULL` is cleared, an omitted field is left untouched.

#### Usage

    Project$setScenario(id, ...)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

- `...`:

  Passed to the matching authoring free function (e.g. the
  partial-update fields of
  [`setScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/setScenario.md)
  /
  [`setIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/setIndividual.md),
  or the remaining arguments of
  [`createScenariosFromPKML()`](https://esqlabs.github.io/esqlabsR/dev/reference/createScenariosFromPKML.md)).

------------------------------------------------------------------------

### `Project$renameScenario()`

Rename a scenario. See
[`renameScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/renameScenario.md).

#### Usage

    Project$renameScenario(id, newId)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

- `newId`:

  Character new id for
  [`renameScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/renameScenario.md)
  /
  [`duplicateScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/duplicateScenario.md).

------------------------------------------------------------------------

### `Project$duplicateScenario()`

Duplicate a scenario. See
[`duplicateScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/duplicateScenario.md).

#### Usage

    Project$duplicateScenario(id, newId)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

- `newId`:

  Character new id for
  [`renameScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/renameScenario.md)
  /
  [`duplicateScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/duplicateScenario.md).

------------------------------------------------------------------------

### `Project$createScenariosFromPKML()`

Create scenarios from PKML model files. See
[`createScenariosFromPKML()`](https://esqlabs.github.io/esqlabsR/dev/reference/createScenariosFromPKML.md).

#### Usage

    Project$createScenariosFromPKML(pkmlFilePaths, ...)

#### Arguments

- `pkmlFilePaths`:

  Character vector of `.pkml` file paths. See
  [`createScenariosFromPKML()`](https://esqlabs.github.io/esqlabsR/dev/reference/createScenariosFromPKML.md).

- `...`:

  Passed to the matching authoring free function (e.g. the
  partial-update fields of
  [`setScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/setScenario.md)
  /
  [`setIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/setIndividual.md),
  or the remaining arguments of
  [`createScenariosFromPKML()`](https://esqlabs.github.io/esqlabsR/dev/reference/createScenariosFromPKML.md)).

------------------------------------------------------------------------

### `Project$addIndividual()`

Add an individual. See
[`addIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/addIndividual.md).

#### Usage

    Project$addIndividual(...)

#### Arguments

- `...`:

  Passed to the matching authoring free function (e.g. the
  partial-update fields of
  [`setScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/setScenario.md)
  /
  [`setIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/setIndividual.md),
  or the remaining arguments of
  [`createScenariosFromPKML()`](https://esqlabs.github.io/esqlabsR/dev/reference/createScenariosFromPKML.md)).

------------------------------------------------------------------------

### `Project$removeIndividual()`

Remove individuals. See
[`removeIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeIndividual.md).

#### Usage

    Project$removeIndividual(id)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

------------------------------------------------------------------------

### `Project$setIndividual()`

Modify an existing individual. See
[`setIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/setIndividual.md).

#### Usage

    Project$setIndividual(...)

#### Arguments

- `...`:

  Passed to the matching authoring free function (e.g. the
  partial-update fields of
  [`setScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/setScenario.md)
  /
  [`setIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/setIndividual.md),
  or the remaining arguments of
  [`createScenariosFromPKML()`](https://esqlabs.github.io/esqlabsR/dev/reference/createScenariosFromPKML.md)).

------------------------------------------------------------------------

### `Project$addPopulation()`

Add a population. See
[`addPopulation()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPopulation.md).

#### Usage

    Project$addPopulation(...)

#### Arguments

- `...`:

  Passed to the matching authoring free function (e.g. the
  partial-update fields of
  [`setScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/setScenario.md)
  /
  [`setIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/setIndividual.md),
  or the remaining arguments of
  [`createScenariosFromPKML()`](https://esqlabs.github.io/esqlabsR/dev/reference/createScenariosFromPKML.md)).

------------------------------------------------------------------------

### `Project$removePopulation()`

Remove populations. See
[`removePopulation()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePopulation.md).

#### Usage

    Project$removePopulation(id)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

------------------------------------------------------------------------

### `Project$setPopulation()`

Modify an existing population. See
[`setPopulation()`](https://esqlabs.github.io/esqlabsR/dev/reference/setPopulation.md).

#### Usage

    Project$setPopulation(...)

#### Arguments

- `...`:

  Passed to the matching authoring free function (e.g. the
  partial-update fields of
  [`setScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/setScenario.md)
  /
  [`setIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/setIndividual.md),
  or the remaining arguments of
  [`createScenariosFromPKML()`](https://esqlabs.github.io/esqlabsR/dev/reference/createScenariosFromPKML.md)).

------------------------------------------------------------------------

### `Project$getProgrammaticPopulation()`

Internal: fetch a session-added programmatic population by id, or `NULL`
if none was injected under that id. Used by the run path to resolve a
`{type: "programmatic"}` sentinel from the runtime store.

#### Usage

    Project$getProgrammaticPopulation(id)

#### Arguments

- `id`:

  Population id.

------------------------------------------------------------------------

### `Project$addApplication()`

Add an application. See
[`addApplication()`](https://esqlabs.github.io/esqlabsR/dev/reference/addApplication.md).

#### Usage

    Project$addApplication(id, parameterSets = NULL, overwrite = FALSE)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

- `parameterSets, initialConditions, outputPaths`:

  Character vectors (or `NULL`) of definition ids a scenario references,
  or the target of an `add*`/`set*`/`remove*` call. See
  [`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md).

- `overwrite`:

  Logical scalar controlling duplicate-collision behaviour of the `add*`
  authoring methods. See
  [`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md),
  [`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md).

------------------------------------------------------------------------

### `Project$removeApplication()`

Remove applications. See
[`removeApplication()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeApplication.md).

#### Usage

    Project$removeApplication(id)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

------------------------------------------------------------------------

### `Project$setApplicationParameterSets()`

Set an application's parameter sets. See
[`setApplicationParameterSets()`](https://esqlabs.github.io/esqlabsR/dev/reference/setApplicationParameterSets.md).

#### Usage

    Project$setApplicationParameterSets(id, parameterSets)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

- `parameterSets, initialConditions, outputPaths`:

  Character vectors (or `NULL`) of definition ids a scenario references,
  or the target of an `add*`/`set*`/`remove*` call. See
  [`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md).

------------------------------------------------------------------------

### `Project$addOutputPath()`

Add an output path. See
[`addOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/addOutputPath.md).

#### Usage

    Project$addOutputPath(id, path, overwrite = FALSE)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

- `path`:

  Character path: an output-path string
  ([`addOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/addOutputPath.md))
  or a parameter/initial-condition path
  ([`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md)
  /
  [`addInitialConditionEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addInitialConditionEntry.md)).

- `overwrite`:

  Logical scalar controlling duplicate-collision behaviour of the `add*`
  authoring methods. See
  [`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md),
  [`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md).

------------------------------------------------------------------------

### `Project$removeOutputPath()`

Remove output paths. See
[`removeOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeOutputPath.md).

#### Usage

    Project$removeOutputPath(id)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

------------------------------------------------------------------------

### `Project$setOutputPath()`

Modify an existing output path. See
[`setOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/setOutputPath.md).

#### Usage

    Project$setOutputPath(id, path)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

- `path`:

  Character path: an output-path string
  ([`addOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/addOutputPath.md))
  or a parameter/initial-condition path
  ([`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md)
  /
  [`addInitialConditionEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addInitialConditionEntry.md)).

------------------------------------------------------------------------

### `Project$addParameterSet()`

Add a parameter set. See
[`addParameterSet()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterSet.md).

#### Usage

    Project$addParameterSet(id, overwrite = FALSE)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

- `overwrite`:

  Logical scalar controlling duplicate-collision behaviour of the `add*`
  authoring methods. See
  [`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md),
  [`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md).

------------------------------------------------------------------------

### `Project$removeParameterSet()`

Remove parameter sets. See
[`removeParameterSet()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeParameterSet.md).

#### Usage

    Project$removeParameterSet(id)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

------------------------------------------------------------------------

### `Project$addParameterEntry()`

Add an entry to a parameter set. See
[`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md).

#### Usage

    Project$addParameterEntry(
      id,
      containerPath,
      parameterName,
      value,
      units = NULL,
      overwrite = FALSE
    )

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

- `containerPath, parameterName, value, units, unit`:

  Parameter- and initial-condition entry fields. See
  [`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md)
  /
  [`addInitialConditionEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addInitialConditionEntry.md).

- `overwrite`:

  Logical scalar controlling duplicate-collision behaviour of the `add*`
  authoring methods. See
  [`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md),
  [`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md).

------------------------------------------------------------------------

### `Project$removeParameterEntry()`

Remove an entry from a parameter set. See
[`removeParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeParameterEntry.md).

#### Usage

    Project$removeParameterEntry(id, containerPath, parameterName)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

- `containerPath, parameterName, value, units, unit`:

  Parameter- and initial-condition entry fields. See
  [`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md)
  /
  [`addInitialConditionEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addInitialConditionEntry.md).

------------------------------------------------------------------------

### `Project$addInitialConditions()`

Add an initial-conditions set. See
[`addInitialConditions()`](https://esqlabs.github.io/esqlabsR/dev/reference/addInitialConditions.md).

#### Usage

    Project$addInitialConditions(id, overwrite = FALSE)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

- `overwrite`:

  Logical scalar controlling duplicate-collision behaviour of the `add*`
  authoring methods. See
  [`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md),
  [`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md).

------------------------------------------------------------------------

### `Project$removeInitialConditions()`

Remove initial-conditions sets. See
[`removeInitialConditions()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeInitialConditions.md).

#### Usage

    Project$removeInitialConditions(id)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

------------------------------------------------------------------------

### `Project$addInitialConditionEntry()`

Add an entry to an initial-conditions set. See
[`addInitialConditionEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addInitialConditionEntry.md).

#### Usage

    Project$addInitialConditionEntry(id, path, value, unit, overwrite = FALSE)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

- `path`:

  Character path: an output-path string
  ([`addOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/addOutputPath.md))
  or a parameter/initial-condition path
  ([`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md)
  /
  [`addInitialConditionEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addInitialConditionEntry.md)).

- `overwrite`:

  Logical scalar controlling duplicate-collision behaviour of the `add*`
  authoring methods. See
  [`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md),
  [`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md).

- `containerPath, parameterName, value, units, unit`:

  Parameter- and initial-condition entry fields. See
  [`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md)
  /
  [`addInitialConditionEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addInitialConditionEntry.md).

------------------------------------------------------------------------

### `Project$removeInitialConditionEntry()`

Remove an entry from an initial-conditions set. See
[`removeInitialConditionEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeInitialConditionEntry.md).

#### Usage

    Project$removeInitialConditionEntry(id, path)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

- `path`:

  Character path: an output-path string
  ([`addOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/addOutputPath.md))
  or a parameter/initial-condition path
  ([`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md)
  /
  [`addInitialConditionEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addInitialConditionEntry.md)).

------------------------------------------------------------------------

### `Project$addPlot()`

Add a plot. See
[`addPlot()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPlot.md).

#### Usage

    Project$addPlot(...)

#### Arguments

- `...`:

  Passed to the matching authoring free function (e.g. the
  partial-update fields of
  [`setScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/setScenario.md)
  /
  [`setIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/setIndividual.md),
  or the remaining arguments of
  [`createScenariosFromPKML()`](https://esqlabs.github.io/esqlabsR/dev/reference/createScenariosFromPKML.md)).

------------------------------------------------------------------------

### `Project$removePlot()`

Remove plots. See
[`removePlot()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePlot.md).

#### Usage

    Project$removePlot(id)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

------------------------------------------------------------------------

### `Project$addPlotGrid()`

Add a plot grid. See
[`addPlotGrid()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPlotGrid.md).

#### Usage

    Project$addPlotGrid(...)

#### Arguments

- `...`:

  Passed to the matching authoring free function (e.g. the
  partial-update fields of
  [`setScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/setScenario.md)
  /
  [`setIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/setIndividual.md),
  or the remaining arguments of
  [`createScenariosFromPKML()`](https://esqlabs.github.io/esqlabsR/dev/reference/createScenariosFromPKML.md)).

------------------------------------------------------------------------

### `Project$removePlotGrid()`

Remove plot grids. See
[`removePlotGrid()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePlotGrid.md).

#### Usage

    Project$removePlotGrid(id)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

------------------------------------------------------------------------

### `Project$addDataCombined()`

Add a data-combined entry. See
[`addDataCombined()`](https://esqlabs.github.io/esqlabsR/dev/reference/addDataCombined.md).

#### Usage

    Project$addDataCombined(
      id,
      simulated = list(),
      observed = list(),
      overwrite = FALSE
    )

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

- `simulated, observed`:

  Simulated / observed inputs to a `DataCombined`. See
  [`addDataCombined()`](https://esqlabs.github.io/esqlabsR/dev/reference/addDataCombined.md).

- `overwrite`:

  Logical scalar controlling duplicate-collision behaviour of the `add*`
  authoring methods. See
  [`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md),
  [`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md).

------------------------------------------------------------------------

### `Project$removeDataCombined()`

Remove data-combined entries. See
[`removeDataCombined()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeDataCombined.md).

#### Usage

    Project$removeDataCombined(id)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

------------------------------------------------------------------------

### `Project$addObservedData()`

Add observed data. See
[`addObservedData()`](https://esqlabs.github.io/esqlabsR/dev/reference/addObservedData.md).

#### Usage

    Project$addObservedData(entry, overwrite = FALSE)

#### Arguments

- `entry`:

  An observed-data source: a `DataSet` or a configuration list. See
  [`addObservedData()`](https://esqlabs.github.io/esqlabsR/dev/reference/addObservedData.md).

- `overwrite`:

  Logical scalar controlling duplicate-collision behaviour of the `add*`
  authoring methods. See
  [`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md),
  [`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md).

------------------------------------------------------------------------

### `Project$removeObservedData()`

Remove observed data. See
[`removeObservedData()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeObservedData.md).

#### Usage

    Project$removeObservedData(id)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

------------------------------------------------------------------------

### `Project$loadObservedData()`

Load the project's observed data. See
[`loadObservedData()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadObservedData.md).

#### Usage

    Project$loadObservedData()

------------------------------------------------------------------------

### `Project$getObservedDataNames()`

Names of the project's observed data. See
[`getObservedDataNames()`](https://esqlabs.github.io/esqlabsR/dev/reference/getObservedDataNames.md).

#### Usage

    Project$getObservedDataNames()

------------------------------------------------------------------------

### `Project$addPITask()`

Add a parameter-identification task. See
[`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md).

#### Usage

    Project$addPITask(
      id,
      scenarios,
      parameters,
      outputMappings,
      configuration = list(),
      overwrite = FALSE
    )

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

- `scenarios, parameters, outputMappings, configuration`:

  Parameter-identification task components. See
  [`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md).

- `overwrite`:

  Logical scalar controlling duplicate-collision behaviour of the `add*`
  authoring methods. See
  [`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md),
  [`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md).

------------------------------------------------------------------------

### `Project$removePITask()`

Remove parameter-identification tasks. See
[`removePITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePITask.md).

#### Usage

    Project$removePITask(id)

#### Arguments

- `id`:

  Character id (name) of the definition to add, modify, or remove.

------------------------------------------------------------------------

### `Project$addPIParameter()`

Add a parameter to a PI task. See
[`addPIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIParameter.md).

#### Usage

    Project$addPIParameter(
      task,
      path,
      scenarios,
      minValue,
      maxValue,
      startValue,
      units = NULL,
      id = NULL,
      overwrite = FALSE
    )

#### Arguments

- `task`:

  Character id of the parameter-identification task a sub-item belongs
  to. See
  [`addPIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIParameter.md).

- `path`:

  Character path: an output-path string
  ([`addOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/addOutputPath.md))
  or a parameter/initial-condition path
  ([`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md)
  /
  [`addInitialConditionEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addInitialConditionEntry.md)).

- `scenarios, parameters, outputMappings, configuration`:

  Parameter-identification task components. See
  [`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md).

- `minValue, maxValue, startValue`:

  Numeric bounds and start value of a PI parameter. See
  [`addPIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIParameter.md).

- `id`:

  Character id (name) of the definition to add, modify, or remove.

- `overwrite`:

  Logical scalar controlling duplicate-collision behaviour of the `add*`
  authoring methods. See
  [`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md),
  [`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md).

- `containerPath, parameterName, value, units, unit`:

  Parameter- and initial-condition entry fields. See
  [`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md)
  /
  [`addInitialConditionEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addInitialConditionEntry.md).

------------------------------------------------------------------------

### `Project$removePIParameter()`

Remove a parameter from a PI task. See
[`removePIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePIParameter.md).

#### Usage

    Project$removePIParameter(task, id)

#### Arguments

- `task`:

  Character id of the parameter-identification task a sub-item belongs
  to. See
  [`addPIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIParameter.md).

- `id`:

  Character id (name) of the definition to add, modify, or remove.

------------------------------------------------------------------------

### `Project$addPIOutputMapping()`

Add an output mapping to a PI task. See
[`addPIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIOutputMapping.md).

#### Usage

    Project$addPIOutputMapping(
      task,
      outputPath,
      observedData,
      scenarios,
      scaling = NULL,
      xOffset = 0,
      yOffset = 0,
      xFactor = 1,
      yFactor = 1,
      weight = NULL,
      id = NULL,
      overwrite = FALSE
    )

#### Arguments

- `task`:

  Character id of the parameter-identification task a sub-item belongs
  to. See
  [`addPIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIParameter.md).

- `outputPath, observedData, scaling, xOffset, yOffset, xFactor, yFactor, weight`:

  Fields of a PI output mapping. See
  [`addPIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIOutputMapping.md).

- `scenarios, parameters, outputMappings, configuration`:

  Parameter-identification task components. See
  [`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md).

- `id`:

  Character id (name) of the definition to add, modify, or remove.

- `overwrite`:

  Logical scalar controlling duplicate-collision behaviour of the `add*`
  authoring methods. See
  [`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md),
  [`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md).

------------------------------------------------------------------------

### `Project$removePIOutputMapping()`

Remove an output mapping from a PI task. See
[`removePIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePIOutputMapping.md).

#### Usage

    Project$removePIOutputMapping(task, id)

#### Arguments

- `task`:

  Character id of the parameter-identification task a sub-item belongs
  to. See
  [`addPIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIParameter.md).

- `id`:

  Character id (name) of the definition to add, modify, or remove.

------------------------------------------------------------------------

### `Project$save()`

Save the project's in-memory edits to its on-disk tree. See
[`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md).

#### Usage

    Project$save()

------------------------------------------------------------------------

### `Project$reload()`

Discard in-memory edits and re-read from disk. See
[`reloadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/reloadProject.md).

#### Usage

    Project$reload()

------------------------------------------------------------------------

### `Project$validate()`

Validate the project. See
[`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md).

#### Usage

    Project$validate()

------------------------------------------------------------------------

### `Project$ensureValid()`

Package-internal pre-op validation gate. Runs targeted validation for
the `sections` an operation depends on and aborts with a formatted
multi-error message on any critical errors, short-circuiting when the
project is already validated. Called by the run/plot/parameter-
identification entry points; not intended for end users.

#### Usage

    Project$ensureValid(sections, opName)

#### Arguments

- `sections`:

  Non-empty character vector of section names the calling operation
  requires.

- `opName`:

  Short label used in the abort message (e.g. `"runScenarios"`).

------------------------------------------------------------------------

### `Project$rawFilePaths()`

Package-internal reader for the raw `filePaths` block: a named list of
`list(value, description)` records for the four live working folders.
Unlike `project$paths$...` (which returns resolved values), this keeps
the per-folder descriptions the Excel bridge round-trips. Consumed by
the JSON writer and the Excel exporter; not intended for end users.

#### Usage

    Project$rawFilePaths()

------------------------------------------------------------------------

### `Project$rawExcel()`

Package-internal reader for the raw `excel` block: a named list of
`list(value, description)` records for the Excel-bridge sheet-name
fields. Unlike `project$excel$...` (which returns resolved values), this
keeps the descriptions. Empty when the project has no Excel side-car.
Consumed by the JSON writer and the Excel exporter; not intended for end
users.

#### Usage

    Project$rawExcel()

------------------------------------------------------------------------

### `Project$isModified()`

Package-internal reader for the in-memory dirty bit: `TRUE` when the
project carries edits not yet reconciled to the on-disk `definitions/`
tree. The same signal
[`projectStatus()`](https://esqlabs.github.io/esqlabsR/dev/reference/projectStatus.md)
reports on its memory-vs-tree axis. Not intended for end users; users
read the sync state through `project$status` or
[`projectStatus()`](https://esqlabs.github.io/esqlabsR/dev/reference/projectStatus.md).

#### Usage

    Project$isModified()

------------------------------------------------------------------------

### `Project$print()`

Print a summary of the Project. Each section is rendered by the same
per-group block method the field groups (`project$info`,
`project$paths`, `project$definitions`, `project$excel`) print, so the
project summary and a group's own print never drift.

#### Usage

    Project$print(...)

#### Arguments

- `...`:

  Unused; present for S3 method consistency.
