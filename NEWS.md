# esqlabsR (development version)

## Breaking changes

- The project model is now JSON-first. A project is a `Project` (R6 class) loaded with `loadProject()`; `Project` replaces `ProjectConfiguration` as the canonical in-memory representation, merging the project sections (scenarios, individuals, populations, applications, observed data, plots, parameter identification) with the file paths previously held by `ProjectConfiguration`. Authoring is write-through: every `add*` / `remove*` / `set*` edit persists to its definition file immediately, so there is no separate save step; `saveSnapshot()` writes a single-file shareable freeze-frame and `loadSnapshot()` reads one. Excel becomes a secondary I/O bridge through `importProjectFromExcel()` and `exportProjectToExcel()`. (#908)
- Every project section is stored as a per-definition tree, not inside `Project.json`. A project on disk is now a directory: each section is a subfolder under a `definitions/` directory next to `Project.json`, with one JSON file per definition (`definitions/scenarios/`, `definitions/individuals/`, `definitions/populations/`, `definitions/parameter-sets/`, `definitions/applications/`, `definitions/output-paths/`, `definitions/observed-data/`, `definitions/parameter-identification/`, and the plots section split across `definitions/data-combined/`, `definitions/plots/`, and `definitions/plot-grids/`, one file per data combination, plot, and plot grid). The `definitions/` folder holds the project's authored definition files, separated from the referenced working files (`Models/`, `Data/`, `Populations/`, `Results/`). `loadProject()` reads the tree; every section mutator (`addScenario()`, `addIndividual()`, `addOutputPath()`, the parameter-set, application, plot, observed-data, and PI helpers, and any write-back through `project$<section>`) is write-through, structurally validating each changed definition and writing or deleting only that definition's single file (so a mutation leaves the other definitions' files untouched on disk and adding many definitions one at a time scales linearly, not quadratically). A bulk tree write is all-or-nothing: if any definition fails to serialize, no file is written, so a partial tree never lands on disk. A definition's id is canonicalized to a safe, lowercase single-path-segment id so it maps to a file inside its `definitions/<kind>/` subfolder, and a scenario's `scenarioName` must match the key it is stored under; a non-ASCII id round-trips correctly. A single-file `Project.json` with every section inlined still loads (treated as a self-contained snapshot), so existing single-file projects keep working. The load path enforces the same id discipline the write path does: a `definitions/` (or `definitions/<kind>/`) path that exists as a regular file rather than a directory aborts the load (a corrupted or mis-synced tree is no longer read as an empty project), a definition file missing its id field or whose id disagrees with its filename aborts naming the file (so two files can never silently collapse onto one id), and a scalar field corrupted into an empty object `{}` (the usual `jsonlite` `null` round-trip) on any kind aborts naming the field and file. Two `observedData` declarations whose sources share a basename now fail fast rather than silently dropping one. (#908)
- `ScenarioConfiguration`, `addScenarioConfigurationsToExcel()`, `createScenarioConfigurationsFromPKML()`, `readScenarioConfigurationFromExcel()`, `setApplications()`, `LegacyScenario`, and `createScenarios()` are removed. `runScenarios()` now accepts only a `Project`, and scenarios are constructed from PKML via `createScenariosFromPKML(pkmlFilePaths, project, ...)`. (#908)
- `Scenario` is now a plain-data record built with `Scenario()`. Reading an entry from `project$scenarios` returns an independent copy; the section accessor is read-only, so to apply a change you read the copy, edit it, and re-submit it through an authoring function (`sc <- project$scenarios[["X"]]; sc$modelFile <- "..."; setScenario(project, "X", ...)`). (#908, #1048)
- The `Project` section accessors (`scenarios`, `outputPaths`, `individuals`, `populations`, `parameterSets`, `observedData`, `dataCombined`, `plots`, `plotGrids`, `parameterIdentification`) are read-only from the project handle: assigning through one (`project$scenarios <- ...`, `project$scenarios[["X"]] <- sc`, the nested `project$scenarios[["X"]]$field <- v`, or `project$scenarios[-i] <- ...`) aborts with a message pointing at the authoring functions. The only sanctioned ways to change a definition are an authoring function (`addScenario()` / `setScenario()` / `removeScenario()` and their per-section siblings) or editing the definition's JSON file directly; both write through to the definition file, structurally validating each changed definition (a wrong-typed or unknown field is rejected, not silently persisted). `project$jsonPath` is also read-only. Because every authoring edit lands on disk immediately, the `project$modified` field and the `project$sync()` method are removed; `project$syncStatus()` now reports only whether the project's Excel side-car (`Project.xlsx`) is in sync with its definition files (returning `excel_in_sync`, or `NA` when there is no side-car). (#1048)
- Project mutation is done through standalone functions, not methods: `addScenario(project, ...)`, `removeScenario(project, ...)`, and the matching `add*` / `remove*` helpers for every section. (#908)
- `validateAllConfigurations()` is removed; use `validateProject(project)`. (#908)
- Parameter identification is JSON-first. PI tasks live as a `parameterIdentification` section on `Project` and run via `runPI(project, tasks = NULL, observedData = NULL, stopIfParameterNotFound = TRUE)`, built from the plain-data records `PITask`, `PIParameter`, and `PIOutputMapping` (one entry per optimisation variable, one per output/dataset pair). The Excel `PITaskConfiguration`, `readPITaskConfigurationFromExcel()`, and `createPITasks()` are removed. `runPI()` hard-fails on build errors (bad parameter paths, unknown outputs, missing observed data) and only soft-fails on numerical optimisation failures. (#928)
- `createDefaultProjectConfiguration()` is removed; use `loadProject()`. (#908)
- `loadObservedDataFromExcel()` and `loadObservedDataFromPKML()` are removed; use `loadObservedData(project)` on a JSON-first `Project`. (#908)
- `readPopulationCharacteristicsFromXLS()`, `readIndividualCharacteristicsFromXLS()`, `writeIndividualToXLS()`, `writeParameterStructureToXLS()`, and `exportParametersToXLS()` are removed. The supported Excel surface is restricted to Excel <-> JSON interop via `importProjectFromExcel()` and `exportProjectToExcel()`. (#908)
- `ExportConfiguration` (R6 class) and `createEsqlabsExportConfiguration()` are removed. Save the plot objects returned by `createPlots()` directly via `ggplot2::ggsave()`. (#908)
- Individual parameter sets in `Individuals.xlsx` must now be specified explicitly via the required `Individual Parameter Sets` column in the `IndividualBiometrics` sheet, a comma-separated list of sheet names applied in order. The previous fallback that applied a sheet named after the `IndividualId` is removed; existing files must add and populate the new column. (#970)
- The leading identifier argument is now uniformly named `id` across every `add*` / `set*` / `remove*` function, so the same argument names a definition everywhere: `addScenario(project, id, ...)`, `setScenario(project, id, ...)`, `removeScenario(project, id)`, `addIndividual()` / `setIndividual()` / `removeIndividual()`, `addPopulation()` / `setPopulation()` / `removePopulation()`, `addApplication()` / `removeApplication()` / `setApplicationParameterSets()`, `addPlot()` / `removePlot()`, `addPlotGrid()` / `removePlotGrid()`, `addDataCombined()` / `removeDataCombined()`, and `removeObservedData()` (previously `scenarioName`, `individualId`, `populationId`, `applicationId`, `plotId`, or `name`). `addOutputPath()` / `removeOutputPath()` / `setOutputPath()` and the parameter-set and PI-task functions already used `id`. Positional calls are unaffected; update calls that pass the leading identifier by name. (#908)
- Reference arguments are suffixless, named after the definition kind they point at rather than carrying an `Id` / `Ids` / `Name` / `Names` suffix. `addScenario()` / `setScenario()` now take `individual`, `population`, `application`, `parameterSets`, and `outputPaths` (were `individualId`, `populationId`, `applicationProtocol`, `modelParameterSets`, `outputPathIds`); `runScenarios()`, `loadScenarioResults()`, and `createScenariosFromPKML()` take `scenarios` (was `scenarioNames`); `createPlots()` takes `plotGrids` and `plots` (were `plotGridNames` and `plotIds`); `createDataCombined()` takes `dataCombined` and `plotGrids`; `addPlot()` takes `dataCombined` (was `dataCombinedId`); `addPlotGrid()` takes `plots` (was `plotIds`); `runPI()` takes `tasks` (was `piTaskNames`); and `addPIParameter()`, `addPIOutputMapping()`, `removePIParameter()`, `removePIOutputMapping()` take `task` (was `taskId`), with `addPIOutputMapping()` also taking `outputPath` and `observedData` (were `outputPathId`, `observedDataId`). The on-disk definition JSON keys match the new argument names (a scenario file now uses `individual` / `population` / `application` / `parameterSets` / `outputPaths`, a plot file `dataCombined`, a plot grid `plots`, a PI output mapping `outputPath` / `observedData`). Update calls that pass these by name and any hand-edited definition files. (#908)
- Definition ids are canonicalized rather than rejected. When an id is supplied to an `add*` / `set*` / `remove*` function, or referenced through a foreign-key argument (a scenario's `individual` / `population` / `application` / `parameterSets` / `outputPaths`, an individual's or application's `parameterSets`, a PI task's scenario / output-path references), it is lowercased and made a safe single-path-segment id (forbidden characters `/ \ : * ? " < > |`, control characters, leading/trailing dots or spaces are replaced, and a Windows reserved basename such as `CON` is suffixed). The same transform is applied to both a definition and a reference, so a definition and a reference made from the same typed string always resolve to each other. A warning names the canonical id whenever it differs from the input, safely quoting an id or reference that itself contains `{` or `}` rather than mishandling it; two distinct ids that canonicalize to the same id error. A scenario name that previously had to be a valid path segment is now canonicalized to one instead of being rejected, and two ids differing only in case no longer collide on a case-insensitive filesystem (they canonicalize to one id, so the second is a duplicate). An id too long to be a safe filename (over 250 bytes) is rejected up front with a message naming the id and the limit, rather than failing later with an opaque file-connection error. (#908)
- The three parameter-set kinds are unified into one. `modelParameterSets`, `individualParameterSets`, and `applicationParameterSets` (separate `Project` sections and separate JSON sections) are merged into a single `parameterSets` map keyed by set id; a scenario, an individual, and an application all reference their sets through a `parameterSets` argument, and all three resolve against the one map. The per-kind authoring functions (`addModelParameterSet()` / `addIndividualParameterSet()` / `addApplicationParameterSet()`, their `*Entry` variants, and the matching `remove*`) are replaced by one family: `addParameterSet()`, `removeParameterSet()`, `addParameterEntry()`, `removeParameterEntry()`. `addParameterEntry()` and `removeParameterEntry()` accept parallel vectors for `containerPath` / `parameterName` / `value` / `units`, so a whole set is built (or trimmed) in one call and written to disk once; a per-entry loop, which rewrites the whole set file on every call, is no longer needed for bulk authoring. Set ids must be unique across what were the three namespaces; a `Project.json` (or Excel import) that defines the same id in more than one section fails the load. A legacy `Project.json` that still carries the three separate sections is migrated into the single `parameterSets` map on load. (#1077)
- `removePIParameter()` and `removePIOutputMapping()` now auto-remove the parent PI task from the project when it becomes empty (no parameters and no output mappings), emitting a warning. This aligns with the behavior of `removeParameterEntry()` for empty parameter sets. (#1079)
- `saveScenarioResults()` renames its second argument from `projectConfiguration` to `project` to match the v6 naming convention. Update any calls that pass the argument by name. (#1062)

## Major changes

- `loadProject(path)` loads a `Project` from a `Project.json` and is the primary entry point for the workflow. (#908)
- The `Project.json` container separates the project's two path concerns and gains metadata. The four live working folders the runtime reads (`modelFolder`, `dataFolder`, `outputFolder`, `populationsFolder`) stay in the `filePaths` block (`project$filePaths`), while the Excel import/export sheet-name fields (`configurationsFolder`, `modelParamsFile`, `individualsFile`, `populationsFile`, `scenariosFile`, `applicationsFile`, `plotsFile`, `parameterIdentificationFile`, `initialConditionsFile`) move to a separate `excel` block (`project$excel`), written only when the project has an Excel side-car. The container also carries an optional human `name` and `description` (surfaced in `print(project)`, writable via `project$name` / `project$description`, and preserved through an `exportProjectToExcel()` / `importProjectFromExcel()` round-trip), an optional `definitionsFolder` (default `"definitions"`) that makes the definition-tree location configurable, and an optional `defaultSimulationRunOptions`. A legacy `Project.json` that still lists all eleven path fields in one flat `filePaths` block loads unchanged (the fields are split into the two blocks on read). (#908)
- `runScenarios()` falls back to the project's `defaultSimulationRunOptions` (a `{numberOfCores, checkForNegativeValues, showProgress}` block in `Project.json`) when the caller passes no `simulationRunOptions`, so a run is reproducible from the shared project artifact; an explicit `simulationRunOptions` argument still wins, and an absent project default keeps the previous behaviour. (#908)
- `saveSnapshot(project, path)` writes a derived single-file snapshot with every section inlined, for sharing or archiving, and `loadSnapshot(file, dir)` loads a `Project` from such a snapshot, writing it back out as a full on-disk tree project at `dir` (a `Project.json` container plus a `definitions/<kind>/` tree for every section) and returning the `Project` bound to `dir`; loading a snapshot materializes it. The snapshot is a `.esqlabsR` file (JSON content with a distinct extension that marks it as a portable shareable freeze-frame, separate from the `Project.json` container of a live tree project): `saveSnapshot()` normalizes its output to `.esqlabsR` (a path with no extension or a `.json` extension is written as `.esqlabsR`; a different explicit extension is honored as given, with a note), and `loadSnapshot()` reads a `.esqlabsR` file and still accepts a plain inlined `Project.json` for back-compatibility, canonicalizing every id and every reference to one (across scenarios, output paths, parameter sets, individuals, applications, plot `dataCombined` rows, and parameter-identification tasks) as it migrates, so a legacy single-file project with non-canonical ids explodes losslessly into the tree with its foreign keys intact. The `saveSnapshot()` `path` must resolve to a location other than the project's own container (a snapshot is a derived artifact, not the authoritative `definitions/` tree); `loadSnapshot()`'s `dir` is created when absent and must not already contain a project. Reloading a snapshot reproduces the project, so snapshot then load then snapshot is a fixed point. (#908)
- `validateProject(project)` validates a parsed project and reports per-section critical errors and warnings. A dangling cross-reference (a scenario, individual, application, plot, or PI task pointing at an id with no matching definition) now suggests the closest existing ids ("did you mean '...'?") in its message. A DataCombined entry that is missing its required `label` is now reported here, so a label-less record is caught up front instead of passing validation and then failing later at plotting with a misleading message. Removing a definition that is still referenced warns at removal time as well; `removeScenario()` now warns when a `dataCombined` definition still points at the removed scenario, matching the existing warnings from `removeIndividual()` and the other removers. The returned results object now prints a readable summary grouped by definition type (a cross marks each critical error, a `!` marks each warning, with overall counts in the header), while the structured object stays fully indexable (`results$scenarios$critical_errors`). (#908)
- `createDataCombined(project, ...)` and `createPlots(project, ...)` accept a `Project` directly and resolve their inputs from the JSON, with `loadObservedData(project)` dispatching across `excel` / `pkml` / `script` observed-data sources. Both take the run output (the named list of Scenario Results from `runScenarios()`) through an argument named `scenarioResults`. `createPlots()` gains a `plots` argument that renders standalone single plots (each the same render a grid cell gets, keyed by `plotId` in the returned list) alongside the plot grids selected by `plotGrids`; the two arguments are independent selectors (a plot that is also inside a requested grid still gets its own standalone entry), and with neither argument the default is still all plot grids. (#908)
- The plots concern is three top-level project sections, each a keyed list like every other section: `project$dataCombined` (keyed by `dataCombinedId`), `project$plots` (the plot list, keyed by `plotId`), and `project$plotGrids` (keyed by `plotGridId`). `project$plots` is the plot list itself (it no longer nests a `dataCombined` / `plotConfiguration` / `plotGrids` trio); a single plot is `project$plots[["id"]]`, a single grid `project$plotGrids[["id"]]`, and a single data combination `project$dataCombined[["id"]]`, each a named list of its fields. On disk each section is still its own one-file-per-definition tree (`definitions/data-combined/`, `definitions/plots/`, `definitions/plot-grids/`). (#908)
- The whole programmatic authoring API now accepts a vector of ids and acts on all of them in a single write-through: `addScenario()` / `setScenario()` / `removeScenario()`, `addIndividual()` / `setIndividual()` / `removeIndividual()`, `addPopulation()` / `setPopulation()` / `removePopulation()`, `addApplication()` / `removeApplication()` / `setApplicationParameterSets()`, `addOutputPath()` / `setOutputPath()` / `removeOutputPath()`, `addParameterSet()` / `removeParameterSet()` (and the already-vectorized `addParameterEntry()` / `removeParameterEntry()`), `removeObservedData()`, `removePITask()`, and the plot helpers `addPlot()` / `removePlot()` / `addPlotGrid()` / `removePlotGrid()` / `addDataCombined()` / `removeDataCombined()`. For the `add*` / `set*` functions the id vector sets the count; a scalar-per-definition field is length 1 (recycled to every definition) or the same length as `id` (aligned by position), and a vector-valued-per-definition field (an individual's or application's `parameterSets`, a scenario's `parameterSets` / `outputPaths`, a plot's `quantiles`, a grid's `plots`) is applied whole to every definition (pass a list as long as `id` to vary it per definition). The batch is all-or-nothing (if any definition is invalid, nothing is written), and `remove*` takes a vector of ids (a not-found id warns and is skipped). `addObservedData()`, `renameScenario()`, `duplicateScenario()`, `addPITask()`, and the per-task PI sub-definition helpers stay single-definition. (#908)
- A single project definition now prints a readable summary of its configuration instead of a raw list. The structured definition types (a scenario, individual, population, application, parameter set, observed-data source, plot, plot grid, DataCombined, and the parameter-identification records) print in the OSP house style, showing the id and configured fields; e.g. `project$individuals[["adult_male"]]`, `project$parameterSets[["global"]]`, or `project$plots[["p1"]]`. An output path stays a bare OSPS-notation string (`project$outputPaths[["id"]]`) and prints as that string. (#908)
- A section accessor now prints a count and the definition ids instead of dumping the raw list. `project$individuals`, `project$scenarios`, `project$populations`, `project$applications`, `project$outputPaths`, `project$parameterSets`, `project$observedData`, `project$parameterIdentification`, `project$dataCombined`, `project$plots`, and `project$plotGrids` print as `<section>: N definitions` followed by the ids. The accessor still behaves exactly as the underlying named list (`length()`, `names()`, `[[`, `[`, `c()`), so existing code that indexes a section is unaffected. (#908)
- `Project`'s `print()` now renders through the shared `ospsuite.utils` print styling, grouping the console summary into a `Paths` section (the working folders), a `Definitions` section (per-section entry counts, with zero-count sections omitted), and, only when the project has an Excel side-car, an `Excel` section, matching the house style of the other print methods. (#1109)
- The full programmatic mutation API: `addScenario()` / `removeScenario()`, `addIndividual()` / `removeIndividual()`, `addPopulation()` / `removePopulation()`, `addApplication()` / `removeApplication()`, `addOutputPath()` / `removeOutputPath()`, the unified parameter-set helpers (`addParameterSet()`, `removeParameterSet()`, `addParameterEntry()`, `removeParameterEntry()`), `addObservedData()` / `removeObservedData()`, the plot helpers, and the PI helpers `addPITask()` / `removePITask()` (plus inline `addPIParameter()` / `addPIOutputMapping()` and their removals). (#908, #1076, #1077)
- `setIndividual(project, id, ...)` modifies one or more fields of an existing individual in place (partial update: only the fields you pass change), with the same validation as `addIndividual()`, instead of the read-copy-mutate-write-back idiom. (#908)
- `setOutputPath(project, id, path)` changes the OSPS-notation path string bound to an existing output-path id in place, leaving every scenario reference to that id intact. (#908)
- `setPopulation(project, id, ...)` modifies one or more fields of an existing population in place (partial update: only the fields you pass change), with the same validation as `addPopulation()`. (#908)
- `setScenario(project, id, ...)` modifies one or more fields of an existing scenario in place (partial update: only the fields you pass change; passing `NULL` clears an optional field), with the same foreign-key and structural validation as `addScenario()` and the same write-through persistence as a scenario write-back. (#908)
- `duplicateScenario(project, id, newId)` creates an independent deep copy of an existing scenario under a new id, leaving the original untouched; the copy is written through as a new definition file. The new id is canonicalized like `addScenario()`'s, and an id that already belongs to a scenario errors. (#908)
- `renameScenario(project, id, newId)` renames an existing scenario, moving its definition file (the old file is removed and a new one written), updating the in-memory key, and keeping the stored name equal to the new key so a reload round-trips. Both ids are canonicalized like `addScenario()`'s; a non-existent source id or an already-taken target id errors. (#908)
- `PITask()`, `PIParameter()`, and `PIOutputMapping()` are exported, so a parameter identification task can be authored from scratch with only `library(esqlabsR)`: compose the records, pass them to `addPITask(project, ...)`, then run with `runPI(project)`. A hand-built record is identical to one parsed from a `Project.json`. (#928)
- `runPI(project)` now warns when a parameter identification run reports convergence but the uncertainty for a parameter could not be quantified (standard deviation, CV, and confidence interval all `NA`), naming the task and the parameter and listing the likely causes (a singular or ill-conditioned Hessian, the estimate at a bound, or an objective insensitive to the parameter), so an estimate with no usable uncertainty is no longer silently reported as converged. (#928)
- `addInitialConditions()`, `removeInitialConditions()`, `addInitialConditionEntry()`, and `removeInitialConditionEntry()` author molecule start values (initial conditions). Initial conditions are their own project section (`project$initialConditions`), a map of set id to a list of `{path, value, unit}` records, stored on disk as a per-definition tree under `definitions/initial-conditions/`. A set is applied to a scenario through the scenario's `initialConditions` argument (`addScenario()` / `setScenario()`), a whole-vector reference validated eagerly at add time and again by `validateProject()`; at run time the referenced sets are applied via `ospsuite::setQuantityValuesByPath()` after the parameters. `addInitialConditionEntry()` / `removeInitialConditionEntry()` accept parallel vectors for `path` / `value` / `unit`, so a whole set is built (or trimmed) in one write. Initial conditions round-trip through Excel via a per-set-sheet `InitialConditions.xlsx` workbook (the `Is Present`, `Scale Divisor`, and `Neg. Values Allowed` columns are regenerated with defaults on export), and `readInitialConditionsFromXLS()` reads such a workbook into a `{paths, values, units}` structure directly. (#973)
- `initializeSimulation()` gains an `additionalInitialConditions` argument (a `{paths, values, units}` structure) that sets molecule start values via `ospsuite::setQuantityValuesByPath()` after the parameters are applied. (#973)
- `initProject(destination, type, createExcel, overwrite)` scaffolds a project. `type` is `"minimal"` (default) or `"example"`; with `createExcel = TRUE` (default) Excel side-cars are also produced. (#908)
- `createScenariosFromPKML(pkmlFilePaths, project, ...)` reads scenarios from PKML files and adds them to a `Project` in place, returning the project invisibly. Output paths are registered in `project$outputPaths` (reusing an existing id when the literal path is already registered, otherwise generating a readable one), scenario names are made unique against the project, and a scenario has no application protocol by default since the PKML embeds its own. Steady-state overwrite of formula-defined parameters is controlled by the `overwriteFormulasInSS` field on a `Scenario` (corresponding to `ignoreIfFormula = FALSE` in `ospsuite::getSteadyState()`), defaulting to `FALSE`. (#908, #1050)
- `importProjectFromExcel(projectConfigPath, outputDir, silent)` reads an Excel-based project and writes a v2.0 `Project.json`; `exportProjectToExcel(project, outputDir, silent)` writes the reverse; `projectStatus(projectConfigPath, jsonPath)` reports synchronisation between the JSON file and its Excel side-cars (returning `excel_in_sync`). The Excel round-trip covers every project section except observed data: observed data is loaded from its own source (a PKML file or an importer configuration), not from an Excel sheet, so it is not written to or read back from the Excel side-cars. Parameter identification is bridged through a `ParameterIdentification.xlsx` workbook with three `taskId`-joined sheets (`PITasks`, with each task's `configuration` flattened to `config.*` columns, `PIParameters`, and `PIOutputMappings`), so a project's PI tasks survive an export then import; a parameter's empty-string `units` reimports as the equivalent unitless (`NULL`) value, since Excel cannot store an empty string distinctly from an empty cell. (#908)
- `exampleProjectPath()` returns the path to the bundled example `Project.json`. (#908)

## Deprecations

Deprecation intensity follows the lifecycle policy: forwarding shims that still work and have a direct replacement are soft-deprecated (a quiet, once-per-session note); shims that will be removed next cycle are deprecated with a warning on every call; hard removals are defunct and abort.

### Soft-deprecated (quiet forwarding shims)

- `createDataCombinedFromExcel()` and `createPlotsFromExcel()` forward to `createDataCombined()` / `createPlots()`; pass a `Project` from `loadProject()`. (#908)
- `exampleProjectConfigurationPath()` forwards to `exampleProjectPath()`. (#908)

### Deprecated with a warning (removed next cycle)

- `createProjectConfiguration()` warns and forwards to `loadProject()`; its default `path` changes from `"ProjectConfiguration.xlsx"` to `"Project.json"`. (#908)
- `ProjectConfiguration()` warns and forwards to `Project$new()`. (#908)
- `projectConfigurationStatus()` warns and forwards to `projectStatus()`. (#908)
- `restoreProjectConfiguration()` warns and forwards to `exportProjectToExcel()`. (#908)
- `snapshotProjectConfiguration()` warns and forwards to `importProjectFromExcel()`. (#908)

## Minor improvements and bug fixes

- `addIndividual()` now rejects a non-numeric `weight` / `height` / `age` with a clear error instead of silently coercing it to `NA` (e.g. `weight = "80kg"`). (#1055)
- `addOutputPath()` now rejects an empty or `NA` path, matching `setOutputPath()`; previously an empty path was accepted and stored. (#1055)
- `addPopulation()` now rejects a non-integer `numberOfIndividuals` (e.g. `2.5`); it must be a positive whole number. (#1055)
- `createDataCombined()` now aborts when a requested `plotGrids` name is not defined in the project, naming the unknown grids, instead of silently dropping it and returning an incomplete or empty result. (#1094)
- `createPlots()` now aborts when a plot grid is missing its `plotGridId`, instead of failing later with an opaque error or misreporting two id-less grids as a duplicate-id violation. (#1094)
- `initProject()` now creates a `definitions/` directory in the project scaffold, matching its documented contract that the scaffold includes a `definitions/` tree. (#1088)
- `loadObservedData()` now aborts when two observed-data sources resolve to the same data-set name (previously the later source silently shadowed the earlier), and a `script` source that returns a list of data sets is keyed by each data set's own name. (#1055)
- `loadScenarioResults()` now restores the full four-field record produced by `runScenarios()`: it reloads the `population` from `<scenario>_population.csv` for population scenarios (previously dropped) and extracts `outputValues` for the simulation's recorded output paths with the population attached, so a reloaded result matches the original run. (#1054)
- `saveScenarioResults()` now reports a failed save with a cli warning that names the affected scenario and carries the underlying error message, instead of a generic base warning, and continues saving the remaining scenarios. It also aborts up front when two scenario names collapse to the same file-safe name (e.g. `"A/B"` and `"A_B"`), which previously overwrote each other silently. (#1054, #1084)
- `runPI(project, tasks = ...)` now canonicalizes the requested task ids, so a task referenced by the name it was first passed to `addPITask()` resolves even though the task was filed under its canonicalized id. (#928)
- `runScenarios()` gains a `stopIfParameterNotFound` argument (default `TRUE`): set it to `FALSE` to skip a `customParams` path that matches no parameter in a scenario's simulation with a warning instead of aborting the run. (#1073)

# esqlabsR 5.6.0

## New features

- `ProjectConfiguration` now stores the `esqlabsR` package version it was last saved with. When loading a configuration, the stored version is compared against the currently installed version. On mismatch or missing version, the user is interactively prompted to update the version in the configuration file and continue, or to stop. The user should always consult the [package NEWS](https://esqlabs.github.io/esqlabsR/news/index.html) for breaking changes before confirming the update.
- Added `ignoreVersionCheck` parameter to `createProjectConfiguration()` and `createDefaultProjectConfiguration()`. When `TRUE`, the version check is skipped. This is intended for non-interactive contexts such as automated tests or scripts run from the console where user input cannot be assured. When using this option, it is the responsibility of the user to ensure that the project is compatible with the currently installed version of `esqlabsR`.

## Minor improvements and bug fixes

- `loadObservedData()` now passes the `sheets` argument directly to `ospsuite::loadDataSetsFromExcel()`, removing the deprecated `importAllSheets` workaround. The `sheets` parameter takes precedence over any sheets defined in `importerConfiguration`: `importerConfiguration$sheets` is always set to `NULL` before loading, so the passed configuration object is mutated as a side effect (#982).
- Refactored `exportParametersToXLS()` to eliminate code duplication by delegating to `writeParameterStructureToXLS()`. The function now extracts parameter data into a structure and passes it to `writeParameterStructureToXLS()` for writing. No changes to functionality or API.
- Added a warning when axis limits contain zero while the corresponding axis scale is set to `log` in `Plots.xlsx`. Previously, this combination silently produced empty plots (\#967).
- `createDataCombinedFromExcel()` now throws an error listing all DataCombined IDs that cannot be found in the Excel file (\#740).
- `extendParameterStructure()` now supports `NULL` for `parameters` and `newParameters` arguments. When `NULL` is provided, a valid empty structure is returned or combined with the non-NULL argument (#583).
- `sensitivityTimeProfiles()` now accepts `xUnits` and `yUnits` as plain strings (e.g., `yUnits = "nmol/l"`) in addition to lists. Single string values are automatically coerced to a list (\#822).
- `snapshotProjectConfiguration()` no longer fails when population files are PK-Sim exported CSVs that do not have sheet names (\#980).
- Remove false warnings whenever a `ProjectConfiguration` is created (\#964).

# esqlabsR 5.5.2

## Breaking changes

- R version >=4.4 is required
- ospsuite.utils version >=1.10.0 is required

## New features

- Added comprehensive three-tier validation system for Excel configuration files. New exported functions:
  - `validateAllConfigurations()`: Validates all project configuration files
  - `validationSummary()`: Returns summary of validation results
  - `isAnyCriticalErrors()`: Checks if validation found blocking errors
- Added validation documentation to project-structure vignette

## Minor improvements and bug fixes

- Fixed variable scoping issues in validation functions
- Simplified validation logic to check data frame structure instead of R6 objects
- Using native operator `%||%` instead of importing from the `ospsuite.utils` package.

# esqlabsR 5.5.1

## Minor improvements and bug fixes

- Improved Excel validation for plot configurations with clearer error messages (\#848). When axis limits (xAxisLimits, yAxisLimits, xValuesLimits, yValuesLimits) are incorrectly formatted (e.g., space-separated "72 80" instead of comma-separated "72, 80"), users now receive specific error messages indicating the field name, plot ID, and correct format. Uses ospsuite.utils validation functions internally.
- Enhanced `createScenarioConfigurationsFromPKML()` with vector argument support - all parameters now support named vectors and vector recycling for flexible scenario creation. (\#890)
- Added support for named vectors in `outputPaths` parameter across scenario functions - names serve as aliases for output paths, e.g., `c("plasma" = "Organism|VenousBlood|Plasma|Drug|Concentration in container")`. (\#890)
- Added Excel append functionality to `exportParametersToXLS()` - new `append` parameter allows adding parameters to existing Excel files without overwriting. (\#890)
- Added Excel sheet name sanitization for application protocols - protocol names are automatically sanitized to comply with Excel naming rules, removing invalid characters and truncating long names. (\#890)
- Improved `.validateCharVector()` to enforce atomic character vectors (\#881).
- Parenthesis in parameter sheet names are ignored, allowing separating sheet names with spaces (\#883).
- Used `cliFormat` function for consistent and maintainable message formatting (\#900).
- Fixed an issue where PK parameter calculations failed when no baseline data (`ParameterFactor == 1`) was available due to simulation error. These cases are now skipped with a warning.

# esqlabsR 5.5.0

## Breaking changes

- Protein ontogenies for individuals and populations are now defined in one column
  `Protein Ontogenies` in the sheets `IndividualBiometrics` (for individuals) or `Demographics`
  (for populations). The columns `Protein` and `Ontogeny` are no longer supported. The new column
  `Protein Ontogenies` is a comma-separated list of protein names and ontogeny names pairs. For example:
  `CYP3A4:CYP3A4,CYP2D6:CYP2C8` will create a CYP3A4 ontogeny for the protein CYP3A4 and a CYP2D6 ontogeny for the protein CYP2C8. (#825)

- The function `createDataCombinedFromExcel()` gets a new signature. The arguments
  `file` and `sheet` are removed. The file from which the `DataCombined` objects
  are created is now passed as part of the `ProjectConfiguration` passed as
  `projectConfiguration` argument, the sheet is always `DataCombined`.

- The function `createDataCombinedFromExcel()` gets a new argument `plotGridNames`.
  The `plotGridNames` argument is a character vector of names of the plots
  specified in the sheet `plotGrids`. The function will then create and return
  `DataCombined` used in the specified plots. The new argument can be combined with
  `dataCombinedNames`.Useful in combination with the new argument `dataCombinedList`
  of the function `createPlotsFromExcel()`.

- Argument `dataCombinedNames = NULL` of the function `createDataCombinedFromExcel()`
  does not create `DataCombined` for all entries in the excel file any more. If
  `dataCombinedNames = NULL`, `plotGridNames` must be specified. If both arguments
  are `NULL`, an empty list is returned.

## Major changes

- User-defined parameters passed to the `createScenarios()` or `Scenario$new()`
  in the `customParams` argument are applied last. Up to this version, they
  were overwritten by the administration protocol (\#817).

- Project Configuration Version Control - Added comprehensive snapshot and restore functionality for project configurations:
  - `snapshotProjectConfiguration()` exports all Excel configuration files to a single JSON file for version control
  - `restoreProjectConfiguration()` recreates Excel files from JSON snapshots for easy project sharing
  - `projectConfigurationStatus()` checks synchronization between Excel files and JSON snapshots
  - Perfect for team collaboration, Git version control, and project backup strategies
  - Comprehensive documentation for version control features is now included in `vignette("project-structure")`.

- `createPlotsFromExcel()` now accepts a (named) list of `DataCombined` objects as input
  to create plots defined in the `plotGridNames` argument. Missing `DataCombined`
  will be created from the Excel file (default behavior).
- New `saveSensitivityCalculation()` and `loadSensitivityCalculation()` functions
  to save and restore sensitivity analysis results (\#862).
- Add `createScenarioConfigurationsFromPKML()` and `addScenarioConfigurationsToExcel()` functions that automate scenario creation and writing to Excel from PKML files (\#853).
- Added species-specific parameter sheets for the species Beagle, Dog, Minipig, and Mouse.
  It is now possible to create scenarios for each species implemented in PK-Sim
  (except for the experimental cat and cattle) from the base human simulation.

## Minor improvements and bug fixes

- `readScenarioConfigurationFromExcel()` ignores rows where `Scenario_name` is empty.
- Fixed a bug when the dimension in the y-axis label of `sensitivityTimeProfiles()`
  did not match the unit (\#823).
- `sensitivityTimeProfiles()` accepts a `DataSet` or a list of `DataSet` objects for `observedData` (\#831).
- `sensitivityTornadoPlot()` accepts a new `xAxisZoomRange` parameter to control the
  visible x-axis range in the plot (\#840).
- When creating a scenario, the name of the scenario is set as the name of the simulation.
  This way, when saving the simulation to PKML and loading in MoBi, the loaded simulation
  will have the updated name.
- Fixed a bug in `createPlotsFromExcel()` when subtitle of PlotConfiguration was
  not applied (\#845).
- Added example usage of `sensitivityTornadoPlot()` to the sensitivity analysis vignette (\#847).
- New vignette on sensitivity analysis plot customization (\#858).
- Corrected x-axis label in `sensitivityTornadoPlot()` to reflect changes in PK parameter (\#861).
- Better error message when `SteadyState = TRUE` and `SteadyStateTime` but not `SteadyStateTimeUnit` is defined in the scenario configuration (\#863).
- `createPlotsFromExcel()` does not fail when `plotGrids` has no `title` column. (\#860)
- The package `ospsuite.utils` is imported but not the direct dependency. (\#836)

# esqlabsR 5.4.0

## Breaking changes

- {esqlabsR}`now requires`{ospsuite.utils}` version \>= 1.7.0.

- The Importer configuration provided with the template project has been
  updated to include `Gender` in the naming pattern. The new naming pattern is as follows:

  `{Study Id}_{Molecule}_{Subject Id}_{Species}_{Gender}_{Organ}_{Compartment}_{Dose}_{Route}_{Group Id}`.

  This change will only affect new projects initailized with the new package version.
  Keep in mind that `Plots.xlsx` files created with the old naming pattern will not work with the new configuration.
  You will have to manually add the `Gender` naming section (if no gender was specified in the
  observed data excel sheet, simply add an additional `_`) after the `Species` section to the
  names of the data sets specified in the `DataCombined` sheet of the `Plots.xlsx` file.

- Snake case functions have been renamed to follow camelCase convention. This includes:
  - `init_project()` function has been renamed to `initProject()`,
  - `esqlabs_colors()` function has been renamed to `esqlabsColors()`.
- The functions `hillFunction()`, `foldChangeFunction()`, and `sineFunction()`
  have been removed.
- The function `escapeForRegex()` has been removed from the package.

## Major changes

- `ProjectConfiguration.xslx` configuration file now support environment variables.
  When creating project configuration using `createProjectConfiguration()` or when
  modifying the projectConfiguration object directly, the package will look for
  matching environment variables and build the paths accordingly. A message is
  shown to the user to make this transparent.
- Complete `sensitivitySpiderPlot` documentation (\#799)

- `parameterPaths` in `sensitivityCalculation()` can now be a named vector.
  The names will be stored and used as custom labels in all relevant plotting functions (\#811).

## Minor improvements and bug fixes

- Improved print outputs for all classes

- Classes do not inherit from the deprecated `Printable` class from the `{ospsuite.utils}` package.

- Print methods for all classes are now implemented using the `ospPrint\*` functions
  introduced in version 1.7.0. of the `{ospsuite.utils}` package.

- Fix when `createPlotsFromExcel` or `createDataCombinedFromExcel` would return
  wrong names of DataCombined for which the output path for a simulation scenario is not defined (\#800).

## Minor improvements and bug fixes

- Fix warnings related to NSE evaluation (\#762)

- Figures defined for the export in the `exportConfiguration` sheet of the `Plots.xlsx`
  file are now exported to the subfolder `Figures\<Current Time Stamp>` of the `Results` folder
  defined in the `ProjectConfiguration` (\#778).

- Fix warning cannot be displayed when no individual model parameters are displayed.

- `sensitivityCalculation()` now supports non-default PK parameters, e.g., user-defined PK-Parameters
  (see https://www.open-systems-pharmacology.org/OSPSuite-R/articles/pk-analysis.html#user-defined-pk-parameters
  for how to create user-defined PK parameters). (\#788)
- Fix `extendPopulationFromXLS()` did not created correct parameter paths (\#769).

# esqlabsR 5.3.0

## Breaking changes

- Function `sensitivityCalculation()` with renamed parameter `pkDataFilePath`
  to `saOutputFilePath` and updated output files to include PK parameter units (\#696).
- Function `sensitivityCalculation()` now accepts `variationRange` as both `absolute`
  and `relative` values, specified by the `variationType` parameter. Added support
  for passing `variationRange` as a list, allowing individual variations for
  each parameter in `parameterPaths` (\#731).

## Major Changes

- `createDefaultProjectConfiguration` is now soft-deprecated in favor of
  `createProjectConfiguration`. (\#692)
- `ProjectConfiguration` fields have been renamed to be more consistant and
  easier to understand. (\#692)
- Files and folders names in project directory have been harmonized and
  improved (\#692)
  - `Parameters` folder is now `Configurations`
  - `Populations` folder is now `PopulationsCSV`
  - `ApplicationParameters.xslx` file is now `Applications.xlsx`
  - `PopulationParameters.xslx` file is now `Populations.xlsx`
  - Previous directory structure is still supported but deprecated.
- `{ospsuite.parameteridentification}` is not longer a dependency of
  `{esqlabsR}`.

## Minor improvements and bug fixes

- When an application protocol defined for a scenario is not found in the excel
  file, an error is thrown.

- Handle simulation failures in `sensitivityCalculation` (\#694).

- Function `sensitivityCalculation()` supports new parameter
  `customOutputFunctions`, allowing user-defined functions for PK parameter
  calculation (\#685).

- Function `sensitivitySpiderPlot` now supports `xAxisType` and `yAxisType`
  options for switching between percent and absolute values (\#695).
- Function `sensitivityCalculation()` will throw more informative messages when
  `outputPaths` or `parameterPaths` is not a character vector or NULL (\#712).
- Minor improvements and bug fixes (\#720).
- Embedded Shiny Application were removed. (\#736). `Function Visualizer` can
  be found [in its dedicated
  repository](https://github.com/esqLABS/esqlabsR.FunctionVisualizer) while
  `Unit Converter` is available as a RStudio Addin
  [here](https://github.com/Open-Systems-Pharmacology/OSPSuite.Addins).

- Test ospsuite plotting functions are compatible with `esqlabsPlotConfiguration`
  (\#733).

# esqlabsR 5.2.0

## Breaking changes

- `{esqlabsR}` now requires `{ospsuite}` version 12.1.0 or higher to benefit
  from embedded PK-Sim core files.

- Function `exportSteadyStateToXLS` has been removed in favor of
  `ospsuite::exportSteadyStateToXLS` (\#598)

- Function `sensitivitySpiderPlot` overhauled with new parameters. `yAxisType`
  to toggle between `percent`and `absolute` values. `xAxisScale` and
  `yAxisScale` to set axis scales to `log` or `lin`. `yAxisFacetScales` to
  choose between `free` and `fixed` scales for y-axis. Further plot
  customization options through `defaultPlotConfiguration` (\#632).

- Function `sensitivityTimeProfiles` with new signature and enhanced visuals:
  - `xAxisScale` and `yAxisScale` to set axis scales to `log` or `lin`
    (\#669).
  - `xUnits` and `yUnits` to change units in the plot (\#688).
  - Enhanced plot customization options through `defaultPlotConfiguration`
    as described in the documentation (\#669).
  - Support for adding `observedData` (single or multiple `DataSet` objects)
    to the plots (\#674).
- Function `writeExcel()` is no longer exported. (\#672)

## Major Changes

- New `sensitivityTornadoPlot` function for generating tornado plots that
  evaluate the impact of parameter changes on sensitivity analysis outcomes.
  It utilizes `parameterFactor` to depict the scaling effects of parameter
  modifications. Provides plot customization options through
  `defaultPlotConfiguration` (\#652).

- New function `writeParameterStructureToXLS` to write a list of parameter
  paths, values, and units (e.g., imported using the `readParametersFromXLS()`
  function) to an Excel file. In contrast to `exportParametersToXLS()`, which
  writes an excel file for a list of `Parameter` objects, this function
  expects the parameter structure as used throughout the package.

## Minor improvements and bug fixes

- Function `createScenarios` gets a new argument `stopIfParameterNotFound`. If
  `TRUE` (default), a scenario will not be created and an error is thrown if
  any user-defined parameter (e.g., provided in Excel files) is not found in
  the simulation. If `FALSE`, non-existing parameters are ignored. (\#559)

- Constructor of a `Scenario` class gets a new argument
  `stopIfParameterNotFound`.

- More explicit error is shown if x/yOffsetsUnit is not set when x/yOffset is
  defined.
- Plotting functions have improved dynamic axis breaks, limits calculation,
  and labeling (\#691).

# esqlabsR 5.1.3

## Minor improvements and bug fixes

- Continuous Integration/Continuous Deployment pipeline improvements (\#590,
  \#592, \#601)
- Several bug fixes (\#581, \#585, \#594, \#600)

# esqlabsR 5.1.2

## Minor improvements and bug fixes

- Fixes a bug where scenario results could not be saved or loaded when
  scenario name contains a slash or a backslash (\#548)
- Add support for `nsd` argument from `ospsuite::plotPopulationTimeProfile()`
  aggregation methods. A new column `nsd` was added to the `PlotConfiguration`
  sheet in the example `Plot.xlsx` (\#544).
- Documentation includes PK-Sim installation instructions (\#537).
- Better error message if some ids are not unique in the excel configuration
  files (\#568)
- Handles better empty rows in the excel configuration files (\#569)
- Cleaner NEWS file (\#527).

# esqlabsR 5.1.1

- Update required dependencies versions

# esqlabsR 5.1.0

## Breaking Changes

- When importing observed data using the default importer configuration, data
  set naming is grouped by `StudyId` at the first place.
  - Before: `{Molecule}_{Study Id}_{Subject
Id}_{Species}_{Organ}_{Compartment}_{Dose}_{Route}_{Group Id}`
  - After: `{Study Id}_{Molecule}_{Subject
Id}_{Species}_{Organ}_{Compartment}_{Dose}_{Route}_{Group Id}`
    This will result in different data set names, and plots specifying the
    data sets by the old naming will fail. For compatibility, use custom
    importer configuration with the old naming:
  ```
    importerConfiguration <- ospsuite::loadDataImporterConfiguration(
      configurationFilePath = projectConfiguration$dataImporterConfigurationFile
      )
      importerConfiguration$namingPattern <- "{Molecule}_{Study Id}_{Subject Id}_{Species}_{Organ}_{Compartment}_{Dose}_{Route}_{Group Id}"
  ```

## Major changes

- `loadObservedData()` gets a new argument `importerConfiguration`. The user
  can now provide a custom importer configuration for loading the data.
- Plots.xlsx, sheet 'plotConfiguration', now uses `xValuesLimits` and
  `yValuesLimits` to set axis limits of the plots by default. This approach
  filters data outside of the limits. See
  <https://ggplot2.tidyverse.org/reference/coord_cartesian.html#ref-examples>
  for more details. The user can still use `xAxisLimits` and `yAxisLimits`.

- Sheet 'plotConfiguration' in the Excel file 'Plots' gets additional column
  'aggregation'. The value is passed to the function
  `plotPopulationTimeProfile()`. Supported values are listed in
  `ospsuite::DataAggregationMethods`.
  - `arithmetic`: population results are plotted as arithmetic mean +-
    arithmetic standard deviation
  - `geometric`: population results are plotted as geometric mean +-
    geometric standard deviation
  - `quantiles` (default): population results are plotted as quantiles
    defined in the column `quantiles`.

- Protein ontogenies can be defined for populations and individuals. To
  specify ontogenies for proteins in the simulation, list the proteins you
  want to define ontogenies for in the column 'Protein' of files
  'PopulationParameters.xlsx' or 'Individuals.xlsx', separated by a ','.
  Specify the ontogenies available in PK-Sim (see article
  <https://www.open-systems-pharmacology.org/OSPSuite-R/articles/create-individual.html#adding-enzyme-ontogenies>)
  in the column 'Ontogenies'. The number of entries in the both columns must
  be equal.
- Excel file 'PopulationParameters.xlsx' gets additional columns 'Protein' and
  'Ontogeny'.
- Excel file 'Individuals.xlsx' gets additional columns 'Protein' and
  'Ontogeny'.

## Minor improvements and bug fixes

- When a scenario fails, `runScenarios()` does not crash any more, but a
  warning is shown with the name of the failed scenario. The returned
  `outputValues` is `NULL`.
- Throw a warning instead of an error if a path specified in
  `ProjectConfiguration` does not exist. `$outputFolder` existence is not
  checked anymore.
- `stringToNum()` does not show a warning `NAs introduced by coercion` when a
  value cannot be converted to a numeric any more. For such values, `NA` is
  silently returned.
- exportParametersToXLS - ignore parameters with NaN (\#480)
- Show a meaningful error when no time unit is specified for a scenario
  (\#483)

- New vignette/article about figure creation.

- Plots are using new color palette.

- Some modifications to plot configuration files for better plots. (\#456)

# esqlabsR 5.0.0

## Breaking Changes

- Field `setTestParameters` removed from `ScenarioConfiguration`

- Function `initializeScenario()` has been removed.

- Definition of simulation time in the `Scenarios.xlsx` file changed. The new
  expected format is a triplet of values \<StartTime, EndTime, Resolution\>,
  where `Resolution` is the number of simulated points per time unit defined
  in the column `TimeUnit`.
- Field `poinstPerMinute` of `ScenarioConfiguration` has been removed.
- Function `compareSimulationParameters()` has been removed and replaced by
  `compareSimulations()`
- `Scenarios` excel file gets additional columns `SteadyStateTime`,
  `SteadyStateTimeUnit`, `PopulationId`, `OutputPathsIds`.
- `readScenarioConfigurationFromExcel()` has a new signature and requires a
  list of `scenarioNames` and a `ProjectConfiguration`. The output is a named
  list of `ScenarioConfiguration` objects.
- Output paths are not set from global variable `OutputPaths` any more but
  from the respective field of `ScenarioConfgiruation`
- `ProjectConfiguration` does not have field `$outputDevice` any more.

- `ScenarioConfiguration` does not store `SimulationRunOptions` any more.
  Simulation run options must be passed to the `runScenarios()` function.
  Different run options cannot be used within one scenarios run.
- Enum `GraphicsDevices` has been removed.

- Function `initializeSimulation()` does not have arguments
  `simulateSteadyState`, `steadyStateTime` and `simulationRunOptions` any
  more.

## Major changes

- New class `Scenario` that represents a scenario created from a
  `ScenarioConfiguration`.
- `ScenarioConfiguration` gets a new field `outputPaths` which is a list of
  output paths for which the results will be calculated. If `NULL` (default),
  outputs as defined in the simulation are used.
- Paths of model outputs are defined in the excel file `Scenarios.xlsx`. In
  the sheet `OutputPaths`, create an entry for each output. The column
  `OutputPath` is the full path to the output, while `OutputPathId` is an
  identifier that conveniently allows to select the correct output.  
  In the `Scenarios` sheet, enter the IDs of all paths the outputs should be
  generated for, separated by a `,`, e.g. `Aciclovir_PVB, Aciclovir_fat_cell`.
  If no outputs are specified, the outputs as defined in the simulation
  `.pkml` file will be produced.
- `ScenarioConfiguration` gets a new field `populationId`, specifying the id
  of the population as defined in the `PopulationParameters.xlsx` file, sheet
  `Demographics`. If the field is `NULL`, the scenario is simulated as an
  individual simulation, otherwise a population simulation is performed.
- `ScenarioConfiguration` gets a new field `readPopulationFromCSV`. If `FALSE`
  (default), a new population is created from defined population demographics.
  If `TRUE`, a simulation will be imported from a csv sheet located in the
  folder `Parameters/Populations` and named as the `PopulationId`.
- `runScenarios()` supports scenario configurations for population
  simulations.

- Target folder for saving `*.pkml` simulations when
  `runScenarios(scenarioConfigurations, saveSimulationsToPKML = TRUE)` changed
  from `Models/Simulations/<DateSuffix>` to
  `Results/SimulationResults/<DateSuffix>`.
- `sensitivityCalculation()` - fixed bug in wrong calculation of sensitivity
  values. Please be aware that the results produced by earlier versions are
  wrong.
- The workflow for running scenarios changed to:
  - Create a `ProjectConfiguration` with
    `createDefaultProjectConfiguration()`
  - Create `ScenarioConfigurations`, e.g. with
    readScenarioConfigurationFromExcel(scenarioNames,
    projectConfiguration)\`
  - Run scenarios with `runScenarios(scenarioConfigurations)` Alternatively:
  - Create a `ProjectConfiguration` with
    `createDefaultProjectConfiguration()`
  - Create `ScenarioConfigurations`, e.g. with

`readScenarioConfigurationFromExcel(scenarioNames, projectConfiguration)`

- `ProjectConfiguration` gets a new field `plotsFile`. It is the name of the
  excel file with plot definitions and must be located in the `paramsFolder`.
- When defining an individual of other species than human in
  `ScenarioConfiguration` and applying it to a human model, missing
  species-specific parameters are applied and the scaling works properly.
  Supported scalings are: Human to rat, human to monkey, human to rabbit.
- `initializeSimulation()` does not perform steady-state run any more. This is
  done as part of the `runScenarios()` function.
- New function `loadObservedDataFromPKML()` to load data from `*.pkml` located
  in the "PKML" sub-folder of the "Data" folder.
- New function `createScenarios()` to create `Scenario` objects from
  `ScenarioConfiguration` objects.
- Plots can be created by calling the new function `createPlotsFromExcel()`.
  It requires as input parameters `simulatedScenarios` (a list of simulated
  scenarios as returned by `runScenarios()`), `observedData` (a list of
  `DataSet` objects) and a `ProjectConfiguration` object
  `projectConfiguration`.
- New function `createDataCombinedFromExcel()` creates `DataCombined` objects
  as defined in the `Plots.xlsx` file.
- New function `saveScenarioResults()` to save results produced by the
  `runScenarios()` function to csv files and corresponding simulations to pkml
  files.
- New function `loadScenarioResults()` to load results from csv files into a
  structure as produced by the `runScenarios()` function.
- New function `compareSimulation()` to compare two simulations for
  differences.
- `runScenarios()` also returns a `Population` object for population
  simulations.
- `runScenarios()` gets a new argument `savePopulationToCSV`, with default
  value `FALSE`.
- `esqlabsR` extends the `tlf::ExportConfiguration` class to dynamically
  calculate the height of the exported figure from the number of rows and the
  new parameter `heightPerRow`.

## Minor changes and bug fixes

- Function `stringToNum()` gets additional arguments `lloqMode` and `uloqMode`
  that determine how entries of type "\<number" and "\>number" will be
  treated.
- `readScenarioConfigurationFromExcel()` will read all scenarios defined in
  the `Scenarios.xlsx` file if no scenario names are specified (argument
  `scenarioNames = NULL`).
- Function `setApplications()` is deprecated.

- Dark gray frame around legends by default.

# esqlabsR 4.0.0

## Breaking changes

- The package requires R version \>=4.1.

- The package gains new dependencies:
  - [`{ospsuite.parameteridentification}`](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/)
  - [`{tlf}`](https://www.open-systems-pharmacology.org/TLF-Library/).

- Function `getSteadyState()` has been moved to package
  `{ospsuite.parameteridentification}`.

- Function `loadObservedData()` requires a `ProjectConfiguration` instead of a
  `ScenarioConfiguration`.

- `DataMapping`, `DataMappingConfiguration`, `XYData`, `DataConfiguration` and
  associated functions for creating standard figures are moved to
  `esqlabsRLegacy` [package](https://esqlabs.github.io/esqlabsRLegacy/).

## Major changes

- Three new functions to create configuration objects needed for data
  visualization workflows:
  - `createEsqlabsPlotConfiguration()`
  - `createEsqlabsPlotGridConfiguration()`
  - `createEsqlabsExportConfiguration()`

- New function `getAllApplicationParameters()` that returns all parameters of
  applications in a simulation
- New function `exportParametersToXLS()` to write parameter information into
  an excel file that can be loaded in MoBi or R using the
  `readParametersFromXLS()` function.
- New function `writeExcel()` that is a wrapper for creating a directory (if
  not present) and writing to excel file using `writexl::write_xlsx()`.

# esqlabsR 3.0.0

## Major changes

- To carry out and visualize sensitivity analysis:
  - `sensitivityCalculation()`
  - `sensitivitySpiderPlot()`
  - `sensitivityTimeProfiles()`

- Classes and functions for standard esqLABS simulation workflow:
  - `ProjectConfiguration`
  - `ScenarioConfiguration`
  - `createDefaultProjectConfiguration()`
  - `readScenarioConfigurationFromExcel()`
  - `setApplications()`
  - `initializeScenario()`

- Maintenance and bug fixes.

- The package gains a new dependency:
  [`{ospsuite.utils}`](https://www.open-systems-pharmacology.org/OSPSuite.RUtils/).

# esqlabsR 2.0.0

- Maintenance and bug fixes.

# esqlabsR 1.0.0

- Initial release of the package.
