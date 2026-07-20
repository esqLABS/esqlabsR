# Project R6 class ----

# The container path fields that belong to the Excel import/export bridge
# (the `excel` block), as opposed to the live working folders
# (`simulationsFolder`, `dataFolder`, `outputFolder`, `populationsFolder`) that
# the runtime reads
# (the `filePaths` block). A legacy `Project.json` carries both sets in one
# flat `filePaths` block; this fixed mapping is what splits them on read and
# routes each to its own container block on write.
.excelFilePathFields <- c(
  "configurationsFolder",
  "modelParamsFile",
  "individualsFile",
  "populationsFile",
  "scenariosFile",
  "applicationsFile",
  "plotsFile",
  "parameterIdentificationFile",
  "initialConditionsFile"
)

#' @title Project
#' @docType class
#' @description An R6 class representing an esqlabsR project.
#'
#'   Changes you make to a loaded project — with `addScenario()`,
#'   `setIndividual()`, `removeParameterSet()`, and the other add/set/remove
#'   functions — live only in your R session until you write them to the
#'   project files with [saveProject()]. This makes a loaded project a safe
#'   place to experiment: discard unsaved changes with [reloadProject()], or
#'   use [snapshotProject()] / [restoreProject()] to set a save-point and
#'   return to it later.
#'
#'   A `Project` cannot be copied with `$clone()` — because nothing is
#'   written to disk until you save, there is no need for a working copy: the
#'   loaded project itself is one. Reading a definition (for example
#'   `sc <- project$definitions$scenarios[["my_scenario"]]`) hands you an independent
#'   copy, so changing `sc` does not change the project until you re-submit
#'   it with the matching set function. The one exception is observed data
#'   added as a `DataSet` object via [addObservedData()]: the `DataSet` is
#'   shared, so changes to that object are seen by the project too.
#'
#'   The public authoring methods (`project$addScenario(...)`,
#'   `project$addOutputPath(...)`, the whole `add*` / `set*` / `remove*` family)
#'   mirror the exported free functions of the same name; the free function is
#'   the primary entry point, carries the full per-argument documentation, and
#'   forwards to the method. The method arguments are summarised once below
#'   (roxygen2 documents R6 method arguments in the class topic); for the
#'   authoritative, per-function argument descriptions see the linked free
#'   function, e.g. [addScenario()], [setScenario()], [addOutputPath()],
#'   [addParameterEntry()], [addPITask()], [createScenariosFromPKML()].
#'
#' @param id Character id (name) of the definition to add, modify, or remove.
#' @param modelFile Character name of the `.pkml` model file. See
#'   [addScenario()].
#' @param individual,population,application Character id (or `NULL`) of the
#'   individual / population / application a scenario references. See
#'   [addScenario()].
#' @param parameterSets,initialConditions,outputPaths Character vectors (or
#'   `NULL`) of definition ids a scenario references, or the target of an
#'   `add*`/`set*`/`remove*` call. See [addScenario()].
#' @param simulationTime,simulationTimeUnit Simulation time specification and
#'   its unit. See [addScenario()].
#' @param steadyState,steadyStateTime,steadyStateTimeUnit Steady-state flag,
#'   time, and unit. See [addScenario()].
#' @param overwriteFormulasInSS,readPopulationFromCSV Logical scenario options.
#'   See [addScenario()].
#' @param newId Character new id for [renameScenario()] / [duplicateScenario()].
#' @param path Character path: an output-path string ([addOutputPath()]) or a
#'   parameter/initial-condition path ([addParameterEntry()] /
#'   [addInitialConditionEntry()]).
#' @param containerPath,parameterName,value,units,unit Parameter- and
#'   initial-condition entry fields. See [addParameterEntry()] /
#'   [addInitialConditionEntry()].
#' @param entry An observed-data source: a `DataSet` or a configuration list.
#'   See [addObservedData()].
#' @param simulated,observed Simulated / observed inputs to a `DataCombined`.
#'   See [addDataCombined()].
#' @param task Character id of the parameter-identification task a sub-item
#'   belongs to. See [addPIParameter()].
#' @param scenarios,parameters,outputMappings,configuration
#'   Parameter-identification task components. See [addPITask()].
#' @param minValue,maxValue,startValue Numeric bounds and start value of a PI
#'   parameter. See [addPIParameter()].
#' @param outputPath,observedData,scaling,xOffset,yOffset,xFactor,yFactor,weight
#'   Fields of a PI output mapping. See [addPIOutputMapping()].
#' @param pkmlFilePaths Character vector of `.pkml` file paths. See
#'   [createScenariosFromPKML()].
#' @param ... Passed to the matching authoring free function (e.g. the
#'   partial-update fields of [setScenario()] / [setIndividual()], or the
#'   remaining arguments of [createScenariosFromPKML()]).
#' @format NULL
#' @import fs
#' @export
Project <- R6::R6Class(
  "Project",
  cloneable = FALSE,
  active = list(
    #' @field info Project identity and metadata, as a writable field group.
    #'   Read a field with `project$info$name`; write one with
    #'   `project$info$name <- "..."`. Writable fields: `name` (human-readable
    #'   project name), `description` (free-text description). Read-only fields:
    #'   `schemaVersion` (schema version declared in the JSON, always `"2.0"`
    #'   for projects this package loads), `esqlabsRVersion` (informational
    #'   version string from the JSON), `projectFilePath` (absolute path to the
    #'   JSON file the project was loaded from, `NULL` for an in-memory
    #'   project), and `projectDirPath` (the directory containing that file, the
    #'   base for resolving relative paths). Assigning a writable field sets the
    #'   dirty bit; assigning a read-only field aborts. The two version fields
    #'   are managed by the load/save machinery, not by users.
    info = function(value) {
      if (!missing(value)) {
        return(private$.acceptGroupWriteback(value, "info"))
      }
      private$.infoGroup()
    },

    #' @field paths The project's working-folder paths, as a writable field
    #'   group: `simulationsFolder` (pkml simulation files, `Models/Simulations`,
    #'   sitting under `Models/` alongside the `Snapshots` folder for PK-Sim /
    #'   MoBi snapshots), `dataFolder` (experimental data), `outputFolder`
    #'   (results), `populationsFolder` (population CSVs loaded by
    #'   [runScenarios()]), and `definitionsFolder` (the folder holding the
    #'   definition files, default `"definitions"`).
    #'   Read a field with `project$paths$simulationsFolder` (returned resolved
    #'   against `projectDirPath`); write one with
    #'   `project$paths$simulationsFolder <- "Models"` (stored verbatim, resolved
    #'   on the next read). Assigning any field sets the dirty bit. Changing
    #'   `definitionsFolder` redirects where the next [saveProject()] writes the
    #'   definition files; nothing moves on disk until that save. The
    #'   Excel-bridge sheet-name fields live in the separate `excel` group.
    paths = function(value) {
      if (!missing(value)) {
        return(private$.acceptGroupWriteback(value, "paths"))
      }
      private$.pathsGroup()
    },

    #' @field excel The Excel import/export bridge sheet-name fields, as a
    #'   writable field group: `configurationsFolder`, `modelParamsFile`,
    #'   `individualsFile`, `populationsFile`, `scenariosFile`,
    #'   `applicationsFile`, `plotsFile`, `parameterIdentificationFile`,
    #'   `initialConditionsFile`. Read a field with
    #'   `project$excel$modelParamsFile` (returned resolved against
    #'   `configurationsFolder`, itself resolved against `projectDirPath`);
    #'   write one with `project$excel$modelParamsFile <- "P.xlsx"` (stored
    #'   verbatim). Assigning any field sets the dirty bit. Empty for a project
    #'   created directly in the JSON format, without Excel files.
    excel = function(value) {
      if (!missing(value)) {
        return(private$.acceptGroupWriteback(value, "excel"))
      }
      private$.excelGroup()
    },

    #' @field defaultSimulationRunOptions Named list of the project-level
    #'   default simulation run options (the `defaultSimulationRunOptions` JSON
    #'   field), or `NULL` when none are declared. Used by [runScenarios()] as
    #'   the default `simulationRunOptions` when the caller does not pass one.
    #'   Recognized fields: `numberOfCores`, `checkForNegativeValues`,
    #'   `showProgress`.
    defaultSimulationRunOptions = function(value) {
      if (!missing(value)) {
        private$.defaultSimulationRunOptions <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.defaultSimulationRunOptions
    },

    #' @field definitions The project's definition sections, as a read-only
    #'   field group. Each section is one field: `outputPaths`, `scenarios`,
    #'   `parameterSets`, `initialConditions`, `individuals`, `populations`,
    #'   `applications`, `observedData`, `dataCombined`, `plots`, `plotGrids`,
    #'   `parameterIdentification`. Read a section with
    #'   `project$definitions$scenarios` (returned as a printable, read-only
    #'   named list keyed by id). The group is read-only from the handle: every
    #'   assignment form aborts. The only sanctioned way to change a section is
    #'   an authoring function ([addScenario()] / [setScenario()] /
    #'   [removeScenario()] and their per-section siblings) or editing the
    #'   definition's JSON file; those route through the internal write seam,
    #'   which updates the in-memory backing field, sets the dirty bit, and
    #'   clears the validation-cache flag so the next run/plot re-validates.
    #'   Nothing touches the `definitions/<kind>/` tree until [saveProject()]
    #'   reconciles it to memory.
    definitions = function(value) {
      if (!missing(value)) {
        return(private$.acceptGroupWriteback(value, "definitions"))
      }
      private$.definitionsGroup()
    },

    #' @field asList Returns the current project as a list matching the JSON
    #'   schema. Reflects any in-memory modifications. Read-only.
    asList = function(value) {
      if (!missing(value)) {
        cli::cli_abort("{.field asList} is readonly")
      }
      .projectToJson(self)
    },

    #' @field status Read-only sync report as a structured list:
    #'   `tree_in_sync` (`FALSE` when the project carries unsaved changes,
    #'   `NA` for a project that exists only in the R session, without a
    #'   folder on disk), `excel_in_sync` (`TRUE`/`FALSE`, or `NA` when the
    #'   project has no Excel file or it cannot be read), and `details` (the
    #'   differences, empty when everything is in sync). The same information
    #'   [projectStatus()] prints. Read-only; assignment aborts.
    status = function(value) {
      if (!missing(value)) {
        cli::cli_abort("{.field status} is readonly")
      }
      .projectSyncStatus(self, silent = TRUE)
    }
  ),
  public = list(
    #' @description Construct a `Project` from a JSON file path, or create an
    #'   empty in-memory project when called with no arguments.
    #'
    #' @param projectFilePath A string representing the path to the project
    #'   JSON file.
    initialize = function(projectFilePath = character()) {
      private$.validatedSinceMutation <- FALSE
      if (is.character(projectFilePath) && length(projectFilePath) == 0L) {
        private$.projectDirPath <- NULL
        return(invisible(self))
      }
      if (
        !is.character(projectFilePath) ||
          length(projectFilePath) != 1L ||
          is.na(projectFilePath) ||
          !nzchar(projectFilePath)
      ) {
        cli::cli_abort(messages$invalidPathArgument())
      }
      private$.read_json(projectFilePath)
      invisible(self)
    },

    # Public authoring methods ----
    #
    # One method per exported authoring free function. Each is a thin forwarder
    # to an `_impl()` free function in the relevant domain file, handing that
    # impl its own `self` / `private` so the logic can touch the private state
    # seam directly (the `_impl` functions are not attached to the object, so a
    # user cannot call them or reach `private` through them). The exported free
    # functions (`addScenario()` etc.) forward here and remain the primary,
    # documented entry point. Each method's parameters are documented once, on
    # that free function, and pulled in here with `@inheritParams` so no
    # `@param` block is ever duplicated between a method and its free function.

    #' @description Add scenarios. See [addScenario()], the primary entry point.
    addScenario = function(
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
      readPopulationFromCSV = FALSE
    ) {
      .addScenario_impl(
        self,
        private,
        id,
        modelFile,
        individual,
        population,
        application,
        parameterSets,
        initialConditions,
        outputPaths,
        simulationTime,
        simulationTimeUnit,
        steadyState,
        steadyStateTime,
        steadyStateTimeUnit,
        overwriteFormulasInSS,
        readPopulationFromCSV
      )
    },

    #' @description Remove scenarios. See [removeScenario()].
    removeScenario = function(id) {
      .removeScenario_impl(self, private, id)
    },

    #' @description Modify fields of existing scenarios. See [setScenario()].
    #'   The `...` carries only the fields to change (partial update); a field
    #'   passed as `NULL` is cleared, an omitted field is left untouched.
    setScenario = function(id, ...) {
      .setScenario_impl(self, private, id, ...)
    },

    #' @description Rename a scenario. See [renameScenario()].
    renameScenario = function(id, newId) {
      .renameScenario_impl(self, private, id, newId)
    },

    #' @description Duplicate a scenario. See [duplicateScenario()].
    duplicateScenario = function(id, newId) {
      .duplicateScenario_impl(self, private, id, newId)
    },

    #' @description Create scenarios from PKML model files. See
    #'   [createScenariosFromPKML()].
    createScenariosFromPKML = function(pkmlFilePaths, ...) {
      .createScenariosFromPKML_impl(self, private, pkmlFilePaths, ...)
    },

    #' @description Add an individual. See [addIndividual()].
    addIndividual = function(...) {
      .addIndividual_impl(self, private, ...)
    },

    #' @description Remove individuals. See [removeIndividual()].
    removeIndividual = function(id) {
      .removeIndividual_impl(self, private, id)
    },

    #' @description Modify an existing individual. See [setIndividual()].
    setIndividual = function(...) {
      .setIndividual_impl(self, private, ...)
    },

    #' @description Add a population. See [addPopulation()].
    addPopulation = function(...) {
      .addPopulation_impl(self, private, ...)
    },

    #' @description Remove populations. See [removePopulation()].
    removePopulation = function(id) {
      .removePopulation_impl(self, private, id)
    },

    #' @description Modify an existing population. See [setPopulation()].
    setPopulation = function(...) {
      .setPopulation_impl(self, private, ...)
    },

    #' @description Add an application. See [addApplication()].
    addApplication = function(id, parameterSets = NULL) {
      .addApplication_impl(self, private, id, parameterSets)
    },

    #' @description Remove applications. See [removeApplication()].
    removeApplication = function(id) {
      .removeApplication_impl(self, private, id)
    },

    #' @description Set an application's parameter sets. See
    #'   [setApplicationParameterSets()].
    setApplicationParameterSets = function(id, parameterSets) {
      .setApplicationParameterSets_impl(self, private, id, parameterSets)
    },

    #' @description Add an output path. See [addOutputPath()].
    addOutputPath = function(id, path) {
      .addOutputPath_impl(self, private, id, path)
    },

    #' @description Remove output paths. See [removeOutputPath()].
    removeOutputPath = function(id) {
      .removeOutputPath_impl(self, private, id)
    },

    #' @description Modify an existing output path. See [setOutputPath()].
    setOutputPath = function(id, path) {
      .setOutputPath_impl(self, private, id, path)
    },

    #' @description Add a parameter set. See [addParameterSet()].
    addParameterSet = function(id) {
      .addParameterSet_impl(self, private, id)
    },

    #' @description Remove parameter sets. See [removeParameterSet()].
    removeParameterSet = function(id) {
      .removeParameterSet_impl(self, private, id)
    },

    #' @description Add an entry to a parameter set. See [addParameterEntry()].
    addParameterEntry = function(
      id,
      containerPath,
      parameterName,
      value,
      units
    ) {
      .addParameterEntry_impl(
        self,
        private,
        id,
        containerPath,
        parameterName,
        value,
        units
      )
    },

    #' @description Remove an entry from a parameter set. See
    #'   [removeParameterEntry()].
    removeParameterEntry = function(id, containerPath, parameterName) {
      .removeParameterEntry_impl(
        self,
        private,
        id,
        containerPath,
        parameterName
      )
    },

    #' @description Add an initial-conditions set. See [addInitialConditions()].
    addInitialConditions = function(id) {
      .addInitialConditions_impl(self, private, id)
    },

    #' @description Remove initial-conditions sets. See
    #'   [removeInitialConditions()].
    removeInitialConditions = function(id) {
      .removeInitialConditions_impl(self, private, id)
    },

    #' @description Add an entry to an initial-conditions set. See
    #'   [addInitialConditionEntry()].
    addInitialConditionEntry = function(id, path, value, unit) {
      .addInitialConditionEntry_impl(self, private, id, path, value, unit)
    },

    #' @description Remove an entry from an initial-conditions set. See
    #'   [removeInitialConditionEntry()].
    removeInitialConditionEntry = function(id, path) {
      .removeInitialConditionEntry_impl(self, private, id, path)
    },

    #' @description Add a plot. See [addPlot()].
    addPlot = function(...) {
      .addPlot_impl(self, private, ...)
    },

    #' @description Remove plots. See [removePlot()].
    removePlot = function(id) {
      .removePlot_impl(self, private, id)
    },

    #' @description Add a plot grid. See [addPlotGrid()].
    addPlotGrid = function(...) {
      .addPlotGrid_impl(self, private, ...)
    },

    #' @description Remove plot grids. See [removePlotGrid()].
    removePlotGrid = function(id) {
      .removePlotGrid_impl(self, private, id)
    },

    #' @description Add a data-combined entry. See [addDataCombined()].
    addDataCombined = function(id, simulated = list(), observed = list()) {
      .addDataCombined_impl(self, private, id, simulated, observed)
    },

    #' @description Remove data-combined entries. See [removeDataCombined()].
    removeDataCombined = function(id) {
      .removeDataCombined_impl(self, private, id)
    },

    #' @description Add observed data. See [addObservedData()].
    addObservedData = function(entry) {
      .addObservedData_impl(self, private, entry)
    },

    #' @description Remove observed data. See [removeObservedData()].
    removeObservedData = function(id) {
      .removeObservedData_impl(self, private, id)
    },

    #' @description Load the project's observed data. See [loadObservedData()].
    loadObservedData = function() {
      .loadObservedData_impl(self, private)
    },

    #' @description Names of the project's observed data. See
    #'   [getObservedDataNames()].
    getObservedDataNames = function() {
      .getObservedDataNames_impl(self, private)
    },

    #' @description Add a parameter-identification task. See [addPITask()].
    addPITask = function(
      id,
      scenarios,
      parameters,
      outputMappings,
      configuration = list()
    ) {
      .addPITask_impl(
        self,
        private,
        id,
        scenarios,
        parameters,
        outputMappings,
        configuration
      )
    },

    #' @description Remove parameter-identification tasks. See [removePITask()].
    removePITask = function(id) {
      .removePITask_impl(self, private, id)
    },

    #' @description Add a parameter to a PI task. See [addPIParameter()].
    addPIParameter = function(
      task,
      path,
      scenarios,
      minValue,
      maxValue,
      startValue,
      units = NULL,
      id = NULL
    ) {
      .addPIParameter_impl(
        self,
        private,
        task,
        path,
        scenarios,
        minValue,
        maxValue,
        startValue,
        units,
        id
      )
    },

    #' @description Remove a parameter from a PI task. See [removePIParameter()].
    removePIParameter = function(task, id) {
      .removePIParameter_impl(self, private, task, id)
    },

    #' @description Add an output mapping to a PI task. See
    #'   [addPIOutputMapping()].
    addPIOutputMapping = function(
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
      id = NULL
    ) {
      .addPIOutputMapping_impl(
        self,
        private,
        task,
        outputPath,
        observedData,
        scenarios,
        scaling,
        xOffset,
        yOffset,
        xFactor,
        yFactor,
        weight,
        id
      )
    },

    #' @description Remove an output mapping from a PI task. See
    #'   [removePIOutputMapping()].
    removePIOutputMapping = function(task, id) {
      .removePIOutputMapping_impl(self, private, task, id)
    },

    #' @description Save the project's in-memory edits to its on-disk tree. See
    #'   [saveProject()].
    save = function() {
      .saveProject_impl(self, private)
    },

    #' @description Discard in-memory edits and re-read from disk. See
    #'   [reloadProject()].
    reload = function() {
      .reloadProject_impl(self, private)
    },

    #' @description Validate the project. See [validateProject()].
    validate = function() {
      .validateProject_impl(self, private)
    },

    #' @description Package-internal pre-op validation gate. Runs targeted
    #'   validation for the `sections` an operation depends on and aborts with a
    #'   formatted multi-error message on any critical errors, short-circuiting
    #'   when the project is already validated. Called by the run/plot/parameter-
    #'   identification entry points; not intended for end users.
    #' @param sections Non-empty character vector of section names the calling
    #'   operation requires.
    #' @param opName Short label used in the abort message (e.g.
    #'   `"runScenarios"`).
    #' @keywords internal
    ensureValid = function(sections, opName) {
      private$.ensureValid(sections, opName)
    },

    #' @description Package-internal reader for the raw `filePaths` block: a
    #'   named list of `list(value, description)` records for the four live
    #'   working folders. Unlike `project$paths$...` (which returns resolved
    #'   values), this keeps the per-folder descriptions the Excel bridge
    #'   round-trips. Consumed by the JSON writer and the Excel exporter; not
    #'   intended for end users.
    #' @keywords internal
    rawFilePaths = function() {
      private$.filePathsData
    },

    #' @description Package-internal reader for the raw `excel` block: a named
    #'   list of `list(value, description)` records for the Excel-bridge
    #'   sheet-name fields. Unlike `project$excel$...` (which returns resolved
    #'   values), this keeps the descriptions. Empty when the project has no
    #'   Excel side-car. Consumed by the JSON writer and the Excel exporter; not
    #'   intended for end users.
    #' @keywords internal
    rawExcel = function() {
      private$.excelData
    },

    #' @description Package-internal reader for the in-memory dirty bit: `TRUE`
    #'   when the project carries edits not yet reconciled to the on-disk
    #'   `definitions/` tree. The same signal `projectStatus()` reports on its
    #'   memory-vs-tree axis. Not intended for end users; users read the sync
    #'   state through `project$status` or [projectStatus()].
    #' @keywords internal
    isModified = function() {
      private$.isModified()
    },

    #' @description Print a summary of the Project. Each section is rendered by
    #'   the same per-group block method the field groups (`project$info`,
    #'   `project$paths`, `project$definitions`, `project$excel`) print, so the
    #'   project summary and a group's own print never drift.
    #' @param ... Unused; present for S3 method consistency.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)

      # Passive dirty-state cue: mark the class line when there are unsaved
      # in-memory edits. `ospPrintClass()` prints the `<Project 'name'>` line
      # itself and takes no suffix, so the marker is printed immediately after
      # so the rendered first line reads `<Project 'name'> [unsaved changes]`.
      if (isTRUE(private$.modified)) {
        cli::cli_text("{.emph [unsaved changes]}")
      }

      private$.printInfoBlock()
      private$.printPathsBlock()
      private$.printDefinitionsBlock()
      private$.printExcelBlock()

      invisible(self)
    }
  ),
  private = list(
    .projectFilePath = NULL,
    .projectDirPath = NULL,
    .validatedSinceMutation = FALSE,
    # In-memory dirty bit: TRUE when the in-memory project has edits that have
    # not yet been reconciled to the on-disk `definitions/` tree. Set by
    # `.setSection()`, the container-metadata setters, and `.markModified()`;
    # cleared on load / reload / a successful `saveProject()`. It feeds the
    # `print()` marker, the clean-save short-circuit, and the memory-vs-tree
    # axis of `projectStatus()`. A private field with no active binding keeps it
    # off the public surface. A freshly constructed in-memory project starts
    # clean (FALSE).
    .modified = FALSE,
    # Working-folder paths (the `filePaths` block): the four live folders the
    # runtime reads (`simulationsFolder`, `dataFolder`, `outputFolder`,
    # `populationsFolder`).
    .filePathsData = list(),
    # Excel import/export bridge sheet names (the `excel` block): the
    # vestigial fields only the Excel bridge reads. Empty for a from-scratch
    # JSON project with no Excel side-car, in which case no `excel` block is
    # written to `Project.json`.
    .excelData = list(),
    .programmaticDataSets = list(),
    .observedDataNamesCache = NULL,

    # Backing stores for the container metadata + the section-data active
    # bindings. The parser writes these directly so loading does not invalidate
    # the validation cache.
    .schemaVersion = NULL,
    .esqlabsRVersion = NULL,
    .name = NULL,
    .description = NULL,
    .definitionsFolder = NULL,
    .defaultSimulationRunOptions = NULL,
    .outputPaths = NULL,
    .scenarios = NULL,
    .parameterSets = NULL,
    .initialConditions = NULL,
    .individuals = NULL,
    .populations = NULL,
    .applications = NULL,
    .observedData = NULL,
    .dataCombined = NULL,
    .plots = NULL,
    .plotGrids = NULL,
    .parameterIdentification = NULL,

    # Resolve a section kind to its private backing-field name (`.<kind>`),
    # aborting on an unknown kind. Each section maps one-to-one onto a private
    # backing field named `.<kind>` and onto its `definitions/<kind>/` tree. The
    # set of valid kinds is the single source of truth `.definitionKindNames()`
    # (derived from the definition-tree specs), so a typo cannot silently create a
    # stray `private$.<typo>` field and the kind list is not duplicated here.
    .sectionField = function(kind) {
      if (
        !is.character(kind) ||
          length(kind) != 1L ||
          !(kind %in% .definitionKindNames())
      ) {
        cli::cli_abort("Unknown project section {.val {kind}}.")
      }
      paste0(".", kind)
    },

    # Invalidate the validation cache. Any mutation clears the flag so the next
    # `runScenarios()` / `createPlots()` re-validates the new shape.
    .invalidate = function() {
      private$.validatedSinceMutation <- FALSE
      invisible(self)
    },

    # Container-metadata edit (name, description, definitionsFolder, folders,
    # ...): invalidate the validation cache and set the in-memory dirty bit.
    # It does not touch disk; the metadata is persisted only when
    # `saveProject()` reconciles the container `Project.json` to memory.
    .invalidateContainer = function() {
      private$.invalidate()
      private$.modified <- TRUE
      invisible(self)
    },

    # Field-group proxy builders ----
    #
    # Each builds a `.projectFieldGroup()` proxy for one surface group
    # (`project$info`, `project$paths`, `project$excel`, `project$definitions`).
    # The proxy's per-field getter/setter closures are created here, inside a
    # method, so they capture the live `self`/`private` of this instance and can
    # read and write the backing state directly. External code holds only the
    # proxy, never `private`; the closures are the only bridge back. A fresh
    # proxy is built on each group access (the proxy carries no state beyond its
    # closures, and the closures re-read the live backing fields every call).

    # Handle an assignment into a group active binding (`project$info <- ...`).
    # A nested field write (`project$info$name <- "x"`) mutates the proxy
    # environment in place through the field's active binding, then R re-assigns
    # the (same) proxy back into the group binding; that write-back must be
    # accepted as a no-op. Any other value is a genuine attempt to replace the
    # whole group, which is not allowed: the group is a live view, not a slot.
    .acceptGroupWriteback = function(value, group) {
      # A genuine write-back is *this* instance's own proxy for the same group:
      # the `owner` attribute must be identical to this instance's `private`
      # (an environment, compared by reference). A proxy from another `Project`
      # is not a write-back and must be rejected, not silently swallowed.
      isWriteback <- inherits(value, "ProjectFieldGroup") &&
        identical(attr(value, "group"), group) &&
        identical(attr(value, "owner"), private)
      if (!isWriteback) {
        # `call = NULL`: the abort fires from inside the group active-binding
        # setter, whose frame is this internal helper, not a user function.
        cli::cli_abort(
          c(
            "{.field {group}} is a field group and cannot be replaced.",
            "i" = "Assign an individual field instead, e.g. \\
            {.code project${group}$<field> <- value}."
          ),
          call = NULL
        )
      }
      invisible(value)
    },

    # `project$info`: identity + metadata. `name`/`description` are writable
    # (each write sets the dirty bit via `.invalidateContainer()`);
    # `schemaVersion`/`esqlabsRVersion` are machine-managed and
    # `projectFilePath`/`projectDirPath` are derived, so all four are read-only.
    .infoGroup = function() {
      .projectFieldGroup(
        list(
          name = list(
            get = function() private$.name,
            set = function(value) {
              private$.name <- value
              private$.invalidateContainer()
            }
          ),
          description = list(
            get = function() private$.description,
            set = function(value) {
              private$.description <- value
              private$.invalidateContainer()
            }
          ),
          schemaVersion = list(
            get = function() private$.schemaVersion,
            set = NULL
          ),
          esqlabsRVersion = list(
            get = function() private$.esqlabsRVersion,
            set = NULL
          ),
          projectFilePath = list(
            get = function() private$.projectFilePath,
            set = NULL
          ),
          projectDirPath = list(
            get = function() private$.projectDirPath,
            set = NULL
          )
        ),
        group = "info",
        printer = function() private$.printInfoBlock(),
        owner = private
      )
    },

    # Build one writable get/set spec for a `{value, description}` record field
    # stored under `private[[store]][[name]]`. The getter resolves the raw value
    # against `parentFn()` (a resolver returning the base path) via
    # `.clean_path()`, so a folder/file field reads resolved and writes verbatim;
    # the setter stores the raw value and invalidates the container. Shared by
    # the `paths` and `excel` groups, which differ only in `store` and `parentFn`.
    .recordFieldSpec = function(store, name, parentFn) {
      force(store)
      force(name)
      force(parentFn)
      list(
        get = function() {
          private$.clean_path(private[[store]][[name]]$value, parentFn())
        },
        set = function(value) {
          private[[store]][[name]]$value <- value
          private$.invalidateContainer()
        }
      )
    },

    # `project$paths`: the working-folder paths. Each folder is writable;
    # reading resolves the stored value against `projectDirPath` (raw on write,
    # resolved on read), exactly as the former flat getters did. Every write
    # sets the dirty bit. `definitionsFolder` is stored separately (it is not a
    # `filePaths` record) and defaults to `"definitions"`.
    .pathsGroup = function() {
      # Working folders resolve through `.resolveWorkingFolder()` (not the
      # plain `.recordFieldSpec` resolver) so a folder value read from an
      # untrusted `Project.json` is contained under the project directory,
      # unless it opts out with an explicit `${VAR}`. The setter is the same
      # raw-store-and-invalidate as any record field.
      folderField <- function(name) {
        force(name)
        list(
          get = function() private$.resolveWorkingFolder(name),
          set = function(value) {
            private$.filePathsData[[name]]$value <- value
            private$.invalidateContainer()
          }
        )
      }
      .projectFieldGroup(
        list(
          simulationsFolder = folderField("simulationsFolder"),
          dataFolder = folderField("dataFolder"),
          outputFolder = folderField("outputFolder"),
          populationsFolder = folderField("populationsFolder"),
          definitionsFolder = list(
            get = function() private$.definitionsFolder %||% "definitions",
            set = function(value) {
              private$.definitionsFolder <- value
              private$.invalidateContainer()
            }
          )
        ),
        group = "paths",
        printer = function() private$.printPathsBlock(),
        owner = private
      )
    },

    # `project$excel`: the Excel-bridge sheet-name fields. `configurationsFolder`
    # resolves against `projectDirPath`; every other field resolves against the
    # (resolved) `configurationsFolder`, as the former flat getters did. All are
    # writable (raw on write, resolved on read); every write sets the dirty bit.
    .excelGroup = function() {
      configResolved <- function() {
        private$.clean_path(
          private$.excelData$configurationsFolder$value,
          private$.projectDirPath
        )
      }
      fileField <- function(name) {
        private$.recordFieldSpec(".excelData", name, configResolved)
      }
      .projectFieldGroup(
        list(
          configurationsFolder = list(
            get = configResolved,
            set = function(value) {
              private$.excelData$configurationsFolder$value <- value
              private$.invalidateContainer()
            }
          ),
          modelParamsFile = fileField("modelParamsFile"),
          individualsFile = fileField("individualsFile"),
          populationsFile = fileField("populationsFile"),
          scenariosFile = fileField("scenariosFile"),
          applicationsFile = fileField("applicationsFile"),
          plotsFile = fileField("plotsFile"),
          parameterIdentificationFile = fileField(
            "parameterIdentificationFile"
          ),
          initialConditionsFile = fileField("initialConditionsFile")
        ),
        group = "excel",
        printer = function() private$.printExcelBlock(),
        owner = private
      )
    },

    # `project$definitions`: the definition sections. Read-only from the handle;
    # each field returns the section wrapped in a printable, read-only
    # `DefinitionList` (so `project$definitions$scenarios[["id"]]` reads and any
    # nested assignment still aborts). The field set is the single source of
    # truth `.definitionKindNames()`, so the group always matches the actual
    # sections. Assignment routes through `.definitionListReadOnlyError()`, whose
    # message points at the authoring functions.
    .definitionsGroup = function() {
      kinds <- .definitionKindNames()
      spec <- lapply(kinds, function(kind) {
        force(kind)
        list(
          get = function() {
            .asDefinitionList(private$.getSection(kind), kind)
          },
          set = NULL
        )
      })
      names(spec) <- kinds
      .projectFieldGroup(
        spec,
        group = "definitions",
        onReadOnly = function(field) {
          # The abort fires from inside an active-binding setter, whose call
          # frame is the internal accessor closure; `call = NULL` keeps that
          # internal frame out of the message (the `DefinitionList` `[[<-` /
          # `$<-` methods, which have a real user frame, keep the default).
          .definitionListReadOnlyError(field, call = NULL)
        },
        printer = function() private$.printDefinitionsBlock(),
        owner = private
      )
    },

    # Per-group print blocks ----
    #
    # One method per surface group. Each renders that group's section exactly as
    # it appears in `Project$print()`. Both the whole-project print and the
    # group's own `print.ProjectFieldGroup` call these, so the rendering is
    # defined once and the two prints never drift.

    # `project$info`: the metadata bullets. `print_empty = FALSE` drops the
    # NULL/empty entries (e.g. the JSON file for an in-memory project). The JSON
    # file is shown as its basename, not the machine-specific absolute path.
    .printInfoBlock = function() {
      ospsuite.utils::ospPrintItems(
        list(
          "Name" = private$.name,
          "Description" = private$.description,
          "Schema Version" = private$.schemaVersion,
          "esqlabsR Version" = private$.esqlabsRVersion,
          "JSON File" = if (!is.null(private$.projectFilePath)) {
            fs::path_file(private$.projectFilePath)
          }
        )
      )
      invisible(self)
    },

    # `project$paths`: the working folders, shown relative to the project
    # directory. The absolute prefix is machine-specific (and, for a project
    # loaded from a temp copy, varies in length by OS), so printing it is both
    # noisy and non-reproducible; the resolved value is kept for an in-memory
    # project that has no directory to relativize against. `configurationsFolder`
    # and the workbook file fields belong to the Excel block, not here. Unset
    # (NULL) folders are dropped and the header is omitted when none is set.
    .printPathsBlock = function() {
      relToProject <- function(path) {
        dir <- private$.projectDirPath
        if (is.null(path) || is.null(dir)) {
          return(path)
        }
        as.character(fs::path_rel(path, start = dir))
      }
      paths <- self$paths
      items <- Filter(
        Negate(is.null),
        list(
          "Simulations Folder" = relToProject(paths$simulationsFolder),
          "Data Folder" = relToProject(paths$dataFolder),
          "Populations Folder" = relToProject(paths$populationsFolder),
          "Output Folder" = relToProject(paths$outputFolder),
          "Definitions Folder" = paths$definitionsFolder
        )
      )
      if (length(items) > 0L) {
        ospsuite.utils::ospPrintHeader("Paths")
        ospsuite.utils::ospPrintItems(items)
      }
      invisible(self)
    },

    # `project$definitions`: the per-section entry counts. `ospPrintItems()`
    # prints an integer `0`, so zero-count sections are dropped explicitly (not
    # via `print_empty`). The header is omitted when every section is empty.
    .printDefinitionsBlock = function() {
      counts <- list(
        "Scenarios" = length(private$.scenarios),
        "Individuals" = length(private$.individuals),
        "Populations" = length(private$.populations),
        "Parameter Sets" = length(private$.parameterSets),
        "Initial Conditions" = length(private$.initialConditions),
        "Applications" = length(private$.applications),
        "Output Paths" = length(private$.outputPaths),
        "Observed Data" = length(private$.observedData),
        "Data Combined" = length(private$.dataCombined),
        "Plots" = length(private$.plots),
        "Plot Grids" = length(private$.plotGrids),
        "Parameter Identification" = length(private$.parameterIdentification)
      )
      counts <- Filter(function(n) n > 0L, counts)
      if (length(counts) > 0L) {
        ospsuite.utils::ospPrintHeader("Definitions")
        ospsuite.utils::ospPrintItems(counts)
      }
      invisible(self)
    },

    # `project$excel`: the Excel side-car sheet names, shown only when the
    # project has one. The raw field names are relabeled to friendly labels,
    # falling back to the raw name so a future field is never silently dropped.
    # The stored (raw) sheet-name values are shown, not the resolved paths.
    .printExcelBlock = function() {
      excel <- lapply(private$.excelData, function(entry) entry$value)
      if (length(excel) == 0L) {
        return(invisible(self))
      }
      labels <- c(
        "configurationsFolder" = "Configurations Folder",
        "modelParamsFile" = "Model Parameters File",
        "individualsFile" = "Individuals File",
        "populationsFile" = "Populations File",
        "scenariosFile" = "Scenarios File",
        "applicationsFile" = "Applications File",
        "plotsFile" = "Plots File",
        "parameterIdentificationFile" = "Parameter Identification File",
        "initialConditionsFile" = "Initial Conditions File"
      )
      names(excel) <- vapply(
        names(excel),
        function(field) labels[[field]] %||% field,
        character(1L)
      )
      ospsuite.utils::ospPrintHeader("Excel")
      ospsuite.utils::ospPrintItems(excel)
      invisible(self)
    },

    # Package-internal read/write seam ----
    #
    # These private methods are the single sanctioned way to read and write a
    # project's in-memory state. They carry no `#'` roxygen and are dot-prefixed
    # private members, so they never appear on the public surface: a modeler
    # cannot call `project$.setSection(...)`. The public authoring methods
    # (`addScenario()`, `setParameter()`, the whole `add*`/`set*`/`remove*`
    # family) route every state change through them; the authoring logic lives
    # in `_impl()` free functions in the domain files, which receive `private`
    # from the calling method and reach the seam through it.

    # Read one definition section. Returns the plain backing list (NOT wrapped
    # in the read-only `DefinitionList` the public `project$<section>` getter
    # returns), so an authoring impl may bind it to a local copy and
    # subscript-assign that copy before re-submitting it via `.setSection()`.
    # Resolves `kind` through `.sectionField()`, which aborts on an unknown kind.
    .getSection = function(kind) {
      private[[private$.sectionField(kind)]]
    },

    # Write one definition section. The only sanctioned way to change a section:
    # it stores the new list in the private backing field, sets the in-memory
    # dirty bit, and invalidates the validation cache. It does not touch disk;
    # nothing persists until `saveProject()` reconciles the tree to memory.
    # Accepts a plain list; a `DefinitionList` is unwrapped defensively.
    .setSection = function(kind, value) {
      field <- private$.sectionField(kind)
      value <- .unwrapDefinitionList(value)
      private[[field]] <- value
      # Writing observed data invalidates the cached observed-data names, as the
      # active-binding setter did.
      if (identical(kind, "observedData")) {
        private$.observedDataNamesCache <- NULL
      }
      private$.modified <- TRUE
      private$.invalidate()
      invisible(value)
    },

    # Record that a full project validation has succeeded with no critical
    # errors. Sets the validation-cache flag.
    .markValidated = function() {
      private$.validatedSinceMutation <- TRUE
      invisible(self)
    },

    # Read the validation-cache flag: `TRUE` when a full validation has
    # succeeded since the last mutation or load, so the pre-op validation gate
    # (`.ensureValid()`) can skip a redundant re-run. The mirror of
    # `.isModified()` for the validation axis; a genuinely-private method with
    # no public binding, so the flag never appears on the object surface.
    .isValidated = function() {
      private$.validatedSinceMutation
    },

    # Pre-op validation gate. Runs targeted validation for the sections an
    # operation depends on and aborts with a formatted multi-error message if
    # any critical errors are found, short-circuiting when the project has been
    # fully validated since its last mutation (`.isValidated()`). Does not flip
    # the cache flag itself, because it only runs a subset of validators; only a
    # full `validateProject()` marks the project validated. Lives on the class
    # (not as a free function) so it can read the private validation-cache flag
    # without a public binding; the run/plot/PI entry points reach it through
    # the `ensureValid()` public forwarder.
    .ensureValid = function(sections, opName) {
      if (isTRUE(private$.isValidated())) {
        return(invisible(NULL))
      }
      results <- .runProjectValidation(self, sections = sections)
      if (isAnyCriticalErrors(results)) {
        .abortValidationErrors(results, opName)
      }
      invisible(NULL)
    },

    # Invoked by mutators after a successful programmatic change. Sets the
    # in-memory dirty bit and clears the validation-cache flag (via
    # `.invalidate()`) so the next `runScenarios()` / `createPlots()`
    # re-validates the project. Does not touch disk.
    .markModified = function() {
      private$.modified <- TRUE
      private$.invalidate()
      invisible(self)
    },

    # Read the in-memory dirty bit: `TRUE` when there are edits not yet
    # reconciled to the on-disk tree.
    .isModified = function() {
      private$.modified
    },

    # Clear the in-memory dirty bit, marking memory as in sync with the on-disk
    # tree. Called after a successful `saveProject()`.
    .clearModified = function() {
      private$.modified <- FALSE
      invisible(self)
    },

    # Re-read the project from its bound file, discarding any in-memory edits
    # (the undo). Re-runs the parser against `projectFilePath`, which overwrites
    # every backing field and clears the dirty and validation flags. Object
    # identity is preserved (the same R6 instance is mutated), so existing
    # handles stay valid.
    .reload = function() {
      private$.read_json(private$.projectFilePath)
      invisible(self)
    },

    .replace_env_var = function(path) {
      if (length(path) == 0L) {
        return(path)
      }
      pattern <- "\\$\\{?([A-Za-z_][A-Za-z0-9_]*)\\}?"
      m <- gregexpr(pattern, path, perl = TRUE)
      regmatches(path, m) <- lapply(regmatches(path, m), function(matches) {
        vapply(
          matches,
          function(match) {
            name <- sub(pattern, "\\1", match)
            if (identical(name, "PATH")) {
              return(match)
            }
            val <- Sys.getenv(name, unset = NA)
            if (is.na(val)) match else val
          },
          character(1)
        )
      })
      path
    },

    .clean_path = function(
      path,
      parent = NULL,
      replace_env_vars = TRUE
    ) {
      if (
        is.null(path) ||
          length(path) == 0L ||
          (length(path) == 1L && is.na(path))
      ) {
        return(NULL)
      }
      if (replace_env_vars) {
        path <- private$.replace_env_var(path)
      }
      if (
        is.null(parent) ||
          (length(parent) == 1L && is.na(parent)) ||
          fs::is_absolute_path(path)
      ) {
        fs::path_abs(path)
      } else {
        fs::path_abs(file.path(parent, path))
      }
    },

    # Resolve one working-folder value (`filePaths`) against `projectDirPath`
    # and require the result to stay under the project directory, so an
    # untrusted `Project.json` cannot point a working folder at an arbitrary
    # location (`"dataFolder": "/etc"`, or a `../`-escaping relative folder)
    # and then reference a plainly-"contained" file inside it. This is the
    # root-level companion to `.resolveProjectPath()`, which contains the leaf
    # paths joined onto these folders.
    #
    # The `${VAR}` environment-variable form is the sanctioned way to place a
    # folder outside the project tree (e.g. shared-drive data), so a raw value
    # that carries a `${VAR}` is exempt from the containment check; only a bare
    # absolute or `../`-escaping literal is rejected. Containment is judged on
    # the raw stored value (pre-expansion) for that reason.
    .resolveWorkingFolder = function(name) {
      raw <- private$.filePathsData[[name]]$value
      projectDir <- private$.projectDirPath
      resolved <- private$.clean_path(raw, projectDir)
      if (is.null(resolved)) {
        return(NULL)
      }
      # A from-scratch in-memory project has no on-disk directory yet, so there
      # is no project root to contain the folder against; skip the check (the
      # containment boundary comes into being only once the project is loaded
      # from / saved to a directory).
      hasProjectDir <- !is.null(projectDir) &&
        length(projectDir) == 1L &&
        !is.na(projectDir) &&
        nzchar(projectDir)
      # An explicit `${VAR}` opts into an out-of-project location; skip the
      # containment check for it (the variable value is the user's choice).
      declaresEnvVar <- is.character(raw) &&
        length(raw) == 1L &&
        grepl("\\$\\{?[A-Za-z_]", raw)
      # The resolved folder is already absolute; `.pathEscapesRoot()` compares
      # an absolute path to the root directly, so this rejects a folder value
      # that resolves outside the project directory.
      if (
        hasProjectDir &&
          !declaresEnvVar &&
          .pathEscapesRoot(as.character(resolved), projectDir)
      ) {
        cli::cli_abort(messages$projectPathEscapesRoot(
          name,
          raw,
          private$.projectDirPath
        ))
      }
      resolved
    },

    .read_json = function(jsonPath) {
      jsonPath <- fs::path_abs(jsonPath)
      if (!fs::file_exists(jsonPath)) {
        cli::cli_abort(messages$fileNotFound(jsonPath))
      }
      jsonData <- tryCatch(
        jsonlite::fromJSON(jsonPath, simplifyVector = FALSE),
        error = function(e) {
          cli::cli_abort(
            "Failed to parse {.file {jsonPath}} as JSON.",
            parent = e
          )
        }
      )
      if (!identical(jsonData$schemaVersion, "2.0")) {
        cli::cli_abort(
          "Unsupported schemaVersion: {.val {jsonData$schemaVersion %||% '<missing>'}}. Expected {.val 2.0}."
        )
      }
      private$.schemaVersion <- jsonData$schemaVersion
      private$.esqlabsRVersion <- jsonData$esqlabsRVersion
      private$.name <- jsonData$name
      private$.description <- jsonData$description
      private$.definitionsFolder <- jsonData$definitionsFolder
      private$.defaultSimulationRunOptions <- jsonData$defaultSimulationRunOptions
      private$.projectFilePath <- jsonPath
      private$.projectDirPath <- dirname(jsonPath)

      # The container separates two concerns: the live working folders
      # (the `filePaths` block) the runtime reads, and the Excel-bridge
      # sheet-name fields (the `excel` block) only the Excel bridge reads. A
      # legacy project carries both sets in one flat `filePaths` block; split
      # it on read so both on-disk shapes load (the field-to-block mapping is
      # fixed, so the partition is deterministic). A new-shape project reads
      # each block from its own key; any Excel field that still appears in
      # `filePaths` (e.g. a hand-edited file) is routed to the Excel store too.
      fp <- jsonData$filePaths %||% list()
      excel <- jsonData$excel %||% list()
      # A hand-edited `Project.json` could carry both the legacy `modelFolder`
      # and the current `simulationsFolder` key. They map to the same slot, so
      # rather than let iteration order decide, warn and drop the legacy key so
      # the current `simulationsFolder` deterministically wins.
      hasSimulationsCollision <- all(
        c("modelFolder", "simulationsFolder") %in% names(fp)
      )
      if (hasSimulationsCollision) {
        cli::cli_warn(messages$duplicateSimulationsFolderKey())
        fp[["modelFolder"]] <- NULL
      }
      private$.filePathsData <- list()
      private$.excelData <- list()
      for (n in names(fp)) {
        # Accept the pre-6.0.0 key `modelFolder` and store it under the current
        # name `simulationsFolder`, so a legacy `Project.json` (or an
        # Excel-imported project whose `Property` column still says
        # `modelFolder`) resolves without a manual edit.
        key <- if (identical(n, "modelFolder")) "simulationsFolder" else n
        if (key %in% .excelFilePathFields) {
          private$.excelData[[key]] <- list(value = fp[[n]], description = "")
        } else {
          private$.filePathsData[[key]] <- list(
            value = fp[[n]],
            description = ""
          )
        }
      }
      for (n in names(excel)) {
        private$.excelData[[n]] <- list(value = excel[[n]], description = "")
      }

      # Every authored section is a definition tree under `definitions/<kind>/`; a
      # single-file snapshot with no tree falls back to the inline section in
      # `Project.json`. `.loadDefinitionTree()` resolves tree-vs-inline per kind and
      # the kind's spec parses the raw records into the in-memory shape. Output
      # paths load before scenarios because scenarios dereference their
      # `outputPathIds` against the project-level `outputPaths` map. The
      # `parameterSets` inline fallback merges any legacy three-section
      # `Project.json` into the one map (a clash aborts the load).
      private$.outputPaths <- private$.loadSection("outputPaths", jsonData)
      private$.parameterSets <- private$.loadSection("parameterSets", jsonData)
      private$.initialConditions <- private$.loadSection(
        "initialConditions",
        jsonData
      )
      private$.individuals <- private$.loadSection("individuals", jsonData)
      private$.populations <- private$.loadSection("populations", jsonData)
      private$.applications <- private$.loadSection("applications", jsonData)
      private$.scenarios <- private$.loadSection("scenarios", jsonData)
      private$.observedData <- private$.loadSection("observedData", jsonData)
      # The plots concern is three independent top-level sections, each its own
      # keyed kind: `dataCombined` (`definitions/data-combined/`), `plots`
      # (`definitions/plots/`, the plot list), and `plotGrids`
      # (`definitions/plot-grids/`). Each loads from its own tree (or its own
      # top-level inline snapshot section as the fallback).
      private$.dataCombined <- private$.loadSection("dataCombined", jsonData)
      private$.plots <- private$.loadSection("plots", jsonData)
      private$.plotGrids <- private$.loadSection("plotGrids", jsonData)
      private$.parameterIdentification <- private$.loadSection(
        "parameterIdentification",
        jsonData
      )

      # Session-only observed-data state (runtime programmatic DataSets added via
      # `addObservedData(project, <DataSet>)`, and the observed-data names cache)
      # is not part of the on-disk tree, so a re-read must drop it: otherwise a
      # `reloadProject()` would leave a programmatic dataset that the reloaded
      # `observedData` section no longer carries, and `loadObservedData()` would
      # still return the discarded runtime dataset. Reset it so a reload fully
      # returns the object to the on-disk state.
      private$.programmaticDataSets <- list()
      private$.observedDataNamesCache <- NULL

      private$.validatedSinceMutation <- FALSE
      # A freshly loaded project has no unsaved changes: memory equals the tree.
      private$.modified <- FALSE
    },

    # Load one section: read its `definitions/<kind>/` tree (or the inline
    # snapshot fallback) and parse the raw records into the in-memory shape via
    # the kind's spec.
    .loadSection = function(kind, jsonData) {
      spec <- .definitionTreeSpec(kind)
      records <- .loadDefinitionTree(
        private$.projectDirPath,
        kind,
        spec$inline(jsonData),
        private$.definitionsFolder %||% "definitions"
      )
      spec$parse(records, self)
    }
  )
)

#' @rdname Project
#' @usage NULL
#' @export
ProjectConfiguration <- function(
  projectConfigurationFilePath = character(),
  ...
) {
  lifecycle::deprecate_warn("6.0.0", "ProjectConfiguration()", "Project$new()")
  Project$new(projectFilePath = projectConfigurationFilePath)
}
