# Project R6 class ----

# The container path fields that belong to the Excel import/export bridge
# (the `excel` block), as opposed to the live working folders (`modelFolder`,
# `dataFolder`, `outputFolder`, `populationsFolder`) that the runtime reads
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
#'   Under the explicit-save model, memory is the source of truth for a loaded
#'   project: every `add*` / `set*` / `remove*` authoring edit mutates memory
#'   in place and sets an internal dirty bit, but nothing touches the on-disk
#'   `definitions/` tree until [saveProject()] reconciles it. A loaded project
#'   is therefore a disk-free scratch space you commit deliberately; use
#'   [reloadProject()] to discard unsaved edits, and
#'   [snapshotProject()] / [restoreProject()] to checkpoint and roll back.
#'
#'   `Project` is not cloneable (`cloneable = FALSE`): explicit-save already
#'   makes every loaded project a disk-free scratch space, so the clone-for-
#'   scratch-work idiom the auto-save model needed is obsolete. All section
#'   data (`scenarios`, `individuals`, `populations`, `outputPaths`, the
#'   parameter sets, `plots`, `parameterIdentification`, ...) consists of
#'   plain-data records with copy semantics, so reading a record hands back an
#'   independent copy; the exception is programmatic observed data added via
#'   [addObservedData()] with a `DataSet` object, whose `ospsuite::DataSet`
#'   wraps an external handle shared by reference.
#' @format NULL
#' @import fs
#' @export
Project <- R6::R6Class(
  "Project",
  cloneable = FALSE,
  active = list(
    #' @field projectFilePath Read-only. Absolute path to the JSON
    #'   configuration file the project was loaded from. All other relative
    #'   paths in the project are resolved against the file's directory
    #'   (see `projectDirPath`). `NULL` for an empty in-memory project; in
    #'   that case all path fields must be absolute.
    projectFilePath = function(value) {
      if (!missing(value)) {
        cli::cli_abort("{.field projectFilePath} is readonly")
      }
      private$.projectFilePath
    },

    #' @field projectDirPath Read-only. Directory containing the JSON
    #'   configuration file (i.e. `dirname(projectFilePath)`). Used as the
    #'   base for resolving relative paths. `NULL` if the project was not
    #'   loaded from a file.
    projectDirPath = function(value) {
      if (!missing(value)) {
        cli::cli_abort("{.field projectDirPath} is readonly")
      }
      private$.projectDirPath
    },

    #' @field validatedSinceMutation Read-only logical. `TRUE` if a full
    #'   [validateProject()] has succeeded since the last project mutation
    #'   or load. Cleared by any mutation. Used internally by automatic
    #'   validation hooks (e.g. in [runScenarios()] and [createPlots()]) to
    #'   skip redundant re-validation of an unchanged project.
    validatedSinceMutation = function(value) {
      if (!missing(value)) {
        cli::cli_abort("{.field validatedSinceMutation} is readonly")
      }
      private$.validatedSinceMutation
    },

    #' @field name Human-readable project name (the `name` JSON field). May be
    #'   `NULL` when the project declares no name. Writing updates memory and
    #'   sets the dirty bit; it persists on the next [saveProject()].
    name = function(value) {
      if (!missing(value)) {
        private$.name <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.name
    },

    #' @field description Optional free-text project description (the
    #'   `description` JSON field). May be `NULL`. Writing updates memory and
    #'   sets the dirty bit; it persists on the next [saveProject()].
    description = function(value) {
      if (!missing(value)) {
        private$.description <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.description
    },

    #' @field definitionsFolder Name of the folder (relative to
    #'   `projectDirPath`) that holds the project's authored definitions
    #'   tree. Defaults to `"definitions"`. Writing updates memory and sets the
    #'   dirty bit; it changes where the next [saveProject()] writes the tree
    #'   and where the tree is read from, but nothing moves on disk until the
    #'   next save.
    definitionsFolder = function(value) {
      if (!missing(value)) {
        private$.definitionsFolder <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.definitionsFolder %||% "definitions"
    },

    #' @field defaultSimulationRunOptions Named list of the project-level
    #'   default simulation run options (the `defaultSimulationRunOptions` JSON
    #'   field), or `NULL` when none are declared. Used by [runScenarios()] as
    #'   the default `simulationRunOptions` when the caller does not pass one.
    #'   Recognized fields: `numberOfCores`, `checkForNegativeValues`,
    #'   `showProgress`. Writing updates memory and sets the dirty bit; it
    #'   persists on the next [saveProject()].
    defaultSimulationRunOptions = function(value) {
      if (!missing(value)) {
        private$.defaultSimulationRunOptions <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.defaultSimulationRunOptions
    },

    #' @field excel Read-only named list of the Excel import/export bridge
    #'   sheet-name fields (`configurationsFolder`, `modelParamsFile`,
    #'   `individualsFile`, `populationsFile`, `scenariosFile`,
    #'   `applicationsFile`, `plotsFile`, `parameterIdentificationFile`,
    #'   `initialConditionsFile`).
    #'   Returned verbatim as strings (no resolution). Empty for a from-scratch
    #'   JSON project that has no Excel side-car.
    excel = function(value) {
      if (!missing(value)) {
        cli::cli_abort("{.field excel} is readonly")
      }
      data <- private$.excelData
      if (length(data) == 0L) {
        return(structure(list(), names = character(0L)))
      }
      lapply(data, function(entry) entry$value)
    },

    #' @field schemaVersion Schema version declared in the JSON. Always "2.0"
    #'   for projects loaded by this parser. Writing updates memory and sets
    #'   the dirty bit; it persists on the next [saveProject()].
    schemaVersion = function(value) {
      if (!missing(value)) {
        private$.schemaVersion <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.schemaVersion
    },

    #' @field esqlabsRVersion Informational version string from the JSON.
    #'   Writing updates memory and sets the dirty bit; it persists on the next
    #'   [saveProject()].
    esqlabsRVersion = function(value) {
      if (!missing(value)) {
        private$.esqlabsRVersion <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.esqlabsRVersion
    },

    #' @field jsonPath Read-only. Absolute path the project was loaded from
    #'   (an alias of `projectFilePath`), or `NULL` for an in-memory project.
    jsonPath = function(value) {
      if (!missing(value)) {
        cli::cli_abort("{.field jsonPath} is readonly")
      }
      private$.projectFilePath
    },

    #' @field modelFolder Path to the folder containing pkml simulation files.
    modelFolder = function(value) {
      if (!missing(value)) {
        private$.filePathsData$modelFolder$value <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.clean_path(
        private$.filePathsData$modelFolder$value,
        self$projectDirPath
      )
    },

    #' @field configurationsFolder Path to the folder containing configuration
    #'   files. Part of the Excel import/export bridge (the `excel` container
    #'   block).
    configurationsFolder = function(value) {
      if (!missing(value)) {
        private$.excelData$configurationsFolder$value <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.clean_path(
        private$.excelData$configurationsFolder$value,
        self$projectDirPath
      )
    },

    #' @field modelParamsFile Path to the Excel file with global model
    #'   parameterization. Part of the Excel import/export bridge (the `excel`
    #'   container block).
    modelParamsFile = function(value) {
      if (!missing(value)) {
        private$.excelData$modelParamsFile$value <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.clean_path(
        private$.excelData$modelParamsFile$value,
        self$configurationsFolder
      )
    },

    #' @field individualsFile Path to the Excel file with individual-specific
    #'   model parameterization. Part of the Excel import/export bridge (the
    #'   `excel` container block).
    individualsFile = function(value) {
      if (!missing(value)) {
        private$.excelData$individualsFile$value <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.clean_path(
        private$.excelData$individualsFile$value,
        self$configurationsFolder
      )
    },

    #' @field populationsFile Path to the Excel file with population
    #'   information. Part of the Excel import/export bridge (the `excel`
    #'   container block).
    populationsFile = function(value) {
      if (!missing(value)) {
        private$.excelData$populationsFile$value <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.clean_path(
        private$.excelData$populationsFile$value,
        self$configurationsFolder
      )
    },

    #' @field scenariosFile Path to the Excel file with scenario definitions.
    #'   Part of the Excel import/export bridge (the `excel` container block).
    scenariosFile = function(value) {
      if (!missing(value)) {
        private$.excelData$scenariosFile$value <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.clean_path(
        private$.excelData$scenariosFile$value,
        self$configurationsFolder
      )
    },

    #' @field applicationsFile Path to the Excel file with scenario-specific
    #'   parameters such as application protocol parameters. Part of the Excel
    #'   import/export bridge (the `excel` container block).
    applicationsFile = function(value) {
      if (!missing(value)) {
        private$.excelData$applicationsFile$value <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.clean_path(
        private$.excelData$applicationsFile$value,
        self$configurationsFolder
      )
    },

    #' @field plotsFile Path to the Excel file with plot definitions. Part of
    #'   the Excel import/export bridge (the `excel` container block).
    plotsFile = function(value) {
      if (!missing(value)) {
        private$.excelData$plotsFile$value <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.clean_path(
        private$.excelData$plotsFile$value,
        self$configurationsFolder
      )
    },

    #' @field parameterIdentificationFile Name of the Excel workbook holding
    #'   the parameter-identification sheets (`PITasks`, `PIParameters`,
    #'   `PIOutputMappings`). Resolved relative to `configurationsFolder`.
    parameterIdentificationFile = function(value) {
      if (!missing(value)) {
        private$.excelData$parameterIdentificationFile$value <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.clean_path(
        private$.excelData$parameterIdentificationFile$value,
        self$configurationsFolder
      )
    },

    #' @field initialConditionsFile Name of the Excel workbook holding the
    #'   initial-condition (molecule start value) sheets, one sheet per set.
    #'   Part of the Excel import/export bridge (the `excel` container block);
    #'   resolved relative to `configurationsFolder`.
    initialConditionsFile = function(value) {
      if (!missing(value)) {
        private$.excelData$initialConditionsFile$value <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.clean_path(
        private$.excelData$initialConditionsFile$value,
        self$configurationsFolder
      )
    },

    #' @field populationsFolder Name of the folder containing population
    #'   definitions as CSV files. Resolved relative to `projectDirPath`.
    #'   Used by `runScenarios()` to load population CSVs at simulation time.
    populationsFolder = function(value) {
      if (!missing(value)) {
        private$.filePathsData$populationsFolder$value <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.clean_path(
        private$.filePathsData$populationsFolder$value,
        self$projectDirPath
      )
    },

    #' @field dataFolder Path to the folder where experimental data files are
    #'   located.
    dataFolder = function(value) {
      if (!missing(value)) {
        private$.filePathsData$dataFolder$value <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.clean_path(
        private$.filePathsData$dataFolder$value,
        self$projectDirPath
      )
    },

    #' @field outputFolder Path to the folder where the results should be saved
    #'   relative to the "Code" folder
    outputFolder = function(value) {
      if (!missing(value)) {
        private$.filePathsData$outputFolder$value <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.clean_path(
        private$.filePathsData$outputFolder$value,
        self$projectDirPath
      )
    },

    # Section data. Each accessor is READ-ONLY from the handle: the getter
    # returns the section wrapped in a printable, read-only `DefinitionList`,
    # and every assignment form (`project$x <- v`, `project$x[["id"]] <- v`,
    # the nested `project$x[["id"]]$f <- v`, `project$x[-i] <- v`) aborts. The
    # only sanctioned way to change a section is an authoring function
    # (`addScenario()` / `setScenario()` / `removeScenario()` and their
    # per-section siblings) or editing the definition's JSON file; those route
    # through the internal `.setSection()` entry point, which updates the
    # in-memory backing field, sets the dirty bit, and clears
    # `validatedSinceMutation` so the next run/plot re-validates. Nothing
    # touches the `definitions/<kind>/` tree until `saveProject()` reconciles
    # it to memory.

    #' @field outputPaths Read-only named list mapping output-path IDs to
    #'   OSPS-notation path strings (e.g. `list(PVB = "Organism|...")`). To
    #'   change it, use [addOutputPath()] / [setOutputPath()] /
    #'   [removeOutputPath()] or edit the definition files under
    #'   `definitions/output-paths/`; an authoring edit updates memory and sets
    #'   the dirty bit, persisting on the next [saveProject()].
    outputPaths = function(value) {
      if (!missing(value)) {
        .definitionListReadOnlyError("outputPaths")
      }
      .asDefinitionList(private$.outputPaths, "outputPaths")
    },

    #' @field scenarios Read-only named list of `Scenario` records, keyed by
    #'   scenario name. To change it, use [addScenario()] / [setScenario()] /
    #'   [removeScenario()] (or [renameScenario()] / [duplicateScenario()]), or
    #'   edit the definition files under `definitions/scenarios/`. The canonical
    #'   edit loop is read-modify-resubmit: read a record
    #'   (`sc <- project$scenarios[["id"]]`), change the detached copy
    #'   (`sc$modelFile <- ...`), then re-submit it
    #'   (`setScenario(project, "id", ...)`). An authoring edit updates memory
    #'   and sets the dirty bit; it persists to the tree on the next
    #'   [saveProject()].
    scenarios = function(value) {
      if (!missing(value)) {
        .definitionListReadOnlyError("scenarios")
      }
      .asDefinitionList(private$.scenarios, "scenarios")
    },

    #' @field parameterSets Read-only named list of parameter structures, keyed
    #'   by set id. This single section holds every parameter set in the
    #'   project: a scenario references the sets it applies through its
    #'   `modelParameterSets` field, an individual or application through its
    #'   `parameterSets` field; all three resolve against this one map. To
    #'   change it, use [addParameterSet()] / [removeParameterSet()] /
    #'   [addParameterEntry()] / [removeParameterEntry()] or edit the definition
    #'   files under `definitions/parameter-sets/`.
    parameterSets = function(value) {
      if (!missing(value)) {
        .definitionListReadOnlyError("parameterSets")
      }
      .asDefinitionList(private$.parameterSets, "parameterSets")
    },

    #' @field initialConditions Read-only named list of initial-condition sets,
    #'   keyed by set id. Each set is a list of molecule start-value records
    #'   (`path`, `value`, `unit`), applied to a scenario's simulation via its
    #'   `initialConditions` field. To change it, use [addInitialConditions()] /
    #'   [removeInitialConditions()] / [addInitialConditionEntry()] /
    #'   [removeInitialConditionEntry()] or edit the definition files under
    #'   `definitions/initial-conditions/`.
    initialConditions = function(value) {
      if (!missing(value)) {
        .definitionListReadOnlyError("initialConditions")
      }
      .asDefinitionList(private$.initialConditions, "initialConditions")
    },

    #' @field individuals Read-only named list of plain lists, keyed by
    #'   individualId. To change it, use [addIndividual()] / [setIndividual()] /
    #'   [removeIndividual()] or edit the definition files under
    #'   `definitions/individuals/`.
    individuals = function(value) {
      if (!missing(value)) {
        .definitionListReadOnlyError("individuals")
      }
      .asDefinitionList(private$.individuals, "individuals")
    },

    #' @field populations Read-only named list of plain lists, keyed by
    #'   populationId. To change it, use [addPopulation()] / [setPopulation()] /
    #'   [removePopulation()] or edit the definition files under
    #'   `definitions/populations/`.
    populations = function(value) {
      if (!missing(value)) {
        .definitionListReadOnlyError("populations")
      }
      .asDefinitionList(private$.populations, "populations")
    },

    #' @field applications Read-only named list of parameter structures, keyed
    #'   by protocol name. To change it, use [addApplication()] /
    #'   [removeApplication()] or edit the definition files under
    #'   `definitions/applications/`.
    applications = function(value) {
      if (!missing(value)) {
        .definitionListReadOnlyError("applications")
      }
      .asDefinitionList(private$.applications, "applications")
    },

    #' @field observedData Read-only list of observed data source declarations.
    #'   To change it, use [addObservedData()] / [removeObservedData()] or edit
    #'   the definition files under `definitions/observed-data/` (one file per
    #'   declaration).
    observedData = function(value) {
      if (!missing(value)) {
        .definitionListReadOnlyError("observedData")
      }
      .asDefinitionList(private$.observedData, "observedData")
    },

    #' @field dataCombined Read-only named list of `DataCombined` definitions,
    #'   keyed by `dataCombinedId`. Each entry pairs simulated and/or observed
    #'   curves. To change it, use [addDataCombined()] / [removeDataCombined()]
    #'   or edit the definition files under `definitions/data-combined/`.
    dataCombined = function(value) {
      if (!missing(value)) {
        .definitionListReadOnlyError("dataCombined")
      }
      .asDefinitionList(private$.dataCombined, "dataCombined")
    },

    #' @field plots Read-only named list of plot definitions, keyed by `plotId`.
    #'   Each entry is a single plot's configuration (`dataCombinedId`,
    #'   `plotType`, and styling fields). To change it, use [addPlot()] /
    #'   [removePlot()] or edit the definition files under `definitions/plots/`.
    plots = function(value) {
      if (!missing(value)) {
        .definitionListReadOnlyError("plots")
      }
      .asDefinitionList(private$.plots, "plots")
    },

    #' @field plotGrids Read-only named list of plot-grid definitions, keyed by
    #'   `plotGridId`. Each entry lays out one or more plots. To change it, use
    #'   [addPlotGrid()] / [removePlotGrid()] or edit the definition files under
    #'   `definitions/plot-grids/`.
    plotGrids = function(value) {
      if (!missing(value)) {
        .definitionListReadOnlyError("plotGrids")
      }
      .asDefinitionList(private$.plotGrids, "plotGrids")
    },

    #' @field parameterIdentification Read-only named list keyed by PI task id;
    #'   each entry is a `PITask` record. May be `NULL` or an empty list when the
    #'   project declares no PI tasks. To change it, use [addPITask()] /
    #'   [removePITask()] (and the per-task [addPIParameter()] /
    #'   [addPIOutputMapping()] and their removals) or edit the definition files
    #'   under `definitions/parameter-identification/`.
    parameterIdentification = function(value) {
      if (!missing(value)) {
        .definitionListReadOnlyError("parameterIdentification")
      }
      .asDefinitionList(
        private$.parameterIdentification,
        "parameterIdentification"
      )
    },

    #' @field filePaths Read-only named list of the project's working-folder
    #'   paths (the `filePaths` JSON block): `modelFolder`, `dataFolder`,
    #'   `outputFolder`, and `populationsFolder`. Values are returned verbatim
    #'   as strings; no resolution is performed at this stage. The Excel-bridge
    #'   sheet-name fields live in the separate `excel` block (`project$excel`),
    #'   not here.
    filePaths = function(value) {
      if (!missing(value)) {
        cli::cli_abort("{.field filePaths} is readonly")
      }
      data <- private$.filePathsData
      if (length(data) == 0L) {
        return(structure(list(), names = character(0L)))
      }
      lapply(data, function(entry) entry$value)
    },

    #' @field asList Returns the current project as a list matching the JSON
    #'   schema. Reflects any in-memory modifications. Read-only.
    asList = function(value) {
      if (!missing(value)) {
        cli::cli_abort("{.field asList} is readonly")
      }
      .projectToJson(self)
    },

    #' @field status Read-only, machine-oriented two-axis sync report as a
    #'   structured list: `tree_in_sync` (`FALSE` when there are unsaved
    #'   in-memory edits vs. the on-disk tree, `NA` for an unbound in-memory
    #'   project), `excel_in_sync` (`TRUE`/`FALSE`, or `NA` when no Excel
    #'   side-car is configured or it cannot be read), and `details` (per-axis
    #'   differences, empty when both axes are in sync). The same information
    #'   [syncStatus()] prints. Read-only; assignment aborts.
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

    #' @description Internal method to record that a full project
    #'   validation has succeeded with no critical errors. Sets the
    #'   `validatedSinceMutation` flag. Not intended for end-user use.
    #' @keywords internal
    .markValidated = function() {
      private$.validatedSinceMutation <- TRUE
      invisible(self)
    },

    #' @description Internal method invoked by mutators after a successful
    #'   programmatic change. Sets the in-memory dirty bit and clears the
    #'   `validatedSinceMutation` flag so the next `runScenarios()` /
    #'   `createPlots()` re-validates the project. Does not touch disk. Not
    #'   intended for end-user use.
    #' @keywords internal
    .markModified = function() {
      private$.modified <- TRUE
      private$.invalidate()
      invisible(self)
    },

    #' @description Internal accessor for the in-memory dirty bit: `TRUE` when
    #'   there are edits not yet reconciled to the on-disk tree. Read by
    #'   [saveProject()] (clean-save short-circuit) and by [syncStatus()] (the
    #'   memory-vs-tree axis). Not intended for end-user use.
    #' @keywords internal
    .isModified = function() {
      private$.modified
    },

    #' @description Internal method to clear the in-memory dirty bit, marking
    #'   memory as in sync with the on-disk tree. Called after a successful
    #'   [saveProject()]. Not intended for end-user use.
    #' @keywords internal
    .clearModified = function() {
      private$.modified <- FALSE
      invisible(self)
    },

    #' @description Internal method to re-read the project from its bound file,
    #'   discarding any in-memory edits (the undo). Re-runs the parser against
    #'   `projectFilePath`, which overwrites every backing field and clears the
    #'   dirty and validation flags. Object identity is preserved (R6
    #'   reference), so existing handles stay valid. Not intended for end-user
    #'   use; call [reloadProject()].
    #' @keywords internal
    .reload = function() {
      private$.read_json(private$.projectFilePath)
      invisible(self)
    },

    #' @description Internal read accessor for one definition section. Returns
    #'   the plain backing list (NOT wrapped in the read-only `DefinitionList`
    #'   the public `project$<section>` getter returns), so an authoring
    #'   function may bind it to a local copy and subscript-assign that copy
    #'   before re-submitting it via `.setSection()`. Not intended for end-user
    #'   use; the public accessor is the read-only end-user surface.
    #' @param kind Character scalar naming the section (e.g. `"scenarios"`).
    #' @keywords internal
    .getSection = function(kind) {
      private[[private$.sectionField(kind)]]
    },

    #' @description Internal write entry point for one definition section. This
    #'   is the only sanctioned way to change a section: it stores the new list
    #'   in the private backing field, sets the in-memory dirty bit, and
    #'   invalidates the validation cache. It does not touch disk; nothing
    #'   persists until [saveProject()] reconciles the tree to memory. The
    #'   public `project$<section> <- ...` setter aborts read-only; every
    #'   `add*`/`set*`/`remove*` authoring function routes its write here.
    #'   Accepts a plain list; a `DefinitionList` is unwrapped defensively. Not
    #'   intended for end-user use.
    #' @param kind Character scalar naming the section (e.g. `"scenarios"`).
    #' @param value The new section list.
    #' @keywords internal
    .setSection = function(kind, value) {
      field <- private$.sectionField(kind)
      value <- .unwrapDefinitionList(value)
      private[[field]] <- value
      # Writing observed data invalidates the cached observed-data names, as
      # the active-binding setter did.
      if (identical(kind, "observedData")) {
        private$.observedDataNamesCache <- NULL
      }
      private$.modified <- TRUE
      private$.invalidate()
      invisible(value)
    },

    #' @description Internal method to retrieve the raw working-folder metadata
    #'   (the `filePaths` block: a named list of `list(value, description)`
    #'   entries for the four live folders). Not intended for end-user use;
    #'   consumed by the Excel import/export bridge.
    #' @keywords internal
    .getFilePathsData = function() {
      private$.filePathsData
    },

    #' @description Internal method to retrieve the raw Excel-bridge metadata
    #'   (the `excel` block: a named list of `list(value, description)` entries
    #'   for the sheet-name fields). Empty when the project has no Excel
    #'   side-car. Not intended for end-user use; consumed by the Excel
    #'   import/export bridge.
    #' @keywords internal
    .getExcelData = function() {
      private$.excelData
    },

    #' @description Print a summary of the Project.
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

      # Show file locations relative to the project directory rather than as
      # absolute paths. The absolute prefix is machine-specific (and, for a
      # project loaded from a temp copy, varies in length by OS), so printing it
      # is both noisy for the user and a source of non-reproducible output. The
      # container is shown as its basename (`JSON File`); the working folders are
      # made relative to `projectDirPath`. Falls back to the raw value when there
      # is no project directory (an in-memory project), matching the already
      # project-relative Excel block below.
      relToProject <- function(path) {
        dir <- self$projectDirPath
        if (is.null(path) || is.null(dir)) {
          return(path)
        }
        as.character(fs::path_rel(path, start = dir))
      }

      # Metadata bullets. `print_empty = FALSE` drops the NULL/empty entries
      # (e.g. `jsonPath` for an in-memory project), so no explicit filtering
      # is needed here.
      ospsuite.utils::ospPrintItems(
        list(
          "Name" = self$name,
          "Description" = self$description,
          "Schema Version" = self$schemaVersion,
          "esqlabsR Version" = self$esqlabsRVersion,
          "JSON File" = if (!is.null(self$jsonPath)) {
            fs::path_file(self$jsonPath)
          }
        )
      )

      # Paths section: only the four live working folders, shown relative to the
      # project directory. `configurationsFolder` and the workbook file fields
      # belong to the Excel block, not here. Drop unset (NULL) folders and omit
      # the header when none is set.
      paths <- Filter(
        Negate(is.null),
        list(
          "Simulations Folder" = relToProject(self$modelFolder),
          "Data Folder" = relToProject(self$dataFolder),
          "Populations Folder" = relToProject(self$populationsFolder),
          "Output Folder" = relToProject(self$outputFolder)
        )
      )
      if (length(paths) > 0L) {
        ospsuite.utils::ospPrintHeader("Paths")
        ospsuite.utils::ospPrintItems(paths)
      }

      # Definitions section: the per-section entry counts. `ospPrintItems()`
      # prints an integer `0`, so zero-count sections are dropped explicitly
      # (not via `print_empty`). Omit the header when every section is empty.
      counts <- list(
        "Scenarios" = length(self$scenarios),
        "Individuals" = length(self$individuals),
        "Populations" = length(self$populations),
        "Parameter Sets" = length(self$parameterSets),
        "Initial Conditions" = length(self$initialConditions),
        "Applications" = length(self$applications),
        "Output Paths" = length(self$outputPaths),
        "Observed Data" = length(self$observedData),
        "Data Combined" = length(self$dataCombined),
        "Plots" = length(self$plots),
        "Plot Grids" = length(self$plotGrids),
        "Parameter Identification" = length(self$parameterIdentification)
      )
      counts <- Filter(function(n) n > 0L, counts)
      if (length(counts) > 0L) {
        ospsuite.utils::ospPrintHeader("Definitions")
        ospsuite.utils::ospPrintItems(counts)
      }

      # Excel section: only when the project has an Excel side-car. Relabel the
      # raw `excel` field names to friendly labels, falling back to the raw name
      # so a future field is never silently dropped.
      excel <- self$excel
      if (length(excel) > 0L) {
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
      }

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
    # axis of `syncStatus()`. A private field with no active binding keeps it
    # off the public surface. A freshly constructed in-memory project starts
    # clean (FALSE).
    .modified = FALSE,
    # Working-folder paths (the `filePaths` block): the four live folders the
    # runtime reads (`modelFolder`, `dataFolder`, `outputFolder`,
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

    # Invalidate the validation cache. Any mutation clears
    # `validatedSinceMutation` so the next `runScenarios()` / `createPlots()`
    # re-validates the new shape.
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
      private$.filePathsData <- list()
      private$.excelData <- list()
      for (n in names(fp)) {
        if (n %in% .excelFilePathFields) {
          private$.excelData[[n]] <- list(value = fp[[n]], description = "")
        } else {
          private$.filePathsData[[n]] <- list(value = fp[[n]], description = "")
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
        self$definitionsFolder
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
