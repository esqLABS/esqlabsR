# Project R6 class ----

# The seven container path fields that belong to the Excel import/export
# bridge (the `excel` block), as opposed to the four live working folders
# (`modelFolder`, `dataFolder`, `outputFolder`, `populationsFolder`) that the
# runtime reads (the `filePaths` block). A legacy `Project.json` carries all
# eleven in one flat `filePaths` block; this fixed mapping is what splits them
# on read and routes each to its own container block on write.
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
#' @description An R6 class representing an esqlabsR project
#'
#'   `Project` is cloneable. All section data (`scenarios`, `individuals`,
#'   `populations`, `outputPaths`, the parameter sets, `plots`,
#'   `parameterIdentification`, ...) consists of plain-data records with
#'   copy semantics, so a clone's section data is fully independent of the
#'   source: mutating one does not affect the other. The one exception is
#'   programmatic observed data added via [addObservedData()] with a
#'   `DataSet` object; those `ospsuite::DataSet` objects wrap external
#'   handles and are shared by reference between a project and its clone.
#'
#'   A clone is also detached from the source on disk: it does not own the
#'   source's definitions tree, so its write-through mutations (`addScenario()`,
#'   `removeScenario()`, and the other `add*` / `remove*` / `set*` helpers)
#'   stay in memory only and never touch the source's `definitions/`
#'   directory. Use [saveSnapshot()] to write the clone to a single-file
#'   snapshot you can reload with [loadSnapshot()].
#' @format NULL
#' @import fs
#' @export
Project <- R6::R6Class(
  "Project",
  cloneable = TRUE,
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
    #'   `NULL` when the project declares no name. Writing persists the
    #'   container for a bound project.
    name = function(value) {
      if (!missing(value)) {
        private$.name <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.name
    },

    #' @field description Optional free-text project description (the
    #'   `description` JSON field). May be `NULL`. Writing persists the
    #'   container for a bound project.
    description = function(value) {
      if (!missing(value)) {
        private$.description <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.description
    },

    #' @field definitionsFolder Name of the folder (relative to
    #'   `projectDirPath`) that holds the project's authored entity-definition
    #'   tree. Defaults to `"definitions"`. Writing persists the container for
    #'   a bound project; changing it moves where future write-through edits
    #'   are read from and written to.
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
    #'   `showProgress`. Writing persists the container for a bound project.
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
    #'   `applicationsFile`, `plotsFile`, `parameterIdentificationFile`).
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
    #'   for projects loaded by this parser. Writing persists the container
    #'   for a bound project.
    schemaVersion = function(value) {
      if (!missing(value)) {
        private$.schemaVersion <- value
        private$.invalidateContainer()
        return(invisible(value))
      }
      private$.schemaVersion
    },

    #' @field esqlabsRVersion Informational version string from the JSON.
    #'   Writing persists the container for a bound project.
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
    # per-section siblings) or editing the definition's JSON file; those write
    # through the internal `.setSection()` entry point, which structurally
    # validates each changed entity, persists it to (or deletes it from) its
    # `definitions/<kind>/` tree, and clears `validatedSinceMutation` so the
    # next run/plot re-validates.

    #' @field outputPaths Read-only named list mapping output-path IDs to
    #'   OSPS-notation path strings (e.g. `list(PVB = "Organism|...")`). To
    #'   change it, use [addOutputPath()] / [setOutputPath()] /
    #'   [removeOutputPath()] or edit the definition files under
    #'   `definitions/output-paths/`; an authoring write persists each changed
    #'   output path to (or deletes it from) its file.
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
    #'   (`setScenario(project, "id", ...)`). An authoring write structurally
    #'   validates each changed scenario and persists it to (or deletes it from)
    #'   its file; an in-memory project (no directory) holds scenarios in memory
    #'   only.
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
    #'   [addParameterEntry()] / [removeParameterEntry()] or edit the entity
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

    #' @description Internal method to rebind the project to a new file
    #'   location. Updates `projectFilePath` / `jsonPath` and `projectDirPath`
    #'   (the base for relative-path resolution) so any relative-path access
    #'   targets the new file, and re-claims the definitions tree so a clone bound to
    #'   its own location becomes write-through there. Not a mutation, so it
    #'   leaves the flags untouched. Not intended for end-user use.
    #' @param path Absolute or relative path the project was bound to.
    #' @keywords internal
    .rebindPath = function(path) {
      path <- fs::path_abs(path)
      private$.projectFilePath <- path
      private$.projectDirPath <- dirname(path)
      # Re-binding makes this instance the owner of the (new) entity tree, so a
      # clone bound to its own location becomes write-through there.
      private$.claimEntityTree()
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
    #'   programmatic change. Clears the `validatedSinceMutation` flag so the
    #'   next `runScenarios()` / `createPlots()` re-validates the project. Not
    #'   intended for end-user use.
    #' @keywords internal
    .markModified = function() {
      private$.invalidate()
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
    #'   is the only sanctioned way to change a section: it runs the same
    #'   structural validation and write-through persistence the public
    #'   active-binding setter used to, then stores the new list in the private
    #'   backing field and invalidates the validation cache. The public
    #'   `project$<section> <- ...` setter no longer writes (it aborts
    #'   read-only); every `add*`/`set*`/`remove*` authoring function routes its
    #'   write here. Accepts a plain list; a `DefinitionList` is unwrapped
    #'   defensively. Not intended for end-user use.
    #' @param kind Character scalar naming the section (e.g. `"scenarios"`).
    #' @param value The new section list.
    #' @keywords internal
    .setSection = function(kind, value) {
      field <- private$.sectionField(kind)
      value <- .unwrapDefinitionList(value)
      old <- private[[field]]
      # Persist before updating the backing store, so a structural failure
      # during serialization aborts the write and leaves disk and memory
      # unchanged (the same ordering the active-binding setter relied on).
      private$.persistSectionChanges(kind, old, value)
      private[[field]] <- value
      # Writing observed data invalidates the cached observed-data names, as
      # the active-binding setter did.
      if (identical(kind, "observedData")) {
        private$.observedDataNamesCache <- NULL
      }
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
    #'   for the seven sheet-name fields). Empty when the project has no Excel
    #'   side-car. Not intended for end-user use; consumed by the Excel
    #'   import/export bridge.
    #' @keywords internal
    .getExcelData = function() {
      private$.excelData
    },

    #' @description Report whether the project's Excel side-car is in sync
    #'   with its definition files. Every section is write-through to its
    #'   `definitions/<kind>/` tree and the `Project.json` container is a
    #'   derived snapshot, so the only drift that can still occur is between
    #'   the project and a sibling `Project.xlsx` produced by
    #'   [exportProjectToExcel()]. This method compares the two; it is
    #'   read-only and never reconciles them (use [exportProjectToExcel()] or
    #'   [importProjectFromExcel()] to do that).
    #' @param silent Logical. If `TRUE`, suppresses informational messages.
    #'   Defaults to `FALSE`.
    #' @return Invisibly, a list with two components: `excel_in_sync` (a
    #'   logical, or `NA` when there is no Excel side-car to compare against)
    #'   and `details` (a list of the per-section differences, empty when in
    #'   sync or when there is nothing to compare). This is the same shape the
    #'   path-based [projectStatus()] returns; the two share one comparison
    #'   builder.
    syncStatus = function(silent = FALSE) {
      .projectSyncStatus(self, silent = silent)
    },

    #' @description Print a summary of the Project.
    #' @param ... Unused; present for S3 method consistency.
    print = function(...) {
      cat(
        "<Project> (schema ",
        self$schemaVersion %||% "unknown",
        ")\n",
        sep = ""
      )
      if (!is.null(self$name)) {
        cat("  name:            ", self$name, "\n", sep = "")
      }
      if (!is.null(self$description)) {
        cat("  description:     ", self$description, "\n", sep = "")
      }
      if (!is.null(self$jsonPath)) {
        cat("  jsonPath:        ", self$jsonPath, "\n", sep = "")
      }
      cat(
        "  esqlabsRVersion: ",
        self$esqlabsRVersion %||% "NA",
        "\n",
        sep = ""
      )
      cat("  files:\n")
      cat(
        "    modelFolder:         ",
        self$modelFolder %||% "(unset)",
        "\n",
        sep = ""
      )
      cat(
        "    configurationsFolder:",
        self$configurationsFolder %||% "(unset)",
        "\n",
        sep = ""
      )
      cat(
        "    dataFolder:          ",
        self$dataFolder %||% "(unset)",
        "\n",
        sep = ""
      )
      cat(
        "    outputFolder:        ",
        self$outputFolder %||% "(unset)",
        "\n",
        sep = ""
      )
      cat("  scenarios:       ", length(self$scenarios), "\n", sep = "")
      cat("  individuals:     ", length(self$individuals), "\n", sep = "")
      cat("  populations:     ", length(self$populations), "\n", sep = "")
      cat(
        "  parameterSets:   ",
        length(self$parameterSets),
        " set(s)\n",
        sep = ""
      )
      cat(
        "  initialConditions: ",
        length(self$initialConditions),
        " set(s)\n",
        sep = ""
      )
      cat("  applications:    ", length(self$applications), "\n", sep = "")
      cat("  outputPaths:     ", length(self$outputPaths), "\n", sep = "")
      cat(
        "  observedData:    ",
        length(self$observedData),
        " source(s)\n",
        sep = ""
      )
      cat("  dataCombined:    ", length(self$dataCombined), "\n", sep = "")
      cat("  plots:           ", length(self$plots), "\n", sep = "")
      cat("  plotGrids:       ", length(self$plotGrids), "\n", sep = "")
      cat(
        "  parameterIdentification: ",
        length(self$parameterIdentification),
        " task(s)\n",
        sep = ""
      )
      invisible(self)
    }
  ),
  private = list(
    .projectFilePath = NULL,
    .projectDirPath = NULL,
    .validatedSinceMutation = FALSE,
    # Disk-ownership token for the project's entity tree. A bound project
    # (one loaded with `loadProject()` or saved with `saveSnapshot()`/a re-bind)
    # records itself as the token's owner. R6's `clone()` copies private fields
    # by value but shares this token environment by reference, so a clone is not
    # the recorded owner; `.ownsEntityTree()` returns `FALSE` for it and its
    # write-through becomes an in-memory no-op until a re-bind to a new location.
    # This keeps a clone's on-disk state independent of the source it was cloned
    # from.
    .entityTreeOwnerToken = NULL,
    # Working-folder paths (the `filePaths` block): the four live folders the
    # runtime reads (`modelFolder`, `dataFolder`, `outputFolder`,
    # `populationsFolder`).
    .filePathsData = list(),
    # Excel import/export bridge sheet names (the `excel` block): the seven
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
    # set of valid kinds is the single source of truth `.entityKindNames()`
    # (derived from the entity-tree specs), so a typo cannot silently create a
    # stray `private$.<typo>` field and the kind list is not duplicated here.
    .sectionField = function(kind) {
      if (
        !is.character(kind) ||
          length(kind) != 1L ||
          !(kind %in% .entityKindNames())
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

    # Container-metadata edit: invalidate the validation cache, then write the
    # container through to `Project.json` so the metadata (schema/version, file
    # paths, folders) is persisted immediately, just as every section is.
    .invalidateContainer = function() {
      private$.invalidate()
      private$.persistContainer()
      invisible(self)
    },

    # Write the `Project.json` container (every inline section emptied, the
    # tree owns them) to the project's own location, when this instance owns
    # its entity tree. A NULL directory (in-memory project) or a non-owning
    # clone is a silent no-op (its container edits stay in memory until it is
    # bound to its own location).
    .persistContainer = function() {
      if (is.null(private$.projectFilePath) || !private$.ownsEntityTree()) {
        return(invisible(NULL))
      }
      .saveProjectJson(
        self,
        private$.projectFilePath,
        containerOnly = TRUE
      )
      invisible(NULL)
    },

    # Record this instance as the owner of its entity tree. Called whenever
    # the project binds to a directory (`.read_json`, `.rebindPath`).
    .claimEntityTree = function() {
      private$.entityTreeOwnerToken <- new.env(parent = emptyenv())
      private$.entityTreeOwnerToken$owner <- self
      invisible(self)
    },

    # TRUE only when this instance recorded itself as the entity-tree owner.
    # A clone shares the token by reference (its `owner` is the source), so it
    # is not the owner until a re-bind to its own location.
    .ownsEntityTree = function() {
      token <- private$.entityTreeOwnerToken
      !is.null(token) && identical(token$owner, self)
    },

    # Write-through for one project section: persist only what changed to its
    # `definitions/<kind>/` tree. Every section now maps one-to-one onto a kind
    # (the plots trio was split into three top-level sections
    # `dataCombined` / `plots` / `plotGrids`, each its own keyed kind).
    .persistSectionChanges = function(kind, old, new) {
      private$.persistKindChanges(kind, old, new)
    },

    # Persist one kind's changes (the unit a single `definitions/<kind>/` tree
    # owns). Persist only what changed between the old and new section value.
    #
    # An in-memory project (no directory) is a silent no-op, and so is a clone
    # that does not own the tree (its mutations stay in memory until a re-bind
    # to its own location). The caller invokes this before it updates the
    # in-memory backing store, so a structural failure during serialization
    # aborts the write and leaves both disk and memory unchanged.
    #
    # First write into a project whose `definitions/<kind>/` directory does not
    # yet exist (a snapshot-loaded project, whose sections were inlined in
    # `Project.json` with no tree on disk) takes a full-materialize path: the
    # diff-only path would write just the changed entity and assume the
    # untouched siblings already exist as files, but they do not, so on the
    # next load the now-present tree would win and the inline siblings would be
    # lost. `new` is the authoritative full set to write; the dir was absent,
    # so there is nothing stale to remove.
    .persistKindChanges = function(kind, old, new) {
      spec <- .entityTreeSpec(kind)
      dir <- .entityKindDir(
        private$.projectDirPath,
        spec$kind,
        self$definitionsFolder
      )
      if (is.null(dir)) {
        return(invisible(NULL))
      }
      if (!private$.ownsEntityTree()) {
        return(invisible(NULL))
      }

      # First write into a not-yet-materialized tree writes the full set.
      if (!dir.exists(dir)) {
        .writeEntityTree(new, kind, self, private$.projectDirPath)
        return(invisible(NULL))
      }

      old <- old %||% list()
      new <- new %||% list()

      # A keyed kind is a named map whose names are the entity ids (which equal
      # the on-disk filenames). For such a kind the diff is computed directly on
      # the in-memory maps (by key presence and per-entity value identity) and
      # only the entities that actually changed are serialized and written, so a
      # single mutation costs O(changed entities), not O(section size). The
      # plots `plotConfiguration` / `plotGrids` parts are keyed lists, so they
      # take this fast path too. An `observedData` section is an unnamed list
      # keyed by a derived id (its `names()` are not the on-disk ids), so it
      # cannot be diffed by name and falls back to the whole-section serialize
      # diff below (it is small and never a buildup hot path).
      keyed <- !is.data.frame(new) &&
        !is.data.frame(old) &&
        (length(new) == 0L || !is.null(names(new)))
      if (keyed && (length(old) == 0L || !is.null(names(old)))) {
        newNames <- names(new)
        oldNames <- names(old)
        # Added ids are new keys absent from old; removed ids are old keys
        # absent from new (both via cheap name set ops, no value comparison).
        # An id present in both whose value object is not the very same record
        # (an in-place edit) is changed. `match()` aligns the common keys so
        # only those need the per-entity `identical()` check, keeping the diff
        # proportional to the number of common keys without re-checking added
        # ones.
        inOld <- newNames %in% oldNames
        addedIds <- newNames[!inOld]
        commonIds <- newNames[inOld]
        # Extract the common-key sublists once (C-level subset) and compare them
        # as whole objects first: an unchanged element shares its record object
        # with the old list (an `[[<-` mutation does not copy untouched
        # siblings), so when nothing in the common set changed this single
        # `identical()` short-circuits on reference equality with no R-level
        # per-element loop. Only when some common entity did change (an in-place
        # edit) do we fall back to a per-element scan to find which one(s).
        nc <- new[commonIds]
        oc <- old[commonIds]
        changedCommon <- character()
        if (length(commonIds) > 0L && !identical(nc, oc)) {
          changedCommon <- commonIds[
            !vapply(
              seq_along(commonIds),
              function(i) identical(nc[[i]], oc[[i]]),
              logical(1),
              USE.NAMES = FALSE
            )
          ]
        }
        changedIds <- c(addedIds, changedCommon)
        # Serialize only the changed entities (validating each), in memory and
        # before writing any file, so a serializer-hostile record aborts the
        # whole write and leaves the tree (and the not-yet-updated in-memory
        # store) unchanged. The serializer also enforces that each key is a
        # canonical (lowercase, safe) id, so two keys can never collide on a
        # case-insensitive filesystem.
        if (length(changedIds) > 0L) {
          serializedChanged <- spec$serialize(new[changedIds], self)
          for (id in names(serializedChanged)) {
            .writeEntityJson(serializedChanged[[id]], .entityFilePath(dir, id))
          }
        }
        # A genuinely-removed entity's on-disk id is its serialized key; for a
        # keyed kind that equals the map key, so the removed files can be deleted
        # without serializing.
        #
        # Stale removal is reconciled against the previous in-memory section
        # (`old`), not against the directory listing. So a file created
        # out-of-band on disk (an orphan with no in-memory entity, e.g. one a
        # concurrent process or a hand-edit dropped in) is not deleted by an
        # unrelated mutation here; it survives until the next `loadProject()`,
        # which re-reads the whole tree and re-derives the section from what is
        # actually on disk. This is deliberate: the in-memory diff cannot tell an
        # orphan from a sibling it simply did not load, and reconciling against
        # the directory on every write would couple each mutation to a directory
        # scan for no data-safety gain (no entity is lost; the orphan is just
        # picked up, or ignored, on the next load).
        for (id in oldNames[!(oldNames %in% newNames)]) {
          f <- .entityFilePath(dir, id)
          if (file.exists(f)) {
            file.remove(f)
          }
        }
        return(invisible(NULL))
      }

      # Fallback for a kind whose on-disk id is derived rather than the map key
      # (the unnamed `observedData` list): serialize the full old and new
      # sections to their `id -> record` maps in memory before writing any file
      # (same atomicity guarantee), then diff on the derived ids.
      serializedNew <- spec$serialize(new, self)
      serializedOld <- spec$serialize(old, self)
      changed <- Filter(
        function(id) !identical(serializedNew[[id]], serializedOld[[id]]),
        names(serializedNew)
      )
      for (id in changed) {
        .writeEntityJson(serializedNew[[id]], .entityFilePath(dir, id))
      }
      for (id in setdiff(names(serializedOld), names(serializedNew))) {
        f <- .entityFilePath(dir, id)
        if (file.exists(f)) {
          file.remove(f)
        }
      }
      invisible(NULL)
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
      private$.claimEntityTree()

      # The container separates two concerns: the four live working folders
      # (the `filePaths` block) the runtime reads, and the seven Excel-bridge
      # sheet-name fields (the `excel` block) only the Excel bridge reads. A
      # legacy project carries all eleven in one flat `filePaths` block; split
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

      # Every authored section is an entity tree under `definitions/<kind>/`; a
      # single-file snapshot with no tree falls back to the inline section in
      # `Project.json`. `.loadEntityTree()` resolves tree-vs-inline per kind and
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

      private$.validatedSinceMutation <- FALSE
    },

    # Load one section: read its `definitions/<kind>/` tree (or the inline
    # snapshot fallback) and parse the raw records into the in-memory shape via
    # the kind's spec.
    .loadSection = function(kind, jsonData) {
      spec <- .entityTreeSpec(kind)
      records <- .loadEntityTree(
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
