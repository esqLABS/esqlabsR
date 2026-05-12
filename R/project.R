# Project R6 class (internal, work-in-progress) ----
#
# Holds an esqlabsR project parsed from a v2.0 `Project.json` file.
#
# This class is the seed of a larger refactor that will eventually replace the
# Excel-driven `ProjectConfiguration` workflow with a JSON-first one. As of this
# branch nothing in the package reads from `Project` at runtime — `runScenarios`,
# the validators, and the plotting layer all still drive off the existing
# `ProjectConfiguration` / `ScenarioConfiguration` classes and Excel files.
#
# `Project` is therefore intentionally kept internal:
#
#   * Not added to `NAMESPACE` (no `@export`).
#   * No mutation API. Public fields are read-only; mutations would belong to a
#     later milestone where the class actually owns project state.
#   * No auto-validation, no cross-reference resolution, no path resolution
#     beyond what's required to print sensibly.
#
# Construction goes through `.loadProjectJson()` (`R/project-parse.R`).

#' @title Project (internal, schema 2.0)
#' @docType class
#' @description An R6 class representing an esqlabsR project parsed from a
#'   v2.0 `Project.json` file. Internal; not exported. Exists as the in-memory
#'   shape future code will read from once the JSON-first workflow lands.
#' @format NULL
#' @keywords internal
#' @noRd
Project <- R6::R6Class(
  "Project",
  cloneable = TRUE,
  active = list(
    #' @field schemaVersion Schema version declared in the JSON. Always "2.0"
    #'   for projects loaded by this parser.
    schemaVersion = function(value) {
      if (!missing(value))
        cli::cli_abort("{.field schemaVersion} is read-only.")
      private$.schemaVersion
    },

    #' @field validatedSinceMutation Read-only logical. `TRUE` if a full
    #'   [validateProject()] has succeeded with no critical errors since
    #'   the project was loaded or last mutated. Cleared by any mutation.
    #'   Used by [runScenarios()] and [createPlots()] to skip redundant
    #'   re-validation of an unchanged project.
    validatedSinceMutation = function(value) {
      if (!missing(value)) {
        cli::cli_abort("{.field validatedSinceMutation} is read-only.")
      }
      private$.validatedSinceMutation
    },

    #' @field modified Read-only logical. `TRUE` if any configuration
    #'   property has been modified since the project was loaded or
    #'   saved. Cleared internally when the project is freshly loaded.
    modified = function(value) {
      if (!missing(value)) cli::cli_abort("{.field modified} is read-only.")
      private$.modified
    },

    #' @field esqlabsRVersion Informational version string from the JSON.
    esqlabsRVersion = function(value) {
      if (!missing(value))
        cli::cli_abort("{.field esqlabsRVersion} is read-only.")
      private$.esqlabsRVersion
    },

    #' @field jsonPath Absolute path the project was loaded from, or `NULL`.
    jsonPath = function(value) {
      if (!missing(value)) cli::cli_abort("{.field jsonPath} is read-only.")
      private$.jsonPath
    },

    #' @field projectDirPath Absolute path to the directory containing the JSON
    #'   file, or `NULL` if the project was not loaded from disk. All relative
    #'   paths in the JSON are interpreted relative to this directory.
    projectDirPath = function(value) {
      if (!missing(value))
        cli::cli_abort("{.field projectDirPath} is read-only.")
      private$.projectDirPath
    },

    #' @field filePaths Named list of declared file/folder paths (the
    #'   `filePaths` JSON section). Values are stored verbatim as strings; no
    #'   resolution is performed at this stage.
    filePaths = function(value) {
      if (!missing(value)) cli::cli_abort("{.field filePaths} is read-only.")
      private$.filePaths
    },

    #' @field configurationsFolder Read-only. Absolute path to the
    #'   `configurationsFolder` slot under `filePaths`, resolved
    #'   relative to `projectDirPath`. `NULL` when the slot is unset.
    configurationsFolder = function(value) {
      if (!missing(value)) {
        cli::cli_abort("{.field configurationsFolder} is read-only.")
      }
      private$.clean_path(
        private$.filePaths$configurationsFolder,
        parent = private$.projectDirPath
      )
    },

    #' @field modelFolder Read-only. Absolute path to the `modelFolder`
    #'   slot under `filePaths`, resolved relative to `projectDirPath`.
    #'   `NULL` when the slot is unset.
    modelFolder = function(value) {
      if (!missing(value)) {
        cli::cli_abort("{.field modelFolder} is read-only.")
      }
      private$.clean_path(
        private$.filePaths$modelFolder,
        parent = private$.projectDirPath
      )
    },

    #' @field populationsFolder Read-only. Absolute path to the
    #'   `populationsFolder` slot under `filePaths`, resolved relative
    #'   to `projectDirPath`. Holds population CSV files loaded by
    #'   `runScenarios()` for scenarios with `readPopulationFromCSV =
    #'   TRUE`. `NULL` when the slot is unset.
    populationsFolder = function(value) {
      if (!missing(value)) {
        cli::cli_abort("{.field populationsFolder} is read-only.")
      }
      private$.clean_path(
        private$.filePaths$populationsFolder,
        parent = private$.projectDirPath
      )
    },

    #' @field dataFolder Read-only. Absolute path to the `dataFolder`
    #'   slot under `filePaths`, resolved relative to `projectDirPath`.
    #'   Holds observed-data sources (Excel workbooks, PKML files,
    #'   importer configurations, scripts) consumed by
    #'   `loadObservedData()`. `NULL` when the slot is unset.
    dataFolder = function(value) {
      if (!missing(value)) {
        cli::cli_abort("{.field dataFolder} is read-only.")
      }
      private$.clean_path(
        private$.filePaths$dataFolder,
        parent = private$.projectDirPath
      )
    },

    #' @field outputPaths Named list mapping output-path IDs to literal output
    #'   path strings.
    outputPaths = function(value) {
      if (!missing(value)) {
        private$.outputPaths <- value
      }
      private$.outputPaths
    },

    #' @field scenarios Named list of `Scenario` objects, indexed
    #'   by scenario name. Built by `.parseScenarios()` from the
    #'   raw JSON `scenarios` array; round-trips back through
    #'   `.scenariosToJson()`.
    scenarios = function(value) {
      if (!missing(value)) {
        private$.scenarios <- value
      }
      private$.scenarios
    },

    #' @field modelParameterSets Named list keyed by parameter-set name; each
    #'   value is a list of parameter entries.
    modelParameterSets = function(value) {
      if (!missing(value)) {
        private$.modelParameterSets <- value
      }
      private$.modelParameterSets
    },

    #' @field individualParameterSets Named list keyed by parameter-set name;
    #'   each value is a list of parameter entries. Referenced by id from
    #'   `individuals[[id]]$parameterSets`.
    individualParameterSets = function(value) {
      if (!missing(value)) {
        private$.individualParameterSets <- value
      }
      private$.individualParameterSets
    },

    #' @field applicationParameterSets Named list keyed by parameter-set name;
    #'   each value is a list of parameter entries. Referenced by id from
    #'   `applications[[name]]$parameterSets`.
    applicationParameterSets = function(value) {
      if (!missing(value)) {
        private$.applicationParameterSets <- value
      }
      private$.applicationParameterSets
    },

    #' @field individuals Named list of individual entries, keyed by
    #'   individualId.
    individuals = function(value) {
      if (!missing(value)) {
        private$.individuals <- value
      }
      private$.individuals
    },

    #' @field populations Named list of population entries, keyed by
    #'   populationId.
    populations = function(value) {
      if (!missing(value)) {
        private$.populations <- value
      }
      private$.populations
    },

    #' @field applications Named list keyed by application-protocol name.
    applications = function(value) {
      if (!missing(value)) {
        private$.applications <- value
      }
      private$.applications
    },

    #' @field observedData List of observed-data source entries.
    observedData = function(value) {
      if (!missing(value)) {
        private$.observedData <- value
      }
      private$.observedData
    },

    #' @field plots Named list with sub-entries `dataCombined`,
    #'   `plotConfiguration`, and `plotGrids`. `NULL` if the JSON omits the
    #'   `plots` section. `plotGrids` is a data frame whose `plotIDs` column
    #'   holds, for each row, a single comma-separated string (e.g.
    #'   `"P1, P2"`) rather than a JSON array; this matches the v2.0 schema
    #'   and the existing Excel `Plots` sheet convention. Splitting and
    #'   normalising is deferred to the plots chapter.
    plots = function(value) {
      if (!missing(value)) {
        private$.plots <- value
      }
      private$.plots
    },

    #' @field parameterIdentification Named list keyed by PI task id; each
    #'   entry is a `PITask` record. May be `NULL` or an empty list when
    #'   the project declares no PI tasks. Read-write active binding;
    #'   mutation discipline is enforced inside `add*` / `remove*` exports,
    #'   not by the binding.
    parameterIdentification = function(value) {
      if (!missing(value)) {
        private$.parameterIdentification <- value
      }
      private$.parameterIdentification
    }
  ),
  public = list(
    #' @description Construct a `Project` directly from already-parsed pieces.
    #'   Direct construction is intended for use by `.loadProjectJson()` only;
    #'   callers should go through that function.
    #'
    #' @param schemaVersion Schema version string (must be "2.0").
    #' @param esqlabsRVersion Informational version string.
    #' @param jsonPath Absolute path of the source JSON, or `NULL`.
    #' @param projectDirPath Absolute path of the source directory, or `NULL`.
    #' @param filePaths Named list of file paths.
    #' @param outputPaths Named list of output-path IDs to paths.
    #' @param scenarios Named list of `Scenario` objects (typically
    #'   produced by `.parseScenarios()`), indexed by scenario name.
    #' @param modelParameterSets Named list of model parameter sets.
    #' @param individualParameterSets Named list of individual parameter sets.
    #' @param applicationParameterSets Named list of application parameter sets.
    #' @param individuals List of individual entries.
    #' @param populations List of population entries.
    #' @param applications Named list of application-protocol entries.
    #' @param observedData List of observed-data source entries.
    #' @param plots Named list of plot sub-sections, or `NULL`.
    #' @param parameterIdentification Named list of `PITask` objects keyed
    #'   by task id. Defaults to an empty list.
    initialize = function(
      schemaVersion,
      esqlabsRVersion,
      jsonPath,
      projectDirPath,
      filePaths,
      outputPaths,
      scenarios,
      modelParameterSets,
      individualParameterSets,
      applicationParameterSets,
      individuals,
      populations,
      applications,
      observedData,
      plots,
      parameterIdentification = list()
    ) {
      private$.schemaVersion <- schemaVersion
      private$.esqlabsRVersion <- esqlabsRVersion
      private$.jsonPath <- jsonPath
      private$.projectDirPath <- projectDirPath
      private$.filePaths <- filePaths
      private$.outputPaths <- outputPaths
      private$.scenarios <- scenarios
      private$.modelParameterSets <- modelParameterSets
      private$.individualParameterSets <- individualParameterSets
      private$.applicationParameterSets <- applicationParameterSets
      private$.individuals <- individuals
      private$.populations <- populations
      private$.applications <- applications
      private$.observedData <- observedData
      private$.plots <- plots
      private$.parameterIdentification <- parameterIdentification
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
    #'   programmatic change. Sets the `modified` flag and clears the
    #'   `validatedSinceMutation` flag so that any cached validation
    #'   result is invalidated. Not intended for end-user use.
    #' @keywords internal
    .markModified = function() {
      private$.invalidate()
      invisible(self)
    },

    #' @description Add a scenario programmatically. Delegates to the
    #'   standalone [addScenario()] function.
    #' @param scenarioName Character. Name for the new scenario.
    #' @param modelFile Character. Name of the `.pkml` model file.
    #' @param ... Additional arguments passed to [addScenario()].
    addScenario = function(scenarioName, modelFile, ...) {
      addScenario(
        project = self,
        scenarioName = scenarioName,
        modelFile = modelFile,
        ...
      )
    },

    #' @description Remove a scenario programmatically. Delegates to the
    #'   standalone [removeScenario()] function.
    #' @param name Character.
    removeScenario = function(name) {
      removeScenario(project = self, name = name)
    },

    #' @description Add an individual programmatically. Delegates to the
    #'   standalone [addIndividual()] function.
    #' @param individualId Character. Unique ID.
    #' @param species Character. Species name.
    #' @param ... Additional fields passed to [addIndividual()].
    addIndividual = function(individualId, species, ...) {
      addIndividual(
        project = self,
        individualId = individualId,
        species = species,
        ...
      )
    },

    #' @description Remove an individual programmatically. Delegates to the
    #'   standalone [removeIndividual()] function.
    #' @param individualId Character.
    removeIndividual = function(individualId) {
      removeIndividual(project = self, individualId = individualId)
    },

    #' @description Replace the parameter-set references on an individual.
    #' @param individualId Character.
    #' @param parameterSets Character vector of set ids.
    setIndividualParameterSets = function(individualId, parameterSets) {
      setIndividualParameterSets(self, individualId, parameterSets)
    },

    #' @description Add a population programmatically.
    #' @param populationId Character.
    #' @param species Character.
    #' @param numberOfIndividuals Integer.
    #' @param ... Passed to [addPopulation()].
    addPopulation = function(populationId, species, numberOfIndividuals, ...) {
      addPopulation(
        project = self,
        populationId = populationId,
        species = species,
        numberOfIndividuals = numberOfIndividuals,
        ...
      )
    },

    #' @description Remove a population programmatically.
    #' @param populationId Character.
    removePopulation = function(populationId) {
      removePopulation(project = self, populationId = populationId)
    },

    #' @description Add an application protocol programmatically.
    #' @param applicationId Character.
    #' @param parameterSets Optional character vector of set ids.
    addApplication = function(applicationId, parameterSets = NULL) {
      addApplication(
        project = self,
        applicationId = applicationId,
        parameterSets = parameterSets
      )
    },

    #' @description Remove an application protocol programmatically.
    #' @param applicationId Character.
    removeApplication = function(applicationId) {
      removeApplication(project = self, applicationId = applicationId)
    },

    #' @description Replace the parameter-set references on an application.
    #' @param applicationId Character.
    #' @param parameterSets Character vector of set ids.
    setApplicationParameterSets = function(applicationId, parameterSets) {
      setApplicationParameterSets(self, applicationId, parameterSets)
    },

    #' @description Create a model parameter set.
    #' @param id Character.
    addModelParameterSet = function(id) {
      addModelParameterSet(self, id)
    },

    #' @description Remove a model parameter set.
    #' @param id Character.
    removeModelParameterSet = function(id) {
      removeModelParameterSet(self, id)
    },

    #' @description Add a parameter entry to a named model-parameter set.
    #' @param id Character.
    #' @param containerPath Character.
    #' @param parameterName Character.
    #' @param value Numeric.
    #' @param units Character.
    addModelParameterEntry = function(
      id,
      containerPath,
      parameterName,
      value,
      units
    ) {
      addModelParameterEntry(
        self,
        id,
        containerPath,
        parameterName,
        value,
        units
      )
    },

    #' @description Remove a parameter entry from a named model-parameter set.
    #' @param id Character.
    #' @param containerPath Character.
    #' @param parameterName Character.
    removeModelParameterEntry = function(id, containerPath, parameterName) {
      removeModelParameterEntry(self, id, containerPath, parameterName)
    },

    #' @description Create an individual parameter set.
    #' @param id Character.
    addIndividualParameterSet = function(id) {
      addIndividualParameterSet(self, id)
    },

    #' @description Remove an individual parameter set.
    #' @param id Character.
    removeIndividualParameterSet = function(id) {
      removeIndividualParameterSet(self, id)
    },

    #' @description Add a parameter entry to a named individual parameter set.
    #' @param id Character.
    #' @param containerPath Character.
    #' @param parameterName Character.
    #' @param value Numeric.
    #' @param units Character.
    addIndividualParameterSetEntry = function(
      id,
      containerPath,
      parameterName,
      value,
      units
    ) {
      addIndividualParameterSetEntry(
        self,
        id,
        containerPath,
        parameterName,
        value,
        units
      )
    },

    #' @description Remove a parameter entry from a named individual
    #'   parameter set.
    #' @param id Character.
    #' @param containerPath Character.
    #' @param parameterName Character.
    removeIndividualParameterSetEntry = function(
      id,
      containerPath,
      parameterName
    ) {
      removeIndividualParameterSetEntry(
        self,
        id,
        containerPath,
        parameterName
      )
    },

    #' @description Create an application parameter set.
    #' @param id Character.
    addApplicationParameterSet = function(id) {
      addApplicationParameterSet(self, id)
    },

    #' @description Remove an application parameter set.
    #' @param id Character.
    removeApplicationParameterSet = function(id) {
      removeApplicationParameterSet(self, id)
    },

    #' @description Add a parameter entry to a named application parameter set.
    #' @param id Character.
    #' @param containerPath Character.
    #' @param parameterName Character.
    #' @param value Numeric.
    #' @param units Character.
    addApplicationParameterSetEntry = function(
      id,
      containerPath,
      parameterName,
      value,
      units
    ) {
      addApplicationParameterSetEntry(
        self,
        id,
        containerPath,
        parameterName,
        value,
        units
      )
    },

    #' @description Remove a parameter entry from a named application
    #'   parameter set.
    #' @param id Character.
    #' @param containerPath Character.
    #' @param parameterName Character.
    removeApplicationParameterSetEntry = function(
      id,
      containerPath,
      parameterName
    ) {
      removeApplicationParameterSetEntry(
        self,
        id,
        containerPath,
        parameterName
      )
    },

    #' @description Add one or more output paths programmatically.
    #' @param id Character vector.
    #' @param path Character vector.
    addOutputPath = function(id, path) {
      addOutputPath(self, id, path)
    },

    #' @description Remove an output path programmatically.
    #' @param id Character.
    removeOutputPath = function(id) {
      removeOutputPath(self, id)
    },

    #' @description Add observed data programmatically.
    #' @param entry Either a `DataSet` object or a list with observedData
    #'   config (see [addObservedData()]).
    addObservedData = function(entry) {
      addObservedData(project = self, entry = entry)
    },

    #' @description Remove observed data programmatically.
    #' @param name DataSet name or config entry file basename.
    removeObservedData = function(name) {
      removeObservedData(project = self, name = name)
    },

    #' @description Add a DataCombined programmatically.
    #' @param name DataCombined name.
    #' @param simulated List of simulated entry lists.
    #' @param observed List of observed entry lists.
    addDataCombined = function(name, simulated = list(), observed = list()) {
      addDataCombined(
        project = self,
        name = name,
        simulated = simulated,
        observed = observed
      )
    },

    #' @description Remove a DataCombined programmatically.
    #' @param name DataCombined name.
    removeDataCombined = function(name) {
      removeDataCombined(project = self, name = name)
    },

    #' @description Add a plot configuration programmatically.
    #' @param plotID Unique plot identifier.
    #' @param dataCombinedName DataCombined the plot draws from.
    #' @param plotType One of the supported plot types.
    #' @param ... Optional plot-configuration fields.
    addPlot = function(plotID, dataCombinedName, plotType, ...) {
      addPlot(
        project = self,
        plotID = plotID,
        dataCombinedName = dataCombinedName,
        plotType = plotType,
        ...
      )
    },

    #' @description Remove a plot configuration programmatically.
    #' @param plotID Plot identifier.
    removePlot = function(plotID) {
      removePlot(project = self, plotID = plotID)
    },

    #' @description Add a plot grid programmatically.
    #' @param name Plot-grid name.
    #' @param plotIDs Character vector of plot IDs.
    #' @param ... Optional plot-grid fields.
    addPlotGrid = function(name, plotIDs, ...) {
      addPlotGrid(
        project = self,
        name = name,
        plotIDs = plotIDs,
        ...
      )
    },

    #' @description Remove a plot grid programmatically.
    #' @param name Plot-grid name.
    removePlotGrid = function(name) {
      removePlotGrid(project = self, name = name)
    },

    #' @description Add a PI task programmatically. Delegates to
    #'   [addPITask()].
    #' @param id PI task id (character scalar).
    #' @param scenarios Character vector of scenario names.
    #' @param parameters List of `PIParameter` records.
    #' @param outputMappings List of `PIOutputMapping` records.
    #' @param configuration Named list. See [addPITask()] for the shape.
    addPITask = function(
      id,
      scenarios,
      parameters,
      outputMappings,
      configuration = list()
    ) {
      addPITask(
        project = self,
        id = id,
        scenarios = scenarios,
        parameters = parameters,
        outputMappings = outputMappings,
        configuration = configuration
      )
    },

    #' @description Remove a PI task programmatically. Delegates to
    #'   [removePITask()].
    #' @param id PI task id (character scalar).
    removePITask = function(id) {
      removePITask(project = self, id = id)
    },

    #' @description Add a PI parameter. Delegates to [addPIParameter()].
    #' @param taskId PI task id (character scalar).
    #' @param path Parameter path (character scalar).
    #' @param scenarios Character vector of scenario names.
    #' @param minValue,maxValue,startValue Numeric scalars.
    #' @param units Optional character scalar.
    #' @param id Optional character scalar.
    addPIParameter = function(
      taskId,
      path,
      scenarios,
      minValue,
      maxValue,
      startValue,
      units = NULL,
      id = NULL
    ) {
      addPIParameter(
        self,
        taskId = taskId,
        path = path,
        scenarios = scenarios,
        minValue = minValue,
        maxValue = maxValue,
        startValue = startValue,
        units = units,
        id = id
      )
    },

    #' @description Remove a PI parameter. Delegates to
    #'   [removePIParameter()].
    #' @param taskId PI task id.
    #' @param id Parameter id.
    removePIParameter = function(taskId, id) {
      removePIParameter(self, taskId = taskId, id = id)
    },

    #' @description Add a PI output mapping. Delegates to
    #'   [addPIOutputMapping()].
    #' @param taskId PI task id.
    #' @param outputPathId Output-path id (character scalar).
    #' @param observedDataId Observed-data id (character scalar).
    #' @param scenarios Character vector of scenario names.
    #' @param scaling,xOffset,yOffset,xFactor,yFactor,weight Optional
    #'   fitting metadata.
    #' @param id Optional character scalar.
    addPIOutputMapping = function(
      taskId,
      outputPathId,
      observedDataId,
      scenarios,
      scaling = NULL,
      xOffset = 0,
      yOffset = 0,
      xFactor = 1,
      yFactor = 1,
      weight = NULL,
      id = NULL
    ) {
      addPIOutputMapping(
        self,
        taskId = taskId,
        outputPathId = outputPathId,
        observedDataId = observedDataId,
        scenarios = scenarios,
        scaling = scaling,
        xOffset = xOffset,
        yOffset = yOffset,
        xFactor = xFactor,
        yFactor = yFactor,
        weight = weight,
        id = id
      )
    },

    #' @description Remove a PI output mapping. Delegates to
    #'   [removePIOutputMapping()].
    #' @param taskId PI task id.
    #' @param id Output mapping id.
    removePIOutputMapping = function(taskId, id) {
      removePIOutputMapping(self, taskId = taskId, id = id)
    },

    #' @description Print a one-section-per-line summary of the project.
    #' @param ... Unused; present for S3 method consistency.
    print = function(...) {
      cat("<Project> (schema ", private$.schemaVersion, ")\n", sep = "")
      if (!is.null(private$.jsonPath)) {
        cat("  jsonPath:        ", private$.jsonPath, "\n", sep = "")
      }
      cat(
        "  esqlabsRVersion: ",
        private$.esqlabsRVersion %||% "NA",
        "\n",
        sep = ""
      )
      cat("  scenarios:       ", length(private$.scenarios), "\n", sep = "")
      cat("  individuals:     ", length(private$.individuals), "\n", sep = "")
      cat("  populations:     ", length(private$.populations), "\n", sep = "")
      cat(
        "  modelParameterSets:       ",
        length(private$.modelParameterSets),
        " set(s)\n",
        sep = ""
      )
      cat(
        "  individualParameterSets:  ",
        length(private$.individualParameterSets),
        " set(s)\n",
        sep = ""
      )
      cat(
        "  applicationParameterSets: ",
        length(private$.applicationParameterSets),
        " set(s)\n",
        sep = ""
      )
      cat("  applications:    ", length(private$.applications), "\n", sep = "")
      cat("  outputPaths:     ", length(private$.outputPaths), "\n", sep = "")
      cat(
        "  observedData:    ",
        length(private$.observedData),
        " source(s)\n",
        sep = ""
      )
      if (is.null(private$.plots)) {
        cat("  plots:           (none)\n")
      } else {
        cat(
          "  plots:           ",
          length(private$.plots$dataCombined %||% list()),
          " dataCombined / ",
          length(private$.plots$plotConfiguration %||% list()),
          " plot(s) / ",
          length(private$.plots$plotGrids %||% list()),
          " grid(s)\n",
          sep = ""
        )
      }
      invisible(self)
    }
  ),
  private = list(
    .invalidate = function() {
      private$.modified <- TRUE
      private$.validatedSinceMutation <- FALSE
      invisible(self)
    },

    .replace_env_var = function(path) {
      # Expand $VAR / ${VAR} references in `path`. Skip the system PATH
      # variable because expanding it inside a filesystem path would
      # never be useful and is the canonical "I forgot to escape" footgun.
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
      must_work = TRUE,
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
        abs_path <- fs::path_abs(path)
      } else {
        abs_path <- fs::path_abs(file.path(parent, path))
      }
      abs_path
    },

    .schemaVersion = NULL,
    .esqlabsRVersion = NULL,
    .jsonPath = NULL,
    .projectDirPath = NULL,
    .filePaths = list(),
    .outputPaths = list(),
    .scenarios = list(),
    .modelParameterSets = list(),
    .individualParameterSets = list(),
    .applicationParameterSets = list(),
    .individuals = list(),
    .populations = list(),
    .applications = list(),
    .observedData = list(),
    .plots = NULL,
    .parameterIdentification = list(),
    .validatedSinceMutation = FALSE,
    .modified = FALSE,
    .programmaticDataSets = list(),
    .observedDataNamesCache = NULL
  )
)
