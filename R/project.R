# Project R6 class ----

#' @title Project
#' @docType class
#' @description An R6 class representing an esqlabsR project
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
      if (!missing(value)) stop("projectFilePath is readonly")
      private$.projectFilePath
    },

    #' @field projectDirPath Read-only. Directory containing the JSON
    #'   configuration file (i.e. `dirname(projectFilePath)`). Used as the
    #'   base for resolving relative paths. `NULL` if the project was not
    #'   loaded from a file.
    projectDirPath = function(value) {
      if (!missing(value)) stop("projectDirPath is readonly")
      private$.projectDirPath
    },

    #' @field modified Read-only logical. `TRUE` if any configuration property
    #'   has been modified since the project was loaded or saved. Cleared
    #'   internally by [saveProject()].
    modified = function(value) {
      if (!missing(value)) stop("modified is readonly")
      private$.modified
    },

    #' @field validatedSinceMutation Read-only logical. `TRUE` if a full
    #'   [validateProject()] has succeeded since the last project mutation
    #'   or load. Cleared by any mutation. Used internally by automatic
    #'   validation hooks (e.g. in [runScenarios()] and [createPlots()]) to
    #'   skip redundant re-validation of an unchanged project.
    validatedSinceMutation = function(value) {
      if (!missing(value)) stop("validatedSinceMutation is readonly")
      private$.validatedSinceMutation
    },

    #' @field modelFolder Path to the folder containing pkml simulation files.
    modelFolder = function(value) {
      if (!missing(value)) {
        private$.filePathsData$modelFolder$value <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.clean_path(
        private$.filePathsData$modelFolder$value,
        self$projectDirPath,
        must_work = FALSE
      )
    },

    #' @field configurationsFolder Path to the folder containing configuration
    #'   files. Used by the Excel import/export bridge.
    configurationsFolder = function(value) {
      if (!missing(value)) {
        private$.filePathsData$configurationsFolder$value <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.clean_path(
        private$.filePathsData$configurationsFolder$value,
        self$projectDirPath,
        must_work = FALSE
      )
    },

    #' @field modelParamsFile Path to the Excel file with global model
    #'   parameterization. Used by the Excel import/export bridge.
    modelParamsFile = function(value) {
      if (!missing(value)) {
        private$.filePathsData$modelParamsFile$value <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.clean_path(
        private$.filePathsData$modelParamsFile$value,
        self$configurationsFolder,
        must_work = FALSE
      )
    },

    #' @field individualsFile Path to the Excel file with individual-specific
    #'   model parameterization. Used by the Excel import/export bridge.
    individualsFile = function(value) {
      if (!missing(value)) {
        private$.filePathsData$individualsFile$value <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.clean_path(
        private$.filePathsData$individualsFile$value,
        self$configurationsFolder,
        must_work = FALSE
      )
    },

    #' @field populationsFile Path to the Excel file with population
    #'   information. Used by the Excel import/export bridge.
    populationsFile = function(value) {
      if (!missing(value)) {
        private$.filePathsData$populationsFile$value <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.clean_path(
        private$.filePathsData$populationsFile$value,
        self$configurationsFolder,
        must_work = FALSE
      )
    },

    #' @field scenariosFile Path to the Excel file with scenario definitions.
    #'   Used by the Excel import/export bridge.
    scenariosFile = function(value) {
      if (!missing(value)) {
        private$.filePathsData$scenariosFile$value <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.clean_path(
        private$.filePathsData$scenariosFile$value,
        self$configurationsFolder,
        must_work = FALSE
      )
    },

    #' @field applicationsFile Path to the Excel file with scenario-specific
    #'   parameters such as application protocol parameters. Used by the
    #'   Excel import/export bridge.
    applicationsFile = function(value) {
      if (!missing(value)) {
        private$.filePathsData$applicationsFile$value <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.clean_path(
        private$.filePathsData$applicationsFile$value,
        self$configurationsFolder,
        must_work = FALSE
      )
    },

    #' @field plotsFile Path to the Excel file with plot definitions. Used by
    #'   the Excel import/export bridge.
    plotsFile = function(value) {
      if (!missing(value)) {
        private$.filePathsData$plotsFile$value <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.clean_path(
        private$.filePathsData$plotsFile$value,
        self$configurationsFolder,
        must_work = FALSE
      )
    },

    #' @field populationsFolder Name of the folder containing population
    #'   definitions as CSV files. Resolved relative to `projectDirPath`.
    #'   Used by `runScenarios()` to load population CSVs at simulation time.
    populationsFolder = function(value) {
      if (!missing(value)) {
        private$.filePathsData$populationsFolder$value <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.clean_path(
        private$.filePathsData$populationsFolder$value,
        self$projectDirPath,
        must_work = FALSE
      )
    },

    #' @field dataFolder Path to the folder where experimental data files are
    #'   located.
    dataFolder = function(value) {
      if (!missing(value)) {
        private$.filePathsData$dataFolder$value <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.clean_path(
        private$.filePathsData$dataFolder$value,
        self$projectDirPath,
        must_work = FALSE
      )
    },

    #' @field outputFolder Path to the folder where the results should be saved
    #'   relative to the "Code" folder
    outputFolder = function(value) {
      if (!missing(value)) {
        private$.filePathsData$outputFolder$value <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.clean_path(
        private$.filePathsData$outputFolder$value,
        self$projectDirPath,
        must_work = FALSE
      )
    },

    #' @field filePaths Read-only named list of declared file/folder paths
    #'   (the `filePaths` JSON section). Values are returned verbatim as
    #'   strings; no resolution is performed at this stage.
    filePaths = function(value) {
      if (!missing(value)) stop("filePaths is readonly")
      data <- private$.filePathsData
      if (length(data) == 0L) {
        return(structure(list(), names = character(0L)))
      }
      lapply(data, function(entry) entry$value)
    },

    #' @field asList Returns the current project as a list matching the JSON
    #'   schema. Reflects any in-memory modifications. Read-only.
    asList = function(value) {
      if (!missing(value)) stop("asList is readonly")
      .projectToJson(self)
    }
  ),
  public = list(
    #' @field schemaVersion Schema version declared in the JSON. Always "2.0"
    #'   for projects loaded by this parser.
    schemaVersion = NULL,

    #' @field esqlabsRVersion Informational version string from the JSON.
    esqlabsRVersion = NULL,

    #' @field jsonPath Absolute path the project was loaded from, or `NULL`.
    jsonPath = NULL,

    #' @field outputPaths Named character vector. Names are IDs, values are
    #'   output path strings.
    outputPaths = NULL,

    #' @field scenarios Named list of `Scenario` objects, keyed by scenario
    #'   name. Populated by JSON loading.
    scenarios = NULL,

    #' @field modelParameterSets Named list of parameter structures, keyed by
    #'   set name.
    modelParameterSets = NULL,

    #' @field individualParameterSets Named list of parameter structures,
    #'   keyed by set name.
    individualParameterSets = NULL,

    #' @field applicationParameterSets Named list of parameter structures,
    #'   keyed by set name.
    applicationParameterSets = NULL,

    #' @field individuals Named list of plain lists, keyed by individualId.
    individuals = NULL,

    #' @field populations Named list of plain lists, keyed by populationId.
    populations = NULL,

    #' @field applications Named list of parameter structures, keyed by
    #'   protocol name.
    applications = NULL,

    #' @field observedData List of observed data source declarations parsed from
    #'   JSON.
    observedData = NULL,

    #' @field plots List with 3 elements: `dataCombined`, `plotConfiguration`,
    #'   `plotGrids`.
    plots = NULL,

    #' @field parameterIdentification Named list keyed by PI task id; each
    #'   entry is a `PITask` record. May be `NULL` or an empty list when
    #'   the project declares no PI tasks.
    parameterIdentification = list(),

    #' @description Construct a `Project` from a JSON file path, or create an
    #'   empty in-memory project when called with no arguments.
    #'
    #' @param projectFilePath A string representing the path to the project
    #'   JSON file.
    initialize = function(projectFilePath = character()) {
      private$.modified <- FALSE
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
        stop(messages$invalidPathArgument(), call. = FALSE)
      }
      private$.read_json(projectFilePath)
      invisible(self)
    },

    #' @description Internal method to clear the `modified` flag after saving.
    #'   Not intended for end-user use.
    #' @keywords internal
    .markSaved = function() {
      private$.modified <- FALSE
      private$.validatedSinceMutation <- FALSE
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
    #'   programmatic change. Sets the `modified` flag and clears the
    #'   `validatedSinceMutation` flag. Not intended for end-user use.
    #' @keywords internal
    .markModified = function() {
      private$.invalidate()
      invisible(self)
    },

    #' @description Internal method to retrieve the raw filePaths metadata
    #'   (a named list of `list(value, description)` entries). Not intended for
    #'   end-user use; consumed by the Excel import/export bridge.
    #' @keywords internal
    .getFilePathsData = function() {
      private$.filePathsData
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

    #' @description Check synchronization status of a Project.
    #' @param silent Logical. If `TRUE`, suppresses informational messages.
    #'   Defaults to `FALSE`.
    sync = function(silent = FALSE) {
      .projectSync(self, silent = silent)
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
        "  modelParameterSets:       ",
        length(self$modelParameterSets),
        " set(s)\n",
        sep = ""
      )
      cat(
        "  individualParameterSets:  ",
        length(self$individualParameterSets),
        " set(s)\n",
        sep = ""
      )
      cat(
        "  applicationParameterSets: ",
        length(self$applicationParameterSets),
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
      if (is.null(self$plots)) {
        cat("  plots:           (none)\n")
      } else {
        cat(
          "  plots:           ",
          length(self$plots$dataCombined %||% list()),
          " dataCombined / ",
          length(self$plots$plotConfiguration %||% list()),
          " plot(s) / ",
          length(self$plots$plotGrids %||% list()),
          " grid(s)\n",
          sep = ""
        )
      }
      invisible(self)
    }
  ),
  private = list(
    .projectFilePath = NULL,
    .projectDirPath = NULL,
    .modified = FALSE,
    .validatedSinceMutation = FALSE,
    .filePathsData = list(),
    .programmaticDataSets = list(),
    .observedDataNamesCache = NULL,
    .warned_paths = character(),

    .invalidate = function() {
      private$.modified <- TRUE
      private$.validatedSinceMutation <- FALSE
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
      if (!fs::file_exists(abs_path) && must_work == TRUE) {
        if (!(abs_path %in% private$.warned_paths)) {
          warning(messages$fileNotFound(abs_path))
          private$.warned_paths <- c(private$.warned_paths, abs_path)
        }
      }
      abs_path
    },

    .read_json = function(jsonPath) {
      jsonPath <- fs::path_abs(jsonPath)
      if (!fs::file_exists(jsonPath)) stop(messages$fileNotFound(jsonPath))
      jsonData <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
      if (!identical(jsonData$schemaVersion, "2.0")) {
        stop(
          "Unsupported schemaVersion: ",
          format(jsonData$schemaVersion %||% "<missing>"),
          ". Expected '2.0'."
        )
      }
      self$schemaVersion <- jsonData$schemaVersion
      self$esqlabsRVersion <- jsonData$esqlabsRVersion
      self$jsonPath <- jsonPath
      private$.projectFilePath <- jsonPath
      private$.projectDirPath <- dirname(jsonPath)

      fp <- jsonData$filePaths %||% list()
      private$.filePathsData <- list()
      for (n in names(fp)) {
        private$.filePathsData[[n]] <- list(value = fp[[n]], description = "")
      }

      self$outputPaths <- jsonData$outputPaths %||% list()
      self$modelParameterSets <- jsonData$modelParameterSets %||% list()
      self$individualParameterSets <- jsonData$individualParameterSets %||%
        list()
      self$applicationParameterSets <- jsonData$applicationParameterSets %||%
        list()
      self$individuals <- .parseIndividuals(jsonData$individuals)
      self$populations <- .parsePopulations(jsonData$populations)
      self$applications <- .parseApplications(jsonData$applications)
      self$scenarios <- .parseScenarios(jsonData$scenarios, self$outputPaths)
      self$observedData <- jsonData$observedData %||% list()
      self$plots <- .parsePlots(jsonData$plots)
      self$parameterIdentification <- .parsePITasks(
        jsonData$parameterIdentification
      )

      private$.modified <- FALSE
      private$.validatedSinceMutation <- FALSE
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
  lifecycle::deprecate_warn("7.0.0", "ProjectConfiguration()", "Project$new()")
  Project$new(projectFilePath = projectConfigurationFilePath)
}
