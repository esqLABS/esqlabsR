# Project R6 class ----

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
      if (!missing(value))
        cli::cli_abort("{.field projectFilePath} is readonly")
      private$.projectFilePath
    },

    #' @field projectDirPath Read-only. Directory containing the JSON
    #'   configuration file (i.e. `dirname(projectFilePath)`). Used as the
    #'   base for resolving relative paths. `NULL` if the project was not
    #'   loaded from a file.
    projectDirPath = function(value) {
      if (!missing(value)) cli::cli_abort("{.field projectDirPath} is readonly")
      private$.projectDirPath
    },

    #' @field modified Read-only logical. `TRUE` if any configuration property
    #'   has been modified since the project was loaded or saved. Cleared
    #'   internally by [saveProject()].
    modified = function(value) {
      if (!missing(value)) cli::cli_abort("{.field modified} is readonly")
      private$.modified
    },

    #' @field validatedSinceMutation Read-only logical. `TRUE` if a full
    #'   [validateProject()] has succeeded since the last project mutation
    #'   or load. Cleared by any mutation. Used internally by automatic
    #'   validation hooks (e.g. in [runScenarios()] and [createPlots()]) to
    #'   skip redundant re-validation of an unchanged project.
    validatedSinceMutation = function(value) {
      if (!missing(value))
        cli::cli_abort("{.field validatedSinceMutation} is readonly")
      private$.validatedSinceMutation
    },

    #' @field schemaVersion Schema version declared in the JSON. Always "2.0"
    #'   for projects loaded by this parser. Writing marks the project as
    #'   modified.
    schemaVersion = function(value) {
      if (!missing(value)) {
        private$.schemaVersion <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.schemaVersion
    },

    #' @field esqlabsRVersion Informational version string from the JSON.
    #'   Writing marks the project as modified.
    esqlabsRVersion = function(value) {
      if (!missing(value)) {
        private$.esqlabsRVersion <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.esqlabsRVersion
    },

    #' @field jsonPath Read-only. Absolute path the project was loaded from
    #'   (an alias of `projectFilePath`), or `NULL` for an in-memory project.
    jsonPath = function(value) {
      if (!missing(value)) cli::cli_abort("{.field jsonPath} is readonly")
      private$.projectFilePath
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
        self$projectDirPath
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
        self$projectDirPath
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
        self$configurationsFolder
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
        self$configurationsFolder
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
        self$configurationsFolder
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
        self$configurationsFolder
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
        self$configurationsFolder
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
        self$configurationsFolder
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
        self$projectDirPath
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
        self$projectDirPath
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
        self$projectDirPath
      )
    },

    # Section data. Each binding stores the section in a private backing
    # field and invalidates on write, so any assignment (including
    # subscript forms like `project$scenarios[[name]] <- sc`, which R
    # desugars into a full read-modify-write through the setter) marks
    # the project modified and clears `validatedSinceMutation`.

    #' @field outputPaths Named list mapping output-path IDs to OSPS-notation
    #'   path strings (e.g. `list(PVB = "Organism|...")`). A named character
    #'   vector is also accepted on write and coerced to a list on save.
    #'   Writing marks the project as modified.
    outputPaths = function(value) {
      if (!missing(value)) {
        private$.outputPaths <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.outputPaths
    },

    #' @field scenarios Named list of `Scenario` records, keyed by scenario
    #'   name. Entries are plain-data records with copy semantics; write a
    #'   modified entry back (e.g. `project$scenarios[[name]] <- sc`) to
    #'   mutate the project. Writing marks the project as modified.
    scenarios = function(value) {
      if (!missing(value)) {
        private$.scenarios <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.scenarios
    },

    #' @field modelParameterSets Named list of parameter structures, keyed by
    #'   set name. Writing marks the project as modified.
    modelParameterSets = function(value) {
      if (!missing(value)) {
        private$.modelParameterSets <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.modelParameterSets
    },

    #' @field individualParameterSets Named list of parameter structures,
    #'   keyed by set name. Writing marks the project as modified.
    individualParameterSets = function(value) {
      if (!missing(value)) {
        private$.individualParameterSets <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.individualParameterSets
    },

    #' @field applicationParameterSets Named list of parameter structures,
    #'   keyed by set name. Writing marks the project as modified.
    applicationParameterSets = function(value) {
      if (!missing(value)) {
        private$.applicationParameterSets <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.applicationParameterSets
    },

    #' @field individuals Named list of plain lists, keyed by individualId.
    #'   Writing marks the project as modified.
    individuals = function(value) {
      if (!missing(value)) {
        private$.individuals <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.individuals
    },

    #' @field populations Named list of plain lists, keyed by populationId.
    #'   Writing marks the project as modified.
    populations = function(value) {
      if (!missing(value)) {
        private$.populations <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.populations
    },

    #' @field applications Named list of parameter structures, keyed by
    #'   protocol name. Writing marks the project as modified.
    applications = function(value) {
      if (!missing(value)) {
        private$.applications <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.applications
    },

    #' @field observedData List of observed data source declarations parsed
    #'   from JSON. Writing marks the project as modified and resets the
    #'   cached observed-data names.
    observedData = function(value) {
      if (!missing(value)) {
        private$.observedData <- value
        private$.observedDataNamesCache <- NULL
        private$.invalidate()
        return(invisible(value))
      }
      private$.observedData
    },

    #' @field plots List with 3 elements: `dataCombined`, `plotConfiguration`,
    #'   `plotGrids`. Writing marks the project as modified.
    plots = function(value) {
      if (!missing(value)) {
        private$.plots <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.plots
    },

    #' @field parameterIdentification Named list keyed by PI task id; each
    #'   entry is a `PITask` record. May be `NULL` or an empty list when
    #'   the project declares no PI tasks. Writing marks the project as
    #'   modified.
    parameterIdentification = function(value) {
      if (!missing(value)) {
        private$.parameterIdentification <- value
        private$.invalidate()
        return(invisible(value))
      }
      private$.parameterIdentification
    },

    #' @field filePaths Read-only named list of declared file/folder paths
    #'   (the `filePaths` JSON section). Values are returned verbatim as
    #'   strings; no resolution is performed at this stage.
    filePaths = function(value) {
      if (!missing(value)) cli::cli_abort("{.field filePaths} is readonly")
      data <- private$.filePathsData
      if (length(data) == 0L) {
        return(structure(list(), names = character(0L)))
      }
      lapply(data, function(entry) entry$value)
    },

    #' @field asList Returns the current project as a list matching the JSON
    #'   schema. Reflects any in-memory modifications. Read-only.
    asList = function(value) {
      if (!missing(value)) cli::cli_abort("{.field asList} is readonly")
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
        cli::cli_abort(messages$invalidPathArgument())
      }
      private$.read_json(projectFilePath)
      invisible(self)
    },

    #' @description Internal method to clear the `modified` flag after saving.
    #'   Saving is not a mutation, so it leaves `validatedSinceMutation`
    #'   untouched: a project validated before a save stays validated, and
    #'   a later `runScenarios()` / `createPlots()` need not re-validate.
    #'   Not intended for end-user use.
    #' @keywords internal
    .markSaved = function() {
      private$.modified <- FALSE
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
        # plotConfiguration / plotGrids are data frames (one row per plot /
        # grid), dataCombined is a named list; count rows or length
        # accordingly so the summary reports plot counts, not column counts.
        countSection <- function(x) {
          if (is.data.frame(x)) nrow(x) else length(x %||% list())
        }
        cat(
          "  plots:           ",
          countSection(self$plots$dataCombined),
          " dataCombined / ",
          countSection(self$plots$plotConfiguration),
          " plot(s) / ",
          countSection(self$plots$plotGrids),
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

    # Backing stores for the section-data active bindings. The parser
    # writes these directly so loading does not flip `modified`.
    .schemaVersion = NULL,
    .esqlabsRVersion = NULL,
    .outputPaths = NULL,
    .scenarios = NULL,
    .modelParameterSets = NULL,
    .individualParameterSets = NULL,
    .applicationParameterSets = NULL,
    .individuals = NULL,
    .populations = NULL,
    .applications = NULL,
    .observedData = NULL,
    .plots = NULL,
    .parameterIdentification = list(),

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
      if (!fs::file_exists(jsonPath))
        cli::cli_abort(messages$fileNotFound(jsonPath))
      jsonData <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
      if (!identical(jsonData$schemaVersion, "2.0")) {
        cli::cli_abort(
          "Unsupported schemaVersion: {.val {jsonData$schemaVersion %||% '<missing>'}}. Expected {.val 2.0}."
        )
      }
      private$.schemaVersion <- jsonData$schemaVersion
      private$.esqlabsRVersion <- jsonData$esqlabsRVersion
      private$.projectFilePath <- jsonPath
      private$.projectDirPath <- dirname(jsonPath)

      fp <- jsonData$filePaths %||% list()
      private$.filePathsData <- list()
      for (n in names(fp)) {
        private$.filePathsData[[n]] <- list(value = fp[[n]], description = "")
      }

      private$.outputPaths <- jsonData$outputPaths %||% list()
      private$.modelParameterSets <- jsonData$modelParameterSets %||% list()
      private$.individualParameterSets <- jsonData$individualParameterSets %||%
        list()
      private$.applicationParameterSets <- jsonData$applicationParameterSets %||%
        list()
      private$.individuals <- .parseIndividuals(jsonData$individuals)
      private$.populations <- .parsePopulations(jsonData$populations)
      private$.applications <- .parseApplications(jsonData$applications)
      private$.scenarios <- .parseScenarios(
        jsonData$scenarios,
        private$.outputPaths
      )
      private$.observedData <- jsonData$observedData %||% list()
      private$.plots <- .parsePlots(jsonData$plots)
      private$.parameterIdentification <- .parsePITasks(
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
  lifecycle::deprecate_warn("6.0.0", "ProjectConfiguration()", "Project$new()")
  Project$new(projectFilePath = projectConfigurationFilePath)
}
