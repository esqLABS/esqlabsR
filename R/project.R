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

    #' @field outputPaths Named list mapping output-path IDs to literal output
    #'   path strings.
    outputPaths = function(value) {
      if (!missing(value)) cli::cli_abort("{.field outputPaths} is read-only.")
      private$.outputPaths
    },

    #' @field scenarios Named list of `Scenario` objects, indexed
    #'   by scenario name. Built by `.parseScenarios()` from the
    #'   raw JSON `scenarios` array; round-trips back through
    #'   `.scenariosToJson()`.
    scenarios = function(value) {
      if (!missing(value)) cli::cli_abort("{.field scenarios} is read-only.")
      private$.scenarios
    },

    #' @field modelParameters Named list keyed by parameter-set name; each
    #'   value is a list of parameter entries.
    modelParameters = function(value) {
      if (!missing(value))
        cli::cli_abort("{.field modelParameters} is read-only.")
      private$.modelParameters
    },

    #' @field individuals List of individual entries.
    individuals = function(value) {
      if (!missing(value)) cli::cli_abort("{.field individuals} is read-only.")
      private$.individuals
    },

    #' @field populations List of population entries.
    populations = function(value) {
      if (!missing(value)) cli::cli_abort("{.field populations} is read-only.")
      private$.populations
    },

    #' @field applications Named list keyed by application-protocol name.
    applications = function(value) {
      if (!missing(value)) cli::cli_abort("{.field applications} is read-only.")
      private$.applications
    },

    #' @field observedData List of observed-data source entries.
    observedData = function(value) {
      if (!missing(value)) cli::cli_abort("{.field observedData} is read-only.")
      private$.observedData
    },

    #' @field plots Named list with sub-entries `dataCombined`,
    #'   `plotConfiguration`, and `plotGrids`. `NULL` if the JSON omits the
    #'   `plots` section. Each `plotGrids[[i]]$plotIDs` is a single
    #'   comma-separated string (e.g. `"P1, P2"`), not a JSON array; this
    #'   matches the v2.0 schema and the existing Excel `Plots` sheet
    #'   convention. Splitting/normalising is deferred to the plots chapter.
    plots = function(value) {
      if (!missing(value)) cli::cli_abort("{.field plots} is read-only.")
      private$.plots
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
    #' @param modelParameters Named list of parameter sets.
    #' @param individuals List of individual entries.
    #' @param populations List of population entries.
    #' @param applications Named list of application-protocol entries.
    #' @param observedData List of observed-data source entries.
    #' @param plots Named list of plot sub-sections, or `NULL`.
    initialize = function(
      schemaVersion,
      esqlabsRVersion,
      jsonPath,
      projectDirPath,
      filePaths,
      outputPaths,
      scenarios,
      modelParameters,
      individuals,
      populations,
      applications,
      observedData,
      plots
    ) {
      private$.schemaVersion <- schemaVersion
      private$.esqlabsRVersion <- esqlabsRVersion
      private$.jsonPath <- jsonPath
      private$.projectDirPath <- projectDirPath
      private$.filePaths <- filePaths
      private$.outputPaths <- outputPaths
      private$.scenarios <- scenarios
      private$.modelParameters <- modelParameters
      private$.individuals <- individuals
      private$.populations <- populations
      private$.applications <- applications
      private$.observedData <- observedData
      private$.plots <- plots
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
        "  modelParameters: ",
        length(private$.modelParameters),
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
        vapply(matches, function(match) {
          name <- sub(pattern, "\\1", match)
          if (identical(name, "PATH")) {
            return(match)
          }
          val <- Sys.getenv(name, unset = NA)
          if (is.na(val)) match else val
        }, character(1))
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
    .modelParameters = list(),
    .individuals = list(),
    .populations = list(),
    .applications = list(),
    .observedData = list(),
    .plots = NULL
  )
)
