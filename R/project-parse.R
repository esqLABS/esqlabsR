# v2.0 Project.json parser (internal, work-in-progress) ----
#
# Reads a v2.0 `Project.json` file from disk and returns an internal `Project`
# object. The parser is JSON-faithful: every section ends up in the `Project`
# as a plain list (or named list) shaped exactly the way `jsonlite::fromJSON`
# produces it with `simplifyVector = FALSE`. No coercion, no validation beyond
# the schema-version guard, no cross-reference resolution.
#
# Distinct from the existing v6 `ProjectConfiguration` JSON snapshot loader in
# `R/utilities-config-json.R`: that one consumes Excel-shaped snapshots
# (`column_names` / `rows`); this one consumes the new domain-typed v2.0
# schema. The two coexist because v2.0 is not yet wired into any runtime
# entry point on this branch.

#' Internal: load a v2.0 `Project.json` into a `Project` object.
#'
#' Not exported. Callers must use `esqlabsR:::.loadProjectJson()`.
#'
#' @param path Path to a `Project.json` file. Must exist and declare
#'   `schemaVersion == "2.0"`.
#'
#' @return A `Project` (R6) holding the parsed sections.
#'
#' @keywords internal
#' @noRd
.loadProjectJson <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    stop("`path` must be a single non-NA string.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("Project file does not exist: ", path, call. = FALSE)
  }

  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)

  schemaVersion <- raw$schemaVersion
  if (!identical(schemaVersion, "2.0")) {
    stop(
      "Unsupported schemaVersion: ",
      format(schemaVersion %||% "<missing>"),
      ". Expected '2.0'.",
      call. = FALSE
    )
  }

  jsonPath <- normalizePath(path, winslash = "/", mustWork = FALSE)
  projectDirPath <- dirname(jsonPath)

  Project$new(
    schemaVersion = schemaVersion,
    esqlabsRVersion = raw$esqlabsRVersion,
    jsonPath = jsonPath,
    projectDirPath = projectDirPath,
    filePaths = raw$filePaths %||% list(),
    outputPaths = raw$outputPaths %||% list(),
    scenarios = raw$scenarios %||% list(),
    modelParameters = raw$modelParameters %||% list(),
    individuals = raw$individuals %||% list(),
    populations = raw$populations %||% list(),
    applications = raw$applications %||% list(),
    observedData = raw$observedData %||% list(),
    plots = raw$plots
  )
}
