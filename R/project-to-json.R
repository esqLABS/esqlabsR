# v2.0 Project → JSON serialization (internal, work-in-progress) ----
#
# Inverse of `.loadProjectJson()` (`R/project-parse.R`). Walks an internal
# `Project` and emits a v2.0 `Project.json` file. The contract is:
#
#   .loadProjectJson(path) |> .saveProjectJson(out)  produces a JSON file
#   that, when re-parsed, yields a `Project` structurally identical to the
#   first one.
#
# Layered to mirror the end-state shape: a top-level `.projectToJson()`
# delegates to per-section helpers (`.filePathsToJson`, `.scenariosToJson`,
# etc.). Today every per-section helper is essentially a passthrough — the
# parser stores each section JSON-faithfully, so there is no transformation
# to perform on the way out. The seams exist so future migrations
# (relative-path resolution in `.filePathsToJson`, `outputPaths` →
# `outputPathIds` rewriting in `.scenariosToJson`, unit conversions, plot
# nesting, ...) can land in one section at a time without rearranging the
# top-level call shape.

#' Internal: render a `Project` to a JSON-shaped R list in the v2.0 schema.
#'
#' Not exported. Companion to `.loadProjectJson()`. The list returned here is
#' the canonical input to `jsonlite::write_json` (see `.saveProjectJson()`);
#' writing and re-parsing it yields a structurally identical `Project`.
#'
#' @param project A `Project` (R6) instance.
#'
#' @return A nested list shaped exactly the v2.0 JSON schema, ready for
#'   `jsonlite::write_json(..., auto_unbox = TRUE, null = "null")`.
#'
#' @keywords internal
#' @noRd
.projectToJson <- function(project) {
  if (!inherits(project, "Project")) {
    stop("`project` must be a Project R6 instance.", call. = FALSE)
  }

  list(
    schemaVersion = project$schemaVersion,
    esqlabsRVersion = project$esqlabsRVersion,
    filePaths = .filePathsToJson(project),
    observedData = .observedDataToJson(project),
    outputPaths = .outputPathsToJson(project),
    scenarios = .scenariosToJson(project),
    modelParameters = .modelParametersToJson(project),
    individuals = .individualsToJson(project),
    populations = .populationsToJson(project),
    applications = .applicationsToJson(project),
    plots = .plotsToJson(project)
  )
}

#' Internal: write a `Project` to a `Project.json` file in the v2.0 schema.
#'
#' Not exported. `jsonlite::write_json` writes UTF-8 directly, sidestepping the
#' locale-conversion hazard that `writeLines` would carry on Windows.
#'
#' @param project A `Project` (R6) instance.
#' @param path Destination path. Parent directory must exist.
#'
#' @return `path`, invisibly.
#'
#' @keywords internal
#' @noRd
.saveProjectJson <- function(project, path) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    stop("`path` must be a single non-NA string.", call. = FALSE)
  }
  parent <- dirname(path)
  if (!dir.exists(parent)) {
    stop("Parent directory does not exist: ", parent, call. = FALSE)
  }

  jsonlite::write_json(
    .projectToJson(project),
    path,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )
  invisible(path)
}

# Per-section helpers ---------------------------------------------------------
#
# Each helper is paired with a `.parse<Section>()` (currently inlined in
# `.loadProjectJson`) and is the canonical place for that section's
# JSON-shape concerns. Today most helpers are trivial because the parser is
# JSON-faithful; the bodies will grow as section-specific transformations
# move here from caller code (e.g. relative-path normalization, ID
# dereferencing, unit conversions).

# JSON object. Will eventually convert absolute paths back to relative paths
# rooted at `project$projectDirPath`.
.filePathsToJson <- function(project) {
  .asJsonObject(project$filePaths)
}

# JSON object (map of id → output path string).
.outputPathsToJson <- function(project) {
  .asJsonObject(project$outputPaths)
}

# JSON array of scenario objects. Reverses the parse-time
# transformations: literal `outputPaths` rebuilt as `outputPathIds`
# against the project lookup, parsed `simulationTime` rejoined to
# `"a, b, c; d, e, f"`, base-unit `steadyStateTime` converted back to
# its declared unit. Field order matches the example fixture so
# round-trip diffs stay zero-noise.
.scenariosToJson <- function(project) {
  scenarios <- project$scenarios
  if (is.null(scenarios) || length(scenarios) == 0L) {
    return(list())
  }

  outputPathsLookup <- unlist(project$outputPaths, use.names = TRUE)

  unname(lapply(scenarios, function(sc) {
    outputPathIds <- NULL
    if (!is.null(sc$outputPaths) && length(outputPathsLookup) > 0L) {
      idx <- match(sc$outputPaths, outputPathsLookup)
      ids <- names(outputPathsLookup)[idx]
      ids <- ids[!is.na(ids)]
      if (length(ids) > 0L) outputPathIds <- as.list(ids)
    }

    simTimeStr <- NULL
    if (!is.null(sc$simulationTime)) {
      intervals <- vapply(
        sc$simulationTime,
        function(int) paste(int, collapse = ", "),
        character(1)
      )
      simTimeStr <- paste(intervals, collapse = "; ")
    }

    list(
      name = sc$scenarioName,
      individualId = sc$individualId,
      populationId = if (sc$simulationType == "Population") {
        sc$populationId
      } else {
        NULL
      },
      readPopulationFromCSV = sc$readPopulationFromCSV,
      modelParameters = if (is.null(sc$modelParameters)) {
        NULL
      } else {
        as.list(sc$modelParameters)
      },
      applicationProtocol = if (
        is.null(sc$applicationProtocol) || is.na(sc$applicationProtocol)
      ) {
        NULL
      } else {
        sc$applicationProtocol
      },
      simulationTime = simTimeStr,
      simulationTimeUnit = sc$simulationTimeUnit,
      steadyState = sc$simulateSteadyState,
      steadyStateTime = if (
        sc$simulateSteadyState && !is.null(sc$steadyStateTimeUnit)
      ) {
        ospsuite::toUnit(
          quantityOrDimension = ospDimensions$Time,
          values = sc$steadyStateTime,
          targetUnit = sc$steadyStateTimeUnit
        )
      } else {
        NULL
      },
      steadyStateTimeUnit = if (sc$simulateSteadyState) {
        sc$steadyStateTimeUnit
      } else {
        NULL
      },
      overwriteFormulasInSS = sc$overwriteFormulasInSS,
      modelFile = sc$modelFile,
      outputPathIds = outputPathIds
    )
  }))
}

# JSON object (map of parameter-set name → array of parameter entries).
.modelParametersToJson <- function(project) {
  .asJsonObject(project$modelParameters)
}

# JSON array of individual objects.
.individualsToJson <- function(project) {
  project$individuals
}

# JSON array of population objects.
.populationsToJson <- function(project) {
  project$populations
}

# JSON object (map of protocol name → application object).
.applicationsToJson <- function(project) {
  .asJsonObject(project$applications)
}

# JSON array of observed-data source entries.
.observedDataToJson <- function(project) {
  project$observedData
}

# JSON object with `dataCombined` / `plotConfiguration` / `plotGrids` arrays;
# `null` when the project carries no plots section.
.plotsToJson <- function(project) {
  project$plots
}

# Shape-coercion helper -------------------------------------------------------

# `list()` is ambiguous in JSON: jsonlite renders an empty list as `[]`, but
# the schema requires `{}` for the map-shaped sections (`filePaths`,
# `outputPaths`, `applications`, `modelParameters`). Setting a zero-length
# names attribute triggers jsonlite's named-list serialization path.
.asJsonObject <- function(x) {
  if (length(x) == 0L) {
    return(structure(list(), names = character(0L)))
  }
  x
}
