# v2.0 Project.json parser (internal) ----
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
# schema.

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
    stop(messages$invalidPathArgument(), call. = FALSE)
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

  outputPaths <- raw$outputPaths %||% list()
  scenarios <- .parseScenarios(raw$scenarios, outputPaths)

  Project$new(
    schemaVersion = schemaVersion,
    esqlabsRVersion = raw$esqlabsRVersion,
    jsonPath = jsonPath,
    projectDirPath = projectDirPath,
    filePaths = raw$filePaths %||% list(),
    outputPaths = outputPaths,
    scenarios = scenarios,
    modelParameters = raw$modelParameters %||% list(),
    individuals = raw$individuals %||% list(),
    populations = raw$populations %||% list(),
    applications = raw$applications %||% list(),
    observedData = raw$observedData %||% list(),
    plots = raw$plots
  )
}

# Internal: parse the JSON `scenarios` array into a named list of
# `Scenario` objects, keyed by scenario name.
#
# `scenariosData` is the raw `simplifyVector = FALSE` shape produced by
# `jsonlite::fromJSON()`: a list of plain named lists. `outputPaths` is
# the project-level lookup table (named list or named character vector
# of `id -> literal path`); used to resolve `outputPathIds`.
#
# This helper handles only what's needed at parse time: field copies,
# `simulationType` derivation from `populationId` presence,
# `simulationTime` string parsing, `steadyStateTime` unit conversion,
# `outputPathIds` -> literal `outputPaths` resolution. Validation
# beyond a couple of must-have parse errors is deferred to Chapter 4.
#
# @keywords internal
# @noRd
.parseScenarios <- function(scenariosData, outputPaths) {
  if (is.null(scenariosData)) {
    return(list())
  }

  result <- list()
  for (entry in scenariosData) {
    sc <- Scenario$new()
    sc$scenarioName <- entry$name
    sc$modelFile <- entry$modelFile
    sc$applicationProtocol <- entry$applicationProtocol %||% NA
    sc$individualId <- entry$individualId

    if (!is.null(entry$populationId)) {
      sc$populationId <- entry$populationId
      sc$simulationType <- "Population"
    }
    if (!is.null(entry$readPopulationFromCSV)) {
      sc$readPopulationFromCSV <- entry$readPopulationFromCSV
    }
    if (!is.null(entry$modelParameters)) {
      sc$modelParameters <- unlist(entry$modelParameters)
    }
    if (!is.null(entry$simulationTime)) {
      sc$simulationTime <- .parseSimulationTimeIntervals(
        entry$simulationTime
      )
      sc$simulationTimeUnit <- entry$simulationTimeUnit
    }
    if (isTRUE(entry$steadyState)) {
      sc$simulateSteadyState <- TRUE
    }
    if (!is.null(entry$steadyStateTime)) {
      if (is.null(entry$steadyStateTimeUnit)) {
        stop(
          "Scenario '",
          entry$name,
          "' has 'steadyStateTime' set but ",
          "'steadyStateTimeUnit' is null. Please specify a unit ",
          "(e.g. \"min\").",
          call. = FALSE
        )
      }
      sc$steadyStateTime <- ospsuite::toBaseUnit(
        quantityOrDimension = ospDimensions$Time,
        values = entry$steadyStateTime,
        unit = entry$steadyStateTimeUnit
      )
      sc$steadyStateTimeUnit <- entry$steadyStateTimeUnit
    }
    if (!is.null(entry$overwriteFormulasInSS)) {
      sc$overwriteFormulasInSS <- entry$overwriteFormulasInSS
    }

    if (!is.null(entry$outputPathIds)) {
      pathIds <- unlist(entry$outputPathIds)
      unknown <- setdiff(pathIds, names(outputPaths))
      if (length(unknown) > 0) {
        stop(
          "Scenario '",
          entry$name,
          "' references unknown outputPathIds: ",
          paste(unknown, collapse = ", "),
          call. = FALSE
        )
      }
      sc$outputPaths <- setNames(
        unlist(outputPaths[pathIds], use.names = FALSE),
        pathIds
      )
    }

    result[[entry$name]] <- sc
  }
  result
}
