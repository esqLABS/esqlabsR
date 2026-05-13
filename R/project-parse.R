# v2.0 Project.json per-section parsers (internal) ----
#
# Helpers called by `Project$.read_json()` to turn raw JSON sections into the
# in-memory shape `Project` exposes. JSON-faithful: each section ends up as a
# plain list (or named list) shaped the way `jsonlite::fromJSON(...,
# simplifyVector = FALSE)` produces it. No coercion, no validation beyond the
# schema-version guard, no cross-reference resolution.

# Parse the `individuals` JSON array into a named list keyed by
# `individualId`. Per-entry numeric fields are coerced via `as.double`.
# Each entry is stamped with `class = c("Individual", "list")` to enable
# S3 dispatch on individual objects.
#
# @keywords internal
# @noRd
.parseIndividuals <- function(individualsData) {
  if (is.null(individualsData) || length(individualsData) == 0L) {
    return(list())
  }
  result <- list()
  for (entry in individualsData) {
    indiv <- list()
    if (!is.null(entry$species)) indiv$species <- entry$species
    for (field in c("population", "gender", "proteinOntogenies")) {
      if (!is.null(entry[[field]])) indiv[[field]] <- entry[[field]]
    }
    for (field in c("weight", "height", "age")) {
      if (!is.null(entry[[field]])) {
        indiv[[field]] <- as.double(entry[[field]])
      }
    }
    if (!is.null(entry$parameterSets)) {
      indiv$parameterSets <- as.character(unlist(entry$parameterSets))
    }
    class(indiv) <- c("Individual", "list")
    result[[entry$individualId]] <- indiv
  }
  result
}

# Parse the `populations` JSON array into a named list keyed by
# `populationId`. Numeric fields are coerced via `as.double`. Each entry
# is stamped with `class = c("Population", "list")` to enable S3 dispatch.
#
# @keywords internal
# @noRd
.parsePopulations <- function(populationsData) {
  if (is.null(populationsData) || length(populationsData) == 0L) {
    return(list())
  }
  numericFields <- c(
    "numberOfIndividuals",
    "proportionOfFemales",
    "weightMin",
    "weightMax",
    "heightMin",
    "heightMax",
    "ageMin",
    "ageMax",
    "BMIMin",
    "BMIMax"
  )
  result <- list()
  for (entry in populationsData) {
    popData <- list()
    for (field in names(entry)) {
      if (field == "populationId") next
      val <- entry[[field]]
      if (is.null(val)) next
      if (field %in% numericFields) {
        val <- as.double(val)
      }
      popData[[field]] <- val
    }
    class(popData) <- c("Population", "list")
    result[[entry$populationId]] <- popData
  }
  result
}

# Parse the `applications` JSON object. Each entry is stamped with
# `class = c("Application", "list")`. The current schema stores
# applications as a map of name -> object containing only
# `parameterSets`. The map is preserved verbatim except for the class
# attribute and a coercion of `parameterSets` to character.
#
# @keywords internal
# @noRd
.parseApplications <- function(appsData) {
  if (is.null(appsData) || length(appsData) == 0L) {
    return(structure(list(), names = character(0L)))
  }
  result <- list()
  for (id in names(appsData)) {
    entry <- appsData[[id]]
    app <- list()
    if (!is.null(entry$parameterSets)) {
      app$parameterSets <- as.character(unlist(entry$parameterSets))
    }
    class(app) <- c("Application", "list")
    result[[id]] <- app
  }
  result
}

# Parse the `plots` JSON section into the asymmetric in-memory shape:
#   * dataCombined   : named list keyed by name (drops the redundant `name`
#                      field on each entry).
#   * plotConfiguration / plotGrids : data.frames, NA-padded across rows.
# Returns NULL when the JSON omits the `plots` section.
#
# @keywords internal
# @noRd
.parsePlots <- function(plotsData) {
  if (is.null(plotsData)) {
    return(NULL)
  }
  list(
    dataCombined = .parseNestedDataCombined(plotsData$dataCombined),
    plotConfiguration = .listOfListsToDataFrame(plotsData$plotConfiguration),
    plotGrids = .listOfListsToDataFrame(plotsData$plotGrids)
  )
}

# Drop the redundant `name` field (it becomes the list key) and re-key the
# list by name. Per-entry sub-lists (`simulated`, `observed`) pass through
# verbatim so adding optional fields at the JSON level does not require a
# code change here.
#
# @keywords internal
# @noRd
.parseNestedDataCombined <- function(nestedData) {
  if (is.null(nestedData) || length(nestedData) == 0) {
    return(list())
  }
  result <- list()
  for (dc in nestedData) {
    result[[dc$name]] <- list(
      simulated = dc$simulated %||% list(),
      observed = dc$observed %||% list()
    )
  }
  result
}

# Convert a list of named lists (a JSON array of objects) to a data.frame,
# padding missing fields across rows with NA.
#
# @keywords internal
# @noRd
.listOfListsToDataFrame <- function(data) {
  if (is.null(data) || length(data) == 0) {
    return(data.frame())
  }
  allCols <- unique(unlist(lapply(data, names)))
  rows <- lapply(data, function(entry) {
    row <- lapply(allCols, function(col) {
      val <- entry[[col]]
      if (is.null(val)) NA else val
    })
    names(row) <- allCols
    as.data.frame(row, stringsAsFactors = FALSE)
  })
  as.data.frame(dplyr::bind_rows(rows))
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
    if (!is.null(entry$modelParameterSets)) {
      sc$modelParameterSets <- unlist(entry$modelParameterSets)
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

# Parse the `parameterIdentification` JSON array into a named list keyed
# by task id. Each entry becomes a `PITask` containing a list of
# `PIParameter` and a list of `PIOutputMapping` records. Returns an
# empty list when the section is absent or empty.
#
# @keywords internal
# @noRd
.parsePITasks <- function(piData) {
  if (is.null(piData) || length(piData) == 0L) {
    return(list())
  }
  result <- list()
  for (rawTask in piData) {
    parameters <- .parsePIParameters(rawTask$parameters %||% list(), rawTask$id)
    outputMappings <- .parsePIOutputMappings(
      rawTask$outputMappings %||% list(),
      rawTask$id
    )
    task <- PITask(
      id = rawTask$id,
      scenarios = as.character(unlist(rawTask$scenarios %||% list())),
      parameters = parameters,
      outputMappings = outputMappings,
      configuration = rawTask$configuration %||% list()
    )
    result[[rawTask$id]] <- task
  }
  result
}

# @keywords internal
# @noRd
.parsePIParameters <- function(rawList, taskId) {
  out <- vector("list", length(rawList))
  for (i in seq_along(rawList)) {
    raw <- rawList[[i]]
    id <- raw$id %||% paste0(taskId, "_param_", i)
    out[[i]] <- PIParameter(
      id = id,
      scenarios = as.character(unlist(raw$scenarios %||% list())),
      path = raw$path,
      units = raw$units,
      minValue = raw$minValue,
      maxValue = raw$maxValue,
      startValue = raw$startValue
    )
  }
  out
}

# @keywords internal
# @noRd
.parsePIOutputMappings <- function(rawList, taskId) {
  out <- vector("list", length(rawList))
  for (i in seq_along(rawList)) {
    raw <- rawList[[i]]
    id <- raw$id %||% paste0(taskId, "_mapping_", i)
    out[[i]] <- PIOutputMapping(
      id = id,
      scenarios = as.character(unlist(raw$scenarios %||% list())),
      outputPathId = raw$outputPathId,
      observedDataId = raw$observedDataId,
      scaling = raw$scaling,
      xOffset = raw$xOffset %||% 0,
      yOffset = raw$yOffset %||% 0,
      xFactor = raw$xFactor %||% 1,
      yFactor = raw$yFactor %||% 1,
      weight = raw$weight
    )
  }
  out
}
