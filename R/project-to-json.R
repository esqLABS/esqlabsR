# v2.0 Project → JSON serialization (internal) ----
#
# Inverse of `Project$.read_json()` (called by `Project$new()`). Walks a
# `Project` and emits a v2.0 `Project.json` file. The contract is:
#
#   loadProject(path) |> saveProject(out)  produces a JSON file that, when
#   re-loaded, yields a `Project` structurally identical to the first one.
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
#' Not exported. Companion to `Project$.read_json()`. The list returned here
#' is the canonical input to `jsonlite::write_json` (see `.saveProjectJson()`);
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
    modelParameterSets = .modelParameterSetsToJson(project),
    individuals = .individualsToJson(project),
    individualParameterSets = .individualParameterSetsToJson(project),
    populations = .populationsToJson(project),
    applications = .applicationsToJson(project),
    applicationParameterSets = .applicationParameterSetsToJson(project),
    plots = .plotsToJson(project),
    parameterIdentification = .parameterIdentificationToJson(project)
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
  if (
    !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)
  ) {
    stop(messages$invalidPathArgument(), call. = FALSE)
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
# Each helper is paired with a `.parse<Section>()` (called from
# `Project$.read_json()`) and is the canonical place for that section's
# JSON-shape concerns. Today most helpers are trivial because the parser is
# JSON-faithful; the bodies will grow as section-specific transformations
# move here from caller code (e.g. relative-path normalization, ID
# dereferencing, unit conversions).

# JSON object. Walks the raw `{value, description}` records in
# `.getFilePathsData()` and emits a flat `{name: value}` map.
.filePathsToJson <- function(project) {
  data <- project$.getFilePathsData()
  if (length(data) == 0L) return(setNames(list(), character(0)))
  result <- lapply(data, function(entry) entry$value)
  .asJsonObject(result)
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

  unname(lapply(scenarios, function(sc) {
    # Default to `list()` so the JSON output is `[]` when the scenario
    # has no resolved paths (whether the JSON had `outputPathIds: []`,
    # omitted the key, or `sc$outputPaths` was set to `NULL`
    # programmatically). Round-trip preserves array-shape symmetry with
    # the parser, which collapses absent and empty-array to `NULL`.
    outputPathIds <- list()
    if (!is.null(sc$outputPaths)) {
      pathIds <- names(sc$outputPaths)
      if (is.null(pathIds) || any(pathIds == "")) {
        cli::cli_abort(
          c(
            "Scenario {.val {sc$scenarioName}} has {.field outputPaths} without ids.",
            "i" = "Expected a named character vector: id-as-name, literal-path-as-value."
          )
        )
      }
      unknown <- setdiff(pathIds, names(project$outputPaths))
      if (length(unknown) > 0) {
        cli::cli_abort(
          "Scenario {.val {sc$scenarioName}} references unknown outputPathIds: {.val {unknown}}."
        )
      }
      outputPathIds <- as.list(pathIds)
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

    if (sc$simulateSteadyState && is.null(sc$steadyStateTimeUnit)) {
      stop(
        "Scenario '",
        sc$scenarioName,
        "' has simulateSteadyState=TRUE but steadyStateTimeUnit is NULL. ",
        "Set steadyStateTimeUnit (e.g. \"min\") so the value can round-trip.",
        call. = FALSE
      )
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
      # `as.list(NULL)` -> `list()`; this collapses both "key absent" and
      # "empty array" in the parsed scenario to JSON `[]`. Matches the
      # end-state serializer in `json-as-primary-input-v2`.
      modelParameterSets = as.list(sc$modelParameterSets),
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
.modelParameterSetsToJson <- function(project) {
  .asJsonObject(project$modelParameterSets)
}

# JSON object (map of parameter-set name → array of parameter entries).
.individualParameterSetsToJson <- function(project) {
  .asJsonObject(project$individualParameterSets)
}

# JSON object (map of parameter-set name → array of parameter entries).
.applicationParameterSetsToJson <- function(project) {
  .asJsonObject(project$applicationParameterSets)
}

# JSON array of individual objects. The in-memory shape is a named list
# keyed by `individualId`; serialization re-attaches that key as the
# `individualId` field on each entry.
.individualsToJson <- function(project) {
  individuals <- project$individuals
  if (is.null(individuals) || length(individuals) == 0L) {
    return(list())
  }
  unname(lapply(names(individuals), function(id) {
    indiv <- individuals[[id]]
    entry <- list(individualId = id)
    for (field in c("species", "population", "gender", "proteinOntogenies")) {
      if (!is.null(indiv[[field]])) entry[[field]] <- indiv[[field]]
    }
    for (field in c("weight", "height", "age")) {
      val <- indiv[[field]]
      if (length(val) > 0L && !is.na(val)) {
        entry[[field]] <- as.double(val)
      }
    }
    if (!is.null(indiv$parameterSets) && length(indiv$parameterSets) > 0L) {
      entry$parameterSets <- as.list(indiv$parameterSets)
    }
    entry
  }))
}

# JSON array of population objects. The in-memory shape is a named list
# keyed by `populationId`; serialization re-attaches that key.
.populationsToJson <- function(project) {
  populations <- project$populations
  if (is.null(populations) || length(populations) == 0L) {
    return(list())
  }
  unname(lapply(names(populations), function(id) {
    pop <- populations[[id]]
    c(list(populationId = id), unclass(pop))
  }))
}

# JSON object (map of protocol name → application object). Strips the
# `Application` class attribute and emits `{}` for entries with no
# parameter-set references (matches the v2.0 schema "object with optional
# parameterSets" shape).
.applicationsToJson <- function(project) {
  applications <- project$applications
  if (is.null(applications) || length(applications) == 0L) {
    return(structure(list(), names = character(0L)))
  }
  result <- list()
  for (id in names(applications)) {
    app <- applications[[id]]
    entry <- list()
    if (!is.null(app$parameterSets) && length(app$parameterSets) > 0L) {
      entry$parameterSets <- as.list(app$parameterSets)
    }
    result[[id]] <- entry
  }
  result
}

# JSON array of observed-data source entries.
.observedDataToJson <- function(project) {
  project$observedData
}

# `.plotsToJson` and its data-shape helpers (`.dataCombinedToNestedJson`,
# `.dataFrameToListOfLists`) live in R/plots.R alongside the plots section
# parse + validate + mutation API.

# Shape-coercion helper -------------------------------------------------------

# `list()` is ambiguous in JSON: jsonlite renders an empty list as `[]`, but
# the schema requires `{}` for the map-shaped sections (`filePaths`,
# `outputPaths`, `applications`, `modelParameterSets`,
# `individualParameterSets`, `applicationParameterSets`). Setting a
# zero-length names attribute triggers jsonlite's named-list serialization
# path.
.asJsonObject <- function(x) {
  if (length(x) == 0L) {
    return(structure(list(), names = character(0L)))
  }
  x
}

# Serialize Project$parameterIdentification (a named list of PITask
# records) back to the JSON-shaped list. Returns NULL when there are no
# PI tasks, matching the parser-symmetric "absent section" shape used
# by other sections.
#
# @keywords internal
# @noRd
.parameterIdentificationToJson <- function(project) {
  tasks <- project$parameterIdentification
  if (is.null(tasks) || length(tasks) == 0L) {
    return(NULL)
  }
  lapply(tasks, function(task) {
    list(
      id = task$id,
      scenarios = as.list(task$scenarios),
      parameters = lapply(task$parameters, .piParameterToJson),
      outputMappings = lapply(task$outputMappings, .piOutputMappingToJson),
      configuration = task$configuration
    )
  }) |>
    unname()
}

# @keywords internal
# @noRd
.piParameterToJson <- function(p) {
  list(
    id = p$id,
    scenarios = as.list(p$scenarios),
    path = p$path,
    units = p$units,
    minValue = p$minValue,
    maxValue = p$maxValue,
    startValue = p$startValue
  )
}

# @keywords internal
# @noRd
.piOutputMappingToJson <- function(m) {
  list(
    id = m$id,
    scenarios = as.list(m$scenarios),
    outputPathId = m$outputPathId,
    observedDataId = m$observedDataId,
    scaling = m$scaling,
    xOffset = m$xOffset,
    yOffset = m$yOffset,
    xFactor = m$xFactor,
    yFactor = m$yFactor,
    weight = m$weight
  )
}
