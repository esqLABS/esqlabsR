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
    cli::cli_abort("{.arg project} must be a {.cls Project} R6 instance.")
  }

  list(
    # Default the version so an empty `Project$new()` serializes a file that
    # `loadProject()` accepts (mirrors the Excel bridge in project-excel.R).
    schemaVersion = project$schemaVersion %||% "2.0",
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
    cli::cli_abort(messages$invalidPathArgument())
  }
  parent <- dirname(path)
  if (!dir.exists(parent)) {
    cli::cli_abort("Parent directory does not exist: {.path {parent}}")
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
  if (length(data) == 0L) {
    return(stats::setNames(list(), character(0)))
  }
  result <- lapply(data, function(entry) entry$value)
  .asJsonObject(result)
}

# JSON object (map of id → output path string). Coerces a named character
# vector to a list so jsonlite emits a JSON object, not an array (which
# would silently drop every id). Errors on a non-empty unnamed value.
.outputPathsToJson <- function(project) {
  outputPaths <- project$outputPaths
  if (length(outputPaths) > 0L) {
    nms <- names(outputPaths)
    if (is.null(nms) || any(nms == "")) {
      cli::cli_abort(c(
        "{.field outputPaths} must be a named map of id to path string.",
        "i" = "Found {length(outputPaths)} entr{?y/ies} without an id."
      ))
    }
    if (!is.list(outputPaths)) {
      outputPaths <- as.list(outputPaths)
    }
  }
  .asJsonObject(outputPaths)
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
      cli::cli_abort(c(
        "Scenario {.val {sc$scenarioName}} has {.field simulateSteadyState}=TRUE \\
        but {.field steadyStateTimeUnit} is NULL.",
        "i" = "Set {.field steadyStateTimeUnit} (e.g. {.val min}) so the value \\
        can round-trip."
      ))
    }

    list(
      name = sc$scenarioName,
      individualId = sc$individualId,
      # Emit the populationId verbatim rather than keying off the derived
      # `simulationType`, so a drifted record (populationId set while the
      # type reads "Individual") does not silently lose its populationId.
      populationId = sc$populationId,
      readPopulationFromCSV = sc$readPopulationFromCSV,
      # `as.list(NULL)` -> `list()`; this collapses both "key absent" and
      # "empty array" in the parsed scenario to JSON `[]`. Matches the
      # end-state serializer in `json-as-primary-input-v2`.
      modelParameterSets = as.list(sc$modelParameterSets),
      initialValuesSheets = as.list(sc$initialValuesSheets),
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
      # Emit the steady-state time/unit whenever a unit is declared,
      # independently of `simulateSteadyState`. A declared time with the
      # flag off (e.g. `steadyState: false` plus a preset time) is valid
      # JSON the parser reads back, so dropping it would lose data.
      steadyStateTime = if (!is.null(sc$steadyStateTimeUnit)) {
        ospsuite::toUnit(
          quantityOrDimension = ospDimensions$Time,
          values = sc$steadyStateTime,
          targetUnit = sc$steadyStateTimeUnit
        )
      } else {
        NULL
      },
      steadyStateTimeUnit = sc$steadyStateTimeUnit,
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
# keyed by `individualId`; serialization re-attaches that key and otherwise
# passes every field through in record order (mirroring `.populationsToJson`),
# so unknown fields from a newer schema round-trip. `parameterSets` is
# emitted as a JSON array.
.individualsToJson <- function(project) {
  individuals <- project$individuals
  if (is.null(individuals) || length(individuals) == 0L) {
    return(list())
  }
  unname(lapply(names(individuals), function(id) {
    indiv <- unclass(individuals[[id]])
    if (!is.null(indiv$parameterSets)) {
      indiv$parameterSets <- as.list(indiv$parameterSets)
    }
    c(list(individualId = id), indiv)
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
