# v2.0 Project → JSON serialization (internal) ----
#
# Inverse of `Project$.read_json()` (called by `Project$new()`). Walks a
# `Project` and emits a v2.0 `Project.json` file. The contract is:
#
#   loadProject(path) |> saveSnapshot(out)  produces a JSON file that, when
#   re-loaded, yields a `Project` structurally identical to the first one.
#
# Layered to mirror the end-state shape: a top-level `.projectToJson()`
# delegates to per-section helpers (`.filePathsToJson`, `.scenariosToJson`,
# etc.). Today every per-section helper is essentially a passthrough — the
# parser stores each section JSON-faithfully, so there is no transformation
# to perform on the way out. The seams exist so future migrations
# (relative-path resolution in `.filePathsToJson`, the named-vector
# `outputPaths` record field rewritten to the `outputPaths` id array in
# `.scenariosToJson`, unit conversions, plot nesting, ...) can land in one
# section at a time without rearranging the top-level call shape.

#' Internal: render a `Project` to a JSON-shaped R list in the v2.0 schema.
#'
#' Not exported. Companion to `Project$.read_json()`. The list returned here
#' is the canonical input to `jsonlite::write_json` (see `.saveProjectJson()`);
#' writing and re-parsing it yields a structurally identical `Project`.
#'
#' @param project A `Project` (R6) instance.
#' @param includeScenarios Logical. When `TRUE` (default), the `scenarios`
#'   array is inlined (the self-contained snapshot shape). When `FALSE`,
#'   `scenarios` is emitted as an empty array, because scenarios are
#'   persisted as a definition tree alongside the container, not inline. Ignored
#'   when `containerOnly` is `TRUE` (every section is emptied then).
#' @param containerOnly Logical. When `TRUE`, every tree-owned section
#'   (scenarios, individuals, populations, parameterSets, initialConditions,
#'   applications, outputPaths, observedData, dataCombined, plots, plotGrids,
#'   parameterIdentification) is emitted in its canonical empty shape rather
#'   than serialized, leaving only the container itself (metadata, filePaths,
#'   defaultSimulationRunOptions, excel). This is the on-disk `Project.json`
#'   container shape: the `definitions/<kind>/` tree owns every section and
#'   wins on reload, so re-serializing the sections here is wasted work. When
#'   `FALSE` (default) the sections are serialized (the self-contained snapshot
#'   shape).
#'
#' @return A nested list shaped exactly the v2.0 JSON schema, ready for
#'   `jsonlite::write_json(..., auto_unbox = TRUE, null = "null")`.
#'
#' @keywords internal
#' @noRd
.projectToJson <- function(
  project,
  includeScenarios = TRUE,
  containerOnly = FALSE
) {
  if (!inherits(project, "Project")) {
    cli::cli_abort("{.arg project} must be a {.cls Project} R6 instance.")
  }

  # The container separates two path concerns: the live working folders
  # (`filePaths`) the runtime reads, and the Excel-bridge sheet names
  # (`excel`). The `excel` block is emitted only when the project actually
  # carries Excel-bridge fields (an Excel side-car exists); a from-scratch JSON
  # project omits it. The `name` / `description` metadata, `definitionsFolder`,
  # and `defaultSimulationRunOptions` are emitted only when set, so an empty
  # `Project$new()` still serializes a minimal, round-trippable file.
  #
  # `containerOnly` serializes none of the tree-owned sections: each is emitted
  # in its canonical empty shape (the same shape an empty project would yield),
  # because the `definitions/<kind>/` tree on disk owns every section and is
  # authoritative on reload. This makes a container write O(container size)
  # instead of O(sum of all section sizes).
  excel <- .excelToJson(project)
  sections <- if (containerOnly) {
    .emptyTreeSectionsJson()
  } else {
    list(
      observedData = .observedDataToJson(project),
      outputPaths = .outputPathsToJson(project),
      scenarios = if (includeScenarios) .scenariosToJson(project) else list(),
      parameterSets = .parameterSetsToJson(project),
      initialConditions = .initialConditionsToJson(project),
      individuals = .individualsToJson(project),
      populations = .populationsToJson(project),
      applications = .applicationsToJson(project),
      dataCombined = .dataCombinedSectionToJson(project),
      plots = .plotsSectionToJson(project),
      plotGrids = .plotGridsSectionToJson(project),
      parameterIdentification = .parameterIdentificationToJson(project)
    )
  }
  out <- c(
    list(
      # Default the version so an empty `Project$new()` serializes a file that
      # `loadProject()` accepts (mirrors the Excel bridge in project-excel.R).
      schemaVersion = project$schemaVersion %||% "2.0",
      esqlabsRVersion = project$esqlabsRVersion,
      name = project$name,
      description = project$description,
      definitionsFolder = project$definitionsFolder,
      filePaths = .filePathsToJson(project),
      defaultSimulationRunOptions = project$defaultSimulationRunOptions
    ),
    sections
  )
  if (length(excel) > 0L) {
    out$excel <- excel
  }
  out
}

# The canonical empty-shape JSON value for every tree-owned section, in the
# same key order `.projectToJson()` emits. Used by the container-only write
# path so a `Project.json` container holds only the container itself, the
# `definitions/<kind>/` tree owning the sections. Each value matches what the
# section's serializer returns for an empty project: the array-shaped sections
# (`observedData`, `scenarios`, `individuals`, `populations`) emit `[]`; the
# map-shaped sections (`outputPaths`, `parameterSets`, `initialConditions`,
# `applications`) emit `{}`; the sections that round-trip an absent key
# (`dataCombined`, `plots`, `plotGrids`, `parameterIdentification`) emit `NULL`.
#
# @keywords internal
# @noRd
.emptyTreeSectionsJson <- function() {
  emptyArray <- list()
  emptyObject <- structure(list(), names = character(0L))
  list(
    observedData = emptyArray,
    outputPaths = emptyObject,
    scenarios = emptyArray,
    parameterSets = emptyObject,
    initialConditions = emptyObject,
    individuals = emptyArray,
    populations = emptyArray,
    applications = emptyObject,
    dataCombined = NULL,
    plots = NULL,
    plotGrids = NULL,
    parameterIdentification = NULL
  )
}

#' Internal: write a `Project` to a `Project.json` file in the v2.0 schema.
#'
#' Not exported. `jsonlite::write_json` writes UTF-8 directly, sidestepping the
#' locale-conversion hazard that `writeLines` would carry on Windows.
#'
#' @param project A `Project` (R6) instance.
#' @param path Destination path. Parent directory must exist.
#' @param includeScenarios Logical. Passed to `.projectToJson()`: `TRUE`
#'   (default) inlines the scenarios array (snapshot shape); `FALSE` writes
#'   the container shape with scenarios held as a definition tree alongside.
#'   Ignored when `containerOnly` is `TRUE`.
#' @param containerOnly Logical. Passed to `.projectToJson()`: `TRUE` writes
#'   only the container (metadata, filePaths, defaultSimulationRunOptions,
#'   excel) with every tree-owned section emptied; `FALSE` (default)
#'   serializes the sections too.
#'
#' @return `path`, invisibly.
#'
#' @keywords internal
#' @noRd
.saveProjectJson <- function(
  project,
  path,
  includeScenarios = TRUE,
  containerOnly = FALSE
) {
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
    .projectToJson(
      project,
      includeScenarios = includeScenarios,
      containerOnly = containerOnly
    ),
    path,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE,
    digits = NA
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

# JSON object (the `filePaths` block). Walks the raw `{value, description}`
# records in `project$rawFilePaths()` (the four live working folders only) and
# emits a flat `{name: value}` map. The Excel-bridge sheet names are emitted
# separately by `.excelToJson()`.
.filePathsToJson <- function(project) {
  data <- project$rawFilePaths()
  if (length(data) == 0L) {
    return(stats::setNames(list(), character(0)))
  }
  result <- lapply(data, function(entry) entry$value)
  .asJsonObject(result)
}

# JSON object (the `excel` block) or an empty list when the project has no
# Excel side-car. Walks the raw `{value, description}` records in
# `project$rawExcel()` (the Excel-bridge sheet-name fields) and emits a flat
# `{name: value}` map. Returns `list()` (length 0) when there are no fields, so
# `.projectToJson()` can omit the `excel` key entirely for a from-scratch JSON
# project.
.excelToJson <- function(project) {
  data <- project$rawExcel()
  if (length(data) == 0L) {
    return(list())
  }
  lapply(data, function(entry) entry$value)
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

# JSON array of scenario objects. A thin wrapper over the per-scenario
# serializer `.scenarioToJson()`; used by the monolithic snapshot
# (`.projectToJson()`). The definition-files writer calls `.scenarioToJson()`
# directly, one scenario per file.
.scenariosToJson <- function(project) {
  scenarios <- project$scenarios
  if (is.null(scenarios) || length(scenarios) == 0L) {
    return(list())
  }
  unname(lapply(scenarios, .scenarioToJson))
}

# Serialize one `Scenario` record to its JSON object shape. Reverses the
# parse-time transformations: the literal `outputPaths` record field rebuilt
# as the `outputPaths` id array against the project lookup, parsed
# `simulationTime` rejoined to `"a, b, c; d, e, f"`, base-unit
# `steadyStateTime` converted
# back to its declared unit. Field order matches the example fixture so
# round-trip diffs stay zero-noise. The same object is one element of the
# monolithic `scenarios` array and the entire content of one scenario
# definition file.
#
# @keywords internal
# @noRd
.scenarioToJson <- function(sc) {
  # Default to `list()` so the JSON output is `[]` when the scenario
  # has no resolved paths (whether the JSON had `outputPaths: []`,
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
    # A dangling outputPathId (one not in the project lookup) is a
    # referential issue caught lazily by the cross-reference validator,
    # not a serialization error; the id round-trips verbatim.
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
    individual = sc$individualId,
    # Emit the population id verbatim rather than keying off the derived
    # `simulationType`, so a drifted record (populationId set while the
    # type reads "Individual") does not silently lose its population.
    population = sc$populationId,
    readPopulationFromCSV = sc$readPopulationFromCSV,
    # `as.list(NULL)` -> `list()`; this collapses both "key absent" and
    # "empty array" in the parsed scenario to JSON `[]`. Matches the
    # end-state serializer in `json-as-primary-input-v2`.
    parameterSets = as.list(sc$modelParameterSets),
    initialConditions = as.list(sc$initialConditions),
    application = if (
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
    outputPaths = outputPathIds
  )
}

# JSON object (map of parameter-set id → array of parameter entries). The
# single unified parameter-set section; a scenario / individual / application
# all reference into it.
.parameterSetsToJson <- function(project) {
  sets <- project$parameterSets
  # Strip the `ParameterSet` class wrapper from each set's array-of-entries so
  # it never reaches JSON (the wrapper exists only for the print method).
  sets <- lapply(sets, unclass)
  .asJsonObject(sets)
}

# JSON object (map of initial-condition set id → array of `{path, value, unit}`
# records). Applied to a scenario's simulation via its `initialConditions`
# field.
.initialConditionsToJson <- function(project) {
  sets <- project$initialConditions
  # Strip the `InitialConditionSet` class wrapper from each set's array-of-
  # entries so it never reaches JSON (the wrapper exists only for the print
  # method).
  sets <- lapply(sets, unclass)
  .asJsonObject(sets)
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

# JSON array of observed-data source entries. Strips the
# `ObservedDataSource` class wrapper from each entry (carried only for the
# print method) so it never reaches JSON.
.observedDataToJson <- function(project) {
  lapply(project$observedData, unclass)
}

# The three plots-section serializers (`.dataCombinedSectionToJson`,
# `.plotsSectionToJson`, `.plotGridsSectionToJson`) and their data-shape
# helpers (`.dataCombinedToNestedJson`, `.plotEntriesToJson`) live in R/plots.R
# alongside the plots-section parse + validate + mutation API.

# Shape-coercion helper -------------------------------------------------------

# `list()` is ambiguous in JSON: jsonlite renders an empty list as `[]`, but
# the schema requires `{}` for the map-shaped sections (`filePaths`,
# `outputPaths`, `applications`, `parameterSets`). Setting a zero-length
# names attribute triggers jsonlite's named-list serialization path.
.asJsonObject <- function(x) {
  # Strip the printable DefinitionList wrapper a section accessor adds on read,
  # so the class never reaches the serialized JSON.
  x <- .unwrapDefinitionList(x)
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
    # The on-disk JSON keys are suffixless (`outputPath` / `observedData`); the
    # kept record fields keep their id-suffixed names, mirroring the parser
    # (`.parsePIOutputMappings`) and the `PIOutputMapping()` mapping seam.
    outputPath = m$outputPathId,
    observedData = m$observedDataId,
    scaling = m$scaling,
    xOffset = m$xOffset,
    yOffset = m$yOffset,
    xFactor = m$xFactor,
    yFactor = m$yFactor,
    weight = m$weight
  )
}
