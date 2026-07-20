# Create scenarios from PKML ----

#' Create scenarios from PKML files
#'
#' @description
#' Reads scenarios from PKML files, extracting output paths and simulation
#' time settings, and adds them to a `Project` in place. Output paths are
#' registered in `outputPaths` definitions (reusing an existing id when the
#' literal path is already registered, otherwise generating a readable one),
#' and scenario names are made unique against the scenarios already on the
#' project. The function mutates `project` directly and returns it invisibly,
#' like the other `add*` mutators; the created scenario names are reported in
#' a message.
#'
#' @param pkmlFilePaths Character vector of paths to PKML files to create
#'   scenarios from. Can be a single string (recycled for all scenarios) or
#'   a vector with the same length as the number of scenarios being created
#'   (determined by the longest vector argument).
#' @param project A `Project` object holding base information.
#' @param scenarios Character vector. Optional custom names for the
#'   scenarios. If `NULL` (default), scenario names will be extracted from
#'   the simulation names in the PKML files. If provided, must have the same
#'   length as `pkmlFilePaths`.
#' @param individual Character vector. Optional individual ids to use for
#'   scenarios. If `NULL` (default), no individual will be specified. Can be
#'   a single string (recycled for all scenarios) or a vector with the same
#'   length as `pkmlFilePaths`.
#' @param population Character vector. Optional population ids to use for
#'   scenarios. If `NULL` (default), no population will be specified. If
#'   provided, sets simulation type to "Population". Can be a single string
#'   (recycled for all scenarios) or a vector with the same length as
#'   `pkmlFilePaths`.
#' @param application Character vector. Optional application protocol
#'   ids to use for scenarios, each referencing `applications` definitions. If
#'   `NULL` (default), the scenario has no application protocol (the PKML file
#'   already embeds its own application). Values are used verbatim and are
#'   validated against `applications` definitions. Can be a single string
#'   (recycled for all scenarios) or a vector with the same length as
#'   `pkmlFilePaths`.
#' @param parameterSets Character vector. Optional parameter set
#'   ids to apply to scenarios (referencing `parameterSets` definitions).
#'   If `NULL` (default), no parameter sets will be applied. Can be a
#'   single string (recycled for all scenarios) or a vector with the same
#'   length as `pkmlFilePaths`. If providing multiple set ids per scenario,
#'   separate them with commas in the string.
#' @param outputPaths Character vector or named vector. Optional output paths
#'   to use for scenarios. If `NULL` (default), output paths will be
#'   extracted from the PKML files' output selections. Can be a single string
#'   (recycled for all scenarios) or a vector with the same length as
#'   `pkmlFilePaths`. If providing multiple paths per scenario, separate them
#'   with commas in the string. Named vectors are supported where the names
#'   become the registered output-path ids, e.g.,
#'   `c("plasma" = "Organism|VenousBlood|Plasma|Drug|Concentration")`. When a
#'   literal path is already registered in `outputPaths` definitions, its existing
#'   id is reused; unnamed new paths receive a readable generated id.
#' @param simulationTime Character vector. Optional simulation time to use for
#'   scenarios as character strings containing one or multiple time intervals
#'   separated by a `';'`. Each time interval is a triplet of values
#'   `<StartTime, EndTime, Resolution>`, where `Resolution` is the number of
#'   simulated points per time unit defined in the `simulationTimeUnit`. If
#'   `NULL` (default), simulation time will be extracted from the PKML files'
#'   output schema intervals. Can be a single string (recycled for all
#'   scenarios) or a vector with the same length as `pkmlFilePaths`.
#' @param simulationTimeUnit Character vector. Optional simulation time units.
#'   Only used when `simulationTime` is provided. If `NULL` (default), will
#'   be extracted from the PKML file's output schema intervals, or set to
#'   `"min"` (minutes) if not available. Can be a single string (recycled
#'   for all scenarios) or a vector with the same length as `pkmlFilePaths`.
#' @param steadyState Logical vector. Whether to simulate steady-state for
#'   each scenario. Default is `FALSE`. Can be a single logical value
#'   (recycled for all scenarios) or a vector with the same length as
#'   `pkmlFilePaths`.
#' @param steadyStateTime Numeric vector. Steady-state times in
#'   `steadyStateTimeUnit`. If `NULL` (default), `1000` is used (matching
#'   [addScenario()]). The value is stored in base units (minutes) on the
#'   scenario. Can be a single numeric value (recycled for all scenarios) or a
#'   vector with the same length as `pkmlFilePaths`.
#' @param steadyStateTimeUnit Character vector. Steady-state time units. Only
#'   used when `steadyState = TRUE` and `steadyStateTime` is provided. If
#'   `NULL` (default), `"min"` will be used. Can be a single string (recycled
#'   for all scenarios) or a vector with the same length as `pkmlFilePaths`.
#' @param overwriteFormulasInSS Logical vector. Whether to overwrite
#'   formula-defined parameters with their steady-state values. When `TRUE`,
#'   corresponds to `ignoreIfFormula = FALSE` in `ospsuite::getSteadyState()`
#'   (formulas are overwritten). Default is `FALSE` (formula-defined
#'   parameters are kept unchanged). Can be a single logical value (recycled
#'   for all scenarios) or a vector with the same length as `pkmlFilePaths`.
#' @param readPopulationFromCSV Logical vector. Whether to read population
#'   from CSV for each scenario. Default is `FALSE`. Can be a single logical
#'   value (recycled for all scenarios) or a vector with the same length as
#'   `pkmlFilePaths`.
#' @param paramSheets `r lifecycle::badge("deprecated")` Use
#'   `parameterSets` instead.
#'
#' @details
#' This function extracts the following information from PKML files:
#' * **Output paths**: All selected outputs for the simulation from
#'   `outputSelections$allOutputs`.
#' * **Simulation time**: Time intervals with start time, end time, and
#'   resolution from `outputSchema$intervals`.
#' * **Simulation time unit**: Time unit from the output schema intervals
#'   (e.g., `"h"` for hours).
#'
#' ## Vector arguments and recycling
#'
#' All arguments support vectorization to create scenarios with different
#' parameter values:
#' * **Length 1**: The value is recycled (applied to all scenarios).
#' * **Length > 1**: All vector arguments must have the same length, which
#'   determines the number of scenarios.
#' * **Mixed lengths**: An error is thrown if vector arguments have
#'   inconsistent lengths.
#'
#' The number of scenarios created is determined by the longest vector
#' argument. All shorter vectors (including `pkmlFilePaths`) are recycled to
#' match this length.
#'
#' This allows you to efficiently create multiple scenarios in several ways:
#' * **Same PKML, different settings**: Use a single PKML file with vectors
#'   of different parameter values.
#' * **Different PKMLs, same settings**: Use multiple PKML files with single
#'   parameter values.
#' * **Different PKMLs, different settings**: Use vectors of both PKML files
#'   and parameter values.
#'
#' The function handles duplicate scenario names, both against each other and
#' against the scenarios already on the project, by appending indices (e.g.,
#' `"Scenario"`, `"Scenario_2"`).
#'
#' @returns The `project`, invisibly, with the new scenarios added to
#'   `scenarios` definitions and any new output paths registered in
#'   `outputPaths` definitions.
#' @export
#'
#' @examples
#' \dontrun{
#' # Load project
#' project <- loadProject("Project.json")
#'
#' # Read scenarios from a single PKML file into the project
#' createScenariosFromPKML(
#'   pkmlFilePaths = "path/to/simulation.pkml",
#'   project = project
#' )
#'
#' # The project now holds the new scenarios (already written through to
#' # their definition files); run them
#' results <- runScenarios(project)
#'
#' # Example of vector recycling: single value applied to all scenarios
#' createScenariosFromPKML(
#'   pkmlFilePaths = c("sim1.pkml", "sim2.pkml", "sim3.pkml"),
#'   project = project,
#'   individual = "Individual_001",
#'   steadyState = TRUE,
#'   steadyStateTime = 1000
#' )
#'
#' # Example of vector arguments: different values per scenario
#' createScenariosFromPKML(
#'   pkmlFilePaths = c("pediatric.pkml", "adult.pkml", "elderly.pkml"),
#'   project = project,
#'   scenarios = c("Pediatric", "Adult", "Elderly"),
#'   individual = c("Child_001", "Adult_001", "Elderly_001"),
#'   steadyState = c(FALSE, TRUE, TRUE),
#'   steadyStateTime = c(NA, 2000, 1500)
#' )
#' }
createScenariosFromPKML <- function(
  pkmlFilePaths,
  project,
  scenarios = NULL,
  individual = NULL,
  population = NULL,
  application = NULL,
  parameterSets = NULL,
  outputPaths = NULL,
  simulationTime = NULL,
  simulationTimeUnit = NULL,
  steadyState = FALSE,
  steadyStateTime = NULL,
  steadyStateTimeUnit = NULL,
  overwriteFormulasInSS = FALSE,
  readPopulationFromCSV = FALSE,
  paramSheets = lifecycle::deprecated()
) {
  validateIsOfType(project, "Project")
  project$createScenariosFromPKML(
    pkmlFilePaths,
    scenarios = scenarios,
    individual = individual,
    population = population,
    application = application,
    parameterSets = parameterSets,
    outputPaths = outputPaths,
    simulationTime = simulationTime,
    simulationTimeUnit = simulationTimeUnit,
    steadyState = steadyState,
    steadyStateTime = steadyStateTime,
    steadyStateTimeUnit = steadyStateTimeUnit,
    overwriteFormulasInSS = overwriteFormulasInSS,
    readPopulationFromCSV = readPopulationFromCSV,
    paramSheets = paramSheets
  )
}

# Implementation behind `project$createScenariosFromPKML()` /
# `createScenariosFromPKML()`. Its happy path composes the public authoring
# methods (`addOutputPath()` / `addScenario()`); the transactional rollback on
# failure restores the affected sections through its own `private`.
#
# @keywords internal
# @noRd
.createScenariosFromPKML_impl <- function(
  self,
  private,
  pkmlFilePaths,
  scenarios = NULL,
  individual = NULL,
  population = NULL,
  application = NULL,
  parameterSets = NULL,
  outputPaths = NULL,
  simulationTime = NULL,
  simulationTimeUnit = NULL,
  steadyState = FALSE,
  steadyStateTime = NULL,
  steadyStateTimeUnit = NULL,
  overwriteFormulasInSS = FALSE,
  readPopulationFromCSV = FALSE,
  paramSheets = lifecycle::deprecated()
) {
  # Attribute any abort to the public authoring function the user called
  # (the free-function forwarder), not this internal `_impl`.
  rlang::local_error_call(rlang::caller_env(2))
  # Handle deprecated paramSheets argument
  if (lifecycle::is_present(paramSheets)) {
    lifecycle::deprecate_soft(
      what = "createScenariosFromPKML(paramSheets)",
      with = "createScenariosFromPKML(parameterSets)",
      when = "6.0.0"
    )
    parameterSets <- parameterSets %||% paramSheets
  }

  # Validate inputs
  validateIsCharacter(pkmlFilePaths)
  if (!is.null(scenarios)) {
    validateIsCharacter(scenarios)
  }
  if (!is.null(individual)) {
    validateIsCharacter(individual)
  }
  if (!is.null(population)) {
    validateIsCharacter(population)
  }
  if (!is.null(application)) {
    validateIsCharacter(application)
  }
  if (!is.null(parameterSets)) {
    validateIsCharacter(parameterSets)
  }
  if (!is.null(outputPaths)) {
    validateIsCharacter(outputPaths)
  }
  if (!is.null(simulationTime)) {
    validateIsCharacter(simulationTime)
  }
  if (!is.null(simulationTimeUnit)) {
    validateIsCharacter(simulationTimeUnit)
  }
  validateIsLogical(steadyState)
  if (!is.null(steadyStateTime)) {
    validateIsNumeric(steadyStateTime)
  }
  if (!is.null(steadyStateTimeUnit)) {
    validateIsCharacter(steadyStateTimeUnit)
  }
  validateIsLogical(overwriteFormulasInSS)
  validateIsLogical(readPopulationFromCSV)

  # Get the number of scenarios to create based on vector arguments.
  # Note: project is excluded as it should always be a single object.
  nScenarios <- .getScenarioCount(
    pkmlFilePaths,
    scenarios,
    individual,
    population,
    application,
    parameterSets,
    outputPaths,
    simulationTime,
    simulationTimeUnit,
    steadyState,
    steadyStateTime,
    steadyStateTimeUnit,
    overwriteFormulasInSS,
    readPopulationFromCSV
  )

  # Recycle or validate all vector arguments (including pkmlFilePaths)
  pkmlFilePaths <- .recycleOrValidateVector(
    pkmlFilePaths,
    "pkmlFilePaths",
    nScenarios
  )
  scenarios <- .recycleOrValidateVector(
    scenarios,
    "scenarios",
    nScenarios
  )
  individual <- .recycleOrValidateVector(
    individual,
    "individual",
    nScenarios
  )
  population <- .recycleOrValidateVector(
    population,
    "population",
    nScenarios
  )
  application <- .recycleOrValidateVector(
    application,
    "application",
    nScenarios
  )
  parameterSets <- .recycleOrValidateVector(
    parameterSets,
    "parameterSets",
    nScenarios
  )

  # Special handling for outputPaths to preserve named vectors
  if (!is.null(outputPaths)) {
    if (is.list(outputPaths)) {
      # outputPaths is a list: validate length
      if (length(outputPaths) == 1) {
        # Recycle single list element to all scenarios
        outputPaths <- rep(outputPaths, nScenarios)
      } else if (length(outputPaths) != nScenarios) {
        cli::cli_abort(messages$invalidArgumentLength(
          length(outputPaths),
          nScenarios
        ))
      }
    } else {
      # outputPaths is a vector
      if (length(outputPaths) == 1) {
        # Single vector: recycle entire vector to each scenario
        # (preserves named vectors)
        outputPaths <- rep(list(outputPaths), nScenarios)
      } else if (length(outputPaths) == nScenarios) {
        # Vector matches scenarios length: use as-is
        # (for comma-separated strings)
        outputPaths <- .recycleOrValidateVector(
          outputPaths,
          "outputPaths",
          nScenarios
        )
      } else {
        cli::cli_abort(messages$invalidArgumentLength(
          length(outputPaths),
          nScenarios
        ))
      }
    }
  }

  simulationTime <- .recycleOrValidateVector(
    simulationTime,
    "simulationTime",
    nScenarios
  )
  simulationTimeUnit <- .recycleOrValidateVector(
    simulationTimeUnit,
    "simulationTimeUnit",
    nScenarios
  )
  steadyState <- .recycleOrValidateVector(
    steadyState,
    "steadyState",
    nScenarios
  )
  steadyStateTime <- .recycleOrValidateVector(
    steadyStateTime,
    "steadyStateTime",
    nScenarios
  )
  steadyStateTimeUnit <- .recycleOrValidateVector(
    steadyStateTimeUnit,
    "steadyStateTimeUnit",
    nScenarios
  )
  overwriteFormulasInSS <- .recycleOrValidateVector(
    overwriteFormulasInSS,
    "overwriteFormulasInSS",
    nScenarios
  )
  readPopulationFromCSV <- .recycleOrValidateVector(
    readPopulationFromCSV,
    "readPopulationFromCSV",
    nScenarios
  )

  # Phase A: build a self-contained spec for every scenario without touching
  # the project. Names are resolved against the existing project scenarios up
  # front so the duplicate-name abort in `addScenario()` can never fire, and
  # output-path ids are resolved against both the project and the entries
  # accumulated earlier in this same call (`pending`).

  # Resolve final, collision-free names before building any spec.
  requestedNames <- character(length(pkmlFilePaths))
  simulationCache <- list()
  for (i in seq_along(pkmlFilePaths)) {
    pkmlPath <- pkmlFilePaths[[i]]
    if (!file.exists(pkmlPath)) {
      cli::cli_abort(messages$fileNotFound(pkmlPath))
    }
    # Memoize the load: a single PKML recycled across N scenarios would
    # otherwise be parsed N times.
    if (is.null(simulationCache[[pkmlPath]])) {
      simulationCache[[pkmlPath]] <- ospsuite::loadSimulation(
        filePath = pkmlPath,
        loadFromCache = FALSE
      )
    }
    requestedNames[[i]] <- if (is.null(scenarios)) {
      simulationCache[[pkmlPath]]$name
    } else {
      scenarios[[i]]
    }
  }
  # Canonicalize each requested id before deduping so collision suffixing
  # operates in canonical (lowercase, safe) space and the canonical ids the
  # specs are built with are exactly what `addScenario()` will store, so its
  # duplicate-name abort can never fire on a case-only or sanitization clash.
  # Per-element (no within-call collision error): a recycled PKML legitimately
  # yields identical names that `.dedupeScenarioNames()` then suffixes.
  requestedNames <- vapply(
    requestedNames,
    function(n) suppressWarnings(.canonicalizeOneId(n)),
    character(1),
    USE.NAMES = FALSE
  )
  finalNames <- .dedupeScenarioNames(
    requestedNames,
    names(self$definitions$scenarios)
  )

  specs <- vector("list", length(pkmlFilePaths))
  # Accumulated id -> path map for output paths registered during this call.
  pending <- character()

  for (i in seq_along(pkmlFilePaths)) {
    pkmlPath <- pkmlFilePaths[[i]]
    simulation <- simulationCache[[pkmlPath]]
    scenarioName <- finalNames[[i]]

    # Resolve the model file relative to the project's model folder so the
    # stored path is portable. `as.character()` strips the `fs_path` class so
    # the value round-trips identically through save/load.
    if (is.null(self$paths$modelFolder)) {
      cli::cli_warn(messages$noModelFolderUsingAbsolutePath(pkmlPath))
      modelFile <- as.character(fs::path_abs(pkmlPath))
    } else {
      modelFile <- as.character(fs::path_rel(
        pkmlPath,
        start = self$paths$modelFolder
      ))
    }

    # Application protocol: user value verbatim (PKML embeds its application,
    # so the default is absent). `NA` means absent and is mapped to NULL for
    # `addScenario()`, whose FK check rejects NA.
    applicationProtocol <- NULL
    if (!is.null(application)) {
      candidate <- application[[i]]
      if (!is.na(candidate)) {
        applicationProtocol <- candidate
      }
    }

    # Parameter sets: split a single comma-separated string into ids.
    modelParameterSetIds <- NULL
    if (!is.null(parameterSets)) {
      paramSetIds <- parameterSets[[i]]
      if (!is.na(paramSetIds) && nchar(paramSetIds) > 0) {
        modelParameterSetIds <- .splitCommaSeparated(paramSetIds)
      }
    }

    # Output paths: user-supplied (list element or comma-separated string) or
    # extracted from the PKML; resolved to ids registered against the project.
    outputPathIds <- NULL
    scenarioOutputPaths <- if (!is.null(outputPaths)) {
      if (is.list(outputPaths)) outputPaths[[i]] else outputPaths[i]
    } else {
      .extractOutputPathsFromPkml(simulation)
    }
    if (
      !is.null(scenarioOutputPaths) &&
        !any(is.na(scenarioOutputPaths)) &&
        all(nchar(scenarioOutputPaths) > 0)
    ) {
      # An unnamed single string may carry comma-separated paths.
      if (
        length(scenarioOutputPaths) == 1 &&
          is.null(names(scenarioOutputPaths))
      ) {
        scenarioOutputPaths <- .splitCommaSeparated(scenarioOutputPaths)
      }
      resolved <- .resolveScenarioOutputPaths(
        scenarioOutputPaths,
        self,
        pending
      )
      outputPathIds <- resolved$outputPathIds
      pending <- c(pending, resolved$newEntries)
    }

    # Simulation time: user string passed through (addScenario parses it), or
    # extracted from the PKML output schema.
    if (!is.null(simulationTime)) {
      simulationTimeStr <- simulationTime[[i]]
      scenarioSimTimeUnit <- if (!is.null(simulationTimeUnit)) {
        simulationTimeUnit[[i]]
      } else {
        NULL
      }
    } else {
      extracted <- .extractSimulationTimeFromPkml(
        simulation,
        targetUnit = if (!is.null(simulationTimeUnit)) {
          simulationTimeUnit[[i]]
        } else {
          NULL
        }
      )
      simulationTimeStr <- extracted$simulationTime
      scenarioSimTimeUnit <- extracted$simulationTimeUnit
    }

    # Steady state: pass the user value or fall back to `addScenario()`'s
    # defaults (1000 / "min"). `addScenario()` converts to the base unit and
    # always sets the unit, so the serializer never aborts.
    scenarioSteadyStateTime <- 1000
    if (!is.null(steadyStateTime)) {
      candidate <- steadyStateTime[[i]]
      if (!is.na(candidate)) {
        scenarioSteadyStateTime <- candidate
      }
    }
    scenarioSteadyStateTimeUnit <- if (!is.null(steadyStateTimeUnit)) {
      steadyStateTimeUnit[[i]]
    } else {
      "min"
    }

    specs[[i]] <- list(
      scenarioName = scenarioName,
      modelFile = modelFile,
      individual = if (!is.null(individual)) individual[[i]],
      population = if (!is.null(population)) population[[i]],
      application = applicationProtocol,
      parameterSets = modelParameterSetIds,
      outputPaths = outputPathIds,
      simulationTime = simulationTimeStr,
      simulationTimeUnit = scenarioSimTimeUnit,
      steadyState = steadyState[[i]],
      steadyStateTime = scenarioSteadyStateTime,
      steadyStateTimeUnit = scenarioSteadyStateTimeUnit,
      overwriteFormulasInSS = overwriteFormulasInSS[[i]],
      readPopulationFromCSV = readPopulationFromCSV[[i]]
    )
  }

  # Phase B: apply the specs to the project transactionally. A failing
  # `addScenario()` (e.g. an unknown individual) on scenario `i` must not
  # leave scenarios 1..i-1 and freshly registered output paths behind, so
  # snapshot the section fields and restore them on error.
  oldScenarios <- private$.getSection("scenarios")
  oldOutputPaths <- private$.getSection("outputPaths")
  wasValidated <- private$.isValidated()
  wasModified <- private$.isModified()

  tryCatch(
    {
      if (length(pending) > 0) {
        addOutputPath(
          self,
          id = names(pending),
          path = unname(pending)
        )
      }
      for (spec in specs) {
        addScenario(
          self,
          id = spec$scenarioName,
          modelFile = spec$modelFile,
          individual = spec$individual,
          population = spec$population,
          application = spec$application,
          parameterSets = spec$parameterSets,
          outputPaths = spec$outputPaths,
          simulationTime = spec$simulationTime,
          simulationTimeUnit = spec$simulationTimeUnit,
          steadyState = spec$steadyState,
          steadyStateTime = spec$steadyStateTime,
          steadyStateTimeUnit = spec$steadyStateTimeUnit,
          overwriteFormulasInSS = spec$overwriteFormulasInSS,
          readPopulationFromCSV = spec$readPopulationFromCSV
        )
      }
    },
    error = function(cnd) {
      private$.setSection("scenarios", oldScenarios)
      private$.setSection("outputPaths", oldOutputPaths)
      # `.setSection()` marks the project modified, so restore the pre-call
      # dirty and validation flags: a rollback must leave an initially-clean
      # project reporting no unsaved changes.
      if (!wasModified) {
        private$.clearModified()
      }
      if (wasValidated) {
        private$.markValidated()
      }
      stop(cnd)
    }
  )

  addedNames <- vapply(specs, function(spec) spec$scenarioName, character(1))
  if (length(addedNames) > 0) {
    cli::cli_inform(messages$scenariosAddedToProject(addedNames))
  }

  invisible(self)
}

#' @keywords internal
#' @noRd
# Resolve a vector of requested scenario names against the names already on
# the project (`existingNames`) plus the names resolved earlier in the same
# call, appending `_2`, `_3`, ... until each candidate is free. Warns once per
# rename.
.dedupeScenarioNames <- function(requestedNames, existingNames) {
  taken <- existingNames %||% character()
  resolved <- character(length(requestedNames))
  for (i in seq_along(requestedNames)) {
    candidate <- requestedNames[[i]]
    if (candidate %in% taken) {
      original <- candidate
      suffix <- 2L
      repeat {
        candidate <- paste0(original, "_", suffix)
        if (!(candidate %in% taken)) {
          break
        }
        suffix <- suffix + 1L
      }
      cli::cli_warn(messages$autocorrectDuplicateScenarioNames(
        original,
        candidate
      ))
    }
    taken <- c(taken, candidate)
    resolved[[i]] <- candidate
  }
  resolved
}

#' @keywords internal
#' @noRd
# Split a single comma-separated string into a trimmed character vector.
.splitCommaSeparated <- function(x) {
  trimws(strsplit(x, ",", fixed = TRUE)[[1]])
}

#' @keywords internal
#' @noRd
# Extract the selected output paths from a loaded PKML simulation, or NULL.
.extractOutputPathsFromPkml <- function(simulation) {
  if (is.null(simulation$outputSelections$allOutputs)) {
    return(NULL)
  }
  vapply(
    simulation$outputSelections$allOutputs,
    function(x) x$path,
    character(1)
  )
}

#' @keywords internal
#' @noRd
# Extract simulation time intervals from a PKML output schema as the
# "start, end, resolution; ..." string `addScenario()` expects, together with
# the resolved time unit. `targetUnit` (if provided) overrides the PKML's own
# display unit and interval bounds are converted to it. Returns a list with
# `simulationTime` and `simulationTimeUnit`, both NULL when the schema has no
# intervals.
.extractSimulationTimeFromPkml <- function(simulation, targetUnit = NULL) {
  empty <- list(simulationTime = NULL, simulationTimeUnit = NULL)
  intervals <- simulation$outputSchema$intervals
  if (is.null(intervals) || length(intervals) == 0) {
    return(empty)
  }

  unit <- targetUnit
  if (is.null(unit)) {
    unit <- intervals[[1]]$startTime$displayUnit %||% "min"
  }

  toTargetUnit <- function(value, fromUnit) {
    if (fromUnit == unit) {
      return(value)
    }
    ospsuite::toUnit(
      quantityOrDimension = ospsuite::ospDimensions$Time,
      values = value,
      targetUnit = unit
    )
  }

  intervalStrings <- vapply(
    intervals,
    function(interval) {
      paste(
        toTargetUnit(interval$startTime$value, interval$startTime$displayUnit),
        toTargetUnit(interval$endTime$value, interval$endTime$displayUnit),
        interval$resolution$value,
        sep = ", "
      )
    },
    character(1)
  )

  list(
    simulationTime = paste(intervalStrings, collapse = "; "),
    simulationTimeUnit = unit
  )
}

#' @keywords internal
#' @noRd
# Build a readable output-path id from an OSPS-notation path by joining its
# last two `|` segments, replacing non-alphanumeric runs with `_`, and
# deduplicating against `takenIds` with numeric suffixes.
.generateOutputPathId <- function(path, takenIds) {
  segments <- strsplit(path, "|", fixed = TRUE)[[1]]
  tail <- utils::tail(segments, 2)
  base <- paste(tail, collapse = "_")
  base <- gsub("[^[:alnum:]]+", "_", base)
  base <- gsub("^_+|_+$", "", base)
  if (nchar(base) == 0) {
    base <- "outputPath"
  }
  # Generate the id already canonical (lowercase, safe) so `addOutputPath()`
  # does not warn about canonicalizing an id the package itself created.
  base <- .canonicalizeOneId(base)
  candidate <- base
  suffix <- 2L
  while (candidate %in% takenIds) {
    candidate <- paste0(base, "_", suffix)
    suffix <- suffix + 1L
  }
  candidate
}

#' @keywords internal
#' @noRd
# Resolve a scenario's literal output paths to ids registered against the
# project. `pending` is the id -> path map accumulated for earlier scenarios
# in the same call. Precedence per path:
#   1. literal path already registered (project or pending) -> reuse that id;
#   2. user-supplied name colliding with a registered id mapped to a
#      *different* path -> abort; otherwise register under the user's name;
#   3. unnamed new path -> register under a generated readable id.
# Returns `list(outputPathIds, newEntries)`, both named-by-id where relevant.
.resolveScenarioOutputPaths <- function(paths, project, pending) {
  projectPaths <- unlist(project$definitions$outputPaths, use.names = TRUE)
  userNames <- names(paths)
  outputPathIds <- character(length(paths))
  newEntries <- character()

  for (i in seq_along(paths)) {
    path <- unname(paths[[i]])
    known <- c(projectPaths, pending, newEntries)
    matchIdx <- which(known == path)

    if (length(matchIdx) > 0) {
      # Reuse the existing id for this literal path. If the user supplied a
      # name that differs from the registered id, inform them it was ignored.
      registeredId <- names(known)[[matchIdx[[1]]]]
      outputPathIds[[i]] <- registeredId
      userName <- if (!is.null(userNames)) userNames[[i]] else ""
      if (!is.null(userName) && nzchar(userName) && userName != registeredId) {
        cli::cli_inform(messages$outputPathAliasIgnored(
          userName,
          registeredId,
          path
        ))
      }
      next
    }

    userName <- if (!is.null(userNames)) userNames[[i]] else ""
    takenIds <- c(names(projectPaths), names(pending), names(newEntries))

    if (!is.null(userName) && nzchar(userName)) {
      id <- userName
      if (id %in% takenIds) {
        existingPath <- c(projectPaths, pending, newEntries)[[id]]
        cli::cli_abort(messages$outputPathIdCollision(id, existingPath, path))
      }
    } else {
      id <- .generateOutputPathId(path, takenIds)
    }

    outputPathIds[[i]] <- id
    newEntries[[id]] <- path
  }

  list(outputPathIds = outputPathIds, newEntries = newEntries)
}

#' Get the number of scenarios to create based on vector arguments
#'
#' @param pkmlFilePaths Character vector of PKML file paths.
#' @param ... Other vector arguments to check for length consistency.
#'
#' @details
#' Determines the number of scenarios to create based on the length of
#' vector arguments. All vector arguments with length > 1 must have the
#' same length, which determines the final number of scenarios.
#'
#' @returns Integer number of scenarios to create.
#' @keywords internal
.getScenarioCount <- function(pkmlFilePaths, ...) {
  args <- list(...)
  all_args <- c(list(pkmlFilePaths = pkmlFilePaths), args)
  lengths <- vapply(
    all_args,
    function(x) if (is.null(x)) 0L else length(x),
    integer(1)
  )

  # Filter out NULL arguments (length 0)
  valid_lengths <- lengths[lengths > 0]
  # Get lengths > 1 (vectors that determine scenario count)
  vector_lengths <- valid_lengths[valid_lengths > 1]

  if (length(vector_lengths) == 0) {
    # No vectors with length > 1, so `pkmlFilePaths` is length 0 or 1 (a
    # length > 1 would appear in `vector_lengths`). Its length is the scenario
    # count: 0 (empty input, a no-op) or 1 (a single scenario).
    return(as.integer(length(pkmlFilePaths)))
  } else {
    # At least one vector with length > 1
    if (length(unique(vector_lengths)) == 1) {
      # All vectors with length > 1 have the same length
      return(as.integer(vector_lengths[1]))
    } else {
      # Inconsistent vector lengths
      cli::cli_abort(messages$inconsistentArgumentLengths(vector_lengths))
    }
  }
}

#' Recycle or validate vector arguments for scenario creation
#'
#' @param arg Vector argument to recycle or validate.
#' @param argName Character string name of the argument for error messages.
#' @param nScenarios Integer number of scenarios to create.
#'
#' @details
#' Handles vector recycling for `createScenariosFromPKML()`'s per-scenario
#' arguments (not simulation parameters). Single values are recycled to all
#' scenarios, vectors with the correct length are used as-is, and invalid
#' lengths throw an error.
#'
#' @returns Vector with the correct length for all scenarios, or `NULL` if
#'   input was `NULL`.
#' @keywords internal
.recycleOrValidateVector <- function(arg, argName, nScenarios) {
  if (is.null(arg)) {
    return(NULL)
  }

  if (length(arg) == 1) {
    # Recycle single value to all scenarios
    return(rep(arg, nScenarios))
  } else if (length(arg) == nScenarios) {
    # Vector has same length as number of scenarios
    return(arg)
  } else {
    # Invalid length
    cli::cli_abort(messages$invalidArgumentLengthScenarios(
      argName,
      arg,
      nScenarios
    ))
  }
}
