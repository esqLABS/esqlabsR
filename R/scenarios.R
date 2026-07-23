# Scenario ----
#
# Plain-data scenario record produced by `.parseScenarios()` below.
# Holds the typed shape of a JSON scenario entry
# without any runtime side effects: no Simulation construction, no
# Population loading, no parameter merging. A `Scenario` is a classed
# list with copy semantics: extracted entries are independent copies,
# never references into a `Project`.

# Explicit field list backing `as.list.Scenario()`. Kept in sync with
# the v2.0 `Project.json` scenario entry shape so adding a method on
# `Scenario` does not change the JSON-shape contract.
.scenarioFieldNames <- c(
  "scenarioName",
  "modelFile",
  "applicationProtocol",
  "individualId",
  "populationId",
  "outputPaths",
  "simulationType",
  "readPopulationFromCSV",
  "simulateSteadyState",
  "simulationTime",
  "simulationTimeUnit",
  "steadyStateTime",
  "steadyStateTimeUnit",
  "overwriteFormulasInSS",
  "modelParameterSets",
  "initialConditions"
)

#' Create a Scenario
#'
#' @description Builds a plain-data `Scenario` record holding the
#'   configuration fields of a v2.0 `Project.json` scenario entry. It does
#'   not create or hold ospsuite runtime objects; the runtime is built by
#'   [runScenarios()] at execution time.
#'
#'   A `Scenario` is a named list with copy semantics: an entry extracted
#'   from `scenarios` definitions is an independent copy. The section accessor is
#'   read-only, so to apply a change you pass the record to an authoring
#'   function (`addScenario()` / `setScenario()`), which validates and writes
#'   it through to the project.
#'
#' @param scenarioName Character. Name of the scenario.
#' @param modelFile Character. Name of the `.pkml` model file (relative to
#'   the model folder).
#' @param applicationProtocol Character or `NA`. Name of the application
#'   protocol; `NA` when absent.
#' @param individualId Character or `NULL`. ID referencing
#'   `individuals` definitions.
#' @param populationId Character or `NULL`. ID referencing
#'   `populations` definitions.
#' @param outputPaths Named character vector of literal output paths.
#'   Names are the ids referencing `outputPaths` definitions; values are the
#'   literal paths. `NULL` when the scenario declares no outputs.
#'   Round-trip serialization reads `names(outputPaths)` to rebuild the
#'   `outputPaths` id array, so the named-vector invariant must be preserved.
#' @param simulationType Character. `"Individual"` or `"Population"`.
#'   Defaults to `"Population"` when `populationId` is set,
#'   `"Individual"` otherwise.
#' @param readPopulationFromCSV Logical. If `TRUE`, load population from
#'   CSV.
#' @param simulateSteadyState Logical. If `TRUE`, run steady-state before
#'   the main simulation.
#' @param simulationTime List of length-3 numeric vectors
#'   `c(start, end, resolution)`.
#' @param simulationTimeUnit Character. Time unit for `simulationTime`.
#' @param steadyStateTime Numeric. Steady-state time **in base unit
#'   (minutes)**.
#' @param steadyStateTimeUnit Character. Original unit for
#'   `steadyStateTime`, preserved for round-trip serialization.
#' @param overwriteFormulasInSS Logical. Overwrite formula parameters
#'   during steady-state.
#' @param modelParameterSets Character vector. Parameter-set ids
#'   referencing `parameterSets` definitions.
#' @param initialConditions Character vector. Initial-condition set ids
#'   referencing `initialConditions` definitions.
#'
#' @returns A `Scenario` object: a named list carrying exactly the fields
#'   above.
#' @export
Scenario <- function(
  scenarioName = NULL,
  modelFile = NULL,
  applicationProtocol = NULL,
  individualId = NULL,
  populationId = NULL,
  outputPaths = NULL,
  simulationType = if (is.null(populationId)) "Individual" else "Population",
  readPopulationFromCSV = FALSE,
  simulateSteadyState = FALSE,
  simulationTime = NULL,
  simulationTimeUnit = NULL,
  steadyStateTime = 1000,
  steadyStateTimeUnit = NULL,
  overwriteFormulasInSS = FALSE,
  modelParameterSets = NULL,
  initialConditions = NULL
) {
  # `mget()` reads every formal by name (the formals are exactly
  # `.scenarioFieldNames`, in order), keeping NULL-valued slots so the record
  # always carries the full shape regardless of which arguments are set.
  structure(mget(.scenarioFieldNames), class = c("Scenario", "list"))
}

#' @exportS3Method
#' @noRd
as.list.Scenario <- function(x, ...) {
  unclass(x)
}

#' @exportS3Method
#' @noRd
print.Scenario <- function(x, ...) {
  protocol <- if (
    !is.null(x$applicationProtocol) && !is.na(x$applicationProtocol)
  ) {
    x$applicationProtocol
  } else {
    ""
  }
  ospsuite.utils::ospPrintClass(x)
  ospsuite.utils::ospPrintItems(
    list(
      "Name" = x$scenarioName %||% "",
      "Model" = x$modelFile %||% "",
      "Type" = x$simulationType %||% "",
      "Individual" = x$individualId %||% "",
      "Population" = x$populationId %||% "",
      "Protocol" = protocol,
      "Parameter Sets" = paste(x$modelParameterSets, collapse = ", "),
      "Initial Conditions" = paste(x$initialConditions, collapse = ", "),
      "Output Paths" = length(x$outputPaths %||% list()),
      "Steady State" = x$simulateSteadyState %||% FALSE
    ),
    print_empty = TRUE
  )
  invisible(x)
}

# Parse ----
#
# Internal: parse the JSON `scenarios` array into a named list of
# `Scenario` objects, keyed by scenario name.
#
# `scenariosData` is the raw `simplifyVector = FALSE` shape produced by
# `jsonlite::fromJSON()`: a list of plain named lists. `outputPaths` is
# the project-level lookup table (named list or named character vector
# of `id -> literal path`); used to resolve the scenario's `outputPaths`
# id array.
#
# This helper handles only what's needed at parse time: field copies,
# `simulationType` derivation from `population` presence,
# `simulationTime` string parsing, `steadyStateTime` unit conversion,
# the `outputPaths` id array -> literal `outputPaths` resolution.
#
# @keywords internal
# @noRd
.parseScenarios <- function(scenariosData, outputPaths) {
  if (is.null(scenariosData)) {
    return(list())
  }

  result <- list()
  for (entry in scenariosData) {
    # The scenario name is the definition id and the filename stem; assert it is a
    # usable scalar that matches the file before anything else, so a file with
    # no `name` (or one disagreeing with its filename) aborts naming the file
    # rather than producing an opaque base-R index error or a dangling key.
    scenarioName <- .keyedTreeRecordId(entry, "name", "scenario")
    # Reject a malformed scalar field early, with a message naming the
    # scenario and field. A hand-edited file (e.g. the standard `jsonlite`
    # round-trip that turns `"population": null` into `{}`) otherwise
    # produces a non-scalar value that mis-derives `simulationType` and later
    # crashes validation with an opaque internal error that names nothing.
    .assertScalarScenarioField(entry, "individual")
    .assertScalarScenarioField(entry, "population")
    .assertScalarScenarioField(entry, "application")
    .assertScalarScenarioField(entry, "modelFile")
    # Backstop the per-field checks above with the generic empty-object guard,
    # so any other scalar field corrupted by the `null -> {}` round-trip (e.g.
    # `steadyStateTime: {}`) also fails fast instead of slipping through.
    .assertNoEmptyObjectFields(entry, "scenario")
    # Use `[[` rather than `$` throughout: list `$` does partial matching,
    # so `entry$simulationTime` would wrongly resolve to `simulationTimeUnit`
    # (and `steadyStateTime` to `steadyStateTimeUnit`) when only the unit is
    # declared.
    simulationTime <- NULL
    if (!is.null(entry[["simulationTime"]])) {
      simulationTime <- .parseSimulationTimeIntervals(entry[["simulationTime"]])
    }
    # Preserve a declared unit even when `simulationTime` is null, so a
    # standalone `simulationTimeUnit` round-trips instead of being dropped.
    simulationTimeUnit <- entry[["simulationTimeUnit"]]

    steadyStateTime <- 1000
    steadyStateTimeUnit <- NULL
    if (!is.null(entry[["steadyStateTime"]])) {
      if (is.null(entry[["steadyStateTimeUnit"]])) {
        cli::cli_abort(
          "Scenario {.val {entry[['name']]}} has {.field steadyStateTime} set \\
          but {.field steadyStateTimeUnit} is null. Please specify a unit \\
          (e.g. {.val min})."
        )
      }
      # Coerce to double before the unit conversion: `jsonlite::fromJSON`
      # reads a whole number (e.g. `1000`) as integer, and a same-unit
      # conversion preserves that integer type, so without this the parsed
      # record's `steadyStateTime` would be integer while a freshly built
      # scenario (default `1000`, a double) is double, breaking a round trip.
      steadyStateTime <- ospsuite::toBaseUnit(
        quantityOrDimension = ospDimensions$Time,
        values = as.double(entry[["steadyStateTime"]]),
        unit = entry[["steadyStateTimeUnit"]]
      )
      steadyStateTimeUnit <- entry[["steadyStateTimeUnit"]]
    }

    scenarioOutputPaths <- NULL
    # Collapse a repeated id to a single entry (first-seen order) so the same
    # output path is never resolved, run, or plotted twice; `unique()` keeps
    # the first occurrence.
    pathIds <- unique(unlist(entry[["outputPaths"]]))
    # An absent key and an empty `outputPaths: []` both collapse to NULL
    # (no resolved paths), matching the serializer's array-shape symmetry.
    if (length(pathIds) > 0L) {
      # Referential integrity is lazy: an output-path id that is not in the
      # project-level `outputPaths` map is kept as a name with an `NA`
      # literal path, so it round-trips and the cross-reference validator
      # (which checks `names(sc$outputPaths)`) flags it at validate/run/plot
      # time rather than aborting the load.
      resolved <- vapply(
        pathIds,
        function(id) outputPaths[[id]] %||% NA_character_,
        character(1),
        USE.NAMES = FALSE
      )
      scenarioOutputPaths <- stats::setNames(resolved, pathIds)
    }

    result[[scenarioName]] <- Scenario(
      scenarioName = scenarioName,
      modelFile = entry[["modelFile"]],
      applicationProtocol = entry[["application"]] %||% NA,
      individualId = entry[["individual"]],
      populationId = entry[["population"]],
      outputPaths = scenarioOutputPaths,
      readPopulationFromCSV = entry[["readPopulationFromCSV"]] %||% FALSE,
      simulateSteadyState = isTRUE(entry[["steadyState"]]),
      simulationTime = simulationTime,
      simulationTimeUnit = simulationTimeUnit,
      steadyStateTime = steadyStateTime,
      steadyStateTimeUnit = steadyStateTimeUnit,
      overwriteFormulasInSS = entry[["overwriteFormulasInSS"]] %||% FALSE,
      modelParameterSets = if (!is.null(entry[["parameterSets"]])) {
        unlist(entry[["parameterSets"]])
      },
      initialConditions = if (!is.null(entry[["initialConditions"]])) {
        unlist(entry[["initialConditions"]])
      }
    )
  }
  result
}

# Assert that a scenario JSON field, when present, is a length-1 string (or
# `NULL`/absent). Catches a hand-edited file that turned a scalar field into
# an object/array (e.g. `"population": null` round-tripped to `{}`), which
# would otherwise mis-derive `simulationType` and crash validation with an
# opaque internal error. The message names the scenario and the field so the
# user can find the offending file (`definitions/scenarios/<name>.json`).
#
# @keywords internal
# @noRd
.assertScalarScenarioField <- function(entry, field) {
  value <- entry[[field]]
  if (is.null(value)) {
    return(invisible(NULL))
  }
  if (!is.character(value) || length(value) != 1L) {
    name <- entry[["name"]] %||% "<unnamed>"
    cli::cli_abort(c(
      "Scenario {.val {name}} has an invalid {.field {field}}.",
      "i" = "Expected a single string or {.code null}; check \\
      {.file definitions/scenarios/{name}.json}.",
      "i" = "A hand-edit that turned {.code \"{field}\": null} into an empty \\
      object {.code {{}}} is the usual cause."
    ))
  }
  invisible(NULL)
}

# Section validation adapters ----
#
# Registered in `.validationAdapters` (R/validation.R) and called by
# `.runProjectValidation()`. Each pulls the right slice of the project
# and delegates to a section-local `.validate*` helper. They only run
# when `validateProject()` (or a targeted `.ensureValid`) is called;
# they do not run during parsing or simulation.

#' @keywords internal
#' @noRd
.scenariosValidatorAdapter <- function(project) {
  .validateScenarios(
    project$definitions$scenarios,
    project$paths$simulationsFolder
  )
}

#' @keywords internal
#' @noRd
.applicationsValidatorAdapter <- function(project) {
  .validateApplications(project$definitions$applications)
}

#' Validate the `scenarios` section of a Project
#'
#' Per-entry checks: `modelFile` is set and non-empty, resolves on disk
#' (warning), `simulationType` is one of the supported values, and
#' population-typed scenarios declare a `populationId`.
#'
#' Cross-section reference checks (individual, parameterSets, application,
#' …) live in `.validateCrossReferences()`.
#'
#' @param scenarios Named list of `Scenario` objects from
#'   `scenarios` definitions.
#' @param simulationsFolder Character. Absolute path to the project's
#'   simulations folder, used to resolve relative `modelFile` paths. May be
#'   `NULL`.
#' @return validationResult.
#' @keywords internal
#' @noRd
.validateScenarios <- function(scenarios, simulationsFolder = NULL) {
  result <- validationResult$new()

  if (is.null(scenarios) || length(scenarios) == 0) {
    result$addWarning("Data", "No scenarios defined")
    return(result)
  }

  for (name in names(scenarios)) {
    sc <- scenarios[[name]]

    if (is.null(sc$modelFile) || sc$modelFile == "") {
      result$addCriticalError(
        "Missing Fields",
        paste0("Scenario '", name, "' has no modelFile")
      )
    } else if (!is.null(simulationsFolder)) {
      # A relative `modelFile` that escapes the simulations folder is a
      # critical error here, matching the abort `.prepareScenario()` raises at
      # run time, so `validateProject()` does not give false assurance on a
      # path the runtime would reject. An absolute `modelFile` is used verbatim
      # at run time (no containment), so it is not checked here either.
      if (
        !fs::is_absolute_path(sc$modelFile) &&
          .pathEscapesRoot(sc$modelFile, simulationsFolder)
      ) {
        result$addCriticalError(
          "Path Containment",
          paste0(
            "Scenario '",
            name,
            "' references a modelFile outside the project folder: ",
            sc$modelFile
          )
        )
      } else {
        modelFilePath <- file.path(simulationsFolder, sc$modelFile)
        if (!file.exists(modelFilePath)) {
          result$addWarning(
            "File Not Found",
            paste0(
              "Scenario '",
              name,
              "' references non-existent modelFile: ",
              sc$modelFile
            )
          )
        }
      }
    }

    simType <- sc$simulationType %||% ""
    if (!simType %in% c("Individual", "Population")) {
      result$addCriticalError(
        "Validation",
        paste0(
          "Scenario '",
          name,
          "' has invalid simulationType '",
          simType,
          "'"
        )
      )
    }

    # `.parseScenarios` already rejects a non-scalar `populationId` on load, so
    # `sc$populationId` is a length-1 string or `NULL` here. Use a scalar-safe
    # emptiness test so a stray non-scalar value can never make the `&&`
    # operand `NA` and crash with an opaque internal error.
    hasPopulationId <- !is.null(sc$populationId) &&
      length(sc$populationId) == 1L &&
      !is.na(sc$populationId) &&
      nzchar(sc$populationId)

    if (simType == "Population" && !hasPopulationId) {
      result$addCriticalError(
        "Missing Fields",
        paste0("Population scenario '", name, "' has no populationId")
      )
    }

    if (simType != "Population" && hasPopulationId) {
      result$addWarning(
        "Validation",
        paste0(
          "Scenario '",
          name,
          "' has a populationId but simulationType is '",
          simType,
          "'; it will load as a Population scenario on the next reload."
        )
      )
    }
  }

  result
}

#' Structural fail-fast validation for one scenario, run on write-through
#'
#' Checks a single scenario's own shape (independent of cross-references)
#' before it is persisted to its definition file: it must be a `Scenario`
#' record, carry a name that is a safe single path segment and matches the
#' list key it is stored under, carry a non-empty `modelFile`, and declare
#' a supported `simulationType`. Referential checks (does `individual` /
#' `outputPaths` / ... resolve) stay lazy and live in
#' `.validateCrossReferences()`; they are not enforced here so a project
#' can pass through a transiently-inconsistent graph during editing.
#'
#' Aborts on the first structural problem so a malformed scenario never
#' lands on disk.
#'
#' @param sc A `Scenario` record.
#' @param name The scenario's key (the authoritative id; the definition file
#'   is named after it).
#' @keywords internal
#' @noRd
.validateScenarioStructure <- function(sc, name) {
  if (!inherits(sc, "Scenario")) {
    cli::cli_abort(
      "Scenario {.val {name}} must be a {.cls Scenario} record."
    )
  }
  # The list key is the authoritative scenario id: it names the definition file
  # and is what removal, cross-reference validation, and `names()` key off.
  # The authoring API (`addScenario()` / `setScenario()`) canonicalizes the
  # id before it reaches here, so the key is already safe. This structural
  # backstop still rejects any key that is not already canonical (mixed case, a
  # forbidden character, ...) and points the user at the canonical form, so a
  # non-canonical key reaching the write path (the section accessor is
  # read-only, so only an internal `.setSection()` write can) is caught here.
  # This subsumes the old case-insensitive-collision guard: two keys differing
  # only in case cannot both be canonical, so they can never both reach the
  # tree.
  canonical <- suppressWarnings(.canonicalizeId(name))
  if (!identical(name, canonical)) {
    cli::cli_abort(c(
      "Scenario id {.val {name}} is not a canonical definition-file id.",
      "i" = "Use {.code addScenario(project, {.val {name}}, ...)}, which \\
      canonicalizes it to {.val {canonical}}, or store the scenario under \\
      the key {.val {canonical}}."
    ))
  }
  # The record's own `scenarioName` must agree with the key it is stored
  # under, or the serialized JSON (`name = sc$scenarioName`) would contradict
  # the filename and the cross-reference key.
  if (
    !is.null(sc$scenarioName) &&
      !identical(sc$scenarioName, name)
  ) {
    cli::cli_abort(c(
      "Scenario {.field scenarioName} {.val {sc$scenarioName}} does not match \\
      the key {.val {name}} it is stored under.",
      "i" = "Store a scenario under a key equal to its {.field scenarioName} \\
      (or leave {.field scenarioName} unset)."
    ))
  }
  if (
    is.null(sc$modelFile) ||
      !is.character(sc$modelFile) ||
      length(sc$modelFile) != 1L ||
      is.na(sc$modelFile) ||
      !nzchar(sc$modelFile)
  ) {
    cli::cli_abort(
      "Scenario {.val {name}} has no {.field modelFile}; \\
      it must be a non-empty string."
    )
  }
  simType <- sc$simulationType %||% ""
  if (!simType %in% c("Individual", "Population")) {
    cli::cli_abort(
      "Scenario {.val {name}} has invalid {.field simulationType} \\
      {.val {simType}}; expected {.val Individual} or {.val Population}."
    )
  }
  # Structural validation must be a true superset of what the serializer
  # (`.scenarioToJson`) requires: a record that passes here must always
  # serialize. The two checks below mirror the serializer's own aborts so a
  # write-through never validates a scenario the writer then chokes on.
  if (isTRUE(sc$simulateSteadyState) && is.null(sc$steadyStateTimeUnit)) {
    cli::cli_abort(c(
      "Scenario {.val {name}} has {.field simulateSteadyState}=TRUE but \\
      {.field steadyStateTimeUnit} is NULL.",
      "i" = "Set {.field steadyStateTimeUnit} (e.g. {.val min}) so the \\
      steady-state time can round-trip."
    ))
  }
  if (!is.null(sc$outputPaths)) {
    pathIds <- names(sc$outputPaths)
    if (
      !is.character(sc$outputPaths) ||
        is.null(pathIds) ||
        any(pathIds == "")
    ) {
      cli::cli_abort(c(
        "Scenario {.val {name}} has {.field outputPaths} without ids.",
        "i" = "Expected a named character vector: id-as-name, \\
        literal-path-as-value."
      ))
    }
  }
  # The scalar reference fields the serializer emits verbatim
  # (`.scenarioToJson`) must be a length-1 scalar when present, or a wrong-typed
  # value (e.g. `individualId <- list(...)`) would silently serialize to a
  # malformed definition file the parser then rejects on the next load. NULL or a
  # length-1 `NA` is allowed: both are the established "no reference" sentinel
  # the serializer handles (an Individual scenario has no population; a scenario
  # may have no application).
  for (field in c("individualId", "populationId", "applicationProtocol")) {
    val <- sc[[field]]
    okScalar <- is.null(val) ||
      (length(val) == 1L && (is.character(val) || is.na(val)))
    if (!okScalar) {
      cli::cli_abort(
        "Scenario {.val {name}} field {.field {field}} must be a single \\
        string (or NULL), not {.obj_type_friendly {val}}."
      )
    }
  }
  # The section accessor is read-only, so the only way a record reaches this
  # write-through backstop is an authoring function (which sets only known
  # fields) or an internal `.setSection()` call. Reject any field the
  # serializer does not know, so a typo'd or stale field cannot be silently
  # dropped on write and then be absent on the next load. `simulationType` is
  # the derived discriminant validated above. The allowed set is exactly the
  # `Scenario` record shape, `.scenarioFieldNames`.
  knownScenarioFields <- .scenarioFieldNames
  unknown <- setdiff(names(sc), knownScenarioFields)
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "Scenario {.val {name}} has unknown field{?s} {.field {unknown}}.",
      "i" = "Allowed fields: {.field {knownScenarioFields}}."
    ))
  }
  invisible(NULL)
}

#' Validate the `applications` section of a Project
#'
#' The applications section is currently a thin wrapper around its
#' `parameterSets` references, all of which are checked in
#' `.validateCrossReferences()`. This adapter exists so that the
#' canonical section list still resolves to a working validator (and so
#' that future shape checks have an obvious home).
#'
#' @param applications Named list from `applications` definitions.
#' @return validationResult.
#' @keywords internal
#' @noRd
.validateApplications <- function(applications) {
  result <- validationResult$new()
  if (is.null(applications) || length(applications) == 0) {
    result$addWarning("Data", "No applications defined")
  }
  result
}

#' Run a set of scenarios from a `Project`.
#'
#' @description Loads simulations, applies parameters, runs the
#'   simulations, and collects results for one or more scenarios
#'   defined on a parsed [Project]. The project must already have been
#'   loaded with [loadProject()].
#'
#' @param project A [Project] object loaded from a `Project.json` file.
#' @param scenarios Optional character vector of scenario names to
#'   run. `NULL` (default) runs all scenarios in the project. Names are
#'   matched case-insensitively against the canonical ids scenarios were
#'   authored under, so the name you passed to [addScenario()] resolves.
#' @param customParams A list with vectors `paths`, `values`, and
#'   `units` — applied to every selected scenario as the final
#'   parameter layer.
#' @param simulationRunOptions Optional [ospsuite::SimulationRunOptions]
#'   for the simulation run. `NULL` (default) uses the package
#'   defaults.
#' @param validate Logical. If `TRUE` (default), runs the relevant
#'   section validators via [validateProject()] before simulating and
#'   aborts with a formatted summary on critical errors. Set to
#'   `FALSE` to skip the pre-flight check (e.g. when the caller has
#'   already validated the project).
#' @param stopIfParameterNotFound Logical. If `TRUE` (default), a
#'   `customParams` path that matches no parameter in a scenario's
#'   simulation aborts the run. Set to `FALSE` to skip such paths with a
#'   warning instead.
#' @param stopIfFails Logical. If `TRUE` (default), a scenario whose
#'   simulation produced no results aborts the run with an error. Set to
#'   `FALSE` to instead warn and leave that scenario's `outputValues`
#'   `NULL` while the other scenarios are still returned.
#'
#' @returns A named list keyed by scenario name. Each entry is a list
#'   with `simulation` (the initialized [ospsuite::Simulation]),
#'   `results` ([ospsuite::SimulationResults]), `outputValues` (the
#'   computed output values, or `NULL` if simulation failed), and
#'   `population` (an [ospsuite::Population] for population
#'   scenarios, or `NULL` for individual scenarios).
#'
#' @details If a scenario's simulation fails, `runScenarios()` aborts by
#'   default (`stopIfFails = TRUE`). Set `stopIfFails = FALSE` to instead
#'   produce a warning and leave that scenario's `outputValues` `NULL`.
#'
#' @seealso [buildSimulations()] to obtain the parameterized simulations
#'   without running them.
#'
#' @export
runScenarios <- function(
  project,
  scenarios = NULL,
  customParams = NULL,
  simulationRunOptions = NULL,
  validate = TRUE,
  stopIfParameterNotFound = TRUE,
  stopIfFails = TRUE
) {
  if (!inherits(project, "Project")) {
    cli::cli_abort(
      "{.arg project} must be a {.cls Project} \
                    (see {.fn loadProject})."
    )
  }
  .runScenariosFromProject(
    project,
    scenarios,
    customParams,
    simulationRunOptions,
    validate,
    stopIfParameterNotFound,
    stopIfFails
  )
}

#' Build the simulations for a set of scenarios without running them
#'
#' @description Loads and fully parameterizes (but does not run) the
#'   [ospsuite::Simulation] (and, for a population scenario, the
#'   [ospsuite::Population]) for one or more scenarios defined on a parsed
#'   [Project]. Use this to inspect or modify a simulation before running
#'   it yourself, to save the configured simulation to PKML, or to hand it
#'   to another OSP-suite routine. To simulate and collect results in one
#'   step, use [runScenarios()].
#'
#' @param project A [Project] object loaded from a `Project.json` file.
#' @param scenarios Optional character vector of scenario names to
#'   build. `NULL` (default) builds all scenarios in the project. Names are
#'   matched case-insensitively against the canonical ids scenarios were
#'   authored under, so the name you passed to [addScenario()] resolves.
#' @param customParams A list with vectors `paths`, `values`, and
#'   `units` — applied to every selected scenario as the final
#'   parameter layer.
#' @param simulationRunOptions Optional [ospsuite::SimulationRunOptions].
#'   Consulted only for a scenario with `simulateSteadyState` set (the
#'   steady-state pre-solve still runs); it is not applied to the returned
#'   simulations, since they are not run here. `NULL` (default) falls back
#'   to the project's `defaultSimulationRunOptions`.
#' @param validate Logical. If `TRUE` (default), runs the relevant
#'   section validators via [validateProject()] before building and
#'   aborts with a formatted summary on critical errors. Set to
#'   `FALSE` to skip the pre-flight check.
#' @param stopIfParameterNotFound Logical. If `TRUE` (default), a
#'   `customParams` path that matches no parameter in a scenario's
#'   simulation aborts the build. Set to `FALSE` to skip such paths with
#'   a warning instead.
#'
#' @returns A named list keyed by scenario name. Each entry is a list
#'   with `simulation` (the initialized, not-yet-run
#'   [ospsuite::Simulation]) and `population` (an [ospsuite::Population]
#'   for population scenarios, or `NULL` for individual scenarios). Pass
#'   the result to [ospsuite::runSimulations()], or inspect and edit the
#'   `Simulation` first. This is not the shape [saveScenarioResults()]
#'   expects; use [runScenarios()] for that.
#'
#' @seealso [runScenarios()] to build and run in one step.
#'
#' @export
buildSimulations <- function(
  project,
  scenarios = NULL,
  customParams = NULL,
  simulationRunOptions = NULL,
  validate = TRUE,
  stopIfParameterNotFound = TRUE
) {
  if (!inherits(project, "Project")) {
    cli::cli_abort(
      "{.arg project} must be a {.cls Project} \
                    (see {.fn loadProject})."
    )
  }
  .buildSimulationsFromProject(
    project,
    scenarios,
    customParams,
    simulationRunOptions,
    validate,
    stopIfParameterNotFound
  )
}

# Public CRUD: scenarios ----

#' Add one or more scenarios programmatically to a Project
#'
#' Creates new `Scenario` records and adds them to `scenarios` definitions after
#' validating all references. The call vectorizes over a vector of ids (see
#' the recycling rule under Details). Scalar-per-definition fields (`modelFile`,
#' `individual`, `population`, `application`, `simulationTime`,
#' `simulationTimeUnit`, `steadyState`, `steadyStateTime`,
#' `steadyStateTimeUnit`, `overwriteFormulasInSS`, `readPopulationFromCSV`)
#' follow the recycle/align rule. The vector-valued-per-definition fields
#' `parameterSets` and `outputPaths` are applied whole to every
#' scenario; to give a different set per scenario, pass a list of the same
#' length as `id` (one character vector per scenario). `initialConditions`
#' follows the same whole-vector rule.
#'
#' @inherit vectorizedAuthoring details
#'
#' @param project A `Project` object.
#' @param id Character vector of ids (names) for the new scenarios (the number
#'   of scenarios to add). Each is canonicalized to a safe, lowercase,
#'   single-path-segment id (a warning names the result if it changed); each
#'   canonical id must not already exist in `scenarios` definitions.
#' @param modelFile Character. Name of the `.pkml` model file (relative
#'   to model folder).
#' @param individual Character or `NULL`. Id referencing
#'   `individuals` definitions.
#' @param population Character or `NULL`. Id referencing
#'   `populations` definitions.
#' @param application Character or `NULL`. Id of the application protocol
#'   referencing `applications` definitions.
#' @param parameterSets Character vector or `NULL`. Parameter-set ids
#'   referencing `parameterSets` definitions. Applied whole to every scenario.
#' @param initialConditions Character vector or `NULL`. Initial-condition set
#'   ids referencing `initialConditions` definitions. Applied whole to every
#'   scenario.
#' @param outputPaths Character vector or `NULL`. Output-path ids referencing
#'   `outputPaths` definitions. Applied whole to every scenario.
#' @param simulationTime Character or `NULL`. Format
#'   `"start, end, resolution"` or
#'   `"start, end, resolution; start, end, resolution"` for multiple
#'   intervals.
#' @param simulationTimeUnit Character. Time unit string. Default `"h"`.
#' @param steadyState Logical. Whether to simulate steady state. Default
#'   `FALSE`.
#' @param steadyStateTime Numeric. Steady-state time in
#'   `steadyStateTimeUnit`. Default `1000`.
#' @param steadyStateTimeUnit Character. Unit for `steadyStateTime`.
#'   Default `"min"`.
#' @param overwriteFormulasInSS Logical. Overwrite formulas during
#'   steady state. Default `FALSE`.
#' @param readPopulationFromCSV Logical. Load population from CSV.
#'   Default `FALSE`.
#' @param overwrite Logical. When `FALSE` (default), an id that already exists
#'   aborts. When `TRUE`, the existing scenario is replaced (last-write-wins).
#'   Distinct from `overwriteFormulasInSS`, which is a steady-state model
#'   option unrelated to duplicate handling.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family scenario
addScenario <- function(
  project,
  id,
  modelFile,
  individual = NULL,
  population = NULL,
  application = NULL,
  parameterSets = NULL,
  initialConditions = NULL,
  outputPaths = NULL,
  simulationTime = NULL,
  simulationTimeUnit = "h",
  steadyState = FALSE,
  steadyStateTime = 1000,
  steadyStateTimeUnit = "min",
  overwriteFormulasInSS = FALSE,
  readPopulationFromCSV = FALSE,
  overwrite = FALSE
) {
  validateIsOfType(project, "Project")
  project$addScenario(
    id,
    modelFile,
    individual,
    population,
    application,
    parameterSets,
    initialConditions,
    outputPaths,
    simulationTime,
    simulationTimeUnit,
    steadyState,
    steadyStateTime,
    steadyStateTimeUnit,
    overwriteFormulasInSS,
    readPopulationFromCSV,
    overwrite
  )
}

# Implementation behind `project$addScenario()` / `addScenario()`. Receives the
# project's own `self` / `private` from the calling method, so it reaches the
# section seam directly through `private$` without any accessor.
#
# @keywords internal
# @noRd
.addScenario_impl <- function(
  self,
  private,
  id,
  modelFile,
  individual = NULL,
  population = NULL,
  application = NULL,
  parameterSets = NULL,
  initialConditions = NULL,
  outputPaths = NULL,
  simulationTime = NULL,
  simulationTimeUnit = "h",
  steadyState = FALSE,
  steadyStateTime = 1000,
  steadyStateTimeUnit = "min",
  overwriteFormulasInSS = FALSE,
  readPopulationFromCSV = FALSE,
  overwrite = FALSE,
  .call
) {
  rlang::local_error_call(.call)
  .assertIdVector(id)
  id <- .canonicalizeId(id)
  n <- length(id)

  perDefinition <- .alignAuthoringArgs(
    id,
    scalarFields = list(
      modelFile = modelFile,
      individual = individual,
      population = population,
      application = application,
      simulationTime = simulationTime,
      simulationTimeUnit = simulationTimeUnit,
      steadyState = steadyState,
      steadyStateTime = steadyStateTime,
      steadyStateTimeUnit = steadyStateTimeUnit,
      overwriteFormulasInSS = overwriteFormulasInSS,
      readPopulationFromCSV = readPopulationFromCSV
    ),
    wholeFields = list(
      parameterSets = parameterSets,
      initialConditions = initialConditions,
      outputPaths = outputPaths
    )
  )

  .assertNoOverwriteClash(
    id,
    names(self$definitions$scenarios),
    "scenario",
    overwrite
  )
  call <- .call
  scenarios <- .collectCanonicalizedRefs(lapply(seq_len(n), function(i) {
    .buildScenarioEntry(self, id[[i]], perDefinition[[i]], call = call)
  }))

  newScenarios <- private$.getSection("scenarios") %||% list()
  for (i in seq_len(n)) {
    newScenarios[[id[[i]]]] <- scenarios[[i]]
  }
  private$.setSection("scenarios", newScenarios)
  invisible(self)
}

# Build one `Scenario` record from its id and per-definition field list, running
# the same foreign-key + structural checks `addScenario()` always has. Aborts
# naming the scenario on a problem. `call` attributes the abort to the public
# caller.
#
# @keywords internal
# @noRd
.buildScenarioEntry <- function(
  project,
  id,
  fields,
  call = rlang::caller_env()
) {
  errors <- character()
  modelFile <- fields$modelFile
  if (
    !is.character(modelFile) ||
      length(modelFile) != 1L ||
      is.na(modelFile) ||
      nchar(modelFile) == 0
  ) {
    errors <- c(errors, "modelFile must be a non-empty string")
  }

  # Canonicalize the foreign-key references the moment they enter the API, so a
  # definition and any later reference made from the same typed string land on
  # the same canonical id.
  individual <- .canonicalizeIdRef(fields$individual)
  population <- .canonicalizeIdRef(fields$population)
  application <- .canonicalizeIdRef(fields$application)
  parameterSets <- .canonicalizeIdRef(fields$parameterSets)
  initialConditions <- .canonicalizeIdRef(fields$initialConditions)
  outputPaths <- .canonicalizeIdRef(fields$outputPaths)
  # Collapse a repeated id (first-seen order) so the same output path is never
  # resolved, run, or plotted twice. A repeat is not an error, just redundant.
  if (!is.null(outputPaths)) {
    outputPaths <- unique(outputPaths)
  }

  errors <- c(
    errors,
    .checkScalarScenarioFK(
      individual,
      "individual",
      project$definitions$individuals,
      "individuals"
    ),
    .checkScalarScenarioFK(
      population,
      "population",
      project$definitions$populations,
      "populations"
    ),
    .checkScalarScenarioFK(
      application,
      "application",
      project$definitions$applications,
      "applications"
    ),
    .checkVectorScenarioFK(
      parameterSets,
      "parameterSets",
      project$definitions$parameterSets,
      "project$definitions$parameterSets"
    ),
    .checkVectorScenarioFK(
      initialConditions,
      "initialConditions",
      project$definitions$initialConditions,
      "project$definitions$initialConditions"
    ),
    .checkVectorScenarioFK(
      outputPaths,
      "outputPaths",
      project$definitions$outputPaths,
      "outputPaths"
    )
  )

  if (length(errors) > 0L) {
    cli::cli_abort(
      c(
        "Cannot add scenario {.val {id}}:",
        stats::setNames(errors, rep("x", length(errors)))
      ),
      call = call
    )
  }

  steadyStateTimeUnit <- fields$steadyStateTimeUnit
  Scenario(
    scenarioName = id,
    modelFile = modelFile,
    applicationProtocol = application %||% NA,
    individualId = individual,
    populationId = population,
    outputPaths = if (!is.null(outputPaths)) {
      stats::setNames(
        unlist(project$definitions$outputPaths[outputPaths], use.names = FALSE),
        outputPaths
      )
    },
    readPopulationFromCSV = fields$readPopulationFromCSV,
    simulateSteadyState = fields$steadyState,
    simulationTime = if (!is.null(fields$simulationTime)) {
      .parseSimulationTimeIntervals(fields$simulationTime)
    },
    simulationTimeUnit = fields$simulationTimeUnit,
    # The field contract stores steadyStateTime in the base unit (minutes);
    # convert from the user-declared unit so a non-minute unit round-trips
    # correctly (the serializer converts back to steadyStateTimeUnit).
    steadyStateTime = ospsuite::toBaseUnit(
      quantityOrDimension = ospDimensions$Time,
      values = fields$steadyStateTime,
      unit = steadyStateTimeUnit
    ),
    steadyStateTimeUnit = steadyStateTimeUnit,
    overwriteFormulasInSS = fields$overwriteFormulasInSS,
    modelParameterSets = parameterSets,
    initialConditions = initialConditions
  )
}

#' Remove one or more scenarios from a Project
#' @param project A `Project` object.
#' @param id Character vector of scenario ids to remove in one write-through.
#'   Each is canonicalized the same way [addScenario()] canonicalizes it, so
#'   the same typed id removes what it created. A not-found id warns and is
#'   skipped.
#' @returns The `project` object, invisibly.
#' @export
#' @family scenario
removeScenario <- function(project, id) {
  validateIsOfType(project, "Project")
  project$removeScenario(id)
}

# Implementation behind `project$removeScenario()` / `removeScenario()`.
#
# @keywords internal
# @noRd
.removeScenario_impl <- function(self, private, id, .call) {
  rlang::local_error_call(.call)
  .assertIdVector(id)
  id <- .canonicalizeId(id)

  missingIds <- setdiff(id, names(self$definitions$scenarios))
  if (length(missingIds) > 0L) {
    cli::cli_warn("scenario {.val {missingIds}} not found; no-op.")
  }
  toRemove <- intersect(id, names(self$definitions$scenarios))
  for (one in toRemove) {
    .warnIfReferenced(self, "scenario", one)
  }
  if (length(toRemove) == 0L) {
    return(invisible(self))
  }
  scenarios <- private$.getSection("scenarios")
  scenarios[toRemove] <- NULL
  private$.setSection("scenarios", scenarios)
  invisible(self)
}

#' Modify fields of an existing scenario
#'
#' @description Changes one or more fields of the scenario identified by
#'   `id` and persists the change the same way [addScenario()]
#'   does (write-through to the scenario definition). The section accessor
#'   `project$definitions$scenarios` is read-only, so this is the way to revise
#'   an existing scenario: read it if you need the current values
#'   (`sc <- project$definitions$scenarios[[name]]`), then pass the changes here
#'   (`setScenario(project, name, ...)`).
#'
#'   Only the arguments you pass are changed; every other field keeps its
#'   current value (partial update). For an optional field, passing `NULL`
#'   clears it (e.g. `individual = NULL` detaches the individual), whereas
#'   omitting the argument leaves it untouched. The required `modelFile`
#'   cannot be cleared.
#'
#'   References are validated exactly as in [addScenario()]: every supplied
#'   foreign-key argument (`individual`, `population`, `application`,
#'   `parameterSets`, `initialConditions`, `outputPaths`) must resolve in the
#'   project, and the changed scenario must pass structural validation before it
#'   is written, so an invalid change touches neither memory nor disk. A
#'   dangling reference is rejected eagerly with an immediate error, not
#'   deferred to [validateProject()].
#'
#'   The call vectorizes over a vector of ids (see the recycling rule under
#'   Details): a supplied scalar-per-definition field is recycled or aligned
#'   across `id`, and the whole-vector fields `parameterSets` /
#'   `initialConditions` / `outputPaths` are applied whole to every scenario. A
#'   field left unsupplied is untouched on every scenario.
#'
#' @inherit vectorizedAuthoring details
#' @inheritParams addScenario
#' @param id Character vector. Ids of the scenarios to modify. Each is
#'   canonicalized the same way [addScenario()] canonicalizes it, and must
#'   already exist in `scenarios` definitions.
#' @param simulationTimeUnit Character time-unit string. Omitting the argument
#'   leaves the current value untouched (there is no default; this is a
#'   partial update).
#' @param steadyState Logical, whether to simulate steady state. Omitting the
#'   argument leaves the current value untouched (there is no default; this is
#'   a partial update).
#' @param steadyStateTime Numeric steady-state time in `steadyStateTimeUnit`.
#'   Omitting the argument leaves the current value untouched (there is no
#'   default; this is a partial update).
#' @param steadyStateTimeUnit Character unit for `steadyStateTime`. Omitting
#'   the argument leaves the current value untouched (there is no default; this
#'   is a partial update).
#' @param overwriteFormulasInSS Logical, whether to overwrite formulas during
#'   steady state. Omitting the argument leaves the current value untouched
#'   (there is no default; this is a partial update).
#' @param readPopulationFromCSV Logical, whether to load the population from
#'   CSV. Omitting the argument leaves the current value untouched (there is no
#'   default; this is a partial update).
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family scenario
setScenario <- function(
  project,
  id,
  modelFile,
  individual,
  population,
  application,
  parameterSets,
  initialConditions,
  outputPaths,
  simulationTime,
  simulationTimeUnit,
  steadyState,
  steadyStateTime,
  steadyStateTimeUnit,
  overwriteFormulasInSS,
  readPopulationFromCSV
) {
  validateIsOfType(project, "Project")

  # Capture only the fields the caller actually supplied (partial update). A
  # supplied `NULL` (e.g. `individual = NULL`) clears the field, distinct
  # from an unsupplied argument; the `x[name] <- list(value)` form preserves a
  # NULL-valued supplied field as a present-but-NULL list element (a plain
  # `x$name <- NULL` would instead drop the name). Forward only the supplied
  # fields to the method so its `...` carries exactly what the user gave, which
  # is how the method distinguishes "clear this" from "leave untouched".
  supplied <- list()
  if (!missing(modelFile)) {
    supplied["modelFile"] <- list(modelFile)
  }
  if (!missing(individual)) {
    supplied["individual"] <- list(individual)
  }
  if (!missing(population)) {
    supplied["population"] <- list(population)
  }
  if (!missing(application)) {
    supplied["application"] <- list(application)
  }
  if (!missing(parameterSets)) {
    supplied["parameterSets"] <- list(parameterSets)
  }
  if (!missing(initialConditions)) {
    supplied["initialConditions"] <- list(initialConditions)
  }
  if (!missing(outputPaths)) {
    supplied["outputPaths"] <- list(outputPaths)
  }
  if (!missing(simulationTime)) {
    supplied["simulationTime"] <- list(simulationTime)
  }
  if (!missing(simulationTimeUnit)) {
    supplied["simulationTimeUnit"] <- list(simulationTimeUnit)
  }
  if (!missing(steadyState)) {
    supplied["steadyState"] <- list(steadyState)
  }
  if (!missing(steadyStateTime)) {
    supplied["steadyStateTime"] <- list(steadyStateTime)
  }
  if (!missing(steadyStateTimeUnit)) {
    supplied["steadyStateTimeUnit"] <- list(steadyStateTimeUnit)
  }
  if (!missing(overwriteFormulasInSS)) {
    supplied["overwriteFormulasInSS"] <- list(overwriteFormulasInSS)
  }
  if (!missing(readPopulationFromCSV)) {
    supplied["readPopulationFromCSV"] <- list(readPopulationFromCSV)
  }

  do.call(project$setScenario, c(list(id), supplied))
}

# Implementation behind `project$setScenario()` / `setScenario()`. The `...`
# carries only the fields the caller supplied (partial update); a present but
# NULL field clears it, an absent field is left untouched.
#
# @keywords internal
# @noRd
.setScenario_impl <- function(self, private, id, ..., .call) {
  rlang::local_error_call(.call)
  .assertIdVector(id)
  id <- .canonicalizeId(id)
  n <- length(id)
  missingIds <- setdiff(id, names(self$definitions$scenarios))
  if (length(missingIds) > 0L) {
    cli::cli_abort(c(
      "Cannot modify scenario {.val {missingIds}}: it does not exist.",
      "i" = "Use {.fn addScenario} to create it first."
    ))
  }

  dots <- list(...)
  wholeNames <- intersect(
    c("parameterSets", "initialConditions", "outputPaths"),
    names(dots)
  )
  scalarSupplied <- dots[setdiff(names(dots), wholeNames)]
  wholeSupplied <- dots[wholeNames]

  perDefinition <- .alignAuthoringArgs(
    id,
    scalarFields = scalarSupplied,
    wholeFields = wholeSupplied
  )
  suppliedNames <- names(dots)

  call <- .call
  updated <- .collectCanonicalizedRefs(lapply(seq_len(n), function(i) {
    .setOneScenario(
      self,
      id[[i]],
      perDefinition[[i]][suppliedNames],
      call = call
    )
  }))

  scenarios <- private$.getSection("scenarios")
  for (i in seq_len(n)) {
    scenarios[[id[[i]]]] <- updated[[i]]
  }
  private$.setSection("scenarios", scenarios)
  invisible(self)
}

# Apply a partial-update field set to one existing scenario, returning the
# updated `Scenario` record. `fields` carries only the names the caller
# supplied (a present-but-NULL field clears it). Runs the same FK + apply
# logic the scalar `setScenario()` always has. Aborts naming the scenario.
#
# @keywords internal
# @noRd
.setOneScenario <- function(project, id, fields, call = rlang::caller_env()) {
  sc <- project$definitions$scenarios[[id]]
  errors <- character()
  supplied <- names(fields)

  # Canonicalize the supplied foreign-key references (matching the definition
  # side) before validating, so a reference resolves to its definition.
  if ("individual" %in% supplied) {
    fields$individual <- .canonicalizeIdRef(fields$individual)
  }
  if ("population" %in% supplied) {
    fields$population <- .canonicalizeIdRef(fields$population)
  }
  if ("application" %in% supplied) {
    fields$application <- .canonicalizeIdRef(fields$application)
  }
  if ("parameterSets" %in% supplied) {
    fields$parameterSets <- .canonicalizeIdRef(fields$parameterSets)
  }
  if ("initialConditions" %in% supplied) {
    fields$initialConditions <- .canonicalizeIdRef(fields$initialConditions)
  }
  if ("outputPaths" %in% supplied) {
    fields$outputPaths <- .canonicalizeIdRef(fields$outputPaths)
    # Collapse a repeated id (first-seen order) so the same output path is
    # never resolved, run, or plotted twice; a repeat is redundant, not an
    # error.
    if (!is.null(fields$outputPaths)) {
      fields$outputPaths <- unique(fields$outputPaths)
    }
  }

  # Validate only the foreign-key arguments the caller actually supplied,
  # reusing the exact checks `addScenario()` runs.
  if ("individual" %in% supplied) {
    errors <- c(
      errors,
      .checkScalarScenarioFK(
        fields$individual,
        "individual",
        project$definitions$individuals,
        "individuals"
      )
    )
  }
  if ("population" %in% supplied) {
    errors <- c(
      errors,
      .checkScalarScenarioFK(
        fields$population,
        "population",
        project$definitions$populations,
        "populations"
      )
    )
  }
  if ("application" %in% supplied) {
    errors <- c(
      errors,
      .checkScalarScenarioFK(
        fields$application,
        "application",
        project$definitions$applications,
        "applications"
      )
    )
  }
  if ("parameterSets" %in% supplied) {
    errors <- c(
      errors,
      .checkVectorScenarioFK(
        fields$parameterSets,
        "parameterSets",
        project$definitions$parameterSets,
        "project$definitions$parameterSets"
      )
    )
  }
  if ("initialConditions" %in% supplied) {
    errors <- c(
      errors,
      .checkVectorScenarioFK(
        fields$initialConditions,
        "initialConditions",
        project$definitions$initialConditions,
        "project$definitions$initialConditions"
      )
    )
  }
  if ("outputPaths" %in% supplied) {
    errors <- c(
      errors,
      .checkVectorScenarioFK(
        fields$outputPaths,
        "outputPaths",
        project$definitions$outputPaths,
        "outputPaths"
      )
    )
  }

  if (length(errors) > 0L) {
    cli::cli_abort(
      c(
        "Cannot modify scenario {.val {id}}:",
        stats::setNames(errors, rep("x", length(errors)))
      ),
      call = call
    )
  }

  # Apply each supplied field to the working record. The record's
  # `applicationProtocol` field carries `NA` (not `NULL`) when absent,
  # matching the Scenario() contract.
  if ("modelFile" %in% supplied) {
    sc$modelFile <- fields$modelFile
  }
  if ("individual" %in% supplied) {
    sc$individualId <- fields$individual
  }
  if ("population" %in% supplied) {
    sc$populationId <- fields$population
    sc$simulationType <- if (is.null(fields$population)) {
      "Individual"
    } else {
      "Population"
    }
  }
  if ("application" %in% supplied) {
    sc$applicationProtocol <- fields$application %||% NA
  }
  if ("parameterSets" %in% supplied) {
    sc$modelParameterSets <- fields$parameterSets
  }
  if ("initialConditions" %in% supplied) {
    sc$initialConditions <- fields$initialConditions
  }
  if ("outputPaths" %in% supplied) {
    sc$outputPaths <- if (is.null(fields$outputPaths)) {
      NULL
    } else {
      stats::setNames(
        unlist(
          project$definitions$outputPaths[fields$outputPaths],
          use.names = FALSE
        ),
        fields$outputPaths
      )
    }
  }
  if ("simulationTime" %in% supplied) {
    sc$simulationTime <- if (is.null(fields$simulationTime)) {
      NULL
    } else {
      .parseSimulationTimeIntervals(fields$simulationTime)
    }
  }
  if ("simulationTimeUnit" %in% supplied) {
    sc$simulationTimeUnit <- fields$simulationTimeUnit
  }
  if ("steadyState" %in% supplied) {
    sc$simulateSteadyState <- fields$steadyState
  }
  if ("steadyStateTime" %in% supplied || "steadyStateTimeUnit" %in% supplied) {
    # steadyStateTime is stored in the base unit (minutes). When a value is
    # supplied it is interpreted under the effective unit (the newly supplied
    # unit if given, else the record's current unit) and converted to the base
    # value. A unit-only change is a pure relabel: it updates the declared unit
    # while leaving the stored base value untouched (converting there would
    # rescale the physical duration).
    newUnit <- if ("steadyStateTimeUnit" %in% supplied) {
      fields$steadyStateTimeUnit
    } else {
      sc$steadyStateTimeUnit
    }
    sc$steadyStateTimeUnit <- newUnit
    if ("steadyStateTime" %in% supplied) {
      sc$steadyStateTime <- if (is.null(newUnit)) {
        fields$steadyStateTime
      } else {
        ospsuite::toBaseUnit(
          quantityOrDimension = ospDimensions$Time,
          values = fields$steadyStateTime,
          unit = newUnit
        )
      }
    }
  }
  if ("overwriteFormulasInSS" %in% supplied) {
    sc$overwriteFormulasInSS <- fields$overwriteFormulasInSS
  }
  if ("readPopulationFromCSV" %in% supplied) {
    sc$readPopulationFromCSV <- fields$readPopulationFromCSV
  }
  sc
}

#' Rename an existing scenario
#'
#' @description Renames the scenario currently keyed `id` to `newId`,
#'   preserving its configuration. The change is write-through: the scenario's
#'   old definition is removed and a new one written under `newId`, the
#'   in-memory key changes, and the record's stored name is updated to match
#'   the new key so a reload round-trips (the name-equals-key invariant the
#'   project relies on).
#'
#'   Both `id` and `newId` are canonicalized the same way [addScenario()]
#'   canonicalizes an id (lowercased, made a safe single-path-segment id, with
#'   a warning when the value changed), so the same typed strings used to
#'   create and reference a scenario resolve consistently.
#'
#' @param project A `Project` object.
#' @param id Character. Id of the scenario to rename; must exist in
#'   `scenarios` definitions (after canonicalization).
#' @param newId Character. New id for the scenario; its canonical form must
#'   not already belong to a different scenario.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family scenario
renameScenario <- function(project, id, newId) {
  validateIsOfType(project, "Project")
  project$renameScenario(id, newId)
}

# Implementation behind `project$renameScenario()` / `renameScenario()`.
#
# @keywords internal
# @noRd
.renameScenario_impl <- function(self, private, id, newId, .call) {
  rlang::local_error_call(.call)
  id <- .assertScenarioIdArg(id, "id")
  newId <- .assertScenarioIdArg(newId, "newId")

  id <- .canonicalizeId(id)
  .assertScenarioExists(self, id, "rename")
  newId <- .canonicalizeId(newId)

  if (identical(id, newId)) {
    return(invisible(self))
  }
  .assertScenarioTargetFree(self, newId)

  sc <- self$definitions$scenarios[[id]]
  # Keep the record's stored name in step with its new key so the definition file
  # the write-through emits (`name = sc$scenarioName`) and the key agree, which
  # is what `.validateScenarioStructure()` enforces.
  sc$scenarioName <- newId

  # Renaming de-references the old id just like removing it: warn about any
  # holder still naming the scenario by its old id before the key changes.
  .warnIfReferenced(self, "scenario", id)

  # Rebuild the whole section in one write so the write-through diff sees the
  # new key (written through to `newId`'s file) and the gone key (its file
  # removed) together.
  scenarios <- private$.getSection("scenarios")
  scenarios[[id]] <- NULL
  scenarios[[newId]] <- sc
  private$.setSection("scenarios", scenarios)

  invisible(self)
}

#' Duplicate an existing scenario
#'
#' @description Creates a deep copy of the scenario currently keyed `id` under
#'   `newId`, leaving the original untouched. The copy is a new definition
#'   written through to `newId` (the in-memory store and the on-disk project
#'   both gain an independent scenario).
#'
#'   Both `id` and `newId` are canonicalized the same way [addScenario()]
#'   canonicalizes an id (lowercased, made a safe single-path-segment id, with
#'   a warning when the value changed).
#'
#' @param project A `Project` object.
#' @param id Character. Id of the scenario to copy; must exist in
#'   `scenarios` definitions (after canonicalization).
#' @param newId Character. Id for the new copy; its canonical form must not
#'   already belong to an existing scenario.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family scenario
duplicateScenario <- function(project, id, newId) {
  validateIsOfType(project, "Project")
  project$duplicateScenario(id, newId)
}

# Implementation behind `project$duplicateScenario()` / `duplicateScenario()`.
#
# @keywords internal
# @noRd
.duplicateScenario_impl <- function(self, private, id, newId, .call) {
  rlang::local_error_call(.call)
  id <- .assertScenarioIdArg(id, "id")
  newId <- .assertScenarioIdArg(newId, "newId")

  id <- .canonicalizeId(id)
  .assertScenarioExists(self, id, "duplicate")
  newId <- .canonicalizeId(newId)
  .assertScenarioTargetFree(self, newId)

  # A `Scenario` is a plain-data list with copy semantics, so this is already a
  # deep, independent copy; only its stored name has to follow the new key.
  copy <- private$.getSection("scenarios")[[id]]
  copy$scenarioName <- newId

  scenarios <- private$.getSection("scenarios")
  scenarios[[newId]] <- copy
  private$.setSection("scenarios", scenarios)

  invisible(self)
}

# Validate that a scenario id argument (`id` / `newId` of `renameScenario()` /
# `duplicateScenario()`) is a non-empty string, aborting with the argument
# name when it is not. Returns the value unchanged so callers can write
# `id <- .assertScenarioIdArg(id, "id")`. `call` attributes the abort to the
# public caller.
#
# @keywords internal
# @noRd
.assertScenarioIdArg <- function(value, argName, call = rlang::caller_env()) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      nchar(value) == 0
  ) {
    cli::cli_abort("{.arg {argName}} must be a non-empty string", call = call)
  }
  value
}

# Abort when a (canonical) scenario `id` is not in the project, with a
# "did you mean '...'?" suggestion drawn from the existing scenario ids, the
# same way the cross-reference validator phrases a dangling reference. `call`
# attributes the abort to the public caller.
#
# @keywords internal
# @noRd
.assertScenarioExists <- function(
  project,
  id,
  action,
  call = rlang::caller_env()
) {
  if (id %in% names(project$definitions$scenarios)) {
    return(invisible(NULL))
  }
  suggestion <- .suggestSuffix(id, names(project$definitions$scenarios))
  cli::cli_abort(
    c(
      "Cannot {action} scenario {.val {id}}: it does not exist.",
      "i" = paste0(
        "Available scenarios: ",
        "{.val {names(project$definitions$scenarios)}}",
        suggestion
      )
    ),
    call = call
  )
}

# Abort when a (canonical) target id `newId` already belongs to a scenario, so
# a rename/duplicate never silently overwrites an existing scenario. `call`
# attributes the abort to the public caller.
#
# @keywords internal
# @noRd
.assertScenarioTargetFree <- function(
  project,
  newId,
  call = rlang::caller_env()
) {
  if (newId %in% names(project$definitions$scenarios)) {
    cli::cli_abort(
      "Cannot use {.val {newId}}: a scenario with that id already exists.",
      call = call
    )
  }
  invisible(NULL)
}

# Foreign-key validation helpers shared by `addScenario()` and
# `setScenario()`. Each returns a character vector of error strings (empty
# when the value is valid or NULL), so the caller can accumulate and report
# all reference problems at once.

# Validate a scalar foreign-key argument (`individual`, `population`,
# `application`): NULL is allowed (the reference is absent); a
# non-empty string must resolve as a name in `lookup`.
#
# @keywords internal
# @noRd
.checkScalarScenarioFK <- function(value, argName, lookup, lookupLabel) {
  if (is.null(value)) {
    return(character())
  }
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      nchar(value) == 0
  ) {
    return(paste0(argName, " must be a non-empty string or NULL"))
  }
  if (!(value %in% names(lookup))) {
    return(paste0(argName, " '", value, "' not found in ", lookupLabel))
  }
  character()
}

# Validate a vector foreign-key argument (`parameterSets`,
# `outputPaths`): NULL is allowed; a non-empty character vector must have
# no NA/empty entries and every element must resolve in `lookup`.
#
# @keywords internal
# @noRd
.checkVectorScenarioFK <- function(value, argName, lookup, lookupLabel) {
  if (is.null(value)) {
    return(character())
  }
  if (
    !is.character(value) ||
      length(value) == 0L ||
      any(is.na(value)) ||
      any(nchar(value) == 0)
  ) {
    return(paste0(
      argName,
      " must be a non-empty character vector with no NA or empty entries"
    ))
  }
  bad <- setdiff(value, names(lookup))
  if (length(bad) > 0L) {
    return(paste0(
      argName,
      " not found in ",
      lookupLabel,
      ": ",
      paste(bad, collapse = ", ")
    ))
  }
  character()
}
