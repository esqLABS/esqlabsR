# Project (JSON) validation framework.
#
# Implements Chapter 4 of the JSON-as-primary-input refactor (see
# .claude/superpowers/specs/2026-05-04-json-based-project-chapters.md).
#
# This file is the JSON-driven counterpart to the Excel-driven
# `validateAllConfigurations()` (R/validation-all-configurations.R).
#
# The dispatcher is a named list of adapters in `.validationAdapters`.
# Each section file (R/utilities-scenarios.R, R/utilities-individual.R,
# ...) defines a top-level `.<section>ValidatorAdapter <- function(project)`
# that pulls the right slice of the project and calls a section-local
# `.validate<Section>()` function. Adding a new section means dropping
# an adapter into the section's R file and registering it in
# `.validationAdapters` below.
#
# `crossReferences` is intentionally NOT in the adapter list, it runs
# after all section validators because it inspects their partial
# results to decide whether to skip itself (skip on prior critical
# errors). It is appended as a fixed final phase by the dispatcher
# rather than masquerading as a section.

# Public API ----

#' Validate a Project
#'
#' Runs every section validator (and a cross-reference pass) against a
#' parsed `Project` and returns a named list of `validationResult`
#' objects, one per section, in canonical order. Sets the project's
#' `validatedSinceMutation` flag when no section produced critical
#' errors so subsequent `runScenarios()` / `createPlots()` calls can
#' skip a redundant validation pass.
#'
#' @param project A `Project` object (typically produced by
#'   [loadProject()]). Path inputs are not accepted here; load the
#'   project first.
#' @return Named list of `validationResult` objects with class
#'   `"ValidationResults"`. Order matches `.validationAdapters`,
#'   with `crossReferences` last.
#' @export
#' @seealso [validateAllConfigurations()] for the legacy Excel-driven
#'   validator, [isAnyCriticalErrors()], [validationSummary()].
#' @examples
#' \dontrun{
#' project <- loadProject("Project.json")
#' results <- validateProject(project)
#' if (isAnyCriticalErrors(results)) {
#'   print(validationSummary(results))
#' }
#' }
validateProject <- function(project) {
  if (!inherits(project, "Project")) {
    cli::cli_abort(
      "{.arg project} must be a {.cls Project} object; got {.cls {class(project)[[1]]}}."
    )
  }

  results <- .runProjectValidation(project, sections = NULL)

  if (!isAnyCriticalErrors(results)) {
    project$.markValidated()
  }

  results
}

# Section validator dispatch ----

#' Canonical ordered registry of section validator adapters
#'
#' Named list mapping each section name to the adapter that validates
#' it. Order determines the order of keys in the `validateProject()`
#' result. `crossReferences` is not listed here; it is appended as a
#' fixed final phase by [.runProjectValidation()] so it can see
#' partial section results.
#'
#' Building the list at package load means a missing or misspelled
#' adapter symbol surfaces as a build-time error, rather than at
#' runtime when a user calls [validateProject()].
#'
#' @keywords internal
#' @noRd
.validationAdapters <- list(
  individuals = .individualsValidatorAdapter,
  individualParameterSets = .individualParameterSetsValidatorAdapter,
  populations = .populationsValidatorAdapter,
  scenarios = .scenariosValidatorAdapter,
  outputPaths = .outputPathsValidatorAdapter,
  modelParameterSets = .modelParameterSetsValidatorAdapter,
  applications = .applicationsValidatorAdapter,
  applicationParameterSets = .applicationParameterSetsValidatorAdapter,
  plots = .plotsValidatorAdapter,
  observedData = .observedDataValidatorAdapter,
  parameterIdentification = .parameterIdentificationValidatorAdapter
)

#' Run a (possibly targeted) project validation
#'
#' Internal orchestration helper. Runs the requested section validators
#' in canonical order and returns a `ValidationResults` list.
#' `crossReferences` is always run last when included so it sees prior
#' section results.
#'
#' @param project A loaded `Project` object.
#' @param sections Character vector of section names to validate, or
#'   `NULL` for a full validation. Unknown names are dropped silently.
#' @return Named list of `validationResult` objects with class
#'   `"ValidationResults"`. Only requested sections are present.
#' @keywords internal
#' @noRd
.runProjectValidation <- function(project, sections = NULL) {
  known <- c(names(.validationAdapters), "crossReferences")
  if (is.null(sections)) {
    sections <- known
  } else {
    sections <- intersect(known, sections)
  }

  results <- list()
  for (section in sections) {
    if (section == "crossReferences") {
      results[[section]] <- .validateCrossReferences(project, results)
      next
    }
    results[[section]] <- .validationAdapters[[section]](project)
  }

  class(results) <- c("ValidationResults", class(results))
  results
}

#' Ensure a Project passes validation before an operation
#'
#' Runs targeted validation for the sections an operation depends on,
#' and aborts with a formatted multi-error message if any critical
#' errors are found. Short-circuits when the project has been fully
#' validated since its last mutation (the `validatedSinceMutation`
#' flag).
#'
#' This helper does not itself flip the cache flag, because it only
#' runs a subset of validators. Only `validateProject()` (a full run)
#' sets the flag.
#'
#' @param project A `Project` object.
#' @param sections Non-empty character vector of section names required
#'   by the calling operation.
#' @param opName Short label used in the abort message (e.g.
#'   `"runScenarios"`).
#' @return `invisible(NULL)` on success.
#' @keywords internal
#' @noRd
.ensureValid <- function(project, sections, opName) {
  if (isTRUE(project$validatedSinceMutation)) {
    return(invisible(NULL))
  }

  results <- .runProjectValidation(project, sections = sections)

  if (isAnyCriticalErrors(results)) {
    .abortValidationErrors(results, opName)
  }

  invisible(NULL)
}

#' Format and abort with the critical errors found in a validation run
#'
#' @keywords internal
#' @noRd
.abortValidationErrors <- function(results, opName) {
  lines <- character()
  for (section in names(results)) {
    r <- results[[section]]
    if (!inherits(r, "validationResult") || !r$has_critical_errors()) {
      next
    }
    for (e in r$critical_errors) {
      lines <- c(lines, paste0("[", section, "] ", e$message))
    }
  }
  bullets <- stats::setNames(lines, rep("x", length(lines)))
  cli::cli_abort(c(
    "Cannot {opName}: project has {length(lines)} critical validation \\
    error{?s}.",
    bullets,
    "i" = "Run {.code validateProject(project)} for a full report."
  ))
}

# Shared helpers used by section adapters ----

#' Warn if a removed entity is still referenced elsewhere in the project.
#'
#' Walks the project to find inbound references to `id` of the given
#' `entityType` and emits a `cli::cli_warn()` listing them. Used by the
#' `remove*()` mutators: removal proceeds anyway, leaving the dangling
#' reference for the next [validateProject()] call to surface.
#'
#' @param project A `Project` object.
#' @param entityType One of `"individual"`, `"population"`, `"application"`,
#'   `"modelParameterSet"`, `"individualParameterSet"`,
#'   `"applicationParameterSet"`, `"outputPath"`.
#' @param id Character scalar of the id being removed.
#' @return `invisible(NULL)`.
#' @keywords internal
#' @noRd
.warnIfReferenced <- function(project, entityType, id) {
  if (entityType == "individualParameterSet") {
    holders <- character()
    for (indId in names(project$individuals %||% list())) {
      ind <- project$individuals[[indId]]
      if (id %in% (ind$parameterSets %||% character(0))) {
        holders <- c(holders, indId)
      }
    }
    if (length(holders) > 0) {
      cli::cli_warn(c(
        "Removed individualParameterSet {.val {id}} is still referenced by {length(holders)} individual{?s}:",
        "*" = "{holders}",
        "i" = "These individuals now have a dangling reference. Update or remove them."
      ))
    }
    return(invisible(NULL))
  }
  if (entityType == "applicationParameterSet") {
    holders <- character()
    for (appId in names(project$applications %||% list())) {
      app <- project$applications[[appId]]
      if (id %in% (app$parameterSets %||% character(0))) {
        holders <- c(holders, appId)
      }
    }
    if (length(holders) > 0) {
      cli::cli_warn(c(
        "Removed applicationParameterSet {.val {id}} is still referenced by {length(holders)} application{?s}:",
        "*" = "{holders}",
        "i" = "These applications now have a dangling reference. Update or remove them."
      ))
    }
    return(invisible(NULL))
  }

  scenarios <- project$scenarios %||% list()
  if (length(scenarios) == 0) {
    return(invisible(NULL))
  }

  refs <- character()
  for (name in names(scenarios)) {
    sc <- scenarios[[name]]
    hit <- switch(
      entityType,
      "individual" = identical(sc$individualId, id),
      "population" = identical(sc$populationId, id),
      "application" = identical(sc$applicationProtocol, id),
      "modelParameterSet" = isTRUE(id %in% sc$modelParameterSets),
      "outputPath" = {
        pathValue <- project$outputPaths[[id]]
        isTRUE(pathValue %in% sc$outputPaths)
      },
      FALSE
    )
    if (isTRUE(hit)) refs <- c(refs, name)
  }

  if (length(refs) > 0) {
    cli::cli_warn(c(
      "Removed {entityType} {.val {id}} is still referenced by {length(refs)} scenario{?s}:",
      "*" = "{refs}",
      "i" = "These scenarios now have a dangling reference. Update or remove them."
    ))
  }
  invisible(NULL)
}

#' Check for duplicate values and add a critical error when found
#'
#' @param ids Character vector of IDs to check.
#' @param fieldName Name of the field for the error message.
#' @param result `validationResult` to mutate.
#' @return The (mutated) `result`, returned for fluent chaining.
#' @keywords internal
#' @noRd
.check_no_duplicates <- function(ids, fieldName, result) {
  dupes <- ids[duplicated(ids) & !is.na(ids)]
  if (length(dupes) > 0) {
    result$add_critical_error(
      "Uniqueness",
      paste0(
        "Duplicate ",
        fieldName,
        " values: ",
        paste(unique(dupes), collapse = ", ")
      )
    )
  }
  result
}

#' Check that required fields are present and non-empty on an entry
#'
#' @param entry Named list / record-like value.
#' @param requiredFields Character vector of field names to check.
#' @param entryName Label for the entry, used in the error message.
#' @param result `validationResult` to mutate.
#' @return The (mutated) `result`.
#' @keywords internal
#' @noRd
.check_required_fields <- function(
  entry,
  requiredFields,
  entryName,
  result
) {
  for (field in requiredFields) {
    val <- entry[[field]]
    if (is.null(val) || (length(val) == 1 && (is.na(val) || val == ""))) {
      result$add_critical_error(
        "Missing Fields",
        paste0(
          "Required field '",
          field,
          "' is missing or empty in ",
          entryName
        )
      )
    }
  }
  result
}

#' Validate a parameter-set structure
#'
#' Shared body used by the model / individual / application
#' parameter-set adapters. Each parameter set is expected to be a list
#' with `paths`, `values`, and `units` parallel vectors.
#'
#' @keywords internal
#' @noRd
.validateParameterSets <- function(parameterSets, sectionName) {
  result <- validationResult$new()

  if (is.null(parameterSets) || length(parameterSets) == 0) {
    result$add_warning("Data", paste0("No ", sectionName, " defined"))
    return(result)
  }

  for (setName in names(parameterSets)) {
    set <- parameterSets[[setName]]
    paths <- set$paths %||% character(0)
    values <- set$values %||% numeric(0)

    if (length(paths) != length(values)) {
      result$add_critical_error(
        "Structure",
        paste0(
          "Set '",
          setName,
          "' in ",
          sectionName,
          ": paths and values have different lengths"
        )
      )
      next
    }

    if (length(paths) == 0) {
      next
    }

    if (any(is.na(paths) | paths == "")) {
      result$add_critical_error(
        "Missing Fields",
        paste0(
          "Set '",
          setName,
          "' in ",
          sectionName,
          " contains empty parameter paths"
        )
      )
    }

    if (!is.numeric(values)) {
      result$add_warning(
        "Data Type",
        paste0(
          "Set '",
          setName,
          "' in ",
          sectionName,
          " contains non-numeric values"
        )
      )
    }

    dupes <- paths[duplicated(paths) & !is.na(paths)]
    if (length(dupes) > 0) {
      result$add_warning(
        "Uniqueness",
        paste0(
          "Duplicate parameter paths in set '",
          setName,
          "': ",
          paste(unique(dupes), collapse = ", ")
        )
      )
    }
  }

  result
}

# Cross-reference validation (monolith) ----

#' Validate cross-references between Project sections
#'
#' Hand-rolled monolith that checks references that span sections:
#' `scenario.individualId/populationId` against the individuals and
#' populations sections, `scenario.modelParameterSets` and
#' `scenario.applicationProtocol` against their respective lookups,
#' `individual.parameterSets` and `application.parameterSets` against
#' the corresponding parameter-set sections, and
#' `dataCombined.simulated.scenario` against scenarios. Skips itself
#' when any prior section validator already flagged a critical error
#' and surfaces a single "skipped" warning naming the checks that were
#' not performed, since cross-references built on broken sections tend
#' to produce noisy spurious failures.
#'
#' Future deepening: the end-state walks per-section `references()`
#' declarations rather than hand-coding the FK list here. Out of scope
#' for Chapter 4.
#'
#' @param project Project object.
#' @param validationResults Already-collected per-section results.
#' @return validationResult.
#' @keywords internal
#' @noRd
.validateCrossReferences <- function(project, validationResults) {
  result <- validationResult$new()

  if (isAnyCriticalErrors(validationResults)) {
    skipped <- c(
      "scenario individualId / populationId references",
      "scenario modelParameterSets references",
      "scenario applicationProtocol references",
      "individual parameterSets references",
      "application parameterSets references",
      "plot dataCombined scenario references",
      "PI scenarios / outputPath references"
    )
    result$add_warning(
      "Skipped",
      paste0(
        "Cross-reference validation skipped due to critical errors. ",
        "Re-run validation after fixing them to also check: ",
        paste(skipped, collapse = "; "),
        "."
      )
    )
    return(result)
  }

  scenarioList <- project$scenarios %||% list()
  individualIds <- names(project$individuals %||% list())
  populationIds <- names(project$populations %||% list())
  modelParamKeys <- names(project$modelParameterSets %||% list())
  applicationKeys <- names(project$applications %||% list())

  for (scName in names(scenarioList)) {
    sc <- scenarioList[[scName]]

    if (
      !is.null(sc$individualId) &&
        !is.na(sc$individualId) &&
        !sc$individualId %in% individualIds
    ) {
      result$add_critical_error(
        "Invalid Reference",
        paste0(
          "Scenario '",
          scName,
          "' references undefined individualId '",
          sc$individualId,
          "'"
        )
      )
    }

    if (
      !is.null(sc$populationId) &&
        !is.na(sc$populationId) &&
        !sc$populationId %in% populationIds
    ) {
      result$add_critical_error(
        "Invalid Reference",
        paste0(
          "Scenario '",
          scName,
          "' references undefined populationId '",
          sc$populationId,
          "'"
        )
      )
    }

    if (!is.null(sc$modelParameterSets) && length(sc$modelParameterSets) > 0) {
      invalidSets <- setdiff(sc$modelParameterSets, modelParamKeys)
      invalidSets <- invalidSets[!is.na(invalidSets) & invalidSets != ""]
      if (length(invalidSets) > 0) {
        result$add_critical_error(
          "Invalid Reference",
          paste0(
            "Scenario '",
            scName,
            "' references undefined model parameter sets: ",
            paste(invalidSets, collapse = ", ")
          )
        )
      }
    }

    if (
      !is.null(sc$applicationProtocol) &&
        !is.na(sc$applicationProtocol) &&
        !sc$applicationProtocol %in% applicationKeys
    ) {
      result$add_critical_error(
        "Invalid Reference",
        paste0(
          "Scenario '",
          scName,
          "' references undefined applicationProtocol '",
          sc$applicationProtocol,
          "'"
        )
      )
    }
  }

  individualSetKeys <- names(project$individualParameterSets %||% list())
  for (id in names(project$individuals %||% list())) {
    refs <- project$individuals[[id]]$parameterSets %||% character(0)
    refs <- as.character(unlist(refs))
    invalid <- setdiff(refs, individualSetKeys)
    if (length(invalid) > 0) {
      result$add_critical_error(
        "Invalid Reference",
        paste0(
          "Individual '",
          id,
          "' references undefined individualParameterSets: ",
          paste(invalid, collapse = ", ")
        )
      )
    }
  }

  applicationSetKeys <- names(project$applicationParameterSets %||% list())
  for (id in names(project$applications %||% list())) {
    refs <- project$applications[[id]]$parameterSets %||% character(0)
    refs <- as.character(unlist(refs))
    invalid <- setdiff(refs, applicationSetKeys)
    if (length(invalid) > 0) {
      result$add_critical_error(
        "Invalid Reference",
        paste0(
          "Application '",
          id,
          "' references undefined applicationParameterSets: ",
          paste(invalid, collapse = ", ")
        )
      )
    }
  }

  dataCombined <- project$plots$dataCombined
  if (!is.null(dataCombined) && length(dataCombined) > 0) {
    referencedScenarios <- unlist(lapply(dataCombined, function(dc) {
      vapply(
        dc$simulated %||% list(),
        function(e) e$scenario %||% NA_character_,
        character(1)
      )
    }))
    referencedScenarios <- referencedScenarios[!is.na(referencedScenarios)]
    invalidScenarios <- setdiff(referencedScenarios, names(scenarioList))
    if (length(invalidScenarios) > 0) {
      result$add_critical_error(
        "Invalid Reference",
        paste0(
          "dataCombined references undefined scenarios: ",
          paste(invalidScenarios, collapse = ", ")
        )
      )
    }
  }

  # parameterIdentification cross-references -----------------------
  piTasks <- project$parameterIdentification %||% list()
  scenarioNames <- names(scenarioList)
  outputPathIds <- names(project$outputPaths %||% list())

  for (taskId in names(piTasks)) {
    task <- piTasks[[taskId]]

    badTaskScenarios <- setdiff(task$scenarios, scenarioNames)
    if (length(badTaskScenarios) > 0L) {
      result$add_critical_error(
        "Invalid Reference",
        paste0(
          "PI task '",
          taskId,
          "' references undefined scenarios: ",
          paste(badTaskScenarios, collapse = ", ")
        )
      )
    }

    for (p in task$parameters) {
      bad <- setdiff(p$scenarios, scenarioNames)
      if (length(bad) > 0L) {
        result$add_critical_error(
          "Invalid Reference",
          paste0(
            "PI task '",
            taskId,
            "', parameter '",
            p$id,
            "' references undefined scenarios: ",
            paste(bad, collapse = ", ")
          )
        )
      }
    }

    for (m in task$outputMappings) {
      badScenarios <- setdiff(m$scenarios, scenarioNames)
      if (length(badScenarios) > 0L) {
        result$add_critical_error(
          "Invalid Reference",
          paste0(
            "PI task '",
            taskId,
            "', outputMapping '",
            m$id,
            "' references undefined scenarios: ",
            paste(badScenarios, collapse = ", ")
          )
        )
      }
      if (!(m$outputPathId %in% outputPathIds)) {
        result$add_critical_error(
          "Invalid Reference",
          paste0(
            "PI task '",
            taskId,
            "', outputMapping '",
            m$id,
            "' references undefined outputPathId '",
            m$outputPathId,
            "'"
          )
        )
      }
    }
  }

  result
}
