# Project (JSON) validation framework.
#
# The dispatcher is a named list of adapters in `.validationAdapters`.
# Each section file (R/scenarios.R, R/individuals.R, R/populations.R,
# ...) defines a top-level `.<section>ValidatorAdapter <- function(project)`
# that pulls the right slice of the project and calls a section-local
# `.validate<Section>()` function. Adding a new section means dropping
# an adapter into the section's R file and registering it in
# `.validationAdapters` below.
#
# `crossReferences` is intentionally NOT in the adapter list: it owns no
# section, it resolves the references that span sections, and it needs
# to know which sections the current run is about (a section adapter is
# handed only its own slice). It is appended as a fixed final phase by
# the dispatcher rather than masquerading as a section.

# validationResult class ----

#' @title validationResult
#' @description R6 class for storing validation results
#' @export
validationResult <- R6::R6Class(
  "validationResult",
  public = list(
    #' @field critical_errors List of critical errors (blocking issues)
    critical_errors = list(),

    #' @field warnings List of warnings (non-blocking issues)
    warnings = list(),

    #' @description Initialize a new ValidationResult
    initialize = function() {
      self$critical_errors <- list()
      self$warnings <- list()
    },

    #' @description Add a critical error
    #' @param category Error category (e.g., "Structure", "Missing Fields", "Uniqueness")
    #' @param message Error message
    #' @param details Optional list with additional details (sheet, row, column)
    addCriticalError = function(category, message, details = NULL) {
      error_entry <- list(
        category = category,
        message = message,
        details = details,
        timestamp = Sys.time()
      )
      self$critical_errors <- append(self$critical_errors, list(error_entry))
    },

    #' @description Add a warning
    #' @param category Warning category (e.g., "Data", "Structure")
    #' @param message Warning message
    #' @param details Optional list with additional details (sheet, row, column)
    addWarning = function(category, message, details = NULL) {
      warning_entry <- list(
        category = category,
        message = message,
        details = details,
        timestamp = Sys.time()
      )
      self$warnings <- append(self$warnings, list(warning_entry))
    },

    #' @description Check if validation passed (no critical errors)
    isValid = function() {
      length(self$critical_errors) == 0
    },

    #' @description Check if there are critical errors
    hasCriticalErrors = function() {
      length(self$critical_errors) > 0
    },

    #' @description Get formatted messages for display
    getFormattedMessages = function() {
      list(
        critical = lapply(self$critical_errors, function(e) {
          paste0("[", e$category, "] ", e$message)
        }),
        warnings = lapply(self$warnings, function(w) {
          paste0("[", w$category, "] ", w$message)
        })
      )
    },

    #' @description Get validation summary
    getSummary = function() {
      list(
        has_critical_errors = self$hasCriticalErrors(),
        critical_error_count = length(self$critical_errors),
        warning_count = length(self$warnings)
      )
    }
  )
)

# Printing ----

#' Print a project validation report
#'
#' Renders the named list of per-section `validationResult` objects that
#' [validateProject()] returns into a human-readable summary, grouped by
#' definition type (the list keys: `scenarios`, `individuals`, and the
#' rest). The structured object itself is unchanged and stays indexable
#' (`results$scenarios$critical_errors`); only the console view differs.
#'
#' The summary opens with overall counts (the same aggregation as
#' [validationSummary()]), then lists each definition type that has at
#' least one issue: a cross marks each critical error, a `!` marks each
#' warning, and the `category` of each entry is shown as a sub-label.
#' Definition types with no issues are folded into a compact
#' "N sections OK" tail. A fully valid result prints a single "no issues"
#' line. Glyphs and styling come from `cli`, so the output degrades
#' gracefully to plain ASCII when unicode or colour is unavailable.
#'
#' @param x A `ValidationResults` object, the value of [validateProject()].
#' @param ... Ignored, present for S3 compatibility.
#' @return `x`, invisibly.
#' @exportS3Method
#' @seealso [validateProject()], [validationSummary()],
#'   [isAnyCriticalErrors()].
print.ValidationResults <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

#' Format a project validation report
#'
#' Builds the character vector of lines that [print.ValidationResults()]
#' writes to the console. Exposed as a `format` method so the rendered
#' report can be captured as a string. See [print.ValidationResults()]
#' for the layout.
#'
#' @param x A `ValidationResults` object, the value of [validateProject()].
#' @param ... Ignored, present for S3 compatibility.
#' @return A character vector, one element per line of the report.
#' @exportS3Method
#' @seealso [print.ValidationResults()], [validateProject()].
format.ValidationResults <- function(x, ...) {
  summary <- validationSummary(x)
  nErrors <- summary$total_critical_errors
  nWarnings <- summary$total_warnings

  header <- "{.strong Validation report}: {nErrors} critical error{?s}, {nWarnings} warning{?s}."
  lines <- cli::format_inline(header)

  if (nErrors == 0 && nWarnings == 0) {
    okGlyph <- cli::col_green(cli::symbol$tick)
    lines <- c(
      lines,
      cli::format_inline("{okGlyph} No issues found.")
    )
    return(lines)
  }

  # Walk the list in its canonical key order; only definition types that are a
  # `validationResult` with at least one entry are shown, the rest are tallied
  # for the compact "OK" tail.
  okSections <- character()
  for (type in names(x)) {
    result <- x[[type]]
    if (!inherits(result, "validationResult")) {
      next
    }
    nTypeIssues <- length(result$critical_errors) + length(result$warnings)
    if (nTypeIssues == 0) {
      okSections <- c(okSections, type)
      next
    }
    lines <- c(lines, cli::format_inline("{.field {type}}"))
    for (e in result$critical_errors) {
      lines <- c(lines, .formatValidationEntry(e, "critical"))
    }
    for (w in result$warnings) {
      lines <- c(lines, .formatValidationEntry(w, "warning"))
    }
  }

  if (length(okSections) > 0) {
    lines <- c(
      lines,
      cli::format_inline("{length(okSections)} section{?s} OK.")
    )
  }

  lines
}

#' Format one critical-error / warning entry as a glyph-prefixed line
#'
#' A cross (styled red) for a critical error, a `!` (styled yellow) for a
#' warning, then the `category` in brackets and the message. The message
#' text is user-controlled, so it is passed as data (not a glue template)
#' to avoid evaluating `{...}` it may contain.
#'
#' @param entry A `list(category, message, details, timestamp)` record.
#' @param kind `"critical"` or `"warning"`.
#' @keywords internal
#' @noRd
.formatValidationEntry <- function(entry, kind) {
  glyph <- if (kind == "critical") {
    cli::col_red(cli::symbol$cross)
  } else {
    cli::col_yellow("!")
  }
  category <- entry$category %||% "General"
  message <- entry$message %||% ""
  cli::format_inline("  {glyph} [{category}] {message}")
}

# Public API ----

#' Validate a Project
#'
#' Runs every section validator (and a cross-reference pass) against a
#' parsed `Project` and returns a named list of `validationResult`
#' objects, one per section, in canonical order. On a clean run (no
#' section produced critical errors) it marks the project validated, so
#' subsequent `runScenarios()` / `createPlots()` calls can skip a
#' redundant validation pass until the next edit.
#'
#' @param project A `Project` object (typically produced by
#'   [loadProject()]). Path inputs are not accepted here; load the
#'   project first.
#' @return Named list of `validationResult` objects with class
#'   `"ValidationResults"`. Order matches `.validationAdapters`,
#'   with `crossReferences` last.
#' @export
#' @seealso [isAnyCriticalErrors()], [validationSummary()],
#'   [print.ValidationResults()].
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
  project$validate()
}

# Implementation behind `project$validate()` / `validateProject()`. Marks the
# project validated through its own `private` when no critical errors surface.
#
# @keywords internal
# @noRd
.validateProject_impl <- function(self, private, .call) {
  rlang::local_error_call(.call)
  results <- .runProjectValidation(self, sections = NULL)

  if (!isAnyCriticalErrors(results)) {
    private$.markValidated()
  }

  results
}

#' @title isAnyCriticalErrors
#'
#' @description Reports whether any section of a validation run produced a
#'   critical error, collapsing the per-section results from
#'   [validateProject()] into a single logical.
#'
#' @param validationResults Named list of class `"ValidationResults"`, the
#'   output of [validateProject()].
#' @return A single logical: `TRUE` if any section has critical errors,
#'   otherwise `FALSE`.
#' @export
#' @seealso [validateProject()], [validationSummary()].
#' @examples
#' \dontrun{
#' project <- loadProject("Project.json")
#' results <- validateProject(project)
#' if (isAnyCriticalErrors(results)) {
#'   print(validationSummary(results))
#' }
#' }
isAnyCriticalErrors <- function(validationResults) {
  any(vapply(
    validationResults,
    function(r) {
      if (inherits(r, "validationResult")) {
        r$hasCriticalErrors()
      } else {
        FALSE
      }
    },
    logical(1)
  ))
}

#' @title validationSummary
#'
#' @description Aggregates the per-section results from [validateProject()]
#'   into overall counts of critical errors and warnings, plus the names of
#'   the sections that produced each.
#'
#' @param validationResults Named list of class `"ValidationResults"`, the
#'   output of [validateProject()].
#' @return A list with `total_critical_errors`, `total_warnings`,
#'   `sections_with_errors`, and `sections_with_warnings`.
#' @export
#' @seealso [validateProject()], [isAnyCriticalErrors()].
#' @examples
#' \dontrun{
#' project <- loadProject("Project.json")
#' results <- validateProject(project)
#' summary <- validationSummary(results)
#' summary$total_critical_errors
#' }
validationSummary <- function(validationResults) {
  summary <- list(
    total_critical_errors = 0,
    total_warnings = 0,
    sections_with_errors = character(),
    sections_with_warnings = character()
  )

  for (name in names(validationResults)) {
    result <- validationResults[[name]]
    if (inherits(result, "validationResult")) {
      if (result$hasCriticalErrors()) {
        summary$total_critical_errors <- summary$total_critical_errors +
          length(result$critical_errors)
        summary$sections_with_errors <- c(summary$sections_with_errors, name)
      }
      if (length(result$warnings) > 0) {
        summary$total_warnings <- summary$total_warnings +
          length(result$warnings)
        summary$sections_with_warnings <- c(
          summary$sections_with_warnings,
          name
        )
      }
    }
  }

  summary
}

# Section validator dispatch ----

#' Canonical ordered registry of section validator adapters
#'
#' Named list mapping each section name to the adapter that validates
#' it. Order determines the order of keys in the `validateProject()`
#' result. `crossReferences` is not listed here; it is appended as a
#' fixed final phase by `.runProjectValidation()` so it can see
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
  populations = .populationsValidatorAdapter,
  scenarios = .scenariosValidatorAdapter,
  outputPaths = .outputPathsValidatorAdapter,
  parameterSets = .parameterSetsValidatorAdapter,
  initialConditions = .initialConditionsValidatorAdapter,
  applications = .applicationsValidatorAdapter,
  plots = .plotsValidatorAdapter,
  dataCombined = .dataCombinedValidatorAdapter,
  observedData = .observedDataValidatorAdapter,
  parameterIdentification = .parameterIdentificationValidatorAdapter
)

#' Run a (possibly targeted) project validation
#'
#' Internal orchestration helper. Runs the requested section validators
#' in canonical order and returns a `ValidationResults` list.
#' `crossReferences` is always run last when included, and is told which
#' sections the run is about so it resolves only the references those
#' sections hold.
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
      results[[section]] <- .validateCrossReferences(project, sections)
      next
    }
    results[[section]] <- .validationAdapters[[section]](project)
  }

  class(results) <- c("ValidationResults", class(results))
  results
}

#' Format and abort with the critical errors found in a validation run
#'
#' @keywords internal
#' @noRd
.abortValidationErrors <- function(results, opName) {
  lines <- character()
  for (section in names(results)) {
    r <- results[[section]]
    if (!inherits(r, "validationResult") || !r$hasCriticalErrors()) {
      next
    }
    for (e in r$critical_errors) {
      lines <- c(lines, paste0("[", section, "] ", e$message))
    }
  }
  # Validation messages embed user-controlled ids (scenario names, paths)
  # that may contain glue metacharacters. Double the braces so cli treats
  # them literally instead of trying to evaluate `{...}` expressions.
  lines <- .escapeCliBraces(lines)
  bullets <- stats::setNames(lines, rep("x", length(lines)))
  cli::cli_abort(c(
    "Cannot {opName}: project has {length(lines)} critical validation \\
    error{?s}.",
    bullets,
    "i" = "Run {.code validateProject(project)} for a full report."
  ))
}

#' Escape glue/cli metacharacters in literal text
#'
#' Doubles `{` and `}` so a string can be placed inside a `cli` message
#' as literal text without `cli` attempting to evaluate `{...}` glue
#' expressions. Used to neutralise user-controlled ids (scenario names,
#' parameter paths) embedded in validation messages.
#'
#' @keywords internal
#' @noRd
.escapeCliBraces <- function(x) {
  x <- gsub("{", "{{", x, fixed = TRUE)
  gsub("}", "}}", x, fixed = TRUE)
}

# Shared helpers used by section adapters ----

#' Warn if a removed definition is still referenced elsewhere in the project.
#'
#' Walks the project to find inbound references to `id` of the given
#' `definitionType` and emits a `cli::cli_warn()` listing them. Used by the
#' `remove*()` mutators: removal proceeds anyway, leaving the dangling
#' reference for the next [validateProject()] call to surface. For
#' `"outputPath"`, both scenario `outputPaths` and parameter identification
#' output mappings are scanned.
#'
#' @param project A `Project` object.
#' @param definitionType One of `"individual"`, `"population"`, `"application"`,
#'   `"parameterSet"`, `"initialConditions"`, `"outputPath"`, `"scenario"`.
#' @param id Character scalar of the id being removed.
#' @return `invisible(NULL)`.
#' @keywords internal
#' @noRd
.warnIfReferenced <- function(project, definitionType, id) {
  if (definitionType == "parameterSet") {
    # A single parameter set can be referenced from three sides: a scenario's
    # `modelParameterSets`, an individual's `parameterSets`, an application's
    # `parameterSets`. Scan all three since they now share one namespace.
    holders <- character()
    scenarios <- project$definitions$scenarios %||% list()
    for (scName in names(scenarios)) {
      if (
        id %in%
          (scenarios[[scName]]$modelParameterSets %||% character(0))
      ) {
        holders <- c(holders, paste0("scenario '", scName, "'"))
      }
    }
    individuals <- project$definitions$individuals %||% list()
    for (indId in names(individuals)) {
      if (id %in% (individuals[[indId]]$parameterSets %||% character(0))) {
        holders <- c(holders, paste0("individual '", indId, "'"))
      }
    }
    applications <- project$definitions$applications %||% list()
    for (appId in names(applications)) {
      if (id %in% (applications[[appId]]$parameterSets %||% character(0))) {
        holders <- c(holders, paste0("application '", appId, "'"))
      }
    }
    if (length(holders) > 0) {
      cli::cli_warn(c(
        "Removed parameterSet {.val {id}} is still referenced by {length(holders)} definition{?s}:",
        "*" = "{holders}",
        "i" = "These now have a dangling reference. Update or remove them."
      ))
    }
    return(invisible(NULL))
  }

  if (definitionType == "initialConditions") {
    # An initial-condition set is referenced from a scenario's
    # `initialConditions` field (a character vector of set ids).
    holders <- character()
    scenarios <- project$definitions$scenarios %||% list()
    for (scName in names(scenarios)) {
      if (
        id %in%
          (scenarios[[scName]]$initialConditions %||% character(0))
      ) {
        holders <- c(holders, paste0("scenario '", scName, "'"))
      }
    }
    if (length(holders) > 0) {
      cli::cli_warn(c(
        "Removed initialConditions {.val {id}} is still referenced by {length(holders)} scenario{?s}:",
        "*" = "{holders}",
        "i" = "These now have a dangling reference. Update or remove them."
      ))
    }
    return(invisible(NULL))
  }

  if (definitionType == "outputPath") {
    piHolders <- character()
    piTasks <- project$definitions$parameterIdentification %||% list()
    for (taskId in names(piTasks)) {
      task <- piTasks[[taskId]]
      for (m in task$outputMappings %||% list()) {
        if (identical(m$outputPathId, id)) {
          piHolders <- c(piHolders, taskId)
        }
      }
    }
    piHolders <- unique(piHolders)
    if (length(piHolders) > 0) {
      cli::cli_warn(c(
        "Removed outputPath {.val {id}} is still referenced by {length(piHolders)} parameter identification task{?s}:",
        "*" = "{piHolders}",
        "i" = "These PI tasks now have a dangling reference. Update or remove them."
      ))
    }
  }

  if (definitionType == "scenario") {
    # A scenario is referenced from the other direction: a `dataCombined`
    # entry's `simulated[*]$scenario` names the scenario whose results it
    # plots. Scan every dataCombined simulated entry for the removed id.
    dcHolders <- character()
    dataCombined <- project$definitions$dataCombined %||% list()
    for (dcId in names(dataCombined)) {
      for (entry in dataCombined[[dcId]]$simulated %||% list()) {
        if (identical(entry$scenario, id)) {
          dcHolders <- c(dcHolders, dcId)
        }
      }
    }
    dcHolders <- unique(dcHolders)
    if (length(dcHolders) > 0) {
      cli::cli_warn(c(
        "Removed scenario {.val {id}} is still referenced by {length(dcHolders)} dataCombined definition{?s}:",
        "*" = "{dcHolders}",
        "i" = "These now have a dangling reference. Update or remove them."
      ))
    }
    return(invisible(NULL))
  }

  scenarios <- project$definitions$scenarios %||% list()
  if (length(scenarios) == 0) {
    return(invisible(NULL))
  }

  refs <- character()
  for (name in names(scenarios)) {
    sc <- scenarios[[name]]
    hit <- switch(
      definitionType,
      "individual" = identical(sc$individualId, id),
      "population" = identical(sc$populationId, id),
      "application" = identical(sc$applicationProtocol, id),
      "outputPath" = {
        # `sc$outputPaths` is a named vector keyed by output-path id (names are
        # the ids, values the resolved paths). Match on the id, not the resolved
        # path value: two ids can share one path (value match over-reports) and
        # an id may resolve to NA (value match misses it).
        isTRUE(id %in% names(sc$outputPaths))
      },
      FALSE
    )
    if (isTRUE(hit)) refs <- c(refs, name)
  }

  if (length(refs) > 0) {
    cli::cli_warn(c(
      "Removed {definitionType} {.val {id}} is still referenced by {length(refs)} scenario{?s}:",
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
.checkNoDuplicates <- function(ids, fieldName, result) {
  dupes <- ids[duplicated(ids) & !is.na(ids)]
  if (length(dupes) > 0) {
    result$addCriticalError(
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
.checkRequiredFields <- function(
  entry,
  requiredFields,
  entryName,
  result
) {
  for (field in requiredFields) {
    val <- entry[[field]]
    if (is.null(val) || (length(val) == 1 && (is.na(val) || val == ""))) {
      result$addCriticalError(
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
#' parameter-set adapters. Each parameter set is the array-of-records
#' shape the parser and the `add*ParameterEntry()` mutators produce: a
#' list of `list(containerPath, parameterName, value, units)` entries.
#' The full parameter path is `containerPath|parameterName`.
#'
#' @keywords internal
#' @noRd
.validateParameterSets <- function(parameterSets, sectionName) {
  result <- validationResult$new()

  if (is.null(parameterSets) || length(parameterSets) == 0) {
    result$addWarning("Data", paste0("No ", sectionName, " defined"))
    return(result)
  }

  for (setName in names(parameterSets)) {
    set <- parameterSets[[setName]]
    if (length(set) == 0) {
      next
    }

    containerPaths <- vapply(
      set,
      function(e) as.character(e$containerPath %||% NA_character_),
      character(1)
    )
    parameterNames <- vapply(
      set,
      function(e) as.character(e$parameterName %||% NA_character_),
      character(1)
    )
    values <- lapply(set, function(e) e$value)

    if (
      any(
        is.na(containerPaths) |
          containerPaths == "" |
          is.na(parameterNames) |
          parameterNames == ""
      )
    ) {
      result$addCriticalError(
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

    nonNumeric <- vapply(
      values,
      function(v) is.null(v) || length(v) != 1L || !is.numeric(v) || is.na(v),
      logical(1)
    )
    if (any(nonNumeric)) {
      result$addWarning(
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

    fullPaths <- paste(containerPaths, parameterNames, sep = "|")
    dupes <- fullPaths[duplicated(fullPaths)]
    if (length(dupes) > 0) {
      result$addWarning(
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

#' Validate the initial-condition sets
#'
#' Each set is the array-of-records shape the parser and
#' `addInitialConditionEntry()` produce: a list of
#' `list(path, value, unit)` entries, one molecule start value each.
#'
#' The rules are the ones `.validateInitialConditionEntryArgs()`
#' (R/parameters.R) already applies on the authoring path, restated here for
#' the entries that never went through it: a set hand-written into the
#' definitions tree, or one built by the Excel importer. `path` and `unit` are
#' critical because a blank either way makes the set unusable
#' (`ospsuite::setQuantityValuesByPath()` rejects a blank unit at run time);
#' a non-numeric `value` and a duplicated `path` are warnings, matching how
#' `.validateParameterSets()` grades the same two problems.
#'
#' @keywords internal
#' @noRd
.validateInitialConditions <- function(initialConditions, sectionName) {
  result <- validationResult$new()

  if (is.null(initialConditions) || length(initialConditions) == 0) {
    result$addWarning("Data", paste0("No ", sectionName, " defined"))
    return(result)
  }

  # Read one field as a scalar string, or `NA` when the entry does not hold
  # exactly one value for it. A hand-edited set can write `"unit": []` or
  # `"unit": ["mg", "g"]`, either of which would otherwise abort `vapply()`
  # with an internal length error instead of being reported.
  scalarField <- function(entries, field) {
    vapply(
      entries,
      function(e) {
        value <- e[[field]]
        if (length(value) != 1L) NA_character_ else as.character(value)
      },
      character(1)
    )
  }

  for (setName in names(initialConditions)) {
    set <- initialConditions[[setName]]
    if (length(set) == 0) {
      next
    }

    # An entry that is a bare string rather than a record (`["Organism|Liver|
    # Aciclovir"]`) has no fields to read; treating it as an empty record folds
    # it into the missing-field errors below instead of aborting on `$`.
    entries <- lapply(set, function(e) if (is.list(e)) e else list())

    paths <- scalarField(entries, "path")
    units <- scalarField(entries, "unit")
    values <- lapply(entries, function(e) e$value)

    if (any(is.na(paths) | paths == "")) {
      result$addCriticalError(
        "Missing Fields",
        paste0(
          "Set '",
          setName,
          "' in ",
          sectionName,
          " contains empty molecule paths"
        )
      )
    }

    if (any(is.na(units) | units == "")) {
      result$addCriticalError(
        "Missing Fields",
        paste0(
          "Set '",
          setName,
          "' in ",
          sectionName,
          " contains entries without a unit"
        )
      )
    }

    nonNumeric <- vapply(
      values,
      function(v) is.null(v) || length(v) != 1L || !is.numeric(v) || is.na(v),
      logical(1)
    )
    if (any(nonNumeric)) {
      result$addWarning(
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

    # Only real paths can duplicate: the blanks and `NA`s are already reported
    # by the missing-path error above, and `duplicated()` would otherwise count
    # two path-less entries as a duplicate of each other.
    realPaths <- paths[!is.na(paths) & paths != ""]
    dupes <- realPaths[duplicated(realPaths)]
    if (length(dupes) > 0) {
      result$addWarning(
        "Uniqueness",
        paste0(
          "Duplicate molecule paths in set '",
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

#' Canonicalize ids for a cross-reference membership test
#'
#' On-disk section keys are canonicalized (lower-cased, slugified) by the
#' authoring / load path, but a hand-edited `Project.json` can carry a
#' non-canonical reference (e.g. `individualId: "Adult"` against a disk key of
#' `adult`). Cross-reference resolution must compare on the same footing, so
#' both the reference side and the candidate keys are run through the same
#' deterministic transform (`.canonicalizeOneId()`) before the `%in%` /
#' `setdiff()` test. A case-only (or otherwise canonically-equal) mismatch then
#' resolves instead of being reported as a dangling reference.
#'
#' Silent by design: this is a read-only comparison, not an authoring edit, so
#' it never warns about the canonicalization (unlike `.canonicalizeIdRef()`).
#' `NA` and `NULL` pass through so the callers' own presence guards still apply,
#' and the transform is applied element-wise to a vector of references.
#'
#' @param ids Character vector (or `NULL`) of ids / references.
#' @return `ids` with each non-`NA` element canonicalized; `NULL`/`NA` preserved.
#' @keywords internal
#' @noRd
.canonicalizeForCompare <- function(ids) {
  if (is.null(ids)) {
    return(ids)
  }
  ids <- as.character(ids)
  keep <- !is.na(ids)
  if (any(keep)) {
    ids[keep] <- vapply(
      ids[keep],
      .canonicalizeOneId,
      character(1),
      USE.NAMES = FALSE
    )
  }
  ids
}

#' Does a single reference resolve against a set of candidate keys?
#'
#' Canonicalizes BOTH the reference and the candidate keys before the `%in%`
#' test, so a case-only (or otherwise canonically-equal) reference resolves. The
#' caller keeps the candidate keys in their original spelling for the "did you
#' mean" suffix; this helper canonicalizes them locally only for the comparison.
#'
#' @param ref Character scalar reference (original spelling).
#' @param candidateKeys Candidate keys in their original spelling.
#' @return `TRUE` if `ref` canonically matches a candidate key.
#' @keywords internal
#' @noRd
.refResolves <- function(ref, candidateKeys) {
  .canonicalizeForCompare(ref) %in% .canonicalizeForCompare(candidateKeys)
}

#' Dangling references in a reference vector, compared canonically
#'
#' Returns the subset of `refs` (in their ORIGINAL spelling, so error messages
#' stay faithful to what the user wrote) whose canonical form is not among the
#' canonical forms of `candidateKeys`. `NA` and empty-string references are
#' dropped (they carry no reference). A case-only mismatch canonicalizes onto a
#' candidate and so is not returned. Candidate keys are passed in their original
#' spelling (so a caller can reuse the same vector for `.suggestSuffixMulti()`)
#' and canonicalized here for the comparison.
#'
#' @param refs Character vector of references (original spelling).
#' @param candidateKeys Candidate keys in their original spelling.
#' @return Character vector of dangling references, original spelling.
#' @keywords internal
#' @noRd
.danglingRefs <- function(refs, candidateKeys) {
  refs <- as.character(refs)
  present <- !is.na(refs) & refs != ""
  refs <- refs[present]
  if (length(refs) == 0L) {
    return(character(0))
  }
  canonicalCandidates <- .canonicalizeForCompare(candidateKeys)
  dangling <- !.canonicalizeForCompare(refs) %in% canonicalCandidates
  refs[dangling]
}

#' Validate cross-references between Project sections
#'
#' Hand-rolled monolith that checks references that span sections:
#' `scenario.individual/population` against the individuals and
#' populations sections, `scenario.parameterSets`,
#' `scenario.application`, and `scenario.outputPaths` against
#' their respective lookups, `individual.parameterSets` and
#' `application.parameterSets` against the corresponding parameter-set
#' sections, and `dataCombined.simulated.scenario` against scenarios.
#'
#' The phase always evaluates. A critical error in some other section does not
#' suppress it, because it is the phase the `runScenarios()` / `createPlots()` /
#' `runPI()` gates actually depend on: dropping it there let a scenario naming a
#' nonexistent individual reach the simulation backend.
#'
#' `sections` is the section list the run was asked for, and it scopes the phase
#' to the references those sections HOLD. That is what keeps each gate to its own
#' concern: `runScenarios()` asks for the sections a simulation is built from, so
#' a `dataCombined` entry naming a nonexistent scenario (a plotting-only
#' reference) is not graded against it, while `createPlots()` does ask for
#' `dataCombined` and is. The candidate side is never scoped: a reference resolves
#' against every definition the project has, whether or not that section is in
#' the run.
#'
#' Future deepening: the end-state walks per-section `references()`
#' declarations rather than hand-coding the FK list here. Out of scope
#' for Chapter 4.
#'
#' @param project Project object.
#' @param sections Character vector of the section names in scope, or `NULL` for
#'   every one of them. A vector naming no reference-holding section resolves
#'   nothing.
#' @return validationResult.
#' @keywords internal
#' @noRd
# Record a critical error for each definition in `definitions` whose
# `parameterSets` field names a set the project does not define. `ids` is the
# subset to check (empty when the section is out of scope) and `label` names the
# kind in the message, e.g. "Individual". `result` is mutated in place.
#
# @keywords internal
# @noRd
.checkParameterSetRefs <- function(
  definitions,
  ids,
  parameterSetKeys,
  label,
  result
) {
  for (id in ids) {
    refs <- as.character(unlist(
      definitions[[id]]$parameterSets %||% character(0)
    ))
    invalid <- .danglingRefs(refs, parameterSetKeys)
    if (length(invalid) > 0) {
      result$addCriticalError(
        "Invalid Reference",
        paste0(
          label,
          " '",
          id,
          "' references undefined parameterSets: ",
          paste(invalid, collapse = ", "),
          .suggestSuffixMulti(invalid, parameterSetKeys)
        )
      )
    }
  }
}

.validateCrossReferences <- function(project, sections = NULL) {
  result <- validationResult$new()
  inScope <- function(section) is.null(sections) || section %in% sections

  scenarioList <- project$definitions$scenarios %||% list()
  # Keep the section keys in their ORIGINAL spelling: the "did you mean" suffix
  # (`.suggestSuffix*`) shows them verbatim, and the membership helpers
  # (`.refResolves`, `.danglingRefs`) canonicalize BOTH sides internally, so a
  # hand-edited reference that differs from its definition only by case (or
  # another canonically-equal spelling) resolves instead of being flagged as
  # dangling.
  individualIds <- names(project$definitions$individuals %||% list())
  populationIds <- names(project$definitions$populations %||% list())
  parameterSetKeys <- names(project$definitions$parameterSets %||% list())
  initialConditionKeys <- names(
    project$definitions$initialConditions %||% list()
  )
  applicationKeys <- names(project$definitions$applications %||% list())
  outputPathKeys <- names(project$definitions$outputPaths %||% list())

  scenarioHolders <- if (inScope("scenarios")) names(scenarioList) else NULL
  for (scName in scenarioHolders) {
    sc <- scenarioList[[scName]]

    if (
      !is.null(sc$individualId) &&
        !is.na(sc$individualId) &&
        !.refResolves(sc$individualId, individualIds)
    ) {
      result$addCriticalError(
        "Invalid Reference",
        paste0(
          "Scenario '",
          scName,
          "' references undefined individual '",
          sc$individualId,
          "'",
          .suggestSuffix(sc$individualId, individualIds)
        )
      )
    }

    if (
      !is.null(sc$populationId) &&
        !is.na(sc$populationId) &&
        !.refResolves(sc$populationId, populationIds)
    ) {
      result$addCriticalError(
        "Invalid Reference",
        paste0(
          "Scenario '",
          scName,
          "' references undefined population '",
          sc$populationId,
          "'",
          .suggestSuffix(sc$populationId, populationIds)
        )
      )
    }

    if (!is.null(sc$modelParameterSets) && length(sc$modelParameterSets) > 0) {
      invalidSets <- .danglingRefs(sc$modelParameterSets, parameterSetKeys)
      if (length(invalidSets) > 0) {
        result$addCriticalError(
          "Invalid Reference",
          paste0(
            "Scenario '",
            scName,
            "' references undefined model parameter sets: ",
            paste(invalidSets, collapse = ", "),
            .suggestSuffixMulti(invalidSets, parameterSetKeys)
          )
        )
      }
    }

    if (!is.null(sc$initialConditions) && length(sc$initialConditions) > 0) {
      invalidICs <- .danglingRefs(sc$initialConditions, initialConditionKeys)
      if (length(invalidICs) > 0) {
        result$addCriticalError(
          "Invalid Reference",
          paste0(
            "Scenario '",
            scName,
            "' references undefined initial-condition sets: ",
            paste(invalidICs, collapse = ", "),
            .suggestSuffixMulti(invalidICs, initialConditionKeys)
          )
        )
      }
    }

    if (
      !is.null(sc$applicationProtocol) &&
        !is.na(sc$applicationProtocol) &&
        !.refResolves(sc$applicationProtocol, applicationKeys)
    ) {
      result$addCriticalError(
        "Invalid Reference",
        paste0(
          "Scenario '",
          scName,
          "' references undefined application '",
          sc$applicationProtocol,
          "'",
          .suggestSuffix(sc$applicationProtocol, applicationKeys)
        )
      )
    }

    # The in-memory scenario carries its output paths as a named vector
    # (id-as-name, resolved-path-as-value); the names are the references
    # into the outputPaths definitions, mirroring the serializer's reverse map.
    scOutputPathIds <- names(sc$outputPaths)
    if (!is.null(scOutputPathIds)) {
      invalidOutputPaths <- .danglingRefs(scOutputPathIds, outputPathKeys)
      if (length(invalidOutputPaths) > 0) {
        result$addCriticalError(
          "Invalid Reference",
          paste0(
            "Scenario '",
            scName,
            "' references undefined outputPaths: ",
            paste(invalidOutputPaths, collapse = ", "),
            .suggestSuffixMulti(invalidOutputPaths, outputPathKeys)
          )
        )
      }
    }
  }

  # individuals/applications resolve their parameter-set refs against the same
  # unified section as scenarios, and report them the same way.
  individuals <- project$definitions$individuals %||% list()
  .checkParameterSetRefs(
    individuals,
    if (inScope("individuals")) names(individuals) else NULL,
    parameterSetKeys,
    "Individual",
    result
  )

  applications <- project$definitions$applications %||% list()
  .checkParameterSetRefs(
    applications,
    if (inScope("applications")) names(applications) else NULL,
    parameterSetKeys,
    "Application",
    result
  )

  # Scenario keys keep their original spelling; `.danglingRefs()` canonicalizes
  # both sides for the membership test and `.suggestSuffixMulti()` shows the
  # original spelling in the "did you mean" hint.
  scenarioNames <- names(scenarioList)

  dataCombined <- .unwrapDefinitionList(project$definitions$dataCombined)
  if (inScope("dataCombined") && length(dataCombined) > 0) {
    referencedScenarios <- unlist(lapply(dataCombined, function(dc) {
      vapply(
        dc$simulated %||% list(),
        function(e) e$scenario %||% NA_character_,
        character(1)
      )
    }))
    invalidScenarios <- .danglingRefs(referencedScenarios, scenarioNames)
    if (length(invalidScenarios) > 0) {
      result$addCriticalError(
        "Invalid Reference",
        paste0(
          "dataCombined references undefined scenarios: ",
          paste(invalidScenarios, collapse = ", "),
          .suggestSuffixMulti(invalidScenarios, scenarioNames)
        )
      )
    }
  }

  # parameterIdentification cross-references -----------------------
  # `scenarioNames` / `outputPathIds` keep the original spellings for the
  # "did you mean" suffix; membership is tested canonically via `.danglingRefs()`
  # / `.refResolves()`, which canonicalize both sides internally.
  piTasks <- project$definitions$parameterIdentification %||% list()
  outputPathIds <- names(project$definitions$outputPaths %||% list())

  piHolders <- if (inScope("parameterIdentification")) names(piTasks) else NULL
  # Resolved only when there is a task to check, since answering means loading
  # the observed data.
  observedDataNames <- if (length(piHolders) > 0L) {
    .observedDataNamesForCrossReference(project)
  }
  for (taskId in piHolders) {
    task <- piTasks[[taskId]]

    badTaskScenarios <- .danglingRefs(task$scenarios, scenarioNames)
    if (length(badTaskScenarios) > 0L) {
      result$addCriticalError(
        "Invalid Reference",
        paste0(
          "PI task '",
          taskId,
          "' references undefined scenarios: ",
          paste(badTaskScenarios, collapse = ", "),
          .suggestSuffixMulti(badTaskScenarios, scenarioNames)
        )
      )
    }

    for (p in task$parameters) {
      bad <- .danglingRefs(p$scenarios, scenarioNames)
      if (length(bad) > 0L) {
        result$addCriticalError(
          "Invalid Reference",
          paste0(
            "PI task '",
            taskId,
            "', parameter '",
            p$id,
            "' references undefined scenarios: ",
            paste(bad, collapse = ", "),
            .suggestSuffixMulti(bad, scenarioNames)
          )
        )
      }
    }

    for (m in task$outputMappings) {
      badScenarios <- .danglingRefs(m$scenarios, scenarioNames)
      if (length(badScenarios) > 0L) {
        result$addCriticalError(
          "Invalid Reference",
          paste0(
            "PI task '",
            taskId,
            "', outputMapping '",
            m$id,
            "' references undefined scenarios: ",
            paste(badScenarios, collapse = ", "),
            .suggestSuffixMulti(badScenarios, scenarioNames)
          )
        )
      }
      # An absent `outputPathId` is a missing required field, which `.validatePI()`
      # reports as a section-local concern; this phase only resolves a reference
      # that is there, so the same gap is not counted twice.
      if (
        !.isMissingField(m$outputPathId) &&
          !.refResolves(m$outputPathId, outputPathKeys)
      ) {
        result$addCriticalError(
          "Invalid Reference",
          paste0(
            "PI task '",
            taskId,
            "', outputMapping '",
            m$id,
            "' references undefined outputPath '",
            m$outputPathId,
            "'",
            .suggestSuffix(m$outputPathId, outputPathIds)
          )
        )
      }
      # `observedData` names a data set inside an observed-data source, not a
      # declaration, so it resolves against the loaded data-set names. A `NULL`
      # name set means the project could not be asked without running user code
      # (see `.observedDataNamesForCrossReference()`), which leaves the reference
      # unresolved rather than reported as dangling.
      if (
        !is.null(observedDataNames) &&
          !.isMissingField(m$observedDataId) &&
          !.refResolves(m$observedDataId, observedDataNames)
      ) {
        result$addCriticalError(
          "Invalid Reference",
          paste0(
            "PI task '",
            taskId,
            "', outputMapping '",
            m$id,
            "' references undefined observed data '",
            m$observedDataId,
            "'",
            .suggestSuffix(m$observedDataId, observedDataNames)
          )
        )
      }
    }
  }

  result
}
