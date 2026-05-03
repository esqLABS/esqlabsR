# Validation classes and functions for Project objects

# validationResult class ----

#' @title validationResult
#' @description R6 class for storing validation results
#' @export
validationResult <- R6::R6Class(
  "validationResult",
  public = list(
    #' @field data Successfully validated/processed data
    data = NULL,

    #' @field critical_errors List of critical errors (blocking issues)
    critical_errors = list(),

    #' @field warnings List of warnings (non-blocking issues)
    warnings = list(),

    #' @description Initialize a new ValidationResult
    initialize = function() {
      self$critical_errors <- list()
      self$warnings <- list()
      self$data <- NULL
    },

    #' @description Add a critical error
    #' @param category Error category (e.g., "Structure", "Missing Fields", "Uniqueness")
    #' @param message Error message
    #' @param details Optional list with additional details (sheet, row, column)
    add_critical_error = function(category, message, details = NULL) {
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
    add_warning = function(category, message, details = NULL) {
      warning_entry <- list(
        category = category,
        message = message,
        details = details,
        timestamp = Sys.time()
      )
      self$warnings <- append(self$warnings, list(warning_entry))
    },

    #' @description Set validated data
    #' @param data The validated/processed data to store
    set_data = function(data) {
      self$data <- data
    },

    #' @description Check if validation passed (no critical errors)
    is_valid = function() {
      length(self$critical_errors) == 0
    },

    #' @description Check if there are critical errors
    has_critical_errors = function() {
      length(self$critical_errors) > 0
    },

    #' @description Get formatted messages for display
    get_formatted_messages = function() {
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
    get_summary = function() {
      list(
        has_critical_errors = self$has_critical_errors(),
        critical_error_count = length(self$critical_errors),
        warning_count = length(self$warnings),
        has_data = !is.null(self$data)
      )
    }
  )
)

# Helper functions ----

#' Categorize validation messages
#'
#' Best-effort heuristic categorization based on the wording of upstream errors
#' and warnings. Keep in mind this couples categories to specific phrases; if
#' phrasing drifts, callers may see "Validation" buckets instead of the
#' specific category. Long-term, the throw sites should attach categories via
#' classed conditions instead of relying on regex matching here.
#'
#' @keywords internal
.categorize_message <- function(message) {
  if (
    grepl(
      "missing|empty|not found|required field",
      message,
      ignore.case = TRUE
    )
  ) {
    return("Missing Fields")
  }
  if (grepl("duplicate|unique", message, ignore.case = TRUE)) {
    return("Uniqueness")
  }
  if (
    grepl(
      "not defined|invalid.*reference|references undefined",
      message,
      ignore.case = TRUE
    )
  ) {
    return("Invalid Reference")
  }
  if (grepl("format|separated|Wrong number", message)) {
    return("Format Error")
  }
  if (grepl("length|mismatch", message, ignore.case = TRUE)) {
    return("Structure")
  }
  "Validation"
}

#' Check for duplicate values and add critical error if found
#' @param ids Character vector of IDs to check
#' @param field_name Name of the field for error message
#' @param result validationResult object to add errors to
#' @return The modified validationResult object
#' @keywords internal
.check_no_duplicates <- function(ids, field_name, result) {
  dupes <- ids[duplicated(ids) & !is.na(ids)]
  if (length(dupes) > 0) {
    result$add_critical_error(
      "Uniqueness",
      paste0(
        "Duplicate ",
        field_name,
        " values: ",
        paste(unique(dupes), collapse = ", ")
      )
    )
  }
  result
}

#' Check that required fields are present and non-empty
#' @param entry List entry to check
#' @param required_fields Character vector of required field names
#' @param entry_name Name of the entry for error message
#' @param result validationResult object to add errors to
#' @return The modified validationResult object
#' @keywords internal
.check_required_fields <- function(entry, required_fields, entry_name, result) {
  for (field in required_fields) {
    val <- entry[[field]]
    if (is.null(val) || (length(val) == 1 && (is.na(val) || val == ""))) {
      result$add_critical_error(
        "Missing Fields",
        paste0(
          "Required field '",
          field,
          "' is missing or empty in ",
          entry_name
        )
      )
    }
  }
  result
}

# Section validators ----

#' Validate parameter groups structure
#' @param groups Named list of parameter groups, each with paths, values, units
#' @param section_name Name of the section for error messages
#' @return validationResult object
#' @keywords internal
.validateParameterGroups <- function(groups, section_name) {
  result <- validationResult$new()

  if (is.null(groups) || length(groups) == 0) {
    result$add_warning("Data", paste0("No ", section_name, " defined"))
    return(result)
  }

  for (group_name in names(groups)) {
    group <- groups[[group_name]]
    paths <- group$paths %||% character(0)
    values <- group$values %||% numeric(0)

    if (length(paths) != length(values)) {
      result$add_critical_error(
        "Structure",
        paste0(
          "Group '",
          group_name,
          "' in ",
          section_name,
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
          "Group '",
          group_name,
          "' in ",
          section_name,
          " contains empty parameter paths"
        )
      )
    }

    if (!is.numeric(values)) {
      result$add_warning(
        "Data Type",
        paste0(
          "Group '",
          group_name,
          "' in ",
          section_name,
          " contains non-numeric values"
        )
      )
    }

    dupes <- paths[duplicated(paths) & !is.na(paths)]
    if (length(dupes) > 0) {
      result$add_warning(
        "Uniqueness",
        paste0(
          "Duplicate parameter paths in group '",
          group_name,
          "': ",
          paste(unique(dupes), collapse = ", ")
        )
      )
    }
  }

  result
}

#' Validate cross-references between Project sections
#' @param project Project object
#' @param validationResults Previous validation results from section validators
#' @return validationResult object with cross-reference validation
#' @keywords internal
.validateCrossReferences <- function(project, validationResults) {
  result <- validationResult$new()

  if (isAnyCriticalErrors(validationResults)) {
    skipped <- c(
      "scenario individualId / populationId references",
      "scenario modelParameters references",
      "scenario applicationProtocol references",
      "scenario outputPathIds references",
      "plot scenario / dataSet references"
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

  scenario_list <- project$scenarios %||% list()
  individual_ids <- names(project$individuals %||% list())
  population_ids <- names(project$populations %||% list())
  model_param_keys <- names(project$modelParameters %||% list())
  application_keys <- names(project$applications %||% list())

  for (sc_name in names(scenario_list)) {
    sc <- scenario_list[[sc_name]]

    # Check individualId reference
    if (
      !is.null(sc$individualId) &&
        !is.na(sc$individualId) &&
        !sc$individualId %in% individual_ids
    ) {
      result$add_critical_error(
        "Invalid Reference",
        paste0(
          "Scenario '",
          sc_name,
          "' references undefined individualId '",
          sc$individualId,
          "'"
        )
      )
    }

    # Check populationId reference (for population scenarios)
    if (
      !is.null(sc$populationId) &&
        !is.na(sc$populationId) &&
        !sc$populationId %in% population_ids
    ) {
      result$add_critical_error(
        "Invalid Reference",
        paste0(
          "Scenario '",
          sc_name,
          "' references undefined populationId '",
          sc$populationId,
          "'"
        )
      )
    }

    # Check modelParameters references
    if (!is.null(sc$modelParameters) && length(sc$modelParameters) > 0) {
      invalid_sets <- setdiff(sc$modelParameters, model_param_keys)
      if (length(invalid_sets) > 0) {
        result$add_critical_error(
          "Invalid Reference",
          paste0(
            "Scenario '",
            sc_name,
            "' references undefined model parameter sets: ",
            paste(invalid_sets, collapse = ", ")
          )
        )
      }
    }

    # Check applicationProtocol reference
    if (
      !is.null(sc$applicationProtocol) &&
        !is.na(sc$applicationProtocol) &&
        !sc$applicationProtocol %in% application_keys
    ) {
      result$add_critical_error(
        "Invalid Reference",
        paste0(
          "Scenario '",
          sc_name,
          "' references undefined applicationProtocol '",
          sc$applicationProtocol,
          "'"
        )
      )
    }
  }

  # Check plots dataCombined scenario references
  dataCombined <- project$plots$dataCombined
  if (!is.null(dataCombined) && length(dataCombined) > 0) {
    referenced_scenarios <- unlist(lapply(dataCombined, function(dc) {
      vapply(
        dc$simulated %||% list(),
        function(e) e$scenario %||% NA_character_,
        character(1)
      )
    }))
    referenced_scenarios <- referenced_scenarios[!is.na(referenced_scenarios)]
    invalid_scenarios <- setdiff(referenced_scenarios, names(scenario_list))
    if (length(invalid_scenarios) > 0) {
      result$add_critical_error(
        "Invalid Reference",
        paste0(
          "dataCombined references undefined scenarios: ",
          paste(invalid_scenarios, collapse = ", ")
        )
      )
    }
  }

  result
}

# Public API ----

#' Validate a Project
#'
#' Validates a Project object or JSON file for structural correctness,
#' required fields, data types, and cross-references between sections.
#'
#' @param project Project object or path to Project.json file
#' @return Named list of validationResult objects with class "ValidationResults"
#' @export
#' @examples
#' \dontrun{
#' # Validate from a loaded project
#' project <- loadProject("Project.json")
#' results <- validateProject(project)
#'
#' # Validate directly from a JSON path
#' results <- validateProject("Project.json")
#'
#' # Check for critical errors
#' if (isAnyCriticalErrors(results)) {
#'   print(validationSummary(results))
#' }
#' }
validateProject <- function(project) {
  results <- list()

  if (is.character(project)) {
    if (!file.exists(project)) {
      r <- validationResult$new()
      r$add_critical_error("File", messages$validationFileNotFound(project))
      results$project <- r
      class(results) <- c("ValidationResults", class(results))
      return(results)
    }

    loaded_project <- tryCatch(
      loadProject(path = project),
      error = function(e) {
        r <- validationResult$new()
        r$add_critical_error(
          .categorize_message(conditionMessage(e)),
          conditionMessage(e)
        )
        results$project <<- r
        NULL
      }
    )

    if (is.null(loaded_project)) {
      class(results) <- c("ValidationResults", class(results))
      return(results)
    }

    project <- loaded_project
  }

  results <- .runProjectValidation(project, sections = NULL)

  if (!isAnyCriticalErrors(results)) {
    project$.markValidated()
  }

  results
}

# Section validator dispatch ----
#
# The dispatcher below resolves each section's validator by naming
# convention rather than hardcoding the section list. Each section file
# (R/applications.R, R/individuals.R, etc.) defines a top-level
# `.<section>ValidatorAdapter <- function(project)` that pulls the right
# slice of the project and calls the section's `.validateX` function.
# Adding a new section means dropping a file with a matching adapter; this
# file does not need to change.
#
# crossReferences is intentionally NOT a section adapter. It runs after
# all section validators because it inspects their partial results to
# decide whether to skip itself, so it lives as a fixed phase in the
# dispatcher rather than masquerading as a section.

# Canonical order of section validators. Each name (other than
# crossReferences) must have a matching `.<name>ValidatorAdapter` function
# defined somewhere in the package. The order determines the order of keys
# in the returned ValidationResults list.
.validationSections <- c(
  "individuals",
  "populations",
  "scenarios",
  "outputPaths",
  "modelParameters",
  "applications",
  "plots",
  "observedData",
  "crossReferences"
)

#' Resolve a section name to its validator adapter
#'
#' Looks up `.<section>ValidatorAdapter` in the package namespace. Errors
#' if no such function exists, with a message that points at the missing
#' adapter rather than a generic "function not found".
#'
#' @keywords internal
#' @noRd
.lookupSectionValidatorAdapter <- function(section) {
  adapterName <- paste0(".", section, "ValidatorAdapter")
  if (!exists(adapterName, mode = "function")) {
    stop(sprintf(
      "No validator adapter found for section '%s'. Define `%s <- function(project) ...` in the section's R file.",
      section,
      adapterName
    ))
  }
  get(adapterName, mode = "function")
}

#' Run a (possibly targeted) project validation
#'
#' Internal orchestration helper. Runs the requested section validators in
#' canonical order and returns a `ValidationResults` list. `crossReferences`
#' is always run last when included so it sees prior section results.
#'
#' @param project A loaded `Project` object.
#' @param sections Character vector of section names to validate, or `NULL`
#'   for a full validation. Unknown names are dropped silently.
#' @return Named list of `validationResult` objects with class
#'   `"ValidationResults"`. Only requested sections are present.
#' @keywords internal
.runProjectValidation <- function(project, sections = NULL) {
  if (is.null(sections)) {
    sections <- .validationSections
  } else {
    sections <- intersect(.validationSections, sections)
  }

  results <- list()
  for (section in sections) {
    if (section == "crossReferences") {
      results[[section]] <- .validateCrossReferences(project, results)
      next
    }
    adapter <- .lookupSectionValidatorAdapter(section)
    results[[section]] <- adapter(project)
  }

  class(results) <- c("ValidationResults", class(results))
  results
}

#' Ensure a project passes validation before an operation
#'
#' Runs targeted validation for the sections an operation depends on, and
#' aborts with a formatted multi-error message if any critical errors are
#' found. Short-circuits when the project has been fully validated since
#' its last mutation (the `validatedSinceMutation` flag).
#'
#' This helper does not itself flip the cache flag, because it only runs
#' a subset of validators. Only [validateProject()] (a full run) sets the
#' flag.
#'
#' @param project A `Project` object.
#' @param sections Non-empty character vector of section names required by
#'   the calling operation.
#' @param opName Short label used in the abort message (e.g. `"runScenarios"`).
#' @return `invisible(NULL)` on success.
#' @keywords internal
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

#' Check if validation results contain any critical errors
#' @param validationResults Output from validateProject
#' @return Logical indicating if there are critical errors
#' @export
isAnyCriticalErrors <- function(validationResults) {
  any(sapply(validationResults, function(r) {
    if (inherits(r, "validationResult")) {
      r$has_critical_errors()
    } else {
      FALSE
    }
  }))
}

#' Get summary of all validation results
#' @param validationResults Output from validateProject
#' @return List with summary statistics
#' @export
validationSummary <- function(validationResults) {
  summary <- list(
    total_critical_errors = 0,
    total_warnings = 0,
    files_with_errors = character(),
    files_with_warnings = character()
  )

  for (name in names(validationResults)) {
    result <- validationResults[[name]]
    if (inherits(result, "validationResult")) {
      if (result$has_critical_errors()) {
        summary$total_critical_errors <- summary$total_critical_errors +
          length(result$critical_errors)
        summary$files_with_errors <- c(summary$files_with_errors, name)
      }
      if (length(result$warnings) > 0) {
        summary$total_warnings <- summary$total_warnings +
          length(result$warnings)
        summary$files_with_warnings <- c(summary$files_with_warnings, name)
      }
    }
  }

  summary
}

#' Validate project file
#' @param projectConfigPath Path to Project.xlsx
#' @return validationResult object
#' @keywords internal
.validateProject <- function(projectConfigPath) {
  result <- validationResult$new()

  # Check file exists
  if (!file.exists(projectConfigPath)) {
    result$add_critical_error(
      "File",
      messages$validationFileNotFound(projectConfigPath)
    )
    return(result)
  }

  # Try to load project configuration directly (not using .safe_validate due to scoping issues)
  tryCatch(
    {
      withCallingHandlers(
        {
          config <- loadProject(path = projectConfigPath)

          # Check all referenced files exist
          files_to_check <- list(
            scenarios = config$scenariosFile,
            individuals = config$individualsFile,
            populations = config$populationsFile,
            models = config$modelParamsFile,
            applications = config$applicationsFile,
            plots = config$plotsFile
          )

          for (name in names(files_to_check)) {
            if (
              !is.na(files_to_check[[name]]) &&
                !file.exists(files_to_check[[name]])
            ) {
              result$add_warning(
                "File Reference",
                paste0(
                  "Referenced ",
                  name,
                  " file not found: ",
                  files_to_check[[name]]
                )
              )
            }
          }

          result$set_data(config)
        },
        warning = function(w) {
          category <- .categorize_message(conditionMessage(w))
          result$add_warning(category, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
    },
    error = function(e) {
      category <- .categorize_message(conditionMessage(e))
      result$add_critical_error(category, conditionMessage(e))
    }
  )

  return(result)
}
