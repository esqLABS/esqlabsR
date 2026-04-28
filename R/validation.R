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
  if (grepl(
    "missing|empty|not found|required field",
    message,
    ignore.case = TRUE
  )) {
    return("Missing Fields")
  }
  if (grepl("duplicate|unique", message, ignore.case = TRUE)) {
    return("Uniqueness")
  }
  if (grepl(
    "not defined|invalid.*reference|references undefined",
    message,
    ignore.case = TRUE
  )) {
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

#' Validate individuals section of a Project
#' @param individuals Named list of individuals from project$individuals
#' @return validationResult object
#' @keywords internal
.validateIndividuals <- function(individuals) {
  result <- validationResult$new()

  if (is.null(individuals) || length(individuals) == 0) {
    result$add_warning("Data", "No individuals defined")
    return(result)
  }

  required_fields <- c("species", "gender")
  for (id in names(individuals)) {
    indiv <- individuals[[id]]
    result <- .check_required_fields(
      indiv,
      required_fields,
      paste0("individual '", id, "'"),
      result
    )

    for (num_field in c("weight", "height", "age")) {
      val <- indiv[[num_field]]
      if (!is.null(val) && !is.na(val) && !is.numeric(val)) {
        result$add_warning(
          "Data Type",
          paste0(
            "Field '",
            num_field,
            "' in individual '",
            id,
            "' should be numeric"
          )
        )
      }
    }
  }

  result
}

#' Validate populations section of a Project
#' @param populations Named list of populations from project$populations
#' @return validationResult object
#' @keywords internal
.validatePopulations <- function(populations) {
  result <- validationResult$new()

  if (is.null(populations) || length(populations) == 0) {
    result$add_warning("Data", "No populations defined")
    return(result)
  }

  for (id in names(populations)) {
    pop <- populations[[id]]
    result <- .check_required_fields(
      pop,
      c("species"),
      paste0("population '", id, "'"),
      result
    )

    if (!is.null(pop$proportionOfFemales)) {
      pof <- as.numeric(pop$proportionOfFemales)
      if (!is.na(pof) && (pof < 0 || pof > 100)) {
        result$add_warning(
          "Data Range",
          paste0(
            "proportionOfFemales in population '",
            id,
            "' should be between 0 and 100"
          )
        )
      }
    }

    range_pairs <- list(
      c("ageMin", "ageMax"),
      c("weightMin", "weightMax"),
      c("heightMin", "heightMax"),
      c("BMIMin", "BMIMax")
    )
    for (pair in range_pairs) {
      lo <- pop[[pair[1]]]
      hi <- pop[[pair[2]]]
      if (!is.null(lo) && !is.null(hi) && !is.na(lo) && !is.na(hi) && lo > hi) {
        result$add_warning(
          "Data Range",
          paste0(pair[1], " > ", pair[2], " in population '", id, "'")
        )
      }
    }
  }

  result
}

#' Validate scenarios section of a Project
#' @param scenarios Named list of Scenario objects from project$scenarios
#' @return validationResult object
#' @keywords internal
.validateScenarios <- function(scenarios) {
  result <- validationResult$new()

  if (is.null(scenarios) || length(scenarios) == 0) {
    result$add_warning("Data", "No scenarios defined")
    return(result)
  }

  for (name in names(scenarios)) {
    sc <- scenarios[[name]]

    if (is.null(sc$modelFile) || sc$modelFile == "") {
      result$add_critical_error(
        "Missing Fields",
        paste0("Scenario '", name, "' has no modelFile")
      )
    }

    sim_type <- sc$simulationType %||% ""
    if (!sim_type %in% c("Individual", "Population")) {
      result$add_critical_error(
        "Validation",
        paste0(
          "Scenario '",
          name,
          "' has invalid simulationType '",
          sim_type,
          "'"
        )
      )
    }

    if (
      sim_type == "Population" &&
        (is.null(sc$populationId) || sc$populationId == "")
    ) {
      result$add_critical_error(
        "Missing Fields",
        paste0("Population scenario '", name, "' has no populationId")
      )
    }
  }

  result
}

#' Validate outputPaths section of a Project
#' @param outputPaths Named character vector from project$outputPaths
#' @return validationResult object
#' @keywords internal
.validateOutputPaths <- function(outputPaths) {
  result <- validationResult$new()

  if (is.null(outputPaths) || length(outputPaths) == 0) {
    result$add_warning("Data", "No output paths defined")
    return(result)
  }

  result <- .check_no_duplicates(names(outputPaths), "outputPathId", result)

  empty_paths <- names(outputPaths)[is.na(outputPaths) | outputPaths == ""]
  if (length(empty_paths) > 0) {
    result$add_critical_error(
      "Missing Fields",
      paste0(
        "Empty output path values for IDs: ",
        paste(empty_paths, collapse = ", ")
      )
    )
  }

  dupe_values <- outputPaths[duplicated(outputPaths) & !is.na(outputPaths)]
  if (length(dupe_values) > 0) {
    result$add_warning(
      "Uniqueness",
      paste0(
        "Multiple IDs point to the same output path: ",
        paste(unique(dupe_values), collapse = ", ")
      )
    )
  }

  result
}

#' Validate modelParameters section of a Project
#' @param modelParameters Named list from project$modelParameters
#' @return validationResult object
#' @keywords internal
.validateModelParameters <- function(modelParameters) {
  .validateParameterGroups(modelParameters, "modelParameters")
}

#' Validate applications section of a Project
#' @param applications Named list from project$applications
#' @return validationResult object
#' @keywords internal
.validateApplications <- function(applications) {
  .validateParameterGroups(applications, "applications")
}

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

#' Validate plots section of a Project
#' @param plots List with dataCombined, plotConfiguration, plotGrids,
#'   exportConfiguration data.frames from project$plots
#' @return validationResult object
#' @keywords internal
.validatePlots <- function(plots) {
  result <- validationResult$new()

  if (is.null(plots)) {
    result$add_warning("Data", "No plots defined")
    return(result)
  }

  dc <- plots$dataCombined
  pc <- plots$plotConfiguration

  # Validate dataCombined
  if (is.null(dc) || nrow(dc) == 0) {
    result$add_warning("Data", "dataCombined is empty")
  } else {
    for (col in c("DataCombinedName", "dataType")) {
      if (!col %in% names(dc)) {
        result$add_critical_error(
          "Missing Fields",
          paste0("dataCombined is missing required column '", col, "'")
        )
      }
    }

    if ("dataType" %in% names(dc)) {
      invalid_types <- dc$dataType[
        !is.na(dc$dataType) & !dc$dataType %in% c("simulated", "observed")
      ]
      if (length(invalid_types) > 0) {
        result$add_critical_error(
          "Validation",
          paste0(
            "Invalid dataType values in dataCombined: ",
            paste(unique(invalid_types), collapse = ", ")
          )
        )
      }

      simulated_rows <- dc[!is.na(dc$dataType) & dc$dataType == "simulated", ]
      if (nrow(simulated_rows) > 0 && "scenario" %in% names(dc)) {
        missing_scenario <- is.na(simulated_rows$scenario) |
          simulated_rows$scenario == ""
        if (any(missing_scenario)) {
          result$add_critical_error(
            "Missing Fields",
            "Some simulated rows in dataCombined are missing 'scenario'"
          )
        }
      }
    }
  }

  # Validate plotConfiguration
  if (is.null(pc) || nrow(pc) == 0) {
    result$add_warning("Data", "plotConfiguration is empty")
  } else {
    for (col in c("plotID", "DataCombinedName", "plotType")) {
      if (!col %in% names(pc)) {
        result$add_critical_error(
          "Missing Fields",
          paste0("plotConfiguration is missing required column '", col, "'")
        )
      }
    }

    if ("plotID" %in% names(pc)) {
      result <- .check_no_duplicates(
        pc$plotID[!is.na(pc$plotID)],
        "plotID",
        result
      )
    }

    # Inner cross-ref: plotConfiguration -> dataCombined
    if (
      !is.null(dc) &&
        nrow(dc) > 0 &&
        "DataCombinedName" %in% names(pc) &&
        "DataCombinedName" %in% names(dc)
    ) {
      invalid_dc_refs <- setdiff(
        pc$DataCombinedName[!is.na(pc$DataCombinedName)],
        dc$DataCombinedName
      )
      if (length(invalid_dc_refs) > 0) {
        result$add_critical_error(
          "Invalid Reference",
          paste0(
            "plotConfiguration references unknown DataCombinedName: ",
            paste(invalid_dc_refs, collapse = ", ")
          )
        )
      }
    }
  }

  # plotGrids -> plotConfiguration inner cross-ref (warning only)
  pg <- plots$plotGrids
  if (!is.null(pg) && nrow(pg) > 0 && !is.null(pc) && nrow(pc) > 0) {
    if ("plotIDs" %in% names(pg) && "plotID" %in% names(pc)) {
      all_grid_ids <- unlist(lapply(
        pg$plotIDs[!is.na(pg$plotIDs)],
        function(x) trimws(strsplit(x, ",")[[1]])
      ))
      invalid_grid_refs <- setdiff(all_grid_ids, pc$plotID)
      if (length(invalid_grid_refs) > 0) {
        result$add_warning(
          "Invalid Reference",
          paste0(
            "plotGrids references unknown plotIDs: ",
            paste(invalid_grid_refs, collapse = ", ")
          )
        )
      }
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
  dc <- project$plots$dataCombined
  if (
    !is.null(dc) &&
      nrow(dc) > 0 &&
      "scenario" %in% names(dc) &&
      "dataType" %in% names(dc)
  ) {
    simulated <- dc[!is.na(dc$dataType) & dc$dataType == "simulated", ]
    scenario_names <- names(scenario_list)
    invalid_scenarios <- setdiff(
      simulated$scenario[!is.na(simulated$scenario)],
      scenario_names
    )
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

#' Validate observedData section of a Project
#' @param observedData List of observedData entries from project$observedData
#' @param dataFolder Path to the project's data folder
#' @return validationResult object
#' @keywords internal
.validateObservedData <- function(observedData, dataFolder) {
  result <- validationResult$new()

  if (is.null(observedData) || length(observedData) == 0) {
    result$add_warning("Data", "No observedData defined")
    return(result)
  }

  validTypes <- c("excel", "pkml", "script", "programmatic")

  for (i in seq_along(observedData)) {
    entry <- observedData[[i]]
    entryLabel <- paste0("observedData entry ", i)

    # Check type field
    if (is.null(entry$type)) {
      result$add_critical_error(
        "Missing Fields",
        paste0(entryLabel, " is missing required field 'type'")
      )
      next
    }

    if (!entry$type %in% validTypes) {
      result$add_critical_error(
        "Invalid Value",
        paste0(
          entryLabel,
          " has invalid type '",
          entry$type,
          "'. Must be one of: ",
          paste(validTypes, collapse = ", ")
        )
      )
      next
    }

    # Type-specific validation
    if (entry$type == "excel") {
      if (is.null(entry$file)) {
        result$add_critical_error(
          "Missing Fields",
          paste0(entryLabel, " (excel) is missing required field 'file'")
        )
      } else {
        filePath <- file.path(dataFolder, entry$file)
        if (!file.exists(filePath)) {
          result$add_warning(
            "File Not Found",
            paste0(entryLabel, " references non-existent file: ", entry$file)
          )
        }
      }

      if (is.null(entry$importerConfiguration)) {
        result$add_critical_error(
          "Missing Fields",
          paste0(
            entryLabel,
            " (excel) is missing required field 'importerConfiguration'"
          )
        )
      } else {
        importerPath <- file.path(dataFolder, entry$importerConfiguration)
        if (!file.exists(importerPath)) {
          result$add_warning(
            "File Not Found",
            paste0(
              entryLabel,
              " references non-existent importer config: ",
              entry$importerConfiguration
            )
          )
        }
      }

      if (is.null(entry$sheets) || length(entry$sheets) == 0) {
        result$add_critical_error(
          "Missing Fields",
          paste0(entryLabel, " (excel) is missing required field 'sheets'")
        )
      }
    }

    if (entry$type %in% c("pkml", "script")) {
      if (is.null(entry$file)) {
        result$add_critical_error(
          "Missing Fields",
          paste0(
            entryLabel,
            " (",
            entry$type,
            ") is missing required field 'file'"
          )
        )
      } else {
        filePath <- file.path(dataFolder, entry$file)
        if (!file.exists(filePath)) {
          result$add_warning(
            "File Not Found",
            paste0(entryLabel, " references non-existent file: ", entry$file)
          )
        }
      }
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

  results$individuals <- .validateIndividuals(project$individuals)

  results$populations <- .validatePopulations(project$populations)

  results$scenarios <- .validateScenarios(project$scenarios)

  results$outputPaths <- .validateOutputPaths(project$outputPaths)

  results$modelParameters <- .validateModelParameters(project$modelParameters)

  results$applications <- .validateApplications(project$applications)

  results$plots <- .validatePlots(project$plots)

  results$observedData <- .validateObservedData(
    project$observedData,
    project$dataFolder
  )

  results$crossReferences <- .validateCrossReferences(project, results)

  class(results) <- c("ValidationResults", class(results))
  results
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
