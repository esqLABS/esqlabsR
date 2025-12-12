#' @title validationResult
#' @description R6 class for storing validation results from Excel configuration files
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
