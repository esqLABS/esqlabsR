#' Safe execution wrapper that captures errors and warnings
#' @param expr Expression to evaluate
#' @param result Existing validationResult to append to (optional)
#' @keywords internal
.safe_validate <- function(expr, result = NULL) {
  if (is.null(result)) {
    result <- validationResult$new()
  }

  tryCatch(
    {
      withCallingHandlers(
        {
          # Evaluate in the parent environment to preserve variable scope
          output <- eval(expr, envir = parent.frame())
          result$set_data(output)
        },
        warning = function(w) {
          # Categorize warning based on message content
          category <- .categorize_message(conditionMessage(w))
          result$add_warning(category, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
    },
    error = function(e) {
      # Categorize error based on existing message patterns
      category <- .categorize_message(conditionMessage(e))
      result$add_critical_error(category, conditionMessage(e))
    }
  )

  return(result)
}

#' Categorize validation messages
#' @keywords internal
.categorize_message <- function(message) {
  dplyr::case_when(
    grepl(
      "missing|empty|not found",
      message,
      ignore.case = TRUE
    ) ~ "Missing Fields",
    grepl("duplicate|unique", message, ignore.case = TRUE) ~ "Uniqueness",
    grepl(
      "not defined|invalid.*reference",
      message,
      ignore.case = TRUE
    ) ~ "Invalid Reference",
    grepl("format|separated|Wrong number", message) ~ "Format Error",
    grepl("sheet|column|structure", message, ignore.case = TRUE) ~ "Structure",
    .default = "Validation"
  )
}
