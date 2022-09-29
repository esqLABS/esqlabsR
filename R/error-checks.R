
#' Validate parameter list structure
#'
#' @param parameterStructure Object to be checked. Expected is a named list
#' with names "paths", "values", and "units".
#'
#' @keywords internal
#'
#' @return `TRUE` if validation succeeded (silently). Throws an error otherwise.
.validateParametersStructure <- function(parameterStructure, argumentName = NULL,
                                         nullAllowed = FALSE) {
  if (is.null(parameterStructure) && nullAllowed) {
    return(invisible(TRUE))
  }

  if (!identical(names(parameterStructure), c("paths", "values", "units"))) {
    stop(messages$wrongParametersStructure(argumentName = argumentName))
  }
  return(invisible(TRUE))
}
