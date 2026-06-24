#' Validate parameter list structure
#'
#' @param parameterStructure Object to be checked. Expected is a named list with
#'   names "paths", "values", and "units". `paths` and `units` must be character
#'   vectors and `values` a numeric vector.
#'
#' @keywords internal
#'
#' @returns `TRUE` if validation succeeded (silently). Throws an error
#'   otherwise.
.validateParametersStructure <- function(
  parameterStructure,
  argumentName = NULL,
  nullAllowed = FALSE
) {
  if (is.null(parameterStructure) && nullAllowed) {
    return(invisible(TRUE))
  }

  if (!identical(names(parameterStructure), c("paths", "values", "units"))) {
    stop(messages$wrongParametersStructure(argumentName = argumentName))
  }

  # Validate the data type of each vector. Empty structures may carry `NULL`
  # vectors (e.g. when no parameters were found), which are treated as valid.
  isValidType <- function(x, typeCheck) {
    is.null(x) || typeCheck(x)
  }
  typesValid <-
    isValidType(parameterStructure$paths, is.character) &&
    isValidType(parameterStructure$values, is.numeric) &&
    isValidType(parameterStructure$units, is.character)

  if (!typesValid) {
    stop(messages$wrongParametersStructure(argumentName = argumentName))
  }

  # All three vectors must have the same length
  vectorLengths <- c(
    length(parameterStructure$paths),
    length(parameterStructure$values),
    length(parameterStructure$units)
  )
  if (length(unique(vectorLengths)) != 1L) {
    stop(messages$wrongParametersStructure(argumentName = argumentName))
  }

  return(invisible(TRUE))
}

#' Check if the `object` contains active binding with the name `field`
#'
#' @param object A class or an instance of a class to check
#' @param field Name of the field
#'
#' @returns `TRUE` if the `object` has an active binding `field`, `FALSE`
#'   otherwise.
#' @keywords internal
.validateClassHasField <- function(object, field) {
  if (!any(names(object) == field)) {
    return(FALSE)
  }
  return(TRUE)
}
