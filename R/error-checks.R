#' Check if the file exists. If not, stop with an error.
#'
#' @param filePath Path to the file
validateFileExists <- function(filePath) {
  if (!file.exists(filePath)) {
    stop(messages$errorFileNotFound(filePath))
  }
}

#' Check if the provided object is of a certain length. If not, stop with an
#' error.
#'
#' @param object Object which length will be checked
#' @param length Expected length
validateLength <- function(object, length) {
  if (length(object) == length) {
    return()
  }

  # Name of the variable in the calling function
  objectName <- deparse(substitute(object))

  stop(messages$errorWrongLength(objectName, length))
}
