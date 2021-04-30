isSameLength <- function(...) {
  args <- list(...)
  nrOfLengths <- length(unique(lengths(args)))

  return(nrOfLengths == 1)
}

#' Check if the provided object is of certain type
#'
#' @param object An object or a list of objects
#' @param type String representation of the type that should be checked for
#' @param nullAllowed Boolean flag if \code{NULL} is accepted for the \code{object}. If \code{TRUE},
#' \code{NULL} always returns \code{TRUE}, otherwise \code{NULL} returns \code{FALSE}. Default is \code{FALSE}
#'
#' @return TRUE if the object or all objects inside the list are of the given type.
#' Only the first level of the given list is considered.
isOfType <- function(object, type, nullAllowed = FALSE) {
  if (is.null(object)) {
    if (nullAllowed) {
      return(TRUE)
    }
    return(FALSE)
  }

  type <- .typeNamesFrom(type)
  inheritType <- function(x) {
    if (is.null(x) && nullAllowed) {
      return(TRUE)
    }
    inherits(x, type)
  }
  if (inheritType(object)) {
    return(TRUE)
  }

  object <- c(object)
  all(sapply(object, inheritType))
}

#' Check if the provided object is of certain type. If not, stop with an error.
#'
#' @param object An object or a list of objects
#' @param type String representation of the type that should be checked for
#' @param nullAllowed Boolean flag if \code{NULL} is accepted for the \code{object}. If \code{TRUE},
#' \code{NULL} is always valid, otherwise the error is thrown. Default is \code{FALSE}
#' @export
validateIsOfType <- function(object, type, nullAllowed = FALSE) {
  if (isOfType(object, type, nullAllowed)) {
    return()
  }
  # Name of the variable in the calling function
  objectName <- deparse(substitute(object))
  objectTypes <- .typeNamesFrom(type)

  stop(messages$errorWrongType(objectName, class(object)[1], objectTypes))
}

#' Check if \code{value} is in the given {enum}. If not, stops with an error.
#'
#' @param enum \code{enum} where the \code{value} should be contained
#' @param value \code{value} to search for in the \code{enum}
#' @param nullAllowed If TRUE, \code{value} can be \code{NULL} and the test always passes.
#' If \code{FALSE} (default), NULL is not accepted and the test fails.
#' @export
validateEnumValue <- function(value, enum, nullAllowed = FALSE) {
  if (is.null(value)) {
    if (nullAllowed) {
      return()
    }
    stop(messages$errorEnumValueUndefined(enum))
  }

  enumKey <- enumGetKey(enum, value)
  if (any(names(enum) == enumKey)) {
    return()
  }

  stop(messages$errorValueNotInEnum(enum, enumKey))
}

.typeNamesFrom <- function(type) {
  type <- c(type)
  sapply(type, function(t) {
    if (is.character(t)) {
      return(t)
    }
    t$classname
  })
}


#' Check if the provided object is a string (list of characters). If not, stop with an error.
#'
#' @inheritParams validateIsOfType
#' @export
validateIsString <- function(object, nullAllowed = FALSE) {
  validateIsOfType(object, "character", nullAllowed)
}

#' Check if the provided object is numeric. If not, stop with an error.
#'
#' @inheritParams validateIsOfType
#' @export
validateIsNumeric <- function(object, nullAllowed = FALSE) {
  # Only NA values. It is numeric
  if (all(is.na(object))) {
    return()
  }

  validateIsOfType(object, c("numeric", "integer"), nullAllowed)
}

#' Check if the provided object is integer. If not, stop with an error.
#'
#' @inheritParams validateIsOfType
#' @export
validateIsInteger <- function(object, nullAllowed = FALSE) {
  if (nullAllowed && is.null(object)) {
    return()
  }

  if (all(floor(object) == object, na.rm = TRUE)) {
    return()
  }

  # Name of the variable in the calling function
  objectName <- deparse(substitute(object))
  objectTypes <- "integer"

  stop(messages$errorWrongType(objectName, class(object)[1], objectTypes))
}

#' Check if the provided object is logical. If not, stop with an error.
#'
#' @inheritParams validateIsOfType
#' @export
validateIsLogical <- function(object, nullAllowed = FALSE) {
  validateIsOfType(object, "logical", nullAllowed)
}

#' Check if the provided lists are of the same length. If not, stop with an error.
#'
#' @param ... Lists to be checked.
#' @export
validateIsSameLength <- function(...) {
  if (isSameLength(...)) {
    return()
  }

  # Name of the arguments
  argnames <- sys.call()
  arguments <- paste(lapply(argnames[-1], as.character), collapse = ", ")

  stop(messages$errorDifferentLength(arguments))
}

#' Check if the file exists. If not, stop with an error.
#'
#' @param filePath Path to the file
#' @export
validateFileExists <- function(filePath) {
  if (!file.exists(filePath)) {
    stop(messages$erroFileNotFound(filePath))
  }
}

#' Check if the provided object is of a certain length. If not, stop with an error.
#'
#' @param object Object which length will be checked
#' @param length Expected length
#' @export
validateLength <- function(object, length) {
  if (length(object) == length) {
    return()
  }

  # Name of the variable in the calling function
  objectName <- deparse(substitute(object))

  stop(messages$errorWrongLength(objectName, length))
}
