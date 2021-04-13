#' Get the key mapped to the given value in an \code{enum}
#'
#' @param enum The enum where the key-value pair is stored
#' @param value The value that is mapped to the key
#'
#' @return Key under which the value is stored. If the value is not in the enum, \code{NULL} is returned
#' @export
enumGetKey <- function(enum, value) {
  ospsuite::getEnumKey(enum, value)
}

#' Add a new key-value pairs to an enum, where the value is a list.
#'
#' @param key Key to be added
#' @param values Values to be added
#' @param enum enum the key-value pairs should be added to.
#' WARNING: the original object is not modified!
#' @param overwrite if TRUE and a \code{key} exists,
#' it will be overwritten with the new value. Otherwise, an error is thrown. Default is FALSE.
#'
#' @return Enum with added key-value pair.
#' @export
#'
#' @examples
#' myEnum <- enum(c(a = "b"))
#' myEnum <- enumPut("c", "d", myEnum)
#' myEnum <- enumPut(c("c", "d", "g"), list(12, 2, "a"), myEnum, overwrite = TRUE)
enumPutList <- function(key, values, enum, overwrite = FALSE) {
  if (length(key) > 1) {
    stop(messages$errorEnumPutListMultipleKeys())
  }
  if (enumHasKey(key, enum) && !overwrite) {
    stop(messages$errorKeyInEnumPresent(key))
  }
  enum[[key]] <- values

  return(enum)
}
