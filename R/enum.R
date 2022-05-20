#' Add a new key-value pairs to an enum, where the value is a list.
#'
#' @param key Key to be added
#' @param values Values to be added
#' @param enum enum the key-value pairs should be added to.
#' **WARNING**: the original object is not modified!
#' @param overwrite if TRUE and a `key` exists, it will be overwritten with the
#'   new value. Otherwise, an error is thrown. Default is `FALSE`.
#'
#' @return Enum with added key-value pair.
#' @export
#'
#' @examples
#' library(ospsuite.utils)
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
