#' Create a map data structure.
#'
#' @param keys List of keys
#' @param values List of values
#'
#' @return the Map created
#' @export
map <- function(keys, values) {
  ospsuite:::validateIsSameLength(keys, values)
  # Check if all keys are unique
  if (length(keys) != length(unique(keys))) {
    stop(messages$errorMapKeysNotUnique(keys))
  }

  myMap <- as.list(values)
  names(myMap) <- keys
  return(myMap)
}

#' Get the key mapped to the given value in a \code{map}
#'
#' @param map The map where the key-value pair is stored
#' @param value The value that is mapped to the key
#'
#' @return Key under which the value is stored. If the value is not in the map, \code{NULL} is returned
#' @export
mapGetKey <- function(map, value) {
  enumGetKey(map, value)
}

#' Return the value that is stored under the given key. If the key is not present, an error is thrown.
#'
#' @param map map that contains the key-value pair
#' @param key key under which the value is stored
#'
#' @details Consider using mymap[[key]] instead.
#'
#' @return Value that is assigned to \code{key}. If the key is not present, an error is thrown.
#' @import ospsuite
#' @export
mapGetValue <- function(map, key) {
  ospsuite::enumGetValue(map, key)
}

#' Add new key-value pairs to a map.
#'
#' @param keys Keys of the values to be added
#' @param values Values to be added
#' @param map A map the key-value pairs should be added to.
#' WARNING: the original object is not modified!
#' @param overwrite if TRUE and a value with any of the given \code{keys} exists,
#' it will be overwritten with the new value. Otherwise, an error is thrown. Default is FALSE.
#'
#' @return Map with added key-value pair.
#' @export
#' @import ospsuite
#'
#' @examples
#' myMap <- map("a", "b")
#' myMap <- mapPut("c", "d", myMap)
#' myMap <- mapPut(c("c", "d", "g"), list(12, 2, "a"), myMap, overwrite = TRUE)
mapPut <- function(keys, values, map, overwrite = FALSE) {
  ospsuite::enumPut(keys, values, map, overwrite)
}

#' Add a new key-value pairs to a map, where the value is a list.
#'
#' @param key Key to be added
#' @param values Value to be added
#' @param map map the key-value pair should be added to.
#' WARNING: the original object is not modified!
#' @param overwrite if TRUE and a \code{key} exists,
#' it will be overwritten with the new value. Otherwise, an error is thrown. Default is FALSE.
#'
#' @return Map with added key-value pair.
#' @export
#'
#' @examples
#' myMap <- map("a", "b")
#' myMap <- mapPutList(c("c"), list(12, 2, "a"), myMap, overwrite = TRUE)
mapPutList <- function(key, values, map, overwrite = FALSE) {
  enumPutList(key, values, map, overwrite)
}

#' Remove an entry from a map
#'
#' @param keys Key(s) of entries to be removed from the map
#' @param map Map from which the entries to be removed
#' WARNING: the original object is not modified!
#'
#' @return Map without the removed entries
#' @import ospsuite
#' @export
mapRemove <- function(keys, map) {
  ospsuite::enumRemove(keys, map)
}

#' Check if a map has a certain key.
#'
#' @param key Key to check for
#' @param map Map where to look for the key
#'
#' @return TRUE if a key-value pair for \code{key} exists, FALSE otherwise
#' @import ospsuite
#' @export
mapHasKey <- function(key, map) {
  ospsuite::enumHasKey(key, map)
}

#' Return the keys of a map
#'
#' @param map \code{map} containing the keys
#'
#' @return List of key names
#' @import ospsuite
#' @export
mapKeys <- function(map) {
  ospsuite::enumKeys(map)
}

#' Return the values stored in a map
#'
#' @param map \code{map} containing the values
#'
#' @return List of values stored in the \code{map}
#' @import ospsuite
#' @export
mapValues <- function(map) {
  ospsuite::enumValues(map)
}
