#' Convenience function to avoid testing for null. It returns the first object if it is not null otherwise the second object
#'
#' @param lhs Object that will be returned if not NULL
#' @param rhs Object that will be returned if \code{lhs} is NULL. It maybe well be NULL
#'
#' @return The first parameter if it is not NULL otherwise the second parameter
#' @export
`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) {
    lhs
  } else {
    rhs
  }
}

#' Get the index of the value in an array that is closest to given one.
#'
#' @param value Numerical value
#' @param array Numerical array
#'
#' @return Index of a value within the array which is closest to \code{value}
#' @export
getIndexClosestToValue <- function(value, array) {
  validateIsNumeric(c(value, array))
  idx <- which(abs(array - value) == min(abs(array - value)))

  return(idx)
}

#' Calculate geometric mean of a numeric vector.
#'
#' @param x Numeric array
#' @param na.rm A logical value indicating whether \code{NA} values should be stripped before the computation proceeds.
#' @param trim the fraction (0 to 0.5) of observations to be trimmed from each end of \code{x} before the mean is computed.
#' Values of trim outside that range are taken as the nearest endpoint.
#'
#' @return Geometric mean of\code{x}
#' @export
geomean <- function(x, na.rm = FALSE, trim = 0) {
  exp(mean(log(x), na.rm = na.rm, trim = trim))
}

#' Calculate geometric standard deviation of a numeric vector
#'
#' @param x Numeric array
#' @param na.rm A logical value indicating whether \code{NA} values should be stripped before the computation proceeds.
#'
#' @return Geometric standard deviation of \code{x}
#' @export
geosd <- function(x, na.rm = FALSE) {
  exp(sd(log(x), na.rm = na.rm))
}

#' Calculate quantiles for given xy-vectors
#'
#' @param quantiles A numerical vector with quantile values. Default is
#' \code{c(0.05, 0.5, 0.95)}
#' @param xValues X values, by which y values are grouped
#' @param yValues Values for which the quantiles are calculated
#'
#' @return A list with \code{xValues} and aggregated \code{yValues}
#' @export
getQuantilesYData <- function(xValues, yValues, quantiles = c(0.05, 0.5, 0.95)) {
  validateIsNumeric(c(xValues, yValues, quantiles))
  validateIsSameLength(xValues, yValues)
  output <- list()
  # Aggregate time values
  for (quantile in quantiles) {
    aggregatedData <- aggregate(yValues, by = list(xVals = xValues), FUN = quantile, quantile)
    output[[as.character(quantile)]] <- list()
    output[[as.character(quantile)]][["xValues"]] <- aggregatedData$xVals
    output[[as.character(quantile)]][["yValues"]] <- aggregatedData[[2]]
  }

  return(output)
}


#' Get the simulation container of the entity
#'
#' @param entity Object of type \code{Entity}
#'
#' @return The root container that is the parent of the entity.
getSimulationContainer <- function(entity) {
  validateIsOfType(entity, "Entity")
  if (isOfType(entity, "Container")) {
    if (entity$containerType == "Simulation") {
      return(entity)
    }
  }
  return(getSimulationContainer(entity$parentContainer))
}

#' Returns an instance of the specified .NET Task
#'
#' @param taskName The name of the task to retrieve (without the Get)
#'
#' @return An instance of the Task
#'
#' @details
#' simulationLoader <- getNetTask("SimulationLoader")
getNetTask <- function(taskName) {
  rClr::clrCallStatic("OSPSuite.R.Api", paste0("Get", taskName))
}

#' Return an instance of the .NET Task "DimensionTask".
#'
#' @return An instance of the Task
getDimensionTask <- function() {
  dimTask <- esqlabsEnv$DimensionTask
  if (is.null(dimTask)) {
    dimTask <- getNetTask("DimensionTask")
    esqlabsEnv$DimensionTask <- dimTask
  }
  return(dimTask)
}

#' Get hash code of the .NET object
#'
#' @param netWrapper Any object from the ospsuite-R that inhertis from DotNetWrapper
#'
#' @return Value of the .NET-method "GetHashCode"
getNetHashCode <- function(netWrapper) {
  validateIsOfType(netWrapper, "DotNetWrapper")
  rClr::clrCall(netWrapper$ref, "GetHashCode")
}

#' Escape a string for possible regular expression match
#'
#' @param string String to be escaped
#'
#' @return string with prefix "\\Q" and suffix "\\E" appended.
#' The resulting string will not be recognized as a regular expression pattern.
escapeForRegex <- function(string) {
  paste0("\\Q", string, "\\E")
}

#' Remove an entry from a list
#'
#' @param entry The entry to be removed
#' @param listArg The list from which the entry will be removed
#' @description Removes all occurrences of the entry from the list. If the entry is not in the list
#' nothing is removed.
#' @return The list without the entry. If the input is a vector, it is converted to a list.
#' @export
#'
#' @examples
#' myList <- list("one", "two", "one", "three")
#' myList <- removeFromList("one", myList)
#' print(myList)
removeFromList <- function(entry, listArg) {
  if (is.null(listArg)) {
    return(NULL)
  }
  if (!is.list(listArg)) {
    listArg <- as.list(listArg)
  }
  idx <- which(entry == listArg)
  listArg[idx] <- NULL
  return(listArg)
}


#' Compare values including NA
#'
#' @param v1 Value or a list of values to compare. May include NA.
#' @param v2 Value or a list of values to compare. May include NA.
#' @details From http://www.cookbook-r.com/Manipulating_data/Comparing_vectors_or_factors_with_NA/
#'
#' @return TRUE wherever elements are the same, including NA's,
# and FALSE everywhere else.
#' @export
compareWithNA <- function(v1, v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

#' Is a character part of string?
#'
#' @param char Character to find in the string
#' @param string String that should contain the character
#'
#' @return TRUE if the \code{character} is a substring if \code{string}, FALSE otherwise
#' @export
#'
#' @examples
#' isCharInString("a", "bsdalk")
isCharInString <- function(char, string) {
  any(unlist(strsplit(string, ""), use.names = FALSE) == char)
}

#' Make sure the object is a list
#'
#' @param object To be converted to a list
#'
#' @return If \code{is.list(object) == TRUE}, returns the \code{object}, otherwise \code{list(object)}
enforceIsList <- function(object) {
  if (is.list(object)) {
    return(object)
  }
  return(list(object))
}
