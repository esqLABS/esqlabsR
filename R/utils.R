#' Find value in an array
#'
#' @description Find the index of the value in an array that is closest to given
#'   one. By default, no restriction is applied how big the absolute numerical
#'   distance between `value` and a value in the `array` may be. A limit can be
#'   set by the parameters `thresholdAbs` or `thresholdRel`. If no value within
#'   the `array` has the distance to `value` that is equal to or less than the
#'   threshold, the `value` is considered not present in the `array` and `NULL`
#'   is returned.
#'
#' @param value Numerical value
#' @param array Numerical array
#' @param thresholdAbs Absolute numerical distance by which the closest value in
#'   `array` may differ from `value` to be accepted. If both `thresholdAbs` and
#'   `thresholdRel` are `NULL` (default), no threshold is applied. If
#'   `thresholdAbs` is set, `thresholdRel` is ignored. If 0, only exact match
#'   between `value` and `array` is accepted.
#' @param thresholdRel A fraction by which the closest value may differ from
#'   `value` to be accepted. **WARNING**: setting a relative threshold will
#'   result in only exact matches if `value` is 0!
#'
#' @returns Index of a value within the array which is closest to `value` and
#'   the difference is within the defined threshold. If multiple entries of
#'   `array` have the same difference which is minimal, a vector of indices is
#'   returned. If no value is within the defined threshold, `NULL` is returned.
#' @export
getIndexClosestToValue <- function(
  value,
  array,
  thresholdAbs = NULL,
  thresholdRel = NULL
) {
  # If no absolute threshold is set, calculate if from relative threshold
  if (is.null(thresholdAbs)) {
    # If no relative threshold is set also, no threshold is applied
    if (!is.null(thresholdRel)) {
      thresholdAbs <- abs(value * thresholdRel)
    } else {
      thresholdAbs <- Inf
    }
  }

  validateIsNumeric(c(value, array))

  # Calculate distances
  distances <- abs(array - value)
  idx <- which(distances == min(distances) & distances <= thresholdAbs)

  if (length(idx) == 0) {
    warning(messages$warningValueWithinThresholdNotExisting(
      value,
      thresholdAbs
    ))
    return(NULL)
  }

  return(idx)
}

#' Remove an entry from a list
#'
#' @param entry The entry to be removed
#' @param listArg The list from which the entry will be removed
#' @description Removes all occurrences of the entry from the list. If the entry
#'   is not in the list nothing is removed.
#' @returns The list without the entry. If the input is a vector, it is
#'   converted to a list.
#'
#' @examples
#' myList <- list("one", "two", "one", "three")
#' myList <- removeFromList("one", myList)
#' print(myList)
#' @export
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

#' Compare values including `NA`
#'
#' @param v1 Value or a list of values to compare. May include `NA`.
#' @param v2 Value or a list of values to compare. May include `NA`.
#' @details From http://www.cookbook-r.com/Manipulating_data/Comparing_vectors_or_factors_with_NA/
#'
#' @returns `TRUE` wherever elements are the same, including `NA`'s,
# and `FALSE` everywhere else.
#' @export
compareWithNA <- function(v1, v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

#' Parse simulation time intervals from a string
#' @param simulationTimeIntervalsString A string with format "start,end,res" or
#'   "start1,end1,res1;start2,end2,res2"
#' @returns A list of numeric vectors, or NULL if input is NULL.
#' @keywords internal
#' @noRd
.parseSimulationTimeIntervals <- function(simulationTimeIntervalsString) {
  if (is.null(simulationTimeIntervalsString)) {
    return(NULL)
  }
  simulationTimeIntervals <- strsplit(
    x = simulationTimeIntervalsString,
    split = ";",
    fixed = TRUE
  )[[1]]
  simulationTimeIntervals <- strsplit(
    x = simulationTimeIntervals,
    split = ",",
    fixed = TRUE
  )
  simulationTimeIntervals <- lapply(simulationTimeIntervals, as.numeric)
  validateIsNumeric(simulationTimeIntervals)
  if (any(unlist(simulationTimeIntervals) < 0)) {
    stop(messages$stopWrongTimeIntervalString(simulationTimeIntervalsString))
  }
  if (any(sapply(simulationTimeIntervals, length) != 3)) {
    stop(messages$stopWrongTimeIntervalString(simulationTimeIntervalsString))
  }
  if (any(sapply(simulationTimeIntervals, function(x) x[3] <= 0))) {
    stop(messages$stopWrongTimeIntervalString(simulationTimeIntervalsString))
  }
  if (any(sapply(simulationTimeIntervals, function(x) x[1] >= x[2]))) {
    stop(messages$stopWrongTimeIntervalString(simulationTimeIntervalsString))
  }
  return(simulationTimeIntervals)
}

#' Get the name of the molecule from a quantity
#'
#' @description Returns the name of the molecule to which the quantity object is
#'   associated. The quantity could be the amount of the molecule in a container
#'   ('Organism|VenousBlood|Plasma|Aciclovir'), a parameter of the molecule
#'   ('Aciclovir|Lipophilicity' or
#'   'Organism|VenousBlood|Plasma|Aciclovir|Concentration'), or an observer
#'   ("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous
#'   Blood)").
#'
#'   If the quantity is not associated with a molecule (e.g. 'Organism|Weight'),
#'   an error is thrown.
#'
#' @param quantity A `Quantity` object
#'
#' @returns Name of the molecule the quantity is associated with.
#' @export
#'
#' @examples
#' simulation <- loadSimulation(system.file("extdata", "Aciclovir.pkml", package = "ospsuite"))
#' quantity <- getQuantity(
#'   path = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
#'   container = simulation
#' )
#' getMoleculeNameFromQuantity(quantity = quantity)
getMoleculeNameFromQuantity <- function(quantity) {
  validateIsOfType(quantity, "Quantity")

  quantityType <- quantity$quantityType

  # If the passed quantitiy is a molecule, return its name
  if (any(c("Drug", "Molecule") == quantityType)) {
    return(quantity$name)
  }

  # Otherwise try to get its parent container
  parentContainer <- quantity$parentContainer
  parentContainerType <- parentContainer$containerType

  # If parent container is not a molecule, stop with an error
  if (!(any(c("Drug", "Molecule") == parentContainerType))) {
    stop(messages$cannotGetMoleculeFromQuantity(quantity$path))
  }

  return(parentContainer$name)
}

#' Add a new key-value pairs to an enum, where the value is a list.
#'
#' @param key Key to be added
#' @param values Values to be added
#' @param enum enum the key-value pairs should be added to.
#' **WARNING**: the original object is not modified!
#' @param overwrite if TRUE and a `key` exists, it will be overwritten with the
#'   new value. Otherwise, an error is thrown. Default is `FALSE`.
#'
#' @returns Enum with added key-value pair.
#' @export
#'
#' @examples
#' library(ospsuite.utils)
#' myEnum <- enum(c(a = "b"))
#' myEnum <- enumPut("c", "d", myEnum)
#' myEnum <- enumPut(c("c", "d", "g"), list(12, 2, "a"), myEnum, overwrite = TRUE)
#' myEnum <- enumPutList("g", list(12, 2, "a"), myEnum, overwrite = TRUE)
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
