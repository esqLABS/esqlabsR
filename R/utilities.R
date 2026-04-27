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

#' Calculate geometric mean of a numeric vector
#'
#' @param x Numeric array to calculate geometric mean for
#' @param na.rm A logical value indicating whether `NA` values should be
#'   stripped before the computation proceeds
#' @param trim Fraction (0 to 0.5) of observations to be trimmed from each end
#'   of `x` before the mean is computed. Values of trim outside that range are
#'   taken as the nearest endpoint
#'
#' @returns Geometric mean of `x`
#' @export
geomean <- function(x, na.rm = FALSE, trim = 0) {
  exp(mean(log(x), na.rm = na.rm, trim = trim))
}

#' Calculate geometric standard deviation of a numeric vector
#'
#' @param x Numeric array
#' @param na.rm A logical value indicating whether `NA` values should be
#'   stripped before the computation proceeds.
#'
#' @returns Geometric standard deviation of `x`
#' @export
geosd <- function(x, na.rm = FALSE) {
  exp(stats::sd(log(x), na.rm = na.rm))
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

#' Warn if a removed entity is still referenced by any scenario
#'
#' @param project A `Project` object.
#' @param entityType One of "individual", "population", "application",
#'   "modelParameterSet", "outputPath".
#' @param id The ID/name being removed.
#' @returns `invisible(NULL)`; emits a `cli::cli_warn()` if any scenario
#'   still references `id`.
#' @keywords internal
#' @noRd
.warnIfReferenced <- function(project, entityType, id) {
  scenarios <- project$scenarios %||% list()
  if (length(scenarios) == 0) {
    return(invisible(NULL))
  }

  refs <- character()
  for (name in names(scenarios)) {
    sc <- scenarios[[name]]
    hit <- switch(
      entityType,
      "individual" = identical(sc$individualId, id),
      "population" = identical(sc$populationId, id),
      "application" = identical(sc$applicationProtocol, id),
      "modelParameterSet" = isTRUE(id %in% sc$modelParameters),
      "outputPath" = {
        pathValue <- project$outputPaths[[id]]
        isTRUE(pathValue %in% sc$outputPaths)
      },
      FALSE
    )
    if (isTRUE(hit)) refs <- c(refs, name)
  }

  if (length(refs) > 0) {
    cli::cli_warn(c(
      "Removed {entityType} {.val {id}} is still referenced by {length(refs)} scenario{?s}:",
      "*" = "{refs}",
      "i" = "These scenarios now have a dangling reference. Update or remove them."
    ))
  }
  invisible(NULL)
}
