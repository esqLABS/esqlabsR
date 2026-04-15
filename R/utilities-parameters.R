#' Extend parameters structure with new entries
#'
#' @param parameters A parameter structure (a list with elements `paths`,
#'   `values`, and `units`) or `NULL`. If `NULL`, it is treated as an empty
#'   parameter structure.
#' @param newParameters A parameter structure (a list with elements `paths`,
#'   `values`, and `units`) or `NULL`. If `NULL`, it is treated as an empty
#'   parameter structure whose entries will be added to or overwrite those in
#'   `parameters`.
#'
#' @details This function adds new parameter entries from `newParameters` to
#'   `parameters`. If an entry with the same path is already present in
#'   `parameters`, its value and unit will be overwritten with the values from
#'   `newParameters`.
#'
#' @returns Updated list of parameter paths, values, and units
#' @export
extendParameterStructure <- function(parameters, newParameters) {
  .validateParametersStructure(
    parameterStructure = parameters,
    argumentName = "parameters",
    nullAllowed = TRUE
  )
  .validateParametersStructure(
    parameterStructure = newParameters,
    argumentName = "newParameters",
    nullAllowed = TRUE
  )

  # Normalize NULL inputs to empty parameter structures
  emptyStructure <- list(paths = NULL, values = NULL, units = NULL)
  parameters <- parameters %||% emptyStructure
  newParameters <- newParameters %||% emptyStructure

  # If the parameters structure is empty, return new parameters
  if (isEmpty(parameters$paths)) {
    return(newParameters)
  }

  # If the new parameters structure is empty, return parameters
  if (isEmpty(newParameters$paths)) {
    return(parameters)
  }

  # Convert the input parameter structure into named vectors.
  pathsValuesVector <- parameters$values
  names(pathsValuesVector) <- parameters$paths
  pathsUnitsVector <- parameters$units
  names(pathsUnitsVector) <- parameters$paths

  # Add new entries resp. update with new values
  pathsValuesVector[newParameters$paths] <- newParameters$values
  pathsUnitsVector[newParameters$paths] <- newParameters$units

  return(.parametersVectorToList(pathsValuesVector, pathsUnitsVector))
}

#' Convert parameters vector structure to list structure
#'
#' @param pathsValuesVector Named vector of numerical parameter values with
#'   parameter paths as names
#' @param pathsUnitsVector Named vector of parameter values units with parameter
#'   paths as names
#'
#' @noRd
#'
#' @returns A named list with vectors `paths`, `values`, and `units`
#' @keywords internal
.parametersVectorToList <- function(pathsValuesVector, pathsUnitsVector) {
  paths <- names(pathsValuesVector)

  returnVal <- list(
    paths = paths,
    values = unname(pathsValuesVector[paths]),
    units = unname(pathsUnitsVector[paths])
  )

  return(returnVal)
}

#' @title Check if two parameters are equal with respect to certain properties.
#'
#' @details The parameters are not equal if: The paths of the parameters are not
#' equal; The types of the formulas differ (types checked: isConstant,
#' isDistributed, isExplicit, isTable); Constant formulas have different values;
#' Distributed formulas have different values (not checking for distribution)
#' Explicit formulas: If formula string are not equal, OR one of the parameter
#' values is fixed (formula is overridden), OR both parameter values are fixed
#' and differ, OR checkFormulaValues is TRUE and the values differ (disregarding
#' of overridden or not) Table formulas: If the number of points differ, OR any
#' of the points differ, OR one of the parameter values is fixed (formula is
#' overridden), OR both parameter values are fixed and differ.
#'
#' @param parameter1 First parameter to compare
#' @param parameter2 Second parameter to compare
#' @param checkFormulaValues If TRUE, values of explicit formulas are always
#'   compared. Otherwise, the values are only compared if the formulas are
#'   overridden (isFixedValue == TRUE). FALSE by default.
#' @param compareFormulasByValue If `FALSE`(default), formulas are compared by
#'   their types and string. If `TRUE`, only values are compared.
#'
#' @returns `TRUE` if parameters are considered equal, `FALSE` otherwise
#' @export
isParametersEqual <- function(
  parameter1,
  parameter2,
  checkFormulaValues = FALSE,
  compareFormulasByValue = FALSE
) {
  validateIsOfType(c(parameter1, parameter2), "Parameter")

  # Check for the path
  if (parameter1$path != parameter2$path) {
    return(FALSE)
  }

  formula1 <- parameter1$formula
  formula2 <- parameter2$formula

  # Compare by value
  if (compareFormulasByValue) {
    return(identical(parameter1$value, parameter2$value))
  }

  # Check for formula type equality
  if (
    !all(
      c(
        formula1$isConstant,
        formula1$isDistributed,
        formula1$isExplicit,
        formula1$isTable
      ) ==
        c(
          formula2$isConstant,
          formula2$isDistributed,
          formula2$isExplicit,
          formula2$isTable
        )
    )
  ) {
    return(FALSE)
  }

  # Constant or distributed formula - check for value
  # Comparing using 'identical' to capture NaN and NA cases which can happen
  if (formula1$isConstant || formula1$isDistributed) {
    return(identical(parameter1$value, parameter2$value))
  }

  # Explicit or table formula - check if values are overridden
  if (parameter1$isFixedValue) {
    if (!parameter2$isFixedValue) {
      return(FALSE)
    }
    if (parameter1$value != parameter2$value) {
      return(FALSE)
    }
  }

  # Explicit
  if (formula1$isExplicit) {
    if (
      checkFormulaValues && (!identical(parameter1$value, parameter2$value))
    ) {
      return(FALSE)
    }

    return(formula1$formulaString == formula2$formulaString)
  }

  if (formula1$isTable) {
    return(isTableFormulasEqual(formula1, formula2))
  }

  return(FALSE)
}

#' Check if two table formulas are equal.
#'
#' Table formulas are equal if the number of points is equal and all x-y value
#' pairs are equal between the two formulas
#'
#' @param formula1 First formula to compare
#' @param formula2 Second formula to compare
#'
#' @returns TRUE if the table formulas are equal, FALSE otherwise
#' @export
isTableFormulasEqual <- function(formula1, formula2) {
  allPoints1 <- formula1$allPoints
  allPoints2 <- formula2$allPoints

  if (length(allPoints1) != length(allPoints2)) {
    return(FALSE)
  }

  for (i in seq_along(allPoints1)) {
    point1 <- allPoints1[[i]]
    point2 <- allPoints2[[i]]

    return((point1$x == point2$x) && (point1$y == point2$y))
  }
}

#' Set the values of parameters in the simulation by path, if the `condition` is
#' true.
#'
#' @param parameterPaths A single or a list of parameter path
#' @param values A numeric value that should be assigned to the parameters or a
#'   vector of numeric values, if the value of more than one parameter should be
#'   changed. Must have the same length as `parameterPaths`
#' @param condition A function that receives a parameter path as an argument and
#'   returns `TRUE` of `FALSE`
#' @param units A string or a list of strings defining the units of the
#'   `values`. If `NULL` (default), values are assumed to be in base units. If
#'   not `NULL`, must have the same length as `parameterPaths`.
#' @param simulation Simulation used to retrieve parameter instances from given
#'   paths.
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' condition <- function(path) {
#'   ospsuite::isExplicitFormulaByPath(
#'     path = path,
#'     simulation = sim
#'   )
#' }
#' setParameterValuesByPathWithCondition(
#'   c("Organism|Liver|Volume", "Organism|Volume"),
#'   c(2, 3),
#'   sim,
#'   condition
#' )
#' @import ospsuite
#' @export
setParameterValuesByPathWithCondition <- function(
  parameterPaths, # nolint: object_length_linter.
  values,
  simulation,
  condition = function(path) {
    TRUE
  },
  units = NULL
) {
  for (i in seq_along(parameterPaths)) {
    path <- parameterPaths[[i]]
    if (condition(path)) {
      ospsuite::setParameterValuesByPath(
        parameterPaths = parameterPaths[[i]],
        values = values[[i]],
        simulation = simulation,
        units = units[[i]]
      )
    }
  }
}

#' Split parameter path into container path and parameter name
#'
#' @param parameterPath Full path to the parameter, with path elements separated
#'   by `|`
#'
#' @returns A list with elements `containerPath` and `parameterName`
#' @keywords internal
#' @noRd
.splitParameterPathIntoContainerAndName <- function(parameterPath) {
  fullPathParts <- strsplit(parameterPath, split = "|", fixed = TRUE)[[1]]

  containerPath <- paste(
    fullPathParts[seq_along(fullPathParts) - 1],
    collapse = "|"
  )
  paramName <- fullPathParts[[length(fullPathParts)]]
  return(list(containerPath = containerPath, parameterName = paramName))
}
