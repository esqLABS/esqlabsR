#' Read parameter values from a structured Excel file.
#' Each excel sheet must consist of columns 'Container Path', 'Parameter Name',
#' 'Value', and 'Units'
#'
#' @param paramsXLSpath Path to the excel file
#' @param sheets Names of the excel sheets containing the information about the parameters. Multiple sheets
#' can be processed. If no sheets are provided, the first one in the Excel file is used.
#'
#' @return A list containing lists 'paths' with the full paths to the parameters, 'values' the values of the parameters,
#' and 'units' with the units the values are ín.
#' @import openxlsx
#' @export
readParametersFromXLS <- function(paramsXLSpath, sheets = NULL) {
  columnNames <- c("Container.Path", "Parameter.Name", "Value", "Units")

  validateIsString(paramsXLSpath)
  validateIsString(sheets, nullAllowed = TRUE)

  if (is.null(sheets)) {
    sheets <- c(1)
  }

  paths <- c()
  values <- c()
  units <- c()

  for (sheet in sheets) {
    data <- read.xlsx(xlsxFile = paramsXLSpath, sheet = sheet)

    if (!all(names(data) == columnNames)) {
      stop(messages$errorWrongParamsXLSStructure(paramsXLSpath))
    }
    for (i in seq_along(data[["Container.Path"]])) {
      path <- paste(data[["Container.Path"]][[i]], data[["Parameter.Name"]][[i]], sep = "|")
      value <- data[["Value"]][[i]]
      unit <- data[["Units"]][[i]]

      # Check if the entry with this path is already in the output paths.
      # If yes, overwrite the value, otherwise append a new entry.
      idx <- match(path, paths)
      if (is.na(idx)) {
        paths <- c(paths, path)
        values <- c(values, value)
        units <- c(units, unit)
      } else {
        values[[idx]] <- value
        units[[idx]] <- unit
      }
    }
  }

  return(list(paths = paths, values = values, units = units))
}

#' Check if two parameters are equal is respect to certain properties. The parameters are not equal if:
#' The paths of the parameters are not equal;
#' The types of the formulas differ (types checked: isConstant, isDistributed, isExplicit, isTable);
#' Constant formulas have different values;
#' Distributed formulas have different values (not checking for distribution)
#' Explicit formulas: If formula string are not equal, OR one of the parameter values is fixed (formula is overridden),
#' OR both parameter values are fixed and differ,
#' OR checkFormulaValues is TRUE and the values differ (disregarding of overridden or not)
#' Table formulas: If the number of points differ, OR any of the points differn,
#' OR one of the parameter values is fixed (formula is overridden),
#' OR both parameter values are fixed and differ
#' #'
#' @param parameter1 First parameter to compare
#' @param parameter2 Second parameter to compare
#' @param checkFormulaValues If TRUE, values of explicit formulas are always compared. Otherwise, the values
#' are only compared if the formulas are overridden (isFixedValue == TRUE). FALSE by default.
#'
#' @return
#' @export
isParametersEqual <- function(parameter1, parameter2, checkFormulaValues = FALSE) {
  validateIsOfType(c(parameter1, parameter2), "Parameter")

  # Check for the path
  if (parameter1$path != parameter2$path) {
    return(FALSE)
  }
  formula1 <- parameter1$formula
  formula2 <- parameter2$formula

  # Check for formula type equality
  if (!all(
    c(formula1$isConstant, formula1$isDistributed, formula1$isExplicit, formula1$isTable) ==
      c(formula2$isConstant, formula2$isDistributed, formula2$isExplicit, formula2$isTable)
  )) {
    return(FALSE)
  }

  # Constant or distributed formula - check for value
  if (formula1$isConstant || formula1$isDistributed) {
    return(parameter1$value == parameter2$value)
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
    if (checkFormulaValues && (parameter1$value != parameter2$value)) {
      return(FALSE)
    }

    return(formula1$formulaString == formula2$formulaString)
  }

  if (formula1$isTable) {
    return(isTableFormulasEqual(formula1, formula2))
  }

  #  stop(messages$errorCouldNotCompareParameters(parameter1, parameter2))
  return(FALSE)
}

#' Check if two table formulas are equal.
#' Table formulas are equal if the number of points is equal and all x-y value pairs
#' are equal between the two formulas
#'
#' @param formula1 First formula to compare
#' @param formula2 Second formula to compare
#'
#' @return TRUE if the table formulas are equal, FALSE otherwise
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

#' Set the values of parameters in the simulation by path, if the \code{condition} is true.
#'
#' @param parameterPaths A single or a list of parameter path
#' @param values A numeric value that should be assigned to the parameters or a vector
#' of numeric values, if the value of more than one parameter should be changed. Must have the same
#' length as 'parameterPaths'
#' @param condition A function that receives a \code{Parmeter} as an argument
#' and returns \code{TRUE} of \code{FALSE}
#' #' @param units A string or a list of strings defining the units of the \code{values}. If \code{NULL} (default), values
#' are assumed to be in base units. If not \code{NULL}, must have the same length as 'parameterPaths'.
#' @param simulation Simulation used to retrieve parameter instances from given paths.
#'
#' @examples
#' \dontrun{
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' setParameterValuesByPath("Organism|Liver|Volume", 1, sim)
#'
#' condition <- function(p) {
#'   !p$isFormula
#' }
#' setParameterValuesByPath(c("Organism|Liver|Volume", "Organism|Volume"), c(2, 3), sim, condition)
#' }
#' @export
setParameterValuesByPathWithCondition <- function(parameterPaths, values, simulation, condition = function(p) {
                                                    TRUE
                                                  }, units = NULL) {
  validateIsString(c(parameterPaths, units))
  validateIsNumeric(values)
  validateIsOfType(simulation, "Simulation")

  for (i in seq_along(parameterPaths)) {
    param <- getParameter(parameterPaths[[i]], simulation)
    if (condition(param)) {
      setParameterValuesByPathWithUnit(parameterPaths = parameterPaths[[i]], values = values[[i]], simulation = simulation, units = units)
    }
  }
}

#' Set the values of parameters in the simulation by path with is specified unit.
#'
#' @param parameterPaths A single or a list of parameter path
#' @param values A numeric value that should be assigned to the parameters or a vector
#' of numeric values, if the value of more than one parameter should be changed. Must have the same
#' length as 'parameterPaths'.
#' @param units A string or a list of strings defining the units of the \code{values}. If \code{NULL} (default), values
#' are assumed to be in base units. If not \code{NULL}, must have the same length as 'parameterPaths'.
#' @param simulation Simulation used to retrieve parameter instances from given paths.
#' @export
setParameterValuesByPathWithUnit <- function(parameterPaths, values, simulation, units = NULL) {
  validateIsString(c(parameterPaths, units))
  validateIsNumeric(values)
  validateIsOfType(simulation, "Simulation")

  for (i in seq_along(parameterPaths)) {
    param <- getParameter(parameterPaths[[i]], simulation)
    valueInBaseUnit <- values[[i]]
    if (!is.null(units)) {
      valueInBaseUnit <- toBaseUnit(quantity = param, values = valueInBaseUnit, unit = units[[i]])
    }
  }
  setParameterValues(parameters = param, values = valueInBaseUnit)
}
