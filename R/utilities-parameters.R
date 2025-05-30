#' Read parameter values from a structured Excel file.
#' Each excel sheet must consist of columns 'Container Path', 'Parameter Name',
#' 'Value', and 'Units'
#'
#' @param paramsXLSpath Path to the excel file
#' @param sheets Names of the excel sheets containing the information about the
#'   parameters. Multiple sheets can be processed. If no sheets are provided,
#'   the first one in the Excel file is used.
#'
#' @returns A list containing vectors 'paths' with the full paths to the
#'   parameters, 'values' the values of the parameters, and 'units' with the
#'   units the values are in.
#' @export
readParametersFromXLS <- function(paramsXLSpath, sheets = NULL) {
  columnNames <- c("Container Path", "Parameter Name", "Value", "Units")
  validateIsString(paramsXLSpath)
  validateIsString(sheets, nullAllowed = TRUE)

  if (is.null(sheets)) {
    sheets <- c(1)
  }

  pathsValuesVector <- vector(mode = "numeric")
  pathsUnitsVector <- vector(mode = "character")

  for (sheet in sheets) {
    data <- readExcel(path = paramsXLSpath, sheet = sheet)

    if (!all(columnNames %in% names(data))) {
      stop(messages$errorWrongXLSStructure(filePath = paramsXLSpath, expectedColNames = columnNames))
    }

    fullPaths <- paste(data[["Container Path"]], data[["Parameter Name"]], sep = "|")
    pathsValuesVector[fullPaths] <- as.numeric(data[["Value"]])

    pathsUnitsVector[fullPaths] <- tidyr::replace_na(data = as.character(data[["Units"]]), replace = "")
  }

  return(.parametersVectorToList(pathsValuesVector, pathsUnitsVector))
}

#' Write parameter structure to excel that can be loaded in MoBi
#'
#' @param parameterStructure A list containing vectors 'paths' with the full paths to the parameters,
#' 'values' the values of the parameters, and 'units' with the units the values are in.
#'
#' @param paramsXLSpath Path to the excel file
#' @param sheet (Optional) name of the excel sheet
#' @param append If TRUE, the existing excel file/sheet will be appended with the new
#'  parameter structure. If FALSE (default), the existing file will be
#'  overwritten.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' params <- list(paths = c("Container1|Path1", "Container|Second|Third|Path2"), values = c(1, 2), units = c("", "µmol"))
#'
#' writeParameterStructureToXLS(params, "test.xlsx")
#' }
#'
writeParameterStructureToXLS <- function(parameterStructure, paramsXLSpath, sheet = NULL, append = FALSE) {
  if (isTRUE(append)) {
    existingData <- readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheet)
    parameterStructure$paths <- c(existingData$paths, parameterStructure$paths)
    parameterStructure$values <- c(existingData$values, parameterStructure$values)
    parameterStructure$units <- c(existingData$units, parameterStructure$units)
  }

  .validateParametersStructure(parameterStructure, "parameterStructure")
  # Split full parameter paths into container path and parameter name
  containerPaths <- unlist(lapply(parameterStructure$paths, \(x){
    .splitParameterPathIntoContainerAndName(x)$containerPath
  }), use.names = FALSE)
  parameterNames <- unlist(lapply(parameterStructure$paths, \(x){
    .splitParameterPathIntoContainerAndName(x)$parameterName
  }), use.names = FALSE)

  # Create a data frame with the parameter structure
  output <- data.frame(
    "Container Path" = containerPaths,
    "Parameter Name" = parameterNames,
    Value = parameterStructure$values,
    Units = parameterStructure$units,
    check.names = FALSE
  )

  # Write the results into an excel file.
  # Wrap the output data frame into a list and name the list if sheet name
  # has been provided
  data <- list(output)
  if (!is.null(sheet)) {
    names(data) <- sheet
  }
  .writeExcel(data = data, path = paramsXLSpath)
}

#' Export simulation parameters to excel
#'
#' @description Creates an excel file with information from the passed
#' parameters. The excel sheet will contain columns "Container Path",
#' "Parameter Name", "Value", and "Units". The resulting file can be loaded in
#' `MoBi` or in `R` with the function `readParametersFromXLS()`.
#'
#' @param parameters A single or a list of `Parameter` objects
#' @param paramsXLSpath Path to the excel file
#' @param sheet (Optional) name of the excel sheet

#' @export
exportParametersToXLS <- function(parameters, paramsXLSpath, sheet = NULL) {
  validateIsOfType(parameters, "Parameter")
  validateIsCharacter(sheet, nullAllowed = TRUE)
  if (!is.null(sheet)) {
    validateIsOfLength(sheet, 1)
  }
  # Make sure parameters is a list even if only one parameter is passed
  parameters <- c(parameters)

  parameterContainerPath <-
    parameterUnits <-
    parameterName <- vector("character", length(parameters))
  parameterValue <- vector("numeric", length(parameters))

  for (paramIdx in seq_along(parameters)) {
    param <- parameters[[paramIdx]]
    value <- param$value
    if (!is.nan(value)) {
      parameterContainerPath[[paramIdx]] <- param$parentContainer$path
      parameterName[[paramIdx]] <- param$name
      parameterUnits[[paramIdx]] <- param$unit
      parameterValue[[paramIdx]] <- param$value
    } else {
      # Set to NA so these entries are removed
      parameterContainerPath[[paramIdx]] <- NA
      parameterName[[paramIdx]] <- NA
      parameterUnits[[paramIdx]] <- NA
      parameterValue[[paramIdx]] <- NA
    }
  }

  output <- data.frame(
    unlist(parameterContainerPath, use.names = FALSE),
    unlist(parameterName, use.names = FALSE),
    unlist(parameterValue, use.names = FALSE),
    unlist(parameterUnits, use.names = FALSE)
  ) %>%
    # Remove rows for which all values are NA
    dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.)))

  if (length(output) > 0) {
    colnames(output) <- c("Container Path", "Parameter Name", "Value", "Units")
  }

  # Write the results into an excel file.
  # Wrap the output data frame into a list and name the list if sheet name
  # has been provided
  data <- list(output)
  if (!is.null(sheet)) {
    names(data) <- sheet
  }
  .writeExcel(data = data, path = paramsXLSpath)
}

#' Extend parameters structure with new entries
#'
#' @param parameters A list containing vectors 'paths' with the full paths to the
#'   parameters, 'values' the values of the parameters, and 'units' with the
#'   units the values are in. This list will be extended.
#' @param newParameters A list containing vectors 'paths' with the full paths to the
#'   parameters, 'values' the values of the parameters, and 'units' with the
#'   units the values are in. Entries from this list will extend or overwrite
#'   the list `parameters`
#'
#' @details This function adds new parameter entries from `newParameters` to
#'  `parameters`. If an entry with the same path is already present in `parameters`,
#'  its value and unit will be overwritten with the values from `newParameters`.
#'
#' @returns Updated list of parameter paths, values, and units
#' @export
extendParameterStructure <- function(parameters, newParameters) {
  .validateParametersStructure(
    parameterStructure = parameters,
    argumentName = "parameters"
  )
  .validateParametersStructure(
    parameterStructure = newParameters,
    argumentName = "newParameters"
  )

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
#' @param pathsValuesVector Named vector of numerical parameter values
#' with parameter paths as names
#' @param pathsUnitsVector Named vector of parameter values units with parameter
#' paths as names
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
#' @details
#' The parameters are not equal if:
#' The paths of the parameters are not equal;
#' The types of the formulas differ (types checked: isConstant, isDistributed, isExplicit, isTable);
#' Constant formulas have different values;
#' Distributed formulas have different values (not checking for distribution)
#' Explicit formulas: If formula string are not equal, OR one of the parameter
#' values is fixed (formula is overridden),
#' OR both parameter values are fixed and differ,
#' OR checkFormulaValues is TRUE and the values differ (disregarding of overridden or not)
#' Table formulas: If the number of points differ, OR any of the points differ,
#' OR one of the parameter values is fixed (formula is overridden),
#' OR both parameter values are fixed and differ.
#'
#' @param parameter1 First parameter to compare
#' @param parameter2 Second parameter to compare
#' @param checkFormulaValues If TRUE, values of explicit formulas are always
#'   compared. Otherwise, the values are only compared if the formulas are
#'   overridden (isFixedValue == TRUE). FALSE by default.
#' @param compareFormulasByValue If `FALSE`(default), formulas are compared by their types and string. If `TRUE`,
#'  only values are compared.
#'
#' @returns `TRUE` if parameters are considered equal, `FALSE` otherwise
#' @export
isParametersEqual <- function(parameter1, parameter2, checkFormulaValues = FALSE, compareFormulasByValue = FALSE) {
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
  if (!all(
    c(formula1$isConstant, formula1$isDistributed, formula1$isExplicit, formula1$isTable) ==
      c(formula2$isConstant, formula2$isDistributed, formula2$isExplicit, formula2$isTable)
  )) {
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
    if (checkFormulaValues && (!identical(parameter1$value, parameter2$value))) {
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

#' Set the values of parameters in the simulation by path, if the `condition` is true.
#'
#' @param parameterPaths A single or a list of parameter path
#' @param values A numeric value that should be assigned to the parameters or a vector
#' of numeric values, if the value of more than one parameter should be changed. Must have the same
#' length as 'parameterPaths'
#' @param condition A function that receives a parameter path as an argument
#' and returns `TRUE` of `FALSE`
#' @param units A string or a list of strings defining the units of the `values`. If `NULL` (default), values
#' are assumed to be in base units. If not `NULL`, must have the same length as 'parameterPaths'.
#' @param simulation Simulation used to retrieve parameter instances from given paths.
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
setParameterValuesByPathWithCondition <- function(parameterPaths, # nolint: object_length_linter.
                                                  values,
                                                  simulation,
                                                  condition = function(path) {
                                                    TRUE
                                                  },
                                                  units = NULL) {
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
#' by '|'
#'
#' @returns A list with elements 'containerPath' and 'parameterName'
#' @keywords internal
#' @noRd
.splitParameterPathIntoContainerAndName <- function(parameterPath) {
  fullPathParts <- strsplit(parameterPath, split = "|", fixed = TRUE)[[1]]

  containerPath <- paste(fullPathParts[seq_along(fullPathParts) - 1], collapse = "|")
  paramName <- fullPathParts[[length(fullPathParts)]]
  return(list(containerPath = containerPath, parameterName = paramName))
}
