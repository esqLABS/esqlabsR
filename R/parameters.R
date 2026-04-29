# Parameters: section management for Project$modelParameters
# + the underlying parameter-data-structure manipulation logic used
# everywhere parameter sets are merged, mutated, or split into
# (containerPath, parameterName, value, units) tuples.
#
# Section concerns (parse + validate + serialize + mutation) own
# Project$modelParameters end-to-end. Called by:
#   - Project$.read_json() via .parseParameterGroups()
#   - .runProjectValidation() via .validateModelParameters()
#   - .projectToJson() via .parameterGroupsToJson()
#   - users via the public addModelParameter / removeModelParameter functions.
#
# The lower half (extendParameterStructure, S3 addParameter/removeParameter,
# .addParameterEntry, .removeParameterEntry, etc.) is shared infrastructure
# used by the individuals, applications, and modelParameters sections to
# build and edit the (paths, values, units) parallel-vector structure.

# Parse ----

#' @keywords internal
#' @noRd
.parseParameterGroups <- function(groups) {
  if (is.null(groups)) {
    return(list())
  }
  result <- list()
  for (name in names(groups)) {
    entries <- groups[[name]]
    paths <- character(0)
    values <- numeric(0)
    units <- character(0)
    for (entry in entries) {
      paths <- c(
        paths,
        paste(
          entry$containerPath,
          entry$parameterName,
          sep = "|"
        )
      )
      values <- c(values, as.numeric(entry$value))
      units <- c(units, entry$units %||% "")
    }
    result[[name]] <- list(paths = paths, values = values, units = units)
  }
  result
}

# Validate ----

#' Validate modelParameters section of a Project
#' @param modelParameters Named list from project$modelParameters
#' @return validationResult object
#' @keywords internal
.validateModelParameters <- function(modelParameters) {
  .validateParameterGroups(modelParameters, "modelParameters")
}

#' @keywords internal
#' @noRd
.modelParametersValidatorAdapter <- function(project) {
  .validateModelParameters(project$modelParameters)
}

# Serialize ----

#' @keywords internal
#' @noRd
.parameterGroupsToJson <- function(groups) {
  if (is.null(groups) || length(groups) == 0) {
    return(list())
  }

  result <- list()
  for (name in names(groups)) {
    group <- groups[[name]]
    entries <- list()
    for (i in seq_along(group$paths)) {
      split <- .splitParameterPathIntoContainerAndName(group$paths[i])
      entries[[i]] <- list(
        containerPath = split$containerPath,
        parameterName = split$parameterName,
        value = group$values[i],
        units = if (group$units[i] == "") NULL else group$units[i]
      )
    }
    result[[name]] <- entries
  }
  result
}

# Public CRUD ----

#' Add a parameter to a named model-parameter set
#'
#' @description Adds one parameter entry to the named set in
#' `project$modelParameters`. The set is created on demand if it does not
#' yet exist. Last-write-wins on duplicate paths.
#'
#' @param project A `Project` object.
#' @param id Character scalar, set name. Created if not present.
#' @param containerPath Character scalar.
#' @param parameterName Character scalar.
#' @param value Numeric scalar.
#' @param units Character scalar.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
addModelParameter <- function(
  project,
  id,
  containerPath,
  parameterName,
  value,
  units
) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1 || is.na(id) || nchar(id) == 0) {
    stop("id must be a non-empty string")
  }
  current <- project$modelParameters[[id]]
  project$modelParameters[[id]] <- .addParameterEntry(
    current,
    containerPath,
    parameterName,
    value,
    units
  )
  project$.markModified()
  invisible(project)
}

#' Remove a parameter from a named model-parameter set
#'
#' @description Removes one parameter entry from the named set. If the
#' removed entry was the last in the set, the set itself is auto-removed
#' from `project$modelParameters`. Warns if the set or entry doesn't exist.
#'
#' @param project A `Project` object.
#' @param id Character scalar, set name.
#' @param containerPath Character scalar.
#' @param parameterName Character scalar.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
removeModelParameter <- function(project, id, containerPath, parameterName) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1) {
    stop("id must be a string scalar")
  }
  if (!(id %in% names(project$modelParameters))) {
    cli::cli_warn("model parameter set {.val {id}} not found; no-op.")
    return(invisible(project))
  }
  updated <- .removeParameterEntry(
    project$modelParameters[[id]],
    containerPath,
    parameterName
  )
  if (is.null(updated)) {
    .warnIfReferenced(project, "modelParameterSet", id)
    project$modelParameters[[id]] <- NULL
  } else {
    project$modelParameters[[id]] <- updated
  }
  project$.markModified()
  invisible(project)
}

# Parameter structure manipulation ----

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

# Simulation parameter setters ----

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

# S3 dispatch: addParameter / removeParameter on entity types ----

#' Add a parameter to an entity
#'
#' @description Generic function. Methods exist for objects of class
#' `Individual` and `Application`. Adds one parameter entry (joining
#' `containerPath` and `parameterName` with `|` to form the path) into
#' the entity's `$parameters` parallel-vector structure, with last-write-wins
#' semantics on duplicate paths.
#'
#' @param x The entity (`Individual` or `Application`).
#' @param containerPath Character scalar.
#' @param parameterName Character scalar.
#' @param value Numeric scalar.
#' @param units Character scalar.
#' @param ... Reserved.
#' @returns The modified entity.
#' @export
addParameter <- function(x, containerPath, parameterName, value, units, ...) {
  UseMethod("addParameter")
}

#' @export
addParameter.Individual <- function(
  x,
  containerPath,
  parameterName,
  value,
  units,
  ...
) {
  x$parameters <- .addParameterEntry(
    x$parameters,
    containerPath,
    parameterName,
    value,
    units
  )
  x
}

#' @export
addParameter.Application <- function(
  x,
  containerPath,
  parameterName,
  value,
  units,
  ...
) {
  x$parameters <- .addParameterEntry(
    x$parameters,
    containerPath,
    parameterName,
    value,
    units
  )
  x
}

#' @export
addParameter.default <- function(
  x,
  containerPath,
  parameterName,
  value,
  units,
  ...
) {
  stop(sprintf(
    "addParameter() has no method for class %s",
    paste(class(x), collapse = "/")
  ))
}

#' Remove a parameter from an entity
#'
#' @description Generic function. Methods exist for `Individual` and
#' `Application`. Removes the entry whose path equals
#' `paste(containerPath, parameterName, sep = "|")`. If the removed entry
#' was the last one, the entity's `$parameters` slot becomes `NULL`. Warns
#' and leaves the entity unchanged if no matching entry exists.
#'
#' @param x The entity.
#' @param containerPath Character scalar.
#' @param parameterName Character scalar.
#' @param ... Reserved.
#' @returns The modified entity.
#' @export
removeParameter <- function(x, containerPath, parameterName, ...) {
  UseMethod("removeParameter")
}

#' @export
removeParameter.Individual <- function(x, containerPath, parameterName, ...) {
  x$parameters <- .removeParameterEntry(
    x$parameters,
    containerPath,
    parameterName
  )
  x
}

#' @export
removeParameter.Application <- function(x, containerPath, parameterName, ...) {
  x$parameters <- .removeParameterEntry(
    x$parameters,
    containerPath,
    parameterName
  )
  x
}

#' @export
removeParameter.default <- function(x, containerPath, parameterName, ...) {
  stop(sprintf(
    "removeParameter() has no method for class %s",
    paste(class(x), collapse = "/")
  ))
}

# Low-level (paths, values, units) entry helpers ----

#' @keywords internal
#' @noRd
.addParameterEntry <- function(
  parameters,
  containerPath,
  parameterName,
  value,
  units
) {
  errors <- character()
  if (
    !is.character(containerPath) ||
      length(containerPath) != 1 ||
      is.na(containerPath) ||
      nchar(containerPath) == 0
  ) {
    errors <- c(errors, "containerPath must be a non-empty string")
  }
  if (
    !is.character(parameterName) ||
      length(parameterName) != 1 ||
      is.na(parameterName) ||
      nchar(parameterName) == 0
  ) {
    errors <- c(errors, "parameterName must be a non-empty string")
  }
  if (!is.numeric(value) || length(value) != 1 || is.na(value)) {
    errors <- c(errors, "value must be a numeric scalar")
  }
  if (!is.character(units) || length(units) != 1) {
    errors <- c(errors, "units must be a string scalar (use \"\" for none)")
  }
  if (length(errors) > 0) {
    stop(paste0(
      "Invalid parameter entry:\n- ",
      paste(errors, collapse = "\n- ")
    ))
  }

  newPath <- paste(containerPath, parameterName, sep = "|")
  newEntry <- list(
    paths = newPath,
    values = as.double(value),
    units = units
  )
  extendParameterStructure(
    parameters = parameters,
    newParameters = newEntry
  )
}

#' @keywords internal
#' @noRd
.removeParameterEntry <- function(parameters, containerPath, parameterName) {
  if (is.null(parameters) || length(parameters$paths) == 0) {
    cli::cli_warn(
      "parameter {.val {paste(containerPath, parameterName, sep='|')}} not found; no-op."
    )
    return(parameters)
  }
  target <- paste(containerPath, parameterName, sep = "|")
  idx <- which(parameters$paths == target)
  if (length(idx) == 0) {
    cli::cli_warn("parameter {.val {target}} not found; no-op.")
    return(parameters)
  }
  keep <- -idx
  newPaths <- parameters$paths[keep]
  if (length(newPaths) == 0) {
    return(NULL)
  }
  list(
    paths = newPaths,
    values = parameters$values[keep],
    units = parameters$units[keep]
  )
}

#' Validate parameter list structure
#'
#' @param parameterStructure Object to be checked. Expected is a named list with
#'   names "paths", "values", and "units".
#'
#' @keywords internal
#'
#' @returns `TRUE` if validation succeeded (silently). Throws an error
#'   otherwise.
.validateParametersStructure <- function(
  parameterStructure,
  argumentName = NULL,
  nullAllowed = FALSE
) {
  if (is.null(parameterStructure) && nullAllowed) {
    return(invisible(TRUE))
  }

  if (!identical(names(parameterStructure), c("paths", "values", "units"))) {
    stop(messages$wrongParametersStructure(argumentName = argumentName))
  }
  return(invisible(TRUE))
}
