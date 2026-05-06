# Section validation adapters ----
#
# Registered in `.validationAdapters` (R/validation.R) and called by
# `.runProjectValidation()`.

#' @keywords internal
#' @noRd
.individualsValidatorAdapter <- function(project) {
  .validateIndividuals(project$individuals)
}

#' @keywords internal
#' @noRd
.individualParameterSetsValidatorAdapter <- function(project) {
  .validateParameterSets(
    project$individualParameterSets,
    "individualParameterSets"
  )
}

#' Validate the `individuals` section of a Project
#'
#' Checks `species` and `gender` are present and warns when numeric
#' fields (`weight`, `height`, `age`) are non-numeric. Cross-references
#' to `individualParameterSets` are validated in
#' `.validateCrossReferences()`.
#'
#' @param individuals Named list from `project$individuals`.
#' @return validationResult.
#' @keywords internal
#' @noRd
.validateIndividuals <- function(individuals) {
  result <- validationResult$new()

  if (is.null(individuals) || length(individuals) == 0) {
    result$add_warning("Data", "No individuals defined")
    return(result)
  }

  requiredFields <- c("species", "gender")
  for (id in names(individuals)) {
    indiv <- individuals[[id]]

    result <- .check_required_fields(
      indiv,
      requiredFields,
      paste0("individual '", id, "'"),
      result
    )

    for (numField in c("weight", "height", "age")) {
      val <- indiv[[numField]]
      if (!is.null(val) && !is.na(val) && !is.numeric(val)) {
        result$add_warning(
          "Data Type",
          paste0(
            "Field '",
            numField,
            "' in individual '",
            id,
            "' should be numeric"
          )
        )
      }
    }
  }

  result
}

#' Create a parameter set describing an individual and write it to the Excel
#' file
#'
#' @param individualCharacteristics An `IndividualCharacteristics` object
#'   describing the individual. See `createIndividualCharacterstics` for more
#'   information.
#' @param outputXLSPath Path to the Excel file the parameter set will be written
#'   to
#'
#' @return Path to the created Excel file
#'
#' @seealso createIndividualCharacteristics crateIndividual
#'
#' @examples
#' \dontrun{
#' simulation <- loadSimulation(pathToPKML)
#' humanIndividualCharacteristics <- createIndividualCharacteristics(
#'   species = Species$Human, population = HumanPopulation$European_ICRP_2002,
#'   gender = Gender$Male, weight = 70
#' )
#' writeIndividualToXLS(humanIndividualCharacteristics, pathToExcelFile)
#' }
#'
#' @export
writeIndividualToXLS <- function(individualCharacteristics, outputXLSPath) {
  validateIsString(outputXLSPath)

  individual <- createIndividual(individualCharacteristics)

  columnNames <- c("Container Path", "Parameter Name", "Value", "Units")

  containerPaths <- vector(
    "character",
    length(individual$distributedParameters$paths)
  )
  paramNames <- vector(
    "character",
    length(individual$distributedParameters$paths)
  )
  values <- vector("numeric", length(individual$distributedParameters$paths))
  units <- vector("character", length(individual$distributedParameters$paths))

  for (i in seq_along(individual$distributedParameters$paths)) {
    splittedPath <- .splitParameterPathIntoContainerAndName(individual$distributedParameters$paths[[
      i
    ]])

    containerPaths[i] <- splittedPath$containerPath
    paramNames[i] <- splittedPath$parameterName
    values[i] <- individual$distributedParameters$values[[i]]
    units[i] <- individual$distributedParameters$units[[i]]
  }

  output <- data.frame(
    unlist(containerPaths, use.names = FALSE),
    unlist(paramNames, use.names = FALSE),
    unlist(as.numeric(values), use.names = FALSE),
    unlist(units, use.names = FALSE)
  )
  colnames(output) <- columnNames

  .writeExcel(data = output, path = outputXLSPath)
  return(outputXLSPath)
}

#' Read individual characteristics from file
#'
#' @details Read individual characteristics from an excel sheet
#' and create an `IndividualCharacteristics`-object. The excel sheet must have
#' the columns `IndividualId`, `Species`, `Population`, `Gender`, `Weight [kg]`,
#' `Height [cm]`, `Age [year(s)]`, and `Protein Ontogenies`.

#' @param XLSpath Full path to the excel file
#'
#' @param individualId (String) Id of the individual as stored in the
#'   `IndividualId` column.
#' @param sheet Name of the sheet. If `NULL` (default), the first sheet of the
#'   file is used.
#' @param nullIfNotFound Boolean. If `TRUE` (default), `NULL` is returned if
#' no entry with the give `individualId` exists. Otherwise, an error is thrown.
#'
#' @returns An `IndividualCharacteristics` object
#' @import ospsuite
#' @export
readIndividualCharacteristicsFromXLS <- function(
  XLSpath, # nolint: object_length_linter.
  individualId,
  sheet = "IndividualBiometrics",
  nullIfNotFound = TRUE
) {
  validateIsString(c(XLSpath, individualId))

  # If no sheet has been specified, read from the first sheet
  if (is.null(sheet)) {
    sheet <- c(1)
  }

  columnNames <- c(
    "IndividualId",
    "Species",
    "Population",
    "Gender",
    "Weight [kg]",
    "Height [cm]",
    "Age [year(s)]",
    "Protein Ontogenies"
  )

  data <- readExcel(path = XLSpath, sheet = sheet)
  if (!all(columnNames %in% names(data))) {
    stop(messages$errorWrongXLSStructure(XLSpath, columnNames))
  }
  # Find the row with the given individual id
  rowIdx <- which(data$IndividualId == individualId)
  if (length(rowIdx) == 0) {
    if (nullIfNotFound) {
      return(NULL)
    }
    stop(messages$errorWrongIndividualId(individualId))
  }

  # Create ontogenies for the proteins
  moleculeOntogenies <- .readOntongeniesFromXLS(data[rowIdx, ])

  # Create the IndividualCharacteristics object
  individualCharacteristics <- ospsuite::createIndividualCharacteristics(
    species = data$Species[[rowIdx]],
    population = data$Population[[rowIdx]],
    gender = data$Gender[[rowIdx]],
    weight = data$`Weight [kg]`[[rowIdx]],
    height = data$`Height [cm]`[[rowIdx]],
    age = data$`Age [year(s)]`[[rowIdx]],
    moleculeOntogenies = moleculeOntogenies
  )

  return(individualCharacteristics)
}

#' Read individual-specific model parameters from file
#'
#' @details Reads the list of individual parameter set sheet names from the
#'   `Individual Parameter Sets` column in the `IndividualBiometrics` sheet,
#'   then reads and combines the parameters from each of those sheets.
#'
#' @param XLSpath Full path to the excel file.
#' @param individualId (String) Id of the individual as stored in the
#'   `IndividualId` column.
#' @param scenarioName (String) Name of the scenario, used in warning messages.
#' @param sheet Name of the sheet containing individual biometrics. Defaults to
#'   `"IndividualBiometrics"`.
#'
#' @returns A list with elements `paths`, `values`, and `units` containing the
#'   combined parameters from all listed individual parameter set sheets.
#'   Returns `NULL` if `individualId` is not found in the biometrics sheet.
#' @keywords internal
#' @noRd
.readIndividualParameterSetsFromXLS <- function(
  XLSpath, # nolint: object_length_linter.
  individualId,
  scenarioName,
  sheet = "IndividualBiometrics"
) {
  validateIsString(c(XLSpath, individualId))

  data <- readExcel(path = XLSpath, sheet = sheet)

  rowIdx <- which(data$IndividualId == individualId)
  if (length(rowIdx) == 0) {
    return(NULL)
  }

  # "Individual Parameter Sets" column is required
  paramSetsStr <- data[["Individual Parameter Sets"]][[rowIdx]]

  # Initialize empty params structure
  params <- list(
    paths = character(0),
    values = numeric(0),
    units = character(0)
  )

  # If empty or NA, return empty params structure
  if (is.na(paramSetsStr) || !nzchar(trimws(as.character(paramSetsStr)))) {
    return(params)
  }

  parameterSets <- trimws(strsplit(
    as.character(paramSetsStr),
    ",",
    fixed = TRUE
  )[[1]])
  excelSheets <- readxl::excel_sheets(path = XLSpath)

  for (paramSet in parameterSets) {
    if (any(excelSheets == paramSet)) {
      setParams <- readParametersFromXLS(XLSpath, sheets = paramSet)
      params <- extendParameterStructure(
        parameters = params,
        newParameters = setParams
      )
    } else {
      stop(messages$errorIndividualParameterSetNotFound(
        scenarioName = scenarioName,
        parameterSetName = paramSet
      ))
    }
  }

  return(params)
}

#' Apply an individual to the simulation. For human species, only parameters
#' that do not override formulas are applied. For other species, all parameters
#' returned by `createIndividual` are applied.
#'
#' @param individualCharacteristics `IndividualCharacteristics` describing an
#'   individual. Optional
#' @param simulation `Simulation` loaded from the PKML file
#' @import ospsuite
#' @export
#'
#' @examples
#' \dontrun{
#' simulation <- loadSimulation(filePath = modelPath)
#' humanIndividualCharacteristics <- createIndividualCharacteristics(
#'   species = Species$Human, population = HumanPopulation$European_ICRP_2002,
#'   gender = Gender$Male, weight = 70
#' )
#' applyIndividualParameters(humanIndividualCharacteristics, simulation)
#' }
applyIndividualParameters <- function(individualCharacteristics, simulation) {
  individual <- ospsuite::createIndividual(individualCharacteristics)

  # For human species, only set distributed parameters
  allParamPaths <- individual$distributedParameters$paths
  allParamValues <- individual$distributedParameters$values
  allParamUnits <- individual$distributedParameters$units

  # For other species, also add derived parameters
  if (individualCharacteristics$species != ospsuite::Species$Human) {
    allParamPaths <- c(allParamPaths, individual$derivedParameters$paths)
    allParamValues <- c(allParamValues, individual$derivedParameters$values)
    allParamUnits <- c(allParamUnits, individual$derivedParameters$units)
  }

  ospsuite::setParameterValuesByPath(
    parameterPaths = allParamPaths,
    values = allParamValues,
    simulation = simulation,
    units = allParamUnits,
    stopIfNotFound = FALSE
  )
}

# Public CRUD: individuals ----

#' Add an individual to a Project
#'
#' @param project A `Project` object.
#' @param individualId Character scalar, unique ID for the individual.
#' @param species Character scalar, species name.
#' @param ... Optional named fields: `population`, `gender`, `weight`,
#'   `height`, `age`, `proteinOntogenies`, `parameterSets`. Numeric
#'   fields are coerced via `as.double()`. `parameterSets` is a
#'   character vector of ids referencing
#'   `project$individualParameterSets`. Unknown fields trigger an
#'   error.
#' @returns The `project` object, invisibly.
#' @export
#' @family individual
addIndividual <- function(project, individualId, species, ...) {
  validateIsOfType(project, "Project")
  errors <- character()

  if (
    !is.character(individualId) ||
      length(individualId) != 1L ||
      is.na(individualId) ||
      nchar(individualId) == 0
  ) {
    errors <- c(errors, "individualId must be a non-empty string")
  } else if (individualId %in% names(project$individuals)) {
    errors <- c(
      errors,
      paste0("individual '", individualId, "' already exists")
    )
  }

  if (
    !is.character(species) ||
      length(species) != 1L ||
      is.na(species) ||
      nchar(species) == 0
  ) {
    errors <- c(errors, "species must be a non-empty string")
  }

  dots <- list(...)
  allowed <- c(
    "population",
    "gender",
    "weight",
    "height",
    "age",
    "proteinOntogenies",
    "parameterSets"
  )
  unknown <- setdiff(names(dots), allowed)
  if (length(unknown) > 0L) {
    errors <- c(
      errors,
      paste0(
        "unknown fields: ",
        paste(unknown, collapse = ", "),
        ". Allowed: ",
        paste(allowed, collapse = ", ")
      )
    )
  }

  if (length(errors) > 0L) {
    cli::cli_abort(c(
      "Cannot add individual {.val {individualId}}:",
      stats::setNames(errors, rep("x", length(errors)))
    ))
  }

  entry <- list(species = species)
  for (field in c("population", "gender", "proteinOntogenies")) {
    if (!is.null(dots[[field]])) entry[[field]] <- dots[[field]]
  }
  for (field in c("weight", "height", "age")) {
    if (!is.null(dots[[field]])) {
      entry[[field]] <- as.double(dots[[field]])
    }
  }
  if (!is.null(dots$parameterSets)) {
    if (!is.character(dots$parameterSets)) {
      cli::cli_abort(
        "{.arg parameterSets} must be a character vector of set ids"
      )
    }
    bad <- setdiff(
      dots$parameterSets,
      names(project$individualParameterSets %||% list())
    )
    if (length(bad) > 0L) {
      cli::cli_abort(c(
        "{.arg parameterSets} references undefined individual parameter sets:",
        "x" = "{.val {bad}}"
      ))
    }
    entry$parameterSets <- dots$parameterSets
  }
  class(entry) <- c("Individual", "list")

  project$individuals[[individualId]] <- entry
  project$.markModified()
  invisible(project)
}

#' Remove an individual from a Project
#'
#' @param project A `Project` object.
#' @param individualId Character scalar, ID of the individual to remove.
#' @returns The `project` object, invisibly.
#' @export
#' @family individual
removeIndividual <- function(project, individualId) {
  validateIsOfType(project, "Project")
  if (
    !is.character(individualId) ||
      length(individualId) != 1L ||
      is.na(individualId) ||
      nchar(individualId) == 0
  ) {
    cli::cli_abort("{.arg individualId} must be a non-empty string")
  }
  if (!(individualId %in% names(project$individuals))) {
    cli::cli_warn("individual {.val {individualId}} not found; no-op.")
    return(invisible(project))
  }
  .warnIfReferenced(project, "individual", individualId)
  project$individuals[[individualId]] <- NULL
  project$.markModified()
  invisible(project)
}

#' Replace the parameter-set references on an individual
#'
#' @param project A `Project` object.
#' @param individualId Character scalar.
#' @param parameterSets Character vector of set ids (from
#'   `project$individualParameterSets`). Use `character(0)` to clear.
#' @returns The `project` object, invisibly.
#' @export
#' @family individual
setIndividualParameterSets <- function(project, individualId, parameterSets) {
  validateIsOfType(project, "Project")
  if (!(individualId %in% names(project$individuals))) {
    cli::cli_abort("individual {.val {individualId}} not found")
  }
  if (!is.character(parameterSets)) {
    cli::cli_abort("{.arg parameterSets} must be a character vector")
  }
  bad <- setdiff(
    parameterSets,
    names(project$individualParameterSets %||% list())
  )
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "{.arg parameterSets} references undefined individual parameter sets:",
      "x" = "{.val {bad}}"
    ))
  }
  project$individuals[[individualId]]$parameterSets <- parameterSets
  project$.markModified()
  invisible(project)
}

# Public CRUD: individualParameterSets ----

#' Create an individual parameter set
#' @param project A `Project` object.
#' @param id Character scalar, set name. Must not already exist.
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
addIndividualParameterSet <- function(project, id) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1L || is.na(id) || nchar(id) == 0) {
    cli::cli_abort("{.arg id} must be a non-empty string")
  }
  if (id %in% names(project$individualParameterSets)) {
    cli::cli_abort("individual parameter set {.val {id}} already exists")
  }
  project$individualParameterSets[[id]] <- list()
  project$.markModified()
  invisible(project)
}

#' Remove an individual parameter set
#' @inheritParams addIndividualParameterSet
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
removeIndividualParameterSet <- function(project, id) {
  validateIsOfType(project, "Project")
  if (!(id %in% names(project$individualParameterSets))) {
    cli::cli_warn("individual parameter set {.val {id}} not found; no-op.")
    return(invisible(project))
  }
  .warnIfReferenced(project, "individualParameterSet", id)
  project$individualParameterSets[[id]] <- NULL
  project$.markModified()
  invisible(project)
}

#' Add a parameter entry to a named individual parameter set
#'
#' Adds one parameter entry to the named set in
#' `project$individualParameterSets`. The set is created on demand if it
#' does not yet exist. Last-write-wins on duplicate paths.
#'
#' @param project A `Project` object.
#' @param id Character scalar, set name. Created if not present.
#' @param containerPath Character scalar.
#' @param parameterName Character scalar.
#' @param value Numeric scalar.
#' @param units Character scalar.
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
addIndividualParameterSetEntry <- function(
  project,
  id,
  containerPath,
  parameterName,
  value,
  units
) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1L || is.na(id) || nchar(id) == 0) {
    cli::cli_abort("{.arg id} must be a non-empty string")
  }
  current <- project$individualParameterSets[[id]]
  project$individualParameterSets[[id]] <- .addParameterEntry(
    current,
    containerPath,
    parameterName,
    value,
    units
  )
  project$.markModified()
  invisible(project)
}

#' Remove a parameter entry from a named individual parameter set
#'
#' Removes one parameter entry from the named set. If the removed entry
#' was the last in the set, the set itself is auto-removed from
#' `project$individualParameterSets`. Warns if the set or entry doesn't
#' exist.
#'
#' @inheritParams addIndividualParameterSetEntry
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
removeIndividualParameterSetEntry <- function(
  project,
  id,
  containerPath,
  parameterName
) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1L) {
    cli::cli_abort("{.arg id} must be a string scalar")
  }
  if (!(id %in% names(project$individualParameterSets))) {
    cli::cli_warn("individual parameter set {.val {id}} not found; no-op.")
    return(invisible(project))
  }
  updated <- .removeParameterEntry(
    project$individualParameterSets[[id]],
    containerPath,
    parameterName
  )
  if (is.null(updated)) {
    .warnIfReferenced(project, "individualParameterSet", id)
    project$individualParameterSets[[id]] <- NULL
  } else {
    project$individualParameterSets[[id]] <- updated
  }
  project$.markModified()
  invisible(project)
}
