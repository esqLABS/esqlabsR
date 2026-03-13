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

#' Read individual parameter sets information from file
#'
#' @details Read the species and individual parameter sets for a given
#'   individual from the `IndividualBiometrics` sheet of an Excel file.
#'   The `Individual Parameter Sets` column is optional and, if present, must
#'   contain a comma-separated list of sheet names in the same file.
#'
#' @param XLSpath Full path to the excel file
#' @param individualId (String) Id of the individual as stored in the
#'   `IndividualId` column.
#' @param sheet Name of the sheet containing individual biometrics. Defaults to
#'   `"IndividualBiometrics"`.
#'
#' @returns A list with elements `species` (character string) and
#'   `individualParameterSets` (character vector of sheet names, or `NULL` if
#'   the column is absent or empty). Returns `NULL` if `individualId` is not
#'   found.
#' @export
readIndividualParameterSetsFromXLS <- function(
  XLSpath, # nolint: object_length_linter.
  individualId,
  sheet = "IndividualBiometrics"
) {
  validateIsString(c(XLSpath, individualId))

  data <- readExcel(path = XLSpath, sheet = sheet)

  rowIdx <- which(data$IndividualId == individualId)
  if (length(rowIdx) == 0) {
    return(NULL)
  }

  species <- data$Species[[rowIdx]]

  # "Individual Parameter Sets" column is optional
  individualParameterSets <- NULL
  if ("Individual Parameter Sets" %in% names(data)) {
    paramSetsStr <- data[["Individual Parameter Sets"]][[rowIdx]]
    if (!is.na(paramSetsStr) && nzchar(trimws(as.character(paramSetsStr)))) {
      individualParameterSets <- trimws(
        strsplit(as.character(paramSetsStr), ",", fixed = TRUE)[[1]]
      )
    }
  }

  return(list(species = species, individualParameterSets = individualParameterSets))
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
