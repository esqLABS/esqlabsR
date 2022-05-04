#' Create a parameter set describing an individual and write it to the Excel file
#'
#' @param individualCharacteristics An `IndividualCharacteristics` object
#'   describing the individual. See `createIndividualCharacterstics` for more
#'   information.
#' @param outputXLSPath Path to the Excel file the parameter set will be written to
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

  containerPaths <- vector("character", length(individual$distributedParameters$paths))
  paramNames <- vector("character", length(individual$distributedParameters$paths))
  values <- vector("numeric", length(individual$distributedParameters$paths))
  units <- vector("character", length(individual$distributedParameters$paths))

  for (i in seq_along(individual$distributedParameters$paths)) {
    fullPathParts <- strsplit(individual$distributedParameters$paths[[i]], split = "|", fixed = TRUE)[[1]]
    containerPath <- paste(fullPathParts[1:length(fullPathParts) - 1], collapse = "|")
    paramName <- fullPathParts[[length(fullPathParts)]]

    containerPaths[i] <- containerPath
    paramNames[i] <- paramName
    values[i] <- individual$distributedParameters$values[[i]]
    units[i] <- individual$distributedParameters$units[[i]]
  }

  output <- data.frame(unlist(containerPaths, use.names = FALSE), unlist(paramNames, use.names = FALSE), unlist(as.numeric(values), use.names = FALSE), unlist(units, use.names = FALSE))
  colnames(output) <- columnNames

  writexl::write_xlsx(output, path = outputXLSPath, col_names = TRUE)
}

#' Read individual characteristics from file
#'
#' @details Read individual characteristics from an excel sheet
#' and create an `IndividualCharacteristics`-object. The excel sheet must have
#' the column `IndividualId`, `Species`, `Population`, `Gender`, `Weight [kg]`,
#' `Height [cm]`, `Age [year(s)]`, and `BMI`.

#' @param XLSpath Full path to the excel file
#'
#' @param individualId (String) Id of the individual as stored in the
#'   `IndividualId` column.
#' @param sheet Name of the sheet. If `NULL` (default), the first sheet of the
#'   file is used.
#' @param nullIfNotFound Boolean. If `TRUE` (default), `NULL` is returned if
#' no entry with the give `individualId` exists. Otherwise, an error is thrown.
#'
#' @return An `IndividualCharacteristics` object
#' @import ospsuite
#' @export
readIndividualCharacteristicsFromXLS <- function(XLSpath,
                                                 individualId,
                                                 sheet = NULL,
                                                 nullIfNotFound = TRUE) {
  validateIsString(c(XLSpath, individualId))

  # If no sheet has been specified, read from the first sheet
  if (is.null(sheet)) {
    sheet <- c(1)
  }

  columnNames <- c(
    "IndividualId", "Species", "Population", "Gender", "Weight [kg]",
    "Height [cm]", "Age [year(s)]"
  )


  data <- readExcel(path = XLSpath, sheet = sheet)
  if (!all(names(data) == columnNames)) {
    stop(messages$errorWrongIndividualCharacteristicsXLSStructure(XLSpath, columnNames))
  }
  # Find the row with the given individual id
  rowIdx <- which(data$IndividualId == individualId)
  if (length(rowIdx) == 0) {
    if (nullIfNotFound) {
      return(NULL)
    }
    stop(messages$errorWrongIndividualId(individualId))
  }

  # Create the IndividualCharacteristics object
  individualCharacteristics <- ospsuite::createIndividualCharacteristics(
    species = data$Species[[rowIdx]], population = data$Population[[rowIdx]],
    gender = data$Gender[[rowIdx]],
    weight = data$`Weight [kg]`[[rowIdx]],
    height = data$`Height [cm]`[[rowIdx]],
    age = data$`Age [year(s)]`[[rowIdx]]
  )

  return(individualCharacteristics)
}

#' Apply an individual to the simulation.
#' For human species, only parameters that do not override formulas are applied.
#' For other species, all parameters returned by `createIndividual` are applied.
#'
#' @param individualCharacteristics `IndividualCharacteristics` describing an individual. Optional
#' @param simulation `Simulation` loaded form the PKML file
#' @import ospsuite
#' @export
#'
#' @examples
#' #'
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
