#' Create a parameter set describing an individual and write it to the Excel file
#'
#' @param individualCharacteristics An \code{IndividualCharacteristics} object describing the individual. See
#' \code{createIndividualCharacterstics} for more information
#' @param outputXLSPath Path to the Excel file the parameter set will be written to
#' @seealso createIndividualCharacteristics crateIndividual
#' @export
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
writeIndividualToXLS <- function(individualCharacteristics, outputXLSPath) {
  validateIsOfType(individualCharacteristics, "IndividualCharacteristics")
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

  openxlsx::write.xlsx(output, file = outputXLSPath, colNames = TRUE)
}

#' Read individual characteristics from file
#'
#' @details Read individual characteristics from an excel sheet
#' and create an \code{IndividualCharacteristics}-object. The excel sheet must have
#' the colums `IndividualId`, `Species`, `Population`, `Gender`, `Weight [kg]`,
#' `Height [cm]`, `Age [year(s)]`, and `BMI`.

#' @param XLSpath Full path to the excel file
#' @param individualId (String) Id of the individual as stored in the `IndividualId` column.
#' @param sheet Name of the sheet. If \code{NULL} (default), the first sheet of the
#' file is used.
#'
#' @return An `IndividualCharacteristics` object
#' @export
readIndividualCharacteristicsFromXLS <- function(XLSpath, individualId, sheet = NULL){
  validateIsString(XLSpath, individualId)

  #If no sheet has been specified, read from the first sheet
  if (is.null(sheet)) {
    sheet <- c(1)
  }

  columnNames <- c("IndividualId", "Species", "Population", "Gender", "Weight.[kg]",
                   "Height.[cm]", "Age.[year(s)]")

  data <- openxlsx::read.xlsx(xlsxFile = XLSpath, sheet = sheet, check.names = FALSE)
  if (!all(names(data) == columnNames)) {
    stop(messages$errorWrongIndividualCharacteristicsXLSStructure(XLSpath, columnNames))
  }
  # Find the row with the given individual id
  rowIdx <- which(data$IndividualId == individualId)
  if (length(rowIdx) == 0) {
    stop(messages$errorWrongIndividualId(individualId))
  }

  #Create the IndividualCharacteristics object
  individualCharacteristics <- createIndividualCharacteristics(species = data$Species, population = data$Population, gender = data$Gender, weight = data$`Weight.[kg]`,
                                                               height = data$`Height.[cm]`,
                                                               age = data$`Age.[year(s)]`)

  return(individualCharacteristics)
}

#' Apply an individual to the simulation.
#' For human species, only parameters that do not override formulas are applied.
#' For other species, all parameters returned by \code{createIndividual} are applied.
#'
#' @param individualCharacteristics \code{IndividualCharacteristics} describing an individual. Optional
#' @param simulation \code{Simulation} loaded form the PKML file
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
  validateIsOfType(individualCharacteristics, "IndividualCharacteristics")
  validateIsOfType(simulation, "Simulation")

  individual <- createIndividual(individualCharacteristics)
  allParamPaths <- c(individual$distributedParameters$paths, individual$derivedParameters$paths)
  allParamValues <- c(individual$distributedParameters$values, individual$derivedParameters$values)

  condition <- function(p) {
    TRUE
  }
  # For human species, only set parameters that do not override a formula
  if (individualCharacteristics$species == Species$Human) {
    condition <- function(p) {
      !p$isFormula
    }
  }

  setParameterValuesByPathWithCondition(
    parameterPaths = allParamPaths, values = allParamValues, simulation = simulation,
    condition = condition
  )
}
