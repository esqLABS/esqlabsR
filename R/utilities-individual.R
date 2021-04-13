#' Create a parameter set describing an individual and write it to the Excel file
#'
#' @param individualCharacteristics An \code{IndividualCharacteristics} object describing the individual. See
#' \code{createIndividualCharacterstics} for more information
#' @param outputXLSPath Path to the Excel file the parameter set will be written to
#' @seealso createIndividualCharacteristics crateIndividual
#'
#' @return
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

#' Apply an individual to the simulation.
#' For human species, only parameters that do not override formulas are applied.
#' For other species, all parameters returned by \code{createIndividual} are applied.
#'
#' @param individualCharacteristics \code{IndividualCharacteristics} describing an individual. Optional
#' @param simulation \code{Simulation} loaded form the PKML file

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
