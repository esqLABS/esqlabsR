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

#' Parse protein ontogenies from a string
#'
#' @param ontogenyString A string in the format
#'   "Molecule1:Ontogeny1,Molecule2:Ontogeny2" or NULL.
#'
#' @returns A list of `MoleculeOntogeny` objects, or NULL.
#' @keywords internal
#' @noRd
.readOntongeniesFromList <- function(ontogenyString) {
  if (is.null(ontogenyString) || is.na(ontogenyString) || ontogenyString == "") {
    return(NULL)
  }
  # The string format is "Molecule1:Ontogeny1,Molecule2:Ontogeny2"
  ontogeniesSplit <- unlist(strsplit(ontogenyString, split = ",", fixed = TRUE))
  ontogeniesSplit <- trimws(ontogeniesSplit)
  moleculeOntogenies <- vector("list", length(ontogeniesSplit))
  for (i in seq_along(ontogeniesSplit)) {
    parts <- unlist(strsplit(ontogeniesSplit[[i]], split = ":", fixed = TRUE))
    if (length(parts) != 2) {
      stop(messages$errorWrongOntogenyStructure(ontogeniesSplit[[i]]))
    }
    protein <- parts[[1]]
    ontogeny <- parts[[2]]
    validateEnumValue(value = ontogeny, enum = ospsuite::StandardOntogeny)
    moleculeOntogenies[[i]] <- ospsuite::MoleculeOntogeny$new(
      molecule = protein,
      ontogeny = ospsuite::StandardOntogeny[[ontogeny]]
    )
  }
  moleculeOntogenies
}
