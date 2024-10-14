#' Compare two simulations
#'
#' @details
#' The function compares two simulations and returns a list of entities that differ:
#' - `Parameters`: a named list with a list of all `Parameter` entities that are:
#' - in simulation1 but not in simulation 2 (`In1NotIn2`)
#' - in simulation 2 but not in simulation 1 (`I21NotIn1`)
#' - a list `Different` with all parameters which values differ between the simulations.
#' Two parameters are considered different if their formulas or values differ.
#'
#' @seealso isParametersEqual
#'
#' @param simulation1 First `Simulation` to compare
#' @param simulation2 Second `Simulation` to compare
#' @param compareFormulasByValue If `FALSE` (default), parameters are considered not equal
#' if the have the same value but different formulas (e.g., a constant vs. explicit formula).
#' If `TRUE`, only values are compared.
#'
#' @return Named list with following levels:
#' - `Parameters` with named lists `In1NotIn2`, `In2NotIn1`, and `Different`,
#'   holding the `Parameter` objects that are present in the first but not in the second
#'   simulation, present in the second but not in the first simulation, and
#'   present in both simulations but with different formulas and/or values,
#'   respectively.
#' @export
#'
#' @examples
#' \dontrun{
#' humanSim <- loadSimulation(file.path(modelFolder, "DefaultHuman.pkml"))
#' ratSim <- loadSimulation(file.path(modelFolder, "DefaultRat.pkml"))
#' diffParams <- compareSimulationParameters(humanSim, ratSim)
#' }
compareSimulations <- function(simulation1, simulation2, compareFormulasByValue = FALSE) {
  paths1 <- getAllParameterPathsIn(simulation1)
  paths2 <- getAllParameterPathsIn(simulation2)
  commonPaths <- intersect(paths1, paths2)

  # Get parameter that are present in one but not in another simulation
  pathsIn1NotIn2 <- setdiff(paths1, paths2)
  paramsIn1NotIn2 <- getAllParametersMatching(pathsIn1NotIn2, simulation1)
  pathsIn2NotIn1 <- setdiff(paths2, paths1)
  paramsIn2NotIn1 <- getAllParametersMatching(pathsIn2NotIn1, simulation2)

  # For parameters present in both simulations, compare parameters pair wise and
  # store them if they differ
  paramsDiff <- sapply(commonPaths, function(path) {
    param1 <- getParameter(path, simulation1)
    param2 <- getParameter(path, simulation2)

    if (!isParametersEqual(param1, param2, compareFormulasByValue = compareFormulasByValue)) {
      return(list("simulation1" = param1, "simulation2" = param2))
    }
    return(NULL)
  }, USE.NAMES = TRUE)
  # Remove all NULL entries
  paramsDiff[sapply(paramsDiff, is.null)] <- NULL

  return(list(Parameters = list(In1NotIn2 = paramsIn1NotIn2, In2NotIn1 = paramsIn2NotIn1, Different = paramsDiff)))
}

#' Get parameters of applications in the simulation
#'
#' @param simulation A `Simulation` object
#' @param moleculeNames Names of the molecules which applications parameters
#' will be returned. If `NUll`(default), applications for all molecules are
#'  returned.
#'
#' @details Every application event has a `ProtocolSchemaItem` container that
#' holds parameters describing the dose, start time, infusion time etc. This
#' function returns a list of all constant parameters located under the
#' `ProtocolSchemaItem` container of applications defined for the `moleculeNames`.
#'
#' @return A list of `Parameter` objects defining the applications in the
#' simulation.
#' @export
#'
#' @examples
#' simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
#' simulation <- loadSimulation(simPath)
#' applicationParams <- getAllApplicationParameters(simulation = simulation)
#'
#' applicationParams <- getAllApplicationParameters(
#'   simulation = simulation,
#'   moleculeNames = "Aciclovir"
#' )
getAllApplicationParameters <- function(simulation, moleculeNames = NULL) {
  validateIsOfType(simulation, "Simulation")
  validateIsCharacter(moleculeNames, nullAllowed = TRUE)

  # If no molecules have been specified, get application parameters for all
  # molecules in the simulation
  moleculeNames <- moleculeNames %||% simulation$allFloatingMoleculeNames()

  # Returns an object of class `Application` for each administration event
  applications <- unlist(lapply(moleculeNames, \(x) simulation$allApplicationsFor(x)), use.names = FALSE)

  # Gather all parameters in one list that will be the output of the function
  allParams <- list()

  for (application in applications) {
    # get parent container of the application
    parentContainer <- application$startTime$parentContainer
    # Get all non-formula parameters of ProtocolSchemaItem
    params <- getAllParametersMatching("*", parentContainer)

    for (param in params) {
      if (!param$isFormula) {
        allParams <- c(allParams, param)
      }
    }
  }

  return(allParams)
}
