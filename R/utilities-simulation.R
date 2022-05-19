#' Load a simulation and apply a set of parameters.
#'
#' @description Helper method that combines a set of common steps performed
#'   before running a simulation. This method applies individual parameters data
#'   set and additional user-defined parameters to the simulation and runs the
#'   simulation to its steady-state and applies the steady-state as new initial
#'   conditions.
#'
#' @param simulation `Simulation` loaded from a PKML file
#' @param individualCharacteristics Optional `IndividualCharacteristics`
#'   describing an individual.
#' @param additionalParams Optional named list with lists 'paths', 'values', and
#'   'units'.
#' @param simulateSteadyState Logical. If `TRUE`, the model is simulated for
#'   `steadyStateTime` minutes after applying parameter values defined in
#'   `individualCharacteristics` and `additionalParams`, and the end results
#'   of the simulation are applied as initial conditions for all molecules.
#'   Default is `FALSE`.
#' @param steadyStateTime Simulation time (minutes) for the steady-state
#'   simulation. Must be long enough for system to reach a steady-state 1000 by
#'   default.
#' @param ignoreIfFormula If `TRUE` (default), species and parameters with
#'   initial values defined by a formula are not included in the steady-state
#'   simulation
#' @param stopIfParameterNotFound Logical. If `TRUE` (default), an error is
#'   thrown if any of the `additionalParams` does not exist. If `FALSE`,
#'   non-existent parameters are  ignored.
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
#' userParams <- readParametersFromXLS(parameterXLSPath)
#' initializeSimulation(simulation, humanIndividualCharacteristics, userParams)
#' simulationResults <- runSimulation(simulation = simulation)
#' }
initializeSimulation <- function(simulation,
                                 individualCharacteristics = NULL,
                                 additionalParams = NULL,
                                 simulateSteadyState = FALSE,
                                 steadyStateTime = 1000,
                                 ignoreIfFormula = TRUE,
                                 stopIfParameterNotFound = TRUE) {
  validateIsOfType(simulation, "Simulation", nullAllowed = FALSE)
  validateIsOfType(individualCharacteristics, "IndividualCharacteristics", nullAllowed = TRUE)
  validateIsLogical(simulateSteadyState)

  # Apply parameters of the individual
  if (!is.null(individualCharacteristics)) {
    applyIndividualParameters(individualCharacteristics, simulation)
  }

  # Apply additional parameters
  if (!is.null(additionalParams)) {
    if (all(names(additionalParams) != c("paths", "values", "units"))) {
      stop(messages$wrongParametersStructure("additionalParams"))
    }

    ospsuite::setParameterValuesByPath(
      parameterPaths = additionalParams$paths,
      values = additionalParams$values,
      simulation = simulation,
      units = additionalParams$units,
      stopIfNotFound = FALSE
    )
  }

  if (simulateSteadyState) {
    initialValues <- getSteadyState(simulations = simulation, steadyStateTime = steadyStateTime, ignoreIfFormula = ignoreIfFormula)[[simulation$id]]
    ospsuite::setQuantityValuesByPath(
      quantityPaths = initialValues$paths,
      values = initialValues$values,
      simulation = simulation
    )
  }
}

#' Compare all parameters of two simulations
#'
#' The method compares all parameters
#' of the two simulations and return a named list with lists `In1NotIn2` (paths
#' of parameters present in `simulation1` but not in `simulation2`), `In2NotIn1`
#' (paths of parameters present in `simulation2` but not in `simulation1`), and
#' `Different` a list of paths of parameters that differ between `simulation1`
#' and `simulation1`.
#' Two parameters are considered different if their formulas or values differ
#'
#' @seealso isParametersEqual
#'
#' @param simulation1 First `Simulation` to compare
#' @param simulation2 Second `Simulation` to compare
#'
#' @return Named list with entries `In1NotIn2`, `In2NotIn1`, and `Different`,
#'   holding the paths that are present in the first but not in the second
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
compareSimulationParameters <- function(simulation1, simulation2) {
  sim1Params <- getAllParametersMatching("**", simulation1)
  sim2Params <- getAllParametersMatching("**", simulation2)

  pathsIn1NotIn2 <- list()
  pathsIn2NotIn1 <- list()
  pathsDiff <- list()

  for (param1 in sim1Params) {
    path <- param1$path

    param2 <- getParameter(path, simulation2, stopIfNotFound = FALSE)
    if (is.null(param2)) {
      pathsIn1NotIn2 <- append(pathsIn1NotIn2, values = path)
      next
    }

    if (!isParametersEqual(param1, param2)) {
      pathsDiff <- append(pathsDiff, path)
    }
  }

  for (param2 in sim2Params) {
    path <- param2$path

    param1 <- getParameter(path, simulation1, stopIfNotFound = FALSE)
    if (is.null(param1)) {
      pathsIn2NotIn1 <- append(pathsIn2NotIn1, values = path)
      next
    }
  }

  return(list(In1NotIn2 = pathsIn1NotIn2, In2NotIn1 = pathsIn2NotIn1, Different = pathsDiff))
}
