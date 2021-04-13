#' Load a simulation and apply a set of parameters.
#'
#' @description Helper method that combines a set of common steps perfored before running a simulation.
#' This method applies individual parameters data set and additional user-defined parameters to the simulation and runs the simulation to
#' its steady-state and applies the steady-state as new initial conditions.
#'
#' @inheritParams getSteadyState
#' @param simulation \code{Simulation} loaded form the PKML file
#' @param individualCharacteristics \code{IndividualCharacteristics} describing an individual. Optional
#' @param additionalParams A named list with lists 'paths', 'values', and 'units'. Optional
#' @param simulateSteadyState Logical. If \code{TRUE}, the model is simulated for \code{steadyStateTime} minutes after applying parameter values defined in
#' \code{individualCharacteristics} and code{additionalParams}, and the end results of the simulation are applied as initial conditions for alle molecules.
#' State variable parameters are ignored in this version!
#'
#' @return
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
initializeSimulation <- function(simulation, individualCharacteristics = NULL, additionalParams = NULL, simulateSteadyState = FALSE, steadyStateTime = 1000,
                                 ignoreIfFormula = TRUE) {
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
      stop(messages$errorWrongAdditionalParams)
    }
    for (i in seq_along(additionalParams$paths)) {
      param <- getParameter(additionalParams$paths[[i]], container = simulation)
      unit <- additionalParams$units[[i]]
      if (!is.na(unit)) {
        value <- toBaseUnit(quantity = param, values = additionalParams$values[[i]], unit = unit)
      }
      else {
        value <- additionalParams$values[[i]]
      }
      setParameterValues(param, value)
    }
  }

  if (simulateSteadyState) {
    initialValues <- getSteadyState(simulation = simulation, steadyStateTime = steadyStateTime, ignoreIfFormula = ignoreIfFormula)
    for (i in seq_along(initialValues$quantities)) {
      quantity <- initialValues$quantities[[i]]
      quantity$value <- initialValues$values[[i]]
    }
  }
}

#' Compare all parameters of two simulations
#' The method compares all parameters of the two simulations and return a named list with
#' lists \code{In1NotIn2} (paths of parameters present in \code{simulation1} but not in \code{simulation2}),
#' \code{In2NotIn1} (paths of parameters present in \code{simulation2} but not in \code{simulation1}),
#' and \code{Different} a list of paths of parameters that differ between \code{simulation1} and \code{simulation1}.
#' Two parameters are considered different if their formulas or values differ (@seealso{isParametersEqual})
#' @param simulation1 First \code{Simulation} to compare
#' @param simulation2 Second \code{Simulation} to compare
#'
#' @return Named list with entries \code{In1NotIn2}, \code{In2NotIn1}, and \code{Different}, holding the paths that
#' are present in the first but not in the second simulation, present in the second but not in the first simulation, and present in
#' both simulations but with different formulas and/or values, respectively.
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
