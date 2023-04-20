#' Get the steady-state values of species and state variable parameters.
#'
#' @details The steady-state is considered to be the last value of the
#'   simulation with sufficiently long simulation time, i.e., where the rates of
#'   the processes do not (significantly) change.
#'
#' @param steadyStateTime Simulation time (minutes). Must be long enough for
#'   system to reach a steady-state. 1000 by default
#' @param quantitiesPaths List of quantity paths (molecules and/or parameters)
#'   for which the steady-state will be simulated. If `NULL` (default), all
#'   molecules and state variable parameters are considered. The same list is
#'   applied for all simulations.
#' @param simulations `Simulation` object or a list of `Simulation` objects
#' @param ignoreIfFormula If `TRUE` (default), species and parameters with
#'   initial values defined by a formula are not included.
#' @param stopIfNotFound Boolean. If `TRUE` (default), an error is thrown when
#'   results for certain species were not generated. This may happen when due to
#'   numerical problems some values cannot be calculated, though the whole
#'   simulation converges. Setting this argument to `FALSE` allows to ignore
#'   such errors. Check the outputs for empty values when using this option.
#' @param lowerThreshold Numerical value (in default unit of the output).
#' Any steady-state values below this value are considered as numerical noise
#' and replaced by 0. If `lowerThreshold` is `NULL`, no cut-off is applied.
#' Default value is 1e-15.
#' @param simulationRunOptions Optional instance of a `SimulationRunOptions`
#'  used during the simulation run.
#'
#' @return A named list, where the names are the IDs of the simulations and the
#'   entries are lists containing `paths` and their `values` at the end of the
#'   simulation.
#' @import ospsuite hash
#' @export
getSteadyState <- function(quantitiesPaths = NULL,
                           simulations,
                           steadyStateTime,
                           ignoreIfFormula = TRUE,
                           stopIfNotFound = TRUE,
                           lowerThreshold = 1e-15,
                           simulationRunOptions = NULL) {
  validateIsOfType(simulations, type = "Simulation")
  validateIsString(quantitiesPaths, nullAllowed = TRUE)
  simulations <- toList(simulations)

  if (steadyStateTime <= 0) {
    stop(messages$steadyStateTimeNotPositive(steadyStateTime))
  }

  # First prepare all simulations by setting their outputs and time intervals
  # If no quantities have been specified, the quantities paths may be different
  # for each simulation and must be stored separately
  simulationState <- .storeSimulationState(simulations)
  quantitiesPathsMap <- hash::hash()
  for (simulation in simulations) {
    simId <- simulation$id
    # Set simulation time to the steady-state value.
    ospsuite::clearOutputIntervals(simulation = simulation)
    simulation$outputSchema$addTimePoints(timePoints = steadyStateTime)
    # If no quantities are explicitly specified, simulate all outputs.
    if (is.null(quantitiesPaths)) {
      quantitiesPathsMap[[simId]] <- ospsuite::getAllStateVariablesPaths(simulation)
    } else {
      quantitiesPathsMap[[simId]] <- quantitiesPaths
    }
    ospsuite::clearOutputs(simulation)
    ospsuite::addOutputs(quantitiesOrPaths = quantitiesPathsMap[[simId]], simulation = simulation)
  }

  # Run simulations concurrently
  simulationResults <- ospsuite::runSimulations(
    simulations = simulations,
    simulationRunOptions = simulationRunOptions
  )

  # Iterate through simulations and get their outputs
  outputMap <- hash::hash()
  for (simulation in simulations) {
    simId <- simulation$id
    simResults <- simulationResults[[simId]]

    allOutputs <- ospsuite::getOutputValues(
      simResults,
      quantitiesOrPaths = quantitiesPathsMap[[simId]],
      stopIfNotFound = stopIfNotFound,
      addMetaData = FALSE
    )

    # Get the end values of all outputs
    endValues <- lapply(quantitiesPathsMap[[simId]], function(path) {
      # Check if the quantity is defined by an explicit formula
      isFormulaExplicit <- ospsuite::isExplicitFormulaByPath(
        path = enc2utf8(path),
        simulation = simulation,
        stopIfNotFound = stopIfNotFound
      )

      if (ignoreIfFormula && isFormulaExplicit) {
        return(NULL)
      }
      value <- tail(allOutputs$data[path][[1]], 1)
      # Skip if value is NA. This happens if stopIfNotFound = FALSE and some
      # species are not calculated
      if (is.na(value)) {
        return(NULL)
      }
      # If the value is below the cut-off threshold, replace it by 0
      if (!is.null(lowerThreshold) && value < lowerThreshold) {
        value <- 0
      }
      return(value)
    })

    # Get the indices for which the outputs have been calculated
    indices <- which(lengths(endValues) != 0)

    # Reset simulation output intervals and output selections
    .restoreSimulationState(simulations, simulationState)
    outputMap[[simId]] <- list(paths = quantitiesPathsMap[[simId]][indices], values = endValues[indices])
  }
  return(outputMap)
}

#' Get the simulation container of the entity
#'
#' @param entity Object of type `Entity`
#' @keywords internal
#'
#' @return The root container that is the parent of the entity.
.getSimulationContainer <- function(entity) {
  ospsuite.utils::validateIsOfType(entity, "Entity")
  if (ospsuite.utils::isOfType(entity, "Container")) {
    if (entity$containerType == "Simulation") {
      return(entity)
    }
  }
  return(.getSimulationContainer(entity$parentContainer))
}

#' Stores current simulation output state
#'
#' @description Stores simulation output intervals, output time points,
#' and output selections in the current state.
#'
#' @param simulations List of `Simulation` objects
#'
#' @return A named list with entries `outputIntervals`, `timePoints`, and
#' `outputSelections`. Every entry is a named list with names being the IDs
#' of the simulations.
#' @keywords internal
.storeSimulationState <- function(simulations) {
  simulations <- c(simulations)
  # Create named vectors for the output intervals, time points, and output
  # selections of the simulations in their initial state. Names are IDs of
  # simulations.
  oldOutputIntervals <-
    oldTimePoints <-
    oldOutputSelections <-
    ids <- vector("list", length(simulations))

  for (idx in seq_along(simulations)) {
    simulation <- simulations[[idx]]
    simId <- simulation$id
    # Have to reset both the output intervals and the time points!
    oldOutputIntervals[[idx]] <- simulation$outputSchema$intervals
    oldTimePoints[[idx]] <- simulation$outputSchema$timePoints
    oldOutputSelections[[idx]] <- simulation$outputSelections$allOutputs
    ids[[idx]] <- simId
  }
  names(oldOutputIntervals) <-
    names(oldTimePoints) <-
    names(oldOutputSelections) <- ids

  return(list(
    outputIntervals = oldOutputIntervals,
    timePoints = oldTimePoints,
    outputSelections = oldOutputSelections
  ))
}


#' Restore simulation output state
#'
#' @inheritParams .storeSimulationState
#' @param simStateList Output of the function `.storeSimulationState`.
#' A named list with entries `outputIntervals`, `timePoints`, and
#' `outputSelections`. Every entry is a named list with names being the IDs of
#' the simulations.
#'
#' @keywords internal
.restoreSimulationState <- function(simulations, simStateList) {
  simulations <- c(simulations)
  for (simulation in simulations) {
    simId <- simulation$id
    # reset the output intervals
    simulation$outputSchema$clear()
    for (outputInterval in simStateList$outputIntervals[[simId]]) {
      ospsuite::addOutputInterval(
        simulation = simulation,
        startTime = outputInterval$startTime$value,
        endTime = outputInterval$endTime$value,
        resolution = outputInterval$resolution$value
      )
    }
    if (length(simStateList$timePoints[[simId]]) > 0) {
      simulation$outputSchema$addTimePoints(simStateList$timePoints[[simId]])
    }
    # Reset output selections
    ospsuite::clearOutputs(simulation)
    for (outputSelection in simStateList$outputSelections[[simId]]) {
      ospsuite::addOutputs(quantitiesOrPaths = outputSelection$path, simulation = simulation)
    }
  }
}


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
#'   simulation. Must be long enough for system to reach a steady-state. 1000 by
#'   default.
#' @param ignoreIfFormula If `TRUE` (default), species and parameters with
#'   initial values defined by a formula are not included in the steady-state
#'   simulation
#' @param stopIfParameterNotFound Logical. If `TRUE` (default), an error is
#'   thrown if any of the `additionalParams` does not exist. If `FALSE`,
#'   non-existent parameters are  ignored.
#' @param simulationRunOptions Optional instance of a `SimulationRunOptions`
#'  used during the simulation run.
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
                                 stopIfParameterNotFound = TRUE,
                                 simulationRunOptions = NULL) {
  validateIsOfType(simulation, "Simulation", nullAllowed = FALSE)
  validateIsOfType(individualCharacteristics, "IndividualCharacteristics", nullAllowed = TRUE)
  validateIsLogical(simulateSteadyState)
  validateIsNumeric(steadyStateTime)

  # Apply parameters of the individual
  if (!is.null(individualCharacteristics)) {
    applyIndividualParameters(individualCharacteristics, simulation)

    # Apply additional parameters when scaling from human to another species,
    # if a parameter set for the selected species is available. Required as
    # the the parameter set generated by `createIndividual` does not contain
    # all required parameters.
    # Find individual-specific model parameters
    indivParamsFilePath <- system.file("extdata", "SpeciesParameters.xlsx", package = "esqlabsR")
    excelSheets <- readxl::excel_sheets(path = indivParamsFilePath)

    if (any(excelSheets == individualCharacteristics$species)) {
      indivParams <- readParametersFromXLS(paramsXLSpath = indivParamsFilePath, sheets = individualCharacteristics$species)
      # NOT extending the `additionalParams` structure, otherwise it could
      # overwrite user defined parameters!
      ospsuite::setParameterValuesByPath(
        parameterPaths = indivParams$paths,
        values = indivParams$values,
        simulation = simulation,
        units = indivParams$units,
        stopIfNotFound = FALSE
      )
    }
  }

  # Apply additional parameters
  if (!is.null(additionalParams)) {
    .validateParametersStructure(
      parameterStructure = additionalParams,
      argumentName = "additionalParams"
    )
    # Skip if the correct structure is supplied, but no parameters are defined
    if (!isEmpty(additionalParams$paths)) {
      ospsuite::setParameterValuesByPath(
        parameterPaths = additionalParams$paths,
        values = additionalParams$values,
        simulation = simulation,
        units = additionalParams$units,
        stopIfNotFound = FALSE
      )
    }
  }

  if (simulateSteadyState) {
    initialValues <- getSteadyState(
      simulations = simulation,
      steadyStateTime = steadyStateTime,
      ignoreIfFormula = ignoreIfFormula,
      simulationRunOptions = simulationRunOptions
    )[[simulation$id]]
    ospsuite::setQuantityValuesByPath(
      quantityPaths = initialValues$paths,
      values = initialValues$values,
      simulation = simulation
    )
  }
}

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
#' @param compareFormulasByValue If `FALSE` (default), parameters are considered not equal if the have the same value but different formulas (e.g., a constant vs. explicit formula). If `TRUE`, only values are compared.
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
  applications <- unlist(lapply(moleculeNames, \(x)simulation$allApplicationsFor(x)), use.names = FALSE)

  # Gather all parameters in one list that will be the output of the function
  allParams <- list()

  for (application in applications) {
    # get parent container of the application
    parentContainer <- application$startTime$parentContainer
    containerPath <- parentContainer$path
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
