#' Get the steady-state values of species and state variable parameters.
#'
#' @details The steady-state is considered to be the last value of the
#'   simulation with sufficiently long simulation time, i.e., where the rates of
#'   the processes do not (significantly) change.
#'
#' @param steadyStateTime Simulation time (minutes). Must be long enough for
#'   system to reach a steady-state. 1000 by default
#' @param quantitiesPaths List of quantity paths (molecules and/or parameters)
#'   for which the steady-state is to be simulated. If `NULL` (default), all
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
#' @param lowerThreshold Numerical value (in µmol). Any steady-state values
#'   below this value are considered as numerical noise and replaced by 0. If
#'   `lowerThreshold` is `NULL`, no cut-off is applied. Default value is 1e-15.
#'
#' @return A named list, where the names are the IDs of the simulations and the
#'   entries are lists containing `paths` and their `values` at the end of the
#'   simulation.
#' @import ospsuite rClr hash
#' @export
getSteadyState <- function(quantitiesPaths = NULL,
                           simulations,
                           steadyStateTime,
                           ignoreIfFormula = TRUE,
                           stopIfNotFound = TRUE,
                           lowerThreshold = 1e-15) {
  validateIsOfType(simulations, type = "Simulation")
  validateIsString(object = quantitiesPaths, nullAllowed = TRUE)
  simulations <- toList(simulations)

  if (steadyStateTime <= 0) {
    stop(messages$steadyStateTimeNotPositive(steadyStateTime))
  }

  # First prepare all simulations by setting their outputs and time intervals
  # Create hash maps for the output intervals, time points, and output selections of the simulations in their initial state.
  oldOutputIntervals <- hash::hash()
  oldTimePoints <- hash::hash()
  oldOutputSelections <- hash::hash()
  # If no quantities have been specified, the quantities paths may be different for each simulation and must be stored separately
  quantitiesPathsMap <- hash::hash()
  for (simulation in simulations) {
    simId <- simulation$id
    # Have to reset both the output intervals and the time points!
    oldOutputIntervals[[simId]] <- simulation$outputSchema$intervals
    oldTimePoints[[simId]] <- simulation$outputSchema$timePoints
    oldOutputSelections[[simId]] <- simulation$outputSelections$allOutputs
    # Set simulation time to the steady-state value.
    ospsuite::setOutputInterval(simulation = simulation, startTime = 0, endTime = steadyStateTime, resolution = 1 / steadyStateTime)
    # If no quantities are explicitly specified, simulate all outputs.
    if (is.null(quantitiesPaths)) {
      quantitiesPathsMap[[simId]] <- ospsuite::getAllStateVariablesPaths(simulation)
    } else {
      quantitiesPathsMap[[simId]] <- quantitiesPaths
    }
    ospsuite::addOutputs(quantitiesOrPaths = quantitiesPathsMap[[simId]], simulation = simulation)
  }

  # Run simulations concurrently
  simulationResults <- ospsuite::runSimulations(simulations = simulations)
  # Container task is required for checking the "isFormula" property
  task <- ospsuite:::getContainerTask()

  # Iterate through simulations and get their outputs
  outputMap <- hash::hash()
  for (simulation in simulations) {
    simId <- simulation$id
    # Have to distinguish between a list of simulation results for multiple simulations,
    # or a single simulation results
    simResults <- simulationResults[[simId]] %||% simulationResults

    allOutputs <- ospsuite::getOutputValues(
      simResults,
      quantitiesOrPaths = quantitiesPathsMap[[simId]],
      stopIfNotFound = stopIfNotFound,
      addMetaData = FALSE
    )


    # Get the end values of all outputs
    endValues <- lapply(quantitiesPathsMap[[simId]], function(path) {
      # Check if the quantity is defined by an explicit formula
      isFormulaExplicit <- rClr::clrCall(task, "IsExplicitFormulaByPath", simulation$ref, enc2utf8(path))

      if (ignoreIfFormula && isFormulaExplicit) {
        return(NULL)
      }
      value <- tail(allOutputs$data[path][[1]], 1)
      # Skip if value is NA. This happens if stopIfNotFound = FALSE and some species are not calculated
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

    # reset the output intervals
    simulation$outputSchema$clear()
    for (outputInterval in oldOutputIntervals[[simId]]) {
      ospsuite::addOutputInterval(
        simulation = simulation, startTime = outputInterval$startTime$value,
        endTime = outputInterval$endTime$value,
        resolution = outputInterval$resolution$value
      )
    }
    if (length(oldTimePoints[[simId]]) > 0) {
      simulation$outputSchema$addTimePoints(oldTimePoints[[simId]])
    }
    # Reset output selections
    ospsuite::clearOutputs(simulation)
    for (outputSelection in oldOutputSelections[[simId]]) {
      ospsuite::addOutputs(quantitiesOrPaths = outputSelection$path, simulation = simulation)
    }
    outputMap[[simId]] <- list(paths = quantitiesPathsMap[[simId]][indices], values = endValues[indices])
  }
  return(outputMap)
}

#' Export steady-state to excel
#'
#' @details Simulates a given model to its steady-state and creates an
#' Excel-file with the end values of molecules amounts in all containers and
#' parameter values that have a right-hand-side (state variable parameters).
#' @param simulation `Simulation` object
#' @param quantitiesPaths List of quantity paths (molecules and/or parameters)
#'   for which the steady-state is to be simulated. If `NULL` (default), all
#'   molecules and state variable parameters are considered.
#' @param resultsXLSPath Path to the xls-file where the results will be written
#'   to. If the file does not exist, a new file is created. If no path is
#'   provided, the file will be created in the same directory where the model
#'   file is located.
#' @inheritParams getSteadyState
#' @export
exportSteadyStateToXLS <- function(simulation,
                                   quantitiesPaths = NULL,
                                   resultsXLSPath = "",
                                   steadyStateTime = 1000,
                                   ignoreIfFormula = TRUE,
                                   stopIfNotFound = TRUE,
                                   lowerThreshold = 1e-15) {
  validateIsOfType(simulation, type = "Simulation")
  # If no explicit path to the results-file is provided, store the results file in the same folder as the model file.
  if (resultsXLSPath == "") {
    simulationPath <- tools::file_path_sans_ext(simulation$sourceFile)
    resultsXLSPath <- paste0(simulationPath, "_SS.xlsx")
  }
  # If the provided path to the output file targets a non-existent directory, try to create the directory
  else {
    resultsDir <- dirname(resultsXLSPath)
    if (!file.exists(resultsDir)) {
      dir.create(resultsDir, recursive = TRUE)
    }
  }

  initialValues <- getSteadyState(
    simulations = simulation,
    quantitiesPaths = quantitiesPaths,
    steadyStateTime = steadyStateTime,
    ignoreIfFormula = ignoreIfFormula,
    stopIfNotFound = stopIfNotFound,
    lowerThreshold = lowerThreshold
  )[[simulation$id]]

  nrOfEntries <- length(initialValues$paths)

  # For each simulated species, the output contains the path, species name, the
  # "isPresetn"-flag, the value, the unit, the scale divisor value, and the
  # "negative values allowed"-flag.
  moleculeContainerPath <- c()
  moleculeName <- c()
  moleculeIsPresent <- c()
  moleculeValue <- c()
  moleculeUnits <- c()
  moleculeScaleDivisor <- c()
  moleculeNegValsAllowed <- c()

  # Initial values of state variable parameters
  parameterContainerPath <- c()
  parameterName <- c()
  parameterValue <- c()
  parameterUnits <- c()

  # Iterate through all quantities
  for (i in 1:nrOfEntries) {
    quantity <- ospsuite::getMolecule(
      path = initialValues$paths[[i]],
      container = simulation,
      stopIfNotFound = FALSE
    ) %||%
      ospsuite::getParameter(
        path = initialValues$paths[[i]],
        container = simulation,
        stopIfNotFound = FALSE
      )

    value <- initialValues$values[[i]]

    if (isOfType(quantity, "Molecule")) {
      moleculeValue <- append(moleculeValue, value)
      moleculeContainerPath <- append(moleculeContainerPath, quantity$parentContainer$path)
      moleculeName <- append(moleculeName, quantity$name)
      moleculeIsPresent <- append(moleculeIsPresent, TRUE)
      moleculeUnits <- append(moleculeUnits, quantity$unit)
      moleculeScaleDivisor <- append(moleculeScaleDivisor, quantity$scaleDivisor)
      moleculeNegValsAllowed <- append(moleculeNegValsAllowed, "")
    } else {
      parameterValue <- append(parameterValue, value)
      parameterContainerPath <- append(parameterContainerPath, quantity$parentContainer$path)
      parameterName <- append(parameterName, quantity$name)
      parameterUnits <- append(parameterUnits, quantity$unit)
    }
  }

  speciesInitVals <- data.frame(
    unlist(moleculeContainerPath, use.names = FALSE), unlist(moleculeName, use.names = FALSE), unlist(moleculeIsPresent, use.names = FALSE), unlist(moleculeValue, use.names = FALSE),
    unlist(moleculeUnits, use.names = FALSE), unlist(moleculeScaleDivisor, use.names = FALSE), unlist(moleculeNegValsAllowed, use.names = FALSE)
  )

  if (length(speciesInitVals) > 0) {
    colnames(speciesInitVals) <- c("Container Path", "Molecule Name", "Is Present", "Value", "Units", "Scale Divisor", "Neg. Values Allowed")
  }

  parameterInitVals <- data.frame(
    unlist(parameterContainerPath, use.names = FALSE), unlist(parameterName, use.names = FALSE), unlist(parameterValue, use.names = FALSE), unlist(parameterUnits, use.names = FALSE)
  )

  if (length(parameterInitVals) > 0) {
    colnames(parameterInitVals) <- c("Container Path", "Parameter Name", "Value", "Units")
  }
  # Write the results into an excel file.
  writexl::write_xlsx(list("Molecules" = speciesInitVals, "Parameters" = parameterInitVals), path = resultsXLSPath, col_names = TRUE)
}
