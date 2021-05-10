#' Get all state variable quantities of the simulation
#'
#' @param simulation \code{Simulation} object
#' @param ignoreIfFormula If "TRUE", species and parameters with initial values defined by a formula are not included. Default is "TRUE".
#' @details Returned are instances of all molecules in all compartments and all parameters that are
#' state variables.
#'
#' @return A list of objects of type \code{Quantity}
#' @import ospsuite
#' @export
getAllStateVariables <- function(simulation, ignoreIfFormula = TRUE) {
  ospsuite:::validateIsOfType(simulation, type = "Simulation")

  quantities <- ospsuite::getAllMoleculesMatching("Organism|**", container = simulation)
  # Get all state variable parameters
  allParams <- ospsuite::getAllParametersMatching("**", container = simulation)
  rhsParams <- list()

  rhsParams <- lapply(allParams, function(param) {
    if (param$isStateVariable) {
      if (ignoreIfFormula && param$isFormula) {
        return()
      }
      return(param)
    }
    return()
  })
  quantities <- append(quantities, unlist(rhsParams, use.names = FALSE))
  return(quantities)
}

#' Get the steady-state values of species and state variable parameters.
#'
#' @details The steady-state is considered to be the last value of the simulation with sufficiently
#' long simulation time, i.e., where the rates of the processes do not (significantly) change.
#'
#' @param steadyStateTime Simulation time (minutes). Must be long enough for system to reach a steady-state. 1000 by default
#' @param quantities List of quantities (molecules and/or parameters) for which the steady-state is to be simulated. If \code{NULL} (default),
#' all molecules and state variable parameters are considered.
#' @param simulation Simulation object
#' @param ignoreIfFormula If "TRUE", species and parameters with initial values defined by a formula are not included. Default is "TRUE".
#' @param stopIfNotFound Boolean. If \code{TRUE} (default), an error is thrown when results for certain species were not generated.
#' This may happen when due to numerical problems some values cannot be calculated, though the whole simulation converges. Setting this argument to \code{FALSE} allows
#' to ignore such errors. Check the outputs for empty values when using this option.
#' @param lowerThreshold Numerical value. Any steady-state values below this value are considered as numerical noise and replaced by 0. If \code{lowerThreshold} is \code{NULL},
#' no cut-off is applied. Default value is 1e-15.
#'
#' @return A list containing \code{quantities} and their \code{values} at the end of the simulation.
#' @import ospsuite
#' @export
getSteadyState <- function(quantities = NULL, simulation, steadyStateTime, ignoreIfFormula = TRUE, stopIfNotFound = TRUE, lowerThreshold = 1e-15) {
  ospsuite:::validateIsOfType(simulation, type = "Simulation")
  ospsuite:::validateIsOfType(object = quantities, type = "Quantity", nullAllowed = TRUE)

  if (steadyStateTime <= 0) {
    stop("steadyStateTime must be > 0!")
  }

  # Have to reset both the output intervals and the time points!
  oldOutputIntervals <- simulation$outputSchema$intervals
  oldTimePoints <- simulation$outputSchema$timePoints
  # Set simulation time to the steady-state value.
  ospsuite::setOutputInterval(simulation = simulation, startTime = 0, endTime = steadyStateTime, resolution = 1 / steadyStateTime)
  # If no quantities are explicitly specified, simulate all outputs.
  if (is.null(quantities)) {
    quantities <- getAllStateVariables(simulation, ignoreIfFormula)
  }
  ospsuite::addOutputs(quantities, simulation)

  simulationResults <- ospsuite::runSimulation(simulation)
  allOutputs <- ospsuite::getOutputValues(simulationResults, quantitiesOrPaths = quantities, stopIfNotFound = stopIfNotFound)

  endValues <- lapply(quantities, function(quantity) {
    if (ignoreIfFormula && quantity$isFormula) {
      return(NULL)
    }
    path <- quantity$path
    value <- tail(allOutputs$data[path][[1]], 1)
    # Skip if value is NA. This happens if stopIfNotFound = FALSE and some species are not calculated
    if (is.na(value)) {
      return(NULL)
    }
    # If the value is below the cut-off threshold, replace it by 0
    # if (!is.null(lowerThreshold) && value < lowerThreshold){
    if (!is.null(lowerThreshold) && value < lowerThreshold) {
      value <- 0
    }
    return(value)
  })

  # Get the indices for which the outputs have been calculated
  indices <- which(lengths(endValues) != 0)

  # reset the output intervals
  simulation$outputSchema$clear()
  for (outputInterval in oldOutputIntervals) {
    ospsuite::addOutputInterval(
      simulation = simulation, startTime = outputInterval$startTime$value,
      endTime = outputInterval$endTime$value,
      resolution = outputInterval$resolution$value
    )
  }
  if (length(oldTimePoints) > 0) {
    simulation$outputSchema$addTimePoints(oldTimePoints)
  }

  return(list(quantities = quantities[indices], values = endValues[indices]))
}


#' Write an Excel-file with the steady-state
#'
#' @details Simulates a given model to its steady-state and creates an Excel-file with the end values of molecules amounts in all containers
#' and parameter values that have a right-hand-side (state variable parameters).
#'
#' @inheritParams getSteadyState
#' @param resultsXLSPath Path to the xls-file where the results will be written to. If the file does not exist, a new one is created
#' If no path is provided, the file will be created in the same directory where the model file is located
#' @export
exportSteadyStateToXLS <- function(simulation, quantities = NULL, resultsXLSPath = "", steadyStateTime = 1000, ignoreIfFormula = TRUE,
                                   stopIfNotFound = TRUE, lowerThreshold = 1e-15) {
  ospsuite:::validateIsOfType(simulation, type = "Simulation")
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
    simulation = simulation, steadyStateTime = steadyStateTime, ignoreIfFormula = ignoreIfFormula, stopIfNotFound = stopIfNotFound,
    lowerThreshold = lowerThreshold
  )

  nrOfEntries <- length(initialValues$quantities)

  # For each simulated species, the output contains the path, species name, the "isPresetn"-flag, the value, the unit, the scale divisor value, and the "negative values allowed"-flag.
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
    quantity <- initialValues$quantities[[i]]
    value <- initialValues$values[[i]]

    if (ospsuite:::isOfType(quantity, "Molecule")) {
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
  openxlsx::write.xlsx(list("Molecules" = speciesInitVals, "Parameters" = parameterInitVals), resultsXLSPath, colNames = T)
}
