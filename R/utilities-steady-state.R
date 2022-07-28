#' Export steady-state to excel
#'
#' @details Simulates a given model to its steady-state and creates an
#' Excel-file with the end values of molecules amounts in all containers and
#' parameter values that have a right-hand-side (state variable parameters).
#' @param resultsXLSPath Path to the xls-file where the results will be written
#'   to. If the file does not exist, a new file is created. If no path is
#'   provided, the file will be created in the same directory where the model
#'   file is located. The name of the file will be `<SimulationFileName>_SS`.
#' @param simulation A `Simulation` object that will be updated with the steady
#' state
#' @inheritParams ospsuite.parameteridentification::getSteadyState
#' @import ospsuite.utils ospsuite.parameteridentification
#' @export
exportSteadyStateToXLS <- function(simulation,
                                   quantitiesPaths = NULL,
                                   resultsXLSPath = "",
                                   steadyStateTime = 1000,
                                   ignoreIfFormula = TRUE,
                                   stopIfNotFound = TRUE,
                                   lowerThreshold = 1e-15,
                                   simulationRunOptions = NULL) {
  # If no explicit path to the results-file is provided, store the results file
  # in the same folder as the model file.
  if (resultsXLSPath == "") {
    simulationPath <- tools::file_path_sans_ext(simulation$sourceFile)
    resultsXLSPath <- paste0(simulationPath, "_SS.xlsx")
  }
  # If the provided path to the output file targets a non-existent directory,
  # try to create the directory
  resultsDir <- dirname(resultsXLSPath)
  if (!file.exists(resultsDir)) {
    dir.create(resultsDir, recursive = TRUE)
  }

  initialValues <- getSteadyState(
    simulations = simulation,
    quantitiesPaths = quantitiesPaths,
    steadyStateTime = steadyStateTime,
    ignoreIfFormula = ignoreIfFormula,
    stopIfNotFound = stopIfNotFound,
    lowerThreshold = lowerThreshold,
    simulationRunOptions = simulationRunOptions
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
    unlist(moleculeContainerPath, use.names = FALSE),
    unlist(moleculeName, use.names = FALSE),
    unlist(moleculeIsPresent, use.names = FALSE),
    unlist(moleculeValue, use.names = FALSE),
    unlist(moleculeUnits, use.names = FALSE),
    unlist(moleculeScaleDivisor, use.names = FALSE),
    unlist(moleculeNegValsAllowed, use.names = FALSE)
  )

  if (length(speciesInitVals) > 0) {
    colnames(speciesInitVals) <- c("Container Path", "Molecule Name", "Is Present", "Value", "Units", "Scale Divisor", "Neg. Values Allowed")
  }

  parameterInitVals <- data.frame(
    unlist(parameterContainerPath, use.names = FALSE),
    unlist(parameterName, use.names = FALSE),
    unlist(parameterValue, use.names = FALSE),
    unlist(parameterUnits, use.names = FALSE)
  )

  if (length(parameterInitVals) > 0) {
    colnames(parameterInitVals) <- c("Container Path", "Parameter Name", "Value", "Units")
  }
  # Write the results into an excel file.
  writexl::write_xlsx(
    list("Molecules" = speciesInitVals, "Parameters" = parameterInitVals),
    path = resultsXLSPath,
    col_names = TRUE
  )
}
