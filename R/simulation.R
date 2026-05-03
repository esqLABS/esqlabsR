# Apply individual to simulation ----

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

# Initialize simulation ----

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
#' @param stopIfParameterNotFound Logical. If `TRUE` (default), an error is
#'   thrown if any of the `additionalParams` does not exist. If `FALSE`,
#'   non-existent parameters are  ignored.
#' @export
#'
#' @examples
#' \dontrun{
#' simulation <- loadSimulation(filePath = modelPath)
#' humanIndividualCharacteristics <- createIndividualCharacteristics(
#'   species = Species$Human, population = HumanPopulation$European_ICRP_2002,
#'   gender = Gender$Male, weight = 70
#' )
#' userParams <- list(paths = c("path|to|param"), values = c(1.0), units = c("mol/l"))
#' initializeSimulation(simulation, humanIndividualCharacteristics, userParams)
#' simulationResults <- runSimulations(simulation = simulation)
#' }
initializeSimulation <- function(
  simulation,
  individualCharacteristics = NULL,
  additionalParams = NULL,
  stopIfParameterNotFound = TRUE
) {
  validateIsOfType(simulation, "Simulation", nullAllowed = FALSE)
  validateIsOfType(
    individualCharacteristics,
    "IndividualCharacteristics",
    nullAllowed = TRUE
  )
  .validateParametersStructure(
    additionalParams,
    "additionalParams",
    nullAllowed = TRUE
  )

  # Apply parameters of the individual
  if (!is.null(individualCharacteristics)) {
    applyIndividualParameters(individualCharacteristics, simulation)
  }

  # Apply additional parameters (includes species params, user params, etc.)
  if (!is.null(additionalParams)) {
    # Skip if the correct structure is supplied, but no parameters are defined
    if (!isEmpty(additionalParams$paths)) {
      ospsuite::setParameterValuesByPath(
        parameterPaths = additionalParams$paths,
        values = additionalParams$values,
        simulation = simulation,
        units = additionalParams$units,
        stopIfNotFound = stopIfParameterNotFound
      )
    }
  }
}

# Parameter comparison ----

#' @title Check if two parameters are equal with respect to certain properties.
#'
#' @details The parameters are not equal if: The paths of the parameters are not
#' equal; The types of the formulas differ (types checked: isConstant,
#' isDistributed, isExplicit, isTable); Constant formulas have different values;
#' Distributed formulas have different values (not checking for distribution)
#' Explicit formulas: If formula string are not equal, OR one of the parameter
#' values is fixed (formula is overridden), OR both parameter values are fixed
#' and differ, OR checkFormulaValues is TRUE and the values differ (disregarding
#' of overridden or not) Table formulas: If the number of points differ, OR any
#' of the points differ, OR one of the parameter values is fixed (formula is
#' overridden), OR both parameter values are fixed and differ.
#'
#' @param parameter1 First parameter to compare
#' @param parameter2 Second parameter to compare
#' @param checkFormulaValues If TRUE, values of explicit formulas are always
#'   compared. Otherwise, the values are only compared if the formulas are
#'   overridden (isFixedValue == TRUE). FALSE by default.
#' @param compareFormulasByValue If `FALSE`(default), formulas are compared by
#'   their types and string. If `TRUE`, only values are compared.
#'
#' @returns `TRUE` if parameters are considered equal, `FALSE` otherwise
#' @export
isParametersEqual <- function(
  parameter1,
  parameter2,
  checkFormulaValues = FALSE,
  compareFormulasByValue = FALSE
) {
  validateIsOfType(c(parameter1, parameter2), "Parameter")

  # Check for the path
  if (parameter1$path != parameter2$path) {
    return(FALSE)
  }

  formula1 <- parameter1$formula
  formula2 <- parameter2$formula

  # Compare by value
  if (compareFormulasByValue) {
    return(identical(parameter1$value, parameter2$value))
  }

  # Check for formula type equality
  if (
    !all(
      c(
        formula1$isConstant,
        formula1$isDistributed,
        formula1$isExplicit,
        formula1$isTable
      ) ==
        c(
          formula2$isConstant,
          formula2$isDistributed,
          formula2$isExplicit,
          formula2$isTable
        )
    )
  ) {
    return(FALSE)
  }

  # Constant or distributed formula - check for value
  # Comparing using 'identical' to capture NaN and NA cases which can happen
  if (formula1$isConstant || formula1$isDistributed) {
    return(identical(parameter1$value, parameter2$value))
  }

  # Explicit or table formula - check if values are overridden
  if (parameter1$isFixedValue) {
    if (!parameter2$isFixedValue) {
      return(FALSE)
    }
    if (parameter1$value != parameter2$value) {
      return(FALSE)
    }
  }

  # Explicit
  if (formula1$isExplicit) {
    if (
      checkFormulaValues && (!identical(parameter1$value, parameter2$value))
    ) {
      return(FALSE)
    }

    return(formula1$formulaString == formula2$formulaString)
  }

  if (formula1$isTable) {
    return(isTableFormulasEqual(formula1, formula2))
  }

  return(FALSE)
}

#' Check if two table formulas are equal.
#'
#' Table formulas are equal if the number of points is equal and all x-y value
#' pairs are equal between the two formulas
#'
#' @param formula1 First formula to compare
#' @param formula2 Second formula to compare
#'
#' @returns TRUE if the table formulas are equal, FALSE otherwise
#' @export
isTableFormulasEqual <- function(formula1, formula2) {
  allPoints1 <- formula1$allPoints
  allPoints2 <- formula2$allPoints

  if (length(allPoints1) != length(allPoints2)) {
    return(FALSE)
  }

  for (i in seq_along(allPoints1)) {
    point1 <- allPoints1[[i]]
    point2 <- allPoints2[[i]]

    return((point1$x == point2$x) && (point1$y == point2$y))
  }
}

# Compare simulations ----

#' Compare two simulations
#'
#' @details The function compares two simulations and returns a list of entities
#' that differ:
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
#' @param compareFormulasByValue If `FALSE` (default), parameters are considered
#'   not equal if the have the same value but different formulas (e.g., a
#'   constant vs. explicit formula). If `TRUE`, only values are compared.
#'
#' @returns Named list with following levels:
#' - `Parameters` with named lists `In1NotIn2`, `In2NotIn1`, and `Different`,
#'   holding the `Parameter` objects that are present in the first but not in
#'   the second simulation, present in the second but not in the first
#'   simulation, and present in both simulations but with different formulas
#'   and/or values, respectively.
#' @export
#'
#' @examples
#' \dontrun{
#' humanSim <- loadSimulation(file.path(modelFolder, "DefaultHuman.pkml"))
#' ratSim <- loadSimulation(file.path(modelFolder, "DefaultRat.pkml"))
#' diffParams <- compareSimulationParameters(humanSim, ratSim)
#' }
compareSimulations <- function(
  simulation1,
  simulation2,
  compareFormulasByValue = FALSE
) {
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
  paramsDiff <- sapply(
    commonPaths,
    function(path) {
      param1 <- getParameter(path, simulation1)
      param2 <- getParameter(path, simulation2)

      if (
        !isParametersEqual(
          param1,
          param2,
          compareFormulasByValue = compareFormulasByValue
        )
      ) {
        return(list("simulation1" = param1, "simulation2" = param2))
      }
      return(NULL)
    },
    USE.NAMES = TRUE
  )
  # Remove all NULL entries
  paramsDiff[sapply(paramsDiff, is.null)] <- NULL

  return(list(
    Parameters = list(
      In1NotIn2 = paramsIn1NotIn2,
      In2NotIn1 = paramsIn2NotIn1,
      Different = paramsDiff
    )
  ))
}

#' Get parameters of applications in the simulation
#'
#' @param simulation A `Simulation` object
#' @param moleculeNames Names of the molecules which applications parameters
#'   will be returned. If `NUll`(default), applications for all molecules are
#'   returned.
#'
#' @details Every application event has a `ProtocolSchemaItem` container that
#'   holds parameters describing the dose, start time, infusion time etc. This
#'   function returns a list of all constant parameters located under the
#'   `ProtocolSchemaItem` container of applications defined for the
#'   `moleculeNames`.
#'
#' @returns A list of `Parameter` objects defining the applications in the
#'   simulation.
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
  applications <- unlist(
    lapply(moleculeNames, \(x) simulation$allApplicationsFor(x)),
    use.names = FALSE
  )

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
