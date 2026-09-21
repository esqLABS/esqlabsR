# Apply individual to simulation ----

#' Apply an individual parametrization to a simulation
#'
#' @description Sets the parameter values that describe
#' `individualCharacteristics` in `simulation`.
#'
#' For a human individual, only the parameters that vary between individuals of
#' the same species are set; parameters defined by formulas keep their formulas.
#'
#' For any other species, the simulation is scaled to that species. Two sets of
#' values are applied: the individual parameters returned by
#' `ospsuite::createIndividual()`, including those that replace formulas (organ
#' volumes, blood flow rates, body weight), and the species constants of the
#' PK-Sim database (tissue composition, blood flow, gut geometry and transit,
#' lumen pH, bile salt concentrations), taken from an individual building block
#' that `ospsuite::createIndividualBuildingBlock()` creates for the same
#' characteristics. The values follow the body weight, height, and age given in
#' `individualCharacteristics`.
#'
#' @details Scaling works from a simulation exported for a human individual to
#' every species PK-Sim supports, and between two non-human species. Scaling
#' from a non-human species to human is not supported: a human individual needs
#' age- and height-dependent parameters that are not part of an animal model.
#' The function therefore stops with an error when a human individual is
#' applied to a simulation built for another species. The species is read from
#' the individual stored in the simulation (PK-Sim exports since OSP version
#' 12); an older export counts as human when it has the parameters
#' `Organism|Height` and `Organism|Age`.
#'
#' @param individualCharacteristics `IndividualCharacteristics` describing an
#'   individual, as returned by `ospsuite::createIndividualCharacteristics()`.
#' @param simulation `Simulation` loaded from the PKML file
#' @returns `simulation`, invisibly, with the parameter values applied.
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
#'
#' # Scale a human model to a rat of 250 g
#' ratIndividualCharacteristics <- createIndividualCharacteristics(
#'   species = Species$Rat, weight = 0.25
#' )
#' applyIndividualParameters(ratIndividualCharacteristics, simulation)
#' }
applyIndividualParameters <- function(individualCharacteristics, simulation) {
  if (individualCharacteristics$species == ospsuite::Species$Human) {
    .stopIfNotHumanModel(simulation)
  }
  parameters <- .individualParameterSet(individualCharacteristics, simulation)

  ospsuite::setParameterValuesByPath(
    parameterPaths = parameters$paths,
    values = parameters$values,
    simulation = simulation,
    units = parameters$units,
    stopIfNotFound = FALSE
  )

  invisible(simulation)
}

# Parameters that describe `individualCharacteristics`, as `paths`, `values`,
# and `units`. For a human individual these are the distributed parameters of
# `ospsuite::createIndividual()` only, so that parameters defined by formulas
# keep their formulas. For any other species the derived parameters follow,
# which replace formulas with the values of the species (organ volumes, blood
# flow rates, body weight), and then the species constants of the individual
# building block, see `.speciesParametersFromBuildingBlock()`, which
# `createIndividual()` never returns. `simulation`, when given, restricts the
# species constants to the parameters the model has. Each path appears once;
# the derived parameters and the building block agree on the paths they share.
# @keywords internal
# @noRd
.individualParameterSet <- function(
  individualCharacteristics,
  simulation = NULL
) {
  individual <- ospsuite::createIndividual(individualCharacteristics)
  paths <- individual$distributedParameters$paths
  values <- individual$distributedParameters$values
  units <- individual$distributedParameters$units

  if (individualCharacteristics$species != ospsuite::Species$Human) {
    speciesParameters <- .speciesParametersFromBuildingBlock(
      individualCharacteristics,
      simulation
    )
    paths <- c(
      paths,
      individual$derivedParameters$paths,
      speciesParameters$paths
    )
    values <- c(
      values,
      individual$derivedParameters$values,
      speciesParameters$values
    )
    units <- c(
      units,
      individual$derivedParameters$units,
      speciesParameters$units
    )
  }

  keep <- !duplicated(paths, fromLast = TRUE)
  list(paths = paths[keep], values = values[keep], units = units[keep])
}

# Species constants for a non-human individual: every parameter with a value in
# the individual building block PK-Sim creates for `individualCharacteristics`,
# restricted to the parameters that exist in `simulation` when one is given.
# Entries the building block defines by a formula (wall thickness and wall
# volume of the lumen segments) come back without a value and are left out.
# @keywords internal
# @noRd
.speciesParametersFromBuildingBlock <- function(
  individualCharacteristics,
  simulation = NULL
) {
  gestationalAge <- .snapshotParameterValue(
    individualCharacteristics$gestationalAge
  )
  # If no gestational age is supplied, use the default value.
  if (is.null(gestationalAge)) {
    gestationalAge <- 40
  }
  buildingBlock <- ospsuite::createIndividualBuildingBlock(
    species = individualCharacteristics$species,
    population = individualCharacteristics$population,
    gender = individualCharacteristics$gender,
    weight = .snapshotParameterValue(individualCharacteristics$weight),
    weightUnit = .snapshotParameterUnit(individualCharacteristics$weight, "kg"),
    height = .snapshotParameterValue(individualCharacteristics$height),
    heightUnit = .snapshotParameterUnit(individualCharacteristics$height, "cm"),
    age = .snapshotParameterValue(individualCharacteristics$age),
    ageUnit = .snapshotParameterUnit(individualCharacteristics$age, "year(s)"),
    gestationalAge = gestationalAge,
    gestationalAgeUnit = .snapshotParameterUnit(
      individualCharacteristics$gestationalAge,
      "week(s)"
    ),
    seed = individualCharacteristics$seed
  )
  parameters <- ospsuite::individualsBBToDataFrame(buildingBlock)
  paths <- paste(
    parameters[["Container Path"]],
    parameters[["Parameter Name"]],
    sep = "|"
  )
  # Keep only parameters that are defined by a constant. Parameters that are
  # defined by formulas have the `NaN` value
  keep <- is.finite(parameters[["Value"]])
  if (!is.null(simulation)) {
    keep <- keep & paths %in% ospsuite::getAllParameterPathsIn(simulation)
  }

  list(
    paths = paths[keep],
    values = parameters[["Value"]][keep],
    units = parameters[["Unit"]][keep]
  )
}

# Value of an ospsuite `SnapshotParameter`, `NULL` when the characteristic is
# not set.
# @keywords internal
# @noRd
.snapshotParameterValue <- function(parameter) {
  if (is.null(parameter)) {
    return(NULL)
  }
  value <- parameter$value
  if (is.null(value) || length(value) == 0 || is.na(value)) {
    return(NULL)
  }
  value
}

# Unit of an ospsuite `SnapshotParameter`, `default` when the characteristic or
# its unit is not set.
# @keywords internal
# @noRd
.snapshotParameterUnit <- function(parameter, default) {
  if (is.null(parameter)) {
    return(default)
  }
  unit <- parameter$unit
  if (is.null(unit) || length(unit) == 0 || is.na(unit) || !nzchar(unit)) {
    return(default)
  }
  unit
}

# A human individual only fits a simulation built for a human. Stops when
# `simulation` was built for another species; does nothing when the species
# cannot be told.
# @keywords internal
# @noRd
.stopIfNotHumanModel <- function(simulation) {
  species <- .simulationSpecies(simulation)
  if (is.null(species) || isTRUE(species == ospsuite::Species$Human)) {
    return(invisible(NULL))
  }
  simulationName <- simulation$name
  cli::cli_abort(
    messages$humanIndividualForNonHumanModel(simulationName, species)
  )
}

# Species of the individual `simulation` was built for. PK-Sim exports since
# OSP version 12 store their individual building block, whose species counts.
# For an older export the parameters decide: every human model has
# `Organism|Height` and `Organism|Age`, no animal model has them. `NULL` when
# the model has no `Organism|Weight`, that is, no PK-Sim organism at all.
# `NA` for a non-human model whose species is not stored.
# @keywords internal
# @noRd
.simulationSpecies <- function(simulation) {
  species <- .storedIndividualSpecies(simulation)
  if (!is.null(species)) {
    return(species)
  }
  hasParameter <- function(path) {
    !is.null(ospsuite::getParameter(path, simulation, stopIfNotFound = FALSE))
  }
  if (!hasParameter("Organism|Weight")) {
    return(NULL)
  }
  if (hasParameter("Organism|Height") && hasParameter("Organism|Age")) {
    return(ospsuite::Species$Human)
  }
  NA_character_
}

# Species of the individual building block stored in `simulation`, `NULL` when
# the simulation stores none: a MoBi model, or an export older than OSP
# version 12, for which ospsuite reports the missing configuration as an error.
# @keywords internal
# @noRd
.storedIndividualSpecies <- function(simulation) {
  individual <- tryCatch(
    simulation$configuration$individual,
    error = function(e) NULL
  )
  species <- individual$species
  if (
    is.null(species) ||
      length(species) == 0 ||
      is.na(species) ||
      !nzchar(species)
  ) {
    return(NULL)
  }
  species
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
#'   describing an individual. For a non-human species, the simulation is scaled
#'   to that species, see [applyIndividualParameters()].
#' @param additionalParams Optional named list with lists 'paths', 'values', and
#'   'units'.
#' @param additionalInitialConditions Optional named list with lists 'paths',
#'   'values', and 'units', giving molecule start values to apply via
#'   `ospsuite::setQuantityValuesByPath()` after the parameters.
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
#' userParams <- readParametersFromXLS(parameterXLSPath)
#' initializeSimulation(simulation, humanIndividualCharacteristics, userParams)
#' simulationResults <- runSimulations(simulation = simulation)
#' }
initializeSimulation <- function(
  simulation,
  individualCharacteristics = NULL,
  additionalParams = NULL,
  additionalInitialConditions = NULL,
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
  .validateParametersStructure(
    additionalInitialConditions,
    "additionalInitialConditions",
    nullAllowed = TRUE
  )

  # Apply parameters of the individual
  if (!is.null(individualCharacteristics)) {
    applyIndividualParameters(individualCharacteristics, simulation)
  }

  # Apply additional parameters
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

  # Apply additional initial conditions (molecule start values), after the
  # parameters so a start value overrides any parameter-driven default.
  if (!is.null(additionalInitialConditions)) {
    # Skip if the correct structure is supplied, but no entries are defined
    if (!isEmpty(additionalInitialConditions$paths)) {
      ospsuite::setQuantityValuesByPath(
        quantityPaths = additionalInitialConditions$paths,
        values = additionalInitialConditions$values,
        simulation = simulation,
        units = additionalInitialConditions$units,
        stopIfNotFound = stopIfParameterNotFound
      )
    }
  }
}

#' Compare two simulations
#'
#' @details The function compares two simulations and returns a list of entities
#' that differ:
#' - `Parameters`: a named list with a list of all `Parameter` entities that are:
#' - in simulation1 but not in simulation 2 (`In1NotIn2`)
#' - in simulation 2 but not in simulation 1 (`In2NotIn1`)
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
#' diffParams <- compareSimulations(humanSim, ratSim)
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
  paramsDiff <- lapply(
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
    }
  )
  names(paramsDiff) <- commonPaths
  # Remove all NULL entries
  paramsDiff[vapply(paramsDiff, is.null, logical(1))] <- NULL

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
