#' Create a parameter set describing an individual and write it to the Excel
#' file
#'
#' @param individualCharacteristics An `IndividualCharacteristics` object
#'   describing the individual. See `createIndividualCharacterstics` for more
#'   information.
#' @param outputXLSPath Path to the Excel file the parameter set will be written
#'   to
#'
#' @return Path to the created Excel file
#'
#' @seealso createIndividualCharacteristics crateIndividual
#'
#' @examples
#' \dontrun{
#' simulation <- loadSimulation(pathToPKML)
#' humanIndividualCharacteristics <- createIndividualCharacteristics(
#'   species = Species$Human, population = HumanPopulation$European_ICRP_2002,
#'   gender = Gender$Male, weight = 70
#' )
#' writeIndividualToXLS(humanIndividualCharacteristics, pathToExcelFile)
#' }
#'
#' @export
writeIndividualToXLS <- function(individualCharacteristics, outputXLSPath) {
  validateIsString(outputXLSPath)

  individual <- createIndividual(individualCharacteristics)

  columnNames <- c("Container Path", "Parameter Name", "Value", "Units")

  containerPaths <- vector(
    "character",
    length(individual$distributedParameters$paths)
  )
  paramNames <- vector(
    "character",
    length(individual$distributedParameters$paths)
  )
  values <- vector("numeric", length(individual$distributedParameters$paths))
  units <- vector("character", length(individual$distributedParameters$paths))

  for (i in seq_along(individual$distributedParameters$paths)) {
    splittedPath <- .splitParameterPathIntoContainerAndName(individual$distributedParameters$paths[[
      i
    ]])

    containerPaths[i] <- splittedPath$containerPath
    paramNames[i] <- splittedPath$parameterName
    values[i] <- individual$distributedParameters$values[[i]]
    units[i] <- individual$distributedParameters$units[[i]]
  }

  output <- data.frame(
    unlist(containerPaths, use.names = FALSE),
    unlist(paramNames, use.names = FALSE),
    unlist(as.numeric(values), use.names = FALSE),
    unlist(units, use.names = FALSE)
  )
  colnames(output) <- columnNames

  .writeExcel(data = output, path = outputXLSPath)
  return(outputXLSPath)
}

#' Read individual characteristics from file
#'
#' @details Read individual characteristics from an excel sheet
#' and create an `IndividualCharacteristics`-object. The excel sheet must have
#' the columns `IndividualId`, `Species`, `Population`, `Gender`, `Weight [kg]`,
#' `Height [cm]`, `Age [year(s)]`, and `Protein Ontogenies`.

#' @param XLSpath Full path to the excel file
#'
#' @param individualId (String) Id of the individual as stored in the
#'   `IndividualId` column.
#' @param sheet Name of the sheet. If `NULL` (default), the first sheet of the
#'   file is used.
#' @param nullIfNotFound Boolean. If `TRUE` (default), `NULL` is returned if
#' no entry with the give `individualId` exists. Otherwise, an error is thrown.
#'
#' @returns An `IndividualCharacteristics` object
#' @import ospsuite
#' @export
readIndividualCharacteristicsFromXLS <- function(
  XLSpath, # nolint: object_length_linter.
  individualId,
  sheet = "IndividualBiometrics",
  nullIfNotFound = TRUE
) {
  validateIsString(c(XLSpath, individualId))

  # If no sheet has been specified, read from the first sheet
  if (is.null(sheet)) {
    sheet <- c(1)
  }

  columnNames <- c(
    "IndividualId",
    "Species",
    "Population",
    "Gender",
    "Weight [kg]",
    "Height [cm]",
    "Age [year(s)]",
    "Protein Ontogenies"
  )

  data <- readExcel(path = XLSpath, sheet = sheet)
  if (!all(columnNames %in% names(data))) {
    stop(messages$errorWrongXLSStructure(XLSpath, columnNames))
  }
  # Find the row with the given individual id
  rowIdx <- which(data$IndividualId == individualId)
  if (length(rowIdx) == 0) {
    if (nullIfNotFound) {
      return(NULL)
    }
    stop(messages$errorWrongIndividualId(individualId))
  }

  # Create ontogenies for the proteins
  moleculeOntogenies <- .readOntongeniesFromXLS(data[rowIdx, ])

  # Create the IndividualCharacteristics object
  individualCharacteristics <- ospsuite::createIndividualCharacteristics(
    species = data$Species[[rowIdx]],
    population = data$Population[[rowIdx]],
    gender = data$Gender[[rowIdx]],
    weight = data$`Weight [kg]`[[rowIdx]],
    height = data$`Height [cm]`[[rowIdx]],
    age = data$`Age [year(s)]`[[rowIdx]],
    moleculeOntogenies = moleculeOntogenies
  )

  return(individualCharacteristics)
}

#' Apply an individual to a simulation
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
#' `individualCharacteristics`. Parameters that do not exist in `simulation` are
#' skipped.
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
#' The wall thickness and wall volume of the intestinal lumen segments keep
#' their human formulas after scaling. They only feed the mucosa volumes, which
#' are set to the values of the target species, so simulation results are not
#' affected.
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
  isHuman <- individualCharacteristics$species == ospsuite::Species$Human
  if (isHuman) {
    .stopIfNotHumanModel(simulation)
  }
  individual <- ospsuite::createIndividual(individualCharacteristics)

  # For human species, only set distributed parameters
  allParamPaths <- individual$distributedParameters$paths
  allParamValues <- individual$distributedParameters$values
  allParamUnits <- individual$distributedParameters$units

  # For other species, also add derived parameters
  if (!isHuman) {
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

  # `createIndividual()` returns only the parameters PK-Sim varies between
  # individuals of a species. The constants that differ between species come
  # from the individual building block. They are applied after the derived
  # parameters, which are already at the values of the species.
  if (!isHuman) {
    speciesParams <- .speciesParametersFromBuildingBlock(
      individualCharacteristics,
      simulation
    )
    if (length(speciesParams$paths) > 0) {
      ospsuite::setParameterValuesByPath(
        parameterPaths = speciesParams$paths,
        values = speciesParams$values,
        simulation = simulation,
        units = speciesParams$units,
        stopIfNotFound = FALSE
      )
    }
  }

  invisible(simulation)
}

# Species constants for a non-human individual: every parameter with a value in
# the individual building block PK-Sim creates for `individualCharacteristics`,
# restricted to the parameters that exist in `simulation`. Entries the building
# block defines by a formula (wall thickness and wall volume of the lumen
# segments) come back without a value and are left out.
# @keywords internal
# @noRd
.speciesParametersFromBuildingBlock <- function(
  individualCharacteristics,
  simulation
) {
  gestationalAge <- .snapshotParameterValue(
    individualCharacteristics$gestationalAge
  )
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
  keep <- is.finite(parameters[["Value"]]) &
    paths %in% ospsuite::getAllParameterPathsIn(simulation)

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
  stop(messages$errorHumanIndividualForNonHumanModel(simulationName, species))
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
  if (is.null(species) || length(species) == 0 || is.na(species) || !nzchar(species)) {
    return(NULL)
  }
  species
}
