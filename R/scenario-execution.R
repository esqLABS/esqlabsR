# Internal scenario execution helpers ----

#' Merge the parameter layers for a scenario
#'
#' @description Builds the final parameter structure for a scenario by merging
#' (in order, last-write-wins): scenario `modelParameters` (iterated in listed
#' order), species defaults, individual inline `parameters`, application inline
#' `parameters`, then `customParams`. Pure function: no I/O, no simulation load.
#'
#' @param scenario A scenario plain-list (as in `project$scenarios[[name]]`).
#' @param project A `Project` object.
#' @param customParams Optional caller-supplied parameter structure.
#'
#' @returns A parameter structure `list(paths, values, units)`. May be `NULL`
#'   if no layer sets any parameter.
#' @keywords internal
.mergeScenarioParameters <- function(scenario, project, customParams = NULL) {
  params <- NULL

  # 1. Model parameters (iterate in the order listed on the scenario)
  if (!is.null(scenario$modelParameters)) {
    for (setId in scenario$modelParameters) {
      setParams <- project$modelParameters[[setId]]
      if (!is.null(setParams)) {
        params <- extendParameterStructure(
          parameters = params,
          newParameters = setParams
        )
      }
    }
  }

  # 2. Species defaults (only if individual is set and resolves)
  if (!is.null(scenario$individualId) && !is.na(scenario$individualId)) {
    indivData <- project$individuals[[scenario$individualId]]
    if (!is.null(indivData)) {
      speciesParams <- .getSpeciesParameters(indivData$species)
      if (!is.null(speciesParams)) {
        params <- extendParameterStructure(
          parameters = params,
          newParameters = speciesParams
        )
      }

      # 3. Individual inline parameters
      if (!is.null(indivData$parameters)) {
        params <- extendParameterStructure(
          parameters = params,
          newParameters = indivData$parameters
        )
      }
    }
  }

  # 4. Application inline parameters (back-compat with old parallel-vector
  # shape during the transition; the $paths fallback is removed in Task 7)
  if (
    !is.null(scenario$applicationProtocol) &&
      !is.na(scenario$applicationProtocol)
  ) {
    appData <- project$applications[[scenario$applicationProtocol]]
    if (is.null(appData)) {
      stop(messages$errorApplicationProtocolNotFound(
        scenarioName = scenario$scenarioName,
        applicationProtocol = scenario$applicationProtocol
      ))
    }
    if (!is.null(appData$parameters)) {
      params <- extendParameterStructure(
        parameters = params,
        newParameters = appData$parameters
      )
    }
  }

  # 5. Custom params
  if (!is.null(customParams)) {
    params <- extendParameterStructure(
      parameters = params,
      newParameters = customParams
    )
  }

  params
}

#' Prepare a single scenario for simulation
#'
#' @description Loads simulation, creates ospsuite objects (with caching),
#' merges parameters, sets outputs, sets time intervals, initialises the
#' simulation, handles steady-state, and creates the population object.
#' Does NOT run the simulation.
#'
#' @param scenario A `Scenario` object (plain data class).
#' @param project A `Project` object.
#' @param customParams Optional parameter structure from the caller.
#' @param cache An environment with `$individuals` and `$populations` named lists.
#' @param simulationRunOptions Optional `SimulationRunOptions` (used for steady-state).
#'
#' @returns A list with `simulation` and `population` (NULL for individual scenarios).
#' @keywords internal
.prepareScenario <- function(
  scenario,
  project,
  customParams,
  cache,
  simulationRunOptions
) {
  # 1. Load simulation
  simulation <- ospsuite::loadSimulation(
    filePath = file.path(project$modelFolder, scenario$modelFile),
    loadFromCache = FALSE
  )
  simulation$name <- scenario$scenarioName

  # 2. Build merged parameter structure
  params <- .mergeScenarioParameters(scenario, project, customParams)

  # 2b. Individual characteristics (still needed for simulation initialization)
  individualCharacteristics <- NULL
  if (!is.null(scenario$individualId) && !is.na(scenario$individualId)) {
    indivData <- project$individuals[[scenario$individualId]]
    if (is.null(indivData)) {
      warning(messages$warningNoIndividualCharacteristics(
        scenarioName = scenario$scenarioName,
        individualId = scenario$individualId
      ))
    } else {
      if (!is.null(cache$individuals[[scenario$individualId]])) {
        individualCharacteristics <- cache$individuals[[scenario$individualId]]
      } else {
        moleculeOntogenies <- .readOntongeniesFromList(
          indivData$proteinOntogenies
        )
        individualCharacteristics <- ospsuite::createIndividualCharacteristics(
          species = indivData$species,
          population = indivData$population,
          gender = indivData$gender,
          weight = as.double(indivData$weight),
          height = as.double(indivData$height),
          age = as.double(indivData$age),
          moleculeOntogenies = moleculeOntogenies
        )
        cache$individuals[[scenario$individualId]] <- individualCharacteristics
      }
    }
  }

  # 3. Set outputs
  if (!is.null(scenario$outputPaths)) {
    setOutputs(
      quantitiesOrPaths = scenario$outputPaths,
      simulation = simulation
    )
  }

  # 4. Set simulation time intervals
  if (!is.null(scenario$simulationTime)) {
    if (is.null(scenario$simulationTimeUnit)) {
      stop(messages$stopScenarioMissingTimeUnit(scenario$scenarioName))
    }
    clearOutputIntervals(simulation)
    for (i in seq_along(scenario$simulationTime)) {
      addOutputInterval(
        simulation = simulation,
        startTime = toBaseUnit(
          quantityOrDimension = ospDimensions$Time,
          values = scenario$simulationTime[[i]][1],
          unit = scenario$simulationTimeUnit
        ),
        endTime = toBaseUnit(
          quantityOrDimension = ospDimensions$Time,
          values = scenario$simulationTime[[i]][2],
          unit = scenario$simulationTimeUnit
        ),
        resolution = scenario$simulationTime[[i]][3] /
          toBaseUnit(
            quantityOrDimension = ospDimensions$Time,
            values = 1,
            unit = scenario$simulationTimeUnit
          )
      )
    }
  }

  # 5. Initialize simulation (apply individual + all params)
  initializeSimulation(
    simulation = simulation,
    individualCharacteristics = individualCharacteristics,
    additionalParams = params,
    stopIfParameterNotFound = TRUE
  )

  # 6. Create population (for population scenarios)
  population <- NULL
  if (scenario$simulationType == "Population") {
    if (is.null(scenario$populationId)) {
      stop(messages$noPopulationIdForPopulationScenario(scenario$scenarioName))
    }
    if (scenario$readPopulationFromCSV) {
      populationPath <- paste0(
        file.path(project$populationsFolder, scenario$populationId),
        ".csv"
      )
      population <- loadPopulation(populationPath)
    } else {
      # Create or retrieve from cache
      if (!is.null(cache$populations[[scenario$populationId]])) {
        population <- cache$populations[[scenario$populationId]]
      } else {
        popData <- project$populations[[scenario$populationId]]
        if (is.null(popData)) {
          stop(paste0(
            "Population '",
            scenario$populationId,
            "' referenced by scenario '",
            scenario$scenarioName,
            "' not found in project."
          ))
        }
        moleculeOntogenies <- .readOntongeniesFromList(
          popData$proteinOntogenies
        )
        popArgs <- popData
        popArgs$proteinOntogenies <- NULL
        popArgs$moleculeOntogenies <- moleculeOntogenies
        popResult <- do.call(ospsuite::createPopulationCharacteristics, popArgs)
        popObj <- createPopulation(populationCharacteristics = popResult)
        population <- popObj$population
        cache$populations[[scenario$populationId]] <- population
      }
    }
  }

  # 7. Handle steady state
  if (scenario$simulateSteadyState) {
    ignoreIfFormula <- !scenario$overwriteFormulasInSS
    initialValues <- ospsuite::getSteadyState(
      simulations = list(simulation),
      steadyStateTime = list(scenario$steadyStateTime),
      ignoreIfFormula = ignoreIfFormula,
      simulationRunOptions = simulationRunOptions
    )
    ospsuite::setQuantityValuesByPath(
      quantityPaths = initialValues[[simulation$id]]$paths,
      values = initialValues[[simulation$id]]$values,
      simulation = simulation
    )
  }

  list(
    simulation = simulation,
    population = population
  )
}

#' Collect output values for a single completed scenario
#'
#' @description Given a scenario's simulation, raw results, and population,
#' resolves output quantities and builds the standard return list used by
#' `runScenarios()`.
#'
#' @param scenario A `Scenario` object (plain data class).
#' @param simulation The initialised `Simulation` object.
#' @param results The `SimulationResults` for this simulation (may be `NULL`).
#' @param population The `Population` object (`NULL` for individual scenarios).
#'
#' @returns A list with `simulation`, `results`, `outputValues`, `population`.
#' @keywords internal
.collectScenarioResult <- function(scenario, simulation, results, population) {
  outputQuantities <- NULL
  if (!is.null(scenario$outputPaths)) {
    outputQuantities <- getAllQuantitiesMatching(
      scenario$outputPaths,
      simulation
    )
  }

  outputValues <- NULL
  if (is.null(results)) {
    warning(messages$missingResultsForScenario(scenario$scenarioName))
  } else {
    outputValues <- getOutputValues(
      results,
      quantitiesOrPaths = outputQuantities,
      population = population,
      addMetaData = FALSE
    )
  }

  list(
    simulation = simulation,
    results = results,
    outputValues = outputValues,
    population = population
  )
}

# Run scenarios ----

#' Run a set of scenarios.
#'
#' @param project An object of type `Project` loaded
#'   from JSON. Its `scenarios` field is used to select and run scenarios.
#' @param scenarioNames Optional character vector of scenario names to run. If
#'   `NULL` (default), all scenarios defined in `project` are run.
#' @param customParams A list containing vectors `paths`, `values`, and `units`
#'   that will be applied to all scenarios.
#' @param simulationRunOptions Object of type `SimulationRunOptions` that will
#'   be passed to simulation runs. If `NULL`, default options are used.
#' @param validate If `TRUE` (default), the `scenarios` and `crossReferences`
#'   sections of the project are validated before running. Any critical
#'   errors abort the run with a formatted message. Set to `FALSE` to skip
#'   the pre-flight check (e.g. when intentionally running a partially
#'   defined project). Validation is skipped automatically if a full
#'   [validateProject()] has already succeeded since the last project
#'   mutation.
#'
#' @returns A named list, where the names are scenario names, and the values are
#'   lists with the entries `simulation` being the initialized `Simulation`
#'   object with applied parameters, `results` being `SimulationResults` object
#'   produced by running the simulation, `outputValues` the output values of the
#'   `SimulationResults`, and `population` the `Population` object if the
#'   scenario is a population simulation.
#' @details If simulation of a scenario fails, a warning is produced, and the
#' `outputValues` for this scenario is `NULL`.
#'
#' @export
runScenarios <- function(
  project,
  scenarioNames = NULL,
  customParams = NULL,
  simulationRunOptions = NULL,
  validate = TRUE
) {
  validateIsOfType(project, "Project")
  .validateParametersStructure(
    parameterStructure = customParams,
    argumentName = "customParams",
    nullAllowed = TRUE
  )

  if (isTRUE(validate)) {
    .ensureValid(
      project,
      sections = c("scenarios", "crossReferences"),
      opName = "run scenarios"
    )
  }

  allScenarios <- project$scenarios
  if (is.null(scenarioNames)) {
    scenarioNames <- names(allScenarios)
  }

  # Validate scenario names
  unknownNames <- setdiff(scenarioNames, names(allScenarios))
  if (length(unknownNames) > 0) {
    stop(paste0(
      "Unknown scenario names: ",
      paste(unknownNames, collapse = ", ")
    ))
  }

  # Run-scoped cache for ospsuite objects
  cache <- new.env(parent = emptyenv())
  cache$individuals <- list()
  cache$populations <- list()

  # Prepare all scenarios (load sim, apply params, set outputs/time, init, SS)
  preparedList <- vector("list", length(scenarioNames))
  for (idx in seq_along(scenarioNames)) {
    scenarioName <- scenarioNames[[idx]]
    scenario <- allScenarios[[scenarioName]]
    preparedList[[idx]] <- .prepareScenario(
      scenario = scenario,
      project = project,
      customParams = customParams,
      cache = cache,
      simulationRunOptions = simulationRunOptions
    )
    names(preparedList)[[idx]] <- scenarioName
  }

  # Run all individual simulations in one batch (ospsuite runs them in parallel)
  individualSimulations <- list()
  for (idx in seq_along(scenarioNames)) {
    if (is.null(preparedList[[idx]]$population)) {
      individualSimulations <- c(
        individualSimulations,
        preparedList[[idx]]$simulation
      )
    }
  }
  simulationResults <- list()
  if (length(individualSimulations) > 0) {
    simulationResults <- runSimulations(
      simulations = individualSimulations,
      simulationRunOptions = simulationRunOptions
    )
  }

  # Run population simulations sequentially and append their results.
  # `runSimulations()` only batches independent simulations; each population
  # run requires its own population object, so they cannot be combined in a
  # single batch call.
  for (idx in seq_along(scenarioNames)) {
    prepared <- preparedList[[idx]]
    if (!is.null(prepared$population)) {
      populationResults <- runSimulations(
        simulations = prepared$simulation,
        population = prepared$population,
        simulationRunOptions = simulationRunOptions
      )
      simulationResults <- c(simulationResults, populationResults)
    }
  }

  # Collect output values for each scenario
  returnList <- vector("list", length(scenarioNames))
  for (idx in seq_along(scenarioNames)) {
    scenarioName <- scenarioNames[[idx]]
    scenario <- allScenarios[[scenarioName]]
    prepared <- preparedList[[idx]]
    results <- simulationResults[[prepared$simulation$id]]

    returnList[[idx]] <- .collectScenarioResult(
      scenario = scenario,
      simulation = prepared$simulation,
      results = results,
      population = prepared$population
    )
    names(returnList)[[idx]] <- scenarioName
  }

  return(returnList)
}

# Internal scenario helpers ----

#' Parse protein ontogenies from a string
#'
#' @param ontogenyString A string in the format
#'   "Molecule1:Ontogeny1,Molecule2:Ontogeny2" or NULL.
#'
#' @returns A list of `MoleculeOntogeny` objects, or NULL.
#' @keywords internal
#' @noRd
.readOntongeniesFromList <- function(ontogenyString) {
  if (
    is.null(ontogenyString) || is.na(ontogenyString) || ontogenyString == ""
  ) {
    return(NULL)
  }
  # The string format is "Molecule1:Ontogeny1,Molecule2:Ontogeny2"
  ontogeniesSplit <- unlist(strsplit(ontogenyString, split = ",", fixed = TRUE))
  ontogeniesSplit <- trimws(ontogeniesSplit)
  moleculeOntogenies <- vector("list", length(ontogeniesSplit))
  for (i in seq_along(ontogeniesSplit)) {
    parts <- unlist(strsplit(ontogeniesSplit[[i]], split = ":", fixed = TRUE))
    if (length(parts) != 2) {
      stop(messages$errorWrongOntogenyStructure(ontogeniesSplit[[i]]))
    }
    protein <- parts[[1]]
    ontogeny <- parts[[2]]
    validateEnumValue(value = ontogeny, enum = ospsuite::StandardOntogeny)
    moleculeOntogenies[[i]] <- ospsuite::MoleculeOntogeny$new(
      molecule = protein,
      ontogeny = ospsuite::StandardOntogeny[[ontogeny]]
    )
  }
  moleculeOntogenies
}

#' Read parameter values from a structured Excel file
#'
#' Each excel sheet must consist of columns 'Container Path', 'Parameter Name',
#' 'Value', and 'Units'.
#'
#' @param paramsXLSpath Path to the excel file
#' @param sheets Names of the excel sheets containing the information about the
#'   parameters. Multiple sheets can be processed. If no sheets are provided,
#'   the first one in the Excel file is used.
#'
#' @returns A list containing vectors `paths` with the full paths to the
#'   parameters, `values` the values of the parameters, and `units` with the
#'   units the values are in.
#' @keywords internal
#' @noRd
.readParametersFromXLS <- function(paramsXLSpath, sheets = NULL) {
  columnNames <- c("Container Path", "Parameter Name", "Value", "Units")
  validateIsString(paramsXLSpath)
  validateIsString(sheets, nullAllowed = TRUE)

  if (is.null(sheets)) {
    sheets <- c(1)
  }

  pathsValuesVector <- vector(mode = "numeric")
  pathsUnitsVector <- vector(mode = "character")

  for (sheet in sheets) {
    data <- readExcel(path = paramsXLSpath, sheet = sheet)

    if (!all(columnNames %in% names(data))) {
      stop(messages$errorWrongXLSStructure(
        filePath = paramsXLSpath,
        expectedColNames = columnNames
      ))
    }

    fullPaths <- paste(
      data[["Container Path"]],
      data[["Parameter Name"]],
      sep = "|"
    )
    pathsValuesVector[fullPaths] <- as.numeric(data[["Value"]])

    pathsUnitsVector[fullPaths] <- tidyr::replace_na(
      data = as.character(data[["Units"]]),
      replace = ""
    )
  }

  return(.parametersVectorToList(pathsValuesVector, pathsUnitsVector))
}

#' Read species-specific parameters from the bundled SpeciesParameters.xlsx
#'
#' @param species Character scalar — the species name (e.g. "Rat", "Mouse").
#' @returns A parameter structure list (`paths`, `values`, `units`) or `NULL`
#'   if no sheet matches the species.
#' @keywords internal
#' @noRd
.getSpeciesParameters <- function(species) {
  filePath <- system.file(
    "extdata",
    "SpeciesParameters.xlsx",
    package = "esqlabsR"
  )
  if (!file.exists(filePath)) {
    return(NULL)
  }

  sheets <- readxl::excel_sheets(filePath)
  if (!species %in% sheets) {
    return(NULL)
  }

  .readParametersFromXLS(paramsXLSpath = filePath, sheets = species)
}
