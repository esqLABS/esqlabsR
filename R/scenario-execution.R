# Internal scenario execution helpers ----
#
# Modern (JSON-Project-driven) runtime path. Bodies match
# `json-as-primary-input-v2:R/scenario-execution.R` line-for-line except
# for two transitional helpers (`.parameterGroupToStructure`,
# `.findById`) that bridge the gap between our JSON-faithful parser and
# end-state's pre-shaped section parsers. Those two helpers go away in
# Chapter 6/7 when the section parsers become typed.

# Transitional bridge helpers ----

# Convert a record-shape parameter list (as parsed JSON-faithfully from
# `modelParameters[[group]]`, `individuals[[id]]$parameters`, or
# `applications[[name]]$parameters`) into the parallel-vector
# `list(paths, values, units)` shape consumed by
# `extendParameterStructure()`.
# @keywords internal
# @noRd
.parameterGroupToStructure <- function(entries) {
  if (is.null(entries) || length(entries) == 0L) {
    return(NULL)
  }
  paths <- vapply(entries, function(e) {
    paste(e$containerPath, e$parameterName, sep = "|")
  }, character(1))
  values <- vapply(entries, function(e) as.numeric(e$value), numeric(1))
  units <- vapply(entries, function(e) e$units %||% "", character(1))
  list(paths = paths, values = values, units = units)
}

# Find the first item in `items` whose `[[idField]]` equals `id`.
# Returns `NULL` if no match.
# @keywords internal
# @noRd
.findById <- function(items, idField, id) {
  if (is.null(items) || length(items) == 0L || is.null(id) || is.na(id)) {
    return(NULL)
  }
  for (item in items) {
    if (identical(item[[idField]], id)) {
      return(item)
    }
  }
  NULL
}

# Five-layer merge ----

# Pure function. Builds a `list(paths, values, units)` parameter
# structure (or `NULL`) for one scenario. Layers, in order
# (last-write-wins): scenario `modelParameters` (each group iterated
# in listed order) -> species defaults -> individual inline `parameters`
# -> application inline `parameters` -> caller-supplied `customParams`.
# @keywords internal
# @noRd
.mergeScenarioParameters <- function(scenario, project, customParams = NULL) {
  params <- NULL

  # 1. modelParameters
  if (!is.null(scenario$modelParameters)) {
    for (setId in scenario$modelParameters) {
      setParams <- .parameterGroupToStructure(
        project$modelParameters[[setId]]
      )
      if (!is.null(setParams)) {
        params <- extendParameterStructure(
          parameters = params,
          newParameters = setParams
        )
      }
    }
  }

  # 2. + 3. species defaults + individual inline parameters
  if (!is.null(scenario$individualId) && !is.na(scenario$individualId)) {
    indivData <- .findById(
      project$individuals,
      "individualId",
      scenario$individualId
    )
    if (!is.null(indivData)) {
      speciesParams <- .getSpeciesParameters(indivData$species)
      if (!is.null(speciesParams)) {
        params <- extendParameterStructure(
          parameters = params,
          newParameters = speciesParams
        )
      }
      indivInline <- .parameterGroupToStructure(indivData$parameters)
      if (!is.null(indivInline)) {
        params <- extendParameterStructure(
          parameters = params,
          newParameters = indivInline
        )
      }
    }
  }

  # 4. application inline parameters
  if (
    !is.null(scenario$applicationProtocol) &&
      !is.na(scenario$applicationProtocol)
  ) {
    appData <- project$applications[[scenario$applicationProtocol]]
    if (is.null(appData)) {
      cli::cli_abort(messages$errorApplicationProtocolNotFound(
        scenarioName = scenario$scenarioName,
        applicationProtocol = scenario$applicationProtocol
      ))
    }
    appInline <- .parameterGroupToStructure(appData$parameters)
    if (!is.null(appInline)) {
      params <- extendParameterStructure(
        parameters = params,
        newParameters = appInline
      )
    }
  }

  # 5. customParams
  if (!is.null(customParams)) {
    params <- extendParameterStructure(
      parameters = params,
      newParameters = customParams
    )
  }

  params
}

# Read species defaults from the bundled SpeciesParameters.xlsx if a
# matching sheet exists. `NULL` when the file or sheet is missing.
# @keywords internal
# @noRd
.getSpeciesParameters <- function(species) {
  if (is.null(species) || is.na(species)) {
    return(NULL)
  }
  filePath <- system.file(
    "extdata",
    "SpeciesParameters.xlsx",
    package = "esqlabsR"
  )
  if (!nzchar(filePath) || !file.exists(filePath)) {
    return(NULL)
  }
  sheets <- readxl::excel_sheets(filePath)
  if (!any(sheets == species)) {
    return(NULL)
  }
  readParametersFromXLS(paramsXLSpath = filePath, sheets = species)
}

# .prepareScenario ----

# Prepare a single scenario for simulation: load Simulation, build
# IndividualCharacteristics (with run-scoped cache), set outputs,
# set time intervals, initialize the simulation, build/cache
# Population, run steady-state if requested. Returns
# list(simulation, population). Does NOT run the simulation.
# @keywords internal
# @noRd
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

  # 2b. IndividualCharacteristics
  individualCharacteristics <- NULL
  if (!is.null(scenario$individualId) && !is.na(scenario$individualId)) {
    indivData <- .findById(
      project$individuals,
      "individualId",
      scenario$individualId
    )
    if (is.null(indivData)) {
      cli::cli_warn(messages$warningNoIndividualCharacteristics(
        scenarioName = scenario$scenarioName,
        individualId = scenario$individualId
      ))
    } else {
      cached <- cache$individuals[[scenario$individualId]]
      if (!is.null(cached)) {
        individualCharacteristics <- cached
      } else {
        moleculeOntogenies <- .readOntogeniesFromList(
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
      quantitiesOrPaths = unname(scenario$outputPaths),
      simulation = simulation
    )
  }

  # 4. Set simulation time intervals
  if (!is.null(scenario$simulationTime)) {
    if (is.null(scenario$simulationTimeUnit)) {
      cli::cli_abort(messages$stopScenarioMissingTimeUnit(scenario$scenarioName))
    }
    clearOutputIntervals(simulation)
    for (i in seq_along(scenario$simulationTime)) {
      addOutputInterval(
        simulation = simulation,
        startTime = ospsuite::toBaseUnit(
          quantityOrDimension = ospDimensions$Time,
          values = scenario$simulationTime[[i]][1],
          unit = scenario$simulationTimeUnit
        ),
        endTime = ospsuite::toBaseUnit(
          quantityOrDimension = ospDimensions$Time,
          values = scenario$simulationTime[[i]][2],
          unit = scenario$simulationTimeUnit
        ),
        resolution = scenario$simulationTime[[i]][3] /
          ospsuite::toBaseUnit(
            quantityOrDimension = ospDimensions$Time,
            values = 1,
            unit = scenario$simulationTimeUnit
          )
      )
    }
  }

  # 5. Initialize simulation
  initializeSimulation(
    simulation = simulation,
    individualCharacteristics = individualCharacteristics,
    additionalParams = params,
    stopIfParameterNotFound = TRUE
  )

  # 6. Population
  population <- NULL
  if (scenario$simulationType == "Population") {
    if (is.null(scenario$populationId)) {
      cli::cli_abort(messages$noPopulationIdForPopulationScenario(scenario$scenarioName))
    }
    if (scenario$readPopulationFromCSV) {
      populationPath <- paste0(
        file.path(project$populationsFolder, scenario$populationId),
        ".csv"
      )
      population <- loadPopulation(populationPath)
    } else {
      cached <- cache$populations[[scenario$populationId]]
      if (!is.null(cached)) {
        population <- cached
      } else {
        popData <- .findById(
          project$populations,
          "populationId",
          scenario$populationId
        )
        if (is.null(popData)) {
          cli::cli_abort(
            "Population {.val {scenario$populationId}} referenced by scenario {.val {scenario$scenarioName}} not found in project."
          )
        }
        moleculeOntogenies <- .readOntogeniesFromList(
          popData$proteinOntogenies
        )
        popArgs <- popData
        popArgs$populationId <- NULL
        popArgs$proteinOntogenies <- NULL
        popArgs$moleculeOntogenies <- moleculeOntogenies
        # JSON integers (e.g. ageMin: 18) must be coerced to double because
        # createPopulationCharacteristics passes them to ParameterRange which
        # expects Nullable<Double>, not Int32.
        numericFields <- c(
          "numberOfIndividuals", "proportionOfFemales",
          "weightMin", "weightMax", "heightMin", "heightMax",
          "ageMin", "ageMax", "BMIMin", "BMIMax",
          "gestationalAgeMin", "gestationalAgeMax"
        )
        for (field in numericFields) {
          if (!is.null(popArgs[[field]])) {
            popArgs[[field]] <- as.double(popArgs[[field]])
          }
        }
        popResult <- do.call(ospsuite::createPopulationCharacteristics, popArgs)
        popObj <- createPopulation(populationCharacteristics = popResult)
        population <- popObj$population
        cache$populations[[scenario$populationId]] <- population
      }
    }
  }

  # 7. Steady state
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

  list(simulation = simulation, population = population)
}

# .collectScenarioResult ----

# Resolve output quantities and build the standard return list for one
# scenario.
# @keywords internal
# @noRd
.collectScenarioResult <- function(scenario, simulation, results, population) {
  outputQuantities <- NULL
  if (!is.null(scenario$outputPaths)) {
    outputQuantities <- getAllQuantitiesMatching(
      unname(scenario$outputPaths),
      simulation
    )
  }
  outputValues <- NULL
  if (is.null(results)) {
    cli::cli_warn(messages$missingResultsForScenario(scenario$scenarioName))
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

# Parse "Molecule:Ontogeny,Molecule:Ontogeny" into MoleculeOntogeny list.
# Returns NULL on empty input.
# @keywords internal
# @noRd
.readOntogeniesFromList <- function(ontogenyString) {
  if (
    is.null(ontogenyString) ||
      is.na(ontogenyString) ||
      identical(ontogenyString, "")
  ) {
    return(NULL)
  }
  parts <- trimws(unlist(strsplit(ontogenyString, ",", fixed = TRUE)))
  out <- vector("list", length(parts))
  for (i in seq_along(parts)) {
    pair <- unlist(strsplit(parts[[i]], ":", fixed = TRUE))
    if (length(pair) != 2L) {
      cli::cli_abort(messages$errorWrongOntogenyStructure(parts[[i]]))
    }
    validateEnumValue(value = pair[[2]], enum = ospsuite::StandardOntogeny)
    out[[i]] <- ospsuite::MoleculeOntogeny$new(
      molecule = pair[[1]],
      ontogeny = ospsuite::StandardOntogeny[[pair[[2]]]]
    )
  }
  out
}

# .runScenariosFromProject ----

# Modern runScenarios() body. Internal -- `R/utilities-scenarios.R`
# wraps this with the type-sniff dispatcher in the exported
# `runScenarios()` symbol (Task 7).
# @keywords internal
# @noRd
.runScenariosFromProject <- function(
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
  # `validate` arg accepted for end-state-shape parity. Chapter 4
  # wires it to `.ensureValid()`. No-op here.

  allScenarios <- project$scenarios
  if (is.null(scenarioNames)) {
    scenarioNames <- names(allScenarios)
  }
  unknownNames <- setdiff(scenarioNames, names(allScenarios))
  if (length(unknownNames) > 0) {
    cli::cli_abort(
      "Unknown scenario names: {.val {unknownNames}}."
    )
  }

  cache <- new.env(parent = emptyenv())
  cache$individuals <- list()
  cache$populations <- list()

  prepared <- vector("list", length(scenarioNames))
  for (idx in seq_along(scenarioNames)) {
    name <- scenarioNames[[idx]]
    prepared[[idx]] <- .prepareScenario(
      scenario = allScenarios[[name]],
      project = project,
      customParams = customParams,
      cache = cache,
      simulationRunOptions = simulationRunOptions
    )
    names(prepared)[[idx]] <- name
  }

  individualSimulations <- list()
  for (idx in seq_along(scenarioNames)) {
    if (is.null(prepared[[idx]]$population)) {
      individualSimulations <- c(
        individualSimulations,
        prepared[[idx]]$simulation
      )
    }
  }
  simulationResults <- list()
  if (length(individualSimulations) > 0L) {
    simulationResults <- runSimulations(
      simulations = individualSimulations,
      simulationRunOptions = simulationRunOptions
    )
  }

  for (idx in seq_along(scenarioNames)) {
    p <- prepared[[idx]]
    if (!is.null(p$population)) {
      populationResults <- runSimulations(
        simulations = p$simulation,
        population = p$population,
        simulationRunOptions = simulationRunOptions
      )
      simulationResults <- c(simulationResults, populationResults)
    }
  }

  out <- vector("list", length(scenarioNames))
  for (idx in seq_along(scenarioNames)) {
    name <- scenarioNames[[idx]]
    p <- prepared[[idx]]
    out[[idx]] <- .collectScenarioResult(
      scenario = allScenarios[[name]],
      simulation = p$simulation,
      results = simulationResults[[p$simulation$id]],
      population = p$population
    )
    names(out)[[idx]] <- name
  }
  out
}
