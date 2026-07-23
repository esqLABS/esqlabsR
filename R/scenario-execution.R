# Internal scenario execution helpers ----
#
# Modern (JSON-Project-driven) runtime path.

# Build an `ospsuite::SimulationRunOptions` from the project-level
# `defaultSimulationRunOptions` record (a plain list parsed from the
# `defaultSimulationRunOptions` JSON block), or return NULL when no defaults
# are declared (so the caller keeps the package defaults). Only the three
# settable fields are honored; an unset field keeps the `SimulationRunOptions`
# default. Mirrors the PI-config builder in `R/parameter-identification.R`.
# @keywords internal
# @noRd
.buildSimulationRunOptions <- function(defaults) {
  if (is.null(defaults) || length(defaults) == 0L) {
    return(NULL)
  }
  runOpts <- ospsuite::SimulationRunOptions$new()
  if (!is.null(defaults$numberOfCores)) {
    runOpts$numberOfCores <- as.integer(defaults$numberOfCores)
  }
  if (!is.null(defaults$checkForNegativeValues)) {
    runOpts$checkForNegativeValues <- isTRUE(defaults$checkForNegativeValues)
  }
  if (!is.null(defaults$showProgress)) {
    runOpts$showProgress <- isTRUE(defaults$showProgress)
  }
  runOpts
}

# Convert a record-shape parameter list (one set from the project's unified
# `parameterSets` section) into the parallel-vector
# `list(paths, values, units)` shape consumed by
# `extendParameterStructure()`.
# @keywords internal
# @noRd
.parameterSetToStructure <- function(entries) {
  if (is.null(entries) || length(entries) == 0L) {
    return(NULL)
  }
  paths <- vapply(
    entries,
    function(e) {
      paste(e$containerPath, e$parameterName, sep = "|")
    },
    character(1)
  )
  values <- vapply(entries, function(e) as.numeric(e$value), numeric(1))
  units <- vapply(entries, function(e) e$units %||% "", character(1))
  list(paths = paths, values = values, units = units)
}

# Convert one initial-condition set (a list of `{path, value, unit}` records)
# into the parallel-vector `list(paths, values, units)` shape consumed by
# `initializeSimulation()`'s `additionalInitialConditions`. The IC path is the
# full molecule path (no container/name split), so it maps straight to `paths`.
# @keywords internal
# @noRd
.initialConditionSetToStructure <- function(entries) {
  if (is.null(entries) || length(entries) == 0L) {
    return(NULL)
  }
  paths <- vapply(entries, function(e) e$path, character(1))
  values <- vapply(entries, function(e) as.numeric(e$value), numeric(1))
  units <- vapply(entries, function(e) e$unit %||% "", character(1))
  list(paths = paths, values = values, units = units)
}

# Build a `list(paths, values, units)` initial-condition structure (or `NULL`)
# for one scenario, from its `initialConditions` set references. Each id is
# looked up in the project's `initialConditions` section and its entries folded
# in (last-write-wins on a repeated path, via `extendParameterStructure`).
# Unknown set ids are silently skipped, matching `.mergeScenarioParameters`.
# @keywords internal
# @noRd
.mergeScenarioInitialConditions <- function(scenario, project) {
  if (is.null(scenario$initialConditions)) {
    return(NULL)
  }
  conditions <- NULL
  for (setId in scenario$initialConditions) {
    setConditions <- .initialConditionSetToStructure(
      project$definitions$initialConditions[[setId]]
    )
    if (!is.null(setConditions)) {
      conditions <- extendParameterStructure(
        parameters = conditions,
        newParameters = setConditions
      )
    }
  }
  conditions
}

# Five-layer merge ----

# Pure function. Builds a `list(paths, values, units)` parameter
# structure (or `NULL`) for one scenario. Layers, in order
# (last-write-wins): scenario `modelParameterSets` -> species defaults
# -> individual `parameterSets` -> application `parameterSets` ->
# caller-supplied `customParams`. Each of those reference fields is a
# list of set ids, iterated in listed order; every id is looked up in
# the project's single `parameterSets` section. Unknown ids are silently
# skipped (consistent across all three layers).
# @keywords internal
# @noRd
.mergeScenarioParameters <- function(scenario, project, customParams = NULL) {
  params <- NULL
  # Read the unified parameter-sets section once; the three loops below all
  # resolve their set ids against it.
  parameterSets <- project$definitions$parameterSets

  # 1. modelParameterSets
  if (!is.null(scenario$modelParameterSets)) {
    for (setId in scenario$modelParameterSets) {
      setParams <- .parameterSetToStructure(
        parameterSets[[setId]]
      )
      if (!is.null(setParams)) {
        params <- extendParameterStructure(
          parameters = params,
          newParameters = setParams
        )
      }
    }
  }

  # 2. + 3. species defaults + individual parameterSets
  if (!is.null(scenario$individualId) && !is.na(scenario$individualId)) {
    indivData <- project$definitions$individuals[[scenario$individualId]]
    if (!is.null(indivData)) {
      speciesParams <- .getSpeciesParameters(indivData$species)
      if (!is.null(speciesParams)) {
        params <- extendParameterStructure(
          parameters = params,
          newParameters = speciesParams
        )
      }
      for (setId in unlist(indivData$parameterSets)) {
        setParams <- .parameterSetToStructure(
          parameterSets[[setId]]
        )
        if (!is.null(setParams)) {
          params <- extendParameterStructure(
            parameters = params,
            newParameters = setParams
          )
        }
      }
    }
  }

  # 4. application parameterSets
  if (
    !is.null(scenario$applicationProtocol) &&
      !is.na(scenario$applicationProtocol)
  ) {
    appData <- project$definitions$applications[[scenario$applicationProtocol]]
    if (is.null(appData)) {
      cli::cli_abort(messages$applicationProtocolNotFound(
        scenarioName = scenario$scenarioName,
        applicationProtocol = scenario$applicationProtocol
      ))
    }
    for (setId in unlist(appData$parameterSets)) {
      setParams <- .parameterSetToStructure(
        parameterSets[[setId]]
      )
      if (!is.null(setParams)) {
        params <- extendParameterStructure(
          parameters = params,
          newParameters = setParams
        )
      }
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

# Population resolution ----

# Resolve the `ospsuite::Population` for a population scenario, dispatching on
# the population *entry's* `type`, not the scenario flag. A `programmatic` entry
# comes from the runtime store; a `csv` entry loads its `file`; an entry with no
# `type` is a demographics spec built via `createPopulationCharacteristics`,
# unless the scenario's `readPopulationFromCSV` flag is set (back-compat: that
# still loads `<populationId>.csv`). Resolved objects are cached per run so a
# population shared by two scenarios is built or loaded once.
# @keywords internal
# @noRd
.resolveScenarioPopulation <- function(scenario, project, cache) {
  if (is.null(scenario$populationId)) {
    cli::cli_abort(messages$noPopulationIdForPopulationScenario(
      scenario$scenarioName
    ))
  }
  cached <- cache$populations[[scenario$populationId]]
  if (!is.null(cached)) {
    return(cached)
  }

  popData <- project$definitions$populations[[scenario$populationId]]
  # The entry type wins over the scenario flag; a spec entry falls back to the
  # scenario's `readPopulationFromCSV` for the legacy CSV path.
  effectiveType <- popData$type %||%
    (if (isTRUE(scenario$readPopulationFromCSV)) "csv" else "spec")

  population <- switch(
    effectiveType,
    "programmatic" = .resolveProgrammaticPopulation(scenario, project),
    "csv" = .resolveCsvPopulation(scenario, project, popData),
    "spec" = .resolveSpecPopulation(scenario, popData)
  )
  cache$populations[[scenario$populationId]] <- population
  population
}

# Pull a session-injected `Population` from the runtime store. Fatal if the
# sentinel has no backing object (a project reloaded without re-injecting),
# since a population scenario cannot run without one.
# @keywords internal
# @noRd
.resolveProgrammaticPopulation <- function(scenario, project) {
  population <- project$getProgrammaticPopulation(scenario$populationId)
  if (is.null(population)) {
    cli::cli_abort(messages$populationProgrammaticUnresolved(
      id = scenario$populationId,
      scenarioName = scenario$scenarioName
    ))
  }
  population
}

# Load a population table from CSV. A `csv` entry names its own `file`; the
# legacy scenario-flag path (no `file`) derives `<populationId>.csv`. Either way
# the resolved path must stay under the populations folder (a shared project
# cannot escape itself via a `../` id), which must be declared.
# @keywords internal
# @noRd
.resolveCsvPopulation <- function(scenario, project, popData) {
  if (is.null(project$paths$populationsFolder)) {
    cli::cli_abort(messages$noPopulationsFolderForCSVPopulation(
      scenarioName = scenario$scenarioName,
      populationId = scenario$populationId
    ))
  }
  fileName <- popData$file %||% paste0(scenario$populationId, ".csv")
  populationPath <- .resolveProjectPath(
    fileName,
    project$paths$populationsFolder,
    "populationId"
  )
  loadPopulation(populationPath)
}

# Build a population from a demographics spec via
# `createPopulationCharacteristics`.
# @keywords internal
# @noRd
.resolveSpecPopulation <- function(scenario, popData) {
  if (is.null(popData)) {
    cli::cli_abort(messages$populationNotFoundForScenario(
      populationId = scenario$populationId,
      scenarioName = scenario$scenarioName
    ))
  }
  moleculeOntogenies <- .readOntogeniesFromList(popData$proteinOntogenies)
  popArgs <- unclass(popData)
  popArgs$proteinOntogenies <- NULL
  popArgs$moleculeOntogenies <- moleculeOntogenies
  # JSON integers (e.g. ageMin: 18) must be coerced to double because
  # createPopulationCharacteristics passes them to ParameterRange which
  # expects Nullable<Double>, not Int32.
  numericFields <- c(
    "numberOfIndividuals",
    "proportionOfFemales",
    "weightMin",
    "weightMax",
    "heightMin",
    "heightMax",
    "ageMin",
    "ageMax",
    "BMIMin",
    "BMIMax",
    "gestationalAgeMin",
    "gestationalAgeMax"
  )
  for (field in numericFields) {
    if (!is.null(popArgs[[field]])) {
      popArgs[[field]] <- as.double(popArgs[[field]])
    }
  }
  popResult <- do.call(ospsuite::createPopulationCharacteristics, popArgs)
  createPopulation(populationCharacteristics = popResult)$population
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
  simulationRunOptions,
  stopIfParameterNotFound = TRUE
) {
  # 1. Load simulation. An absolute `modelFile` is used as-is; a relative one
  # is resolved against the project's simulations folder, which must exist for
  # the join to be meaningful (`file.path(NULL, x)` yields `character(0)`). A
  # relative value must stay under the simulations folder, so a shared project
  # cannot point `modelFile` at a file outside itself via `../`.
  if (fs::is_absolute_path(scenario$modelFile)) {
    modelFilePath <- scenario$modelFile
  } else if (!is.null(project$paths$simulationsFolder)) {
    modelFilePath <- .resolveProjectPath(
      scenario$modelFile,
      project$paths$simulationsFolder,
      "modelFile"
    )
  } else {
    cli::cli_abort(messages$noSimulationsFolderForRelativeModelFile(
      scenarioName = scenario$scenarioName,
      modelFile = scenario$modelFile
    ))
  }
  simulation <- ospsuite::loadSimulation(
    filePath = modelFilePath,
    loadFromCache = FALSE
  )
  simulation$name <- scenario$scenarioName

  # 2. Build merged parameter structure
  params <- .mergeScenarioParameters(scenario, project, customParams)

  # 2a. Build merged initial-condition (molecule start value) structure
  initialConditions <- .mergeScenarioInitialConditions(scenario, project)

  # 2b. IndividualCharacteristics
  individualCharacteristics <- NULL
  if (!is.null(scenario$individualId) && !is.na(scenario$individualId)) {
    indivData <- project$definitions$individuals[[scenario$individualId]]
    if (is.null(indivData)) {
      cli::cli_warn(messages$noIndividualCharacteristics(
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
      cli::cli_abort(messages$scenarioMissingTimeUnit(
        scenario$scenarioName
      ))
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
    additionalInitialConditions = initialConditions,
    stopIfParameterNotFound = stopIfParameterNotFound
  )

  # 6. Population
  population <- NULL
  if (scenario$simulationType == "Population") {
    population <- .resolveScenarioPopulation(scenario, project, cache)
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

# .scenarioBuildPreflight ----

# Shared entry guard for `.runScenariosFromProject` / `.buildSimulationsFromProject`:
# validate `project` and `customParams`, resolve `simulationRunOptions` (an
# explicit argument wins; otherwise fall back to the project-level
# `defaultSimulationRunOptions`, leaving NULL = package defaults), and, when
# `validate`, run the section validators the scenario build depends on. Returns
# the resolved `simulationRunOptions`. `opName` names the calling entrypoint in
# any validation abort.
# @keywords internal
# @noRd
.scenarioBuildPreflight <- function(
  project,
  customParams,
  simulationRunOptions,
  validate,
  opName
) {
  validateIsOfType(project, "Project")
  .validateParametersStructure(
    parameterStructure = customParams,
    argumentName = "customParams",
    nullAllowed = TRUE
  )
  if (is.null(simulationRunOptions)) {
    simulationRunOptions <- .buildSimulationRunOptions(
      project$defaultSimulationRunOptions
    )
  }
  if (isTRUE(validate)) {
    project$ensureValid(
      sections = c(
        "outputPaths",
        "scenarios",
        "individuals",
        "populations",
        "applications",
        "parameterSets",
        "crossReferences"
      ),
      opName = opName
    )
  }
  simulationRunOptions
}

# .buildScenarioSimulations ----

# Resolve the requested scenario names and build (but do not run) each one's
# simulation. One run-scoped cache of `IndividualCharacteristics` / `Population`
# objects is shared across the batch, so two scenarios that reference the same
# individual or population build it once. `scenarioNames = NULL` selects every
# scenario; an unknown name aborts. Returns `list(scenarioNames, prepared)`
# where `prepared` is a named list (keyed by scenario name) of
# `.prepareScenario()`'s `list(simulation, population)` return.
# @keywords internal
# @noRd
.buildScenarioSimulations <- function(
  project,
  scenarioNames = NULL,
  customParams = NULL,
  simulationRunOptions = NULL,
  stopIfParameterNotFound = TRUE
) {
  allScenarios <- project$definitions$scenarios
  if (is.null(scenarioNames)) {
    scenarioNames <- names(allScenarios)
  }
  unknownNames <- setdiff(scenarioNames, names(allScenarios))
  if (length(unknownNames) > 0) {
    cli::cli_abort(messages$unknownScenarioNames(unknownNames))
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
      simulationRunOptions = simulationRunOptions,
      stopIfParameterNotFound = stopIfParameterNotFound
    )
    names(prepared)[[idx]] <- name
  }
  list(scenarioNames = scenarioNames, prepared = prepared)
}

# .collectScenarioResult ----

# Resolve output quantities and build the standard return list for one
# scenario. When a scenario produced no results, `stopIfFails` decides whether
# that aborts the run (the default) or only warns and leaves `outputValues`
# NULL.
# @keywords internal
# @noRd
.collectScenarioResult <- function(
  scenario,
  simulation,
  results,
  population,
  stopIfFails = TRUE
) {
  outputQuantities <- NULL
  if (!is.null(scenario$outputPaths)) {
    outputQuantities <- getAllQuantitiesMatching(
      unname(scenario$outputPaths),
      simulation
    )
  }
  outputValues <- NULL
  if (is.null(results)) {
    if (isTRUE(stopIfFails)) {
      cli::cli_abort(messages$missingResultsForScenario(scenario$scenarioName))
    }
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
      cli::cli_abort(messages$wrongOntogenyStructure(parts[[i]]))
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

# @keywords internal
# @noRd
.runScenariosFromProject <- function(
  project,
  scenarioNames = NULL,
  customParams = NULL,
  simulationRunOptions = NULL,
  validate = TRUE,
  stopIfParameterNotFound = TRUE,
  stopIfFails = TRUE
) {
  simulationRunOptions <- .scenarioBuildPreflight(
    project = project,
    customParams = customParams,
    simulationRunOptions = simulationRunOptions,
    validate = validate,
    opName = "runScenarios"
  )

  built <- .buildScenarioSimulations(
    project = project,
    scenarioNames = scenarioNames,
    customParams = customParams,
    simulationRunOptions = simulationRunOptions,
    stopIfParameterNotFound = stopIfParameterNotFound
  )
  scenarioNames <- built$scenarioNames
  prepared <- built$prepared
  # Still needed below to hand each scenario record to `.collectScenarioResult`.
  allScenarios <- project$definitions$scenarios

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
      population = p$population,
      stopIfFails = stopIfFails
    )
    names(out)[[idx]] <- name
  }
  out
}

# .buildSimulationsFromProject ----

# Build (but do not run) the simulations for the requested scenarios. Runs the
# same validation preflight and run-option fallback as `.runScenariosFromProject`
# so a built simulation resolves exactly the same project state a run would, then
# returns `.buildScenarioSimulations()`'s per-scenario `list(simulation,
# population)` map directly. Nothing is simulated, so there is no `stopIfFails`.
# @keywords internal
# @noRd
.buildSimulationsFromProject <- function(
  project,
  scenarioNames = NULL,
  customParams = NULL,
  simulationRunOptions = NULL,
  validate = TRUE,
  stopIfParameterNotFound = TRUE
) {
  simulationRunOptions <- .scenarioBuildPreflight(
    project = project,
    customParams = customParams,
    simulationRunOptions = simulationRunOptions,
    validate = validate,
    opName = "buildSimulations"
  )

  built <- .buildScenarioSimulations(
    project = project,
    scenarioNames = scenarioNames,
    customParams = customParams,
    simulationRunOptions = simulationRunOptions,
    stopIfParameterNotFound = stopIfParameterNotFound
  )
  built$prepared
}
