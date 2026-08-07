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

# Four-layer merge ----

# Pure function. Builds a `list(paths, values, units)` parameter
# structure (or `NULL`) for one scenario. Layers, in order
# (last-write-wins): scenario `modelParameterSets` -> individual
# `parameterSets` -> application `parameterSets` -> caller-supplied
# `customParams`. Each of those reference fields is a
# list of set ids, iterated in listed order; every id is looked up in
# the project's single `parameterSets` section. Unknown ids are silently
# skipped (consistent across all three layers).
#
# Every layer here is authored by the user, which is what lets
# `initializeSimulation()` apply the merged result strictly: a path the user
# wrote that the model does not have is a mistake in their project and must
# abort. The bundled species defaults are deliberately NOT a layer: they are a
# package-shipped superset covering every model of a species, so a path they
# carry that this particular model lacks is normal, not a user error.
# `initializeSimulation()` therefore applies them separately and tolerantly,
# before the merged user layers, which also keeps them overridable by all four.
# @keywords internal
# @noRd
.mergeScenarioParameters <- function(scenario, project, customParams = NULL) {
  params <- NULL
  # Read the unified parameter-sets section once; the three layers below all
  # resolve their set ids against it.
  parameterSets <- project$definitions$parameterSets

  # 1. modelParameterSets
  params <- .extendWithParameterSets(
    params,
    scenario$modelParameterSets,
    parameterSets
  )

  # 2. individual parameterSets
  if (!is.null(scenario$individualId) && !is.na(scenario$individualId)) {
    indivData <- project$definitions$individuals[[scenario$individualId]]
    params <- .extendWithParameterSets(
      params,
      unlist(indivData$parameterSets),
      parameterSets
    )
  }

  # 3. application parameterSets
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
    params <- .extendWithParameterSets(
      params,
      unlist(appData$parameterSets),
      parameterSets
    )
  }

  # 4. customParams
  if (!is.null(customParams)) {
    params <- extendParameterStructure(
      parameters = params,
      newParameters = customParams
    )
  }

  params
}

# Fold each named parameter set into `params`, skipping a set id that resolves
# to nothing. The three layers a scenario draws sets from (its own
# `modelParameterSets`, its individual's, and its application protocol's) merge
# the same way, each overriding the one before it.
#
# @keywords internal
# @noRd
.extendWithParameterSets <- function(params, setIds, parameterSets) {
  for (setId in setIds) {
    setParams <- .parameterSetToStructure(parameterSets[[setId]])
    if (!is.null(setParams)) {
      params <- extendParameterStructure(
        parameters = params,
        newParameters = setParams
      )
    }
  }
  params
}

# Population resolution ----

# Resolve the `ospsuite::Population` for a population scenario, dispatching on
# the population *entry's* `type`, not the scenario flag. A `programmatic` entry
# comes from the runtime store; a `csv` entry loads its `file`; an entry with no
# `type` is a demographics spec built via `createPopulationCharacteristics`,
# unless the scenario's `readPopulationFromCSV` flag is set (back-compat: that
# still loads `<populationId>.csv`). Resolved objects are cached per run, keyed
# on the source they were resolved from, so a population two scenarios resolve
# the same way is built or loaded once.
# @keywords internal
# @noRd
.resolveScenarioPopulation <- function(scenario, project, cache) {
  if (is.null(scenario$populationId)) {
    cli::cli_abort(messages$noPopulationIdForPopulationScenario(
      scenario$scenarioName
    ))
  }

  popData <- project$definitions$populations[[scenario$populationId]]
  # The entry type wins over the scenario flag; a spec entry falls back to the
  # scenario's `readPopulationFromCSV` for the legacy CSV path.
  effectiveType <- popData$type %||%
    (if (isTRUE(scenario$readPopulationFromCSV)) "csv" else "spec")

  # The key holds everything that decides which population an id resolves to,
  # not the id alone: the effective type can come from the *scenario*, so one id
  # resolves to a spec-built population for a scenario without
  # `readPopulationFromCSV` and to the csv table for a scenario with it, and both
  # have to coexist in one batch. `\r` cannot occur in a canonicalized id, which
  # substitutes every control and space character.
  key <- paste(
    scenario$populationId,
    effectiveType,
    popData$file %||% "",
    sep = "\r"
  )
  cached <- cache$populations[[key]]
  if (!is.null(cached)) {
    return(cached)
  }
  # This id already stands for another population in this batch. Each scenario
  # still gets the source it names, but one id meaning two populations is almost
  # always a project mistake (typically `readPopulationFromCSV` set on one
  # scenario and not on another that shares the population), so say so.
  seenKeys <- names(cache$populations) %||% character()
  if (any(startsWith(seenKeys, paste0(scenario$populationId, "\r")))) {
    cli::cli_warn(messages$populationIdResolvedTwoWays(
      populationId = scenario$populationId,
      scenarioName = scenario$scenarioName,
      effectiveType = effectiveType
    ))
  }

  population <- switch(
    effectiveType,
    # A `programmatic` sentinel with no backing object (a project reloaded
    # without re-injecting) is fatal: a population scenario cannot run without
    # one.
    "programmatic" = project$getProgrammaticPopulation(
      scenario$populationId
    ) %||%
      cli::cli_abort(messages$populationProgrammaticUnresolved(
        id = scenario$populationId,
        scenarioName = scenario$scenarioName
      )),
    "csv" = .resolveCsvPopulation(scenario, project, popData),
    "spec" = .resolveSpecPopulation(scenario, popData)
  )
  cache$populations[[key]] <- population
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
  fileName <- .populationCsvFileName(
    scenario$populationId,
    project$paths$populationsFolder,
    popData$file
  )
  populationPath <- .resolveProjectPath(
    fileName,
    project$paths$populationsFolder,
    "populationId"
  )
  # Report an absent file here rather than handing a nonexistent path to the
  # backend, which fails with a raw .NET exception naming neither the scenario
  # nor the folder the file is expected in.
  if (!file.exists(populationPath)) {
    cli::cli_abort(messages$populationCsvNotFound(
      scenarioName = scenario$scenarioName,
      populationId = scenario$populationId,
      fileName = fileName,
      populationsFolder = project$paths$populationsFolder
    ))
  }
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
  .assertNotLfsPointer(modelFilePath)
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
        # weight/height/age are stored as doubles (or absent). Pass them through
        # as-is: an absent biometric stays NULL, which
        # `createIndividualCharacteristics()` defaults. Coercing with
        # `as.double()` would turn a NULL into `numeric(0)`, which defeats
        # ospsuite's own `is.null()` guard and crashes an animal individual that
        # legitimately carries only a weight.
        individualCharacteristics <- ospsuite::createIndividualCharacteristics(
          species = indivData$species,
          population = indivData$population,
          gender = indivData$gender,
          weight = indivData$weight,
          height = indivData$height,
          age = indivData$age,
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
  .applyScenarioSteadyState(simulation, scenario, simulationRunOptions)

  list(simulation = simulation, population = population)
}

# .applyScenarioSteadyState ----

# Bring a scenario's simulation to steady state, when it asks for one: solve for
# the steady-state quantity values at the scenario's `steadyStateTime` and write
# them back as the simulation's start values. A scenario with
# `simulateSteadyState` off is a no-op.
#
# `overwriteFormulasInSS` is the scenario's own spelling of the inverse of
# `ospsuite`'s `ignoreIfFormula`: a quantity whose start value is a formula
# keeps that formula unless the scenario opts into overwriting it.
#
# Mutates `simulation` in place (an `ospsuite::Simulation` is a reference
# object), like the other steps of `.prepareScenario()`, and returns it
# invisibly.
#
# @keywords internal
# @noRd
.applyScenarioSteadyState <- function(
  simulation,
  scenario,
  simulationRunOptions
) {
  if (!scenario$simulateSteadyState) {
    return(invisible(simulation))
  }
  initialValues <- ospsuite::getSteadyState(
    simulations = list(simulation),
    steadyStateTime = list(scenario$steadyStateTime),
    ignoreIfFormula = !scenario$overwriteFormulasInSS,
    simulationRunOptions = simulationRunOptions
  )
  ospsuite::setQuantityValuesByPath(
    quantityPaths = initialValues[[simulation$id]]$paths,
    values = initialValues[[simulation$id]]$values,
    simulation = simulation
  )
  invisible(simulation)
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
  opName,
  .call = rlang::caller_env()
) {
  rlang::local_error_call(.call)
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
        "initialConditions",
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
#
# `canSkip` says whether the calling entrypoint offers `stopIfFails`, so a build
# failure only points at it where it exists (`buildSimulations()` has none).
# @keywords internal
# @noRd
.buildScenarioSimulations <- function(
  project,
  scenarioNames = NULL,
  customParams = NULL,
  simulationRunOptions = NULL,
  stopIfParameterNotFound = TRUE,
  stopIfFails = TRUE,
  canSkip = TRUE,
  .call = rlang::caller_env()
) {
  rlang::local_error_call(.call)
  allScenarios <- project$definitions$scenarios
  if (is.null(scenarioNames)) {
    scenarioNames <- names(allScenarios)
  } else {
    # Match against the canonical ids scenarios were filed under, so a caller
    # can pass the name they authored with. This is the full id
    # canonicalization (case-folding, character substitution, trimming), the
    # same transform the scenario was filed under, so the reference resolves to
    # it. The "canonicalized to a safe form" warning is left in place on
    # purpose: when the passed name is rewritten it names the resolved id, so a
    # mistyped label that lands on a different real scenario is surfaced rather
    # than run silently.
    scenarioNames <- .canonicalizeIdRef(scenarioNames)
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
    names(prepared)[[idx]] <- name
    # A build-time failure of one scenario (e.g. a missing model parameter path)
    # aborts the whole batch by default. When `stopIfFails = FALSE`, surface it
    # as a warning and leave this scenario's entry NULL so the run continues
    # with the scenarios that built; the run loops below skip a NULL entry and
    # `.collectScenarioResult()` records it as producing no results.
    #
    # Either way the failure is reported as belonging to this scenario. A build
    # error can come from the .NET backend, whose own message names neither the
    # scenario nor the file (loading a `.pkml` that is not there, or is not a
    # model), so re-raising it as-is left nothing to act on in a batch of
    # scenarios: the abort now carries the scenario's name and the backend
    # message as its cause.
    result <- tryCatch(
      .prepareScenario(
        scenario = allScenarios[[name]],
        project = project,
        customParams = customParams,
        cache = cache,
        simulationRunOptions = simulationRunOptions,
        stopIfParameterNotFound = stopIfParameterNotFound
      ),
      error = function(e) {
        if (isTRUE(stopIfFails)) {
          # Attribute explicitly: this runs in the handler closure, so the
          # `local_error_call()` installed in the enclosing frame does not reach
          # it.
          cli::cli_abort(
            messages$scenarioBuildFailedAbort(
              scenarioName = name,
              canSkip = canSkip
            ),
            parent = e,
            call = .call
          )
        }
        cli::cli_warn(messages$scenarioBuildFailed(
          scenarioName = name,
          conditionMessage = conditionMessage(e)
        ))
        NULL
      }
    )
    # Single-bracket assignment keeps a skipped scenario's slot as an explicit
    # NULL; `prepared[[idx]] <- NULL` would instead delete the element and
    # shorten the list, breaking the positional loops below.
    prepared[idx] <- list(result)
  }
  list(scenarioNames = scenarioNames, prepared = prepared)
}

# .collectScenarioResult ----

# Resolve output quantities and build the standard return list for one
# scenario. When a scenario produced no results, `stopIfFails` decides whether
# that aborts the run (the default) or only warns and leaves `outputValues`
# NULL. A scenario skipped at build time arrives with a NULL `simulation` and
# has already warned, so it is recorded without a second warning.
# @keywords internal
# @noRd
.collectScenarioResult <- function(
  scenario,
  simulation,
  results,
  population,
  stopIfFails = TRUE,
  .call = rlang::caller_env()
) {
  rlang::local_error_call(.call)
  outputQuantities <- NULL
  # `simulation` is NULL for a scenario skipped at build time (stopIfFails =
  # FALSE); there is nothing to resolve output quantities against, so leave
  # them NULL and let the no-results branch below record the skip.
  if (!is.null(simulation) && !is.null(scenario$outputPaths)) {
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
    # A scenario skipped at build time (NULL simulation) already warned via
    # `scenarioBuildFailed()`; don't warn a second time for the same event. A
    # scenario that built but produced no results still warns here.
    if (!is.null(simulation)) {
      cli::cli_warn(messages$missingResultsForScenario(scenario$scenarioName))
    }
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

# Build the `MoleculeOntogeny` objects of one individual or population from its
# `proteinOntogenies` field. Every shape the field takes is accepted (a character
# vector of one `"Molecule:Ontogeny"` entry per ontogeny, a single comma-joined
# cell, or the list a JSON array parses to), so the value is flattened by
# `.splitProteinOntogenies()` before anything is asked of it: a scalar test such
# as `is.na()` on a two-entry field aborts on the length alone, before a single
# ontogeny is read. Returns NULL when nothing is specified.
# @keywords internal
# @noRd
.readOntogeniesFromList <- function(proteinOntogenies) {
  parts <- .splitProteinOntogenies(proteinOntogenies)
  if (length(parts) == 0L) {
    return(NULL)
  }
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
  stopIfFails = TRUE,
  .call = rlang::caller_env()
) {
  rlang::local_error_call(.call)
  simulationRunOptions <- .scenarioBuildPreflight(
    project = project,
    customParams = customParams,
    simulationRunOptions = simulationRunOptions,
    validate = validate,
    opName = "runScenarios",
    .call = .call
  )

  built <- .buildScenarioSimulations(
    project = project,
    scenarioNames = scenarioNames,
    customParams = customParams,
    simulationRunOptions = simulationRunOptions,
    stopIfParameterNotFound = stopIfParameterNotFound,
    stopIfFails = stopIfFails,
    .call = .call
  )
  scenarioNames <- built$scenarioNames
  prepared <- built$prepared
  # Still needed below to hand each scenario record to `.collectScenarioResult`.
  allScenarios <- project$definitions$scenarios

  # A NULL `prepared` entry is a scenario skipped at build time (only reachable
  # under `stopIfFails = FALSE`); it has no simulation to run and is collected
  # below as producing no results.
  individualSimulations <- list()
  for (idx in seq_along(scenarioNames)) {
    p <- prepared[[idx]]
    if (!is.null(p) && is.null(p$population)) {
      individualSimulations <- c(individualSimulations, p$simulation)
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
    if (!is.null(p) && !is.null(p$population)) {
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
      results = if (is.null(p)) NULL else simulationResults[[p$simulation$id]],
      population = p$population,
      stopIfFails = stopIfFails,
      .call = .call
    )
    names(out)[[idx]] <- name
  }

  # Every scenario gets an entry whatever happened to it, so close a
  # `stopIfFails = FALSE` run by naming the ones that produced none in one
  # place: the per-scenario warnings above are easy to lose in a large batch,
  # and this is what a caller needs to know before reaching into the results.
  skipped <- names(out)[vapply(out, function(r) is.null(r$results), logical(1))]
  if (length(skipped) > 0L) {
    cli::cli_warn(messages$scenariosSkipped(skipped))
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
  stopIfParameterNotFound = TRUE,
  .call = rlang::caller_env()
) {
  rlang::local_error_call(.call)
  simulationRunOptions <- .scenarioBuildPreflight(
    project = project,
    customParams = customParams,
    simulationRunOptions = simulationRunOptions,
    validate = validate,
    opName = "buildSimulations",
    .call = .call
  )

  built <- .buildScenarioSimulations(
    project = project,
    scenarioNames = scenarioNames,
    customParams = customParams,
    simulationRunOptions = simulationRunOptions,
    stopIfParameterNotFound = stopIfParameterNotFound,
    canSkip = FALSE,
    .call = .call
  )
  built$prepared
}
