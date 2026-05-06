# Section validation adapters ----
#
# Registered in `.validationAdapters` (R/validation.R) and called by
# `.runProjectValidation()`. Each pulls the right slice of the project
# and delegates to a section-local `.validate*` helper. They only run
# when `validateProject()` (or a targeted `.ensureValid`) is called;
# they do not run during parsing or simulation.

#' @keywords internal
#' @noRd
.outputPathsValidatorAdapter <- function(project) {
  .validateOutputPaths(project$outputPaths)
}

#' @keywords internal
#' @noRd
.scenariosValidatorAdapter <- function(project) {
  .validateScenarios(project$scenarios)
}

#' @keywords internal
#' @noRd
.applicationsValidatorAdapter <- function(project) {
  .validateApplications(project$applications)
}

#' Validate the `outputPaths` section of a Project
#'
#' Checks for duplicate ids, empty literal paths, and warns when two ids
#' map to the same literal path (the round-trip lossiness flagged in the
#' Chapter 2 PR).
#'
#' @param outputPaths Named character vector / list from
#'   `project$outputPaths`.
#' @return validationResult.
#' @keywords internal
#' @noRd
.validateOutputPaths <- function(outputPaths) {
  result <- validationResult$new()

  if (is.null(outputPaths) || length(outputPaths) == 0) {
    result$add_warning("Data", "No output paths defined")
    return(result)
  }

  result <- .check_no_duplicates(names(outputPaths), "outputPathId", result)

  values <- unlist(outputPaths, use.names = FALSE)
  emptyIds <- names(outputPaths)[is.na(values) | values == ""]
  if (length(emptyIds) > 0) {
    result$add_critical_error(
      "Missing Fields",
      paste0(
        "Empty output path values for IDs: ",
        paste(emptyIds, collapse = ", ")
      )
    )
  }

  dupeValues <- values[duplicated(values) & !is.na(values)]
  if (length(dupeValues) > 0) {
    result$add_warning(
      "Uniqueness",
      paste0(
        "Multiple IDs point to the same output path: ",
        paste(unique(dupeValues), collapse = ", ")
      )
    )
  }

  result
}

#' Validate the `scenarios` section of a Project
#'
#' Per-entry checks: `modelFile` is set and non-empty,
#' `simulationType` is one of the supported values, and
#' population-typed scenarios declare a `populationId`.
#'
#' Cross-section reference checks (individualId, modelParameterSets,
#' applicationProtocol, …) live in `.validateCrossReferences()`.
#'
#' @param scenarios Named list of `Scenario` objects from
#'   `project$scenarios`.
#' @return validationResult.
#' @keywords internal
#' @noRd
.validateScenarios <- function(scenarios) {
  result <- validationResult$new()

  if (is.null(scenarios) || length(scenarios) == 0) {
    result$add_warning("Data", "No scenarios defined")
    return(result)
  }

  for (name in names(scenarios)) {
    sc <- scenarios[[name]]

    if (is.null(sc$modelFile) || sc$modelFile == "") {
      result$add_critical_error(
        "Missing Fields",
        paste0("Scenario '", name, "' has no modelFile")
      )
    }

    simType <- sc$simulationType %||% ""
    if (!simType %in% c("Individual", "Population")) {
      result$add_critical_error(
        "Validation",
        paste0(
          "Scenario '",
          name,
          "' has invalid simulationType '",
          simType,
          "'"
        )
      )
    }

    if (
      simType == "Population" &&
        (is.null(sc$populationId) || sc$populationId == "")
    ) {
      result$add_critical_error(
        "Missing Fields",
        paste0("Population scenario '", name, "' has no populationId")
      )
    }
  }

  result
}

#' Validate the `applications` section of a Project
#'
#' The applications section is currently a thin wrapper around its
#' `parameterSets` references, all of which are checked in
#' `.validateCrossReferences()`. This adapter exists so that the
#' canonical section list still resolves to a working validator (and so
#' that future shape checks have an obvious home).
#'
#' @param applications Named list from `project$applications`.
#' @return validationResult.
#' @keywords internal
#' @noRd
.validateApplications <- function(applications) {
  result <- validationResult$new()
  if (is.null(applications) || length(applications) == 0) {
    result$add_warning("Data", "No applications defined")
  }
  result
}

# Legacy v6 runtime path: runs scenarios that were created via
# `createScenarios()` from `ScenarioConfiguration` / Excel. Soft-
# deprecated via `runScenarios()`'s dispatcher in this file. Body is
# a verbatim move of the v6 `runScenarios()` logic — Chapter 8 deletes.
# @keywords internal
# @noRd
.runLegacyScenarios <- function(scenarios, simulationRunOptions = NULL) {
  scenarios <- ospsuite.utils::toList(scenarios)
  individualSimulations <- list()
  populationScenarios <- list()
  steadyStateGroups <- list()
  for (scenario in scenarios) {
    if (scenario$scenarioType == "Individual") {
      individualSimulations <- c(individualSimulations, scenario$simulation)
    } else {
      populationScenarios <- c(populationScenarios, scenario)
    }
    if (scenario$scenarioConfiguration$simulateSteadyState) {
      ignoreIfFormulaKey <- as.character(
        scenario$scenarioConfiguration$overwriteFormulasInSS
      )
      if (is.null(steadyStateGroups[[ignoreIfFormulaKey]])) {
        steadyStateGroups[[ignoreIfFormulaKey]] <- list(
          simulations = list(),
          times = list()
        )
      }
      steadyStateGroups[[ignoreIfFormulaKey]]$simulations <- c(
        steadyStateGroups[[ignoreIfFormulaKey]]$simulations,
        scenario$simulation
      )
      steadyStateGroups[[ignoreIfFormulaKey]]$times <- c(
        steadyStateGroups[[ignoreIfFormulaKey]]$times,
        scenario$scenarioConfiguration$steadyStateTime
      )
    }
  }
  initialValues <- list()
  for (ignoreIfFormulaKey in names(steadyStateGroups)) {
    group <- steadyStateGroups[[ignoreIfFormulaKey]]
    ignoreIfFormula <- !as.logical(ignoreIfFormulaKey)
    groupValues <- ospsuite::getSteadyState(
      simulations = group$simulations,
      steadyStateTime = group$times,
      ignoreIfFormula = ignoreIfFormula,
      simulationRunOptions = simulationRunOptions
    )
    initialValues <- c(initialValues, groupValues)
  }
  for (ignoreIfFormulaKey in names(steadyStateGroups)) {
    for (simulation in steadyStateGroups[[ignoreIfFormulaKey]]$simulations) {
      ospsuite::setQuantityValuesByPath(
        quantityPaths = initialValues[[simulation$id]]$paths,
        values = initialValues[[simulation$id]]$values,
        simulation = simulation
      )
    }
  }
  simulationResults <- runSimulations(
    simulations = individualSimulations,
    simulationRunOptions = simulationRunOptions
  )
  for (scenario in populationScenarios) {
    populationResults <- runSimulations(
      simulations = scenario$simulation,
      population = scenario$population,
      simulationRunOptions = simulationRunOptions
    )
    simulationResults <- c(simulationResults, populationResults)
  }
  returnList <- vector("list", length(simulationResults))
  for (idx in seq_along(scenarios)) {
    scenario <- scenarios[[idx]]
    scenarioName <- scenario$scenarioConfiguration$scenarioName
    simulation <- scenario$simulation
    id <- simulation$id
    results <- simulationResults[[id]]
    population <- scenario$population
    if (
      !is.null(population) &&
        !isOfType(population, "Population") &&
        is.na(population)
    ) {
      population <- NULL
    }
    outputQuantities <- NULL
    if (!is.null(scenario$scenarioConfiguration$outputPaths)) {
      outputQuantities <- getAllQuantitiesMatching(
        scenario$scenarioConfiguration$outputPaths,
        simulation
      )
    }
    if (is.null(results)) {
      warning(messages$missingResultsForScenario(scenarioName))
      outputValues <- NULL
    } else {
      outputValues <- getOutputValues(
        results,
        quantitiesOrPaths = outputQuantities,
        population = population,
        addMetaData = FALSE
      )
    }
    returnList[[idx]] <- list(
      simulation = simulation,
      results = results,
      outputValues = outputValues,
      population = population
    )
    names(returnList)[[idx]] <- scenarioName
  }
  returnList
}

#' Run a set of scenarios from a `Project`.
#'
#' @description Loads simulations, applies parameters, runs the
#'   simulations, and collects results for one or more scenarios
#'   defined on a parsed [Project]. The project must already have been
#'   loaded with [loadProject()].
#'
#' @param project A [Project] object loaded from a `Project.json` file.
#'   In the legacy positional form, this argument also accepts a list
#'   of [LegacyScenario] objects produced by [createScenarios()] — see
#'   "Details".
#' @param scenarioNames Optional character vector of scenario names to
#'   run. `NULL` (default) runs all scenarios in the project.
#' @param customParams A list with vectors `paths`, `values`, and
#'   `units` — applied to every selected scenario as the final
#'   parameter layer.
#' @param simulationRunOptions Optional [ospsuite::SimulationRunOptions]
#'   for the simulation run. `NULL` (default) uses the package
#'   defaults.
#' @param validate Logical. If `TRUE` (default), runs the relevant
#'   section validators via [validateProject()] before simulating and
#'   aborts with a formatted summary on critical errors. Set to
#'   `FALSE` to skip the pre-flight check (e.g. when the caller has
#'   already validated the project).
#' @param scenarios `r lifecycle::badge("deprecated")` Legacy alias
#'   for the first positional argument when passing a list of
#'   [LegacyScenario] objects. Use `project = loadProject(...)` and
#'   the modern signature instead.
#'
#' @returns A named list keyed by scenario name. Each entry is a list
#'   with `simulation` (the initialized [ospsuite::Simulation]),
#'   `results` ([ospsuite::SimulationResults]), `outputValues` (the
#'   computed output values, or `NULL` if simulation failed), and
#'   `population` (an [ospsuite::Population] for population
#'   scenarios, or `NULL` for individual scenarios).
#'
#' @details If a scenario's simulation fails, a warning is produced
#'   and `outputValues` for that scenario is `NULL`.
#'
#'   The legacy signatures
#'   `runScenarios(scenariosList, simulationRunOptions)` (positional)
#'   and `runScenarios(scenarios = ..., simulationRunOptions = ...)`
#'   (named) — taking a list of [LegacyScenario] objects produced by
#'   [createScenarios()] — are still accepted but soft-deprecated.
#'   New code should use the JSON-first form shown above.
#'
#' @export
runScenarios <- function(
  project,
  scenarioNames = NULL,
  customParams = NULL,
  simulationRunOptions = NULL,
  validate = TRUE,
  scenarios = lifecycle::deprecated()
) {
  # Legacy named-arg form.
  if (lifecycle::is_present(scenarios)) {
    lifecycle::deprecate_soft(
      when = "6.0.0",
      what = "runScenarios(scenarios = )",
      with = "runScenarios(project = )",
      details = paste(
        "Pass a Project loaded via loadProject() instead of a list of",
        "LegacyScenario objects produced by createScenarios()."
      )
    )
    return(.runLegacyScenarios(
      scenarios = scenarios,
      simulationRunOptions = simulationRunOptions
    ))
  }

  # Modern path.
  if (inherits(project, "Project")) {
    return(.runScenariosFromProject(
      project = project,
      scenarioNames = scenarioNames,
      customParams = customParams,
      simulationRunOptions = simulationRunOptions,
      validate = validate
    ))
  }

  # Legacy positional form: first arg is a scenarios list, second is
  # simulationRunOptions (so it landed in `scenarioNames` due to the
  # modern signature's positional names).
  lifecycle::deprecate_soft(
    when = "6.0.0",
    what = I("runScenarios(scenariosList)"),
    with = "runScenarios(project)",
    details = paste(
      "Pass a Project loaded via loadProject() instead of a list of",
      "LegacyScenario objects produced by createScenarios()."
    )
  )
  legacySimRunOpts <- simulationRunOptions %||% scenarioNames
  .runLegacyScenarios(
    scenarios = project,
    simulationRunOptions = legacySimRunOpts
  )
}

#' Create `Scenario` objects from `ScenarioConfiguration` objects
#'
#' @description Load simulation. Apply parameters from global XLS. Apply
#' individual physiology. Apply individual model parameters. Set simulation
#' outputs. Set simulation time. initializeSimulation(). Create population
#'
#' @param scenarioConfigurations List of `ScenarioConfiguration` objects to be
#'   simulated. See [createScenarios()] for details.
#' @param customParams A list containing vectors 'paths' with the full paths to
#'   the parameters, 'values' the values of the parameters, and 'units' with the
#'   units the values are in. The values will be applied to all scenarios.
#' @param stopIfParameterNotFound Boolean. If `TRUE` (default) and a custom
#'   parameter is not found, an error is thrown. If `FALSE`, non-existing
#'   parameters are ignored.
#'
#' @returns Named list of `Scenario` objects.
#' @export
createScenarios <- function(
  scenarioConfigurations,
  customParams = NULL,
  stopIfParameterNotFound = TRUE
) {
  .validateScenarioConfigurations(scenarioConfigurations)
  .validateParametersStructure(
    parameterStructure = customParams,
    argumentName = "customParams",
    nullAllowed = TRUE
  )

  scenarios <- purrr::map(
    scenarioConfigurations,
    ~ LegacyScenario$new(
      .x,
      customParams = customParams,
      stopIfParameterNotFound = stopIfParameterNotFound
    )
  ) |>
    purrr::set_names(purrr::map(scenarioConfigurations, ~ .x$scenarioName))

  return(scenarios)
}

#' Save results of scenario simulations to csv.
#'
#' @param simulatedScenariosResults Named list with `simulation`, `results`, `outputValues`,
#' and `population` as produced by `runScenarios()`.
#' @param projectConfiguration An instance of `ProjectConfiguration`
#' @param outputFolder Optional - path to the folder where the results will be
#' stored. If `NULL` (default), a sub-folder in
#' `ProjectConfiguration$outputFolder/SimulationResults/<DateSuffix>`.
#' @param saveSimulationsToPKML If `TRUE` (default), simulations corresponding to
#' the results are saved to PKML along with the results.
#'
#' @details For each scenario, a separate csv file will be created. If the scenario
#' is a population simulation, a population is stored along with the results with
#' the file name suffix `_population`. Results can be read with the `loadScenarioResults()` function.
#'
#' @export
#'
#' @returns `outputFolder` or the created output folder path, if no `outputFolder` was provided.
#'
#' @examples \dontrun{
#' projectConfiguration <- esqlabsR::createProjectConfiguration()
#' scenarioConfigurations <- readScenarioConfigurationFromExcel(
#'   projectConfiguration = projectConfiguration
#' )
#' scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations)
#' simulatedScenariosResults <- runScenarios(
#'   scenarios = scenarios
#' )
#' saveScenarioResults(simulatedScenariosResults, projectConfiguration)
#' }
saveScenarioResults <- function(
  simulatedScenariosResults,
  projectConfiguration,
  outputFolder = NULL,
  saveSimulationsToPKML = TRUE
) {
  validateIsLogical(saveSimulationsToPKML)

  outputFolder <- outputFolder %||%
    file.path(
      projectConfiguration$outputFolder,
      "SimulationResults",
      format(Sys.time(), "%F %H-%M")
    )

  for (i in seq_along(simulatedScenariosResults)) {
    results <- simulatedScenariosResults[[i]]$results
    scenarioName <- names(simulatedScenariosResults)[[i]]

    # Replace "\" and "/" by "_" so the file name does not result in folders
    scenarioName <- gsub("[\\\\/]", "_", scenarioName)

    outputPath <- file.path(outputFolder, paste0(scenarioName, ".csv"))
    tryCatch(
      {
        # Create a new folder if it does not exist
        if (!dir.exists(paths = outputFolder)) {
          dir.create(path = outputFolder, recursive = TRUE)
        }
        # Save simulations
        if (saveSimulationsToPKML) {
          outputPathSim <- file.path(
            outputFolder,
            paste0(scenarioName, ".pkml")
          )
          ospsuite::saveSimulation(
            simulation = simulatedScenariosResults[[i]]$simulation,
            filePath = outputPathSim
          )
        }
        # Save population
        if (isOfType(simulatedScenariosResults[[i]]$population, "Population")) {
          ospsuite::exportPopulationToCSV(
            simulatedScenariosResults[[i]]$population,
            filePath = file.path(
              outputFolder,
              paste0(scenarioName, "_population.csv")
            )
          )
        }
        # Save results
        ospsuite::exportResultsToCSV(results = results, filePath = outputPath)
      },
      error = function(cond) {
        warning(paste0("Cannot save to path '", outputFolder, "'"))
        message("Original error message:")
        message(cond)
      },
      warning = function(cond) {
        warning(cond)
      }
    )
  }
  return(outputFolder)
}

#' Load simulated scenarios from csv and pkml.
#'
#' @param scenarioNames Names of simulated scenarios
#' @param resultsFolder Path to the folder where simulation results as csv and
#' the corresponding simulations as pkml are located.
#'
#' @details This function requires simulation results AND the corresponding
#' simulation files being located in the same folder (`resultsFolder`) and have
#' the names of the scenarios.
#'
#' @returns A named list, where the names are scenario names, and the values are
#' lists with the entries `simulation` being the initialized `Simulation` object with applied parameters,
#' `results` being `SimulatioResults` object produced by running the simulation,
#' and `outputValues` the output values of the `SimulationResults`.
#'
#' @export
#'
#' @examples \dontrun{
#' # First simulate scenarios and save the results
#' projectConfiguration <- esqlabsR::createProjectConfiguration()
#' scenarioConfigurations <- readScenarioConfigurationFromExcel(
#'   projectConfiguration = projectConfiguration
#' )
#' scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations)
#' simulatedScenariosResults <- runScenarios(
#'   scenarios = scenarios
#' )
#' saveResults(simulatedScenariosResults, projectConfiguration)
#'
#' # Now load the results
#' scnarioNames <- names(scenarios)
#' simulatedScenariosResults <- loadScenarioResults(
#'   scnarioNames = scnarioNames,
#'   resultsFolder = pathToTheFolder
#' )
#' }
loadScenarioResults <- function(scenarioNames, resultsFolder) {
  simulatedScenariosResults <- list()
  for (i in seq_along(scenarioNames)) {
    scenarioName <- scenarioNames[[i]]
    # Replace "\" and "/" by "_" so the file name does not result in folders.
    # Used only for loading the results, the name of the scenario is not changed.
    scenarioNameForPath <- gsub("[\\\\/]", "_", scenarioName)

    simulation <- loadSimulation(paste0(
      resultsFolder,
      "/",
      scenarioNameForPath,
      ".pkml"
    ))

    results <- importResultsFromCSV(
      simulation = simulation,
      filePaths = paste0(resultsFolder, "/", scenarioNameForPath, ".csv")
    )

    outputValues <- getOutputValues(
      results,
      quantitiesOrPaths = results$allQuantityPaths,
      addMetaData = FALSE
    )
    simulatedScenariosResults[[scenarioNames[[i]]]] <-
      list(
        simulation = simulation,
        results = results,
        outputValues = outputValues
      )
  }

  return(simulatedScenariosResults)
}

# Public CRUD: outputPaths ----

#' Add output paths to a Project
#'
#' @param project A `Project` object.
#' @param id Character vector of output path IDs (unique within the call
#'   and not already present in `project$outputPaths`).
#' @param path Character vector of output paths, same length as `id`.
#' @returns The `project` object, invisibly.
#' @export
#' @family scenario
addOutputPath <- function(project, id, path) {
  validateIsOfType(project, "Project")
  errors <- character()

  if (
    !is.character(id) ||
      length(id) < 1L ||
      any(is.na(id)) ||
      any(nchar(id) == 0)
  ) {
    errors <- c(errors, "id must be a non-empty character vector")
  }
  if (!is.character(path) || length(path) != length(id)) {
    errors <- c(
      errors,
      "id and path must be character vectors of the same length"
    )
  }
  if (is.character(id) && any(duplicated(id))) {
    errors <- c(
      errors,
      paste0(
        "duplicate ids within call: ",
        paste(unique(id[duplicated(id)]), collapse = ", ")
      )
    )
  }
  if (is.character(id)) {
    collisions <- intersect(id, names(project$outputPaths))
    if (length(collisions) > 0) {
      errors <- c(
        errors,
        paste0(
          "outputPath id already exists: ",
          paste(collisions, collapse = ", ")
        )
      )
    }
  }

  if (length(errors) > 0) {
    cli::cli_abort(c(
      "Cannot add outputPath:",
      stats::setNames(errors, rep("x", length(errors)))
    ))
  }

  newPaths <- as.list(path)
  names(newPaths) <- id
  project$outputPaths <- c(project$outputPaths, newPaths)
  project$.markModified()
  invisible(project)
}

#' Remove an output path from a Project
#' @param project A `Project` object.
#' @param id Character scalar.
#' @returns The `project` object, invisibly.
#' @export
#' @family scenario
removeOutputPath <- function(project, id) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1L || is.na(id) || nchar(id) == 0) {
    cli::cli_abort("{.arg id} must be a non-empty string")
  }
  if (!(id %in% names(project$outputPaths))) {
    cli::cli_warn("outputPath {.val {id}} not found; no-op.")
    return(invisible(project))
  }
  .warnIfReferenced(project, "outputPath", id)
  project$outputPaths <- project$outputPaths[setdiff(
    names(project$outputPaths),
    id
  )]
  project$.markModified()
  invisible(project)
}

# Public CRUD: scenarios ----

#' Add a scenario programmatically to a Project
#'
#' Creates a new `Scenario` and adds it to `project$scenarios` after
#' validating all references.
#'
#' @param project A `Project` object.
#' @param scenarioName Character. Name for the new scenario. Must not
#'   already exist in `project$scenarios`.
#' @param modelFile Character. Name of the `.pkml` model file (relative
#'   to model folder).
#' @param individualId Character or `NULL`. ID referencing
#'   `project$individuals`.
#' @param populationId Character or `NULL`. ID referencing
#'   `project$populations`.
#' @param applicationProtocol Character or `NULL`. Protocol name
#'   referencing `project$applications`.
#' @param modelParameterSets Character vector or `NULL`. Set names
#'   referencing `project$modelParameterSets`.
#' @param outputPathIds Character vector or `NULL`. IDs referencing
#'   `project$outputPaths`.
#' @param simulationTime Character or `NULL`. Format
#'   `"start, end, resolution"` or
#'   `"start, end, resolution; start, end, resolution"` for multiple
#'   intervals.
#' @param simulationTimeUnit Character. Time unit string. Default `"h"`.
#' @param steadyState Logical. Whether to simulate steady state. Default
#'   `FALSE`.
#' @param steadyStateTime Numeric. Steady-state time in
#'   `steadyStateTimeUnit`. Default `1000`.
#' @param steadyStateTimeUnit Character. Unit for `steadyStateTime`.
#'   Default `"min"`.
#' @param overwriteFormulasInSS Logical. Overwrite formulas during
#'   steady state. Default `FALSE`.
#' @param readPopulationFromCSV Logical. Load population from CSV.
#'   Default `FALSE`.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family scenario
addScenario <- function(
  project,
  scenarioName,
  modelFile,
  individualId = NULL,
  populationId = NULL,
  applicationProtocol = NULL,
  modelParameterSets = NULL,
  outputPathIds = NULL,
  simulationTime = NULL,
  simulationTimeUnit = "h",
  steadyState = FALSE,
  steadyStateTime = 1000,
  steadyStateTimeUnit = "min",
  overwriteFormulasInSS = FALSE,
  readPopulationFromCSV = FALSE
) {
  validateIsOfType(project, "Project")
  errors <- character()

  if (
    !is.character(scenarioName) ||
      length(scenarioName) != 1L ||
      is.na(scenarioName) ||
      nchar(scenarioName) == 0
  ) {
    errors <- c(errors, "scenarioName must be a non-empty string")
  } else if (scenarioName %in% names(project$scenarios)) {
    errors <- c(
      errors,
      paste0("scenario '", scenarioName, "' already exists")
    )
  }

  if (
    !is.character(modelFile) ||
      length(modelFile) != 1L ||
      is.na(modelFile) ||
      nchar(modelFile) == 0
  ) {
    errors <- c(errors, "modelFile must be a non-empty string")
  }

  if (
    !is.null(individualId) &&
      !(individualId %in% names(project$individuals))
  ) {
    errors <- c(
      errors,
      paste0("individualId '", individualId, "' not found in individuals")
    )
  }
  if (
    !is.null(populationId) &&
      !(populationId %in% names(project$populations))
  ) {
    errors <- c(
      errors,
      paste0("populationId '", populationId, "' not found in populations")
    )
  }
  if (
    !is.null(applicationProtocol) &&
      !(applicationProtocol %in% names(project$applications))
  ) {
    errors <- c(
      errors,
      paste0(
        "applicationProtocol '",
        applicationProtocol,
        "' not found in applications"
      )
    )
  }
  if (!is.null(modelParameterSets)) {
    bad <- setdiff(modelParameterSets, names(project$modelParameterSets))
    if (length(bad) > 0L) {
      errors <- c(
        errors,
        paste0(
          "modelParameterSets not found in project$modelParameterSets: ",
          paste(bad, collapse = ", ")
        )
      )
    }
  }
  if (!is.null(outputPathIds)) {
    bad <- setdiff(outputPathIds, names(project$outputPaths))
    if (length(bad) > 0L) {
      errors <- c(
        errors,
        paste0(
          "outputPathIds not found in outputPaths: ",
          paste(bad, collapse = ", ")
        )
      )
    }
  }

  if (length(errors) > 0L) {
    cli::cli_abort(c(
      "Cannot add scenario {.val {scenarioName}}:",
      stats::setNames(errors, rep("x", length(errors)))
    ))
  }

  sc <- Scenario$new()
  sc$scenarioName <- scenarioName
  sc$modelFile <- modelFile
  sc$individualId <- individualId
  sc$applicationProtocol <- applicationProtocol %||% NA

  if (!is.null(populationId)) {
    sc$populationId <- populationId
    sc$simulationType <- "Population"
  }

  sc$modelParameterSets <- modelParameterSets
  sc$readPopulationFromCSV <- readPopulationFromCSV

  if (!is.null(outputPathIds)) {
    sc$outputPaths <- setNames(
      unlist(project$outputPaths[outputPathIds], use.names = FALSE),
      outputPathIds
    )
  }

  if (!is.null(simulationTime)) {
    sc$simulationTime <- .parseSimulationTimeIntervals(simulationTime)
    sc$simulationTimeUnit <- simulationTimeUnit
  }

  sc$simulateSteadyState <- steadyState
  sc$steadyStateTime <- steadyStateTime
  sc$steadyStateTimeUnit <- steadyStateTimeUnit
  sc$overwriteFormulasInSS <- overwriteFormulasInSS

  project$scenarios[[scenarioName]] <- sc
  project$.markModified()

  invisible(project)
}

#' Remove a scenario from a Project
#' @param project A `Project` object.
#' @param name Character scalar, scenario name.
#' @returns The `project` object, invisibly.
#' @export
#' @family scenario
removeScenario <- function(project, name) {
  validateIsOfType(project, "Project")
  if (
    !is.character(name) ||
      length(name) != 1L ||
      is.na(name) ||
      nchar(name) == 0
  ) {
    cli::cli_abort("{.arg name} must be a non-empty string")
  }
  if (!(name %in% names(project$scenarios))) {
    cli::cli_warn("scenario {.val {name}} not found; no-op.")
    return(invisible(project))
  }
  project$scenarios[[name]] <- NULL
  project$.markModified()
  invisible(project)
}
