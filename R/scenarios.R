# Section validation adapters ----
#
# Registered in `.validationAdapters` (R/validation.R) and called by
# `.runProjectValidation()`. Each pulls the right slice of the project
# and delegates to a section-local `.validate*` helper. They only run
# when `validateProject()` (or a targeted `.ensureValid`) is called;
# they do not run during parsing or simulation.

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

#' Run a set of scenarios from a `Project`.
#'
#' @description Loads simulations, applies parameters, runs the
#'   simulations, and collects results for one or more scenarios
#'   defined on a parsed [Project]. The project must already have been
#'   loaded with [loadProject()].
#'
#' @param project A [Project] object loaded from a `Project.json` file.
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
#' @export
runScenarios <- function(
  project,
  scenarioNames = NULL,
  customParams = NULL,
  simulationRunOptions = NULL,
  validate = TRUE
) {
  if (!inherits(project, "Project")) {
    cli::cli_abort(
      "{.arg project} must be a {.cls Project} \
                    (see {.fn loadProject})."
    )
  }
  .runScenariosFromProject(
    project,
    scenarioNames,
    customParams,
    simulationRunOptions,
    validate
  )
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

  checkScalarFK <- function(value, argName, lookup, lookupLabel) {
    if (is.null(value)) {
      return(character())
    }
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        nchar(value) == 0
    ) {
      return(paste0(argName, " must be a non-empty string or NULL"))
    }
    if (!(value %in% names(lookup))) {
      return(paste0(argName, " '", value, "' not found in ", lookupLabel))
    }
    character()
  }
  checkVectorFK <- function(value, argName, lookup, lookupLabel) {
    if (is.null(value)) {
      return(character())
    }
    if (
      !is.character(value) ||
        length(value) == 0L ||
        any(is.na(value)) ||
        any(nchar(value) == 0)
    ) {
      return(paste0(
        argName,
        " must be a non-empty character vector with no NA or empty entries"
      ))
    }
    bad <- setdiff(value, names(lookup))
    if (length(bad) > 0L) {
      return(paste0(
        argName,
        " not found in ",
        lookupLabel,
        ": ",
        paste(bad, collapse = ", ")
      ))
    }
    character()
  }

  errors <- c(
    errors,
    checkScalarFK(
      individualId,
      "individualId",
      project$individuals,
      "individuals"
    ),
    checkScalarFK(
      populationId,
      "populationId",
      project$populations,
      "populations"
    ),
    checkScalarFK(
      applicationProtocol,
      "applicationProtocol",
      project$applications,
      "applications"
    ),
    checkVectorFK(
      modelParameterSets,
      "modelParameterSets",
      project$modelParameterSets,
      "project$modelParameterSets"
    ),
    checkVectorFK(
      outputPathIds,
      "outputPathIds",
      project$outputPaths,
      "outputPaths"
    )
  )

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

#' Parse simulation time intervals from string format
#'
#' @param simulationTimeIntervalsString Character string. A string containing simulation time intervals
#'   in the format "start1,end1,resolution1;start2,end2,resolution2;...".
#'   Each interval consists of start time, end time, and resolution separated by commas,
#'   and multiple intervals are separated by semicolons.
#'
#' @details Parses a string representation of simulation time intervals into a list
#' of numeric vectors. Each vector contains three elements: start_time, end_time, resolution.
#' The function validates that all values are numeric, positive, and that start times
#' are less than end times.
#'
#' @returns A list of numeric vectors, each containing three elements representing
#' start_time, end_time, resolution for each time interval. Returns `NULL` if
#' the input string is `NULL`.
#'
#' @keywords internal
.parseSimulationTimeIntervals <- function(simulationTimeIntervalsString) {
  # Check if the simulation time intervals are defined
  if (is.null(simulationTimeIntervalsString)) {
    return(NULL)
  }

  # Split the string by ';'
  simulationTimeIntervals <- strsplit(
    x = simulationTimeIntervalsString,
    split = ";",
    fixed = TRUE
  )[[1]]
  # Split each interval by ','
  simulationTimeIntervals <- strsplit(
    x = simulationTimeIntervals,
    split = ",",
    fixed = TRUE
  )
  # Convert to numeric
  simulationTimeIntervals <- lapply(simulationTimeIntervals, as.numeric)
  # Validate that all are numeric
  validateIsNumeric(simulationTimeIntervals)
  # Validate that all are positive
  if (any(unlist(simulationTimeIntervals) < 0)) {
    stop(messages$stopWrongTimeIntervalString(simulationTimeIntervalsString))
  }
  # Validate all intervals are of length 3
  if (any(sapply(simulationTimeIntervals, length) != 3)) {
    stop(messages$stopWrongTimeIntervalString(simulationTimeIntervalsString))
  }
  # Validate all resolution entries are greater than 0
  if (any(sapply(simulationTimeIntervals, function(x) x[3] <= 0))) {
    stop(messages$stopWrongTimeIntervalString(simulationTimeIntervalsString))
  }
  # Validate all start values are smaller than end values
  if (any(sapply(simulationTimeIntervals, function(x) x[1] >= x[2]))) {
    stop(messages$stopWrongTimeIntervalString(simulationTimeIntervalsString))
  }

  return(simulationTimeIntervals)
}
