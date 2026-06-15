# R/parameter-identification.R

# Plain-data record constructors.

#' @keywords internal
#' @noRd
PIParameter <- function(
  id,
  scenarios,
  path,
  units = NULL,
  minValue,
  maxValue,
  startValue
) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || nchar(id) == 0) {
    stop(messages$errorPIRequiredField("id", "PIParameter", "<unset>"))
  }
  if (
    !is.character(scenarios) ||
      length(scenarios) == 0L ||
      any(is.na(scenarios)) ||
      any(nchar(scenarios) == 0)
  ) {
    stop(messages$errorPIScenariosEmpty("PIParameter", id))
  }
  if (
    !is.character(path) || length(path) != 1L || is.na(path) || nchar(path) == 0
  ) {
    stop(messages$errorPIRequiredField("path", "PIParameter", id))
  }
  if (!is.numeric(minValue) || length(minValue) != 1L || is.na(minValue)) {
    stop(messages$errorPIRequiredField("minValue", "PIParameter", id))
  }
  if (!is.numeric(maxValue) || length(maxValue) != 1L || is.na(maxValue)) {
    stop(messages$errorPIRequiredField("maxValue", "PIParameter", id))
  }
  if (
    !is.numeric(startValue) || length(startValue) != 1L || is.na(startValue)
  ) {
    stop(messages$errorPIRequiredField("startValue", "PIParameter", id))
  }
  if (minValue > maxValue || startValue < minValue || startValue > maxValue) {
    stop(messages$errorPIInvalidBounds(path, minValue, startValue, maxValue))
  }

  rec <- list(
    id = id,
    scenarios = as.character(scenarios),
    path = path,
    units = units,
    minValue = as.double(minValue),
    maxValue = as.double(maxValue),
    startValue = as.double(startValue)
  )
  class(rec) <- c("PIParameter", "list")
  rec
}

#' @keywords internal
#' @noRd
PIOutputMapping <- function(
  id,
  scenarios,
  outputPathId,
  observedDataId,
  scaling = NULL,
  xOffset = 0,
  yOffset = 0,
  xFactor = 1,
  yFactor = 1,
  weight = NULL
) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || nchar(id) == 0) {
    stop(messages$errorPIRequiredField("id", "PIOutputMapping", "<unset>"))
  }
  if (
    !is.character(scenarios) ||
      length(scenarios) == 0L ||
      any(is.na(scenarios)) ||
      any(nchar(scenarios) == 0)
  ) {
    stop(messages$errorPIScenariosEmpty("PIOutputMapping", id))
  }
  if (
    !is.character(outputPathId) ||
      length(outputPathId) != 1L ||
      is.na(outputPathId) ||
      nchar(outputPathId) == 0
  ) {
    stop(messages$errorPIRequiredField("outputPathId", "PIOutputMapping", id))
  }
  if (
    !is.character(observedDataId) ||
      length(observedDataId) != 1L ||
      is.na(observedDataId) ||
      nchar(observedDataId) == 0
  ) {
    stop(messages$errorPIRequiredField("observedDataId", "PIOutputMapping", id))
  }

  rec <- list(
    id = id,
    scenarios = as.character(scenarios),
    outputPathId = outputPathId,
    observedDataId = observedDataId,
    scaling = scaling,
    xOffset = as.double(xOffset),
    yOffset = as.double(yOffset),
    xFactor = as.double(xFactor),
    yFactor = as.double(yFactor),
    weight = weight
  )
  class(rec) <- c("PIOutputMapping", "list")
  rec
}

#' @keywords internal
#' @noRd
PITask <- function(
  id,
  scenarios,
  parameters,
  outputMappings,
  configuration = list()
) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || nchar(id) == 0) {
    stop(messages$errorPIRequiredField("id", "PITask", "<unset>"))
  }

  if (
    !is.character(scenarios) ||
      length(scenarios) == 0L ||
      any(is.na(scenarios)) ||
      any(nchar(scenarios) == 0)
  ) {
    stop(messages$errorPIScenariosEmpty("PITask", id))
  }

  if (!is.list(parameters) || length(parameters) == 0L) {
    stop(messages$errorPIEmptyList("parameters", id))
  }
  for (i in seq_along(parameters)) {
    if (!inherits(parameters[[i]], "PIParameter")) {
      stop(messages$errorPIWrongElementType("parameters", i, id, "PIParameter"))
    }
  }

  if (!is.list(outputMappings) || length(outputMappings) == 0L) {
    stop(messages$errorPIEmptyList("outputMappings", id))
  }
  for (i in seq_along(outputMappings)) {
    if (!inherits(outputMappings[[i]], "PIOutputMapping")) {
      stop(
        messages$errorPIWrongElementType(
          "outputMappings",
          i,
          id,
          "PIOutputMapping"
        )
      )
    }
  }

  rec <- list(
    id = id,
    scenarios = as.character(scenarios),
    parameters = parameters,
    outputMappings = outputMappings,
    configuration = configuration
  )
  class(rec) <- c("PITask", "list")
  rec
}

#' @method print PIParameter
#' @rawNamespace S3method(print, PIParameter)
print.PIParameter <- function(x, ...) {
  ospsuite.utils::ospPrintClass(x)
  ospsuite.utils::ospPrintItems(
    list(
      "Id" = x$id,
      "Scenarios" = paste(x$scenarios, collapse = ", "),
      "Path" = x$path,
      "Units" = x$units %||% "",
      "Min / Start / Max" = paste(
        format(x$minValue),
        format(x$startValue),
        format(x$maxValue),
        sep = " / "
      )
    ),
    print_empty = TRUE
  )
  invisible(x)
}

#' @method print PIOutputMapping
#' @rawNamespace S3method(print, PIOutputMapping)
print.PIOutputMapping <- function(x, ...) {
  ospsuite.utils::ospPrintClass(x)
  weightDisplay <- if (is.null(x$weight)) "" else
    paste(x$weight, collapse = ", ")
  ospsuite.utils::ospPrintItems(
    list(
      "Id" = x$id,
      "Scenarios" = paste(x$scenarios, collapse = ", "),
      "Output Path Id" = x$outputPathId,
      "Observed Data Id" = x$observedDataId,
      "Scaling" = x$scaling %||% "",
      "Weight" = weightDisplay
    ),
    print_empty = TRUE
  )
  invisible(x)
}

#' @method print PITask
#' @rawNamespace S3method(print, PITask)
print.PITask <- function(x, ...) {
  ospsuite.utils::ospPrintClass(x)
  ospsuite.utils::ospPrintItems(
    list(
      "Id" = x$id,
      "Scenarios" = paste(x$scenarios, collapse = ", "),
      "Number of Parameters" = length(x$parameters),
      "Number of Output Mappings" = length(x$outputMappings),
      "Algorithm" = x$configuration$algorithm %||% "",
      "CI Method" = x$configuration$ciMethod %||% ""
    ),
    print_empty = TRUE
  )
  invisible(x)
}

# Section validation adapter ----
#
# Looked up by name from R/validation.R via
# .lookupSectionValidatorAdapter(). Section-local concerns only —
# cross-section FK checks live in .validateCrossReferences().

#' @keywords internal
#' @noRd
.parameterIdentificationValidatorAdapter <- function(project) {
  .validatePI(project$parameterIdentification)
}

# Section-local validation: id uniqueness within parameter / outputMapping
# lists; defensive bounds re-check (records were already validated at
# construction time, but late-binding writes through the active binding
# could bypass that). NULL/empty section yields a "no PI tasks" warning,
# not a critical error.
#
# @keywords internal
# @noRd
.validatePI <- function(piTasks) {
  result <- validationResult$new()
  if (is.null(piTasks) || length(piTasks) == 0L) {
    result$add_warning("Data", "No parameterIdentification tasks defined")
    return(result)
  }

  for (taskId in names(piTasks)) {
    task <- piTasks[[taskId]]

    paramIds <- vapply(task$parameters, `[[`, character(1), "id")
    if (anyDuplicated(paramIds) > 0L) {
      result$add_critical_error(
        "Invalid Reference",
        paste0(
          "Duplicate PIParameter ids within task '",
          taskId,
          "': ",
          paste(paramIds[duplicated(paramIds)], collapse = ", ")
        )
      )
    }

    mappingIds <- vapply(task$outputMappings, `[[`, character(1), "id")
    if (anyDuplicated(mappingIds) > 0L) {
      result$add_critical_error(
        "Invalid Reference",
        paste0(
          "Duplicate PIOutputMapping ids within task '",
          taskId,
          "': ",
          paste(mappingIds[duplicated(mappingIds)], collapse = ", ")
        )
      )
    }

    for (p in task$parameters) {
      if (!(p$minValue <= p$startValue && p$startValue <= p$maxValue)) {
        result$add_critical_error(
          "Invalid Bounds",
          messages$errorPIInvalidBounds(
            p$path,
            p$minValue,
            p$startValue,
            p$maxValue
          )
        )
      }
    }
  }

  result
}

# Internal runtime builder ----
#
# Used by runPI(project, ...) (Task 9). Builds the OSP-suite
# ParameterIdentification runtime object for one PITask. Not exported.
#
# Builds simulations via the same .prepareScenario primitive
# .runScenariosFromProject uses, so PI runs against identical scenario
# state.

# @keywords internal
# @noRd
.createSinglePITask <- function(project, piTask, observedData) {
  # Build simulations for this task's scenarios via the modern primitive.
  cache <- new.env(parent = emptyenv())
  cache$individuals <- list()
  cache$populations <- list()

  scenarioNames <- piTask$scenarios
  prepared <- list()
  for (sName in scenarioNames) {
    sc <- project$scenarios[[sName]]
    if (is.null(sc)) {
      stop(messages$errorPIScenarioNotFound(sName, names(project$scenarios)))
    }
    prepared[[sName]] <- .prepareScenario(
      scenario = sc,
      project = project,
      customParams = NULL,
      cache = cache,
      simulationRunOptions = NULL
    )
  }
  simulations <- lapply(prepared, `[[`, "simulation")

  # 1. Set outputs on each simulation (collect all outputPathIds per
  # scenario across the task's mappings, dedupe, set in one pass).
  scenarioOutputPaths <- list()
  for (m in piTask$outputMappings) {
    for (sName in m$scenarios) {
      scenarioOutputPaths[[sName]] <- unique(c(
        scenarioOutputPaths[[sName]],
        project$outputPaths[[m$outputPathId]]
      ))
    }
  }
  for (sName in names(scenarioOutputPaths)) {
    if (!is.null(simulations[[sName]])) {
      ospsuite::setOutputs(
        quantitiesOrPaths = scenarioOutputPaths[[sName]],
        simulation = simulations[[sName]]
      )
    }
  }

  # 2. Build PIParameters runtime objects (one per PIParameter record).
  piParams <- lapply(piTask$parameters, function(p) {
    paramObjs <- lapply(p$scenarios, function(sName) {
      sim <- simulations[[sName]]
      if (is.null(sim)) {
        stop(messages$errorPIScenarioNotFound(sName, names(simulations)))
      }
      param <- ospsuite::getParameter(p$path, container = sim)
      if (is.null(param)) {
        stop(messages$errorPIParameterNotFound(p$path, sim$name))
      }
      param
    })
    runtime <- ospsuite.parameteridentification::PIParameters$new(
      parameters = if (length(paramObjs) == 1L) paramObjs[[1]] else paramObjs
    )
    runtime$minValue <- p$minValue
    runtime$maxValue <- p$maxValue
    runtime$startValue <- p$startValue
    runtime
  })

  # 3. Build PIOutputMapping runtime objects (one per (scenario, mapping)).
  outputMappings <- list()
  for (m in piTask$outputMappings) {
    fullPath <- project$outputPaths[[m$outputPathId]]
    for (sName in m$scenarios) {
      sim <- simulations[[sName]]
      if (is.null(sim)) {
        stop(messages$errorPIScenarioNotFound(sName, names(simulations)))
      }
      quantity <- ospsuite::getQuantity(fullPath, container = sim)
      if (is.null(quantity)) {
        stop(messages$errorPIOutputQuantityNotFound(fullPath, sim$name))
      }
      runtime <- ospsuite.parameteridentification::PIOutputMapping$new(
        quantity = quantity
      )
      ds <- observedData[[m$observedDataId]]
      if (is.null(ds)) {
        stop(messages$errorPIDatasetNotFound(
          m$observedDataId,
          names(observedData)
        ))
      }
      runtime$addObservedDataSets(ds)
      if (!is.null(m$scaling)) {
        runtime$scaling <- m$scaling
      }
      hasNonDefaultTransform <- !all(c(
        m$xOffset == 0,
        m$yOffset == 0,
        m$xFactor == 1,
        m$yFactor == 1
      ))
      if (hasNonDefaultTransform) {
        runtime$setDataTransformations(
          labels = m$observedDataId,
          xOffsets = m$xOffset,
          yOffsets = m$yOffset,
          xFactors = m$xFactor,
          yFactors = m$yFactor
        )
      }
      if (!is.null(m$weight)) {
        runtime$setDataWeights(setNames(list(m$weight), m$observedDataId))
      }
      outputMappings[[length(outputMappings) + 1L]] <- runtime
    }
  }

  # 4. Build PIConfiguration from the JSON nested-block shape.
  piConfig <- .buildPIConfiguration(piTask$configuration)

  # 5. Assemble final ParameterIdentification.
  ospsuite.parameteridentification::ParameterIdentification$new(
    simulations = simulations,
    parameters = piParams,
    outputMappings = outputMappings,
    configuration = piConfig
  )
}

# @keywords internal
# @noRd
.buildPIConfiguration <- function(cfg) {
  piConfig <- ospsuite.parameteridentification::PIConfiguration$new()
  if (!is.null(cfg$algorithm)) piConfig$algorithm <- cfg$algorithm
  if (!is.null(cfg$ciMethod)) piConfig$ciMethod <- cfg$ciMethod
  if (!is.null(cfg$autoEstimateCI)) {
    piConfig$autoEstimateCI <- isTRUE(cfg$autoEstimateCI)
  }
  if (!is.null(cfg$printEvaluationFeedback)) {
    piConfig$printEvaluationFeedback <- isTRUE(cfg$printEvaluationFeedback)
  }
  if (!is.null(cfg$algorithmOptions)) {
    piConfig$algorithmOptions <- cfg$algorithmOptions
  }
  if (!is.null(cfg$ciOptions)) {
    piConfig$ciOptions <- cfg$ciOptions
  }

  ofo <- cfg$objectiveFunction
  if (!is.null(ofo)) {
    current <- piConfig$objectiveFunctionOptions
    for (slot in c(
      "type",
      "residualWeightingMethod",
      "robustMethod",
      "scaleVar",
      "linScaleCV",
      "logScaleSD"
    )) {
      if (!is.null(ofo[[slot]])) {
        target <- if (slot == "type") "objectiveFunctionType" else slot
        current[[target]] <- ofo[[slot]]
      }
    }
    piConfig$objectiveFunctionOptions <- current
  }

  sro <- cfg$simulationRunOptions
  if (!is.null(sro)) {
    runOpts <- ospsuite::SimulationRunOptions$new()
    if (!is.null(sro$numberOfCores)) {
      runOpts$numberOfCores <- as.integer(sro$numberOfCores)
    }
    if (!is.null(sro$checkForNegativeValues)) {
      runOpts$checkForNegativeValues <- isTRUE(sro$checkForNegativeValues)
    }
    piConfig$simulationRunOptions <- runOpts
  }

  piConfig
}

# Public runtime API ----

#' Run Parameter Identification tasks defined in a Project
#'
#' Builds and runs every requested PI task in the Project. Each task is
#' built and executed inside a `tryCatch`; a build failure or
#' optimisation failure on one task degrades to `result = NULL,
#' error = <message>` and the loop continues.
#'
#' @param project A `Project` object (see [loadProject()]).
#' @param piTaskNames Optional character vector. When `NULL` (default),
#'   every task on the Project is run.
#' @param observedData Optional named list of pre-loaded `DataSet`
#'   objects that overrides automatic resolution from
#'   `project$observedData`.
#' @param stopIfParameterNotFound Logical. Forwarded to
#'   `.prepareScenario()` for parameter merging.
#' @returns Named list of per-task results. Each entry is a list with
#'   `task` (the runtime `ParameterIdentification` object), `result`
#'   (the `PIResult` from `task$run()`, or `NULL` on failure), and
#'   optional `error` (the failure message, absent on success).
#' @export
runPI <- function(
  project,
  piTaskNames = NULL,
  observedData = NULL,
  stopIfParameterNotFound = TRUE
) {
  # Soft-deprecation: legacy first-arg shape (a list of pre-built
  # ParameterIdentification objects, not a Project).
  if (!inherits(project, "Project")) {
    lifecycle::deprecate_warn(
      when = "6.1.0",
      what = "runPI(piTasks)",
      with = "runPI(project)",
      details = paste(
        "Pass a Project object loaded with loadProject() instead of a",
        "pre-built list of ParameterIdentification objects."
      )
    )
    cli::cli_abort(
      "{.fn runPI} now requires a {.cls Project} object as its first \\
      argument. Migrate via {.fn loadProject} and a \\
      {.field parameterIdentification} section in your Project.json."
    )
  }

  .ensureValid(
    project,
    sections = c(
      "parameterIdentification",
      "scenarios",
      "outputPaths",
      "observedData",
      "crossReferences"
    ),
    opName = "runPI"
  )

  taskMap <- project$parameterIdentification %||% list()
  if (is.null(piTaskNames)) {
    piTaskNames <- names(taskMap)
  } else {
    missingNames <- setdiff(piTaskNames, names(taskMap))
    if (length(missingNames) > 0L) {
      cli::cli_abort(
        "Unknown {.arg piTaskNames}: {.val {missingNames}}. \\
        Available: {.val {names(taskMap)}}."
      )
    }
  }

  observedData <- observedData %||% loadObservedData(project)

  results <- list()
  for (taskName in piTaskNames) {
    message(messages$messageRunningPITask(taskName))
    piTask <- taskMap[[taskName]]
    entry <- tryCatch(
      {
        runtime <- .createSinglePITask(
          project = project,
          piTask = piTask,
          observedData = observedData
        )
        list(task = runtime, result = runtime$run())
      },
      error = function(e) {
        warning(messages$warningPIOptimizationFailed(taskName, e$message))
        list(task = NULL, result = NULL, error = e$message)
      }
    )
    results[[taskName]] <- entry
  }
  results
}

#' Build Parameter Identification tasks (defunct)
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' This function is removed. Use [runPI()] with a [Project][loadProject()]
#' instead. `runPI(project, ...)` builds and runs PI tasks in one step.
#'
#' @param ... Ignored.
#' @returns Aborts.
#' @export
createPITasks <- function(...) {
  lifecycle::deprecate_warn(
    when = "6.1.0",
    what = "createPITasks()",
    with = "runPI(project)",
    details = paste(
      "createPITasks() is removed. runPI(project) builds and runs",
      "PI tasks in one step."
    )
  )
  cli::cli_abort(
    "{.fn createPITasks} has been removed. Use \\
    {.fn runPI}({.code project}, piTaskNames = ...)."
  )
}

# Mutation API ----

#' Add a Parameter Identification task to a Project
#'
#' @param project A `Project` object.
#' @param id Character scalar. New task id; must not collide with an
#'   existing task id.
#' @param scenarios Character vector of scenario names. Each must exist
#'   in `names(project$scenarios)`.
#' @param parameters Non-empty list of `PIParameter` records.
#' @param outputMappings Non-empty list of `PIOutputMapping` records.
#'   Each `outputPathId` must exist in `names(project$outputPaths)`.
#' @param configuration Named list. See PRD for the nested-block shape.
#' @returns The `project` object, invisibly.
#' @export
#' @family parameterIdentification
addPITask <- function(
  project,
  id,
  scenarios,
  parameters,
  outputMappings,
  configuration = list()
) {
  validateIsOfType(project, "Project")

  errors <- character()
  if (!is.character(id) || length(id) != 1L || is.na(id) || nchar(id) == 0) {
    errors <- c(errors, "id must be a non-empty string")
  } else if (id %in% names(project$parameterIdentification)) {
    errors <- c(errors, paste0("PI task '", id, "' already exists"))
  }

  unknownScenarios <- setdiff(scenarios, names(project$scenarios))
  if (length(unknownScenarios) > 0L) {
    errors <- c(
      errors,
      paste0(
        "scenarios not found in project$scenarios: ",
        paste(unknownScenarios, collapse = ", ")
      )
    )
  }

  for (m in outputMappings) {
    if (!(m$outputPathId %in% names(project$outputPaths))) {
      errors <- c(
        errors,
        paste0(
          "outputPathId '",
          m$outputPathId,
          "' not found in project$outputPaths"
        )
      )
    }
  }

  if (length(errors) > 0L) {
    cli::cli_abort(c(
      "Cannot add PI task {.val {id}}:",
      stats::setNames(errors, rep("x", length(errors)))
    ))
  }

  task <- PITask(
    id = id,
    scenarios = scenarios,
    parameters = parameters,
    outputMappings = outputMappings,
    configuration = configuration
  )
  project$parameterIdentification[[id]] <- task
  project$.markModified()
  invisible(project)
}

#' Remove a Parameter Identification task from a Project
#'
#' Warns and is a no-op when the task id does not exist.
#'
#' @param project A `Project` object.
#' @param id Character scalar.
#' @returns The `project` object, invisibly.
#' @export
#' @family parameterIdentification
removePITask <- function(project, id) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1L || is.na(id) || nchar(id) == 0) {
    cli::cli_abort("{.arg id} must be a non-empty string")
  }
  if (!(id %in% names(project$parameterIdentification))) {
    cli::cli_warn("PI task {.val {id}} not found; no-op.")
    return(invisible(project))
  }
  project$parameterIdentification[[id]] <- NULL
  project$.markModified()
  invisible(project)
}

#' Add a parameter to an existing PI task
#'
#' @param project A `Project` object.
#' @param taskId Character scalar. Existing PI task id.
#' @param path Character scalar. Full simulation parameter path.
#' @param scenarios Character vector of scenario names; each must
#'   exist in `project$scenarios`.
#' @param minValue,maxValue,startValue Numeric scalars.
#' @param units Optional character scalar.
#' @param id Optional character scalar; auto-generated as
#'   `<taskId>_param_<N>` when absent.
#' @returns The `project` object, invisibly.
#' @export
#' @family parameterIdentification
addPIParameter <- function(
  project,
  taskId,
  path,
  scenarios,
  minValue,
  maxValue,
  startValue,
  units = NULL,
  id = NULL
) {
  validateIsOfType(project, "Project")
  if (!(taskId %in% names(project$parameterIdentification))) {
    cli::cli_abort("PI task {.val {taskId}} not found")
  }
  unknownScenarios <- setdiff(scenarios, names(project$scenarios))
  if (length(unknownScenarios) > 0L) {
    cli::cli_abort(
      "scenarios not found: {.val {unknownScenarios}}"
    )
  }
  task <- project$parameterIdentification[[taskId]]
  if (is.null(id)) {
    id <- paste0(taskId, "_param_", length(task$parameters) + 1L)
  }
  if (id %in% vapply(task$parameters, `[[`, character(1), "id")) {
    cli::cli_abort(
      "Parameter {.val {id}} already exists in task {.val {taskId}}"
    )
  }
  newParam <- PIParameter(
    id = id,
    scenarios = scenarios,
    path = path,
    units = units,
    minValue = minValue,
    maxValue = maxValue,
    startValue = startValue
  )
  task$parameters[[length(task$parameters) + 1L]] <- newParam
  project$parameterIdentification[[taskId]] <- task
  project$.markModified()
  invisible(project)
}

#' Remove a parameter from a PI task
#'
#' Warns and is a no-op when the parameter id does not exist.
#'
#' @param project A `Project` object.
#' @param taskId Character scalar. Existing PI task id.
#' @param id Character scalar. Parameter id to remove.
#' @returns The `project` object, invisibly.
#' @export
#' @family parameterIdentification
removePIParameter <- function(project, taskId, id) {
  validateIsOfType(project, "Project")
  if (!(taskId %in% names(project$parameterIdentification))) {
    cli::cli_abort("PI task {.val {taskId}} not found")
  }
  task <- project$parameterIdentification[[taskId]]
  ids <- vapply(task$parameters, `[[`, character(1), "id")
  if (!(id %in% ids)) {
    cli::cli_warn(
      "Parameter {.val {id}} not found in task {.val {taskId}}; no-op."
    )
    return(invisible(project))
  }
  task$parameters <- task$parameters[ids != id]
  project$parameterIdentification[[taskId]] <- task
  project$.markModified()
  invisible(project)
}

#' Add an output mapping to an existing PI task
#'
#' @param project A `Project` object.
#' @param taskId Character scalar. Existing PI task id.
#' @param outputPathId Character scalar. Must exist in
#'   `names(project$outputPaths)`.
#' @param observedDataId Character scalar. Name of the observed dataset.
#' @param scenarios Character vector of scenario names.
#' @param scaling,xOffset,yOffset,xFactor,yFactor,weight Optional
#'   per-mapping fitting metadata. Defaults match `PIOutputMapping()`.
#' @param id Optional character scalar; auto-generated as
#'   `<taskId>_mapping_<N>` when absent.
#' @returns The `project` object, invisibly.
#' @export
#' @family parameterIdentification
addPIOutputMapping <- function(
  project,
  taskId,
  outputPathId,
  observedDataId,
  scenarios,
  scaling = NULL,
  xOffset = 0,
  yOffset = 0,
  xFactor = 1,
  yFactor = 1,
  weight = NULL,
  id = NULL
) {
  validateIsOfType(project, "Project")
  if (!(taskId %in% names(project$parameterIdentification))) {
    cli::cli_abort("PI task {.val {taskId}} not found")
  }
  if (!(outputPathId %in% names(project$outputPaths))) {
    cli::cli_abort(
      "outputPathId {.val {outputPathId}} not found in project$outputPaths"
    )
  }
  unknownScenarios <- setdiff(scenarios, names(project$scenarios))
  if (length(unknownScenarios) > 0L) {
    cli::cli_abort("scenarios not found: {.val {unknownScenarios}}")
  }
  task <- project$parameterIdentification[[taskId]]
  if (is.null(id)) {
    id <- paste0(taskId, "_mapping_", length(task$outputMappings) + 1L)
  }
  if (id %in% vapply(task$outputMappings, `[[`, character(1), "id")) {
    cli::cli_abort(
      "Output mapping {.val {id}} already exists in task {.val {taskId}}"
    )
  }
  newMapping <- PIOutputMapping(
    id = id,
    scenarios = scenarios,
    outputPathId = outputPathId,
    observedDataId = observedDataId,
    scaling = scaling,
    xOffset = xOffset,
    yOffset = yOffset,
    xFactor = xFactor,
    yFactor = yFactor,
    weight = weight
  )
  task$outputMappings[[length(task$outputMappings) + 1L]] <- newMapping
  project$parameterIdentification[[taskId]] <- task
  project$.markModified()
  invisible(project)
}

#' Remove an output mapping from a PI task
#'
#' Warns and is a no-op when the mapping id does not exist.
#'
#' @param project A `Project` object.
#' @param taskId Character scalar. Existing PI task id.
#' @param id Character scalar. Output mapping id to remove.
#' @returns The `project` object, invisibly.
#' @export
#' @family parameterIdentification
removePIOutputMapping <- function(project, taskId, id) {
  validateIsOfType(project, "Project")
  if (!(taskId %in% names(project$parameterIdentification))) {
    cli::cli_abort("PI task {.val {taskId}} not found")
  }
  task <- project$parameterIdentification[[taskId]]
  ids <- vapply(task$outputMappings, `[[`, character(1), "id")
  if (!(id %in% ids)) {
    cli::cli_warn(
      "Output mapping {.val {id}} not found in task {.val {taskId}}; no-op."
    )
    return(invisible(project))
  }
  task$outputMappings <- task$outputMappings[ids != id]
  project$parameterIdentification[[taskId]] <- task
  project$.markModified()
  invisible(project)
}
