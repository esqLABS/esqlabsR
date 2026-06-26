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
    cli::cli_abort(messages$errorPIRequiredField(
      "id",
      "PIParameter",
      "<unset>"
    ))
  }
  if (
    !is.character(scenarios) ||
      length(scenarios) == 0L ||
      any(is.na(scenarios)) ||
      any(nchar(scenarios) == 0)
  ) {
    cli::cli_abort(messages$errorPIScenariosEmpty("PIParameter", id))
  }
  if (
    !is.character(path) || length(path) != 1L || is.na(path) || nchar(path) == 0
  ) {
    cli::cli_abort(messages$errorPIRequiredField("path", "PIParameter", id))
  }
  # `units` is optional: NULL or an empty string both mean "no display unit".
  # A non-empty string is the declared unit. NA or a non-scalar must never
  # reach the builder, where `nchar(NA) == 2` would slip the guard and assign
  # NA to the runtime unit.
  if (
    !is.null(units) &&
      (!is.character(units) || length(units) != 1L || is.na(units))
  ) {
    cli::cli_abort(messages$errorPIRequiredField("units", "PIParameter", id))
  }
  if (!is.numeric(minValue) || length(minValue) != 1L || is.na(minValue)) {
    cli::cli_abort(messages$errorPIRequiredField("minValue", "PIParameter", id))
  }
  if (!is.numeric(maxValue) || length(maxValue) != 1L || is.na(maxValue)) {
    cli::cli_abort(messages$errorPIRequiredField("maxValue", "PIParameter", id))
  }
  if (
    !is.numeric(startValue) || length(startValue) != 1L || is.na(startValue)
  ) {
    cli::cli_abort(messages$errorPIRequiredField(
      "startValue",
      "PIParameter",
      id
    ))
  }
  if (minValue > maxValue || startValue < minValue || startValue > maxValue) {
    cli::cli_abort(messages$errorPIInvalidBounds(
      path,
      minValue,
      startValue,
      maxValue
    ))
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
    cli::cli_abort(messages$errorPIRequiredField(
      "id",
      "PIOutputMapping",
      "<unset>"
    ))
  }
  if (
    !is.character(scenarios) ||
      length(scenarios) == 0L ||
      any(is.na(scenarios)) ||
      any(nchar(scenarios) == 0)
  ) {
    cli::cli_abort(messages$errorPIScenariosEmpty("PIOutputMapping", id))
  }
  if (
    !is.character(outputPathId) ||
      length(outputPathId) != 1L ||
      is.na(outputPathId) ||
      nchar(outputPathId) == 0
  ) {
    cli::cli_abort(messages$errorPIRequiredField(
      "outputPathId",
      "PIOutputMapping",
      id
    ))
  }
  if (
    !is.character(observedDataId) ||
      length(observedDataId) != 1L ||
      is.na(observedDataId) ||
      nchar(observedDataId) == 0
  ) {
    cli::cli_abort(messages$errorPIRequiredField(
      "observedDataId",
      "PIOutputMapping",
      id
    ))
  }
  if (
    !is.null(scaling) &&
      (!is.character(scaling) ||
        length(scaling) != 1L ||
        is.na(scaling) ||
        nchar(scaling) == 0)
  ) {
    cli::cli_abort(messages$errorPIInvalidScaling(id, scaling))
  }
  for (field in c("xOffset", "yOffset", "xFactor", "yFactor")) {
    value <- get(field)
    if (!is.numeric(value) || length(value) != 1L || is.na(value)) {
      cli::cli_abort(messages$errorPIInvalidNumericField(field, id, value))
    }
  }
  # `weight` may arrive as a bare list of numbers from a JSON round trip, so
  # flatten before checking. A non-numeric or NA element is invalid.
  if (!is.null(weight)) {
    flatWeight <- unlist(weight)
    if (
      length(flatWeight) == 0L ||
        !is.numeric(flatWeight) ||
        any(is.na(flatWeight))
    ) {
      cli::cli_abort(messages$errorPIInvalidNumericField("weight", id, weight))
    }
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
    # Coerce to a flat double vector so a JSON round trip (which reparses a
    # length-1 weight as integer and a length-n weight as a bare list) stays
    # identical to the originally constructed value.
    weight = if (is.null(weight)) NULL else as.double(unlist(weight))
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
    cli::cli_abort(messages$errorPIRequiredField("id", "PITask", "<unset>"))
  }

  if (
    !is.character(scenarios) ||
      length(scenarios) == 0L ||
      any(is.na(scenarios)) ||
      any(nchar(scenarios) == 0)
  ) {
    cli::cli_abort(messages$errorPIScenariosEmpty("PITask", id))
  }

  if (!is.list(parameters) || length(parameters) == 0L) {
    cli::cli_abort(messages$errorPIEmptyList("parameters", id))
  }
  for (i in seq_along(parameters)) {
    if (!inherits(parameters[[i]], "PIParameter")) {
      cli::cli_abort(messages$errorPIWrongElementType(
        "parameters",
        i,
        id,
        "PIParameter"
      ))
    }
  }

  if (!is.list(outputMappings) || length(outputMappings) == 0L) {
    cli::cli_abort(messages$errorPIEmptyList("outputMappings", id))
  }
  for (i in seq_along(outputMappings)) {
    if (!inherits(outputMappings[[i]], "PIOutputMapping")) {
      cli::cli_abort(
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

#' @exportS3Method
#' @noRd
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

#' @exportS3Method
#' @noRd
print.PIOutputMapping <- function(x, ...) {
  ospsuite.utils::ospPrintClass(x)
  weightDisplay <- if (is.null(x$weight)) {
    ""
  } else {
    paste(x$weight, collapse = ", ")
  }
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

#' @exportS3Method
#' @noRd
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

# Parse ----
#
# Parse the `parameterIdentification` JSON array into a named list keyed
# by task id. Each entry becomes a `PITask` containing a list of
# `PIParameter` and a list of `PIOutputMapping` records. Returns an
# empty list when the section is absent or empty.
#
# @keywords internal
# @noRd
.parsePITasks <- function(piData) {
  if (is.null(piData) || length(piData) == 0L) {
    return(list())
  }
  result <- list()
  for (rawTask in piData) {
    parameters <- .parsePIParameters(rawTask$parameters %||% list(), rawTask$id)
    outputMappings <- .parsePIOutputMappings(
      rawTask$outputMappings %||% list(),
      rawTask$id
    )
    task <- PITask(
      id = rawTask$id,
      scenarios = as.character(unlist(rawTask$scenarios %||% list())),
      parameters = parameters,
      outputMappings = outputMappings,
      configuration = rawTask$configuration %||% list()
    )
    result[[rawTask$id]] <- task
  }
  result
}

# @keywords internal
# @noRd
.parsePIParameters <- function(rawList, taskId) {
  out <- vector("list", length(rawList))
  for (i in seq_along(rawList)) {
    raw <- rawList[[i]]
    id <- raw$id %||% paste0(taskId, "_param_", i)
    out[[i]] <- PIParameter(
      id = id,
      scenarios = as.character(unlist(raw$scenarios %||% list())),
      path = raw$path,
      units = raw$units,
      minValue = raw$minValue,
      maxValue = raw$maxValue,
      startValue = raw$startValue
    )
  }
  out
}

# @keywords internal
# @noRd
.parsePIOutputMappings <- function(rawList, taskId) {
  out <- vector("list", length(rawList))
  for (i in seq_along(rawList)) {
    raw <- rawList[[i]]
    id <- raw$id %||% paste0(taskId, "_mapping_", i)
    out[[i]] <- PIOutputMapping(
      id = id,
      scenarios = as.character(unlist(raw$scenarios %||% list())),
      outputPathId = raw$outputPathId,
      observedDataId = raw$observedDataId,
      scaling = raw$scaling,
      xOffset = raw$xOffset %||% 0,
      yOffset = raw$yOffset %||% 0,
      xFactor = raw$xFactor %||% 1,
      yFactor = raw$yFactor %||% 1,
      weight = raw$weight
    )
  }
  out
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
# could bypass that); and that each parameter / outputMapping references
# only scenarios that belong to the task's own `scenarios` (the runtime
# build hard-fails otherwise). NULL/empty section yields a "no PI tasks"
# warning, not a critical error.
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
      outsideTask <- setdiff(p$scenarios, task$scenarios)
      if (length(outsideTask) > 0L) {
        result$add_critical_error(
          "Invalid Reference",
          paste0(
            "PI task '",
            taskId,
            "', parameter '",
            p$id,
            "' references scenarios not in the task's scenarios: ",
            paste(outsideTask, collapse = ", ")
          )
        )
      }
    }

    for (m in task$outputMappings) {
      outsideTask <- setdiff(m$scenarios, task$scenarios)
      if (length(outsideTask) > 0L) {
        result$add_critical_error(
          "Invalid Reference",
          paste0(
            "PI task '",
            taskId,
            "', outputMapping '",
            m$id,
            "' references scenarios not in the task's scenarios: ",
            paste(outsideTask, collapse = ", ")
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
.createSinglePITask <- function(
  project,
  piTask,
  observedData,
  stopIfParameterNotFound = TRUE
) {
  # Build simulations for this task's scenarios via the modern primitive.
  cache <- new.env(parent = emptyenv())
  cache$individuals <- list()
  cache$populations <- list()

  scenarioNames <- piTask$scenarios
  prepared <- list()
  for (sName in scenarioNames) {
    sc <- project$scenarios[[sName]]
    if (is.null(sc)) {
      cli::cli_abort(messages$errorPIScenarioNotFound(
        sName,
        names(project$scenarios)
      ))
    }
    prepared[[sName]] <- .prepareScenario(
      scenario = sc,
      project = project,
      customParams = NULL,
      cache = cache,
      simulationRunOptions = NULL,
      stopIfParameterNotFound = stopIfParameterNotFound
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
        cli::cli_abort(messages$errorPIScenarioNotFound(
          sName,
          names(simulations)
        ))
      }
      param <- ospsuite::getParameter(p$path, container = sim)
      if (is.null(param)) {
        cli::cli_abort(messages$errorPIParameterNotFound(p$path, sim$name))
      }
      param
    })
    runtime <- ospsuite.parameteridentification::PIParameters$new(
      parameters = if (length(paramObjs) == 1L) paramObjs[[1]] else paramObjs
    )
    # Apply the declared display unit first so bounds and start value are
    # interpreted in it. Then assign startValue before minValue/maxValue: the
    # upstream setters validate min/max against the current start value, which
    # would otherwise still be the model default and reject any bounds that do
    # not bracket it.
    if (!is.null(p$units) && nchar(p$units) > 0) {
      runtime$unit <- p$units
    }
    runtime$startValue <- p$startValue
    runtime$minValue <- p$minValue
    runtime$maxValue <- p$maxValue
    runtime
  })

  # 3. Build PIOutputMapping runtime objects (one per (scenario, mapping)).
  outputMappings <- list()
  for (m in piTask$outputMappings) {
    fullPath <- project$outputPaths[[m$outputPathId]]
    for (sName in m$scenarios) {
      sim <- simulations[[sName]]
      if (is.null(sim)) {
        cli::cli_abort(messages$errorPIScenarioNotFound(
          sName,
          names(simulations)
        ))
      }
      quantity <- ospsuite::getQuantity(fullPath, container = sim)
      if (is.null(quantity)) {
        cli::cli_abort(messages$errorPIOutputQuantityNotFound(
          fullPath,
          sim$name
        ))
      }
      runtime <- ospsuite.parameteridentification::PIOutputMapping$new(
        quantity = quantity
      )
      ds <- observedData[[m$observedDataId]]
      if (is.null(ds)) {
        cli::cli_abort(messages$errorPIDatasetNotFound(
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
  if (!is.null(cfg$algorithm)) {
    piConfig$algorithm <- cfg$algorithm
  }
  if (!is.null(cfg$ciMethod)) {
    piConfig$ciMethod <- cfg$ciMethod
  }
  if (!is.null(cfg$autoEstimateCI)) {
    piConfig$autoEstimateCI <- isTRUE(cfg$autoEstimateCI)
  }
  if (!is.null(cfg$printEvaluationFeedback)) {
    piConfig$printEvaluationFeedback <- isTRUE(cfg$printEvaluationFeedback)
  }
  # Merge user-supplied algorithmOptions on top of per-algorithm defaults so a
  # partial block still gets all remaining defaults filled in.
  algDefaults <- if (!is.null(cfg$algorithm)) {
    ospsuite.parameteridentification::AlgorithmDefaults[[cfg$algorithm]]
  } else {
    NULL
  }
  if (!is.null(algDefaults) || !is.null(cfg$algorithmOptions)) {
    piConfig$algorithmOptions <- modifyList(
      algDefaults %||% list(),
      cfg$algorithmOptions %||% list()
    )
  }

  # Merge user-supplied ciOptions on top of per-method defaults the same way.
  ciDefaults <- if (!is.null(cfg$ciMethod)) {
    ospsuite.parameteridentification::CIDefaults[[cfg$ciMethod]]
  } else {
    NULL
  }
  if (!is.null(ciDefaults) || !is.null(cfg$ciOptions)) {
    piConfig$ciOptions <- modifyList(
      ciDefaults %||% list(),
      cfg$ciOptions %||% list()
    )
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

# @keywords internal
# @noRd
# Return the first "<prefix><N>" id (N starting at 1) not already present in
# `existingIds`. Scanning for a free slot (rather than length + 1) keeps
# auto-generated ids collision-free after a removal.
.nextFreeId <- function(prefix, existingIds) {
  n <- 1L
  repeat {
    candidate <- paste0(prefix, n)
    if (!(candidate %in% existingIds)) {
      return(candidate)
    }
    n <- n + 1L
  }
}

# Public runtime API ----

#' Run Parameter Identification tasks defined in a Project
#'
#' Builds and runs every requested PI task in the Project. Build errors
#' (typos in parameter paths, unknown outputs, missing observed data, etc.)
#' propagate as hard errors so users can fix them immediately. Only the
#' optimisation step is wrapped in `tryCatch`: a numerical failure inside
#' `task$run()` degrades to `result = NULL, error = <message>` so the loop
#' continues with the remaining tasks.
#'
#' @param project A `Project` object (see [loadProject()]).
#' @param piTaskNames Optional character vector. When `NULL` (default),
#'   every task on the Project is run.
#' @param observedData Optional named list of pre-loaded `DataSet`
#'   objects that overrides automatic resolution from
#'   `project$observedData`.
#' @param stopIfParameterNotFound Logical. When `TRUE` (default), a
#'   parameter listed in a scenario's parameter sets but absent from the
#'   simulation aborts the build; when `FALSE`, it is skipped with a
#'   warning. Forwarded through `.prepareScenario()` to
#'   `initializeSimulation()`.
#' @returns Named list of per-task results. Each entry is a list with
#'   `task` (the runtime `ParameterIdentification` object), `result`
#'   (the `PIResult` from `task$run()`, or `NULL` on optimisation
#'   failure), and optional `error` (the optimiser's failure message,
#'   absent on success).
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
      when = "6.0.0",
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

  # Build every runtime first so a configuration error (path typo, missing
  # dataset, etc.) fails fast and propagates before any optimisation runs.
  # Otherwise a late build error would discard already-completed (potentially
  # hours-long) optimisations.
  runtimes <- list()
  for (taskName in piTaskNames) {
    message(messages$messageBuildingPITask(taskName))
    runtimes[[taskName]] <- .createSinglePITask(
      project = project,
      piTask = taskMap[[taskName]],
      observedData = observedData,
      stopIfParameterNotFound = stopIfParameterNotFound
    )
  }

  results <- list()
  for (taskName in piTaskNames) {
    message(messages$messageRunningPITask(taskName))
    runtime <- runtimes[[taskName]]
    entry <- tryCatch(
      list(task = runtime, result = runtime$run()),
      error = function(e) {
        # Interpolate once here, with the optimizer's `e$message` bound as a local
        # so cli treats it as data. Routing it through a pre-rendered `messages$`
        # string instead would let cli re-parse the message as a glue template, so
        # a literal `{`/`}` in the (uncontrolled) optimizer error would crash the
        # soft-fail handler itself.
        errorMessage <- e$message
        cli::cli_warn(
          "Parameter identification task {.val {taskName}} optimisation \\
          failed: {.emph {errorMessage}}"
        )
        list(task = runtime, result = NULL, error = e$message)
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
    when = "6.0.0",
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

  # Only dereference outputPathId on well-typed records. Malformed entries are
  # left for PITask() to reject with the typed errorPIWrongElementType, instead
  # of dying here on a raw "$ operator is invalid for atomic vectors".
  if (is.list(outputMappings)) {
    for (m in outputMappings) {
      if (
        inherits(m, "PIOutputMapping") &&
          !(m$outputPathId %in% names(project$outputPaths))
      ) {
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
  existingIds <- vapply(task$parameters, `[[`, character(1), "id")
  if (is.null(id)) {
    id <- .nextFreeId(paste0(taskId, "_param_"), existingIds)
  }
  if (id %in% existingIds) {
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
  existingIds <- vapply(task$outputMappings, `[[`, character(1), "id")
  if (is.null(id)) {
    id <- .nextFreeId(paste0(taskId, "_mapping_"), existingIds)
  }
  if (id %in% existingIds) {
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
