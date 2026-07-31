# R/parameter-identification.R

# Plain-data record constructors.

#' Create a Parameter Identification parameter
#'
#' @description Builds a plain-data `PIParameter` record describing one
#'   optimisation variable of a Parameter Identification task: the model
#'   parameter to estimate, the scenarios it is fitted across, and its
#'   search bounds and start value.
#'
#'   A `PIParameter` is a building block for a [PITask()]; pass a list of
#'   them as the `parameters` argument of [PITask()] or [addPITask()], or
#'   add one to an existing task with [addPIParameter()].
#'
#' @param id Character scalar. Identifier for this parameter, unique within
#'   its task. Used as a free label by the PI run, not as a definition-file id.
#' @param scenarios Character vector of scenario ids the parameter is
#'   estimated across. Listing several scenarios fits one shared value
#'   across all of them. The constructor does not check these against the
#'   task's own `scenarios`; any that are not a subset of the task's
#'   scenarios are reported later, by [validateProject()] and at run time.
#' @param path Character scalar. Full simulation path of the model
#'   parameter to estimate (OSPS notation).
#' @param units Optional character scalar. Display unit the bounds and
#'   start value are expressed in. `NULL` or `""` means the model default
#'   unit.
#' @param minValue,maxValue,startValue Numeric scalars. Search bounds and
#'   the starting value; `minValue <= startValue <= maxValue` must hold.
#'
#' @returns A `PIParameter` object: a named list with copy semantics.
#' @seealso [PITask()], [PIOutputMapping()], [addPIParameter()].
#' @export
#' @family parameterIdentification
#' @examples
#' PIParameter(
#'   id = "lipophilicity",
#'   scenarios = "aciclovir_iv",
#'   path = "Aciclovir|Lipophilicity",
#'   minValue = -2,
#'   maxValue = 2,
#'   startValue = 0
#' )
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
    cli::cli_abort(messages$PIRequiredField(
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
    cli::cli_abort(messages$PIScenariosEmpty("PIParameter", id))
  }
  if (
    !is.character(path) || length(path) != 1L || is.na(path) || nchar(path) == 0
  ) {
    cli::cli_abort(messages$PIRequiredField("path", "PIParameter", id))
  }
  # `units` is optional: NULL or an empty string both mean "no display unit".
  # A non-empty string is the declared unit. NA or a non-scalar must never
  # reach the builder, where `nchar(NA) == 2` would slip the guard and assign
  # NA to the runtime unit.
  if (
    !is.null(units) &&
      (!is.character(units) || length(units) != 1L || is.na(units))
  ) {
    cli::cli_abort(messages$PIRequiredField("units", "PIParameter", id))
  }
  if (!is.numeric(minValue) || length(minValue) != 1L || is.na(minValue)) {
    cli::cli_abort(messages$PIRequiredField("minValue", "PIParameter", id))
  }
  if (!is.numeric(maxValue) || length(maxValue) != 1L || is.na(maxValue)) {
    cli::cli_abort(messages$PIRequiredField("maxValue", "PIParameter", id))
  }
  if (
    !is.numeric(startValue) || length(startValue) != 1L || is.na(startValue)
  ) {
    cli::cli_abort(messages$PIRequiredField(
      "startValue",
      "PIParameter",
      id
    ))
  }
  if (minValue > maxValue || startValue < minValue || startValue > maxValue) {
    cli::cli_abort(messages$PIInvalidBounds(
      path,
      minValue,
      startValue,
      maxValue
    ))
  }

  .piParameterRecord(
    id = id,
    scenarios = scenarios,
    path = path,
    units = units,
    minValue = minValue,
    maxValue = maxValue,
    startValue = startValue
  )
}

# Assemble a `PIParameter` record, without the guards above.
#
# Split out of `PIParameter()` for the same reason as `.piOutputMappingRecord()`:
# authoring a parameter with no path or no bounds is a mistake to reject at the
# call, whereas a project file (or a legacy workbook with a blank `MinValue`
# cell) carrying one has to load so `validateProject()` can report it. A project
# that cannot be opened cannot be fixed. `runPI()` gates on that validation, so
# an incomplete parameter never reaches an optimisation.
#
# An absent bound stays absent rather than becoming `NA`: `.validatePI()`
# distinguishes "no value" from "an unusable one" only if the field is missing.
#
# @keywords internal
# @noRd
.piParameterRecord <- function(
  id,
  scenarios,
  path,
  units = NULL,
  minValue = NULL,
  maxValue = NULL,
  startValue = NULL
) {
  asBound <- function(x) if (is.null(x)) NULL else as.double(x)
  rec <- list(
    id = id,
    scenarios = as.character(scenarios),
    path = path,
    units = units,
    minValue = asBound(minValue),
    maxValue = asBound(maxValue),
    startValue = asBound(startValue)
  )
  class(rec) <- c("PIParameter", "list")
  rec
}

#' Create a Parameter Identification output mapping
#'
#' @description Builds a plain-data `PIOutputMapping` record pairing one
#'   simulation output with the observed dataset it is fitted against, plus
#'   the optional per-mapping fitting metadata (scaling, axis offsets and
#'   factors, residual weights).
#'
#'   A `PIOutputMapping` is a building block for a [PITask()]; pass a list
#'   of them as the `outputMappings` argument of [PITask()] or [addPITask()],
#'   or add one to an existing task with [addPIOutputMapping()].
#'
#' @param id Character scalar. Identifier for this mapping, unique within
#'   its task.
#' @param scenarios Character vector of scenario ids the mapping applies to.
#'   Must be a subset of the task's own `scenarios`.
#' @param outputPath Character scalar identifying an output path defined in
#'   `outputPaths`: either its id (a key of `outputPaths`) or the literal
#'   model path it maps to (its value). Both resolve to the same defined
#'   output path; it must already exist (add one with [addOutputPath()]).
#' @param observedData Character scalar. Id of the observed dataset to fit
#'   against.
#' @param scaling Optional character scalar. Residual scaling (e.g. `"lin"`
#'   or `"log"`); `NULL` uses the runtime default.
#' @param xOffset,yOffset,xFactor,yFactor Numeric scalars. Affine transform
#'   applied to the observed data before comparison. Defaults are the
#'   identity transform (`0` offsets, `1` factors).
#' @param weight Optional numeric scalar or vector. Residual weight(s)
#'   applied to the observed dataset; `NULL` leaves the data unweighted.
#'
#' @returns A `PIOutputMapping` object: a named list with copy semantics.
#' @seealso [PITask()], [PIParameter()], [addPIOutputMapping()].
#' @export
#' @family parameterIdentification
#' @examples
#' PIOutputMapping(
#'   id = "pvb",
#'   scenarios = "aciclovir_iv",
#'   outputPath = "aciclovir_pvb",
#'   observedData = "Laskin 1982.Group A",
#'   scaling = "log"
#' )
PIOutputMapping <- function(
  id,
  scenarios,
  outputPath,
  observedData,
  scaling = NULL,
  xOffset = 0,
  yOffset = 0,
  xFactor = 1,
  yFactor = 1,
  weight = NULL
) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || nchar(id) == 0) {
    cli::cli_abort(messages$PIRequiredField(
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
    cli::cli_abort(messages$PIScenariosEmpty("PIOutputMapping", id))
  }
  if (
    !is.character(outputPath) ||
      length(outputPath) != 1L ||
      is.na(outputPath) ||
      nchar(outputPath) == 0
  ) {
    cli::cli_abort(messages$PIRequiredField(
      "outputPath",
      "PIOutputMapping",
      id
    ))
  }
  if (
    !is.character(observedData) ||
      length(observedData) != 1L ||
      is.na(observedData) ||
      nchar(observedData) == 0
  ) {
    cli::cli_abort(messages$PIRequiredField(
      "observedData",
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
    cli::cli_abort(messages$PIInvalidScaling(id, scaling))
  }
  for (field in c("xOffset", "yOffset", "xFactor", "yFactor")) {
    value <- get(field)
    if (!is.numeric(value) || length(value) != 1L || is.na(value)) {
      cli::cli_abort(messages$PIInvalidNumericField(field, id, value))
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
      cli::cli_abort(messages$PIInvalidNumericField("weight", id, weight))
    }
  }

  .piOutputMappingRecord(
    id = id,
    scenarios = scenarios,
    outputPath = outputPath,
    observedData = observedData,
    scaling = scaling,
    xOffset = xOffset,
    yOffset = yOffset,
    xFactor = xFactor,
    yFactor = yFactor,
    weight = weight
  )
}

# Assemble a `PIOutputMapping` record, without the guards above.
#
# The user-facing args and on-disk JSON keys are suffixless (`outputPath` /
# `observedData`), while the in-memory record keeps its id-suffixed field names
# (`outputPathId` / `observedDataId`), which the runtime build, validation, and
# serializer all read. This is the mapping seam: the arg feeds the kept record
# field. The parser (`.parsePIOutputMappings`) reads the JSON key into the arg;
# the serializer (`.piOutputMappingToJson`) mirrors it back to the key.
#
# Split out from `PIOutputMapping()` so authoring and loading can differ in
# strictness while producing one record shape: authoring a mapping with no output
# or no observed data is a mistake to reject at the call, whereas a project file
# (or a legacy workbook) carrying one has to load so `validateProject()` can
# report it, which is the same parse-leniently-then-validate contract the
# `dataCombined` section keeps. `runPI()` gates on that validation, so an
# incomplete mapping never reaches the build.
#
# @keywords internal
# @noRd
.piOutputMappingRecord <- function(
  id,
  scenarios,
  outputPath,
  observedData,
  scaling = NULL,
  xOffset = 0,
  yOffset = 0,
  xFactor = 1,
  yFactor = 1,
  weight = NULL
) {
  rec <- list(
    id = id,
    scenarios = as.character(scenarios),
    outputPathId = outputPath,
    observedDataId = observedData,
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

#' Create a Parameter Identification task
#'
#' @description Builds a plain-data `PITask` record bundling the scenarios,
#'   the optimisation variables ([PIParameter()] records), the output
#'   mappings ([PIOutputMapping()] records), and the solver configuration of
#'   one Parameter Identification run.
#'
#'   A task requires at least one parameter and one output mapping. Compose
#'   the records first, then add the task to a project with [addPITask()] and
#'   run every task on the project with [runPI()]. To grow a task after it is
#'   added, use [addPIParameter()] / [addPIOutputMapping()].
#'
#' @param id Character scalar. Identifier for the task.
#' @param scenarios Character vector of scenario ids the task runs against.
#'   Every scenario referenced by a parameter or output mapping must be in
#'   this set.
#' @param parameters List of [PIParameter()] records. May be empty to create
#'   a task that is seeded later with [addPIParameter()]; a task must have at
#'   least one parameter to run, which [validateProject()] enforces.
#' @param outputMappings List of [PIOutputMapping()] records. May be empty to
#'   create a task that is seeded later with [addPIOutputMapping()]; a task
#'   must have at least one output mapping to run, which [validateProject()]
#'   enforces.
#' @param configuration Named list of solver settings (e.g. `algorithm`,
#'   `ciMethod`, `objectiveFunction`, `simulationRunOptions`). Defaults to
#'   an empty list, leaving every runtime default in place.
#'
#' @returns A `PITask` object: a named list with copy semantics.
#' @seealso [PIParameter()], [PIOutputMapping()], [addPITask()], [runPI()].
#' @export
#' @family parameterIdentification
#' @examples
#' PITask(
#'   id = "aciclovir_fit",
#'   scenarios = "aciclovir_iv",
#'   parameters = list(PIParameter(
#'     id = "lipophilicity",
#'     scenarios = "aciclovir_iv",
#'     path = "Aciclovir|Lipophilicity",
#'     minValue = -2,
#'     maxValue = 2,
#'     startValue = 0
#'   )),
#'   outputMappings = list(PIOutputMapping(
#'     id = "pvb",
#'     scenarios = "aciclovir_iv",
#'     outputPath = "aciclovir_pvb",
#'     observedData = "Laskin 1982.Group A"
#'   ))
#' )
PITask <- function(
  id,
  scenarios,
  parameters,
  outputMappings,
  configuration = list()
) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || nchar(id) == 0) {
    cli::cli_abort(messages$PIRequiredField("id", "PITask", "<unset>"))
  }

  if (
    !is.character(scenarios) ||
      length(scenarios) == 0L ||
      any(is.na(scenarios)) ||
      any(nchar(scenarios) == 0)
  ) {
    cli::cli_abort(messages$PIScenariosEmpty("PITask", id))
  }

  # An empty list is allowed here so a task can be created first and seeded
  # with addPIParameter() / addPIOutputMapping(), matching the create-then-add
  # shape of the rest of the authoring API. A task left empty is caught at
  # validation time (.validatePI), not construction.
  if (!is.list(parameters)) {
    cli::cli_abort(messages$PIMustBeList("parameters", id))
  }
  for (i in seq_along(parameters)) {
    if (!inherits(parameters[[i]], "PIParameter")) {
      cli::cli_abort(messages$PIWrongElementType(
        "parameters",
        i,
        id,
        "PIParameter"
      ))
    }
  }

  if (!is.list(outputMappings)) {
    cli::cli_abort(messages$PIMustBeList("outputMappings", id))
  }
  for (i in seq_along(outputMappings)) {
    if (!inherits(outputMappings[[i]], "PIOutputMapping")) {
      cli::cli_abort(
        messages$PIWrongElementType(
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
    # Validate the task id against the filename (when loaded from a tree file)
    # before building the task, so a file with no `id` or one disagreeing with
    # its filename aborts naming the file (consistent with the other keyed
    # kinds) instead of keying the task under the wrong id.
    id <- .keyedTreeRecordId(rawTask, "id", "parameterIdentification task")
    parameters <- .parsePIParameters(rawTask$parameters %||% list(), id)
    outputMappings <- .parsePIOutputMappings(
      rawTask$outputMappings %||% list(),
      id
    )
    task <- PITask(
      id = id,
      scenarios = as.character(unlist(rawTask$scenarios %||% list())),
      parameters = parameters,
      outputMappings = outputMappings,
      configuration = rawTask$configuration %||% list()
    )
    result[[id]] <- task
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
    # Built without `PIParameter()`'s guards, so a parameter with no path or an
    # incomplete set of bounds loads and is reported by `validateProject()`
    # instead of aborting the load of the whole project. The guards still apply
    # to authoring a parameter through the constructor.
    out[[i]] <- .piParameterRecord(
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
    # Built without `PIOutputMapping()`'s required-field guards, so a mapping
    # that names no output or no observed data loads and is reported by
    # `validateProject()` (which `runPI()` gates on) instead of aborting the load
    # of the whole project. A hand-maintained legacy workbook routinely leaves one
    # such cell blank, and a project that cannot be opened cannot be fixed. The
    # guards still apply to authoring a mapping through the constructor.
    out[[i]] <- .piOutputMappingRecord(
      id = id,
      scenarios = as.character(unlist(raw$scenarios %||% list())),
      # Read the suffixless on-disk JSON keys (`outputPath` / `observedData`);
      # the record keeps them under their id-suffixed field names.
      outputPath = raw[["outputPath"]],
      observedData = raw[["observedData"]],
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
# Registered in `.validationAdapters` (R/validation.R) and called by
# validateProject(). Section-local concerns only; cross-section FK checks live
# in .validateCrossReferences().

#' @keywords internal
#' @noRd
.parameterIdentificationValidatorAdapter <- function(project) {
  .validatePI(project$definitions$parameterIdentification)
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
    result$addWarning("Data", "No parameterIdentification tasks defined")
    return(result)
  }

  for (taskId in names(piTasks)) {
    task <- piTasks[[taskId]]

    # A task may be created empty and seeded incrementally, but an empty task
    # cannot run, so validation (which the run gates on) rejects one left empty.
    if (length(task$parameters) == 0L) {
      result$addCriticalError(
        "Empty PI Task",
        messages$PIEmptyList("parameters", taskId)
      )
    }
    if (length(task$outputMappings) == 0L) {
      result$addCriticalError(
        "Empty PI Task",
        messages$PIEmptyList("outputMappings", taskId)
      )
    }

    paramIds <- vapply(task$parameters, `[[`, character(1), "id")
    .checkNoDuplicates(
      paramIds,
      paste0("PIParameter id within task '", taskId, "'"),
      result
    )

    mappingIds <- vapply(task$outputMappings, `[[`, character(1), "id")
    .checkNoDuplicates(
      mappingIds,
      paste0("PIOutputMapping id within task '", taskId, "'"),
      result
    )

    for (p in task$parameters) {
      # The load path is lenient about a parameter's required fields
      # (`.parsePIParameters()`), so their absence is reported here. The bounds
      # comparison below reads all three, and `NULL <= NULL` is `logical(0)`,
      # which `if` cannot branch on, so it only runs once all three are present.
      missingFields <- c("path", "minValue", "maxValue", "startValue")
      missingFields <- missingFields[vapply(
        missingFields,
        function(field) .isMissingField(p[[field]]),
        logical(1)
      )]
      for (field in missingFields) {
        result$addCriticalError(
          "Missing Fields",
          paste0(
            "PI task '",
            taskId,
            "', parameter '",
            p$id,
            "' is missing required field: ",
            field
          )
        )
      }
      if (
        !any(c("minValue", "maxValue", "startValue") %in% missingFields) &&
          !(p$minValue <= p$startValue && p$startValue <= p$maxValue)
      ) {
        result$addCriticalError(
          "Invalid Bounds",
          messages$PIInvalidBounds(
            p$path,
            p$minValue,
            p$startValue,
            p$maxValue
          )
        )
      }
      outsideTask <- setdiff(p$scenarios, task$scenarios)
      if (length(outsideTask) > 0L) {
        result$addCriticalError(
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
      # Both are required on a mapping and the load path is lenient about both
      # (`.parsePIOutputMappings()`). Reported here rather than in the
      # cross-reference phase, which skips itself once any section has a critical
      # error and so would not be reached alongside a parameter's own gap. The
      # record keeps the id-suffixed field names; the message uses the names the
      # user wrote in the file.
      for (field in c("outputPath", "observedData")) {
        if (.isMissingField(m[[paste0(field, "Id")]])) {
          result$addCriticalError(
            "Missing Fields",
            paste0(
              "PI task '",
              taskId,
              "', outputMapping '",
              m$id,
              "' does not define an ",
              field
            )
          )
        }
      }
      outsideTask <- setdiff(m$scenarios, task$scenarios)
      if (length(outsideTask) > 0L) {
        result$addCriticalError(
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
    sc <- project$definitions$scenarios[[sName]]
    if (is.null(sc)) {
      cli::cli_abort(messages$PIScenarioNotFound(
        sName,
        names(project$definitions$scenarios)
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
        project$definitions$outputPaths[[m$outputPathId]]
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
        cli::cli_abort(messages$PIScenarioNotFound(
          sName,
          names(simulations)
        ))
      }
      param <- ospsuite::getParameter(p$path, container = sim)
      if (is.null(param)) {
        cli::cli_abort(messages$PIParameterNotFound(p$path, sim$name))
      }
      param
    })
    runtime <- ospsuite.parameteridentification::PIParameters$new(
      parameters = if (length(paramObjs) == 1L) paramObjs[[1]] else paramObjs
    )
    # Apply the declared display unit first so bounds and start value are
    # interpreted in it, then the start value. The bound setters validate each
    # new bound against the *other* bound, which still holds its model-default
    # value until we overwrite it. Assign the bounds in whichever order keeps
    # every intermediate state valid: if the new minValue would exceed the
    # stale maxValue, raise maxValue first; otherwise lower minValue first. The
    # PIParameter() record is already validated as minValue <= startValue <=
    # maxValue, so one of the two orders always succeeds.
    if (!is.null(p$units) && nchar(p$units) > 0) {
      runtime$unit <- p$units
    }
    runtime$startValue <- p$startValue
    if (p$minValue >= runtime$maxValue) {
      runtime$maxValue <- p$maxValue
      runtime$minValue <- p$minValue
    } else {
      runtime$minValue <- p$minValue
      runtime$maxValue <- p$maxValue
    }
    runtime
  })

  # 3. Build PIOutputMapping runtime objects (one per (scenario, mapping)).
  outputMappings <- list()
  for (m in piTask$outputMappings) {
    fullPath <- project$definitions$outputPaths[[m$outputPathId]]
    for (sName in m$scenarios) {
      sim <- simulations[[sName]]
      if (is.null(sim)) {
        cli::cli_abort(messages$PIScenarioNotFound(
          sName,
          names(simulations)
        ))
      }
      quantity <- ospsuite::getQuantity(fullPath, container = sim)
      if (is.null(quantity)) {
        cli::cli_abort(messages$PIOutputQuantityNotFound(
          fullPath,
          sim$name
        ))
      }
      runtime <- ospsuite.parameteridentification::PIOutputMapping$new(
        quantity = quantity
      )
      ds <- observedData[[m$observedDataId]]
      if (is.null(ds)) {
        cli::cli_abort(messages$PIDatasetNotFound(
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
        runtime$setDataWeights(stats::setNames(
          list(m$weight),
          m$observedDataId
        ))
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
#' @param tasks Optional character vector of task ids to run. When
#'   `NULL` (default), every task on the Project is run. The ids are
#'   canonicalized the same way `addPITask()` canonicalizes a task id, so a
#'   name typed as it was first passed to `addPITask()` still resolves.
#' @param observedData Optional named list of pre-loaded `DataSet`
#'   objects that overrides automatic resolution from
#'   `observedData` definitions.
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
  tasks = NULL,
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

  project$ensureValid(
    sections = c(
      "parameterIdentification",
      "scenarios",
      "outputPaths",
      "observedData",
      "crossReferences"
    ),
    opName = "runPI"
  )

  taskMap <- project$definitions$parameterIdentification %||% list()
  if (is.null(tasks)) {
    tasks <- names(taskMap)
  } else {
    # Canonicalize the referenced task ids so a name typed as it was first
    # passed to addPITask() (before canonicalization filed the task) still
    # resolves, like every other id reference in the package.
    tasks <- .canonicalizeIdRef(tasks)
    missingNames <- setdiff(tasks, names(taskMap))
    if (length(missingNames) > 0L) {
      cli::cli_abort(
        "Unknown {.arg tasks}: {.val {missingNames}}. \\
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
  for (taskName in tasks) {
    msg <- messages$buildingPITask(taskName)
    cli::cli_inform("{msg}")
    runtimes[[taskName]] <- .createSinglePITask(
      project = project,
      piTask = taskMap[[taskName]],
      observedData = observedData,
      stopIfParameterNotFound = stopIfParameterNotFound
    )
  }

  results <- list()
  for (taskName in tasks) {
    msg <- messages$runningPITask(taskName)
    cli::cli_inform("{msg}")
    runtime <- runtimes[[taskName]]
    entry <- tryCatch(
      {
        result <- runtime$run()
        # A completed run can still report "converged" while the uncertainty
        # quantification (Hessian-based SD/CV/CI) came back all-NA for a
        # parameter. Surface that here so a green convergence is not mistaken
        # for a usable fit.
        .warnUnquantifiedUncertainty(taskName, result)
        list(task = runtime, result = result)
      },
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

# Warn for each parameter of a completed PI run whose uncertainty could not be
# quantified (SD, CV, and the confidence-interval bounds all came back `NA`).
# `runtime$run()` returns `convergence = TRUE` even when the CI step (e.g. a
# Hessian inversion) produces nothing usable, so a green "converged" status can
# hide a parameter with no real uncertainty. Each warning names the task and the
# parameter and points at the likely causes (ill-conditioned/singular Hessian,
# the estimate sitting at a bound, an objective insensitive to the parameter).
# A `result` without a `toList()` method (e.g. a test stub) is a silent no-op.
#
# @keywords internal
# @noRd
.warnUnquantifiedUncertainty <- function(taskName, result) {
  if (is.null(result) || !is.function(result$toList)) {
    return(invisible(NULL))
  }
  info <- result$toList()
  paramNames <- info$paramNames
  if (is.null(paramNames) || length(paramNames) == 0L) {
    return(invisible(NULL))
  }
  # A parameter has no usable uncertainty when SD, CV, and both CI bounds are
  # all NA. These vectors are meant to be parallel to `paramNames` (one entry
  # per parameter), each defaulting to NA when the CI step yielded nothing. Guard
  # the shape: a NULL or wrong-length vector is treated as all-NA of the right
  # length, so the elementwise `&` never recycles or indexes out of range.
  n <- length(paramNames)
  asParallelNA <- function(x) {
    if (length(x) == n) is.na(x) else rep(TRUE, n)
  }
  unquantified <- asParallelNA(info$sd) &
    asParallelNA(info$cv) &
    asParallelNA(info$lowerCI) &
    asParallelNA(info$upperCI)
  for (i in which(unquantified)) {
    paramName <- paramNames[[i]]
    cli::cli_warn(c(
      "Parameter identification task {.val {taskName}}: uncertainty could not \\
      be quantified for parameter {.val {paramName}} (standard deviation, CV, \\
      and confidence interval are all {.val NA}).",
      "i" = "The reported estimate has no usable uncertainty even though \\
      convergence is reported. Likely causes: a singular or ill-conditioned \\
      Hessian, the estimate sitting at a parameter bound, or the objective \\
      being insensitive to this parameter."
    ))
  }
  invisible(NULL)
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
  # Defunct (hard removal): signal a defunct error. `deprecate_stop()` aborts,
  # so no separate `cli_abort` is needed.
  lifecycle::deprecate_stop(
    when = "6.0.0",
    what = "createPITasks()",
    with = "runPI(project)",
    details = paste(
      "createPITasks() is removed. runPI(project) builds and runs",
      "PI tasks in one step."
    )
  )
}

# Mutation API ----

#' Add a Parameter Identification task to a Project
#'
#' @param project A `Project` object.
#' @param id Character scalar. New task id; must not collide with an
#'   existing task id.
#' @param scenarios Character vector of scenario names. Each must exist
#'   in `names(project$definitions$scenarios)`.
#' @param parameters List of `PIParameter` records. May be empty (`list()`)
#'   to create a task seeded later with [addPIParameter()].
#' @param outputMappings List of `PIOutputMapping` records. May be empty
#'   (`list()`) to create a task seeded later with [addPIOutputMapping()].
#'   Each `outputPath` must identify a defined output path, either by its id
#'   (a key in `names(project$definitions$outputPaths)`) or by its literal
#'   model path.
#' @param configuration Named list of solver settings; see the `configuration`
#'   argument of [PITask()] for the supported keys.
#' @param overwrite Logical scalar. When `FALSE` (default), an existing task id
#'   aborts. When `TRUE`, the existing task is replaced (last-write-wins).
#' @returns The `project` object, invisibly.
#' @export
#' @family parameterIdentification
addPITask <- function(
  project,
  id,
  scenarios,
  parameters,
  outputMappings,
  configuration = list(),
  overwrite = FALSE
) {
  validateIsOfType(project, "Project")
  project$addPITask(
    id,
    scenarios,
    parameters,
    outputMappings,
    configuration,
    overwrite
  )
}

# Implementation behind `project$addPITask()` / `addPITask()`.
#
# @keywords internal
# @noRd
.addPITask_impl <- function(
  self,
  private,
  id,
  scenarios,
  parameters,
  outputMappings,
  configuration = list(),
  overwrite = FALSE,
  .call
) {
  rlang::local_error_call(.call)
  errors <- character()
  if (!is.character(id) || length(id) != 1L || is.na(id) || nchar(id) == 0) {
    errors <- c(errors, "id must be a non-empty string")
  } else {
    id <- .canonicalizeId(id)
    if (!overwrite && id %in% names(self$definitions$parameterIdentification)) {
      # addPITask aggregates all field errors into one abort (a task carries its
      # own parameter / output-mapping / scenario references, each separately
      # validated), so the duplicate-id error is one entry in that vector rather
      # than the standalone two-line `messages$definitionAlreadyExists()` the
      # single-id add* functions raise. The wording still matches: "<thing>
      # already exists" plus the overwrite hint.
      errors <- c(
        errors,
        paste0(
          "PI task '",
          id,
          "' already exists; pass overwrite = TRUE to replace it"
        )
      )
    }
  }

  scenarios <- .canonicalizeIdRef(scenarios)
  unknownScenarios <- setdiff(scenarios, names(self$definitions$scenarios))
  if (length(unknownScenarios) > 0L) {
    errors <- c(
      errors,
      paste0(
        "scenarios not found in project$definitions$scenarios: ",
        paste(unknownScenarios, collapse = ", ")
      )
    )
  }

  # Canonicalize the scenario references carried on the inline records so they
  # resolve against the canonical ids their definitions were filed under
  # (observedData is OSPS-owned and left untouched).
  parameters <- lapply(parameters, .canonicalizePIParameterRefs)
  outputMappings <- lapply(outputMappings, .canonicalizePIOutputMappingRefs)

  # Resolve each well-typed mapping's output-path reference (an id, or a
  # literal model path) to the canonical id, rewriting the record so the task
  # stores the id. A reference that resolves to no defined output path is
  # collected as an aggregated error rather than aborting here (this call
  # aggregates every field's error into one abort). Malformed entries are left
  # for PITask() to reject with the typed PIWrongElementType, instead of dying
  # here on a raw "$ operator is invalid for atomic vectors". lapply() above
  # already made outputMappings a list, so no is.list() guard is needed.
  for (i in seq_along(outputMappings)) {
    m <- outputMappings[[i]]
    if (!inherits(m, "PIOutputMapping")) {
      next
    }
    resolved <- .matchOutputPathRef(m$outputPathId, self)
    if (is.null(resolved)) {
      errors <- c(
        errors,
        paste0(
          "outputPath '",
          m$outputPathId,
          "' is neither a defined output-path id nor the model path of one. ",
          "Pass an output-path id (a key in project$definitions$outputPaths) ",
          "or the literal model path of a defined output path; define new ",
          "ones with addOutputPath().",
          .suggestSuffix(m$outputPathId, names(self$definitions$outputPaths))
        )
      )
    } else {
      outputMappings[[i]]$outputPathId <- resolved
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
  tasks <- private$.getSection("parameterIdentification")
  tasks[[id]] <- task
  private$.setSection("parameterIdentification", tasks)
  invisible(self)
}

#' Remove one or more Parameter Identification tasks from a Project
#'
#' Drop the tasks with matching ids in one write-through. Warns (and skips)
#' any id not present.
#'
#' `addPITask()` is not vectorized over ids: each task is composed of its own
#' distinct lists of `PIParameter` / `PIOutputMapping` records, so several
#' tasks are added with several calls. The per-task sub-definition helpers
#' (`addPIParameter()` / `addPIOutputMapping()` and their removals) act on one
#' parent task identified by `task`, so they likewise stay single-definition.
#'
#' @param project A `Project` object.
#' @param id Character vector of task ids. Each is canonicalized the same way
#'   [addPITask()] canonicalizes it.
#' @returns The `project` object, invisibly.
#' @export
#' @family parameterIdentification
removePITask <- function(project, id) {
  validateIsOfType(project, "Project")
  project$removePITask(id)
}

# Implementation behind `project$removePITask()` / `removePITask()`.
#
# @keywords internal
# @noRd
.removePITask_impl <- function(self, private, id, .call) {
  rlang::local_error_call(.call)
  .assertIdVector(id)
  id <- .canonicalizeId(id)
  missingIds <- setdiff(id, names(self$definitions$parameterIdentification))
  if (length(missingIds) > 0L) {
    cli::cli_warn("PI task {.val {missingIds}} not found; no-op.")
  }
  toRemove <- intersect(id, names(self$definitions$parameterIdentification))
  if (length(toRemove) == 0L) {
    return(invisible(self))
  }
  tasks <- private$.getSection("parameterIdentification")
  tasks[toRemove] <- NULL
  private$.setSection("parameterIdentification", tasks)
  invisible(self)
}

# Canonicalize the scenario references on a PIParameter record (its `id` is a
# free label the PI run uses, not a definition-file id, so it is left as-is).
#
# @keywords internal
# @noRd
.canonicalizePIParameterRefs <- function(p) {
  if (inherits(p, "PIParameter") && !is.null(p$scenarios)) {
    p$scenarios <- .canonicalizeIdRef(p$scenarios)
  }
  p
}

# Canonicalize the scenario references on a PIOutputMapping record (the
# observed-data reference is OSPS-owned and left as-is). The output-path
# reference is resolved separately, against the project, by
# `.resolveOutputPathRef()`, because a caller may supply a literal model path
# that canonicalization would mangle.
#
# @keywords internal
# @noRd
.canonicalizePIOutputMappingRefs <- function(m) {
  if (inherits(m, "PIOutputMapping") && !is.null(m$scenarios)) {
    m$scenarios <- .canonicalizeIdRef(m$scenarios)
  }
  m
}

# Match a user-supplied output-path reference to the canonical output-path id
# it names, or `NULL` when it is not a non-empty scalar string or names no
# defined output path. Accepts either the id itself, or the literal model path
# that is the value of an existing `outputPaths` entry (the form legacy PI
# configurations used). Returning `NULL` for a malformed value lets the caller
# raise the typed `outputPathRefNotFound` abort rather than a raw R error on a
# zero- or multi-length condition.
#
# @keywords internal
# @noRd
.matchOutputPathRef <- function(value, project) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    return(NULL)
  }
  outputPaths <- project$definitions$outputPaths
  ids <- names(outputPaths)
  # A literal model path (OSPS notation, e.g. "Organism|...|Plasma (...)") is
  # the value of an entry; resolve it back to that entry's id.
  literalHit <- ids[vapply(outputPaths, identical, logical(1), value)]
  if (length(literalHit) > 0L) {
    return(literalHit[[1L]])
  }
  # Otherwise treat it as an id, canonicalized the same way the definition was
  # filed (silent: a literal path would already have matched above).
  canonical <- .silentlyCanonicalized(.canonicalizeIdRef(value))
  if (canonical %in% ids) {
    return(canonical)
  }
  NULL
}

# Resolve an output-path reference to its canonical id, aborting with a "did
# you mean" hint when it names no defined output path. Wraps
# `.matchOutputPathRef()` for callers that fail fast on a single reference.
#
# @keywords internal
# @noRd
.resolveOutputPathRef <- function(value, project, .call = rlang::caller_env()) {
  id <- .matchOutputPathRef(value, project)
  if (is.null(id)) {
    cli::cli_abort(
      messages$outputPathRefNotFound(
        value,
        names(project$definitions$outputPaths)
      ),
      call = .call
    )
  }
  id
}

#' Add a parameter to an existing PI task
#'
#' @param project A `Project` object.
#' @param task Character scalar. Existing PI task id.
#' @param path Character scalar. Full simulation parameter path.
#' @param scenarios Character vector of scenario names; each must
#'   exist in `scenarios` definitions.
#' @param minValue,maxValue,startValue Numeric scalars.
#' @param units Optional character scalar.
#' @param id Optional character scalar; auto-generated as
#'   `<task>_param_<N>` when absent.
#' @param overwrite Logical scalar. When `FALSE` (default), an explicit `id`
#'   that already exists in the task aborts. When `TRUE`, the existing
#'   parameter is replaced (last-write-wins). Ignored for an auto-generated
#'   `id`, which never collides.
#' @returns The `project` object, invisibly.
#' @export
#' @family parameterIdentification
addPIParameter <- function(
  project,
  task,
  path,
  scenarios,
  minValue,
  maxValue,
  startValue,
  units = NULL,
  id = NULL,
  overwrite = FALSE
) {
  validateIsOfType(project, "Project")
  project$addPIParameter(
    task,
    path,
    scenarios,
    minValue,
    maxValue,
    startValue,
    units,
    id,
    overwrite
  )
}

# Implementation behind `project$addPIParameter()` / `addPIParameter()`.
#
# @keywords internal
# @noRd
.addPIParameter_impl <- function(
  self,
  private,
  task,
  path,
  scenarios,
  minValue,
  maxValue,
  startValue,
  units = NULL,
  id = NULL,
  overwrite = FALSE,
  .call
) {
  rlang::local_error_call(.call)
  task <- .canonicalizeId(task)
  if (!(task %in% names(self$definitions$parameterIdentification))) {
    cli::cli_abort("PI task {.val {task}} not found")
  }
  scenarios <- .canonicalizeIdRef(scenarios)
  unknownScenarios <- setdiff(scenarios, names(self$definitions$scenarios))
  if (length(unknownScenarios) > 0L) {
    cli::cli_abort(
      "scenarios not found: {.val {unknownScenarios}}"
    )
  }
  piTask <- self$definitions$parameterIdentification[[task]]
  existingIds <- vapply(piTask$parameters, `[[`, character(1), "id")
  if (is.null(id)) {
    id <- .nextFreeId(paste0(task, "_param_"), existingIds)
  }
  existingIdx <- which(existingIds == id)
  if (length(existingIdx) > 0L && !overwrite) {
    cli::cli_abort(c(
      "Parameter {.val {id}} already exists in task {.val {task}}.",
      "i" = "Pass {.code overwrite = TRUE} to replace it."
    ))
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
  if (length(existingIdx) > 0L) {
    piTask$parameters[[existingIdx]] <- newParam
  } else {
    piTask$parameters[[length(piTask$parameters) + 1L]] <- newParam
  }
  tasks <- private$.getSection("parameterIdentification")
  tasks[[task]] <- piTask
  private$.setSection("parameterIdentification", tasks)
  invisible(self)
}

#' Remove a parameter from a PI task
#'
#' Warns and is a no-op when the parameter id does not exist. If removing
#' the parameter leaves the task with no parameters AND no output mappings,
#' the task is auto-removed from `parameterIdentification` definitions and a
#' warning is emitted.
#'
#' @param project A `Project` object.
#' @param task Character scalar. Existing PI task id.
#' @param id Character scalar. Parameter id to remove.
#' @returns The `project` object, invisibly.
#' @export
#' @family parameterIdentification
removePIParameter <- function(project, task, id) {
  validateIsOfType(project, "Project")
  project$removePIParameter(task, id)
}

# Implementation behind `project$removePIParameter()` / `removePIParameter()`.
#
# @keywords internal
# @noRd
.removePIParameter_impl <- function(self, private, task, id, .call) {
  rlang::local_error_call(.call)
  task <- .canonicalizeId(task)
  if (!(task %in% names(self$definitions$parameterIdentification))) {
    cli::cli_abort("PI task {.val {task}} not found")
  }
  piTask <- self$definitions$parameterIdentification[[task]]
  ids <- vapply(piTask$parameters, `[[`, character(1), "id")
  if (!(id %in% ids)) {
    cli::cli_warn(
      "Parameter {.val {id}} not found in task {.val {task}}; no-op."
    )
    return(invisible(self))
  }
  piTask$parameters <- piTask$parameters[ids != id]
  tasks <- private$.getSection("parameterIdentification")
  if (length(piTask$parameters) == 0L && length(piTask$outputMappings) == 0L) {
    cli::cli_warn(
      "PI task {.val {task}} is now empty and has been removed."
    )
    tasks[[task]] <- NULL
  } else {
    tasks[[task]] <- piTask
  }
  private$.setSection("parameterIdentification", tasks)
  invisible(self)
}

#' Add an output mapping to an existing PI task
#'
#' @param project A `Project` object.
#' @param task Character scalar. Existing PI task id.
#' @param outputPath Character scalar identifying a defined output path:
#'   either its id (a key in `names(project$definitions$outputPaths)`) or the
#'   literal model path it maps to. Both resolve to the same output path.
#' @param observedData Character scalar. Name of the observed dataset.
#' @param scenarios Character vector of scenario names.
#' @param scaling,xOffset,yOffset,xFactor,yFactor,weight Optional
#'   per-mapping fitting metadata. Defaults match `PIOutputMapping()`.
#' @param id Optional character scalar; auto-generated as
#'   `<task>_mapping_<N>` when absent.
#' @param overwrite Logical scalar. When `FALSE` (default), an explicit `id`
#'   that already exists in the task aborts. When `TRUE`, the existing mapping
#'   is replaced (last-write-wins). Ignored for an auto-generated `id`, which
#'   never collides.
#' @returns The `project` object, invisibly.
#' @export
#' @family parameterIdentification
addPIOutputMapping <- function(
  project,
  task,
  outputPath,
  observedData,
  scenarios,
  scaling = NULL,
  xOffset = 0,
  yOffset = 0,
  xFactor = 1,
  yFactor = 1,
  weight = NULL,
  id = NULL,
  overwrite = FALSE
) {
  validateIsOfType(project, "Project")
  project$addPIOutputMapping(
    task,
    outputPath,
    observedData,
    scenarios,
    scaling,
    xOffset,
    yOffset,
    xFactor,
    yFactor,
    weight,
    id,
    overwrite
  )
}

# Implementation behind `project$addPIOutputMapping()` / `addPIOutputMapping()`.
#
# @keywords internal
# @noRd
.addPIOutputMapping_impl <- function(
  self,
  private,
  task,
  outputPath,
  observedData,
  scenarios,
  scaling = NULL,
  xOffset = 0,
  yOffset = 0,
  xFactor = 1,
  yFactor = 1,
  weight = NULL,
  id = NULL,
  overwrite = FALSE,
  .call
) {
  rlang::local_error_call(.call)
  task <- .canonicalizeId(task)
  if (!(task %in% names(self$definitions$parameterIdentification))) {
    cli::cli_abort("PI task {.val {task}} not found")
  }
  # Accept either an output-path id or the literal model path of a defined
  # output path; store the resolved id.
  outputPath <- .resolveOutputPathRef(outputPath, self)
  scenarios <- .canonicalizeIdRef(scenarios)
  unknownScenarios <- setdiff(scenarios, names(self$definitions$scenarios))
  if (length(unknownScenarios) > 0L) {
    cli::cli_abort("scenarios not found: {.val {unknownScenarios}}")
  }
  piTask <- self$definitions$parameterIdentification[[task]]
  existingIds <- vapply(piTask$outputMappings, `[[`, character(1), "id")
  if (is.null(id)) {
    id <- .nextFreeId(paste0(task, "_mapping_"), existingIds)
  }
  existingIdx <- which(existingIds == id)
  if (length(existingIdx) > 0L && !overwrite) {
    cli::cli_abort(c(
      "Output mapping {.val {id}} already exists in task {.val {task}}.",
      "i" = "Pass {.code overwrite = TRUE} to replace it."
    ))
  }
  newMapping <- PIOutputMapping(
    id = id,
    scenarios = scenarios,
    outputPath = outputPath,
    observedData = observedData,
    scaling = scaling,
    xOffset = xOffset,
    yOffset = yOffset,
    xFactor = xFactor,
    yFactor = yFactor,
    weight = weight
  )
  if (length(existingIdx) > 0L) {
    piTask$outputMappings[[existingIdx]] <- newMapping
  } else {
    piTask$outputMappings[[length(piTask$outputMappings) + 1L]] <- newMapping
  }
  tasks <- private$.getSection("parameterIdentification")
  tasks[[task]] <- piTask
  private$.setSection("parameterIdentification", tasks)
  invisible(self)
}

#' Remove an output mapping from a PI task
#'
#' Warns and is a no-op when the mapping id does not exist. If removing
#' the output mapping leaves the task with no parameters AND no output
#' mappings, the task is auto-removed from
#' `parameterIdentification` definitions and a warning is emitted.
#'
#' @param project A `Project` object.
#' @param task Character scalar. Existing PI task id.
#' @param id Character scalar. Output mapping id to remove.
#' @returns The `project` object, invisibly.
#' @export
#' @family parameterIdentification
removePIOutputMapping <- function(project, task, id) {
  validateIsOfType(project, "Project")
  project$removePIOutputMapping(task, id)
}

# Implementation behind `project$removePIOutputMapping()` /
# `removePIOutputMapping()`.
#
# @keywords internal
# @noRd
.removePIOutputMapping_impl <- function(self, private, task, id, .call) {
  rlang::local_error_call(.call)
  task <- .canonicalizeId(task)
  if (!(task %in% names(self$definitions$parameterIdentification))) {
    cli::cli_abort("PI task {.val {task}} not found")
  }
  piTask <- self$definitions$parameterIdentification[[task]]
  ids <- vapply(piTask$outputMappings, `[[`, character(1), "id")
  if (!(id %in% ids)) {
    cli::cli_warn(
      "Output mapping {.val {id}} not found in task {.val {task}}; no-op."
    )
    return(invisible(self))
  }
  piTask$outputMappings <- piTask$outputMappings[ids != id]
  tasks <- private$.getSection("parameterIdentification")
  if (length(piTask$parameters) == 0L && length(piTask$outputMappings) == 0L) {
    cli::cli_warn(
      "PI task {.val {task}} is now empty and has been removed."
    )
    tasks[[task]] <- NULL
  } else {
    tasks[[task]] <- piTask
  }
  private$.setSection("parameterIdentification", tasks)
  invisible(self)
}
