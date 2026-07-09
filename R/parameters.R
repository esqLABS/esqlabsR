# Section validation adapter ----
#
# Registered in `.validationAdapters` (R/validation.R) and called by
# `.runProjectValidation()`. The actual shape check lives in
# `.validateParameterSets()` (in `R/validation.R`).

#' @keywords internal
#' @noRd
.parameterSetsValidatorAdapter <- function(project) {
  .validateParameterSets(project$parameterSets, "parameterSets")
}

# Merge the parameter-set sections of a parsed `Project.json` into the single
# unified `parameterSets` map. The canonical source is the `parameterSets`
# section; a legacy project that still carries the three separate sections
# (`modelParameterSets` / `individualParameterSets` /
# `applicationParameterSets`) has them merged in. An id that appears in more
# than one input section is a genuine collision and aborts, since the three
# former namespaces now share one and a referrer cannot disambiguate.
#
# @keywords internal
# @noRd
.mergeParameterSetSections <- function(jsonData) {
  legacy <- list(
    modelParameterSets = jsonData$modelParameterSets %||% list(),
    individualParameterSets = jsonData$individualParameterSets %||% list(),
    applicationParameterSets = jsonData$applicationParameterSets %||% list()
  )
  hasLegacy <- any(vapply(legacy, length, integer(1)) > 0L)

  # No legacy sections to fold in: the `parameterSets` section is used as-is,
  # preserving its own shape (absent -> bare `list()`, empty `{}` -> a named
  # empty list, exactly like the other map sections). `%||%` covers an absent
  # section.
  if (!hasLegacy) {
    return(jsonData$parameterSets %||% list())
  }

  sources <- c(list(parameterSets = jsonData$parameterSets %||% list()), legacy)
  merged <- list()
  origin <- character()
  for (section in names(sources)) {
    sets <- sources[[section]]
    for (id in names(sets)) {
      if (id %in% names(merged)) {
        cli::cli_abort(c(
          "Parameter set id {.val {id}} is defined in more than one section.",
          "i" = "It appears in both {.field {origin[[id]]}} and \\
          {.field {section}}; the three former parameter-set kinds now share \\
          one {.field parameterSets} namespace, so ids must be unique. Rename \\
          one of them."
        ))
      }
      merged[[id]] <- sets[[id]]
      origin[[id]] <- section
    }
  }
  merged
}

#' Read parameter values from a structured Excel file. Each excel sheet must
#' consist of columns 'Container Path', 'Parameter Name', 'Value', and 'Units'
#'
#' @param paramsXLSpath Path to the excel file
#' @param sheets Names of the excel sheets containing the information about the
#'   parameters. Multiple sheets can be processed. If no sheets are provided,
#'   the first one in the Excel file is used.
#'
#' @returns A list containing vectors `paths` with the full paths to the
#'   parameters, `values` the values of the parameters, and `units` with the
#'   units the values are in.
#' @export
readParametersFromXLS <- function(paramsXLSpath, sheets = NULL) {
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
      cli::cli_abort(messages$errorWrongXLSStructure(
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

#' Read initial values (molecule start values) from a structured Excel file.
#'
#' @description Each excel sheet must consist of columns `Container Path`,
#'   `Molecule Name`, `Is Present`, `Value`, `Units`, `Scale Divisor`, and
#'   `Neg. Values Allowed`. Units are mandatory for every present molecule; a
#'   present row with a blank `Units` cell is an error.
#'
#' @param filePath Path to the excel file
#' @param sheets Names of the excel sheets containing the information about
#'   the initial values. Multiple sheets can be processed. If no sheets are
#'   provided, the first one in the Excel file is used.
#'
#' @returns A single list combining all processed sheets, containing vectors
#'   `paths` with the full molecule paths, `values` with the values, and `units`
#'   with the units the values are in. When multiple sheets are read, their rows
#'   are merged into this one structure; if the same molecule path occurs more
#'   than once, the last occurrence wins (last sheet, then last row). A duplicate
#'   path, whether within a single sheet or repeated across sheets, triggers a
#'   warning before the earlier value is replaced.
#' @export
#' @family parameters
readInitialConditionsFromXLS <- function(filePath, sheets = NULL) {
  rows <- .readInitialConditionsRows(filePath = filePath, sheets = sheets)

  pathsValuesVector <- vector(mode = "numeric")
  pathsUnitsVector <- vector(mode = "character")
  for (row in rows) {
    pathsValuesVector[row$fullPath] <- row$value
    pathsUnitsVector[row$fullPath] <- row$unit
  }

  return(.parametersVectorToList(pathsValuesVector, pathsUnitsVector))
}

#' Extend parameters structure with new entries
#'
#' @param parameters A parameter structure (a list with elements `paths`,
#'   `values`, and `units`) or `NULL`. If `NULL`, it is treated as an empty
#'   parameter structure.
#' @param newParameters A parameter structure (a list with elements `paths`,
#'   `values`, and `units`) or `NULL`. If `NULL`, it is treated as an empty
#'   parameter structure whose entries will be added to or overwrite those in
#'   `parameters`.
#'
#' @details This function adds new parameter entries from `newParameters` to
#'   `parameters`. If an entry with the same path is already present in
#'   `parameters`, its value and unit will be overwritten with the values from
#'   `newParameters`.
#'
#' @returns Updated list of parameter paths, values, and units
#' @export
extendParameterStructure <- function(parameters, newParameters) {
  .validateParametersStructure(
    parameterStructure = parameters,
    argumentName = "parameters",
    nullAllowed = TRUE
  )
  .validateParametersStructure(
    parameterStructure = newParameters,
    argumentName = "newParameters",
    nullAllowed = TRUE
  )

  # Normalize NULL inputs to empty parameter structures
  emptyStructure <- list(paths = NULL, values = NULL, units = NULL)
  parameters <- parameters %||% emptyStructure
  newParameters <- newParameters %||% emptyStructure

  # If the parameters structure is empty, return new parameters
  if (isEmpty(parameters$paths)) {
    return(newParameters)
  }

  # If the new parameters structure is empty, return parameters
  if (isEmpty(newParameters$paths)) {
    return(parameters)
  }

  # Convert the input parameter structure into named vectors.
  pathsValuesVector <- parameters$values
  names(pathsValuesVector) <- parameters$paths
  pathsUnitsVector <- parameters$units
  names(pathsUnitsVector) <- parameters$paths

  # Add new entries resp. update with new values
  pathsValuesVector[newParameters$paths] <- newParameters$values
  pathsUnitsVector[newParameters$paths] <- newParameters$units

  return(.parametersVectorToList(pathsValuesVector, pathsUnitsVector))
}

#' Convert parameters vector structure to list structure
#'
#' @param pathsValuesVector Named vector of numerical parameter values with
#'   parameter paths as names
#' @param pathsUnitsVector Named vector of parameter values units with parameter
#'   paths as names
#'
#' @noRd
#'
#' @returns A named list with vectors `paths`, `values`, and `units`
#' @keywords internal
.parametersVectorToList <- function(pathsValuesVector, pathsUnitsVector) {
  paths <- names(pathsValuesVector)

  returnVal <- list(
    paths = paths,
    values = unname(pathsValuesVector[paths]),
    units = unname(pathsUnitsVector[paths])
  )

  return(returnVal)
}

# Read and validate initial-condition rows from a structured Excel file.
#
# Shared reader used by both `readInitialConditionsFromXLS()` (which collapses
# the rows into a flat `{paths, values, units}` structure) and the Excel-import
# parser `.parseExcelInitialConditions()` (which maps each row to a JSON
# `{path, value, unit}` record). Centralising the column-structure, `Is
# Present`, blank-path, duplicate-path, value and unit validation here keeps the
# two readers from drifting apart.
#
# @param filePath Path to the excel file.
# @param sheets Sheets to read. If `NULL`, only the first sheet is read.
# @param call Environment used to attribute raised conditions to the public
#   caller rather than this internal helper.
#
# @returns A list of per-row records (one per kept, present molecule), each a
#   list with `sheet`, `containerPath`, `moleculeName`, `fullPath`, `value`,
#   and `unit`. Rows where `Is Present` is explicitly `FALSE`/`0` are dropped.
# @keywords internal
# @noRd
.readInitialConditionsRows <- function(
  filePath,
  sheets = NULL,
  call = rlang::caller_env()
) {
  columnNames <- c(
    "Container Path",
    "Molecule Name",
    "Is Present",
    "Value",
    "Units",
    "Scale Divisor",
    "Neg. Values Allowed"
  )
  validateIsString(filePath)
  validateIsString(sheets, nullAllowed = TRUE)

  if (is.null(sheets)) {
    sheets <- readxl::excel_sheets(filePath)[1L]
  }

  rows <- list()
  seenPaths <- character(0)

  for (sheet in sheets) {
    data <- readExcel(path = filePath, sheet = sheet)

    if (!all(columnNames %in% names(data))) {
      msg <- messages$errorWrongXLSStructure(
        filePath = filePath,
        expectedColNames = columnNames
      )
      cli::cli_abort(c("x" = "{msg}"), call = call)
    }

    # "Is Present" must be a logical value or empty. An empty/NA cell is
    # treated as present. Numeric 0/1 (a common Excel representation of a
    # logical column) is accepted. Any other value (e.g. "yes") is rejected
    # rather than silently coerced to NA (which would be kept as present).
    isPresentCol <- data[["Is Present"]]
    isPresentChr <- trimws(as.character(isPresentCol))
    isBlank <- is.na(isPresentCol) | isPresentChr == ""
    isPresent <- as.logical(isPresentChr)
    numericFlag <- isPresentChr %in% c("0", "1")
    isPresent[numericFlag] <- isPresentChr[numericFlag] == "1"
    invalidIsPresent <- !isBlank & is.na(isPresent)
    if (any(invalidIsPresent)) {
      msg <- messages$errorInvalidIsPresentInInitialConditions(
        filePath = filePath,
        moleculePaths = paste(
          data[["Container Path"]],
          data[["Molecule Name"]],
          sep = "|"
        )[invalidIsPresent]
      )
      cli::cli_abort(c("x" = "{msg}"), call = call)
    }

    # Only include rows where Is Present is not explicitly FALSE
    keepRows <- isBlank | isPresent
    keptRowNumbers <- which(keepRows)
    data <- data[keepRows, ]

    if (nrow(data) == 0) {
      next
    }

    # Container Path and Molecule Name must be filled for every kept row,
    # otherwise the constructed path contains "NA" and fails deep in the
    # ospsuite layer with no reference to the originating row.
    containerPath <- as.character(data[["Container Path"]])
    moleculeName <- as.character(data[["Molecule Name"]])
    missingPathParts <- is.na(containerPath) |
      trimws(containerPath) == "" |
      is.na(moleculeName) |
      trimws(moleculeName) == ""
    if (any(missingPathParts)) {
      msg <- messages$errorMissingPathInInitialConditions(
        filePath = filePath,
        sheet = sheet,
        rows = keptRowNumbers[missingPathParts]
      )
      cli::cli_abort(c("x" = "{msg}"), call = call)
    }

    fullPaths <- paste(containerPath, moleculeName, sep = "|")

    # Warn (rather than silently overwrite) when the same molecule path appears
    # more than once: either within this sheet, or already defined on a prior
    # sheet. The last occurrence wins downstream.
    duplicatePaths <- unique(c(
      fullPaths[duplicated(fullPaths)],
      intersect(fullPaths, seenPaths)
    ))
    if (length(duplicatePaths) > 0) {
      msg <- messages$warningDuplicateInitialConditions(
        filePath = filePath,
        moleculePaths = duplicatePaths
      )
      cli::cli_warn(c("!" = "{msg}"), call = call)
    }

    # Validate values and units before accumulating rows, so a failure leaves
    # no partial state behind.
    parsedValues <- suppressWarnings(as.numeric(data[["Value"]]))
    missingValues <- is.na(parsedValues)
    if (any(missingValues)) {
      msg <- messages$errorMissingValuesInInitialConditions(
        filePath = filePath,
        moleculePaths = fullPaths[missingValues]
      )
      cli::cli_abort(c("x" = "{msg}"), call = call)
    }

    unitsRaw <- as.character(data[["Units"]])
    missingUnits <- is.na(data[["Units"]]) | trimws(unitsRaw) == ""
    if (any(missingUnits)) {
      msg <- messages$errorMissingUnitsInInitialConditions(
        filePath = filePath,
        moleculePaths = fullPaths[missingUnits]
      )
      cli::cli_abort(c("x" = "{msg}"), call = call)
    }

    for (i in seq_len(nrow(data))) {
      rows[[length(rows) + 1L]] <- list(
        sheet = sheet,
        containerPath = containerPath[[i]],
        moleculeName = moleculeName[[i]],
        fullPath = fullPaths[[i]],
        value = parsedValues[[i]],
        unit = unitsRaw[[i]]
      )
    }
    seenPaths <- union(seenPaths, fullPaths)
  }

  rows
}

#' @title Check if two parameters are equal with respect to certain properties.
#'
#' @details The parameters are not equal if: The paths of the parameters are not
#' equal; The types of the formulas differ (types checked: isConstant,
#' isDistributed, isExplicit, isTable); Constant formulas have different values;
#' Distributed formulas have different values (not checking for distribution)
#' Explicit formulas: If formula string are not equal, OR one of the parameter
#' values is fixed (formula is overridden), OR both parameter values are fixed
#' and differ, OR checkFormulaValues is TRUE and the values differ (disregarding
#' of overridden or not) Table formulas: If the number of points differ, OR any
#' of the points differ, OR one of the parameter values is fixed (formula is
#' overridden), OR both parameter values are fixed and differ.
#'
#' @param parameter1 First parameter to compare
#' @param parameter2 Second parameter to compare
#' @param checkFormulaValues If TRUE, values of explicit formulas are always
#'   compared. Otherwise, the values are only compared if the formulas are
#'   overridden (isFixedValue == TRUE). FALSE by default.
#' @param compareFormulasByValue If `FALSE`(default), formulas are compared by
#'   their types and string. If `TRUE`, only values are compared.
#'
#' @returns `TRUE` if parameters are considered equal, `FALSE` otherwise
#' @export
isParametersEqual <- function(
  parameter1,
  parameter2,
  checkFormulaValues = FALSE,
  compareFormulasByValue = FALSE
) {
  validateIsOfType(c(parameter1, parameter2), "Parameter")

  # Check for the path
  if (parameter1$path != parameter2$path) {
    return(FALSE)
  }

  formula1 <- parameter1$formula
  formula2 <- parameter2$formula

  # Compare by value
  if (compareFormulasByValue) {
    return(identical(parameter1$value, parameter2$value))
  }

  # Check for formula type equality
  if (
    !all(
      c(
        formula1$isConstant,
        formula1$isDistributed,
        formula1$isExplicit,
        formula1$isTable
      ) ==
        c(
          formula2$isConstant,
          formula2$isDistributed,
          formula2$isExplicit,
          formula2$isTable
        )
    )
  ) {
    return(FALSE)
  }

  # Constant or distributed formula - check for value
  # Comparing using 'identical' to capture NaN and NA cases which can happen
  if (formula1$isConstant || formula1$isDistributed) {
    return(identical(parameter1$value, parameter2$value))
  }

  # Explicit or table formula - check if values are overridden
  if (parameter1$isFixedValue) {
    if (!parameter2$isFixedValue) {
      return(FALSE)
    }
    if (parameter1$value != parameter2$value) {
      return(FALSE)
    }
  }

  # Explicit
  if (formula1$isExplicit) {
    if (
      checkFormulaValues && (!identical(parameter1$value, parameter2$value))
    ) {
      return(FALSE)
    }

    return(formula1$formulaString == formula2$formulaString)
  }

  if (formula1$isTable) {
    return(isTableFormulasEqual(formula1, formula2))
  }

  return(FALSE)
}

#' Check if two table formulas are equal.
#'
#' Table formulas are equal if the number of points is equal and all x-y value
#' pairs are equal between the two formulas
#'
#' @param formula1 First formula to compare
#' @param formula2 Second formula to compare
#'
#' @returns TRUE if the table formulas are equal, FALSE otherwise
#' @export
isTableFormulasEqual <- function(formula1, formula2) {
  allPoints1 <- formula1$allPoints
  allPoints2 <- formula2$allPoints

  if (length(allPoints1) != length(allPoints2)) {
    return(FALSE)
  }

  # Two empty table formulas (no points) are equal. Otherwise every point's x
  # and y must match; the loop only runs once the lengths are known equal.
  all(vapply(
    seq_along(allPoints1),
    \(i) {
      allPoints1[[i]]$x == allPoints2[[i]]$x &&
        allPoints1[[i]]$y == allPoints2[[i]]$y
    },
    logical(1)
  ))
}

#' Set the values of parameters in the simulation by path, if the `condition` is
#' true.
#'
#' @param parameterPaths A single or a list of parameter path
#' @param values A numeric value that should be assigned to the parameters or a
#'   vector of numeric values, if the value of more than one parameter should be
#'   changed. Must have the same length as `parameterPaths`
#' @param condition A function that receives a parameter path as an argument and
#'   returns `TRUE` of `FALSE`
#' @param units A string or a list of strings defining the units of the
#'   `values`. If `NULL` (default), values are assumed to be in base units. If
#'   not `NULL`, must have the same length as `parameterPaths`.
#' @param simulation Simulation used to retrieve parameter instances from given
#'   paths.
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#' condition <- function(path) {
#'   ospsuite::isExplicitFormulaByPath(
#'     path = path,
#'     simulation = sim
#'   )
#' }
#' setParameterValuesByPathWithCondition(
#'   c("Organism|Liver|Volume", "Organism|Volume"),
#'   c(2, 3),
#'   sim,
#'   condition
#' )
#' @import ospsuite
#' @export
setParameterValuesByPathWithCondition <- function(
  parameterPaths, # nolint: object_length_linter.
  values,
  simulation,
  condition = function(path) {
    TRUE
  },
  units = NULL
) {
  for (i in seq_along(parameterPaths)) {
    path <- parameterPaths[[i]]
    if (condition(path)) {
      ospsuite::setParameterValuesByPath(
        parameterPaths = parameterPaths[[i]],
        values = values[[i]],
        simulation = simulation,
        units = units[[i]]
      )
    }
  }
}

#' Split parameter path into container path and parameter name
#'
#' @param parameterPath Full path to the parameter, with path elements separated
#'   by `|`
#'
#' @returns A list with elements `containerPath` and `parameterName`
#' @keywords internal
#' @noRd
.splitParameterPathIntoContainerAndName <- function(parameterPath) {
  fullPathParts <- strsplit(parameterPath, split = "|", fixed = TRUE)[[1]]

  containerPath <- paste(
    fullPathParts[seq_along(fullPathParts) - 1],
    collapse = "|"
  )
  paramName <- fullPathParts[[length(fullPathParts)]]
  return(list(containerPath = containerPath, parameterName = paramName))
}

# Print ----

#' @exportS3Method
#' @noRd
print.ParameterSet <- function(x, ...) {
  ospsuite.utils::ospPrintClass(x)
  entries <- unclass(x)
  ospsuite.utils::ospPrintItems(
    list("Number of Entries" = length(entries)),
    print_empty = TRUE
  )
  if (length(entries) > 0L) {
    lines <- vapply(
      entries,
      function(e) {
        units <- if (is.null(e$units) || !nzchar(e$units)) {
          ""
        } else {
          paste0(" [", e$units, "]")
        }
        paste0(
          e$containerPath,
          "|",
          e$parameterName,
          " = ",
          format(e$value),
          units
        )
      },
      character(1)
    )
    ospsuite.utils::ospPrintItems(
      stats::setNames(as.list(lines), rep("", length(lines)))
    )
  }
  invisible(x)
}

#' @exportS3Method
#' @noRd
print.InitialConditionSet <- function(x, ...) {
  ospsuite.utils::ospPrintClass(x)
  entries <- unclass(x)
  ospsuite.utils::ospPrintItems(
    list("Number of Entries" = length(entries)),
    print_empty = TRUE
  )
  if (length(entries) > 0L) {
    lines <- vapply(
      entries,
      function(e) {
        unit <- if (is.null(e$unit) || !nzchar(e$unit)) {
          ""
        } else {
          paste0(" [", e$unit, "]")
        }
        paste0(e$path, " = ", format(e$value), unit)
      },
      character(1)
    )
    ospsuite.utils::ospPrintItems(
      stats::setNames(as.list(lines), rep("", length(lines)))
    )
  }
  invisible(x)
}

# Public CRUD: parameterSets ----

#' Create one or more parameter sets
#'
#' Adds empty parameter sets to the project's single `parameterSets` section,
#' vectorizing over a vector of ids (all N added in one write-through). A
#' scenario references the sets it applies through its `modelParameterSets`
#' field, an individual or application through its `parameterSets` field; all
#' three resolve against this one section.
#'
#' @inherit vectorizedAuthoring details
#'
#' @param project A `Project` object.
#' @param id Character vector of set ids. Each is canonicalized to a safe,
#'   lowercase id (a warning names the result if it changed); each canonical
#'   id must not already exist.
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
addParameterSet <- function(project, id) {
  validateIsOfType(project, "Project")
  .assertIdVector(id)
  id <- .canonicalizeId(id)
  .assertNoDuplicateIds(id, "parameter set")
  clash <- intersect(id, names(project$parameterSets))
  if (length(clash) > 0L) {
    cli::cli_abort("parameter set {.val {clash}} already exists")
  }
  parameterSets <- project$.getSection("parameterSets") %||% list()
  for (one in id) {
    parameterSets[[one]] <- .asParameterSet(list())
  }
  project$.setSection("parameterSets", parameterSets)
  invisible(project)
}

#' Remove one or more parameter sets
#'
#' Drop the parameter sets with matching ids in one write-through. Warns (and
#' skips) any id not present, and warns when a removed set is still
#' referenced.
#'
#' @inherit vectorizedAuthoring details
#' @inheritParams addParameterSet
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
removeParameterSet <- function(project, id) {
  validateIsOfType(project, "Project")
  .assertIdVector(id)
  id <- .canonicalizeId(id)
  missingIds <- setdiff(id, names(project$parameterSets))
  if (length(missingIds) > 0L) {
    cli::cli_warn("parameter set {.val {missingIds}} not found; no-op.")
  }
  toRemove <- intersect(id, names(project$parameterSets))
  if (length(toRemove) == 0L) {
    return(invisible(project))
  }
  for (one in toRemove) {
    .warnIfReferenced(project, "parameterSet", one)
  }
  parameterSets <- project$.getSection("parameterSets")
  parameterSets[toRemove] <- NULL
  project$.setSection("parameterSets", parameterSets)
  invisible(project)
}

#' Add one or many parameter entries to a named parameter set
#'
#' Adds parameter entries to the named set in `project$parameterSets`.
#' `containerPath`, `parameterName`, `value`, and `units` accept parallel
#' vectors of equal length N to add all N entries in a single call (and a
#' single write to disk); a scalar call (length-1 vectors) adds one entry.
#' Building a large set with one vectorized call is far cheaper than a loop of
#' scalar calls, since each call rewrites the whole set file.
#'
#' Unlike the other `add*` functions, which abort on a missing parent, this
#' creates the parent set on demand if it does not yet exist (informing you
#' when it does). Last-write-wins on duplicate `(containerPath, parameterName)`
#' pairs, including duplicates within a single vectorized call.
#'
#' @param project A `Project` object.
#' @param id Character scalar, set id. Canonicalized; created if not present.
#' @param containerPath Character vector of container paths (length N).
#' @param parameterName Character vector of parameter names (length N).
#' @param value Numeric vector of values (length N).
#' @param units Character vector of units (length N; use `""` for none).
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
addParameterEntry <- function(
  project,
  id,
  containerPath,
  parameterName,
  value,
  units
) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1L || is.na(id) || nchar(id) == 0) {
    cli::cli_abort("{.arg id} must be a non-empty string")
  }
  id <- .canonicalizeId(id)
  # Validate the batch shape up front so a mismatched call fails fast, before
  # any on-demand set creation is reported.
  .assertParameterEntryVectorLengths(
    containerPath,
    parameterName,
    value,
    units
  )
  current <- project$parameterSets[[id]]
  # Unlike the other `add*` functions, `addParameterEntry()` creates its parent
  # set on demand rather than aborting on a missing parent. Inform the user when
  # it does, so the on-demand creation is not silent.
  if (is.null(current)) {
    cli::cli_inform(
      "Created parameter set {.val {id}} on demand to hold the new entr{?y/ies}."
    )
  }
  # Fold all N entries into the set in memory first, so the single write below
  # triggers exactly one write-through (not one per entry).
  parameterSets <- project$.getSection("parameterSets")
  parameterSets[[id]] <- .asParameterSet(.addParameterEntries(
    current,
    containerPath,
    parameterName,
    value,
    units
  ))
  project$.setSection("parameterSets", parameterSets)
  invisible(project)
}

#' Remove one or many parameter entries from a named parameter set
#'
#' Removes parameter entries from the named set. `containerPath` and
#' `parameterName` accept parallel vectors of equal length N to remove all N
#' entries in a single call (and a single write to disk); a scalar call
#' (length-1 vectors) removes one entry. If every entry of the set is removed,
#' the set itself is auto-removed from `project$parameterSets`. Warns if the
#' set or any named entry doesn't exist.
#'
#' @param project A `Project` object.
#' @param id Character scalar, set id. Canonicalized.
#' @param containerPath Character vector of container paths (length N).
#' @param parameterName Character vector of parameter names (length N).
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
removeParameterEntry <- function(
  project,
  id,
  containerPath,
  parameterName
) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1L) {
    cli::cli_abort("{.arg id} must be a string scalar")
  }
  id <- .canonicalizeId(id)
  if (!(id %in% names(project$parameterSets))) {
    cli::cli_warn("parameter set {.val {id}} not found; no-op.")
    return(invisible(project))
  }
  # Fold all N removals into the set in memory first, so the single assignment
  # below triggers exactly one write-through (not one per entry).
  result <- .removeParameterEntries(
    project$parameterSets[[id]],
    containerPath,
    parameterName
  )
  if (!result$removed) {
    return(invisible(project))
  }
  parameterSets <- project$.getSection("parameterSets")
  if (is.null(result$parameters)) {
    .warnIfReferenced(project, "parameterSet", id)
    parameterSets[[id]] <- NULL
  } else {
    parameterSets[[id]] <- .asParameterSet(result$parameters)
  }
  project$.setSection("parameterSets", parameterSets)
  invisible(project)
}

# Validate scalar inputs for an `(containerPath, parameterName, value,
# units)` parameter entry. Returns a character vector of error messages
# (empty if validation passes).
#
# @keywords internal
# @noRd
.validateParameterEntryArgs <- function(
  containerPath,
  parameterName,
  value,
  units
) {
  errors <- character()
  if (
    !is.character(containerPath) ||
      length(containerPath) != 1L ||
      is.na(containerPath) ||
      nchar(containerPath) == 0
  ) {
    errors <- c(errors, "containerPath must be a non-empty string")
  }
  if (
    !is.character(parameterName) ||
      length(parameterName) != 1L ||
      is.na(parameterName) ||
      nchar(parameterName) == 0
  ) {
    errors <- c(errors, "parameterName must be a non-empty string")
  }
  if (!is.numeric(value) || length(value) != 1L || is.na(value)) {
    errors <- c(errors, "value must be a numeric scalar")
  }
  if (!is.character(units) || length(units) != 1L || is.na(units)) {
    errors <- c(errors, "units must be a string scalar (use \"\" for none)")
  }
  errors
}

# Assert that the parallel `(containerPath, parameterName, value, units)`
# argument vectors all share the same length (the batch size). A scalar
# call is the length-1 case. Aborts naming the lengths otherwise.
#
# @keywords internal
# @noRd
.assertParameterEntryVectorLengths <- function(
  containerPath,
  parameterName,
  value,
  units
) {
  lengths <- c(
    containerPath = length(containerPath),
    parameterName = length(parameterName),
    value = length(value),
    units = length(units)
  )
  if (length(unique(lengths)) != 1L) {
    cli::cli_abort(c(
      "{.arg containerPath}, {.arg parameterName}, {.arg value}, and \\
      {.arg units} must be vectors of the same length.",
      "x" = "Got lengths {.val {lengths[['containerPath']]}}, \\
      {.val {lengths[['parameterName']]}}, {.val {lengths[['value']]}}, \\
      and {.val {lengths[['units']]}}."
    ))
  }
  invisible(lengths[["containerPath"]])
}

# Fold N parameter entries (parallel vectors) into a parameter set in one
# pass, returning the updated list. Each entry is validated and appended (or
# replaced, last-write-wins) via `.addParameterEntry`, so an in-batch duplicate
# `(containerPath, parameterName)` resolves to the last value, matching the
# scalar semantics. N=1 is the scalar case. Folding in memory first lets the
# caller persist the whole set in a single write-through.
#
# @keywords internal
# @noRd
.addParameterEntries <- function(
  parameters,
  containerPath,
  parameterName,
  value,
  units
) {
  n <- .assertParameterEntryVectorLengths(
    containerPath,
    parameterName,
    value,
    units
  )
  for (i in seq_len(n)) {
    parameters <- .addParameterEntry(
      parameters,
      containerPath[[i]],
      parameterName[[i]],
      value[[i]],
      units[[i]]
    )
  }
  parameters
}

# Append (or replace, last-write-wins) one parameter entry to a
# JSON-faithful array-of-records parameter set. `parameters` is a list
# of `list(containerPath, parameterName, value, units)` entries (or
# `NULL`); returns the updated list.
#
# @keywords internal
# @noRd
.addParameterEntry <- function(
  parameters,
  containerPath,
  parameterName,
  value,
  units
) {
  errors <- .validateParameterEntryArgs(
    containerPath,
    parameterName,
    value,
    units
  )
  if (length(errors) > 0L) {
    cli::cli_abort(c(
      "Invalid parameter entry:",
      stats::setNames(errors, rep("x", length(errors)))
    ))
  }

  if (is.null(parameters)) {
    parameters <- list()
  }

  existingIdx <- .findParameterEntryIndex(
    parameters,
    containerPath,
    parameterName
  )
  newEntry <- list(
    containerPath = containerPath,
    parameterName = parameterName,
    value = as.double(value),
    units = if (nchar(units) == 0L) NULL else units
  )
  if (length(existingIdx) > 0L) {
    parameters[[existingIdx]] <- newEntry
  } else {
    parameters[[length(parameters) + 1L]] <- newEntry
  }
  parameters
}

# Drop N parameter entries (parallel vectors) from a parameter set in one
# pass, returning the same `list(parameters=, removed=)` shape as
# `.removeParameterEntry`. `removed` is `TRUE` if ANY named entry was actually
# removed (a not-found entry warns and is skipped, as in the scalar case);
# `parameters` is `NULL` when the removals emptied the set, so the caller
# auto-removes it. N=1 is the scalar case. Folding in memory first lets the
# caller persist the whole set in a single write-through.
#
# @keywords internal
# @noRd
.removeParameterEntries <- function(parameters, containerPath, parameterName) {
  if (length(containerPath) != length(parameterName)) {
    cli::cli_abort(c(
      "{.arg containerPath} and {.arg parameterName} must be vectors of \\
      the same length.",
      "x" = "Got lengths {.val {length(containerPath)}} and \\
      {.val {length(parameterName)}}."
    ))
  }
  anyRemoved <- FALSE
  for (i in seq_along(containerPath)) {
    result <- .removeParameterEntry(
      parameters,
      containerPath[[i]],
      parameterName[[i]]
    )
    anyRemoved <- anyRemoved || result$removed
    # A removal that empties the set yields `NULL`; preserve it so a later
    # not-found entry warns against the empty set, matching the scalar path.
    parameters <- result$parameters
  }
  list(parameters = parameters, removed = anyRemoved)
}

# Drop one parameter entry from a JSON-faithful array-of-records
# parameter set. Returns a list with:
#   - `parameters`: the updated set, or `NULL` if removal emptied the set
#     (callers use the `NULL` to auto-remove the named set).
#   - `removed`: `TRUE` if an entry was actually removed, `FALSE` for a
#     no-op (entry not found). Callers gate the write-through on this so
#     a no-op warn doesn't touch the section (and so doesn't invalidate the
#     validation cache).
#
# @keywords internal
# @noRd
.removeParameterEntry <- function(parameters, containerPath, parameterName) {
  if (is.null(parameters) || length(parameters) == 0L) {
    cli::cli_warn(
      "parameter {.val {paste(containerPath, parameterName, sep = '|')}} not found; no-op."
    )
    return(list(parameters = parameters, removed = FALSE))
  }
  idx <- .findParameterEntryIndex(parameters, containerPath, parameterName)
  if (length(idx) == 0L) {
    cli::cli_warn(
      "parameter {.val {paste(containerPath, parameterName, sep = '|')}} not found; no-op."
    )
    return(list(parameters = parameters, removed = FALSE))
  }
  parameters <- parameters[-idx]
  if (length(parameters) == 0L) {
    return(list(parameters = NULL, removed = TRUE))
  }
  list(parameters = parameters, removed = TRUE)
}

# Locate a `(containerPath, parameterName)` entry in a parameter-set
# array-of-records. Returns an integer index or `integer(0)` if absent.
#
# @keywords internal
# @noRd
.findParameterEntryIndex <- function(
  parameters,
  containerPath,
  parameterName
) {
  if (is.null(parameters) || length(parameters) == 0L) {
    return(integer(0))
  }
  hits <- vapply(
    parameters,
    function(e) {
      identical(e$containerPath, containerPath) &&
        identical(e$parameterName, parameterName)
    },
    logical(1)
  )
  which(hits)
}

# Public CRUD: initialConditions ----

#' Create one or more initial-condition sets
#'
#' Adds empty initial-condition sets to the project's `initialConditions`
#' section, vectorizing over a vector of ids (all N added in one write-through).
#' An initial-condition set holds molecule start values (`path`, `value`,
#' `unit`) a scenario applies through its `initialConditions` field, distinct
#' from parameter sets (which set model parameters, not molecule start values).
#'
#' @inherit vectorizedAuthoring details
#'
#' @param project A `Project` object.
#' @param id Character vector of set ids. Each is canonicalized to a safe,
#'   lowercase id (a warning names the result if it changed); each canonical
#'   id must not already exist.
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
addInitialConditions <- function(project, id) {
  validateIsOfType(project, "Project")
  .assertIdVector(id)
  id <- .canonicalizeId(id)
  .assertNoDuplicateIds(id, "initial-condition set")
  clash <- intersect(id, names(project$initialConditions))
  if (length(clash) > 0L) {
    cli::cli_abort("initial-condition set {.val {clash}} already exists")
  }
  initialConditions <- project$.getSection("initialConditions") %||% list()
  for (one in id) {
    initialConditions[[one]] <- .asInitialConditionSet(list())
  }
  project$.setSection("initialConditions", initialConditions)
  invisible(project)
}

#' Remove one or more initial-condition sets
#'
#' Drop the initial-condition sets with matching ids in one write-through.
#' Warns (and skips) any id not present, and warns when a removed set is still
#' referenced.
#'
#' @inherit vectorizedAuthoring details
#' @inheritParams addInitialConditions
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
removeInitialConditions <- function(project, id) {
  validateIsOfType(project, "Project")
  .assertIdVector(id)
  id <- .canonicalizeId(id)
  missingIds <- setdiff(id, names(project$initialConditions))
  if (length(missingIds) > 0L) {
    cli::cli_warn("initial-condition set {.val {missingIds}} not found; no-op.")
  }
  toRemove <- intersect(id, names(project$initialConditions))
  if (length(toRemove) == 0L) {
    return(invisible(project))
  }
  for (one in toRemove) {
    .warnIfReferenced(project, "initialConditions", one)
  }
  initialConditions <- project$.getSection("initialConditions")
  initialConditions[toRemove] <- NULL
  project$.setSection("initialConditions", initialConditions)
  invisible(project)
}

#' Add one or many entries to a named initial-condition set
#'
#' Adds molecule start-value entries to the named set in
#' `project$initialConditions`. `path`, `value`, and `unit` accept parallel
#' vectors of equal length N to add all N entries in a single call (and a
#' single write to disk); a scalar call (length-1 vectors) adds one entry.
#' Building a large set with one vectorized call is far cheaper than a loop of
#' scalar calls, since each call rewrites the whole set file.
#'
#' Unlike the other `add*` functions, which abort on a missing parent, this
#' creates the parent set on demand if it does not yet exist (informing you
#' when it does). Last-write-wins on a duplicate `path`, including duplicates
#' within a single vectorized call.
#'
#' @param project A `Project` object.
#' @param id Character scalar, set id. Canonicalized; created if not present.
#' @param path Character vector of molecule paths (length N).
#' @param value Numeric vector of start values (length N).
#' @param unit Character vector of units (length N). A unit is mandatory for a
#'   molecule start value; a blank unit is rejected.
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
addInitialConditionEntry <- function(project, id, path, value, unit) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1L || is.na(id) || nchar(id) == 0) {
    cli::cli_abort("{.arg id} must be a non-empty string")
  }
  id <- .canonicalizeId(id)
  # Validate the batch shape up front so a mismatched call fails fast, before
  # any on-demand set creation is reported.
  .assertInitialConditionEntryVectorLengths(path, value, unit)
  current <- project$initialConditions[[id]]
  # Unlike the other `add*` functions, this creates its parent set on demand
  # rather than aborting on a missing parent. Inform the user when it does, so
  # the on-demand creation is not silent.
  if (is.null(current)) {
    cli::cli_inform(
      "Created initial-condition set {.val {id}} on demand to hold the new entr{?y/ies}."
    )
  }
  # Fold all N entries into the set in memory first, so the single write below
  # triggers exactly one write-through (not one per entry).
  initialConditions <- project$.getSection("initialConditions")
  initialConditions[[id]] <- .asInitialConditionSet(.addInitialConditionEntries(
    current,
    path,
    value,
    unit
  ))
  project$.setSection("initialConditions", initialConditions)
  invisible(project)
}

#' Remove one or many entries from a named initial-condition set
#'
#' Removes molecule start-value entries from the named set. `path` accepts a
#' vector of length N to remove all N entries in a single call (and a single
#' write to disk); a scalar call (length-1 vector) removes one entry. If every
#' entry of the set is removed, the set itself is auto-removed from
#' `project$initialConditions`. Warns if the set or any named entry doesn't
#' exist.
#'
#' @param project A `Project` object.
#' @param id Character scalar, set id. Canonicalized.
#' @param path Character vector of molecule paths (length N).
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
removeInitialConditionEntry <- function(project, id, path) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1L) {
    cli::cli_abort("{.arg id} must be a string scalar")
  }
  id <- .canonicalizeId(id)
  if (!(id %in% names(project$initialConditions))) {
    cli::cli_warn("initial-condition set {.val {id}} not found; no-op.")
    return(invisible(project))
  }
  # Fold all N removals into the set in memory first, so the single assignment
  # below triggers exactly one write-through (not one per entry).
  result <- .removeInitialConditionEntries(
    project$initialConditions[[id]],
    path
  )
  if (!result$removed) {
    return(invisible(project))
  }
  initialConditions <- project$.getSection("initialConditions")
  if (is.null(result$entries)) {
    .warnIfReferenced(project, "initialConditions", id)
    initialConditions[[id]] <- NULL
  } else {
    initialConditions[[id]] <- .asInitialConditionSet(result$entries)
  }
  project$.setSection("initialConditions", initialConditions)
  invisible(project)
}

# Validate scalar inputs for a `(path, value, unit)` initial-condition entry.
# Returns a character vector of error messages (empty if validation passes).
#
# @keywords internal
# @noRd
.validateInitialConditionEntryArgs <- function(path, value, unit) {
  errors <- character()
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      nchar(path) == 0
  ) {
    errors <- c(errors, "path must be a non-empty string")
  }
  if (!is.numeric(value) || length(value) != 1L || is.na(value)) {
    errors <- c(errors, "value must be a numeric scalar")
  }
  # A unit is mandatory for a molecule start value: `ospsuite::setQuantityValuesByPath()`
  # rejects a blank unit at run time, so reject it here (fail fast at authoring)
  # rather than deferring an opaque failure to the simulation.
  if (
    !is.character(unit) ||
      length(unit) != 1L ||
      is.na(unit) ||
      nchar(unit) == 0L
  ) {
    errors <- c(errors, "unit must be a non-empty string")
  }
  errors
}

# Assert that the parallel `(path, value, unit)` argument vectors all share the
# same length (the batch size). A scalar call is the length-1 case. Aborts
# naming the lengths otherwise.
#
# @keywords internal
# @noRd
.assertInitialConditionEntryVectorLengths <- function(path, value, unit) {
  lengths <- c(
    path = length(path),
    value = length(value),
    unit = length(unit)
  )
  if (length(unique(lengths)) != 1L) {
    cli::cli_abort(c(
      "{.arg path}, {.arg value}, and {.arg unit} must be vectors of the \\
      same length.",
      "x" = "Got lengths {.val {lengths[['path']]}}, \\
      {.val {lengths[['value']]}}, and {.val {lengths[['unit']]}}."
    ))
  }
  invisible(lengths[["path"]])
}

# Fold N initial-condition entries (parallel vectors) into a set in one pass,
# returning the updated list. Each entry is validated and appended (or replaced,
# last-write-wins) via `.addInitialConditionEntry`, so an in-batch duplicate
# `path` resolves to the last value, matching the scalar semantics. N=1 is the
# scalar case. Folding in memory first lets the caller persist the whole set in
# a single write-through.
#
# @keywords internal
# @noRd
.addInitialConditionEntries <- function(entries, path, value, unit) {
  n <- .assertInitialConditionEntryVectorLengths(path, value, unit)
  for (i in seq_len(n)) {
    entries <- .addInitialConditionEntry(
      entries,
      path[[i]],
      value[[i]],
      unit[[i]]
    )
  }
  entries
}

# Append (or replace, last-write-wins) one initial-condition entry to a
# JSON-faithful array-of-records set. `entries` is a list of
# `list(path, value, unit)` entries (or `NULL`); returns the updated list.
#
# @keywords internal
# @noRd
.addInitialConditionEntry <- function(entries, path, value, unit) {
  errors <- .validateInitialConditionEntryArgs(path, value, unit)
  if (length(errors) > 0L) {
    cli::cli_abort(c(
      "Invalid initial-condition entry:",
      stats::setNames(errors, rep("x", length(errors)))
    ))
  }

  if (is.null(entries)) {
    entries <- list()
  }

  existingIdx <- .findInitialConditionEntryIndex(entries, path)
  # A unit is mandatory (the validator above rejects a blank one), so it is
  # always a real string here, distinct from a parameter entry's optional units.
  newEntry <- list(
    path = path,
    value = as.double(value),
    unit = unit
  )
  if (length(existingIdx) > 0L) {
    entries[[existingIdx]] <- newEntry
  } else {
    entries[[length(entries) + 1L]] <- newEntry
  }
  entries
}

# Drop N initial-condition entries (parallel vector) from a set in one pass,
# returning the same `list(entries=, removed=)` shape as
# `.removeInitialConditionEntry`. `removed` is `TRUE` if ANY named entry was
# actually removed (a not-found entry warns and is skipped, as in the scalar
# case); `entries` is `NULL` when the removals emptied the set, so the caller
# auto-removes it. N=1 is the scalar case. Folding in memory first lets the
# caller persist the whole set in a single write-through.
#
# @keywords internal
# @noRd
.removeInitialConditionEntries <- function(entries, path) {
  anyRemoved <- FALSE
  for (i in seq_along(path)) {
    result <- .removeInitialConditionEntry(entries, path[[i]])
    anyRemoved <- anyRemoved || result$removed
    # A removal that empties the set yields `NULL`; preserve it so a later
    # not-found entry warns against the empty set, matching the scalar path.
    entries <- result$entries
  }
  list(entries = entries, removed = anyRemoved)
}

# Drop one initial-condition entry from a JSON-faithful array-of-records set.
# Returns a list with:
#   - `entries`: the updated set, or `NULL` if removal emptied the set (callers
#     use the `NULL` to auto-remove the named set).
#   - `removed`: `TRUE` if an entry was actually removed, `FALSE` for a no-op
#     (entry not found). Callers gate the write-through on this so a no-op warn
#     doesn't touch the section (and so doesn't invalidate the validation cache).
#
# @keywords internal
# @noRd
.removeInitialConditionEntry <- function(entries, path) {
  if (is.null(entries) || length(entries) == 0L) {
    cli::cli_warn("initial condition {.val {path}} not found; no-op.")
    return(list(entries = entries, removed = FALSE))
  }
  idx <- .findInitialConditionEntryIndex(entries, path)
  if (length(idx) == 0L) {
    cli::cli_warn("initial condition {.val {path}} not found; no-op.")
    return(list(entries = entries, removed = FALSE))
  }
  entries <- entries[-idx]
  if (length(entries) == 0L) {
    return(list(entries = NULL, removed = TRUE))
  }
  list(entries = entries, removed = TRUE)
}

# Locate a `path` entry in an initial-condition array-of-records. Returns an
# integer index or `integer(0)` if absent.
#
# @keywords internal
# @noRd
.findInitialConditionEntryIndex <- function(entries, path) {
  if (is.null(entries) || length(entries) == 0L) {
    return(integer(0))
  }
  hits <- vapply(
    entries,
    function(e) identical(e$path, path),
    logical(1)
  )
  which(hits)
}


#' Validate parameter list structure
#'
#' @param parameterStructure Object to be checked. Expected is a named list with
#'   names "paths", "values", and "units".
#'
#' @keywords internal
#' @returns `TRUE` if validation succeeded (silently). Throws an error otherwise.
.validateParametersStructure <- function(
  parameterStructure,
  argumentName = NULL,
  nullAllowed = FALSE
) {
  if (is.null(parameterStructure) && nullAllowed) {
    return(invisible(TRUE))
  }

  if (!identical(names(parameterStructure), c("paths", "values", "units"))) {
    cli::cli_abort(messages$wrongParametersStructure(
      argumentName = argumentName
    ))
  }
  return(invisible(TRUE))
}
