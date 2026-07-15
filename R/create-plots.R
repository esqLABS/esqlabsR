# Plot generation ----
#
# Public createPlots / createPlotsFromExcel plus the parsing, validation,
# and configuration helpers they use. Stateless: pulls a Project's
# dataCombined / plots / plotGrids sections and returns a named list of
# rendered plot-grid (and optionally standalone plot) objects keyed by id.

#' Generate plots from a Project
#'
#' @description
#' **Returns plot grids, not standalone plots.** By default `createPlots()`
#' builds every plot grid declared in `project$plotGrids` and no standalone
#' plots, and hands back a **named list of plot grids keyed by Plot Grid name**
#' (the `plots` argument opts individual standalone plots into that same list).
#'
#' Reads `project$plots` and `project$plotGrids` (both keyed lists, one entry
#' per plot / grid) to build the requested plot grids and, optionally,
#' standalone single plots. DataCombined objects are resolved via
#' [createDataCombined()] internally unless supplied via `dataCombinedList`.
#'
#' With neither `plotGrids` nor `plots`, all plot grids declared in
#' `project$plotGrids` are built (the default). The two arguments are
#' independent selectors: `plotGrids` selects plot grids (keyed by
#' `plotGridId` in the result), `plots` selects standalone single plots
#' (keyed by `plotId`). A `plotId` that is also part of a requested grid still
#' gets its own standalone entry.
#'
#' @param project A `Project` (see [loadProject()]).
#' @param plotGrids Names of plot grids to build. If `NULL` (default) and
#'   `plots` is also `NULL`, all grids declared in `project$plotGrids` are
#'   built.
#' @param plots Ids of standalone single plots to render directly (not laid
#'   out in a grid), each resolved against `project$plots`. `NULL` (default)
#'   renders no standalone plots; standalone plots are opt-in.
#' @param scenarioResults Named list of Scenario Results from
#'   [runScenarios()] (each entry has `simulation`, `results`,
#'   `outputValues`, `population`). Not the OSPS `SimulationResults`.
#' @param dataCombinedList Optional pre-built named list of `DataCombined`
#'   objects. If `NULL`, the function builds them via [createDataCombined()].
#' @param stopIfNotFound If `TRUE`, errors when a referenced DataCombined or
#'   simulated/observed entry cannot be resolved, or when a requested
#'   `plotGrids` / `plots` id is not defined in the project.
#' @param validate Logical. If `TRUE` (default), runs the relevant
#'   section validators via [validateProject()] before building the
#'   plots and aborts with a formatted summary on critical errors. Set
#'   to `FALSE` to skip the pre-flight check (e.g. when the caller has
#'   already validated the project).
#'
#' @returns A named list of **plot grids** keyed by Plot Grid name: one entry
#'   per requested plot grid (keyed by its `plotGridId`), unioned with one entry
#'   per requested standalone plot (keyed by its `plotId`) when `plots` is
#'   given. Note the list holds plot grids, not standalone `Plot` objects,
#'   unless standalone plots were explicitly requested via `plots`. An empty
#'   list when the project has no plots to build.
#'
#' @import tidyr
#'
#' @export
createPlots <- function(
  project,
  plotGrids = NULL,
  plots = NULL,
  scenarioResults = NULL,
  dataCombinedList = NULL,
  stopIfNotFound = TRUE,
  validate = TRUE
) {
  validateIsOfType(project, "Project")
  if (isTRUE(validate)) {
    .ensureValid(
      project,
      sections = c("plots", "scenarios", "observedData", "crossReferences"),
      opName = "createPlots"
    )
  }
  allPlotConfig <- .unwrapDefinitionList(project$plots) %||% list()
  allPlotGrids <- .unwrapDefinitionList(project$plotGrids) %||% list()

  # Only default to "all grids" when neither selector is given. A caller that
  # asks only for standalone `plots` should not also get every grid.
  requestSpecified <- !is.null(plotGrids) || !is.null(plots)
  if (!requestSpecified) {
    plotGrids <- names(allPlotGrids)
  }

  # Surface unknown requested ids when the caller asked us to stop on
  # unresolved references (mirrors the per-kind lookups below).
  if (isTRUE(stopIfNotFound)) {
    unknownGrids <- setdiff(plotGrids, names(allPlotGrids))
    if (length(unknownGrids) > 0) {
      cli::cli_abort(messages$stopPlotGridNamesNotFound(unknownGrids))
    }
    unknownPlots <- setdiff(plots, names(allPlotConfig))
    if (length(unknownPlots) > 0) {
      cli::cli_abort(messages$stopPlotIdsNotFound(unknownPlots))
    }
  }

  # Filter to only the requested grids and the plot configs they reference,
  # plus the explicitly requested standalone plots. Scoping the build to what
  # is asked for keeps validation and DataCombined building from touching plots
  # whose DataCombined is not built (which would abort).
  selectedGrids <- allPlotGrids[intersect(names(allPlotGrids), plotGrids)]
  selectedPlotIds <- intersect(plots, names(allPlotConfig))

  gridReferencedPlotIds <- unique(unlist(lapply(
    selectedGrids,
    function(g) .splitPlotIDs(g$plotIds)
  )))
  neededPlotIds <- union(gridReferencedPlotIds, selectedPlotIds)
  plotConfig <- allPlotConfig[intersect(names(allPlotConfig), neededPlotIds)]

  if (length(selectedGrids) == 0 && length(selectedPlotIds) == 0) {
    return(list())
  }

  # Build DataCombined for the configs referenced by both selectors: the
  # requested grids and the explicitly requested standalone plots.
  if (is.null(dataCombinedList)) {
    standaloneDataCombinedNames <- unique(unlist(lapply(
      plotConfig[selectedPlotIds],
      function(p) p$dataCombinedId
    )))
    dataCombinedList <- createDataCombined(
      project,
      dataCombined = standaloneDataCombinedNames,
      plotGrids = plotGrids,
      scenarioResults = scenarioResults,
      stopIfNotFound = stopIfNotFound
    )
  }

  .createPlotGridsFromEntries(
    plotConfigurations = plotConfig,
    plotGrids = selectedGrids,
    standalonePlotIds = selectedPlotIds,
    dataCombinedList = dataCombinedList
  )
}

#' @rdname createPlots
#' @export
createPlotsFromExcel <- function(...) {
  lifecycle::deprecate_soft(
    what = "createPlotsFromExcel()",
    with = "createPlots()",
    when = "6.0.0"
  )
  createPlots(...)
}

#' Parse and validate comma-separated Excel field
#'
#' Parses comma-separated values from Excel and validates using ospsuite.utils.
#' Provides Excel-specific error context (plotId, field name) for common issues.
#'
#' @param value Raw value from Excel cell
#' @param fieldName Name of the field for error messages
#' @param plotID Optional plot ID for error context
#' @param expectedLength Expected number of values (NULL for any length)
#' @param expectedType Expected type ("numeric" or "character")
#' @returns Parsed and validated vector
#' @keywords internal
.parseExcelMultiValueField <- function(
  value,
  fieldName,
  plotID = NULL,
  expectedLength = NULL,
  expectedType = "numeric"
) {
  originalValue <- value

  # Parse using scan (existing method)
  parsed <- unlist(trimws(scan(
    text = as.character(value),
    what = "character",
    sep = ",",
    quiet = TRUE
  )))

  # Detect common error: space-separated instead of comma-separated
  if (!is.null(expectedLength) && length(parsed) != expectedLength) {
    # Check if might be space-separated
    spaceSplit <- unlist(strsplit(trimws(as.character(originalValue)), "\\s+"))
    if (length(spaceSplit) == expectedLength) {
      # Check if all parts look numeric (for numeric fields)
      if (expectedType == "numeric") {
        numericTest <- suppressWarnings(as.numeric(spaceSplit))
        if (!any(is.na(numericTest))) {
          # User likely used spaces instead of commas
          msg <- messages$excelFieldFormatError(
            fieldName,
            originalValue,
            plotID,
            "comma-separated"
          )
          cli::cli_abort("{msg}")
        }
      }
    }
  }

  # Validate length using ospsuite.utils
  if (!is.null(expectedLength)) {
    tryCatch(
      ospsuite.utils::validateIsOfLength(parsed, expectedLength),
      error = function(e) {
        msg <- messages$excelFieldLengthError(
          fieldName,
          originalValue,
          plotID,
          expectedLength,
          length(parsed)
        )
        cli::cli_abort("{msg}")
      }
    )
  }

  # Validate type and convert if needed
  if (expectedType == "numeric") {
    numericParsed <- suppressWarnings(as.numeric(parsed))
    tryCatch(
      ospsuite.utils::validateIsNumeric(numericParsed),
      error = function(e) {
        msg <- messages$excelFieldTypeError(
          fieldName,
          originalValue,
          plotID,
          "numeric"
        )
        cli::cli_abort("{msg}")
      }
    )
    return(numericParsed)
  }

  return(parsed)
}

#' Validate that log scale axes do not have limits containing zero
#'
#' @param plotConfiguration A plot configuration object
#' @param plotID Optional plot ID for the warning message
#'
#' @keywords internal
#' @noRd
.validateLogScaleAxisLimits <- function(plotConfiguration, plotID = NULL) {
  axisChecks <- list(
    list(
      scale = "xAxisScale",
      limits = c("xAxisLimits", "xValuesLimits"),
      axis = "x"
    ),
    list(
      scale = "yAxisScale",
      limits = c("yAxisLimits", "yValuesLimits"),
      axis = "y"
    )
  )

  for (check in axisChecks) {
    scaleValue <- plotConfiguration[[check$scale]]
    if (!is.null(scaleValue) && scaleValue == "log") {
      for (limitsField in check$limits) {
        limitsValue <- plotConfiguration[[limitsField]]
        if (!is.null(limitsValue) && 0 %in% limitsValue) {
          msg <- messages$warningLogScaleWithZeroLimit(
            plotID = plotID,
            axisLimitsField = limitsField,
            axis = check$axis
          )
          cli::cli_warn("{msg}")
        }
      }
    }
  }
}

#' Create a plotConfiguration or plotGridConfiguration object from a keyed
#' list of plot / grid fields
#'
#' @param defaultConfiguration default plotConfiguration or
#'   plotGridConfiguration to clone and customize.
#' @param fields Named list of configuration properties (one per field). An
#'   absent optional field is simply absent from the list (no NA cell).
#' @returns A customized plot- or plotGridConfiguration object.
#' @keywords internal
#' @noRd
.createConfigurationFromEntry <- function(defaultConfiguration, fields) {
  newConfiguration <- defaultConfiguration$clone()
  for (colName in names(fields)) {
    value <- fields[[colName]]
    if (is.null(value) || (length(value) == 1L && is.na(value))) {
      next
    }
    # Check if the field name is supported by the configuration class
    if (!.validateClassHasField(object = newConfiguration, field = colName)) {
      msg <- messages$invalidConfigurationPropertyFromExcel(
        propertyName = colName,
        configurationType = class(newConfiguration)[[1]]
      )
      cli::cli_abort("{msg}")
    }
    # Special treatment for axis limits - parse and validate early with clear errors
    if (
      colName %in%
        c(
          "xAxisLimits",
          "yAxisLimits",
          "xValuesLimits",
          "yValuesLimits"
        )
    ) {
      # Use wrapper function with ospsuite.utils validation
      value <- .parseExcelMultiValueField(
        value = value,
        fieldName = colName,
        plotID = fields[["plotId"]],
        expectedLength = 2,
        expectedType = "numeric"
      )
      # Set directly (already validated and converted)
      newConfiguration[[colName]] <- value
    } else {
      # For other fields, use existing logic
      # For fields that require multiple values, values are separated by a ','.
      # Alternatively, the values can be enclosed in "" in case the title should contain a ','.
      # Split the input string by ',' but do not split within ""
      value <- unlist(trimws(scan(
        text = as.character(value),
        what = "character",
        sep = ",",
        quiet = TRUE
      )))

      # Expected type of the field to cast the value to the
      # correct type. For fields that do not have a default value (NULL), we have
      # to assume character until a better solution is found
      expectedType <- "character"
      # Try to get the expected type of the field from the default value
      defVal <- newConfiguration[[colName]]
      if (!is.null(defVal)) {
        expectedType <- typeof(defVal)
      }

      # Caste the value and set it
      newConfiguration[[colName]] <- methods::as(
        object = value,
        Class = expectedType
      )
    }
  }

  return(newConfiguration)
}

#' Check that an object has a named field
#'
#' Check if the `object` contains an active binding with the name `field`
#'
#' @param object A class or an instance of a class to check
#' @param field Name of the field
#'
#' @returns `TRUE` if the `object` has an active binding `field`, `FALSE`
#'   otherwise.
#' @keywords internal
.validateClassHasField <- function(object, field) {
  if (!(field %in% names(object))) {
    return(FALSE)
  }
  return(TRUE)
}

#' Validate the plotConfiguration sheet read from Excel
#' Validate the keyed-list plot configurations before building.
#'
#' Mirrors the checks the lazy `.validatePlots()` runs (and so is redundant
#' when `createPlots(validate = TRUE)`), but guards the build path itself:
#' every plot must declare `dataCombinedId` and `plotType`, `plotId` must be
#' unique, and every referenced `dataCombinedId` must be in the built list.
#'
#' @param plotConfigurations Keyed list (one entry per plot).
#' @param dataCombinedNames Ids of the DataCombined that are built and
#'   available to reference.
#' @keywords internal
#' @noRd
.assertPlotConfigurationsBuildable <- function(
  plotConfigurations,
  dataCombinedNames
) {
  ids <- vapply(
    plotConfigurations,
    function(p) p$plotId %||% NA_character_,
    character(1)
  )
  dataCombinedIds <- lapply(plotConfigurations, function(p) p$dataCombinedId)
  plotTypes <- lapply(plotConfigurations, function(p) p$plotType)

  # Reject any plot missing its `plotId` up front. The duplicate check below
  # only catches two-or-more missing ids (`duplicated(c(NA, NA))` flags the
  # second); a single id-less entry would otherwise slip through and fail
  # opaquely at `configPlotIds <- vapply(..., function(p) p$plotId, ...)` in
  # `.createPlotGridsFromEntries()`. Mirrors the `plotGridId` guard in
  # `.assertPlotGridsBuildable()`.
  if (any(vapply(plotConfigurations, function(p) is.null(p$plotId), logical(1)))) {
    msg <- messages$missingPlotId()
    cli::cli_abort("{msg}")
  }
  if (any(vapply(dataCombinedIds, is.null, logical(1)))) {
    msg <- messages$missingDataCombinedName()
    cli::cli_abort("{msg}")
  }
  duplicatedPlotIds <- ids[duplicated(ids)]
  if (length(duplicatedPlotIds) > 0) {
    msg <- messages$PlotIDsMustBeUnique(duplicatedPlotIds)
    cli::cli_abort("{msg}")
  }
  if (any(vapply(plotTypes, is.null, logical(1)))) {
    msg <- messages$missingPlotType()
    cli::cli_abort("{msg}")
  }
  # Reject any plotType that is not one of the supported kinds. The build
  # `switch()` has no default arm, so an unknown type would otherwise return
  # NULL invisibly (silently dropped from a grid). Aborting here runs before
  # the build regardless of `validate`, naming the offending plot and type.
  invalidTypeIdx <- which(!(unlist(plotTypes) %in% .validPlotTypes))
  if (length(invalidTypeIdx) > 0) {
    badId <- ids[[invalidTypeIdx[[1]]]]
    badType <- plotTypes[[invalidTypeIdx[[1]]]]
    cli::cli_abort(c(
      "Invalid {.field plotType} {.val {badType}} for plot {.val {badId}}.",
      "i" = "Must be one of: {.val {(.validPlotTypes)}}."
    ))
  }
  missingDataCombined <- setdiff(
    unlist(dataCombinedIds),
    dataCombinedNames
  )
  if (length(missingDataCombined) != 0) {
    msg <- messages$stopInvalidDataCombinedName(missingDataCombined)
    cli::cli_abort("{msg}")
  }
  invisible(plotConfigurations)
}

#' Validate the keyed-list plot grids before building.
#'
#' Every grid must declare `plotIds`, `plotGridId` must be unique, and every
#' referenced plot id must be defined.
#'
#' @param plotGrids Keyed list (one entry per grid).
#' @param plotIDs Ids of the plots that are referenced in the plot grids.
#' @keywords internal
#' @noRd
.assertPlotGridsBuildable <- function(plotGrids, plotIDs) {
  if (any(vapply(plotGrids, function(g) is.null(g$plotIds), logical(1)))) {
    msg <- messages$missingPlotIDs()
    cli::cli_abort("{msg}")
  }
  if (any(vapply(plotGrids, function(g) is.null(g$plotGridId), logical(1)))) {
    msg <- messages$missingPlotGridId()
    cli::cli_abort("{msg}")
  }
  gridIds <- vapply(
    plotGrids,
    function(g) g$plotGridId %||% NA_character_,
    character(1)
  )
  duplicatedGridIds <- gridIds[duplicated(gridIds)]
  if (length(duplicatedGridIds) > 0) {
    msg <- messages$PlotGridsNamesMustBeUnique(duplicatedGridIds)
    cli::cli_abort("{msg}")
  }
  referencedPlotIds <- unique(unlist(lapply(
    plotGrids,
    function(g) .splitPlotIDs(g$plotIds)
  )))
  missingPlots <- setdiff(referencedPlotIds, plotIDs)
  if (length(missingPlots) != 0) {
    msg <- messages$errorInvalidPlotID(missingPlots)
    cli::cli_abort("{msg}")
  }
  invisible(plotGrids)
}

#' Build named list of plot-grid (and optionally standalone plot) objects from
#' keyed-list configurations.
#'
#' Used by `createPlots(project, ...)`.
#'
#' @param plotConfigurations keyed list (one entry per plot), each entry a
#'   named list with `plotId`, `dataCombinedId`, `plotType`, `title`,
#'   `subtitle`, plus axis/styling fields. Callers must pass only plots whose
#'   `dataCombinedId` is built in `dataCombinedList`;
#'   `.assertPlotConfigurationsBuildable()` aborts on any `dataCombinedId`
#'   missing from the list.
#' @param plotGrids keyed list (one entry per grid), each entry a named list
#'   with `plotGridId`, `plotIds` (comma-separated string), `title`.
#' @param standalonePlotIds Character vector of plot ids to additionally
#'   return as standalone single plots (each keyed by its `plotId` in the
#'   result).
#' @param dataCombinedList named list of DataCombined objects keyed by name.
#'
#' @noRd
.createPlotGridsFromEntries <- function(
  plotConfigurations,
  plotGrids,
  standalonePlotIds = character(),
  dataCombinedList
) {
  .assertPlotConfigurationsBuildable(
    plotConfigurations,
    names(dataCombinedList)
  )
  configPlotIds <- vapply(
    plotConfigurations,
    function(p) p$plotId,
    character(1)
  )
  .assertPlotGridsBuildable(plotGrids, unique(configPlotIds))

  styleFields <- c(
    "plotId",
    "dataCombinedId",
    "plotType",
    "title",
    "subtitle",
    "xLabel",
    "yLabel",
    "aggregation",
    "quantiles",
    "nsd",
    "foldDistance"
  )

  defaultPlotConfiguration <- createEsqlabsPlotConfiguration()
  plotConfigurationList <- lapply(plotConfigurations, function(entry) {
    plotConfiguration <- .createConfigurationFromEntry(
      defaultConfiguration = defaultPlotConfiguration,
      fields = entry[!(names(entry) %in% styleFields)]
    )
    # Free-text scalar fields are excluded from `styleFields` and re-applied
    # here verbatim, not through `.createConfigurationFromEntry`, so a label
    # containing a comma (e.g. "Concentration, ng/mL") is not shredded into a
    # character vector by the comma-splitting scan.
    if (!is.null(entry$title)) {
      plotConfiguration$title <- entry$title
    }
    if (!is.null(entry$subtitle)) {
      plotConfiguration$subtitle <- entry$subtitle
    }
    if (!is.null(entry$xLabel)) {
      plotConfiguration$xLabel <- entry$xLabel
    }
    if (!is.null(entry$yLabel)) {
      plotConfiguration$yLabel <- entry$yLabel
    }
    .validateLogScaleAxisLimits(plotConfiguration, entry$plotId)
    plotConfiguration
  })
  names(plotConfigurationList) <- configPlotIds

  plotList <- lapply(plotConfigurations, function(entry) {
    plotId <- entry$plotId
    dataCombined <- dataCombinedList[[entry$dataCombinedId]]
    switch(
      entry$plotType,
      individual = plotIndividualTimeProfile(
        dataCombined,
        plotConfigurationList[[plotId]]
      ),
      population = {
        args <- list()
        args$dataCombined <- dataCombined
        args$defaultPlotConfiguration <- plotConfigurationList[[plotId]]
        if (!is.null(entry$aggregation)) {
          args$aggregation <- entry$aggregation
        }
        if (!is.null(entry$quantiles)) {
          args$quantiles <- as.numeric(unlist(
            strsplit(entry$quantiles, split = ",")
          ))
        }
        if (!is.null(entry$nsd)) {
          args$nsd <- as.numeric(entry$nsd)
        }
        do.call(plotPopulationTimeProfile, args)
      },
      observedVsSimulated = {
        if (is.null(entry$foldDistance)) {
          plotObservedVsSimulated(
            dataCombined,
            plotConfigurationList[[plotId]]
          )
        } else {
          plotObservedVsSimulated(
            dataCombined,
            plotConfigurationList[[plotId]],
            foldDistance = as.numeric(unlist(
              strsplit(entry$foldDistance, split = ",")
            ))
          )
        }
      },
      residualsVsSimulated = plotResidualsVsSimulated(
        dataCombined,
        plotConfigurationList[[plotId]]
      ),
      residualsVsTime = plotResidualsVsTime(
        dataCombined,
        plotConfigurationList[[plotId]]
      )
    )
  })
  names(plotList) <- configPlotIds

  defaultPlotGridConfig <- createEsqlabsPlotGridConfiguration()
  gridStyleFields <- c("plotGridId", "plotIds", "title")
  builtGrids <- lapply(plotGrids, function(entry) {
    plotGridConfiguration <- .createConfigurationFromEntry(
      defaultConfiguration = defaultPlotGridConfig,
      fields = entry[!(names(entry) %in% gridStyleFields)]
    )
    if (!is.null(entry$title)) {
      plotGridConfiguration$title <- entry$title
    }
    plotsToAdd <- plotList[intersect(
      .splitPlotIDs(entry$plotIds),
      configPlotIds
    )]
    plotsToAdd <- plotsToAdd[lengths(plotsToAdd) != 0]
    if (length(plotsToAdd) == 0) {
      return(NULL)
    }
    if (length(plotsToAdd) == 1) {
      plotGridConfiguration$tagLevels <- NULL
    }
    plotGridConfiguration$addPlots(plots = plotsToAdd)
    plotGrid(plotGridConfiguration)
  })
  names(builtGrids) <- vapply(plotGrids, function(g) g$plotGridId, character(1))

  # Standalone single plots: return the rendered plot for each requested id
  # (the same render a grid cell gets), keyed by `plotId`. A `plotId` that is
  # also inside a requested grid still gets its own entry here (independent
  # selectors). The grid entries and the standalone entries are unioned.
  standalonePlots <- plotList[intersect(standalonePlotIds, names(plotList))]
  # Drop any NULL entry for symmetry with the grid path. The plotType-enum
  # check in `.assertPlotConfigurationsBuildable()` already prevents unknown
  # types from producing a NULL here, so this is belt-and-suspenders.
  standalonePlots <- standalonePlots[lengths(standalonePlots) != 0]
  c(builtGrids, standalonePlots)
}
