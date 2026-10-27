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
#' builds every plot grid declared in `plotGrids` definitions and no standalone
#' plots, and hands back a **named list of plot grids keyed by Plot Grid name**
#' (the `plots` argument opts individual standalone plots into that same list).
#'
#' Reads `plots` definitions and `plotGrids` definitions (both keyed lists, one entry
#' per plot / grid) to build the requested plot grids and, optionally,
#' standalone single plots. DataCombined objects are resolved via
#' [createDataCombined()] internally unless supplied via `dataCombinedList`.
#'
#' With neither `plotGrids` nor `plots`, all plot grids declared in
#' `plotGrids` definitions are built (the default). The two arguments are
#' independent selectors: `plotGrids` selects plot grids (keyed by
#' `id` in the result), `plots` selects standalone single plots
#' (keyed by `id`). A `id` that is also part of a requested grid still
#' gets its own standalone entry.
#'
#' @param project A `Project` (see [loadProject()]).
#' @param plotGrids Names of plot grids to build. If `NULL` (default) and
#'   `plots` is also `NULL`, all grids declared in `plotGrids` definitions are
#'   built.
#' @param plots Ids of standalone single plots to render directly (not laid
#'   out in a grid), each resolved against `plots` definitions. `NULL` (default)
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
#'   per requested plot grid (keyed by its `id`), unioned with one entry
#'   per requested standalone plot (keyed by its `id`) when `plots` is
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
    project$ensureValid(
      sections = c(
        "plots",
        "dataCombined",
        "scenarios",
        "observedData",
        "crossReferences"
      ),
      opName = "createPlots"
    )
  }
  allPlotConfig <- .unwrapDefinitionList(project$definitions$plots) %||% list()
  allPlotGrids <- .unwrapDefinitionList(project$definitions$plotGrids) %||%
    list()

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
      cli::cli_abort(messages$plotGridNamesNotFound(unknownGrids))
    }
    unknownPlots <- setdiff(plots, names(allPlotConfig))
    if (length(unknownPlots) > 0) {
      cli::cli_abort(messages$plotIdsNotFound(unknownPlots))
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
    function(g) .splitPlotIDs(g$plots)
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
      function(p) p$dataCombined
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
#' @param ... Passed on to [createPlots()].
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
#' Provides Excel-specific error context (id, field name) for common issues.
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
#' The axes of an `observedVsSimulated` plot are logarithmic by default.
#'
#' @param plotConfiguration A Plot definition (a named list); the limit fields
#'   hold their stored values (a comma-separated string or a number).
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

  isOvs <- identical(plotConfiguration[["plotType"]], "observedVsSimulated")
  for (check in axisChecks) {
    scaleValue <- plotConfiguration[[check$scale]]
    # An observedVsSimulated plot has one scale for both axes, logarithmic
    # unless a scale field says otherwise.
    if (isOvs) {
      scaleValue <- plotConfiguration[["xAxisScale"]] %||%
        plotConfiguration[["yAxisScale"]] %||%
        formals(ospsuite::plotPredictedVsObserved)$xyScale
    }
    if (!is.null(scaleValue) && scaleValue == "log") {
      for (limitsField in check$limits) {
        limitsValue <- if (!is.null(plotConfiguration[[limitsField]])) {
          .parsePlotFieldValue(
            plotConfiguration[[limitsField]],
            .plotFields[[limitsField]]
          )
        }
        if (!is.null(limitsValue) && 0 %in% limitsValue) {
          msg <- messages$logScaleWithZeroLimit(
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

#' Create a plotGridConfiguration object from a keyed list of grid fields
#'
#' @param defaultConfiguration default plotGridConfiguration to clone and
#'   customize.
#' @param fields Named list of configuration properties (one per field). An
#'   absent optional field is simply absent from the list (no NA cell).
#' @returns A customized plotGridConfiguration object.
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
    # For fields that require multiple values, values are separated by a ','.
    # Alternatively, the values can be enclosed in "" in case a value should
    # contain a ','. Split the input string by ',' but do not split within "".
    value <- unlist(trimws(scan(
      text = as.character(value),
      what = "character",
      sep = ",",
      quiet = TRUE
    )))

    # Expected type of the field to cast the value to the correct type. For
    # fields that do not have a default value (NULL), we have to assume
    # character until a better solution is found.
    expectedType <- "character"
    defVal <- newConfiguration[[colName]]
    if (!is.null(defVal)) {
      expectedType <- typeof(defVal)
    }
    newConfiguration[[colName]] <- methods::as(
      object = value,
      Class = expectedType
    )
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
#' every plot must declare `dataCombined` and `plotType`, `id` must be
#' unique, and every referenced `dataCombined` must be in the built list.
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
    function(p) p$id %||% NA_character_,
    character(1)
  )
  dataCombinedIds <- lapply(plotConfigurations, function(p) p$dataCombined)
  plotTypes <- lapply(plotConfigurations, function(p) p$plotType)

  # Reject any plot missing its `id` up front. The duplicate check below
  # only catches two-or-more missing ids (`duplicated(c(NA, NA))` flags the
  # second); a single id-less entry would otherwise slip through and fail
  # opaquely at `configPlotIds <- vapply(..., function(p) p$id, ...)` in
  # `.createPlotGridsFromEntries()`. Mirrors the `id` guard in
  # `.assertPlotGridsBuildable()`.
  if (any(vapply(plotConfigurations, function(p) is.null(p$id), logical(1)))) {
    msg <- messages$missingPlotId()
    cli::cli_abort("{msg}")
  }
  if (any(vapply(dataCombinedIds, is.null, logical(1)))) {
    msg <- messages$missingDataCombinedName()
    cli::cli_abort("{msg}")
  }
  duplicatedPlotIds <- ids[duplicated(ids)]
  if (length(duplicatedPlotIds) > 0) {
    msg <- messages$plotIDsMustBeUnique(duplicatedPlotIds)
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
  .abortOnPlotEntryFieldErrors(plotConfigurations)
  missingDataCombined <- setdiff(
    unlist(dataCombinedIds),
    dataCombinedNames
  )
  if (length(missingDataCombined) != 0) {
    msg <- messages$invalidDataCombinedName(missingDataCombined)
    cli::cli_abort("{msg}")
  }
  invisible(plotConfigurations)
}

#' Validate the keyed-list plot grids before building.
#'
#' Every grid must declare `plots`, `id` must be unique, and every
#' referenced plot id must be defined.
#'
#' @param plotGrids Keyed list (one entry per grid).
#' @param plotIDs Ids of the plots that are referenced in the plot grids.
#' @keywords internal
#' @noRd
.assertPlotGridsBuildable <- function(plotGrids, plotIDs) {
  if (any(vapply(plotGrids, function(g) is.null(g$plots), logical(1)))) {
    msg <- messages$missingPlotIDs()
    cli::cli_abort("{msg}")
  }
  if (any(vapply(plotGrids, function(g) is.null(g$id), logical(1)))) {
    msg <- messages$missingPlotGridId()
    cli::cli_abort("{msg}")
  }
  gridIds <- vapply(
    plotGrids,
    function(g) g$id %||% NA_character_,
    character(1)
  )
  duplicatedGridIds <- gridIds[duplicated(gridIds)]
  if (length(duplicatedGridIds) > 0) {
    msg <- messages$plotGridsNamesMustBeUnique(duplicatedGridIds)
    cli::cli_abort("{msg}")
  }
  referencedPlotIds <- unique(unlist(lapply(
    plotGrids,
    function(g) .splitPlotIDs(g$plots)
  )))
  missingPlots <- setdiff(referencedPlotIds, plotIDs)
  if (length(missingPlots) != 0) {
    msg <- messages$invalidPlotID(missingPlots)
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
#'   named list with `id`, `dataCombined`, `plotType`, plus the fields of
#'   `.plotFields`. Callers must pass only plots whose
#'   `dataCombined` is built in `dataCombinedList`;
#'   `.assertPlotConfigurationsBuildable()` aborts on any `dataCombined`
#'   missing from the list.
#' @param plotGrids keyed list (one entry per grid), each entry a named list
#'   with `id`, `plots` (comma-separated string), `title`.
#' @param standalonePlotIds Character vector of plot ids to additionally
#'   return as standalone single plots (each keyed by its `id` in the
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
    function(p) p$id,
    character(1)
  )
  .assertPlotGridsBuildable(plotGrids, unique(configPlotIds))

  plotList <- lapply(plotConfigurations, function(entry) {
    .validateLogScaleAxisLimits(entry, entry$id)
    .buildPlot(entry, dataCombinedList[[entry$dataCombined]])
  })
  names(plotList) <- configPlotIds

  defaultPlotGridConfig <- createEsqlabsPlotGridConfiguration()
  gridStyleFields <- c("id", "plots", "title", "subtitle")
  builtGrids <- lapply(plotGrids, function(entry) {
    plotGridConfiguration <- .createConfigurationFromEntry(
      defaultConfiguration = defaultPlotGridConfig,
      fields = entry[!(names(entry) %in% gridStyleFields)]
    )
    # Free-text scalar fields are excluded from `gridStyleFields` and re-applied
    # here verbatim, not through `.createConfigurationFromEntry`, so a value
    # containing a comma (e.g. "Model A, Model B") is not shredded into a
    # character vector by the comma-splitting scan.
    if (!is.null(entry$title)) {
      plotGridConfiguration$title <- entry$title
    }
    if (!is.null(entry$subtitle)) {
      plotGridConfiguration$subtitle <- entry$subtitle
    }
    plotsToAdd <- plotList[intersect(
      .splitPlotIDs(entry$plots),
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
  names(builtGrids) <- vapply(plotGrids, function(g) g$id, character(1))

  # Standalone single plots: return the rendered plot for each requested id
  # (the same render a grid cell gets), keyed by `id`. A `id` that is
  # also inside a requested grid still gets its own entry here (independent
  # selectors). The grid entries and the standalone entries are unioned.
  standalonePlots <- plotList[intersect(standalonePlotIds, names(plotList))]
  # A plot without data to draw is NULL (the `ospsuite` plot function warns);
  # drop it, as the grid path does.
  standalonePlots <- standalonePlots[lengths(standalonePlots) != 0]
  c(builtGrids, standalonePlots)
}

#' Build one plot from its Plot definition
#'
#' Overlays the Plot definition's fields on [createEsqlabsPlotConfiguration()]
#' and maps the result through `.plotFields`: units, scales, scale arguments,
#' geom attributes, LLOQ, fold-line, and population settings become arguments
#' of the `ospsuite` plot function; titles, theme, and the color, shape, and
#' line type scales are added to the returned plot as `ggplot2` layers
#' (`.plotStyleComponents()`). A field the plot type does not apply has no
#' effect, and an unset field without a house value passes no argument, so the
#' `ospsuite` default applies.
#'
#' @param entry One Plot definition (a named list).
#' @param dataCombined The `DataCombined` the plot draws.
#' @returns A `ggplot` object, or `NULL` when the `ospsuite` plot function finds
#'   no data to draw.
#' @keywords internal
#' @noRd
.buildPlot <- function(entry, dataCombined) {
  plotType <- entry$plotType
  config <- .plotConfigurationForEntry(entry)
  args <- .plotFunctionArgs(config, plotType, dataCombined)

  plot <- switch(
    plotType,
    individual = ,
    population = do.call(ospsuite::plotTimeProfile, args),
    observedVsSimulated = do.call(ospsuite::plotPredictedVsObserved, args),
    residualsVsSimulated = do.call(
      ospsuite::plotResidualsVsCovariate,
      c(args, xAxis = "predicted")
    ),
    residualsVsTime = do.call(
      ospsuite::plotResidualsVsCovariate,
      c(args, xAxis = "time")
    )
  )
  if (is.null(plot)) {
    return(NULL)
  }
  # Adding a scale that replaces one `ospsuite.plots` already set is reported
  # by `ggplot2` with a message; replacing it is the intent.
  suppressMessages(plot + .plotStyleComponents(config, plotType, plot))
}

#' The plot settings of one Plot definition
#'
#' @param entry One Plot definition (a named list).
#' @returns [createEsqlabsPlotConfiguration()] with the definition's fields,
#'   converted to their types, in place of the house values.
#' @keywords internal
#' @noRd
.plotConfigurationForEntry <- function(entry) {
  config <- createEsqlabsPlotConfiguration()
  for (field in intersect(names(entry), names(.plotFields))) {
    if (!is.null(entry[[field]])) {
      config[[field]] <- .parsePlotFieldValue(
        entry[[field]],
        .plotFields[[field]]
      )
    }
  }
  config
}

#' Arguments of the `ospsuite` plot function for one plot
#'
#' @param config Plot settings from `.plotConfigurationForEntry()`.
#' @param plotType The plot type.
#' @param dataCombined The `DataCombined` the plot draws.
#' @returns A named list of arguments without `NULL` entries.
#' @keywords internal
#' @noRd
.plotFunctionArgs <- function(config, plotType, dataCombined) {
  isTimeProfile <- plotType %in% .timeProfilePlotTypes
  isOvs <- identical(plotType, "observedVsSimulated")

  args <- list(plotData = dataCombined, yUnit = config[["yUnit"]])
  if (isTimeProfile || identical(plotType, "residualsVsTime")) {
    args$xUnit <- config[["xUnit"]]
  }
  if (isOvs) {
    # One scale for both axes; the field checks reject two different values.
    args$xyScale <- config[["xAxisScale"]] %||% config[["yAxisScale"]]
  } else {
    args$xScale <- config[["xAxisScale"]]
    args$yScale <- config[["yAxisScale"]]
  }
  args$xScaleArgs <- .plotScaleArgs(config, "x", plotType)
  args$yScaleArgs <- .plotScaleArgs(config, "y", plotType)

  args$geomPointAttributes <- .dropNull(list(
    size = config[["pointsSize"]],
    alpha = config[["pointsAlpha"]]
  ))
  args$geomErrorbarAttributes <- utils::modifyList(
    ospsuite.plots::getDefaultGeomAttributes("Errorbar"),
    .dropNull(list(
      linewidth = config[["errorbarsSize"]],
      width = config[["errorbarsCapSize"]],
      linetype = config[["errorbarsLinetype"]],
      alpha = config[["errorbarsAlpha"]]
    ))
  )
  lineAttributes <- .dropNull(list(
    linewidth = config[["linesSize"]],
    alpha = config[["linesAlpha"]]
  ))
  if (isTimeProfile) {
    args$geomLineAttributes <- utils::modifyList(
      ospsuite.plots::getDefaultGeomAttributes("Line"),
      lineAttributes
    )
  } else {
    if (
      plotType %in% .residualPlotTypes && !is.null(config[["linesLinetype"]])
    ) {
      lineAttributes$linetype <- config[["linesLinetype"]][[1]]
    }
    args$geomComparisonLineAttributes <- utils::modifyList(
      ospsuite.plots::getDefaultGeomAttributes("ComparisonLine"),
      lineAttributes
    )
  }

  if (identical(plotType, "population")) {
    args$aggregation <- config[["aggregation"]]
    args$quantiles <- config[["quantiles"]]
    args$nsd <- config[["nsd"]]
    ribbonAttributes <- utils::modifyList(
      ospsuite.plots::getDefaultGeomAttributes("Ribbon"),
      .dropNull(list(
        alpha = config[["ribbonsAlpha"]],
        linewidth = config[["ribbonsSize"]],
        linetype = config[["ribbonsLinetype"]][1]
      ))
    )
    # `ospsuite.plots` draws the band without an outline (`color = NA`); an
    # outline setting draws it in the band's color.
    if (
      !is.null(config[["ribbonsSize"]]) || !is.null(config[["ribbonsLinetype"]])
    ) {
      ribbonAttributes$color <- NULL
    }
    args$geomRibbonAttributes <- ribbonAttributes
  }

  if (isOvs) {
    if (!is.null(config[["foldDistance"]])) {
      args$comparisonLineVector <- ospsuite.plots::getFoldDistanceList(
        folds = config[["foldDistance"]]
      )
    }
    args$lloqOnBothAxes <- config[["lloqOnBothAxes"]] %||%
      if (!is.null(config[["lloqDirection"]])) {
        identical(config[["lloqDirection"]], "both")
      } else {
        TRUE
      }
  }

  .dropNull(args)
}

#' A list without its `NULL` entries
#'
#' @keywords internal
#' @noRd
.dropNull <- function(x) {
  Filter(Negate(is.null), x)
}

#' Scale arguments for one axis of a plot
#'
#' Each field owns one argument: the limits fields `limits` and `oob`, the
#' ticks `breaks`, the tick labels `labels`, and the expansion `expand`. Axis
#' limits set the visible range and keep the points outside it (a zoom). Value
#' limits remove the points outside them. With both, the axis limits set the
#' range and the points outside the value limits are removed.
#'
#' @param config Plot settings from `.plotConfigurationForEntry()`.
#' @param axis `"x"` or `"y"`.
#' @param plotType The plot type.
#' @returns A list of arguments for the `ggplot2` position scale, or `NULL`
#'   when the plot sets nothing on this axis, so that the `ospsuite.plots`
#'   default applies.
#' @keywords internal
#' @noRd
.plotScaleArgs <- function(config, axis, plotType) {
  field <- function(suffix) config[[paste0(axis, suffix)]]
  args <- list()

  valuesLimits <- field("ValuesLimits")
  axisLimits <- field("AxisLimits")
  if (!is.null(axisLimits)) {
    args$limits <- axisLimits
    args$oob <- scales::oob_keep
    if (!is.null(valuesLimits)) {
      # `oob` receives values and limits on the transformed scale. The limits
      # are the axis limits, so comparing them tells whether the scale is
      # logarithmic, and the value limits are transformed the same way.
      args$oob <- function(x, range) {
        isLog <- isTRUE(all.equal(range, log10(axisLimits)))
        censorRange <- if (isLog) log10(valuesLimits) else valuesLimits
        scales::oob_censor(x, range = censorRange)
      }
    }
  } else if (!is.null(valuesLimits)) {
    args$limits <- valuesLimits
    args$oob <- scales::oob_censor
  }

  if (!is.null(field("AxisTicks"))) {
    args$breaks <- field("AxisTicks")
  }
  tickLabels <- field("AxisTicksLabels")
  if (!is.null(tickLabels) && !identical(tickLabels, "default")) {
    args$labels <- .plotTickLabels(tickLabels)
  }
  if (!is.null(field("AxisExpand"))) {
    args$expand <- if (isTRUE(field("AxisExpand"))) {
      ggplot2::expansion(mult = 0.05)
    } else {
      ggplot2::expansion(mult = 0, add = 0)
    }
  }

  if (length(args) == 0L) {
    return(NULL)
  }
  # The time axis of a time profile starts at 0 by default; keep that unless a
  # limits field replaces it.
  if (axis == "x" && plotType %in% .timeProfilePlotTypes) {
    args <- utils::modifyList(
      eval(formals(ospsuite.plots::plotTimeProfile)$xScaleArgs),
      args
    )
  }
  args
}

#' Tick labels for one tick-label setting
#'
#' @param tickLabels One of `.plotTickLabelNames` or label texts.
#' @returns The `labels` argument of a `ggplot2` scale.
#' @keywords internal
#' @noRd
.plotTickLabels <- function(tickLabels) {
  if (length(tickLabels) > 1L || !tickLabels %in% .plotTickLabelNames) {
    return(tickLabels)
  }
  switch(
    tickLabels,
    none = function(ticks) rep("", length(ticks)),
    identity = function(ticks) as.character(ticks),
    log = .logTickLabels,
    ln = .lnTickLabels,
    sqrt = .sqrtTickLabels,
    greek = .greekTickLabels,
    pi = .piTickLabels,
    percentiles = .percentileTickLabels
  )
}
