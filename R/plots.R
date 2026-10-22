# Plots sections: parse + validate + serialize + mutation.
#
# Owns the three top-level plots-related sections end-to-end:
#   * the dataCombined definitions : named list keyed by `dataCombinedId`.
#   * the plots definitions        : named list keyed by `plotId` (the plot list).
#   * the plotGrids definitions    : named list keyed by `plotGridId`.
# Every entry is a named list of its fields, classed `c("DataCombined","list")`
# / `c("Plot", "list")` / `c("PlotGrid", "list")` so a single definition
# dispatches a print method. The keyed-list shape IS the per-file JSON shape,
# so the parse/serialize step is near-identity. The plotting *engine*
# (createPlots() and its dispatchers) lives in R/create-plots.R and is
# independent of this file.
#
# Called by:
#   - .loadProjectTree() via the three plots definition-tree specs
#     (data-combined / plots / plot-grids).
#   - .runProjectValidation() via .validatePlots()
#   - .projectToJson() via .dataCombinedSectionToJson() / .plotsSectionToJson()
#     / .plotGridsSectionToJson()
#   - users via the public addPlot / removePlot / addPlotGrid /
#     removePlotGrid / addDataCombined / removeDataCombined functions.

# Parse ----
#
# Each plots section persists as its own keyed kind on disk
# (`definitions/data-combined/`, `definitions/plots/`, `definitions/plot-grids/`).
# Each is a keyed list:
#   * dataCombined : named list keyed by `dataCombinedId` (drops the
#                    redundant id field on each entry).
#   * plots        : named list keyed by `plotId`; each entry classed
#                    `c("Plot", "list")`, fields verbatim.
#   * plotGrids    : named list keyed by `plotGridId`; each entry classed
#                    `c("PlotGrid", "list")`, fields verbatim (a grid's
#                    `plotIds` is a comma-separated string).

# Drop the redundant `dataCombinedId` field (it becomes the list key) and
# re-key the list by id. Per-entry sub-lists (`simulated`, `observed`) pass
# through verbatim so adding optional fields at the JSON level does not require
# a code change here.
#
# @keywords internal
# @noRd
.parseNestedDataCombined <- function(nestedData) {
  if (is.null(nestedData) || length(nestedData) == 0) {
    return(list())
  }
  result <- list()
  for (i in seq_along(nestedData)) {
    dc <- nestedData[[i]]
    # Each dataCombined entry must be a named-list object carrying a scalar
    # `dataCombinedId` (the key). A malformed shape (a scalar, an unnamed list,
    # or a missing/empty `dataCombinedId`) would otherwise fail with an opaque
    # base-R "subscript out of bounds" / "less than one element" error naming
    # nothing.
    id <- if (is.list(dc)) dc[["dataCombinedId"]] else NULL
    if (is.null(id) || !is.character(id) || length(id) != 1L || is.na(id)) {
      cli::cli_abort(c(
        "Malformed {.field dataCombined} entry at position {i}.",
        "x" = "Each entry must be an object with a single \\
        {.field dataCombinedId} field.",
        "i" = "Check the {.field plots} section of the project file."
      ))
    }
    entry <- list(
      simulated = dc$simulated %||% list(),
      observed = dc$observed %||% list()
    )
    class(entry) <- c("DataCombined", "list")
    result[[id]] <- entry
  }
  result
}

# Parse a list of JSON objects (one per plot / grid) into a keyed list, keyed
# by each record's id field, dropping NULL/NA-valued fields so an absent
# optional field is simply absent from the entry list (no NA cell). Each entry
# is classed so a single definition dispatches its print method without the
# class leaking into the on-disk JSON (the serializer rebuilds plain records).
# `idClass` is the entry class to stamp (`"Plot"` or `"PlotGrid"`).
#
# The on-disk reference key is suffixless (`dataCombined` on a plot, `plots` on
# a grid) while the in-memory record field keeps its id-suffixed name
# (`dataCombinedId` / `plotIds`); `.plotRefKeyToField()` maps the JSON key onto
# the record field at parse time (the serializer mirrors it).
#
# @keywords internal
# @noRd
.parsePlotEntries <- function(data, idField, idClass) {
  if (is.null(data) || length(data) == 0) {
    return(list())
  }
  result <- list()
  for (entry in data) {
    id <- entry[[idField]]
    cleaned <- Filter(
      function(x) !(is.null(x) || (length(x) == 1L && is.na(x))),
      entry
    )
    cleaned <- .plotRefKeyToField(cleaned, idClass)
    class(cleaned) <- c(idClass, "list")
    result[[id]] <- cleaned
  }
  result
}

# Map the suffixless on-disk reference key to its in-memory record field: a
# plot's `dataCombined` JSON key becomes the `dataCombinedId` record field; a
# grid's `plots` JSON key becomes the `plotIds` record field. The field is
# renamed in place so round-trip diffs stay zero-noise. A record that already
# carries only the record-field name (an in-memory entry built by `addPlot()` /
# `addPlotGrid()`) passes through unchanged. `.plotRefFieldToKey()` is the
# inverse, used at serialize time.
#
# @keywords internal
# @noRd
.plotRefKeyToField <- function(entry, idClass) {
  mapping <- switch(
    idClass,
    Plot = c(dataCombined = "dataCombinedId"),
    PlotGrid = c(plots = "plotIds"),
    character()
  )
  .renamePlotKeys(entry, mapping)
}

# Inverse of `.plotRefKeyToField()`: map the in-memory record field back to the
# suffixless on-disk reference key (`dataCombinedId` -> `dataCombined`,
# `plotIds` -> `plots`). `idClass` is the entry class (`"Plot"` / `"PlotGrid"`).
#
# @keywords internal
# @noRd
.plotRefFieldToKey <- function(entry, idClass) {
  mapping <- switch(
    idClass,
    Plot = c(dataCombinedId = "dataCombined"),
    PlotGrid = c(plotIds = "plots"),
    character()
  )
  .renamePlotKeys(entry, mapping)
}

# Rename the named members of a plot / grid record in place, preserving member
# order so a round-trip leaves the on-disk field order untouched. `mapping` is a
# named character vector `c(old = "new")`. A member absent from the record is a
# no-op for that pair.
#
# @keywords internal
# @noRd
.renamePlotKeys <- function(entry, mapping) {
  nms <- names(entry)
  if (is.null(nms)) {
    return(entry)
  }
  for (old in names(mapping)) {
    hit <- which(nms == old)
    if (length(hit) == 1L) {
      nms[hit] <- mapping[[old]]
    }
  }
  names(entry) <- nms
  entry
}

# Print ----

#' @exportS3Method
#' @noRd
print.Plot <- function(x, ...) {
  ospsuite.utils::ospPrintClass(x)
  ospsuite.utils::ospPrintItems(
    list(
      "Plot Id" = x$plotId %||% "",
      "DataCombined Id" = x$dataCombinedId %||% "",
      "Plot Type" = x$plotType %||% "",
      "Title" = x$title %||% ""
    ),
    print_empty = TRUE
  )
  invisible(x)
}

#' @exportS3Method
#' @noRd
print.PlotGrid <- function(x, ...) {
  ospsuite.utils::ospPrintClass(x)
  ospsuite.utils::ospPrintItems(
    list(
      "Plot Grid Id" = x$plotGridId %||% "",
      "Plot Ids" = x$plotIds %||% "",
      "Title" = x$title %||% ""
    ),
    print_empty = TRUE
  )
  invisible(x)
}

#' @exportS3Method
#' @noRd
print.DataCombined <- function(x, ...) {
  ospsuite.utils::ospPrintClass(x)
  ospsuite.utils::ospPrintItems(
    list(
      "Simulated Entries" = length(x$simulated %||% list()),
      "Observed Entries" = length(x$observed %||% list())
    ),
    print_empty = TRUE
  )
  invisible(x)
}

# Section validation adapter ----
#
# Registered in `.validationAdapters` (R/validation.R) and called by
# `.runProjectValidation()`. The plots adapter is one cross-cutting validator
# that reads the three top-level sections (`dataCombined`, `plots`,
# `plotGrids`), since they are tightly coupled by inner cross-refs.

#' @keywords internal
#' @noRd
.plotsValidatorAdapter <- function(project) {
  .validatePlots(
    .unwrapDefinitionList(project$definitions$dataCombined),
    .unwrapDefinitionList(project$definitions$plots),
    .unwrapDefinitionList(project$definitions$plotGrids)
  )
}

#' Validate the plots-related sections of a Project
#'
#' Covers `plots` (the plot list) and `plotGrids`. The shape of the
#' `dataCombined` section is validated separately by `.validateDataCombined()`;
#' `dataCombined` is passed here only so a plot's `dataCombinedId` can be
#' checked against the set of known dataCombined ids.
#'   * plot entries must declare `plotId`, `dataCombinedId`,
#'     and `plotType`; `plotId` must be unique; `dataCombinedId` must
#'     reference a known dataCombined entry; `plotType` must be one of
#'     `.validPlotTypes`.
#'   * plotGrids entries reference plot ids via a comma-separated `plotIds`
#'     string; unknown ids are a critical error, mirroring the hard failure
#'     in `createPlots()`. This check also runs when the plot list is empty
#'     (every referenced id is then unknown).
#'
#' Both reference checks match a reference to its definition exactly, the same
#' way `createPlots()` resolves it, and report an unresolved reference with the
#' closest existing id.
#'
#' Cross-section references that escape these sections (dataCombined ->
#' scenarios) are validated in `.validateCrossReferences()`.
#'
#' @param dataCombined Named list from `dataCombined` definitions, used only as
#'   the set of known ids a plot's `dataCombinedId` may reference.
#' @param plotConfig Named list from `plots` definitions (the plot list, keyed
#'   by `plotId`).
#' @param plotGrids Named list from `plotGrids` definitions.
#' @return validationResult.
#' @keywords internal
#' @noRd
.validatePlots <- function(
  dataCombined,
  plotConfig = list(),
  plotGrids = list()
) {
  result <- validationResult$new()

  # No plots concern declared at all: warn once, matching the prior behavior
  # when the whole plots section was absent.
  if (
    length(dataCombined %||% list()) == 0 &&
      length(plotConfig %||% list()) == 0 &&
      length(plotGrids %||% list()) == 0
  ) {
    result$addWarning("Data", "No plots defined")
    return(result)
  }

  plotConfig <- plotConfig %||% list()
  plotGrids <- plotGrids %||% list()

  # The shape of each `dataCombined` entry (required `label`/`scenario`/`path`
  # on simulated, `label`/`dataSet` on observed) is validated by the
  # `dataCombined` adapter (`.validateDataCombined()`, `R/data-combined.R`).
  # Here `dataCombined` is used only as the set of known ids the plot list may
  # reference (see the `dataCombinedId` check below).

  if (length(plotConfig) == 0) {
    result$addWarning("Data", "plotConfiguration is empty")
  } else {
    for (field in c("plotId", "dataCombinedId", "plotType")) {
      missingField <- Filter(
        function(p) is.null(p[[field]]),
        plotConfig
      )
      if (length(missingField) > 0) {
        result$addCriticalError(
          "Missing Fields",
          paste0("plotConfiguration is missing required field '", field, "'")
        )
      }
    }

    plotIds <- vapply(
      plotConfig,
      function(p) p$plotId %||% NA_character_,
      character(1)
    )
    result <- .checkNoDuplicates(plotIds, "plotId", result)

    plotTypes <- unlist(lapply(plotConfig, function(p) p$plotType))
    invalidTypes <- setdiff(plotTypes, .validPlotTypes)
    if (length(invalidTypes) > 0) {
      result$addCriticalError(
        "Invalid Reference",
        paste0(
          "plotConfiguration has unknown plotType(s): ",
          paste(unique(invalidTypes), collapse = ", "),
          ". Must be one of: ",
          paste(.validPlotTypes, collapse = ", ")
        )
      )
    }

    referencedDataCombined <- unlist(lapply(
      plotConfig,
      function(p) p$dataCombinedId
    ))
    dataCombinedKeys <- names(dataCombined %||% list())
    # `setdiff()` rather than `.danglingRefs()`: `createPlots()` resolves a
    # plot's `dataCombinedId` by exact lookup, so a reference this reports as
    # resolved has to be one the build can actually find. It also keeps an
    # empty-string reference reportable, which the missing-field loop above
    # does not catch (`""` is not `NULL`).
    invalidDataCombinedRefs <- setdiff(
      referencedDataCombined,
      dataCombinedKeys
    )
    if (length(invalidDataCombinedRefs) > 0) {
      result$addCriticalError(
        "Invalid Reference",
        paste0(
          "plotConfiguration references unknown dataCombinedId: ",
          paste(invalidDataCombinedRefs, collapse = ", "),
          .suggestSuffixMulti(invalidDataCombinedRefs, dataCombinedKeys)
        )
      )
    }

    # Unknown fields and invalid values stop the build, so they are critical
    # here too.
    for (entry in plotConfig) {
      for (message in .plotEntryFieldErrors(entry)) {
        result$addCriticalError("Invalid Field", message)
      }
    }

    # A supported field on a plotType that does not apply it is ignored by the
    # build, so warn (non-blocking) rather than fail: the plot still renders,
    # just without that field.
    result <- .warnPlotTypeIrrelevantFields(plotConfig, result)
  }

  # plotGrid plot id references are a hard failure in createPlots(), so flag
  # unknown ids as critical here too. The check runs even when the plot list
  # is empty (the state after removing the last plot): every referenced id is
  # then unknown.
  if (length(plotGrids) > 0) {
    # A plot with no `plotId` already raised its own critical error above; drop
    # it here so it cannot be offered as a `did you mean 'NA'` suggestion.
    knownPlotIds <- vapply(
      plotConfig,
      function(p) p$plotId %||% NA_character_,
      character(1)
    )
    knownPlotIds <- knownPlotIds[!is.na(knownPlotIds)]
    allGridIds <- unique(unlist(lapply(
      plotGrids,
      function(g) .splitPlotIDs(g$plotIds)
    )))
    invalidGridRefs <- setdiff(allGridIds, knownPlotIds)
    if (length(invalidGridRefs) > 0) {
      result$addCriticalError(
        "Invalid Reference",
        paste0(
          "plotGrids references unknown plotIds: ",
          paste(invalidGridRefs, collapse = ", "),
          .suggestSuffixMulti(invalidGridRefs, knownPlotIds)
        )
      )
    }
  }

  result
}

#' Warn about plot fields that have no effect on a plot configuration
#'
#' Each field in `.plotFields` applies to some plot types only (for example
#' `quantiles` only to `population` plots). When one is set on any other
#' `plotType`, the build path ignores it. The same holds for a text style field
#' without its text (`titleColor` without `title`) and for
#' `foldLinesLegendDiagonal` when the fold lines have no legend. This adds a
#' non-blocking warning to `result` for each such field so the mismatch is
#' surfaced without gating execution.
#'
#' @param plotConfig Named list from `plots` definitions (keyed by `plotId`).
#' @param result `validationResult` to mutate.
#' @return The mutated `validationResult`.
#' @keywords internal
#' @noRd
.warnPlotTypeIrrelevantFields <- function(plotConfig, result) {
  textFields <- c(
    "title",
    "subtitle",
    "caption",
    "legendTitle",
    "watermark"
  )
  for (entry in plotConfig) {
    plotType <- entry$plotType
    if (is.null(plotType) || !(plotType %in% .validPlotTypes)) {
      next
    }
    plotId <- entry$plotId %||% "<unknown>"
    presentFields <- intersect(names(.plotFields), names(entry))
    # A field present but empty (NULL / NA) is not really "set"; skip it.
    presentFields <- Filter(
      function(f) !.isMissingField(entry[[f]]),
      presentFields
    )
    for (field in presentFields) {
      usedByTypes <- .plotFields[[field]]$appliesTo
      if (plotType %in% usedByTypes) {
        next
      }
      result$addWarning(
        "Data",
        paste0(
          "Plot '",
          plotId,
          "' of plotType '",
          plotType,
          "' sets '",
          field,
          "', which only applies to plotType ",
          paste0("'", usedByTypes, "'", collapse = ", "),
          " and is ignored.",
          if (field == "pointsColor") {
            " On a time profile, observed points take the color of their curve: use 'linesColor'."
          }
        )
      )
    }
    for (textField in setdiff(textFields, presentFields)) {
      styleFields <- intersect(
        paste0(textField, .plotTextStyleSuffixes),
        presentFields
      )
      if (length(styleFields) > 0L) {
        result$addWarning(
          "Data",
          paste0(
            "Plot '",
            plotId,
            "' sets ",
            paste0("'", styleFields, "'", collapse = ", "),
            " but not '",
            textField,
            "', so there is no text to style and the setting is ignored."
          )
        )
      }
    }
    if (
      "foldLinesLegendDiagonal" %in%
        presentFields &&
        identical(
          .parsePlotFieldValue(
            entry$foldLinesLegend %||% TRUE,
            .plotFields$foldLinesLegend
          ),
          FALSE
        )
    ) {
      result$addWarning(
        "Data",
        paste0(
          "Plot '",
          plotId,
          "' sets 'foldLinesLegendDiagonal' with 'foldLinesLegend' FALSE: ",
          "the fold lines have no legend, so the setting is ignored."
        )
      )
    }
  }

  result
}

# Serialize ----
#
# The plots concern serializes as three top-level JSON sections in the inlined
# snapshot (`dataCombined`, `plots`, `plotGrids`), each the JSON array its own
# definition tree inlines. `.projectToJson()` emits each via one of these helpers.

# JSON array of dataCombined records (the inlined `dataCombined` section);
# `NULL` when the section is empty so the key round-trips as the absent shape.
.dataCombinedSectionToJson <- function(project) {
  dataCombined <- .unwrapDefinitionList(project$definitions$dataCombined)
  if (is.null(dataCombined) || length(dataCombined) == 0) {
    return(NULL)
  }
  .dataCombinedToNestedJson(dataCombined)
}

# JSON array of plot records (the inlined `plots` section, the plot list);
# `NULL` when empty.
.plotsSectionToJson <- function(project) {
  plots <- .unwrapDefinitionList(project$definitions$plots)
  if (is.null(plots) || length(plots) == 0) {
    return(NULL)
  }
  .plotEntriesToJson(plots)
}

# JSON array of plot-grid records (the inlined `plotGrids` section); `NULL`
# when empty.
.plotGridsSectionToJson <- function(project) {
  plotGrids <- .unwrapDefinitionList(project$definitions$plotGrids)
  if (is.null(plotGrids) || length(plotGrids) == 0) {
    return(NULL)
  }
  .plotEntriesToJson(plotGrids)
}

# Inverts .parseNestedDataCombined: re-adds the `dataCombinedId` field from the
# list key. Empty `simulated`/`observed` lists are omitted to keep the JSON
# terse.
#
# @keywords internal
# @noRd
.dataCombinedToNestedJson <- function(dataCombined) {
  if (is.null(dataCombined) || length(dataCombined) == 0) {
    return(list())
  }
  unname(lapply(names(dataCombined), function(id) {
    dc <- dataCombined[[id]]
    entry <- list(dataCombinedId = id)
    if (length(dc$simulated) > 0) {
      entry$simulated <- dc$simulated
    }
    if (length(dc$observed) > 0) {
      entry$observed <- dc$observed
    }
    entry
  }))
}

# Inverts `.parsePlotEntries`: turn a keyed list of plot / grid entries into a
# plain unnamed JSON array of records (one object per entry). Each entry is
# already the per-record shape; this maps the in-memory reference field back to
# its suffixless on-disk key (`dataCombinedId` -> `dataCombined`, `plotIds` ->
# `plots`), strips the entry class (`c("Plot", "list")` / `c("PlotGrid",
# "list")`) so it never leaks into JSON, and drops the list name (the id field
# is already a field of the record).
#
# @keywords internal
# @noRd
.plotEntriesToJson <- function(entries) {
  if (is.null(entries) || length(entries) == 0) {
    return(list())
  }
  unname(lapply(entries, function(entry) {
    idClass <- class(entry)[[1]]
    entry <- .plotRefFieldToKey(entry, idClass)
    class(entry) <- "list"
    entry
  }))
}

# Public CRUD: plots ----

.validPlotTypes <- c(
  "individual",
  "population",
  "observedVsSimulated",
  "residualsVsSimulated",
  "residualsVsTime"
)

# Plot definition fields ----
#
# A Plot definition accepts the fields of the `DefaultPlotConfiguration` that
# `{tlf}` gave esqlabsR 5.x (the "configuration fields", also the names
# `createEsqlabsPlotConfiguration()` may hold) plus five Plot-only fields. Each
# field has a value rule and the plot types that apply it. This one table is
# read by the validator, `addPlot()`, the build guard in `createPlots()`, the
# Excel import, the plot builder, and the sensitivity name check.

.timeProfilePlotTypes <- c("individual", "population")
.residualPlotTypes <- c("residualsVsSimulated", "residualsVsTime")
.plotIdentityFields <- c("plotId", "dataCombinedId", "plotType")
.plotAxisScales <- c("linear", "log")
.plotFontFaces <- c("plain", "bold", "italic", "bold.italic")
.plotAlignments <- c("left", "center", "right")
.plotLinetypes <- c(
  "blank",
  "solid",
  "dashed",
  "dotted",
  "dotdash",
  "longdash",
  "twodash"
)
.plotLegendPositions <- c(
  "none",
  "insideTop",
  "insideTopLeft",
  "insideLeft",
  "insideBottomLeft",
  "insideBottom",
  "insideBottomRight",
  "insideRight",
  "insideTopRight",
  "outsideTop",
  "outsideTopLeft",
  "outsideLeft",
  "outsideBottomLeft",
  "outsideBottom",
  "outsideBottomRight",
  "outsideRight",
  "outsideTopRight",
  "top",
  "bottom",
  "left",
  "right"
)
.plotTickLabelNames <- c(
  "default",
  "none",
  "identity",
  "log",
  "ln",
  "sqrt",
  "greek",
  "pi",
  "percentiles"
)
# Elements whose text style is set by the `<element>Color`, `...Size`,
# `...FontFace`, `...FontFamily`, `...Angle`, `...Align`, and `...Margin` fields.
.plotTextElements <- c(
  "title",
  "subtitle",
  "caption",
  "xLabel",
  "yLabel",
  "legendTitle",
  "legendKeys",
  "xAxisLabelTicks",
  "yAxisLabelTicks"
)
.plotTextStyleSuffixes <- c(
  "Color",
  "Size",
  "FontFace",
  "FontFamily",
  "Angle",
  "Align",
  "Margin"
)

# One field of the table.
#
# @param type `"text"` (free text, never split), `"string"`, `"choice"`,
#   `"number"`, `"logical"`, `"color"`, `"linetype"`, or `"tickLabels"`.
# @param appliesTo Plot types that apply the field.
# @param count Exact number of values, or `NULL` for "one or more" when
#   `multiple` is `TRUE`.
# @param multiple Whether the field holds several values.
# @param allowed Allowed values of a `"choice"` field.
# @param range Allowed numeric range, inclusive.
# @keywords internal
# @noRd
.plotField <- function(
  type,
  appliesTo = .validPlotTypes,
  multiple = FALSE,
  count = NULL,
  allowed = NULL,
  range = NULL
) {
  list(
    type = type,
    appliesTo = appliesTo,
    multiple = multiple || (!is.null(count) && count > 1L),
    count = count %||% if (!multiple) 1L,
    allowed = allowed,
    range = range
  )
}

# Text style fields of one element (title, legend keys, ...), and of the
# watermark, which has no `Margin`-less variant.
.plotTextStyleFields <- function(element) {
  stats::setNames(
    list(
      .plotField("color"),
      .plotField("number", range = c(0, Inf)),
      .plotField("choice", allowed = .plotFontFaces),
      .plotField("string"),
      .plotField("number"),
      .plotField("choice", allowed = .plotAlignments),
      .plotField("number", count = 4L)
    ),
    paste0(element, .plotTextStyleSuffixes)
  )
}

.plotLineStyleFields <- function(prefix, appliesTo = .validPlotTypes) {
  stats::setNames(
    list(
      .plotField("color", appliesTo),
      .plotField("number", appliesTo, range = c(0, Inf)),
      .plotField("linetype", appliesTo)
    ),
    paste0(prefix, c("Color", "Size", "Linetype"))
  )
}

.plotBackgroundFields <- function(prefix) {
  stats::setNames(
    list(
      .plotField("color"),
      .plotField("color"),
      .plotField("number", range = c(0, Inf)),
      .plotField("linetype")
    ),
    paste0(prefix, c("Fill", "Color", "Size", "Linetype"))
  )
}

.plotConfigurationFields <- function() {
  ovs <- "observedVsSimulated"
  alpha <- c(0, 1)
  c(
    list(
      xUnit = .plotField("string", c(.timeProfilePlotTypes, "residualsVsTime")),
      yUnit = .plotField("string"),
      title = .plotField("text"),
      subtitle = .plotField("text"),
      caption = .plotField("text"),
      xLabel = .plotField("text"),
      yLabel = .plotField("text"),
      legendTitle = .plotField("text"),
      legendPosition = .plotField("choice", allowed = .plotLegendPositions),
      legendBackgroundColor = .plotField("color"),
      legendBackgroundAlpha = .plotField("number", range = alpha),
      legendBorderColor = .plotField("color"),
      legendBorderType = .plotField("linetype"),
      legendBorderSize = .plotField("number", range = c(0, Inf))
    ),
    unlist(
      lapply(.plotTextElements, .plotTextStyleFields),
      recursive = FALSE
    ),
    list(
      xAxisScale = .plotField("choice", allowed = .plotAxisScales),
      yAxisScale = .plotField("choice", allowed = .plotAxisScales),
      xAxisLimits = .plotField("number", count = 2L),
      yAxisLimits = .plotField("number", count = 2L),
      xValuesLimits = .plotField("number", count = 2L),
      yValuesLimits = .plotField("number", count = 2L),
      xAxisTicks = .plotField("number", multiple = TRUE),
      yAxisTicks = .plotField("number", multiple = TRUE),
      xAxisTicksLabels = .plotField("tickLabels", multiple = TRUE),
      yAxisTicksLabels = .plotField("tickLabels", multiple = TRUE),
      xAxisExpand = .plotField("logical"),
      yAxisExpand = .plotField("logical")
    ),
    .plotLineStyleFields("xAxis"),
    .plotLineStyleFields("yAxis"),
    .plotLineStyleFields("xGrid"),
    .plotLineStyleFields("yGrid"),
    .plotBackgroundFields("plotBackground"),
    .plotBackgroundFields("plotPanelBackground"),
    list(watermark = .plotField("text")),
    .plotTextStyleFields("watermark"),
    list(
      linesColor = .plotField("color", .timeProfilePlotTypes, multiple = TRUE),
      linesSize = .plotField("number", range = c(0, Inf)),
      linesLinetype = .plotField("linetype", multiple = TRUE),
      linesAlpha = .plotField("number", range = alpha),
      pointsColor = .plotField(
        "color",
        c(ovs, .residualPlotTypes),
        multiple = TRUE
      ),
      pointsShape = .plotField(
        "choice",
        multiple = TRUE,
        allowed = ospsuite.plots::ospShapeNames
      ),
      pointsSize = .plotField("number", range = c(0, Inf)),
      pointsAlpha = .plotField("number", range = alpha),
      ribbonsFill = .plotField("color", "population", multiple = TRUE),
      ribbonsSize = .plotField("number", "population", range = c(0, Inf)),
      ribbonsLinetype = .plotField("linetype", "population", multiple = TRUE),
      ribbonsAlpha = .plotField("number", "population", range = alpha),
      errorbarsSize = .plotField("number", range = c(0, Inf)),
      errorbarsCapSize = .plotField("number", range = c(0, Inf)),
      errorbarsLinetype = .plotField("linetype"),
      errorbarsAlpha = .plotField("number", range = alpha),
      displayLLOQ = .plotField("logical", c(.timeProfilePlotTypes, ovs)),
      lloqDirection = .plotField(
        "choice",
        c(.timeProfilePlotTypes, ovs),
        allowed = c("horizontal", "vertical", "both")
      ),
      foldLinesLegend = .plotField("logical", ovs),
      foldLinesLegendDiagonal = .plotField("logical", ovs)
    )
  )
}

.plotOnlyFields <- function() {
  list(
    aggregation = .plotField("string", "population"),
    quantiles = .plotField("number", "population", count = 3L, range = c(0, 1)),
    nsd = .plotField("number", "population", range = c(0, Inf)),
    foldDistance = .plotField(
      "number",
      "observedVsSimulated",
      multiple = TRUE,
      range = c(0, Inf)
    ),
    lloqOnBothAxes = .plotField("logical", "observedVsSimulated")
  )
}

.plotFields <- c(.plotConfigurationFields(), .plotOnlyFields())

# Line type numbers 0 to 6 as their names, the `ggplot2` order.
.plotLinetypeName <- function(token) {
  if (grepl("^[0-6]$", token)) {
    return(.plotLinetypes[[as.integer(token) + 1L]])
  }
  token
}

# The values of one stored field, converted to the field's type.
#
# A stored value is a number, a logical, a vector (a JSON array), or one string
# that holds several values separated by commas, where double quotes protect a
# comma inside one value (the `origin/main` reading rule). Free text is never
# split.
#
# @param value The stored value (not `NULL`).
# @param spec The field's entry in `.plotFields`.
# @returns The converted values. A value that does not convert is `NA`.
# @keywords internal
# @noRd
.parsePlotFieldValue <- function(value, spec) {
  value <- unlist(value, use.names = FALSE)
  if (identical(spec$type, "text")) {
    return(paste(as.character(value), collapse = ", "))
  }
  if (is.character(value) && length(value) == 1L) {
    value <- trimws(scan(
      text = value,
      what = "character",
      sep = ",",
      quiet = TRUE,
      strip.white = TRUE
    ))
  }
  switch(
    spec$type,
    number = suppressWarnings(as.numeric(value)),
    logical = vapply(
      value,
      function(x) {
        if (is.logical(x)) {
          return(x)
        }
        token <- tolower(as.character(x))
        if (token %in% c("true", "t", "1", "yes", "y")) {
          return(TRUE)
        }
        if (token %in% c("false", "f", "0", "no", "n")) {
          return(FALSE)
        }
        NA
      },
      logical(1),
      USE.NAMES = FALSE
    ),
    linetype = vapply(
      as.character(value),
      .plotLinetypeName,
      character(1),
      USE.NAMES = FALSE
    ),
    as.character(value)
  )
}

# The problem with one field value, or `NULL` when the value follows its rule.
#
# @keywords internal
# @noRd
.plotFieldValueProblem <- function(plotId, field, value, spec) {
  parsed <- .parsePlotFieldValue(value, spec)
  shown <- paste(as.character(unlist(value)), collapse = ", ")
  problem <- function(rule) {
    messages$invalidPlotFieldValue(plotId, field, shown, rule)
  }
  if (identical(spec$type, "text")) {
    return(NULL)
  }
  if (length(parsed) == 0L) {
    return(problem("a value"))
  }
  if (!is.null(spec$count) && length(parsed) != spec$count) {
    return(problem(paste(
      spec$count,
      if (spec$count == 1L) "value" else "values"
    )))
  }
  if (anyNA(parsed)) {
    return(problem(switch(
      spec$type,
      number = "numbers",
      logical = "TRUE or FALSE",
      "valid values"
    )))
  }
  if (identical(spec$type, "choice") && !all(parsed %in% spec$allowed)) {
    if (field %in% c("xAxisScale", "yAxisScale")) {
      return(messages$invalidPlotAxisScale(plotId, field, shown, spec$allowed))
    }
    return(problem(paste0("one of: ", paste(spec$allowed, collapse = ", "))))
  }
  if (identical(spec$type, "linetype") && !all(parsed %in% .plotLinetypes)) {
    return(problem(paste0(
      "line types: ",
      paste(.plotLinetypes, collapse = ", "),
      ", or 0 to 6"
    )))
  }
  if (identical(spec$type, "color")) {
    isColor <- vapply(
      parsed,
      function(x) {
        !inherits(
          tryCatch(grDevices::col2rgb(x), error = function(e) e),
          "error"
        )
      },
      logical(1)
    )
    if (!all(isColor)) {
      return(problem("color names or hexadecimal color codes"))
    }
  }
  if (
    !is.null(spec$range) &&
      any(parsed < spec$range[[1]] | parsed > spec$range[[2]])
  ) {
    return(problem(paste0(
      "numbers from ",
      spec$range[[1]],
      " to ",
      spec$range[[2]]
    )))
  }
  NULL
}

# The problems with the configuration fields of one Plot definition that stop
# it from being built: an unknown field, a value outside its rule, two
# different scales on an `observedVsSimulated` plot (it has one scale for both
# axes), an `lloqDirection` that the plot type cannot draw or that contradicts
# `lloqOnBothAxes`, and tick label texts without matching ticks. Shared by the
# validator (critical errors) and the two aborting gates, `addPlot()` and
# `.assertPlotConfigurationsBuildable()`, so they can never disagree.
#
# @param entry One Plot definition (a named list).
# @returns A character vector of messages, empty when the fields are valid.
# @keywords internal
# @noRd
.plotEntryFieldErrors <- function(entry) {
  plotId <- entry$plotId %||% "<unknown>"
  plotType <- entry$plotType
  errors <- character()

  unknown <- setdiff(names(entry), c(.plotIdentityFields, names(.plotFields)))
  if (length(unknown) > 0L) {
    errors <- c(errors, messages$unknownPlotFields(plotId, unknown))
  }

  valueErrors <- character()
  for (field in intersect(names(entry), names(.plotFields))) {
    if (is.null(entry[[field]])) {
      next
    }
    valueErrors <- c(
      valueErrors,
      .plotFieldValueProblem(
        plotId,
        field,
        entry[[field]],
        .plotFields[[field]]
      )
    )
  }
  # The checks across fields read the values, so they wait for valid ones.
  if (length(valueErrors) > 0L) {
    return(c(errors, valueErrors))
  }

  if (
    identical(plotType, "observedVsSimulated") &&
      !is.null(entry$xAxisScale) &&
      !is.null(entry$yAxisScale) &&
      !identical(entry$xAxisScale, entry$yAxisScale)
  ) {
    errors <- c(
      errors,
      messages$conflictingPlotAxisScales(
        plotId,
        entry$xAxisScale,
        entry$yAxisScale
      )
    )
  }

  direction <- entry$lloqDirection
  if (!is.null(direction) && !is.null(plotType)) {
    allowedDirections <- if (plotType %in% .timeProfilePlotTypes) {
      "horizontal"
    } else if (identical(plotType, "observedVsSimulated")) {
      c("vertical", "both")
    }
    if (!is.null(allowedDirections) && !direction %in% allowedDirections) {
      errors <- c(
        errors,
        messages$invalidLloqDirection(
          plotId,
          plotType,
          direction,
          allowedDirections
        )
      )
    } else if (
      identical(plotType, "observedVsSimulated") &&
        !is.null(entry$lloqOnBothAxes)
    ) {
      onBothAxes <- .parsePlotFieldValue(
        entry$lloqOnBothAxes,
        .plotFields$lloqOnBothAxes
      )
      if (!identical(onBothAxes, identical(direction, "both"))) {
        errors <- c(
          errors,
          messages$conflictingLloqSettings(plotId, direction, onBothAxes)
        )
      }
    }
  }

  for (axis in c("x", "y")) {
    labelsField <- paste0(axis, "AxisTicksLabels")
    if (is.null(entry[[labelsField]])) {
      next
    }
    labels <- .parsePlotFieldValue(
      entry[[labelsField]],
      .plotFields[[labelsField]]
    )
    if (length(labels) == 1L && labels %in% .plotTickLabelNames) {
      next
    }
    ticksField <- paste0(axis, "AxisTicks")
    ticks <- if (!is.null(entry[[ticksField]])) {
      .parsePlotFieldValue(entry[[ticksField]], .plotFields[[ticksField]])
    }
    if (length(ticks) != length(labels)) {
      errors <- c(
        errors,
        messages$tickLabelsWithoutTicks(
          plotId,
          labelsField,
          ticksField,
          length(labels)
        )
      )
    }
  }

  errors
}

# Abort with every field problem of the given Plot definitions, one bullet per
# problem. Each bullet interpolates its message as a value, so a field name
# holding `{` or `}` is printed as is.
#
# @param entries A list of Plot definitions.
# @returns `entries`, invisibly, when no definition has a field problem.
# @keywords internal
# @noRd
.abortOnPlotEntryFieldErrors <- function(entries, call = rlang::caller_env()) {
  problems <- unlist(lapply(entries, .plotEntryFieldErrors))
  if (length(problems) == 0L) {
    return(invisible(entries))
  }
  bullets <- rlang::set_names(
    sprintf("{problems[[%d]]}", seq_along(problems)),
    rep("x", length(problems))
  )
  cli::cli_abort(
    c(
      "{cli::qty(length(problems))}Invalid plot configuration field{?s}:",
      bullets
    ),
    call = call
  )
}

# Reject a character vector argument (e.g. the id vector that sets N) uniformly
# across the plot add/remove fns. It must be a non-empty character vector with
# no NA / empty element. The caller canonicalizes ids separately.
#
# @keywords internal
# @noRd
.requireNonEmptyStringVector <- function(x, arg) {
  if (
    !is.character(x) ||
      length(x) == 0L ||
      any(is.na(x)) ||
      any(nchar(x) == 0)
  ) {
    cli::cli_abort("{.arg {arg}} must be a non-empty character vector")
  }
  invisible(x)
}

# Recycle / align a scalar-per-definition argument to N definitions. A length-1 value
# is recycled to all N; a length-N value is aligned by position. Any other
# length aborts naming the argument and the lengths. Used by the vectorized
# plot mutators so a scalar field follows the same recycling rule as the id
# vector.
#
# @keywords internal
# @noRd
.recycleScalarArg <- function(x, n, arg) {
  if (length(x) == 1L) {
    return(rep(x, n))
  }
  if (length(x) == n) {
    return(x)
  }
  cli::cli_abort(c(
    "{.arg {arg}} must be length 1 or length {n} (the number of ids).",
    "x" = "It is length {length(x)}."
  ))
}

# Build the N per-definition `...` field sets for a vectorized plot / grid add. A
# `...` field that is a list of length N aligns by position (one element per
# definition); any other `...` field (a scalar, or an atomic vector, e.g.
# `quantiles = c(0.05, 0.5, 0.95)`) is a whole-per-definition value applied to
# every definition. This matches the recycling rule: vector-valued-per-definition
# fields are applied whole, never split positionally; to vary a multi-valued
# field per definition, pass a length-N list. Each per-definition set is normalized
# via `.namedDotsAsFields` (NULL dropped, atomic vector collapsed to CSV).
#
# @keywords internal
# @noRd
.dotsToPerDefinitionFields <- function(dots, n) {
  perDefinition <- vector("list", n)
  for (i in seq_len(n)) {
    fields <- list()
    for (nm in names(dots)) {
      value <- dots[[nm]]
      fields[[nm]] <- if (is.list(value) && length(value) == n) {
        value[[i]]
      } else {
        value
      }
    }
    perDefinition[[i]] <- do.call(.namedDotsAsFields, fields)
  }
  perDefinition
}

# Normalise `...` into a keyed list of optional plot / grid fields:
# - NULL fields are dropped (an absent optional field is simply absent from
#   the entry list).
# - A multi-element atomic vector is collapsed to a comma-separated string
#   (e.g. `quantiles = c(0.05, 0.5, 0.95)` becomes `"0.05, 0.5, 0.95"`).
#   This is the canonical in-memory shape for the multi-value
#   plotConfiguration fields: the JSON stores them as comma-separated
#   strings, the parser keeps them as strings, and the plot builder splits
#   them with `.parsePlotFieldValue()`, where double quotes protect a comma
#   inside one value (`c("A, B", "C")` is stored as `"\"A, B\", C"`).
#
# @keywords internal
# @noRd
.namedDotsAsFields <- function(...) {
  dots <- list(...)
  dots <- Filter(Negate(is.null), dots)
  lapply(dots, function(v) {
    if (length(v) > 1L) {
      # Quote an element that holds a comma, so it reads back as one value.
      if (is.character(v)) {
        v <- ifelse(grepl(",", v, fixed = TRUE), paste0('"', v, '"'), v)
      }
      return(paste(v, collapse = ", "))
    }
    v
  })
}

# A dataCombined field counts as "missing" when it is NULL, or a length-1
# scalar that is NA or the empty string. Shared by the write-time gate
# (`.checkDataCombinedEntry()`) and the lazy validator (`.validatePlots()`) so
# both treat a required field's absence identically.
#
# @keywords internal
# @noRd
.isMissingField <- function(val) {
  is.null(val) ||
    (length(val) == 1L && (is.na(val) || identical(as.character(val), "")))
}

# The required fields of a DataCombined entry, by data type. One definition
# shared by the write-time gate (`.checkDataCombinedEntry()`) and the load-time
# validator (`.checkDataCombinedEntryFields()` in `R/data-combined.R`) so the
# two can never disagree on what a well-formed entry must carry.
#
# @keywords internal
# @noRd
.requiredDataCombinedFields <- function(dataType) {
  if (dataType == "simulated") {
    # `label` is optional on a simulated entry: it defaults to the `path` value
    # exactly as written (see `.effectiveSimulatedLabel()`).
    c("scenario", "path")
  } else {
    c("label", "dataSet")
  }
}

.checkDataCombinedEntry <- function(entry, dataType) {
  required <- .requiredDataCombinedFields(dataType)
  for (field in required) {
    if (.isMissingField(entry[[field]])) {
      cli::cli_abort(
        "DataCombined {dataType} entry is missing required field {.field {field}}."
      )
    }
  }
  invisible(TRUE)
}

# Encode / decode a grid's plot-id set as its stored comma-separated string.
#
# A plot id may legally contain a comma (id canonicalization deliberately keeps
# commas). Grid membership is stored as one comma-separated string, so a bare
# `paste(collapse = ", ")` / `strsplit(",")` round-trip shreds a comma-bearing
# id into several. The pair below escapes a literal backslash as `\\` and a
# literal comma as `\,` on the way in, and reverses it on the way out, so any
# id (comma-bearing or not) survives the round trip. This is the sole in-memory
# contract for the stored `plotIds` string; `addPlotGrid()` encodes with
# `.joinPlotIDs()` and every reader decodes with `.splitPlotIDs()`.
#
# @keywords internal
# @noRd
.joinPlotIDs <- function(plotIds) {
  if (length(plotIds) == 0L) {
    return("")
  }
  escaped <- gsub("\\", "\\\\", plotIds, fixed = TRUE)
  escaped <- gsub(",", "\\,", escaped, fixed = TRUE)
  paste(escaped, collapse = ", ")
}

.splitPlotIDs <- function(plotIdsStr) {
  if (is.null(plotIdsStr) || is.na(plotIdsStr) || !nzchar(plotIdsStr)) {
    return(character())
  }
  # Walk the string tracking escape state: split on unescaped commas only, then
  # collapse `\\` to `\` and `\,` to `,`. Reverses `.joinPlotIDs()` exactly.
  chars <- strsplit(as.character(plotIdsStr), "", fixed = TRUE)[[1]]
  parts <- character()
  current <- ""
  escape <- FALSE
  for (ch in chars) {
    if (escape) {
      current <- paste0(current, ch)
      escape <- FALSE
    } else if (ch == "\\") {
      escape <- TRUE
    } else if (ch == ",") {
      parts <- c(parts, current)
      current <- ""
    } else {
      current <- paste0(current, ch)
    }
  }
  parts <- c(parts, current)
  trimws(parts)
}

#' Add a plot configuration to a Project
#'
#' Add one or more entries to `plots` definitions (a keyed list, one entry per
#' plot). Errors if a `plotId` already exists, if a `dataCombined` is not
#' present in `dataCombined` definitions, if a `plotType` is not one of the
#' supported types, or if a field in `...` is not supported or has an invalid
#' value. Nothing is added when one plot fails these checks.
#'
#' @inherit vectorizedAuthoring details
#'
#' @param project A `Project` object.
#' @param id Character vector of unique plot identifiers (the number of plots
#'   to add). Each is canonicalized to a safe, lowercase id (a warning names
#'   the result if it changed). Stored in the `plotId` field.
#' @param dataCombined Character, length 1 (recycled to all plots) or the
#'   same length as `id` (aligned by position). Each must reference an
#'   existing DataCombined id on the project.
#' @param plotType Character, length 1 (recycled) or the same length as `id`.
#'   Each one of `"individual"`, `"population"`, `"observedVsSimulated"`,
#'   `"residualsVsSimulated"`, `"residualsVsTime"`.
#' @param ... Optional plot-configuration fields: the plot settings of
#'   esqlabsR 5.x (titles and their text style, legend, axes, ticks, grid,
#'   backgrounds, watermark, colors, line, point, ribbon, and error bar style,
#'   LLOQ, fold lines) plus `aggregation`, `quantiles`, `nsd`, `foldDistance`,
#'   and `lloqOnBothAxes`. `vignette("reference-definition-fields")` lists
#'   every field with its values and the plot types it applies to. A field
#'   that is not set takes the ESQlabs value of
#'   [createEsqlabsPlotConfiguration()], or the default of the `{ospsuite}`
#'   plot function. An unknown field name or a value outside its rule is an
#'   error. A field on a plot type that does not apply it is ignored, and
#'   [validateProject()] warns about it.
#'
#'   A multi-value field (e.g. `quantiles = c(0.05, 0.5, 0.95)`) is applied
#'   whole to every plot and stored as a comma-separated string (an element
#'   that holds a comma is put in double quotes); to set a
#'   scalar field differently per plot, pass a list of the same length as
#'   `id`.
#'
#'   Note the deliberate asymmetry with the positional scalar args above: a
#'   length-`N` *vector* passed to `dataCombined` or `plotType` aligns to the
#'   ids **by position** (one value per plot), whereas a length-`N` *atomic
#'   vector* passed as a `...` field is treated as one multi-value field and
#'   applied **whole** to every plot (collapsed to a comma-separated string),
#'   not split one-per-plot. So `title = c("A", "B")` gives every plot the
#'   single title `"A, B"`, not plot 1 `"A"` and plot 2 `"B"`. To vary a
#'   `...` field per plot, pass a length-`N` **list** (`title = list("A",
#'   "B")`).
#'
#'   `...` also accepts `overwrite`, a logical scalar (default `FALSE`): a
#'   plot id that already exists aborts unless `overwrite = TRUE`, which
#'   replaces it (last-write-wins).
#' @returns The `project` object, invisibly.
#' @export
#' @family plots
addPlot <- function(project, id, dataCombined, plotType, ...) {
  validateIsOfType(project, "Project")
  project$addPlot(id, dataCombined, plotType, ...)
}

# Implementation behind `project$addPlot()` / `addPlot()`.
#
# @keywords internal
# @noRd
.addPlot_impl <- function(
  self,
  private,
  id,
  dataCombined,
  plotType,
  ...,
  .call
) {
  rlang::local_error_call(.call)
  .requireNonEmptyStringVector(id, "id")
  id <- .canonicalizeId(id)
  n <- length(id)
  dataCombined <- .canonicalizeIdRef(
    .recycleScalarArg(dataCombined, n, "dataCombined")
  )
  plotType <- .recycleScalarArg(plotType, n, "plotType")
  dots <- list(...)
  # `overwrite` arrives through `...`; pull it out before building fields so it
  # is not stored as a plot field.
  overwrite <- .validateOverwriteFlag(dots[["overwrite"]])
  dots[["overwrite"]] <- NULL
  perDefinitionFields <- .dotsToPerDefinitionFields(dots, n)

  # Validate the whole batch first (all-or-nothing): no entry is folded in (and
  # so nothing is written through) unless every entry is valid. A within-batch
  # duplicate id or an existing id aborts unless overwriting, in which case the
  # last one wins.
  .assertNoOverwriteClash(id, names(self$definitions$plots), "plot", overwrite)
  unknownDc <- setdiff(dataCombined, names(self$definitions$dataCombined))
  if (length(unknownDc) > 0L) {
    cli::cli_abort("dataCombined {.val {unknownDc}} not found in project")
  }
  badType <- setdiff(plotType, .validPlotTypes)
  if (length(badType) > 0L) {
    cli::cli_abort(c(
      "Invalid plotType {.val {badType}}.",
      "i" = "Must be one of: {.val {.validPlotTypes}}."
    ))
  }
  newEntries <- lapply(seq_len(n), function(i) {
    .buildPlotEntry(
      id[[i]],
      dataCombined[[i]],
      plotType[[i]],
      perDefinitionFields[[i]]
    )
  })
  .abortOnPlotEntryFieldErrors(newEntries, call = .call)

  # Fold all N entries into the section in memory, then ONE assignment triggers
  # exactly one write-through.
  plotConfig <- .unwrapDefinitionList(private$.getSection("plots")) %||% list()
  for (i in seq_len(n)) {
    plotConfig[[id[[i]]]] <- newEntries[[i]]
  }
  private$.setSection("plots", plotConfig)
  invisible(self)
}

# Build one classed plotConfiguration entry from its scalar fields and the
# already-normalized optional fields.
#
# @keywords internal
# @noRd
.buildPlotEntry <- function(id, dataCombinedId, plotType, optionalFields) {
  entry <- c(
    list(
      plotId = id,
      dataCombinedId = dataCombinedId,
      plotType = plotType
    ),
    optionalFields
  )
  class(entry) <- c("Plot", "list")
  entry
}

#' Remove one or more plot configurations from a Project
#'
#' Drop the entries with matching `plotId`s. Warns (and skips) any `id` not
#' found, and warns when a removed plot is still referenced by any `plotGrids`
#' entry. All removals are written through in a single pass.
#'
#' @param project A `Project` object.
#' @param id Character vector of plot ids. Each is canonicalized the same way
#'   [addPlot()] canonicalizes it.
#' @returns The `project` object, invisibly.
#' @export
#' @family plots
removePlot <- function(project, id) {
  validateIsOfType(project, "Project")
  project$removePlot(id)
}

# Implementation behind `project$removePlot()` / `removePlot()`.
#
# @keywords internal
# @noRd
.removePlot_impl <- function(self, private, id, .call) {
  rlang::local_error_call(.call)
  .requireNonEmptyStringVector(id, "id")
  id <- .canonicalizeId(id)

  plotConfig <- .unwrapDefinitionList(private$.getSection("plots")) %||% list()
  missingIds <- setdiff(id, names(plotConfig))
  if (length(missingIds) > 0L) {
    cli::cli_warn("plot {.val {missingIds}} not found; no-op.")
  }
  toRemove <- intersect(id, names(plotConfig))
  if (length(toRemove) == 0L) {
    return(invisible(self))
  }

  grids <- .unwrapDefinitionList(private$.getSection("plotGrids")) %||% list()
  if (length(grids) > 0) {
    referencingGrids <- names(grids)[vapply(
      grids,
      function(g) any(toRemove %in% .splitPlotIDs(g$plotIds)),
      logical(1)
    )]
    if (length(referencingGrids) > 0) {
      cli::cli_warn(c(
        "Removed plot{?s} {.val {toRemove}} still referenced by {length(referencingGrids)} plot grid{?s}:",
        "*" = "{referencingGrids}"
      ))
    }
  }

  plotConfig[toRemove] <- NULL
  private$.setSection("plots", plotConfig)
  invisible(self)
}

#' Add one or more plot grids to a Project
#'
#' Add new entries to `plotGrids` definitions (a keyed list, one entry per grid).
#' Errors if a `plotGridId` already exists or if any of the supplied `plots`
#' are not present in `plots` definitions.
#'
#' @inherit vectorizedAuthoring details
#'
#' @param project A `Project` object.
#' @param id Character vector of unique plot-grid ids (the number of grids to
#'   add). Each is canonicalized to a safe, lowercase id (a warning names the
#'   result if it changed). Stored in the `plotGridId` field.
#' @param plots The plot ids each grid includes (stored internally as a
#'   comma-separated string). A character vector is applied whole to every
#'   grid; to give a different set of plot ids per grid, pass a list of the
#'   same length as `id` (one character vector per grid).
#' @param ... Optional plot-grid fields, e.g. `title`, `subtitle`. A scalar
#'   field is recycled to every grid; to set one differently per grid, pass a
#'   list of the same length as `id`.
#'
#'   A length-`N` *atomic vector* passed as a `...` field is treated as one
#'   multi-value field and applied **whole** to every grid (collapsed to a
#'   comma-separated string), not split one-per-grid. So `title = c("A", "B")`
#'   gives every grid the single title `"A, B"`, not grid 1 `"A"` and grid 2
#'   `"B"`. To vary a `...` field per grid, pass a length-`N` **list**
#'   (`title = list("A", "B")`).
#'
#'   `...` also accepts `overwrite`, a logical scalar (default `FALSE`): a
#'   plot-grid id that already exists aborts unless `overwrite = TRUE`, which
#'   replaces it (last-write-wins).
#' @returns The `project` object, invisibly.
#' @export
#' @family plots
addPlotGrid <- function(project, id, plots, ...) {
  validateIsOfType(project, "Project")
  project$addPlotGrid(id, plots, ...)
}

# Implementation behind `project$addPlotGrid()` / `addPlotGrid()`.
#
# @keywords internal
# @noRd
.addPlotGrid_impl <- function(self, private, id, plots, ..., .call) {
  rlang::local_error_call(.call)
  .requireNonEmptyStringVector(id, "id")
  id <- .canonicalizeId(id)
  n <- length(id)
  # `plots` is whole-per-grid: a bare character vector applies to every grid.
  # A per-grid list (one id vector per grid) is meaningful only when there is
  # more than one grid; then a length-N list aligns by position. For a single
  # grid, `plots = list("p1", "p2")` is that grid's whole id set, treated the
  # same as `plots = c("p1", "p2")`, so any list is flattened to a vector.
  perGridPlotIds <- if (n > 1L && is.list(plots) && length(plots) == n) {
    plots
  } else if (is.list(plots)) {
    rep(list(unlist(plots, use.names = FALSE)), n)
  } else {
    rep(list(plots), n)
  }
  dots <- list(...)
  # `overwrite` arrives through `...`; pull it out before building fields so it
  # is not stored as a grid field.
  overwrite <- .validateOverwriteFlag(dots[["overwrite"]])
  dots[["overwrite"]] <- NULL
  perGridFields <- .dotsToPerDefinitionFields(dots, n)

  # Validate the whole batch first (all-or-nothing). A within-batch duplicate id
  # aborts unless overwriting, in which case the last one wins.
  .assertNoOverwriteClash(
    id,
    names(self$definitions$plotGrids),
    "plot grid",
    overwrite
  )
  existingPlotIDs <- names(self$definitions$plots)
  if (is.null(existingPlotIDs)) {
    cli::cli_abort(c(
      "no plots are defined; add plots before creating a plot grid.",
      "i" = "use {.fn addPlot} to add plots referenced by {.arg plots}."
    ))
  }
  canonPlotIds <- lapply(perGridPlotIds, function(p) {
    .requireNonEmptyStringVector(p, "plots")
    .canonicalizeIdRef(p)
  })
  unknown <- setdiff(unique(unlist(canonPlotIds)), existingPlotIDs)
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "{.arg plots} references unknown plotIds:",
      "x" = "{.val {unknown}}"
    ))
  }

  # Fold all N grids in, then ONE assignment triggers one write-through.
  plotGrids <- .unwrapDefinitionList(private$.getSection("plotGrids")) %||%
    list()
  for (i in seq_len(n)) {
    entry <- c(
      list(
        plotGridId = id[[i]],
        plotIds = .joinPlotIDs(canonPlotIds[[i]])
      ),
      perGridFields[[i]]
    )
    class(entry) <- c("PlotGrid", "list")
    plotGrids[[id[[i]]]] <- entry
  }
  private$.setSection("plotGrids", plotGrids)
  invisible(self)
}

#' Remove one or more plot grids from a Project
#'
#' Drop the entries with matching `plotGridId`s. Warns (and skips) any `id`
#' not present. All removals are written through in a single pass.
#'
#' @param project A `Project` object.
#' @param id Character vector of plot-grid ids. Each is canonicalized the same
#'   way [addPlotGrid()] canonicalizes it.
#' @returns The `project` object, invisibly.
#' @export
#' @family plots
removePlotGrid <- function(project, id) {
  validateIsOfType(project, "Project")
  project$removePlotGrid(id)
}

# Implementation behind `project$removePlotGrid()` / `removePlotGrid()`.
#
# @keywords internal
# @noRd
.removePlotGrid_impl <- function(self, private, id, .call) {
  rlang::local_error_call(.call)
  .requireNonEmptyStringVector(id, "id")
  id <- .canonicalizeId(id)

  plotGrids <- .unwrapDefinitionList(private$.getSection("plotGrids")) %||%
    list()
  missingIds <- setdiff(id, names(plotGrids))
  if (length(missingIds) > 0L) {
    cli::cli_warn("plot grid {.val {missingIds}} not found; no-op.")
  }
  toRemove <- intersect(id, names(plotGrids))
  if (length(toRemove) == 0L) {
    return(invisible(self))
  }

  plotGrids[toRemove] <- NULL
  private$.setSection("plotGrids", plotGrids)
  invisible(self)
}

#' Add one or more DataCombined to a Project
#'
#' Append new DataCombined entries (each with one or more simulated and/or
#' observed rows) to `dataCombined` definitions. Pass a vector of ids to add
#' several DataCombined in one call. The labels of the curves in one
#' DataCombined must be distinct, counting default labels and simulated and
#' observed curves together; a repeated label aborts, so a project that cannot
#' be plotted is never written to disk.
#'
#' @inherit vectorizedAuthoring details
#'
#' @param project A `Project` object.
#' @param id Character vector of unique DataCombined ids (the number of
#'   DataCombined to add). Each is canonicalized to a safe, lowercase id (a
#'   warning names the result if it changed).
#' @param simulated For a single DataCombined (`id` length 1), a list of
#'   named lists, each including `scenario` and `path` (optional `label`,
#'   `group`, `xOffsets`, `xOffsetsUnits`, `yOffsets`, `yOffsetsUnits`,
#'   `xScaleFactors`, `yScaleFactors`). `path` may be either a model quantity
#'   path or an output-path id (a key of the project's `outputPaths`
#'   definitions); an id is resolved to its model path when the DataCombined is
#'   built by [createDataCombined()], and is matched the way the project's
#'   other references are matched, so its spelling need not be the canonical
#'   one. When `label` is left out, the curve is named by whatever is written
#'   in `path`. The `scenario` reference is canonicalized to match its scenario
#'   definition. To add several DataCombined in one call, pass a list of the
#'   same length as `id`, one such simulated list per DataCombined.
#' @param observed Like `simulated`, but each named list includes `label` and
#'   `dataSet` (optional fields as `simulated` minus `scenario` and `path`).
#'   `label` is required here.
#' @param overwrite Logical scalar. When `FALSE` (default), an id that already
#'   exists aborts. When `TRUE`, the existing DataCombined is replaced
#'   (last-write-wins).
#' @returns The `project` object, invisibly.
#' @export
#' @family dataCombined
addDataCombined <- function(
  project,
  id,
  simulated = list(),
  observed = list(),
  overwrite = FALSE
) {
  validateIsOfType(project, "Project")
  project$addDataCombined(id, simulated, observed, overwrite)
}

# Implementation behind `project$addDataCombined()` / `addDataCombined()`.
#
# @keywords internal
# @noRd
.addDataCombined_impl <- function(
  self,
  private,
  id,
  simulated = list(),
  observed = list(),
  overwrite = FALSE,
  .call
) {
  rlang::local_error_call(.call)
  .requireNonEmptyStringVector(id, "id")
  id <- .canonicalizeId(id)
  n <- length(id)
  # For a single DataCombined, `simulated` / `observed` are the entry lists. For
  # several, they are per-DataCombined lists (one entry list each); a length-N
  # list aligns by position, otherwise the same list applies to every id.
  perIdSimulated <- .perDataCombinedEntries(simulated, n)
  perIdObserved <- .perDataCombinedEntries(observed, n)

  # Validate the whole batch first (all-or-nothing). A within-batch duplicate id
  # aborts unless overwriting, in which case the last one wins.
  .assertNoOverwriteClash(
    id,
    names(self$definitions$dataCombined),
    "DataCombined",
    overwrite
  )
  for (i in seq_len(n)) {
    if (length(perIdSimulated[[i]]) == 0L && length(perIdObserved[[i]]) == 0L) {
      cli::cli_abort(
        "addDataCombined requires at least one simulated or observed entry"
      )
    }
    for (e in perIdSimulated[[i]]) {
      .checkDataCombinedEntry(e, "simulated")
    }
    for (e in perIdObserved[[i]]) {
      .checkDataCombinedEntry(e, "observed")
    }
    duplicated <- .duplicateDataCombinedLabels(
      perIdSimulated[[i]],
      perIdObserved[[i]]
    )
    if (length(duplicated) > 0L) {
      cli::cli_abort(messages$duplicateDataCombinedLabels(id[[i]], duplicated))
    }
  }

  # Fold all N in, then ONE assignment triggers one write-through.
  dataCombined <- .unwrapDefinitionList(private$.getSection(
    "dataCombined"
  )) %||%
    list()
  for (i in seq_len(n)) {
    # Canonicalize the scenario reference on each simulated entry so it matches
    # the canonical scenario id its definition was filed under.
    sim <- lapply(perIdSimulated[[i]], function(e) {
      if (!is.null(e$scenario)) {
        e$scenario <- .canonicalizeIdRef(e$scenario)
      }
      e
    })
    entry <- list(
      simulated = sim,
      observed = perIdObserved[[i]]
    )
    class(entry) <- c("DataCombined", "list")
    dataCombined[[id[[i]]]] <- entry
  }
  private$.setSection("dataCombined", dataCombined)
  invisible(self)
}

# Resolve the `simulated` / `observed` argument to a per-DataCombined list of
# entry lists. A length-N list of lists aligns by position; anything else (the
# scalar entry list, or an empty list) applies to every id.
#
# @keywords internal
# @noRd
.perDataCombinedEntries <- function(entries, n) {
  isListOfEntryLists <- is.list(entries) &&
    length(entries) == n &&
    all(vapply(entries, is.list, logical(1)))
  if (n > 1L && isListOfEntryLists) {
    return(entries)
  }
  rep(list(entries), n)
}

#' Remove one or more DataCombined from a Project
#'
#' Drop the named entries from `dataCombined` definitions. Warns (and skips) any
#' `id` not present, and warns about any plot entries that still reference a
#' removed id. All removals are written through in one pass.
#'
#' @param project A `Project` object.
#' @param id Character vector of DataCombined ids to remove. Each is
#'   canonicalized the same way [addDataCombined()] canonicalizes it.
#' @returns The `project` object, invisibly.
#' @export
#' @family dataCombined
removeDataCombined <- function(project, id) {
  validateIsOfType(project, "Project")
  project$removeDataCombined(id)
}

# Implementation behind `project$removeDataCombined()` / `removeDataCombined()`.
#
# @keywords internal
# @noRd
.removeDataCombined_impl <- function(self, private, id, .call) {
  rlang::local_error_call(.call)
  .requireNonEmptyStringVector(id, "id")
  id <- .canonicalizeId(id)

  dataCombined <- .unwrapDefinitionList(private$.getSection(
    "dataCombined"
  )) %||%
    list()
  missingIds <- setdiff(id, names(dataCombined))
  if (length(missingIds) > 0L) {
    cli::cli_warn("DataCombined {.val {missingIds}} not found; no-op.")
  }
  toRemove <- intersect(id, names(dataCombined))
  if (length(toRemove) == 0L) {
    return(invisible(self))
  }

  plotCfg <- .unwrapDefinitionList(private$.getSection("plots")) %||% list()
  if (length(plotCfg) > 0) {
    referencingPlots <- names(plotCfg)[vapply(
      plotCfg,
      function(p) isTRUE(p$dataCombinedId %in% toRemove),
      logical(1)
    )]
    if (length(referencingPlots) > 0) {
      cli::cli_warn(c(
        "Removed DataCombined {.val {toRemove}} still referenced by {length(referencingPlots)} plot{?s}:",
        "*" = "{referencingPlots}"
      ))
    }
  }

  dataCombined[toRemove] <- NULL
  private$.setSection("dataCombined", dataCombined)
  invisible(self)
}
