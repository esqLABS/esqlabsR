# Previous-version project snapshot upgrade ----
#
# A previous esqlabsR version froze a project to a single monolithic JSON with
# `snapshotProjectConfiguration()`: the project's Excel workbooks dumped
# sheet-by-sheet as `{column_names, rows}` objects, keyed by workbook name
# (`projectConfiguration`, `Scenarios`, `Individuals`, ...). That file is not a
# v6 `Project.json` (it carries no `schemaVersion` and none of the v6 section
# shapes), so `restoreProject()` cannot read it directly.
#
# This upgrades one on read: it rebuilds the Excel workbooks the snapshot mirrors
# into a scratch directory, then runs them through the Excel bridge
# (`importProjectFromExcel()`) to produce the v6 definitions tree. The parsing
# is the bridge's, not reimplemented here; this file only reverses the
# sheet-to-JSON dump and wires the bridge behind `restoreProject()`.

# The mirror's top-level section keys: the property/path sheet plus one key per
# configuration workbook. Every released `snapshotProjectConfiguration()`, back
# to the first, has written this same fixed set (a newer one adds sections such
# as `ParameterIdentification`; an older one omits them), so recognizing the
# mirror on these keys is stable across versions.
.legacySnapshotSectionKeys <- c(
  "projectConfiguration",
  "modelParameterSets",
  "Individuals",
  "Populations",
  "Scenarios",
  "Applications",
  "Plots",
  "ParameterIdentification"
)

# TRUE when `jsonData` is a previous-version monolithic snapshot rather than a v6
# `Project.json` / `.esqlabsR`. The discriminator is the absent `schemaVersion`
# (every v6 snapshot carries `"2.0"`) together with the mirror's signature
# `projectConfiguration` sheet (its property/path table, which the upgrade needs
# to locate the workbooks) and at least one workbook section, so neither a v6
# snapshot nor an unrelated JSON object is mistaken for one.
#
# @keywords internal
# @noRd
.isLegacySnapshot <- function(jsonData) {
  is.list(jsonData) &&
    is.null(jsonData$schemaVersion) &&
    !is.null(jsonData[["projectConfiguration"]]) &&
    length(intersect(names(jsonData), .legacySnapshotSectionKeys)) >= 2L
}

# Upgrade a previous-version monolithic snapshot to a v6 tree project at `dir`.
# Rebuilds the mirrored Excel workbooks into a scratch directory, runs them
# through `importProjectFromExcel()` (writing the tree into `dir`), informs the
# user, and returns the freshly loaded `Project`. `overwrite` is forwarded to
# the import so it governs replacing an existing project in `dir` the same way a
# v6 restore does. `replacedExistingTree` (whether `dir` already held a project)
# drives the same stale-handle warning the v6 restore path emits on overwrite.
#
# @keywords internal
# @noRd
.upgradeLegacySnapshot <- function(
  jsonData,
  dir,
  overwrite,
  replacedExistingTree = FALSE
) {
  scratch <- tempfile("legacy-snapshot")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)

  pcPath <- .materializeLegacySnapshot(jsonData, scratch)

  # `silent = TRUE`: the bridge's per-step chatter is an implementation detail
  # of the upgrade; the single `cli_inform` below is the user-facing signal.
  #
  # A snapshot never carries the observed-data workbook itself (only the project
  # configuration is mirrored), so the bridge's "configured data file not found"
  # warning fires on every such upgrade. Muffle just that classed warning: it is
  # redundant with the observed-data caveat the `upgradedLegacySnapshot()`
  # inform already carries, and left uncaught it fails the suite under
  # testthat's third edition. Any other bridge warning still surfaces.
  containerPath <- withCallingHandlers(
    importProjectFromExcel(
      pcPath,
      outputDir = dir,
      overwrite = overwrite,
      silent = TRUE
    ),
    esqlabsR_importSkippedObservedData = function(w) {
      invokeRestart("muffleWarning")
    }
  )

  cli::cli_inform(messages$upgradedLegacySnapshot())

  # The overwrite replaced a live tree; any `Project` loaded from `dir` before
  # this call now points at stale in-memory state. Same warning the v6 restore
  # path emits.
  if (replacedExistingTree && overwrite) {
    cli::cli_warn(messages$restoreOverwroteTree(dir))
  }

  loadProject(containerPath)
}

# Rebuild the Excel workbooks a monolithic snapshot mirrors, into `dir`. Writes
# the root `ProjectConfiguration.xlsx` (path/property sheet), one workbook per
# mirrored section under `Configurations/`, and any populations CSVs under
# `Configurations/PopulationsCSV/`. Returns the path to the root
# `ProjectConfiguration.xlsx`, the entry point `importProjectFromExcel()` reads.
#
# @keywords internal
# @noRd
.materializeLegacySnapshot <- function(jsonData, dir) {
  configDir <- file.path(dir, "Configurations")
  dir.create(configDir, recursive = TRUE, showWarnings = FALSE)

  pcPath <- file.path(dir, "Project.xlsx")
  .writeExcel(.legacySheetToDf(jsonData$projectConfiguration), pcPath)

  # Write each configuration workbook under the filename the snapshot's own
  # `projectConfiguration` records for it, not a hardcoded name: those filenames
  # are user-customizable, and the property sheet is copied verbatim into
  # `Project.xlsx`, so the bridge resolves each section from the property value.
  # Writing under a hardcoded name would leave a renamed section's file where
  # the bridge does not look, silently dropping it. Each mirror section maps to
  # its filename property (default filename when the property is absent).
  props <- .legacyConfigProperties(jsonData$projectConfiguration)
  workbookProps <- c(
    modelParameterSets = "modelParamsFile",
    Individuals = "individualsFile",
    Populations = "populationsFile",
    Scenarios = "scenariosFile",
    Applications = "applicationsFile",
    Plots = "plotsFile",
    ParameterIdentification = "parameterIdentificationFile"
  )
  defaultFiles <- c(
    modelParameterSets = "ModelParameters.xlsx",
    Individuals = "Individuals.xlsx",
    Populations = "Populations.xlsx",
    Scenarios = "Scenarios.xlsx",
    Applications = "Applications.xlsx",
    Plots = "Plots.xlsx",
    ParameterIdentification = "ParameterIdentification.xlsx"
  )
  for (section in names(workbookProps)) {
    sheets <- jsonData[[section]]
    if (is.null(sheets) || length(sheets) == 0L) {
      next
    }
    fileName <- props[[workbookProps[[section]]]] %||% defaultFiles[[section]]
    sheetDfs <- lapply(sheets, .legacySheetToDf)
    # Basename the property-supplied filename so a snapshot cannot steer the
    # write outside `configDir`; the bridge resolves it under the same folder.
    .writeExcel(sheetDfs, file.path(configDir, basename(fileName)))
  }

  if (
    !is.null(jsonData$populationsCSV) && length(jsonData$populationsCSV) > 0L
  ) {
    csvDir <- file.path(configDir, "PopulationsCSV")
    dir.create(csvDir, recursive = TRUE, showWarnings = FALSE)
    for (fileName in names(jsonData$populationsCSV)) {
      csvDf <- .legacySheetToDf(jsonData$populationsCSV[[fileName]])
      # Reduce the snapshot-supplied name to a bare filename: a snapshot is
      # read from a caller-supplied path, so a name carrying a path separator
      # or a `../` climb must not steer the write out of `csvDir`.
      utils::write.csv(
        csvDf,
        file.path(csvDir, basename(fileName)),
        row.names = FALSE
      )
    }
  }

  pcPath
}

# Extract the `Property -> Value` map from the mirrored `projectConfiguration`
# sheet, so the materializer can read a workbook's recorded filename rather than
# assume the conventional one. Returns a named character vector (empty when the
# sheet is absent or carries no `Property`/`Value` columns). A blank value is
# dropped so the caller's `%||%` default applies.
#
# @keywords internal
# @noRd
.legacyConfigProperties <- function(projectConfiguration) {
  if (is.null(projectConfiguration)) {
    return(character())
  }
  df <- .legacySheetToDf(projectConfiguration)
  if (!all(c("Property", "Value") %in% names(df)) || nrow(df) == 0L) {
    return(character())
  }
  values <- as.character(df$Value)
  names(values) <- as.character(df$Property)
  values <- values[!is.na(values) & values != ""]
  values
}

# Rebuild one mirrored sheet (`{column_names, rows}`, each row a named list of
# cell strings) into a data frame with the sheet's columns and per-column types
# restored. An empty sheet yields a zero-row frame with the right columns. A
# value that is not a `{column_names, rows}` object (a corrupt or hand-edited
# snapshot) aborts with a clear message rather than a cryptic downstream error.
#
# @keywords internal
# @noRd
.legacySheetToDf <- function(sheet) {
  # `rows` may be absent (an empty sheet), but a present `rows` must be a list
  # of list-shaped records; a scalar or otherwise malformed value is rejected
  # here rather than failing later with a cryptic indexing error.
  if (
    !is.list(sheet) ||
      is.null(sheet$column_names) ||
      (!is.null(sheet$rows) && !is.list(sheet$rows))
  ) {
    cli::cli_abort(messages$legacySnapshotMalformedSheet())
  }
  columnNames <- unlist(sheet$column_names)
  rows <- sheet$rows
  df <- data.frame(
    matrix(ncol = length(columnNames), nrow = length(rows)),
    stringsAsFactors = FALSE
  )
  colnames(df) <- columnNames
  for (i in seq_along(rows)) {
    rowData <- rows[[i]]
    if (!is.list(rowData)) {
      cli::cli_abort(messages$legacySnapshotMalformedSheet())
    }
    for (colName in columnNames) {
      # Read each cell by column name, not position: the exporter writes a row
      # as a named list, so name-keyed access is robust to any key reordering a
      # JSON round-trip might introduce. A null or absent cell becomes `""` (as
      # the previous version's own reader did), so a blank round-trips as blank
      # rather than the string "NA".
      df[i, colName] <- rowData[[colName]] %||% ""
    }
  }
  .restoreColumnTypes(df)
}

# Restore a column's type after the mirror flattened every cell to a string: a
# column whose non-blank values are all `TRUE`/`FALSE` becomes logical, one that
# parses fully as numeric becomes numeric, everything else stays character. This
# matches what the previous version wrote, so the rebuilt workbook reads back
# the same values it was exported from.
#
# @keywords internal
# @noRd
.restoreColumnTypes <- function(df) {
  if (ncol(df) == 0L || nrow(df) == 0L) {
    return(df)
  }
  for (colName in names(df)) {
    values <- df[[colName]]
    present <- values[!is.na(values) & values != ""]
    if (length(present) == 0L) {
      next
    }
    if (all(present %in% c("TRUE", "FALSE"))) {
      df[[colName]] <- as.logical(values)
      next
    }
    # Keep a zero-padded value (a leading zero followed by another digit, e.g.
    # `01`, `007`) as text: it is an id, not a number, and coercing it would
    # drop the padding and break any reference that still uses the padded form.
    # A lone `0` or a decimal like `0.5` is a genuine number and stays eligible.
    if (any(grepl("^0[0-9]", present))) {
      next
    }
    numeric <- suppressWarnings(as.numeric(values))
    # Numeric only when no non-blank value failed to parse (a blank cell is
    # allowed to become NA).
    if (!any(is.na(numeric) & !is.na(values) & values != "")) {
      df[[colName]] <- numeric
    }
  }
  df
}
