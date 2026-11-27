# Converting an esqlabsR 5 project ----
#
# `convertProjectToV6()` is the only part of the package that reads the esqlabsR
# 5 workbook layout. It reshapes the esqlabsR 5 sheets into the v6 workbook
# layout, which `importProjectFromExcel()` reads, and leaves the parsing to that
# import. Everything it needs lives in this file, including copies (prefixed
# `.v5`) of the helpers that read the esqlabsR 5 layout, so removing the
# conversion later touches nothing else.

# The subfolder of the project the esqlabsR 5 workbooks move to.
.esqlabsR5Folder <- "esqlabsR5"

# The converter's user-facing text, built the way the entries of `messages` in
# `R/messages.R` are (see the header there). It is kept here rather than in that
# catalog so that the conversion stays in one file.
.esqlabsR5Messages <- list()

.esqlabsR5Messages$notExcelFile <- function(path) {
  cliBullets(
    "x" = "{.file {path}} is not an Excel file. {.fn convertProjectToV6} \\
    reads the {.file ProjectConfiguration.xlsx} workbook of an esqlabsR 5 \\
    project."
  )
}

.esqlabsR5Messages$unreadableWorkbook <- function(path) {
  cliBullets(
    "x" = "{.file {path}} cannot be read as an Excel workbook."
  )
}

.esqlabsR5Messages$notProjectConfiguration <- function(path) {
  cliBullets(
    "x" = "{.file {path}} is not the project configuration of an esqlabsR 5 \\
    project: its first sheet has no {.field Property} and {.field Value} \\
    columns."
  )
}

.esqlabsR5Messages$alreadyV6Workbook <- function(path) {
  cliBullets(
    "x" = "{.file {path}} is already a v6 workbook: its property sheet has a \\
    {.field schemaVersion} row.",
    "i" = "Import it with {.fn importProjectFromExcel}."
  )
}

.esqlabsR5Messages$targetExists <- function(files) {
  cliBullets(
    "x" = "The conversion writes {cli::qty(length(files))}{?a file/files} \\
    that already {?exists/exist} and {?is/are} not an esqlabsR 5 workbook it \\
    reads: {.file {files}}.",
    "i" = "Move {cli::qty(length(files))}{?it/them} out of the project folder, \\
    then convert again."
  )
}

.esqlabsR5Messages$restoreIncomplete <- function(files) {
  cliBullets(
    "!" = "The project folder could not be put back completely. \\
    {cli::qty(length(files))}{?This file is/These files are} not where \\
    {?it was/they were} before: {.file {files}}.",
    "i" = "Find {cli::qty(length(files))}{?it/them} in the {.file esqlabsR5} \\
    folder and move {?it/them} back by hand."
  )
}

.esqlabsR5Messages$foldersOutside <- function(properties, values) {
  n <- length(properties)
  example <- "${SHARED}/Data"
  cliBullets(
    "x" = "{cli::qty(n)}{?A folder/Folders} of the project {?points/point} \\
    outside the project folder:",
    stats::setNames(
      sprintf(
        "{.field {properties[[%1$d]]}}: {.file {values[[%1$d]]}}",
        seq_len(n)
      ),
      rep("*", n)
    ),
    "i" = "A v6 project keeps its folders inside the project folder, or names \\
    them through an environment variable. Move the folder into the project \\
    and give its path from the project folder, or write the value with the \\
    variable, such as {.val {example}}, then convert again."
  )
}

.esqlabsR5Messages$configurationsFolderOutside <- function(value) {
  cliBullets(
    "x" = "{.field configurationsFolder} points outside the project folder: \\
    {.file {value}}.",
    "i" = "The esqlabsR 5 workbooks move into the {.file esqlabsR5} folder of \\
    the project, so they have to be inside it. Move the configurations folder \\
    into the project and give its path from the project folder, then convert \\
    again."
  )
}

.esqlabsR5Messages$converted <- function(folder) {
  cliBullets(
    "v" = "Converted the esqlabsR 5 project. Its esqlabsR 5 workbooks are now \\
    in {.file {folder}}.",
    "i" = "Edit the v6 workbooks and import them with \\
    {.fn importProjectFromExcel}, or load {.file Project.json} with \\
    {.fn loadProject}."
  )
}

.esqlabsR5Messages$envVarFolders <- function(properties, from, to) {
  n <- length(properties)
  cliBullets(
    "i" = "{cli::qty(n)}{?A folder names/Folders name} an environment \\
    variable the esqlabsR 5 way, as a part of the path. A v6 project writes \\
    it as {.code ${{NAME}}}, so {cli::qty(n)}{?the value was/the values were} \\
    rewritten:",
    stats::setNames(
      sprintf(
        "{.field {properties[[%1$d]]}}: {.val {from[[%1$d]]}} -> {.val {to[[%1$d]]}}",
        seq_len(n)
      ),
      rep("*", n)
    )
  )
}

# Raised by `.v5AppendParameterSets()`. `bullets` are the `old -> new` templates
# of `.canonicalizedIdBullets()`; the caller binds `sourceLabel` and
# `renamedCount` into their environment and passes it as `.envir`, so a sheet
# name holding `{` is never evaluated. `cli::qty()` re-arms the plural after the
# length-1 `sourceLabel`.
.esqlabsR5Messages$renamedParameterSets <- function(bullets) {
  c(
    "!" = "{renamedCount} parameter set{?s} in {.file {sourceLabel}} \\
    {cli::qty(renamedCount)}reuse{?s/} an id that is already taken, so \\
    {?it was/they were} renamed:",
    bullets,
    "i" = "A v6 project keeps every parameter set in the model-parameters \\
    workbook, so one sheet name cannot serve two sets. References made in \\
    {.file {sourceLabel}} point at the {cli::qty(renamedCount)}renamed \\
    set{?s}; rename the sheet and its references in the v6 workbooks to \\
    choose the id yourself."
  )
}

.esqlabsR5Messages$unparametrizedIndividuals <- function(
  filePath,
  ids,
  sheets
) {
  envir <- new.env(parent = parent.frame())
  assign("filePath", filePath, envir = envir)
  assign("ids", ids, envir = envir)
  assign("sheets", sheets, envir = envir)
  assign("n", length(ids), envir = envir)
  list(
    bullets = c(
      "!" = "{cli::qty(n)}{n} individual{?s} in {.file {filePath}} {?is/are} \\
      converted without the parameter set {?it/they} declare{?s/}, because the \\
      sheet did not parse as a parameter sheet:",
      stats::setNames(
        sprintf(
          "Individual {.val {ids[[%1$d]]}}: sheet {.val {sheets[[%1$d]]}}.",
          seq_along(ids)
        ),
        rep("x", length(ids))
      ),
      "i" = "Nothing else reports this: the individual has no parametrization \\
      and the project still validates, so every scenario using it runs on the \\
      unmodified model. Add the set to the v6 model-parameters workbook and \\
      to the individual's {.field ParameterSets} cell, then import again."
    ),
    envir = envir
  )
}

.esqlabsR5Messages$undefinedIndividualSheets <- function(filePath, sheets) {
  envir <- new.env(parent = parent.frame())
  assign("filePath", filePath, envir = envir)
  assign("sheets", sheets, envir = envir)
  assign("n", length(sheets), envir = envir)
  list(
    bullets = c(
      "!" = "{cli::qty(n)}{n} parameter {?sheet/sheets} of {.file {filePath}} \\
      {?names/name} an individual that {.field IndividualBiometrics} does not \\
      define, so that individual is not converted: {.val {sheets}}.",
      "i" = "An individual comes from its {.field IndividualBiometrics} row, so \\
      the sheet becomes a parameter set that no individual uses, and the \\
      {cli::qty(n)}scenario{?s} naming the individual {?has/have} nothing to \\
      resolve to.",
      "i" = "Add a row for it to the {.field IndividualBiometrics} sheet of the \\
      v6 individuals workbook and import again."
    ),
    envir = envir
  )
}

.esqlabsR5Messages$individualParameterSetsColumn <- function(
  filePath,
  ids,
  sets
) {
  envir <- new.env(parent = parent.frame())
  assign("filePath", filePath, envir = envir)
  assign("ids", ids, envir = envir)
  assign("sets", sets, envir = envir)
  assign("n", length(ids), envir = envir)
  list(
    bullets = c(
      "!" = "The {.field Individual Parameter Sets} column of \\
      {.file {filePath}} names {cli::qty(n)}{?a parameter set/parameter sets} \\
      that esqlabsR 5 did not apply, and that {?is/are} not linked to the \\
      individual:",
      stats::setNames(
        sprintf(
          "Individual {.val {ids[[%1$d]]}}: {.val {sets[[%1$d]]}}.",
          seq_along(ids)
        ),
        rep("x", length(ids))
      ),
      "i" = "esqlabsR 5 applies to an individual the sheet named after it, and \\
      so does the converted project. To apply another set as well, add it to \\
      the individual's {.field ParameterSets} cell in the v6 individuals \\
      workbook and import again."
    ),
    envir = envir
  )
}

.esqlabsR5Messages$droppedSheets <- function(dropped) {
  workbooks <- names(dropped)
  n <- length(unlist(dropped))
  cliBullets(
    "i" = "{cli::qty(n)}{?A sheet/Some sheets} with no place in the v6 \\
    workbooks {?was/were} not carried over; the esqlabsR 5 workbooks in \\
    {.file esqlabsR5} keep {?it/them}:",
    stats::setNames(
      sprintf(
        "{.file {workbooks[[%1$d]]}}: {.val {dropped[[%1$d]]}}",
        seq_along(dropped)
      ),
      rep("*", length(dropped))
    )
  )
}

.esqlabsR5Messages$ontogeniesNotReadable <- function(
  recordType,
  recordId,
  proteins,
  ontogenies
) {
  cliBullets(
    "!" = "The protein ontogenies of {recordType} {.val {recordId}} are not \\
    converted: the {.field Protein} and {.field Ontogeny} columns cannot be \\
    paired.",
    "i" = "{length(proteins)} protein{?s} against {length(ontogenies)} \\
    ontogen{?y/ies}; each protein needs exactly one ontogeny.",
    "i" = "Write the pairs into the {.field Protein Ontogenies} cell of the v6 \\
    workbook as {.val Protein:Ontogeny,Protein:Ontogeny} and import again."
  )
}

.esqlabsR5Messages$userDefinedVariability <- function(filePath, rows) {
  envir <- new.env(parent = parent.frame())
  assign("filePath", filePath, envir = envir)
  assign("rows", rows, envir = envir)
  list(
    bullets = c(
      "!" = "The {.field UserDefinedVariability} sheet of {.file {filePath}} is \\
      not converted: {cli::qty(rows)}{rows} row{?s} of parameter variability.",
      "i" = "esqlabsR 5 did not read the sheet, and a v6 population has no \\
      field for it. The esqlabsR 5 workbook in {.file esqlabsR5} keeps it."
    ),
    envir = envir
  )
}

.esqlabsR5Messages$notLogical <- function(field, value, file) {
  where <- if (is.null(file)) {
    "Cannot read the {.field {field}} value {.val {value}} as true or false."
  } else {
    "Cannot read the {.field {field}} value {.val {value}} in {.file {file}} \\
    as true or false."
  }
  cliBullets(
    "x" = where,
    "i" = "Use {.val TRUE} / {.val FALSE}, {.val 1} / {.val 0} or \\
    {.val Yes} / {.val No}, then convert again."
  )
}

.esqlabsR5Messages$skippedPlotExportConfiguration <- function(
  filePath,
  rows,
  grids
) {
  envir <- new.env(parent = parent.frame())
  assign("filePath", filePath, envir = envir)
  assign("rows", rows, envir = envir)
  assign("grids", grids, envir = envir)
  assign("n", length(grids), envir = envir)
  list(
    bullets = c(
      "!" = "The {.field exportConfiguration} sheet of {.file {filePath}} is \\
      not converted: {cli::qty(rows)}{rows} {?row/rows} describing where a plot \\
      grid's figure is written and at what size.",
      if (length(grids) > 0L) {
        c("x" = "{cli::qty(n)}Plot grid{?s} {.val {grids}}.")
      },
      "i" = "A project has no field for a figure's filename, width or height: \\
      {.fn createPlots} returns the plot objects and the caller saves them, for \\
      example with {.fn ospsuite.plots::exportPlot}. The esqlabsR 5 workbook in \\
      {.file esqlabsR5} keeps the sheet."
    ),
    envir = envir
  )
}

.esqlabsR5Messages$conflictingPlotIdColumns <- function(sheet, columns, pairs) {
  envir <- new.env(parent = parent.frame())
  assign("sheet", sheet, envir = envir)
  assign("used", columns[[1]], envir = envir)
  assign("other", columns[[2]], envir = envir)
  assign("pairs", pairs, envir = envir)
  assign("n", length(pairs), envir = envir)
  list(
    bullets = c(
      "!" = "The {.field {sheet}} sheet has both a {.field {used}} and a \\
      {.field {other}} column, and they hold different ids in {n} \\
      row{?s} ({.field {used}} / {.field {other}}): {.val {pairs}}.",
      "i" = "The v6 sheet keeps the id from {.field {used}}. Change it there \\
      and import again to use the other."
    ),
    envir = envir
  )
}

.esqlabsR5Messages$folderHoldsProject <- function(dir, found) {
  cliBullets(
    "x" = "{.file {dir}} already holds a v6 project or an earlier conversion: \\
    {.file {found}}.",
    "i" = "Load the v6 project with {.fn loadProject}. To convert the \\
    esqlabsR 5 workbooks again, first move {cli::qty(length(found))}{?this/these} \\
    out of the folder."
  )
}

#' Convert an esqlabsR 5 project to a v6 project
#'
#' @description Turns a project made with esqlabsR 5 into a project this version
#' of esqlabsR loads, in the project's own folder. It reads the
#' `ProjectConfiguration.xlsx` workbook and the workbooks of the
#' `Configurations/` folder it names, writes them again in the v6 layout, and
#' then runs [importProjectFromExcel()] on them, which writes `Project.json` and
#' the `definitions/` folder.
#'
#' The conversion changes the folder in this order:
#'
#' 1. It reads every esqlabsR 5 workbook and prepares the v6 workbooks. All of
#'    its warnings come from this step, and nothing in the folder changes yet.
#' 2. It moves `ProjectConfiguration.xlsx` and the esqlabsR 5 workbooks it read
#'    into an `esqlabsR5/` subfolder, keeping their paths inside it, so
#'    `Configurations/Scenarios.xlsx` becomes
#'    `esqlabsR5/Configurations/Scenarios.xlsx`.
#' 3. It writes the v6 workbooks: `Project.xlsx` beside where
#'    `ProjectConfiguration.xlsx` was, and the `Configurations/` workbooks
#'    under the names the project gave them.
#' 4. It runs [importProjectFromExcel()] on `Project.xlsx`.
#'
#' Models, observed data and population files stay where they are, so no path
#' in the project changes. If any step fails, or the conversion is
#' interrupted, the folder is put back as it was before and the error is
#' shown.
#'
#' The conversion refuses a folder that already holds a v6 project
#' (`Project.json`, `Project.xlsx` or files in `definitions/`) or an earlier
#' conversion (an `esqlabsR5/` folder), and a workbook that is already in the
#' v6 layout. Afterwards, edit the v6 workbooks and import them with
#' [importProjectFromExcel()], or work with the project in R.
#'
#' @param projectConfigPath Path to the esqlabsR 5 project configuration
#'   workbook. Defaults to `"ProjectConfiguration.xlsx"`.
#' @param silent Logical. If `TRUE`, the summary of the conversion and of the
#'   import is not shown. Warnings are shown either way. Defaults to `FALSE`.
#'
#' @returns The path to the `Project.json` the conversion wrote, invisibly.
#' @export
#' @family projectPersistence
#' @examples
#' \dontrun{
#' jsonPath <- convertProjectToV6("MyProject/ProjectConfiguration.xlsx")
#' project <- loadProject(jsonPath)
#' }
convertProjectToV6 <- function(
  projectConfigPath = "ProjectConfiguration.xlsx",
  silent = FALSE
) {
  ospsuite.utils::validateIsString(projectConfigPath)
  ospsuite.utils::validateIsLogical(silent)
  properties <- .v5CheckConvertible(projectConfigPath)
  plan <- .v5PlanConversion(projectConfigPath, properties)
  jsonPath <- .v5ApplyConversion(plan, silent)
  if (!silent) {
    .v5InformConversion(plan)
  }
  invisible(jsonPath)
}

# Abort unless `projectConfigPath` is an esqlabsR 5 property workbook in a folder
# that holds no v6 project and no earlier conversion. Nothing is changed.
#
# @returns The property sheet, as read.
# @keywords internal
# @noRd
.v5CheckConvertible <- function(
  projectConfigPath,
  .call = rlang::caller_env()
) {
  rlang::local_error_call(.call)
  if (!file.exists(projectConfigPath)) {
    cli::cli_abort(messages$fileNotFound(projectConfigPath))
  }
  if (
    !fs::is_file(projectConfigPath) ||
      is.na(readxl::excel_format(projectConfigPath))
  ) {
    cli::cli_abort(.esqlabsR5Messages$notExcelFile(projectConfigPath))
  }
  properties <- tryCatch(
    readExcel(projectConfigPath),
    error = function(e) {
      cli::cli_abort(
        .esqlabsR5Messages$unreadableWorkbook(projectConfigPath),
        parent = e
      )
    }
  )
  if (!all(c("Property", "Value") %in% names(properties))) {
    cli::cli_abort(
      .esqlabsR5Messages$notProjectConfiguration(projectConfigPath)
    )
  }
  if ("schemaVersion" %in% properties$Property) {
    cli::cli_abort(.esqlabsR5Messages$alreadyV6Workbook(projectConfigPath))
  }

  projectDir <- dirname(fs::path_abs(projectConfigPath))
  definitionFiles <- list.files(
    file.path(projectDir, "definitions"),
    pattern = "\\.json$",
    recursive = TRUE
  )
  # The esqlabsR 5 workbook may itself be called `Project.xlsx`; it moves away
  # before the v6 one is written, so only another file of that name is in the
  # way.
  v6Workbook <- file.path(projectDir, "Project.xlsx")
  found <- c(
    if (file.exists(file.path(projectDir, "Project.json"))) "Project.json",
    if (file.exists(v6Workbook) && !.samePath(v6Workbook, projectConfigPath)) {
      "Project.xlsx"
    },
    if (length(definitionFiles) > 0L) "definitions",
    if (file.exists(file.path(projectDir, .esqlabsR5Folder))) .esqlabsR5Folder
  )
  if (length(found) > 0L) {
    cli::cli_abort(.esqlabsR5Messages$folderHoldsProject(projectDir, found))
  }
  as.data.frame(properties)
}

# The workbook of each section of an esqlabsR 5 project, named by the property
# that names it, with the file name read when the property sheet has no such
# row. The order is the order the import reads them in, which decides which of
# two parameter sets with one id keeps it.
.v5SectionWorkbooks <- c(
  scenariosFile = "Scenarios.xlsx",
  modelParamsFile = "ModelParameters.xlsx",
  individualsFile = "Individuals.xlsx",
  populationsFile = "Populations.xlsx",
  applicationsFile = "Applications.xlsx",
  initialConditionsFile = "InitialConditions.xlsx",
  plotsFile = "Plots.xlsx",
  parameterIdentificationFile = "ParameterIdentification.xlsx"
)

# Read an esqlabsR 5 project and prepare its v6 workbooks, changing nothing on
# disk. Every warning of the conversion, and every abort about the content of
# the workbooks, comes from here, so a conversion that cannot proceed stops
# before the folder is touched.
#
# @returns A list:
#   - projectDir : the project folder
#   - moves      : the esqlabsR 5 files that move into `esqlabsR5/`
#   - outputs    : the files to write, each `list(path, sheets)` (sheets to
#                  write) or `list(path, copyOf)` (a moved file to copy back)
#   - v6Workbook : the property workbook the import is run on
# @keywords internal
# @noRd
.v5PlanConversion <- function(
  projectConfigPath,
  properties,
  .call = rlang::caller_env()
) {
  rlang::local_error_call(.call)
  # Normalized, so the paths the plan holds compare and relativize alike: the
  # section workbooks are normalized too.
  projectConfigPath <- normalizePath(projectConfigPath)
  projectDir <- dirname(projectConfigPath)
  propertyTable <- .v5PropertyTable(properties)
  envVarRewrites <- .v5EnvVarFolders(propertyTable)
  propertyTable$Value <- envVarRewrites$values

  configsFolder <- .v5ConfigurationsFolder(
    .v5PropertyLookup(propertyTable)("configurationsFolder"),
    projectDir,
    .call
  )
  propertyTable <- .v5WorkingFolders(
    propertyTable,
    projectDir,
    configsFolder,
    .call
  )
  props <- .v5PropertyLookup(propertyTable)
  sections <- .v5ReadableWorkbooks(props, configsFolder, .call)
  scenarios <- .v5ScenarioRecords(sections$scenariosFile$path)

  # Each section's v6 workbook, by property: `sheets` to write, or `copy` for a
  # workbook whose layout v6 reads as it is. A workbook left with no sheet is
  # not written.
  workbooks <- lapply(sections, function(section) {
    c(section, list(copy = TRUE))
  })
  workbooks <- .v5ConvertParameterWorkbooks(
    workbooks,
    sections,
    scenarios,
    .v5SectionTargets(props, configsFolder)
  )
  # The sheets that have no v6 place and hold nothing a warning names, by
  # workbook, for the summary.
  droppedSheets <- list()
  if (!is.null(sections$scenariosFile)) {
    converted <- .v5ConvertScenariosWorkbook(
      sections$scenariosFile$path,
      .call
    )
    workbooks$scenariosFile$copy <- FALSE
    workbooks$scenariosFile$sheets <- converted$sheets
    droppedSheets[[sections$scenariosFile$name]] <- converted$dropped
  }
  if (!is.null(sections$plotsFile)) {
    converted <- .v5ConvertPlotsWorkbook(sections$plotsFile$path)
    workbooks$plotsFile$copy <- FALSE
    workbooks$plotsFile$sheets <- converted$sheets
    droppedSheets[[sections$plotsFile$name]] <- converted$dropped
  }
  if (!is.null(sections$populationsFile)) {
    converted <- .v5ConvertPopulationsWorkbook(sections$populationsFile$path)
    workbooks$populationsFile$copy <- FALSE
    workbooks$populationsFile$sheets <- converted$sheets
    droppedSheets[[sections$populationsFile$name]] <- converted$dropped
  }

  outputs <- list()
  for (workbook in workbooks) {
    if (isTRUE(workbook$copy)) {
      outputs[[length(outputs) + 1L]] <- list(
        path = workbook$path,
        copyOf = workbook$path
      )
    } else if (length(workbook$sheets) > 0L) {
      outputs[[length(outputs) + 1L]] <- list(
        path = workbook$path,
        sheets = workbook$sheets
      )
    }
  }

  propertyTable <- .v5DeclareWorkbooks(propertyTable, workbooks, sections)
  propertySheets <- list(Project = .v5PropertySheet(propertyTable))
  # Only a workbook written since 6.0.0 has the sheet, but the import reads it
  # from any property workbook, so it is carried over unchanged.
  if (
    .defaultSolverSettingsSheet %in% readxl::excel_sheets(projectConfigPath)
  ) {
    propertySheets[[.defaultSolverSettingsSheet]] <- as.data.frame(readExcel(
      projectConfigPath,
      sheet = .defaultSolverSettingsSheet
    ))
  }
  v6Workbook <- file.path(projectDir, "Project.xlsx")
  outputs <- c(
    list(list(path = v6Workbook, sheets = propertySheets)),
    outputs
  )

  moves <- c(
    projectConfigPath,
    vapply(sections, function(section) section$path, character(1))
  )
  targets <- vapply(outputs, function(output) output$path, character(1))
  inTheWay <- targets[file.exists(targets) & !(targets %in% moves)]
  if (length(inTheWay) > 0L) {
    cli::cli_abort(.esqlabsR5Messages$targetExists(
      as.character(fs::path_rel(inTheWay, projectDir))
    ))
  }

  list(
    projectDir = projectDir,
    moves = unname(moves),
    outputs = outputs,
    v6Workbook = v6Workbook,
    envVarRewrites = envVarRewrites$rewrites,
    droppedSheets = Filter(length, droppedSheets)
  )
}

# The property sheet as a `Property` / `Value` / `Description` table of
# character columns, without the rows that name no property.
#
# @keywords internal
# @noRd
.v5PropertyTable <- function(properties) {
  table <- data.frame(
    Property = as.character(properties$Property),
    Value = as.character(properties$Value),
    Description = if ("Description" %in% names(properties)) {
      as.character(properties$Description)
    } else {
      NA_character_
    },
    stringsAsFactors = FALSE
  )
  table[!vapply(table$Property, .isBlankCell, logical(1)), , drop = FALSE]
}

# The working folders of an esqlabsR 5 project. `modelFolder` is the esqlabsR 5
# name of `simulationsFolder`.
.v5FolderProperties <- c(
  "dataFolder",
  "modelFolder",
  "simulationsFolder",
  "populationsFolder",
  "outputFolder"
)

# The working folders whose values esqlabsR 5 reads environment variables in.
.v5EnvVarFolderProperties <- c(
  "dataFolder",
  "modelFolder",
  "populationsFolder",
  "outputFolder"
)

# Write the environment variables a working folder names the esqlabsR 5 way as
# `${NAME}`, the way a v6 project names them.
#
# esqlabsR 5 splits a folder value at `/` and replaces each part that is the
# name of a set environment variable by its value, except a part containing
# "path" (so the `PATH` variable of Windows is never read). A v6 project reads
# only `${NAME}`, so each such part becomes `${NAME}`. The project then resolves
# the folder the same way esqlabsR 5 did, and a colleague's own value of the
# variable is used on their computer. A part that is not a valid variable name
# for `${NAME}` is kept as written.
#
# @param table The property table.
# @returns `list(values, rewrites)`: the `Value` column with the rewritten
#   folders, and a data frame of the rewrites (`property`, `from`, `to`).
# @keywords internal
# @noRd
.v5EnvVarFolders <- function(table) {
  values <- table$Value
  rewrites <- data.frame(
    property = character(),
    from = character(),
    to = character(),
    stringsAsFactors = FALSE
  )
  for (i in which(table$Property %in% .v5EnvVarFolderProperties)) {
    value <- values[[i]]
    if (.isBlankCell(value)) {
      next
    }
    rewritten <- .v5EnvVarSegments(value)
    if (!identical(rewritten, value)) {
      values[[i]] <- rewritten
      rewrites[nrow(rewrites) + 1L, ] <- c(
        table$Property[[i]],
        value,
        rewritten
      )
    }
  }
  list(values = values, rewrites = rewrites)
}

# One folder value with each part that names a set environment variable
# written as `${NAME}`; see `.v5EnvVarFolders()`.
#
# @keywords internal
# @noRd
.v5EnvVarSegments <- function(value) {
  segments <- strsplit(value, "/", fixed = TRUE)[[1]]
  for (i in seq_along(segments)) {
    segment <- segments[[i]]
    if (
      !grepl("^[A-Za-z_][A-Za-z0-9_]*$", segment) ||
        grepl("path", segment, ignore.case = TRUE)
    ) {
      next
    }
    if (nzchar(Sys.getenv(segment))) {
      segments[[i]] <- paste0("${", segment, "}")
    }
  }
  rewritten <- paste(segments, collapse = "/")
  # `strsplit()` drops the empty part after a trailing `/`.
  if (endsWith(value, "/")) {
    rewritten <- paste0(rewritten, "/")
  }
  rewritten
}

# Resolve the configurations folder, which has to be inside the project folder:
# the esqlabsR 5 workbooks in it move into `esqlabsR5/`, keeping their path
# inside the project.
#
# @returns The absolute, normalized folder, or NULL when the project names none.
# @keywords internal
# @noRd
.v5ConfigurationsFolder <- function(value, projectDir, .call) {
  rlang::local_error_call(.call)
  if (is.null(value)) {
    return(NULL)
  }
  if (.declaresEnvVarPath(value) || .pathEscapesRoot(value, projectDir)) {
    cli::cli_abort(.esqlabsR5Messages$configurationsFolderOutside(value))
  }
  normalizePath(.absoluteAgainstRoot(value, projectDir), mustWork = FALSE)
}

# Give the working folders the values a v6 project reads them by, and refuse a
# folder outside the project folder, which a v6 project cannot name except
# through an environment variable.
#
# @keywords internal
# @noRd
.v5WorkingFolders <- function(table, projectDir, configsFolder, .call) {
  rlang::local_error_call(.call)
  populations <- which(table$Property == "populationsFolder")
  if (length(populations) > 0L) {
    table$Value[populations] <- vapply(
      table$Value[populations],
      .v5ResolvePopulationsFolder,
      character(1),
      projectDir = projectDir,
      configsFolder = configsFolder,
      USE.NAMES = FALSE
    )
  }

  outside <- vapply(
    seq_len(nrow(table)),
    function(i) {
      value <- table$Value[[i]]
      table$Property[[i]] %in%
        .v5FolderProperties &&
        !.isBlankCell(value) &&
        !.declaresEnvVarPath(value) &&
        .pathEscapesRoot(value, projectDir)
    },
    logical(1)
  )
  if (any(outside)) {
    cli::cli_abort(.esqlabsR5Messages$foldersOutside(
      table$Property[outside],
      table$Value[outside]
    ))
  }
  table
}

# Point `populationsFolder` at the folder that holds the csv files.
#
# esqlabsR 5 reads `populationsFolder` as a folder under the configurations
# folder (`PopulationsCSV`), while a v6 project reads every working folder from
# the project folder. A folder of that name in the project folder is taken as
# written; otherwise, when the folder is under the configurations folder, the
# value becomes its path from the project folder. A `${VAR}` value, an absolute
# value, and a folder found in neither place are kept as written.
#
# @param value The `populationsFolder` value.
# @param projectDir The project folder.
# @param configsFolder The resolved configurations folder, or NULL.
# @returns The value the v6 property sheet holds.
# @keywords internal
# @noRd
.v5ResolvePopulationsFolder <- function(value, projectDir, configsFolder) {
  if (
    .isBlankCell(value) ||
      is.null(configsFolder) ||
      .declaresEnvVarPath(value) ||
      fs::is_absolute_path(value)
  ) {
    return(value)
  }
  root <- normalizePath(projectDir, mustWork = FALSE)
  if (fs::dir_exists(fs::path(root, value))) {
    return(value)
  }
  underConfigs <- fs::path(configsFolder, value)
  if (!fs::dir_exists(underConfigs)) {
    return(value)
  }
  relative <- as.character(fs::path_rel(underConfigs, root))
  if (.pathEscapesRoot(relative, root)) {
    return(value)
  }
  relative
}

# Make the property rows name exactly the section workbooks the conversion
# writes: a row is added for a workbook the project reads under its usual name
# without naming it, and removed for an esqlabsR 5 workbook whose content all
# moved to another workbook. The row of a workbook the project names but that
# does not exist is kept, so the import still reports it.
#
# @keywords internal
# @noRd
.v5DeclareWorkbooks <- function(table, workbooks, sections) {
  for (property in names(.v5SectionWorkbooks)) {
    workbook <- workbooks[[property]]
    written <- !is.null(workbook) &&
      (isTRUE(workbook$copy) || length(workbook$sheets) > 0L)
    if (written && !(property %in% table$Property)) {
      table <- rbind(
        table,
        data.frame(
          Property = property,
          Value = workbook$name,
          Description = NA_character_,
          stringsAsFactors = FALSE
        )
      )
    } else if (!written && property %in% names(sections)) {
      table <- table[table$Property != property, , drop = FALSE]
    }
  }
  table
}

# Where each section workbook goes, `list(name, path)` by property: the file the
# project names, or the usual name. Used for a workbook the conversion writes
# although the esqlabsR 5 project had none, such as the model-parameters
# workbook of a project whose parameter sets were all in the individuals
# workbook.
#
# @keywords internal
# @noRd
.v5SectionTargets <- function(props, configsFolder) {
  if (is.null(configsFolder)) {
    return(list())
  }
  targets <- list()
  for (property in names(.v5SectionWorkbooks)) {
    name <- props(property) %||% .v5SectionWorkbooks[[property]]
    targets[[property]] <- list(
      property = property,
      name = name,
      path = normalizePath(
        .absoluteAgainstRoot(name, configsFolder),
        mustWork = FALSE
      )
    )
  }
  targets
}

# The scenarios of the esqlabsR 5 scenarios workbook, as far as the conversion of
# the other workbooks needs them: `list(id, individual, outputPaths)` for each
# row that names a scenario.
#
# @keywords internal
# @noRd
.v5ScenarioRecords <- function(scenariosFile) {
  if (is.null(scenariosFile)) {
    return(list())
  }
  if (!("Scenarios" %in% readxl::excel_sheets(scenariosFile))) {
    return(list())
  }
  df <- readExcel(scenariosFile, sheet = "Scenarios")
  records <- list()
  for (i in seq_len(nrow(df))) {
    id <- .v5CellValue(df, "Scenario_name", i)
    if (.isBlankCell(id)) {
      next
    }
    records[[length(records) + 1L]] <- list(
      id = as.character(id),
      individual = .naToNull(as.character(.v5CellValue(df, "IndividualId", i))),
      outputPaths = .parseCommaListToArray(.v5CellValue(
        df,
        "OutputPathsIds",
        i
      ))
    )
  }
  records
}

# A `Property -> Value` lookup over the property sheet, returning NULL for a
# property the sheet has no row for or leaves blank.
#
# @keywords internal
# @noRd
.v5PropertyLookup <- function(properties) {
  values <- stats::setNames(
    as.character(properties$Value),
    as.character(properties$Property)
  )
  function(name) {
    if (!(name %in% names(values))) {
      return(NULL)
    }
    value <- values[[name]]
    if (.isBlankCell(value)) NULL else value
  }
}

# The section workbooks that exist, in the order the import reads them, each as
# `list(property, name, path)`. A workbook whose property row is missing is
# looked for under its usual name, as the import does. A file name that climbs
# out of the configurations folder aborts naming the property.
#
# @keywords internal
# @noRd
.v5ReadableWorkbooks <- function(props, configsFolder, .call) {
  rlang::local_error_call(.call)
  if (is.null(configsFolder)) {
    return(list())
  }
  sections <- list()
  for (property in names(.v5SectionWorkbooks)) {
    name <- props(property) %||% .v5SectionWorkbooks[[property]]
    if (.pathEscapesRoot(name, configsFolder)) {
      cli::cli_abort(messages$projectPathEscapesRoot(
        property,
        name,
        configsFolder
      ))
    }
    path <- normalizePath(
      .absoluteAgainstRoot(name, configsFolder),
      mustWork = FALSE
    )
    if (!file.exists(path)) {
      next
    }
    sections[[property]] <- list(property = property, name = name, path = path)
  }
  sections
}

# The v6 property sheet: a `schemaVersion` and an `esqlabsRVersion` row, then
# the esqlabsR 5 rows with `modelFolder` under its v6 name, `simulationsFolder`.
# esqlabsR 5 reads only `modelFolder`, so its value wins over a
# `simulationsFolder` row the sheet may also have.
#
# @keywords internal
# @noRd
.v5PropertySheet <- function(table) {
  if ("modelFolder" %in% table$Property) {
    table <- table[table$Property != "simulationsFolder", , drop = FALSE]
    table$Property[table$Property == "modelFolder"] <- "simulationsFolder"
  }
  table <- table[table$Property != "esqlabsRVersion", , drop = FALSE]
  rbind(
    data.frame(
      Property = c("schemaVersion", "esqlabsRVersion"),
      Value = c("2.0", as.character(utils::packageVersion("esqlabsR"))),
      Description = c(
        "Project structure schema version",
        "esqlabsR version used to generate this file"
      ),
      stringsAsFactors = FALSE
    ),
    table
  )
}

# Report what the conversion changed beyond the layout of the sheets: where the
# esqlabsR 5 workbooks went, and which folder values it rewrote. The messages
# carry the class `esqlabsR_conversionSummary`.
#
# @keywords internal
# @noRd
.v5InformConversion <- function(plan) {
  cli::cli_inform(
    .esqlabsR5Messages$converted(
      .readablePath(file.path(plan$projectDir, .esqlabsR5Folder))
    ),
    class = "esqlabsR_conversionSummary"
  )
  rewrites <- plan$envVarRewrites
  if (nrow(rewrites) > 0L) {
    cli::cli_inform(
      .esqlabsR5Messages$envVarFolders(
        rewrites$property,
        rewrites$from,
        rewrites$to
      ),
      class = "esqlabsR_conversionSummary"
    )
  }
  if (length(plan$droppedSheets) > 0L) {
    cli::cli_inform(
      .esqlabsR5Messages$droppedSheets(plan$droppedSheets),
      class = "esqlabsR_conversionSummary"
    )
  }
  invisible(NULL)
}

# Move the esqlabsR 5 files into `esqlabsR5/`, write the v6 workbooks and import
# them. On an error or an interrupt, the folder is put back as it was and the
# condition goes on to the caller.
#
# @returns The path to the `Project.json` the import wrote.
# @keywords internal
# @noRd
.v5ApplyConversion <- function(plan, silent) {
  projectDir <- plan$projectDir
  definitionsDir <- file.path(projectDir, "definitions")
  state <- rlang::env(
    projectDir = projectDir,
    moved = character(),
    written = character(),
    createdDirs = character(),
    definitionsBefore = if (dir.exists(definitionsDir)) {
      list.files(
        definitionsDir,
        recursive = TRUE,
        all.files = TRUE,
        include.dirs = TRUE
      )
    }
  )
  done <- FALSE
  on.exit(
    if (!done) {
      .v5RestoreFolder(state)
    },
    add = TRUE
  )

  for (from in plan$moves) {
    to <- file.path(
      projectDir,
      .esqlabsR5Folder,
      fs::path_rel(from, projectDir)
    )
    .v5CreateDir(dirname(to), state)
    fs::file_move(from, to)
    state$moved[[from]] <- to
  }

  for (output in plan$outputs) {
    .v5CreateDir(dirname(output$path), state)
    state$written <- c(state$written, output$path)
    if (is.null(output$copyOf)) {
      .writeExcel(output$sheets, output$path)
    } else {
      fs::file_copy(state$moved[[output$copyOf]], output$path)
    }
  }

  jsonPath <- importProjectFromExcel(plan$v6Workbook, silent = silent)
  done <- TRUE
  jsonPath
}

# Create `dir` and the folders above it that do not exist, recording each one
# created so a restore can remove it again.
#
# @keywords internal
# @noRd
.v5CreateDir <- function(dir, state) {
  missing <- character()
  while (!dir.exists(dir)) {
    missing <- c(dir, missing)
    dir <- dirname(dir)
  }
  for (folder in missing) {
    dir.create(folder)
    state$createdDirs <- c(state$createdDirs, folder)
  }
  invisible(NULL)
}

# Put the project folder back as it was before the conversion: remove what the
# import and the conversion wrote, move the esqlabsR 5 files back, and remove the
# folders the conversion created. Each step goes ahead when an earlier one
# fails, so as much as possible is restored; a file that cannot be moved back is
# named in a warning, and the folder holding it is kept.
#
# @keywords internal
# @noRd
.v5RestoreFolder <- function(state) {
  projectDir <- state$projectDir
  unlink(file.path(projectDir, "Project.json"))
  definitionsDir <- file.path(projectDir, "definitions")
  if (is.null(state$definitionsBefore)) {
    unlink(definitionsDir, recursive = TRUE)
  } else if (dir.exists(definitionsDir)) {
    added <- setdiff(
      list.files(
        definitionsDir,
        recursive = TRUE,
        all.files = TRUE,
        include.dirs = TRUE
      ),
      state$definitionsBefore
    )
    # Deepest first, so a folder is empty by the time it is removed.
    for (path in rev(sort(added))) {
      unlink(file.path(definitionsDir, path), recursive = TRUE)
    }
  }
  unlink(state$written)

  notRestored <- character()
  for (from in names(state$moved)) {
    restored <- tryCatch(
      {
        fs::file_move(state$moved[[from]], from)
        TRUE
      },
      error = function(e) FALSE
    )
    if (!restored) {
      notRestored <- c(notRestored, from)
    }
  }
  for (dir in rev(state$createdDirs)) {
    isEmpty <- length(list.files(dir, all.files = TRUE, no.. = TRUE)) == 0L
    if (dir.exists(dir) && isEmpty) {
      unlink(dir, recursive = TRUE)
    }
  }
  if (length(notRestored) > 0L) {
    cli::cli_warn(.esqlabsR5Messages$restoreIncomplete(
      as.character(fs::path_rel(notRestored, projectDir))
    ))
  }
  invisible(NULL)
}

# Parameter sets ----
#
# esqlabsR 5 keeps parameter sets in three workbooks: every sheet of the
# model-parameters workbook, every sheet of the individuals workbook but the
# biometrics one (a sheet named after an individual is that individual's own
# set), and every sheet of the applications workbook (one per application
# protocol). A v6 project keeps them all in the model-parameters workbook, lists
# the application protocols on an `ApplicationProtocols` sheet, and links an
# individual to its sets through the `ParameterSets` column.

# Move every parameter set into the model-parameters workbook, in the order the
# import reads the three workbooks in, and point the individuals and the
# application protocols at the sets they use.
#
# The three workbooks had a sheet namespace each, so a sheet name can be taken
# by an earlier workbook: the later sheet is renamed, and the references its own
# workbook makes follow it (see `.v5AppendParameterSets()`).
#
# @param workbooks The v6 workbooks by property, as `.v5PlanConversion()` builds
#   them.
# @param sections The esqlabsR 5 workbooks read, by property.
# @param scenarios The scenario records (`.v5ScenarioRecords()`).
# @param targets Where each section workbook goes (`.v5SectionTargets()`).
# @returns `workbooks`, with the model-parameters, individuals and applications
#   workbooks written anew.
# @keywords internal
# @noRd
.v5ConvertParameterWorkbooks <- function(
  workbooks,
  sections,
  scenarios,
  targets
) {
  if (length(targets) == 0L) {
    return(workbooks)
  }
  sets <- list()

  modelParams <- sections$modelParamsFile
  if (!is.null(modelParams)) {
    parsed <- .parseExcelParameterSheets(modelParams$path)
    sets <- .v5AppendParameterSets(sets, parsed, modelParams$path)$sets
  }

  individuals <- sections$individualsFile
  if (!is.null(individuals)) {
    converted <- .v5ConvertIndividualsWorkbook(
      individuals$path,
      sets,
      scenarios
    )
    sets <- converted$sets
    workbooks$individualsFile <- c(
      individuals,
      list(copy = FALSE, sheets = converted$sheets)
    )
  }

  applications <- sections$applicationsFile
  if (!is.null(applications)) {
    converted <- .v5ConvertApplicationsWorkbook(applications$path, sets)
    sets <- converted$sets
    workbooks$applicationsFile <- c(
      applications,
      list(copy = FALSE, sheets = converted$sheets)
    )
  }

  if (length(sets) > 0L || !is.null(modelParams)) {
    workbooks$modelParamsFile <- c(
      modelParams %||% targets$modelParamsFile,
      list(copy = FALSE, sheets = lapply(sets, .v5ParameterSetSheet))
    )
  }
  workbooks
}

# A parameter set as a sheet of the model-parameters workbook.
#
# @param entries The set as `.parseExcelParameterSheets()` returns it.
# @keywords internal
# @noRd
.v5ParameterSetSheet <- function(entries) {
  data.frame(
    `Container Path` = vapply(entries, \(e) e$containerPath, character(1)),
    `Parameter Name` = vapply(entries, \(e) e$parameterName, character(1)),
    Value = vapply(entries, \(e) as.double(e$value), double(1)),
    Units = vapply(entries, \(e) e$units %||% NA_character_, character(1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# The longest sheet name Excel allows.
.v5MaxSheetName <- 31L

# A sheet name for a parameter set whose id is not yet taken.
#
# The name is kept when its canonical id is free. Otherwise `_2`, `_3`, ... is
# added, as `.v5UniqueImportedId()` adds it to an id, cutting the name so the
# result still fits an Excel sheet name. The check is on the canonical id,
# because two names that canonicalize alike (`Rat`, `rat`) become one id.
#
# @param name The sheet name.
# @param taken The canonical ids already taken.
# @returns The sheet name to write the set under.
# @keywords internal
# @noRd
.v5UniqueSheetName <- function(name, taken) {
  if (!(.canonicalizeOneId(name) %in% taken)) {
    return(name)
  }
  n <- 1L
  repeat {
    n <- n + 1L
    suffix <- paste0("_", n)
    candidate <- paste0(
      substr(name, 1L, .v5MaxSheetName - nchar(suffix)),
      suffix
    )
    if (!(.canonicalizeOneId(candidate) %in% taken)) {
      return(candidate)
    }
  }
}

# Append the sets of one workbook to the sets collected so far, renaming a set
# whose id an earlier set already took.
#
# Before 6.0.0 the model-parameters, individuals and applications workbooks were
# three namespaces, so the same sheet name in two of them is legitimate; in one
# workbook, two sheet names that canonicalize alike are too. The set collected
# first keeps the id, the later one is renamed (`Indiv1` -> `Indiv1_2`), and a
# warning names each rename and the workbook.
#
# @param sets The sets collected so far, by sheet name.
# @param incoming The sets of one workbook, by sheet name.
# @param source The workbook `incoming` was read from, named in the warning.
# @returns `list(sets, renames)`: the collected sets, and a named character
#   vector mapping each renamed set's canonical id to its new sheet name, which
#   `.v5ApplyIdRenames()` applies to the references the workbook makes.
# @keywords internal
# @noRd
.v5AppendParameterSets <- function(sets, incoming, source) {
  if (length(incoming) == 0L) {
    return(list(sets = sets, renames = character()))
  }
  taken <- vapply(
    names(sets),
    .canonicalizeOneId,
    character(1),
    USE.NAMES = FALSE
  )
  newNames <- character(length(incoming))
  renames <- character()
  for (i in seq_along(incoming)) {
    name <- names(incoming)[[i]]
    newNames[[i]] <- .v5UniqueSheetName(name, taken)
    taken <- c(taken, .canonicalizeOneId(newNames[[i]]))
    if (!identical(newNames[[i]], name)) {
      renames <- c(
        renames,
        stats::setNames(newNames[[i]], .canonicalizeOneId(name))
      )
    }
  }

  if (length(renames) > 0L) {
    renamed <- newNames != names(incoming)
    rendered <- .canonicalizedIdBullets(
      names(incoming)[renamed],
      newNames[renamed]
    )
    rendered$envir$sourceLabel <- basename(source)
    rendered$envir$renamedCount <- length(renames)
    cli::cli_warn(
      .esqlabsR5Messages$renamedParameterSets(rendered$bullets),
      .envir = rendered$envir,
      class = "esqlabsR_importRenamedParameterSets"
    )
  }

  list(
    sets = c(sets, stats::setNames(incoming, newNames)),
    renames = renames
  )
}

# Point parameter-set references at the sets `.v5AppendParameterSets()` renamed.
#
# Matched on the canonical id, as the rename was: a cell spelling `indiv1` for a
# sheet named `Indiv1` names that sheet.
#
# @param ids Referenced parameter-set ids.
# @param renames Named character vector, canonical old id -> new sheet name.
# @returns `ids` with every renamed entry replaced.
# @keywords internal
# @noRd
.v5ApplyIdRenames <- function(ids, renames) {
  if (length(renames) == 0L || length(ids) == 0L) {
    return(ids)
  }
  ids <- as.character(ids)
  canonical <- vapply(ids, .canonicalizeOneId, character(1), USE.NAMES = FALSE)
  mapped <- unname(renames[canonical])
  ifelse(is.na(mapped), ids, mapped)
}

# Drop the references to sheets of a workbook that are not parameter sheets.
#
# A `ParameterSets` cell names sheets of its own workbook, so a sheet that did
# not parse as a parameter set leaves a reference to a set that does not exist.
# Only those references are dropped; the cell may also name a set of another
# workbook. Matched on the canonical id.
#
# @param refs Referenced parameter-set ids.
# @param skippedSheets The sheets of the workbook that are not parameter sheets.
# @returns `refs` without the dropped references.
# @keywords internal
# @noRd
.v5DropSkippedSheetRefs <- function(refs, skippedSheets) {
  if (length(refs) == 0L || length(skippedSheets) == 0L) {
    return(refs)
  }
  skipped <- vapply(skippedSheets, .canonicalizeOneId, character(1))
  refs[!(vapply(refs, .canonicalizeOneId, character(1)) %in% skipped)]
}

# The individuals workbook in the v6 layout: its `IndividualBiometrics` sheet,
# with each individual's parameter sets in the `ParameterSets` column. The other
# sheets are parameter sets and move to the model-parameters workbook.
#
# @param file The esqlabsR 5 individuals workbook.
# @param sets The parameter sets collected so far.
# @param scenarios The scenario records.
# @returns `list(sheets, sets)`.
# @keywords internal
# @noRd
.v5ConvertIndividualsWorkbook <- function(file, sets, scenarios) {
  sheetNames <- readxl::excel_sheets(file)
  biometrics <- NULL
  if ("IndividualBiometrics" %in% sheetNames) {
    biometrics <- as.data.frame(
      readExcel(file, sheet = "IndividualBiometrics"),
      check.names = FALSE
    )
  }
  paramSheetNames <- setdiff(sheetNames, "IndividualBiometrics")
  parsed <- if (length(paramSheetNames) > 0L) {
    .parseExcelParameterSheets(file, sheetNames = paramSheetNames)
  } else {
    list()
  }
  appended <- .v5AppendParameterSets(sets, parsed, file)
  if (is.null(biometrics)) {
    return(list(sheets = list(), sets = appended$sets))
  }

  setSheetNames <- names(parsed)
  skippedSheets <- setdiff(paramSheetNames, setSheetNames)
  ids <- vapply(
    seq_len(nrow(biometrics)),
    function(i) as.character(.v5CellValue(biometrics, "IndividualId", i)),
    character(1)
  )
  declared <- lapply(seq_len(nrow(biometrics)), function(i) {
    .parseCommaListToArray(.v5CellValue(biometrics, "ParameterSets", i))
  })
  .v5WarnUnparametrizedIndividuals(ids, declared, skippedSheets, file)
  .v5WarnUndefinedIndividualSheets(ids, setSheetNames, scenarios, file)
  .v5WarnIndividualParameterSetsColumn(biometrics, ids, file)
  biometrics[["Individual Parameter Sets"]] <- NULL
  biometrics <- .v5MergeOntogenyColumns(
    biometrics,
    "individual",
    "IndividualId"
  )

  sheetCanonical <- vapply(setSheetNames, .canonicalizeOneId, character(1))
  parameterSets <- vapply(
    seq_along(ids),
    function(i) {
      refs <- .v5DropSkippedSheetRefs(declared[[i]], skippedSheets)
      refs <- .v5ApplyIdRenames(refs, appended$renames)
      # A sheet named after the individual is the individual's own set.
      own <- .canonicalizeOneId(ids[[i]])
      if (!is.na(own)) {
        match <- setSheetNames[own == sheetCanonical]
        refs <- unique(c(refs, .v5ApplyIdRenames(match, appended$renames)))
      }
      .formatArrayToCommaList(refs)
    },
    character(1)
  )
  biometrics$ParameterSets <- parameterSets
  list(
    sheets = list(IndividualBiometrics = biometrics),
    sets = appended$sets
  )
}

# The applications workbook in the v6 layout: one `ApplicationProtocols` row per
# protocol, naming its parameter set, which moves to the model-parameters
# workbook.
#
# esqlabsR 5 has one sheet per protocol, so each sheet that is a parameter set
# becomes a protocol of the same name. A workbook that already has an
# `ApplicationProtocols` sheet keeps it, and its other sheets are parameter sets.
#
# @param file The esqlabsR 5 applications workbook.
# @param sets The parameter sets collected so far.
# @returns `list(sheets, sets)`.
# @keywords internal
# @noRd
.v5ConvertApplicationsWorkbook <- function(file, sets) {
  sheetNames <- readxl::excel_sheets(file)
  if ("ApplicationProtocols" %in% sheetNames) {
    protocols <- as.data.frame(
      readExcel(file, sheet = "ApplicationProtocols"),
      check.names = FALSE
    )
    paramSheetNames <- setdiff(sheetNames, "ApplicationProtocols")
    parsed <- if (length(paramSheetNames) > 0L) {
      .parseExcelParameterSheets(file, sheetNames = paramSheetNames)
    } else {
      list()
    }
    appended <- .v5AppendParameterSets(sets, parsed, file)
    if ("ParameterSets" %in% names(protocols)) {
      skippedSheets <- setdiff(paramSheetNames, names(parsed))
      protocols$ParameterSets <- vapply(
        seq_len(nrow(protocols)),
        function(i) {
          refs <- .parseCommaListToArray(protocols$ParameterSets[[i]])
          refs <- .v5DropSkippedSheetRefs(refs, skippedSheets)
          .formatArrayToCommaList(.v5ApplyIdRenames(refs, appended$renames))
        },
        character(1)
      )
    }
    return(list(
      sheets = list(ApplicationProtocols = protocols),
      sets = appended$sets
    ))
  }

  parsed <- .parseExcelParameterSheets(file, sheetNames = sheetNames)
  appended <- .v5AppendParameterSets(sets, parsed, file)
  if (length(parsed) == 0L) {
    return(list(sheets = list(), sets = appended$sets))
  }
  setNames <- .v5ApplyIdRenames(names(parsed), appended$renames)
  protocols <- data.frame(
    ApplicationId = names(parsed),
    ParameterSets = vapply(setNames, .formatArrayToCommaList, character(1)),
    stringsAsFactors = FALSE
  )
  list(
    sheets = list(ApplicationProtocols = protocols),
    sets = appended$sets
  )
}

# Report the sets an `Individual Parameter Sets` cell names besides the
# individual's own sheet.
#
# esqlabsR 5 does not read the column: it applies to an individual the sheet
# named after it, which the conversion links through the `ParameterSets` column.
# Any other set the cell names was never applied, so it is not linked either,
# and is named here, since a modeler may have expected it to be.
#
# @param biometrics The `IndividualBiometrics` sheet.
# @param ids The `IndividualId` cells.
# @param file The workbook, named in the warning.
# @keywords internal
# @noRd
.v5WarnIndividualParameterSetsColumn <- function(biometrics, ids, file) {
  column <- "Individual Parameter Sets"
  if (!(column %in% names(biometrics))) {
    return(invisible(NULL))
  }
  namedIds <- character()
  namedSets <- character()
  for (i in seq_along(ids)) {
    sets <- .parseCommaListToArray(.v5CellValue(biometrics, column, i))
    own <- .canonicalizeOneId(ids[[i]])
    other <- sets[vapply(sets, .canonicalizeOneId, character(1)) != own]
    namedIds <- c(namedIds, rep(ids[[i]], length(other)))
    namedSets <- c(namedSets, other)
  }
  if (length(namedIds) == 0L) {
    return(invisible(NULL))
  }
  .warnFormatted(
    .esqlabsR5Messages$individualParameterSetsColumn(file, namedIds, namedSets),
    "esqlabsR_convertIndividualParameterSets"
  )
}

# Report the individuals a skipped sheet leaves without their parameter set.
#
# A sheet of the individuals workbook that is not a parameter sheet is reported
# on its own (`.parseExcelParameterSheets()`), but not what it costs: an
# individual parametrized through that sheet, by its name or through its
# `ParameterSets` cell, is converted without the set, and the project still
# validates.
#
# @param ids The `IndividualId` cells.
# @param declared The sets each individual's `ParameterSets` cell names.
# @param skippedSheets The sheets of the workbook that are not parameter sheets.
# @param file The workbook, named in the warning.
# @keywords internal
# @noRd
.v5WarnUnparametrizedIndividuals <- function(
  ids,
  declared,
  skippedSheets,
  file
) {
  if (length(ids) == 0L || length(skippedSheets) == 0L) {
    return(invisible(NULL))
  }
  skipped <- vapply(skippedSheets, .canonicalizeOneId, character(1))
  lostIds <- character()
  lostSheets <- character()
  for (i in seq_along(ids)) {
    id <- .canonicalizeOneId(ids[[i]])
    if (is.na(id)) {
      next
    }
    referenced <- c(
      id,
      if (length(declared[[i]]) > 0L) {
        vapply(declared[[i]], .canonicalizeOneId, character(1))
      }
    )
    lost <- skippedSheets[skipped %in% referenced]
    lostIds <- c(lostIds, rep(ids[[i]], length(lost)))
    lostSheets <- c(lostSheets, lost)
  }
  if (length(lostIds) == 0L) {
    return(invisible(NULL))
  }
  .warnFormatted(
    .esqlabsR5Messages$unparametrizedIndividuals(file, lostIds, lostSheets),
    "esqlabsR_importUnparametrizedIndividuals"
  )
}

# Report a parameter sheet named after an individual that the
# `IndividualBiometrics` sheet does not define.
#
# Individuals come from the biometrics rows, so such an individual is not
# converted: its sheet becomes a parameter set no individual uses, and a
# scenario naming the individual names nothing. Only a sheet named after an
# individual a scenario uses is reported, so a set that only lives in this
# workbook is not mistaken for a dropped individual.
#
# @param ids The `IndividualId` cells.
# @param setSheetNames The sheets of the workbook that are parameter sets.
# @param scenarios The scenario records.
# @param file The workbook, named in the warning.
# @keywords internal
# @noRd
.v5WarnUndefinedIndividualSheets <- function(
  ids,
  setSheetNames,
  scenarios,
  file
) {
  if (length(setSheetNames) == 0L) {
    return(invisible(NULL))
  }
  defined <- vapply(ids, .canonicalizeOneId, character(1))
  wanted <- unique(vapply(
    scenarios,
    function(scenario) {
      .canonicalizeOneId(scenario$individual %||% NA_character_)
    },
    character(1)
  ))
  sheetIds <- vapply(setSheetNames, .canonicalizeOneId, character(1))
  dropped <- setSheetNames[sheetIds %in% setdiff(wanted, defined)]
  if (length(dropped) == 0L) {
    return(invisible(NULL))
  }
  .warnFormatted(
    .esqlabsR5Messages$undefinedIndividualSheets(file, dropped),
    "esqlabsR_importUndefinedIndividualSheets"
  )
}

# Scenarios ----

# The boolean columns of a Scenarios sheet. `OverwriteFormulasInSS` is newer
# than the others, so an older sheet lacks it.
.v5ScenarioBooleanColumns <- c(
  "ReadPopulationFromCSV",
  "SteadyState",
  "OverwriteFormulasInSS"
)

# The scenarios workbook in the v6 layout: its `Scenarios` and `OutputPaths`
# sheets, the scenarios with true/false booleans and with a label where the
# scenario name does not survive as an id.
#
# @param file The esqlabsR 5 scenarios workbook.
# @param call The frame an abort is attributed to.
# @returns `list(sheets, dropped)`, `dropped` naming the other sheets.
# @keywords internal
# @noRd
.v5ConvertScenariosWorkbook <- function(file, call = NULL) {
  sheetNames <- readxl::excel_sheets(file)
  sheets <- list()
  for (sheet in intersect(sheetNames, c("Scenarios", "OutputPaths"))) {
    df <- as.data.frame(readExcel(file, sheet = sheet), check.names = FALSE)
    if (identical(sheet, "Scenarios")) {
      df <- .v5ConvertScenariosSheet(df, file, call)
    }
    sheets[[sheet]] <- df
  }
  list(
    sheets = sheets,
    dropped = setdiff(sheetNames, c("Scenarios", "OutputPaths"))
  )
}

# A Scenarios sheet with its booleans as `TRUE` / `FALSE` and its `Label`
# column filled where the scenario name keeps more than its id does.
#
# @param df The sheet.
# @param file The workbook, named when a boolean cell cannot be read.
# @param call The frame an abort is attributed to.
# @keywords internal
# @noRd
.v5ConvertScenariosSheet <- function(df, file = NULL, call = NULL) {
  for (column in intersect(.v5ScenarioBooleanColumns, names(df))) {
    df[[column]] <- vapply(
      df[[column]],
      .v5ToLogical,
      logical(1),
      field = column,
      file = file,
      call = call,
      USE.NAMES = FALSE
    )
  }

  ids <- as.character(df[["Scenario_name"]])
  labelCells <- if ("Label" %in% names(df)) {
    as.character(df[["Label"]])
  } else {
    rep(NA_character_, nrow(df))
  }
  records <- lapply(seq_len(nrow(df)), function(i) {
    record <- list(id = ids[[i]])
    if (!is.na(labelCells[[i]])) {
      record$label <- labelCells[[i]]
    }
    record
  })
  labels <- vapply(
    .v5KeepScenarioNamesAsLabels(records),
    function(record) record$label %||% NA_character_,
    character(1)
  )
  if (all(is.na(labels))) {
    return(df)
  }
  df[["Label"]] <- NULL
  data.frame(
    df[1L],
    Label = labels,
    df[-1L],
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# Keep the `Scenario_name` of each scenario as its label when canonicalizing the
# id changes more than its case, so the spelling esqlabsR 5 showed is not lost:
# `Aciclovir_IV-2.5mg/kg` gets the id `aciclovir_iv-2.5mg_kg` and the label
# `Aciclovir_IV-2.5mg/kg`. A name the id only lowercases gets no label, since the
# id reads the same. Wrapping quotes are cell syntax, not part of the name, so
# they are stripped first. A label the sheet gives in its `Label` column wins.
#
# @param scenarios Records carrying `id` and, optionally, `label`.
# @returns `scenarios`, with the labels added.
# @keywords internal
# @noRd
.v5KeepScenarioNamesAsLabels <- function(scenarios) {
  lapply(scenarios, function(sc) {
    if (is.null(sc$label) && .isNonEmptyString(sc$id)) {
      name <- .stripWrappingQuotes(sc$id)
      if (!identical(.canonicalizeOneId(name), tolower(name))) {
        sc$label <- name
      }
    }
    sc
  })
}

# Read one esqlabsR 5 boolean cell as `TRUE`, `FALSE` or `NA` (blank).
#
# esqlabsR 5 sheets spell a boolean as `TRUE` / `FALSE`, `1` / `0`, `Yes` /
# `No` or `true` / `false`, in any case. A value that is none of these aborts
# naming the field, so a typo stops the conversion rather than turning into
# `FALSE`.
#
# @param x One cell.
# @param field The column, for the abort.
# @param file The workbook, for the abort, or `NULL`.
# @param call The frame the abort is attributed to.
# @keywords internal
# @noRd
.v5ToLogical <- function(x, field, file = NULL, call = NULL) {
  if (is.null(x) || length(x) == 0L || is.na(x)) {
    return(NA)
  }
  if (is.logical(x)) {
    return(x)
  }
  if (is.numeric(x)) {
    if (x == 1) {
      return(TRUE)
    }
    if (x == 0) {
      return(FALSE)
    }
  } else {
    token <- tolower(trimws(as.character(x)))
    if (token == "") {
      return(NA)
    }
    if (token %in% c("true", "t", "yes", "y", "1")) {
      return(TRUE)
    }
    if (token %in% c("false", "f", "no", "n", "0")) {
      return(FALSE)
    }
  }
  cli::cli_abort(
    .esqlabsR5Messages$notLogical(field, x, file),
    call = call
  )
}

# Plots ----

# The plots workbook in the v6 layout: its `DataCombined`, `plotConfiguration`
# and `plotGrids` sheets, the last two with the v6 column names. The other
# sheets have no v6 place: the `dataTypes`, `plotTypes` and `ObservedDataNames`
# lookup lists are dropped, and an `exportConfiguration` sheet that holds rows
# is reported.
#
# @param file The esqlabsR 5 plots workbook.
# @returns `list(sheets, dropped)`, `dropped` naming the sheets not carried over
#   that no warning names.
# @keywords internal
# @noRd
.v5ConvertPlotsWorkbook <- function(file) {
  sheetNames <- readxl::excel_sheets(file)
  read <- function(sheet) {
    as.data.frame(readExcel(file, sheet = sheet), check.names = FALSE)
  }
  converters <- list(
    DataCombined = identity,
    plotConfiguration = .v5ConvertPlotSheet,
    plotGrids = .v5ConvertPlotGridSheet
  )
  sheets <- list()
  dropped <- character()
  for (sheet in sheetNames) {
    if (sheet %in% names(converters)) {
      sheets[[sheet]] <- converters[[sheet]](read(sheet))
      next
    }
    if (identical(sheet, "exportConfiguration")) {
      exportConfiguration <- read(sheet)
      if (nrow(exportConfiguration) > 0L) {
        .v5WarnSkippedPlotExportConfiguration(exportConfiguration, file)
        next
      }
    }
    dropped <- c(dropped, sheet)
  }
  list(sheets = sheets, dropped = dropped)
}

# The esqlabsR 5 `plotConfiguration` sheet with the v6 columns: `plotID` becomes
# `plotId` and `DataCombinedName` becomes `dataCombined`, `"lin"`, the esqlabsR 5
# spelling of the linear axis scale, becomes `"linear"`, and a Yes / No cell of
# a true-or-false plot setting becomes `TRUE` / `FALSE`.
#
# @keywords internal
# @noRd
.v5ConvertPlotSheet <- function(df) {
  df <- .v5MergeIdColumns(df, c("plotId", "plotID"), "plotConfiguration")
  df <- .v5MergeColumns(df, "dataCombined", "DataCombinedName")
  for (column in intersect(c("xAxisScale", "yAxisScale"), names(df))) {
    lin <- !is.na(df[[column]]) & df[[column]] == "lin"
    df[[column]][lin] <- "linear"
  }
  logicalFields <- names(Filter(
    function(field) identical(field$type, "logical"),
    .plotFields
  ))
  for (column in intersect(logicalFields, names(df))) {
    df[[column]] <- .v5PlotLogicalColumn(df[[column]], column)
  }
  df
}

# The esqlabsR 5 `plotGrids` sheet with the v6 columns: `name` becomes
# `plotGridId` and `plotIDs` becomes `plots`.
#
# @keywords internal
# @noRd
.v5ConvertPlotGridSheet <- function(df) {
  df <- .v5MergeIdColumns(df, c("plotGridId", "name"), "plotGrids")
  .v5MergeColumns(df, "plots", "plotIDs")
}

# Merge an id column and its esqlabsR 5 name into the v6 one, which keeps its
# own value where it has one. A row where the two disagree is reported.
#
# @param df The sheet.
# @param idColumn The v6 id column, then its esqlabsR 5 name.
# @param sheet The sheet name, for the warning.
# @keywords internal
# @noRd
.v5MergeIdColumns <- function(df, idColumn, sheet) {
  .v5WarnConflictingPlotIdColumns(df, idColumn, sheet)
  .v5MergeColumns(df, idColumn[[1]], idColumn[[2]], keep = idColumn[[1]])
}

# Merge the esqlabsR 5 column `from` into the v6 column `to`. The value of one
# column fills a blank cell of the other; where both hold one, `keep` names the
# column whose value wins, `from` unless told otherwise. The merged column takes
# the place of whichever of the two comes first.
#
# @keywords internal
# @noRd
.v5MergeColumns <- function(df, to, from, keep = from) {
  if (!(from %in% names(df))) {
    return(df)
  }
  preference <- intersect(unique(c(keep, from, to)), names(df))
  values <- rep(NA_character_, nrow(df))
  for (column in preference) {
    blank <- is.na(values)
    values[blank] <- as.character(df[[column]])[blank]
  }
  merged <- which(names(df) %in% c(to, from))
  before <- seq_len(min(merged) - 1L)
  data.frame(
    df[before],
    stats::setNames(list(values), to),
    df[setdiff(seq_along(df), c(before, merged))],
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# A true-or-false plot setting as `TRUE` / `FALSE` where esqlabsR 5 spelled it
# otherwise. A cell that is no boolean spelling is kept, so validation reports
# it; the column is written as text then, since an Excel column has one type.
#
# @keywords internal
# @noRd
.v5PlotLogicalColumn <- function(values, field) {
  if (is.logical(values)) {
    return(values)
  }
  converted <- lapply(values, function(value) {
    tryCatch(.v5ToLogical(value, field), error = function(e) value)
  })
  if (all(vapply(converted, is.logical, logical(1)))) {
    return(unlist(converted))
  }
  vapply(converted, as.character, character(1))
}

# Report an esqlabsR 5 `exportConfiguration` sheet that holds rows.
#
# The sheet says where each grid's figure is written and at what size, and a v6
# project has no field for any of it: figures are saved by the caller, from the
# plot objects `createPlots()` returns.
#
# @param exportDf The sheet.
# @param plotsFile The workbook, named in the warning.
# @keywords internal
# @noRd
.v5WarnSkippedPlotExportConfiguration <- function(exportDf, plotsFile) {
  grids <- if ("plotGridName" %in% names(exportDf)) {
    unique(as.character(exportDf$plotGridName))
  } else {
    character()
  }
  .warnFormatted(
    .esqlabsR5Messages$skippedPlotExportConfiguration(
      plotsFile,
      nrow(exportDf),
      grids[!is.na(grids)]
    ),
    "esqlabsR_importSkippedPlotExportConfiguration"
  )
}

# Report the rows of a plot sheet whose two id columns both hold a value and
# disagree. A sheet has both only when columns were merged by hand: the v6 one
# (`plotId` / `plotGridId`) and the esqlabsR 5 one (`plotID` / `name`). The v6
# column wins; without the warning a grid naming the other id fails later with
# an unknown-plot error that does not say why.
#
# @param df The sheet.
# @param idColumn The v6 id column, then its esqlabsR 5 name.
# @param sheet The sheet name, for the warning.
# @keywords internal
# @noRd
.v5WarnConflictingPlotIdColumns <- function(df, idColumn, sheet) {
  if (!all(idColumn %in% names(df))) {
    return(invisible(NULL))
  }
  used <- as.character(df[[idColumn[[1]]]])
  other <- as.character(df[[idColumn[[2]]]])
  rows <- which(!is.na(used) & !is.na(other) & used != other)
  if (length(rows) == 0L) {
    return(invisible(NULL))
  }
  .warnFormatted(
    .esqlabsR5Messages$conflictingPlotIdColumns(
      sheet,
      idColumn,
      paste0(used[rows], " / ", other[rows])
    ),
    "esqlabsR_importConflictingPlotIdColumns"
  )
}

# Individuals and populations ----

# Write the esqlabsR 5 `Protein` + `Ontogeny` column pair of an individuals or
# populations sheet as the single `Protein Ontogenies` column a v6 workbook has.
#
# @param df The sheet.
# @param recordType `"individual"` or `"population"`, for the warning.
# @param idColumn The sheet's id column, for the warning.
# @returns `df` with a `Protein Ontogenies` column, where the `Protein` column
#   was when the sheet had none, and without the pair.
# @keywords internal
# @noRd
.v5MergeOntogenyColumns <- function(df, recordType, idColumn) {
  pair <- intersect(c("Protein", "Ontogeny"), names(df))
  if (length(pair) == 0L) {
    return(df)
  }
  cells <- vapply(
    seq_len(nrow(df)),
    function(i) {
      .formatOntogeniesToCell(.v5ProteinOntogenies(
        df[i, , drop = FALSE],
        recordType,
        as.character(.v5CellValue(df, idColumn, i))
      ))
    },
    character(1)
  )
  column <- "Protein Ontogenies"
  if (!(column %in% names(df))) {
    position <- match(pair[[1]], names(df))
    df <- data.frame(
      df[seq_len(position - 1L)],
      stats::setNames(list(NA_character_), column),
      df[setdiff(seq_along(df), seq_len(position - 1L))],
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  df[[column]] <- cells
  df[pair] <- NULL
  df
}

# One row's protein ontogenies, from either spelling an esqlabsR 5 sheet uses.
#
# A single `Protein Ontogenies` cell holds `Protein:Ontogeny,Protein:Ontogeny`;
# a `Protein` + `Ontogeny` pair holds two comma-separated lists, paired up
# positionally (`A,B` with `X,Y` means `A:X` and `B:Y`). The single cell wins
# when both carry a value. A pair that cannot be paired (one column filled, or a
# different number of proteins and ontogenies) warns naming the record, since
# the ontogenies would otherwise leave the project in silence.
#
# @param row One row of the sheet.
# @param recordType `"individual"` or `"population"`, for the warning.
# @param recordId The row's id, for the warning.
# @returns `Protein:Ontogeny` entries, one per ontogeny, or `NULL`.
# @keywords internal
# @noRd
.v5ProteinOntogenies <- function(row, recordType, recordId) {
  single <- .splitProteinOntogenies(row[["Protein Ontogenies"]])
  if (length(single) > 0L) {
    return(single)
  }
  proteins <- .splitProteinOntogenies(row[["Protein"]])
  ontogenies <- .splitProteinOntogenies(row[["Ontogeny"]])
  if (length(proteins) == 0L && length(ontogenies) == 0L) {
    return(NULL)
  }
  if (length(proteins) != length(ontogenies)) {
    cli::cli_warn(.esqlabsR5Messages$ontogeniesNotReadable(
      recordType,
      recordId,
      proteins,
      ontogenies
    ))
    return(NULL)
  }
  paste0(proteins, ":", ontogenies)
}

# The populations workbook in the v6 layout: its first sheet, the populations,
# with the ontogenies in one column. The other sheets have no v6 place; a
# `UserDefinedVariability` sheet that holds rows is reported.
#
# @param file The esqlabsR 5 populations workbook.
# @returns `list(sheets, dropped)`, `dropped` naming the sheets not carried over
#   that no warning names.
# @keywords internal
# @noRd
.v5ConvertPopulationsWorkbook <- function(file) {
  sheetNames <- readxl::excel_sheets(file)
  populations <- as.data.frame(readExcel(file, sheet = 1), check.names = FALSE)
  populations <- .v5MergeOntogenyColumns(
    populations,
    "population",
    "PopulationName"
  )
  dropped <- character()
  for (sheet in sheetNames[-1]) {
    if (identical(sheet, "UserDefinedVariability")) {
      variability <- readExcel(file, sheet = sheet)
      if (nrow(variability) > 0L) {
        .warnFormatted(
          .esqlabsR5Messages$userDefinedVariability(file, nrow(variability)),
          "esqlabsR_convertUserDefinedVariability"
        )
        next
      }
    }
    dropped <- c(dropped, sheet)
  }
  list(
    sheets = stats::setNames(list(populations), sheetNames[[1]]),
    dropped = dropped
  )
}

# Shared cell readers ----

# One cell of a parsed sheet, `NA` where the sheet has no such column or row.
#
# @keywords internal
# @noRd
.v5CellValue <- function(df, column, i = 1L) {
  values <- df[[column]]
  if (is.null(values) || length(values) < i) {
    return(NA)
  }
  values[[i]]
}
