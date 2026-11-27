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
  props <- .v5PropertyLookup(properties)

  configsFolder <- NULL
  if (!.isBlankCell(props("configurationsFolder"))) {
    configsFolder <- .resolveProjectPath(
      props("configurationsFolder"),
      projectDir,
      "configurationsFolder"
    )
  }
  sections <- .v5ReadableWorkbooks(props, configsFolder)

  outputs <- list()
  for (section in sections) {
    outputs[[length(outputs) + 1L]] <- list(
      path = section$path,
      copyOf = section$path
    )
  }

  v6Workbook <- file.path(projectDir, "Project.xlsx")
  outputs <- c(
    list(list(
      path = v6Workbook,
      sheets = list(Project = .v5PropertySheet(properties))
    )),
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
    v6Workbook = v6Workbook
  )
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
.v5ReadableWorkbooks <- function(props, configsFolder) {
  if (is.null(configsFolder)) {
    return(list())
  }
  sections <- list()
  for (property in names(.v5SectionWorkbooks)) {
    name <- props(property) %||% .v5SectionWorkbooks[[property]]
    path <- .resolveProjectPath(name, configsFolder, property)
    path <- normalizePath(path, mustWork = FALSE)
    if (!file.exists(path)) {
      next
    }
    sections[[property]] <- list(property = property, name = name, path = path)
  }
  sections
}

# The v6 property sheet: the esqlabsR 5 rows, under a `schemaVersion` row.
#
# @keywords internal
# @noRd
.v5PropertySheet <- function(properties) {
  description <- if ("Description" %in% names(properties)) {
    as.character(properties$Description)
  } else {
    rep(NA_character_, nrow(properties))
  }
  data.frame(
    Property = c("schemaVersion", as.character(properties$Property)),
    Value = c("2.0", as.character(properties$Value)),
    Description = c("Project structure schema version", description),
    stringsAsFactors = FALSE
  )
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
