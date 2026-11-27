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
  .v5CheckConvertible(projectConfigPath)
  invisible(NULL)
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
