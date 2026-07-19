# Project lifecycle: load, save, init, examples ----

#' Load a project from a JSON configuration file
#'
#' @description Load a `Project` from a JSON file. This is the
#'   primary entry point for working with esqlabsR projects.
#'
#'   On load, the project is checked for the most common reference mistakes
#'   (for example a scenario referring to an individual or population that is
#'   not defined). Such issues are reported as warnings so that obvious
#'   configuration mistakes surface immediately, but loading still succeeds.
#'   Use [validateProject()] for a full report.
#'
#' @param path Path to the `Project.json` file. Defaults to
#'   `Project.json` in the working directory.
#'
#' @returns Object of type `Project`
#' @export
#' @family project persistence
#' @seealso [saveProject()], [reloadProject()], [snapshotProject()],
#'   [restoreProject()], [projectStatus()].
#'
#' @section Editing a loaded project:
#'   Changes you make to a loaded project — with `addScenario()`,
#'   `setIndividual()`, `removeParameterSet()`, `addOutputPath()`, and the
#'   other add/set/remove functions — live only in your R session until you
#'   save them; the files on disk stay as they are. Reading a section
#'   directly (for example `project$scenarios`) never changes the project: a
#'   definition changes only through the add/set/remove functions.
#'
#'   Write your changes to the project files with [saveProject()]. Discard
#'   unsaved changes and go back to what is saved on disk with
#'   [reloadProject()]. Save the current state of the whole project to a
#'   single file you can archive or share with [snapshotProject()], and
#'   recreate a project folder from such a file (or roll an existing one
#'   back) with [restoreProject()]. Check for unsaved changes and outdated
#'   Excel files with [projectStatus()].
#'
#' @examples
#' \dontrun{
#' project <- loadProject("Project.json")
#' results <- runScenarios(project)
#'
#' # Edits stay in memory until you save.
#' addOutputPath(project, "x", "Organism|A|Concentration in container")
#' saveProject(project)
#' }
loadProject <- function(path = "Project.json") {
  project <- Project$new(projectFilePath = path)
  .warnOnCrossReferenceErrors(project)
  project
}

#' Save the project to the disk
#'
#' @description Write your changes to the project files on disk. Changes made
#'   in your R session (e.g. with `addScenario()`, `setIndividual()`,
#'   `removeParameterSet()`) only live in memory until you call
#'   `saveProject()`.
#'
#'   What happens when you save:
#'
#'   - Only files with actual changes are re-written, so `git diff` shows
#'     exactly the definitions you edited.
#'   - If you removed something from the project (e.g. a scenario), its file
#'     in the `definitions/` folder is deleted. Files outside the
#'     `definitions/` folder are never touched.
#'   - The `Project.json` file is updated.
#'
#'   If there is nothing to save, `saveProject()` simply reports that the
#'   project is already up to date. Saving repeatedly is always safe.
#'
#'   Saving does not update the Excel files. If you also work with the Excel
#'   configuration files, refresh them with [exportProjectToExcel()]. Use
#'   [projectStatus()] to check whether project files on disk, the Excel files,
#'   and your R session are in sync.
#'
#' @param project A `Project` loaded from disk with [loadProject()] (or
#'   restored with [restoreProject()]). A project created directly with
#'   `Project$new()` has no folder on disk to save to; use [snapshotProject()]
#'   to write it to a single file, or create a project folder first with
#'   [initProject()].
#'
#' @returns Invisibly, the `project`.
#' @export
#' @family project persistence
#' @seealso [loadProject()], [reloadProject()], [snapshotProject()],
#'   [restoreProject()], [projectStatus()].
#' @examples
#' \dontrun{
#' project <- loadProject("Project.json")
#' addOutputPath(project, "PVB", "Organism|PeripheralVenousBlood|...")
#' saveProject(project) # tree now mirrors memory
#' saveProject(project) # clean save: "Project is already up to date; ..."
#' }
saveProject <- function(project) {
  validateIsOfType(project, "Project")

  if (is.null(project$projectFilePath)) {
    cli::cli_abort(messages$saveProjectNoTree())
  }

  # The dirty bit is the memory-vs-tree divergence signal. A clean save is a
  # reassuring, idempotent no-op, never an error.
  if (!project$.isModified()) {
    cli::cli_inform(messages$projectAlreadyUpToDate())
    return(invisible(project))
  }

  # Drive the full-tree reconciler: `.writeProjectTree()` writes every kind's
  # write-if-different, orphan-reconciled tree and the `containerOnly = TRUE`
  # `Project.json` in one pass, which is exactly `saveProject()`'s contract.
  .writeProjectTree(project, project$projectDirPath)

  project$.clearModified()
  invisible(project)
}

#' Discard a project's unsaved changes and re-read it from disk
#'
#' @description The undo of saving: discard every unsaved change and re-read
#'   the project from its files on disk, in place. The `Project` stays the
#'   same object, so every variable that points to it stays valid.
#'
#'   `reloadProject()` always re-reads the project's files and updates the
#'   project in place, so it also picks up changes made to the files outside
#'   the R session (for example after [restoreProject()] rolled the project
#'   back). It simply produces no announcement when there was nothing to
#'   discard: unlike a clean [saveProject()], a clean reload prints nothing.
#'
#' @param project A `Project` with a folder on disk. A project that exists
#'   only in the R session has nothing to reload from and aborts.
#'
#' @returns Invisibly, the `project`, with unsaved changes discarded.
#' @export
#' @family project persistence
#' @seealso [loadProject()], [saveProject()], [snapshotProject()],
#'   [restoreProject()], [projectStatus()].
#' @examples
#' \dontrun{
#' project <- loadProject("Project.json")
#' addScenario(project, "oops", modelFile = "model.pkml")
#' reloadProject(project) # discard the edit, back to disk
#' }
reloadProject <- function(project) {
  validateIsOfType(project, "Project")

  if (is.null(project$projectFilePath)) {
    cli::cli_abort(messages$reloadProjectNoTree())
  }

  # Always re-read from disk, even when the handle is clean: a clean handle can
  # still be stale after `restoreProject(..., overwrite = TRUE)` rolled the tree
  # back, or after an external edit to the JSON, and the blessed rollback idiom
  # relies on reload refreshing in place. "Silent when clean" is about the
  # message only: `.reload()` emits no success announcement, so a clean reload
  # stays quiet; the cross-reference warning re-fires only when there are
  # genuine cross-ref errors, which is correct regardless of the dirty bit.
  project$.reload()
  .warnOnCrossReferenceErrors(project)
  invisible(project)
}

#' Emit a `cli_warn` listing critical cross-reference errors, if any
#'
#' Runs `scenarios` first so `.validateCrossReferences` can apply its
#' skip-on-prior-errors guard — without this, a structurally invalid
#' scenarios section would still trigger spurious cross-reference warnings.
#'
#' @keywords internal
#' @noRd
.warnOnCrossReferenceErrors <- function(project) {
  results <- .runProjectValidation(
    project,
    sections = c("scenarios", "crossReferences")
  )
  r <- results$crossReferences
  if (is.null(r) || !r$has_critical_errors()) {
    return(invisible(NULL))
  }
  bullets <- vapply(r$critical_errors, function(e) e$message, character(1))
  bullets <- stats::setNames(bullets, rep("x", length(bullets)))
  cli::cli_warn(c(
    "Project has {length(bullets)} unresolved cross-reference{?s}:",
    bullets,
    "i" = "Run {.code validateProject(project)} for the full report."
  ))
  invisible(NULL)
}

#' Check if a directory contains an esqlabsR project
#'
#' @description Checks if a directory already contains an esqlabsR project by
#' looking for the presence of a `Project.json` file, a `ProjectConfiguration`
#' Excel file, or a `Configurations` folder.
#'
#' @param destination A string defining the path to check for an existing
#'   project. Defaults to current working directory.
#'
#' @returns TRUE if an esqlabsR project exists in the directory, FALSE
#'   otherwise.
#' @export
#' @examples
#' \dontrun{
#' # Check if current directory has a project
#' hasProject <- isProjectInitialized()
#'
#' # Check if specific directory has a project
#' hasProject <- isProjectInitialized("path/to/project")
#' }
isProjectInitialized <- function(destination = ".") {
  destination <- fs::path_abs(destination)

  if (!fs::dir_exists(destination)) {
    return(FALSE)
  }

  # Check for Project.json file
  hasJsonFile <- file.exists(file.path(destination, "Project.json"))

  # Check for a *Project*.xlsx file. Match on the basename: fs::dir_ls()
  # globs the full path, so a destination directory whose own path contains
  # "Project" would otherwise match any .xlsx inside it.
  xlsxFiles <- fs::path_file(fs::dir_ls(
    destination,
    glob = "*.xlsx",
    fail = FALSE
  ))
  hasConfigFile <- any(grepl("Project", xlsxFiles, fixed = TRUE))

  # Check for Configurations folder
  hasConfigFolder <- fs::dir_exists(file.path(destination, "Configurations"))

  return(hasJsonFile || hasConfigFile || hasConfigFolder)
}

#' Initialize esqlabsR Project Folders and required Files
#'
#' @description
#'
#' Creates a new JSON-based esqlabsR project in `destination`: a
#' `Project.json` file plus a `definitions/` folder holding one file per
#' definition, alongside the working folders (`Models/`, `Data/`,
#' `Populations/`, `Results/`). By default it also writes the optional Excel
#' configuration files from the JSON; set `createExcel = FALSE` for a
#' JSON-only project.
#'
#' @param destination A string defining the path where to initialize the
#'   project. default to current working directory.
#' @param type Type of project to create: `"minimal"` (default) creates an empty
#'   project with just the directory structure, `"example"` creates a project
#'   with example data, models, and configurations.
#' @param createExcel If `TRUE` (default), generates Excel configuration files
#'   from the JSON. Set to `FALSE` for a JSON-only workflow.
#' @param overwrite If TRUE, overwrites existing project without asking for
#'   permission. If FALSE and a project already exists, asks user for permission
#'   to overwrite.
#' @returns Invisibly returns `destination`, the path the project was
#'   initialized in.
#' @export
#' @family project persistence
initProject <- function(
  destination = ".",
  type = c("minimal", "example"),
  createExcel = TRUE,
  overwrite = FALSE
) {
  destination <- fs::path_abs(destination)
  type <- match.arg(type)

  if (!fs::dir_exists(destination)) {
    cli::cli_abort(
      messages$pathNotFound(destination)
    )
  }

  source_folder <- switch(
    type,
    "minimal" = .projectDirectory("Blank"),
    "example" = .projectDirectory("Example")
  )

  # Check if project already exists
  if (isProjectInitialized(destination)) {
    if (overwrite) {
      # Overwrite without asking
      msg <- messages$overwriteDestination(destination)
      cli::cli_inform("{msg}")
    } else {
      if (!.isInteractive()) {
        cli::cli_abort(messages$cannotPromptNonInteractive())
      }
      if (!.confirmOverwrite()) {
        cli::cli_abort(messages$abortedByUser())
      }
      msg <- messages$overwriteDestination(destination)
      cli::cli_inform("{msg}")
    }
    # Overwrite means REPLACE, not merge. Copying the template with
    # `overwrite = TRUE` refreshes the files it ships, but a definition file the
    # old project's `definitions/<kind>/` tree carried and the template does
    # not would survive and re-load as a stale definition. Remove the known
    # project artifacts (the definitions tree and `Project.json`) first, scoped
    # to just those paths so unrelated user files in the destination are left
    # intact.
    .clearProjectArtifacts(destination)
  }

  # Copy template files (just the JSON for minimal, full fixture for example)
  sourceFiles <- list.files(source_folder, full.names = TRUE)
  copied <- file.copy(
    sourceFiles,
    destination,
    recursive = TRUE,
    overwrite = TRUE
  )
  if (!all(copied)) {
    cli::cli_abort(messages$failedToCopyTemplate(sourceFiles[!copied]))
  }

  # Create empty directory structure
  dirs_to_create <- c(
    "Models/Simulations",
    "Data",
    "Populations",
    "Results/Figures",
    "Results/SimulationResults",
    "definitions"
  )
  for (d in dirs_to_create) {
    dir.create(
      file.path(destination, d),
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  if (createExcel) {
    jsonPath <- file.path(destination, "Project.json")
    project <- loadProject(jsonPath)
    exportProjectToExcel(project, outputDir = destination, silent = TRUE)
  }

  invisible(destination)
}

# Remove the known esqlabsR project artifacts from `destination` before an
# overwrite, so `initProject(overwrite = TRUE)` REPLACES rather than merges. It
# removes only the scaffold the initializer owns:
#   - the definitions tree (`<destination>/<definitionsFolder>/`), the sole
#     source of the stale-definition leak, because a per-definition file the old tree
#     carried and the template does not would otherwise survive the copy and
#     re-load as a stale definition. The existing project's `definitionsFolder`
#     is read from its `Project.json` (default `"definitions"`) so a custom
#     tree location is cleared too;
#   - the `Project.json` container.
# Everything else in `destination` (working folders, and any unrelated user
# file) is left untouched. `unlink()`/`file.remove()` return values are checked
# so a failed removal aborts loudly rather than leaving a half-cleared project.
#
# @keywords internal
# @noRd
.clearProjectArtifacts <- function(destination) {
  jsonPath <- file.path(destination, "Project.json")

  # The definitions folder name is configurable; read it from the existing
  # container so a non-default tree is cleared. A missing or unreadable
  # container falls back to the default folder name.
  definitionsFolder <- "definitions"
  if (file.exists(jsonPath)) {
    existing <- tryCatch(
      jsonlite::fromJSON(jsonPath, simplifyVector = FALSE),
      error = function(e) NULL
    )
    definitionsFolder <- existing$definitionsFolder %||% "definitions"
  }

  definitionsDir <- file.path(destination, definitionsFolder)
  if (dir.exists(definitionsDir)) {
    failed <- unlink(definitionsDir, recursive = TRUE, force = TRUE)
    if (failed != 0L || dir.exists(definitionsDir)) {
      cli::cli_abort(messages$failedToClearProjectArtifacts(definitionsDir))
    }
  }

  if (file.exists(jsonPath)) {
    if (!file.remove(jsonPath)) {
      cli::cli_abort(messages$failedToClearProjectArtifacts(jsonPath))
    }
  }

  invisible(NULL)
}

# Thin wrapper around base::interactive(), as a package-local binding so
# tests can mock the interactive/non-interactive branch of initProject().
#
# @keywords internal
# @noRd
.isInteractive <- function() {
  interactive()
}

# Ask the interactive user whether to overwrite an existing project.
# Returns TRUE only if they pick "Yes". Wrapped in a package-local helper
# so tests can mock it without touching the `utils` namespace.
#
# @keywords internal
# @noRd
.confirmOverwrite <- function() {
  qs <- sample(c("Absolutely not", "Yes", "No way"))
  out <- utils::menu(
    title = "The destination folder seems to already contain an esqlabsR project. Do you want to overwrite it?",
    choices = qs
  )
  out != 0L && qs[[out]] == "Yes"
}

#' Get the path to the example Project.json
#'
#' @returns A string representing the path to the example
#'   `Project.json` file shipped with the package.
#' @export
#' @family project persistence
#' @examples
#' exampleProjectPath()
exampleProjectPath <- function() {
  file.path(.projectDirectory("Example"), "Project.json")
}

#' Get path to esqlabsR project templates
#'
#' esqlabsR comes bundled with project templates in its `inst/extdata`
#' directory. This function makes them easy to access.
#'
#' @param name Name of project directory. If `NULL`, the available names will be
#'   listed.
#' @keywords internal
#' @noRd
.projectDirectory <- function(name = NULL) {
  directory <- system.file("extdata", "projects", package = "esqlabsR")
  if (!is.null(name)) {
    directory <- file.path(directory, name)
  }
  directory
}
