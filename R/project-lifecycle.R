# Project lifecycle: load, save, init, examples ----

#' Load a project from a JSON configuration file
#'
#' @description Load a `Project` from a JSON file. This is the
#'   primary entry point for working with esqlabsR projects.
#'
#'   On load the project is checked for the most common cross-reference
#'   problems (e.g. a scenario referring to an individual or population
#'   that is not defined). Any such issues are reported via [cli::cli_warn()]
#'   so that obvious configuration mistakes surface immediately, but loading
#'   still succeeds. Use [validateProject()] for a full report.
#'
#' @param path Path to the `Project.json` file. Defaults to
#'   `Project.json` in the working directory.
#'
#' @returns Object of type `Project`
#' @export
#' @family project persistence
#'
#' @section Editing a loaded project is write-through:
#'   A loaded project is bound to its directory on disk, and every
#'   authoring edit is write-through: a single `addOutputPath()`,
#'   `addScenario()`, `setIndividual()`, or `removeParameterSet()` writes (or
#'   deletes) the affected entity's file immediately. The `project$<section>`
#'   accessors are read-only, so a definition only ever changes through an
#'   authoring function. There is no separate save step, and there is no undo:
#'   the edit is on disk the moment the call returns.
#'
#'   To experiment without touching the on-disk project, work on a detached
#'   copy. `project$clone()` returns an in-memory copy whose edits stay in
#'   memory (they do not write to the source's `definitions/` tree) until it
#'   is bound to a directory of its own. To capture a shareable freeze-frame
#'   of the current state, use [saveSnapshot()], and reload it elsewhere
#'   with [loadSnapshot()].
#'
#' @examples
#' \dontrun{
#' project <- loadProject("Project.json")
#' results <- runScenarios(project)
#'
#' # Edits are write-through; clone first for scratch work.
#' scratch <- project$clone()
#' addOutputPath(scratch, "x", "Organism|A|Concentration in container")
#' }
loadProject <- function(path = "Project.json") {
  project <- Project$new(projectFilePath = path)
  .warnOnCrossReferenceErrors(project)
  project
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

#' @rdname loadProject
#' @export
createProjectConfiguration <- function(path = "Project.json") {
  lifecycle::deprecate_warn(
    when = "6.0.0",
    what = "createProjectConfiguration()",
    with = "loadProject()"
  )
  loadProject(path)
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
#' Scaffolds a JSON-first esqlabsR project in `destination`: a `Project.json`
#' container plus a `definitions/` tree of authored definitions, alongside the
#' working folders (`Models/`, `Data/`, `Populations/`, `Results/`). By default
#' it also writes optional Excel side-cars from the JSON; set
#' `createExcel = FALSE` for a JSON-only project.
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

#' @rdname exampleProjectPath
#' @export
exampleProjectConfigurationPath <- function() {
  lifecycle::deprecate_soft(
    what = "exampleProjectConfigurationPath()",
    with = "exampleProjectPath()",
    when = "6.0.0"
  )
  exampleProjectPath()
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
