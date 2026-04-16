#' Load a project from a JSON configuration file
#'
#' @description Load a `Project` from a JSON file. This is the
#'   primary entry point for working with esqlabsR projects.
#'
#' @param path Path to the `Project.json` file. Defaults to
#'   `Project.json` in the working directory.
#'
#' @returns Object of type `Project`
#' @export
loadProject <- function(path = "Project.json") {
  Project$new(projectFilePath = path)
}

#' @rdname loadProject
#' @export
createProjectConfiguration <- function(path = "Project.json") {
  lifecycle::deprecate_soft(
    what = "createProjectConfiguration()",
    with = "loadProject()",
    when = "6.0.0"
  )
  loadProject(path)
}

#' @rdname loadProject
#' @export
createDefaultProjectConfiguration <- function(path = "Project.json") {
  lifecycle::deprecate_soft(
    what = "createDefaultProjectConfiguration()",
    with = "loadProject()",
    when = "5.3.0"
  )
  loadProject(path)
}

#' Check if a directory contains an esqlabsR project
#'
#' @description Checks if a directory already contains an esqlabsR project by
#' looking for the presence of Project.xlsx file or Configurations
#' folder.
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

  # Check for Project.xlsx file
  hasConfigFile <- any(stringr::str_detect(
    "Project.*xlsx$",
    fs::dir_ls(destination)
  ))

  # Check for Configurations folder
  hasConfigFolder <- fs::dir_exists(file.path(destination, "Configurations"))

  return(hasConfigFile || hasConfigFolder)
}

#' Initialize esqlabsR Project Folders and required Files
#'
#' @description
#'
#' Creates the default project folder structure with Excel file templates in the
#' working directory.
#'
#' @param destination A string defining the path where to initialize the
#'   project. default to current working directory.
#' @param overwrite If TRUE, overwrites existing project without asking for
#'   permission. If FALSE and a project already exists, asks user for permission
#'   to overwrite.
#' @export
initProject <- function(destination = ".", overwrite = FALSE) {
  destination <- fs::path_abs(destination)

  if (!fs::dir_exists(destination)) {
    stop(
      messages$pathNotFound(destination)
    )
  }

  type <- "example"
  source_folder <- switch(type, "example" = projectDirectory("Blank"))

  # Check if project already exists
  if (isProjectInitialized(destination)) {
    if (overwrite) {
      # Overwrite without asking
      message(messages$overwriteDestination(destination))
    } else {
      # Ask for permission to overwrite
      qs <- sample(c("Absolutely not", "Yes", "No way"))

      out <- utils::menu(
        title = "The destination folder seems to already contain an esqlabsR project. Do you want to overwrite it?",
        choices = qs
      )

      if (out == 0L || qs[[out]] != "Yes") {
        stop(messages$abortedByUser())
      }

      message(messages$overwriteDestination(destination))
    }
  }

  # Copy Blank template files (just the JSON)
  res <- file.copy(
    list.files(source_folder, full.names = TRUE),
    destination,
    recursive = TRUE,
    overwrite = TRUE
  )

  # Create empty directory structure
  dirs_to_create <- c(
    "Models/Simulations",
    "Data",
    "Configurations/PopulationsCSV",
    "Results/Figures",
    "Results/SimulationResults"
  )
  for (d in dirs_to_create) {
    dir.create(file.path(destination, d), recursive = TRUE, showWarnings = FALSE)
  }

  # Generate Excel configuration files from JSON
  jsonPath <- file.path(destination, "Project.json")
  pc <- loadProject(jsonPath)
  exportProjectToExcel(pc, outputDir = destination, silent = TRUE)

  invisible(destination)
}

#' Get the path to example Project.xlsx
#'
#' @returns A string representing the path to the example
#'   Project.xlsx file
#' @export
#' @examples
#' exampleProjectPath()
exampleProjectPath <- function() {
  file.path(projectDirectory("Example"), "Project.xlsx")
}

#' @rdname exampleProjectPath
#' @export
exampleProjectConfigurationPath <- function() {
  lifecycle::deprecate_soft(
    what = "exampleProjectConfigurationPath()",
    with = "exampleProjectPath()",
    when = "7.0.0"
  )
  exampleProjectPath()
}
