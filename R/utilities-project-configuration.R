#' Load a project from a JSON configuration file
#'
#' @description Load a `ProjectConfiguration` from a JSON file. This is the
#'   primary entry point for working with esqlabsR projects.
#'
#' @param path Path to the `ProjectConfiguration.json` file. Defaults to
#'   `ProjectConfiguration.json` in the working directory.
#'
#' @returns Object of type `ProjectConfiguration`
#' @export
loadProject <- function(path = "ProjectConfiguration.json") {
  ProjectConfiguration$new(projectConfigurationFilePath = path)
}

#' @rdname loadProject
#' @export
createProjectConfiguration <- function(path = "ProjectConfiguration.json") {
  lifecycle::deprecate_soft(
    what = "createProjectConfiguration()",
    with = "loadProject()",
    when = "6.0.0"
  )
  loadProject(path)
}

#' @rdname loadProject
#' @export
createDefaultProjectConfiguration <- function(path = "ProjectConfiguration.json") {
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
#' looking for the presence of ProjectConfiguration.xlsx file or Configurations
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

  # Check for ProjectConfiguration.xlsx file
  hasConfigFile <- any(stringr::str_detect(
    "ProjectConfiguration.*xlsx$",
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
  source_folder <- switch(type, "example" = exampleDirectory("TestProject"))

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

  res <- file.copy(
    list.files(source_folder, full.names = TRUE),
    destination,
    recursive = TRUE,
    overwrite = TRUE
  )
}

#' Get the path to example ProjectConfiguration.xlsx
#'
#' @returns A string representing the path to the example
#'   ProjectConfiguration.xlsx file
#' @export
#' @examples
#' exampleProjectConfigurationPath()
exampleProjectConfigurationPath <- function() {
  # Returns the path to the example project configuration file in TestProject
  file.path(exampleDirectory("TestProject"), "ProjectConfiguration.xlsx")
}
