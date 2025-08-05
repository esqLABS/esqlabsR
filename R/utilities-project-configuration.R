#' Create a default `ProjectConfiguration`
#'
#' @description  Create a `ProjectConfiguration` based on the `"ProjectConfiguration.xlsx"`
#'
#' @param path path to the `ProjectConfiguration.xlsx` file. Defaults to the `ProjectConfiguration.xlsx`
#'   file in the working directory.
#'
#' @returns Object of type `ProjectConfiguration`
#' @export
createDefaultProjectConfiguration <- function(
    path = file.path("ProjectConfiguration.xlsx")) {
  lifecycle::deprecate_soft(
    what = "createDefaultProjectConfiguration()",
    with = "createProjectConfiguration()",
    when = "5.3.0"
  )
  return(createProjectConfiguration(path))
}

#' Create a `ProjectConfiguration`
#'
#' @description  Create a `ProjectConfiguration` based on the `"ProjectConfiguration.xlsx"`
#'
#' @param path path to the `ProjectConfiguration.xlsx` file. default to the `ProjectConfiguration.xlsx`
#'  file located in the working directory.
#'
#' @returns Object of type `ProjectConfiguration`
#' @export
createProjectConfiguration <- function(
    path = file.path("ProjectConfiguration.xlsx")) {
  projectConfiguration <- ProjectConfiguration$new(
    projectConfigurationFilePath = path
  )
  return(projectConfiguration)
}

#' Check if a directory contains an esqlabsR project
#'
#' @description
#' Checks if a directory already contains an esqlabsR project by looking for
#' the presence of ProjectConfiguration.xlsx file or Configurations folder.
#'
#' @param destination A string defining the path to check for an existing project.
#' Defaults to current working directory.
#'
#' @returns TRUE if an esqlabsR project exists in the directory, FALSE otherwise.
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
#' Creates the default project folder structure with Excel file templates in
#' the working directory.
#'
#' @param destination A string defining the path where to initialize the project.
#' default to current working directory.
#' @param overwrite If TRUE, overwrites existing project without asking for permission.
#' If FALSE and a project already exists, asks user for permission to overwrite.
#' @export
initProject <- function(destination = ".", overwrite = FALSE) {
  destination <- fs::path_abs(destination)

  if (!fs::dir_exists(destination)) {
    cli::cli_abort(
      "The specified destination folder does not exist. ({destination}) "
    )
  }

  type <- "example"
  source_folder <- switch(type,
    "example" = exampleDirectory("TestProject")
  )

  # Check if project already exists
  if (isProjectInitialized(destination)) {
    if (overwrite) {
      # Overwrite without asking
      cli::cli_inform("Overwriting existing esqlabsR project in {destination}")
    } else {
      # Ask for permission to overwrite
      qs <- sample(c("Absolutely not", "Yes", "No way"))

      out <- utils::menu(
        title = "The destination folder seems to already contain an esqlabsR project. Do you want to overwrite it?",
        choices = qs
      )

      if (out == 0L || qs[[out]] != "Yes") {
        cli::cli_abort("The function was aborted by the user.")
      }

      cli::cli_inform("Overwriting existing esqlabsR project in {destination}")
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
#' @returns A string representing the path to the example ProjectConfiguration.xlsx file
#' @export
#' @examples
#' exampleProjectConfigurationPath()
exampleProjectConfigurationPath <- function() {
  # Returns the path to the example project configuration file in TestProject
  file.path(exampleDirectory("TestProject"), "ProjectConfiguration.xlsx")
}
