#' Create a default `ProjectConfiguration`
#'
#' @description  Create a `ProjectConfiguration` based on the `"ProjectConfiguration.xlsx"`
#'
#' @param path path to the `ProjectConfiguration.xlsx` file. Defaults to the `ProjectConfiguration.xlsx`
#'   file in the working directory.
#'
#' @returns Object of type `ProjectConfiguration`
#' @export
createDefaultProjectConfiguration <- function(path = file.path("ProjectConfiguration.xlsx")) {
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
createProjectConfiguration <- function(path = file.path("ProjectConfiguration.xlsx")) {
  projectConfiguration <- ProjectConfiguration$new(projectConfigurationFilePath = path)
  return(projectConfiguration)
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
#' @inheritParams fs::dir_copy
#' @export
initProject <- function(destination = ".", overwrite = FALSE) {
  if (overwrite) {
    qs <- sample(c("Absolutely not", "Yes", "No way"))

    out <- utils::menu(
      title = "This function will overwrite the existing project. Are you sure you want to continue ?",
      choices = qs
    )

    if (out == 0L || qs[[out]] != "Yes") {
      cli::cli_abort("The function was aborted by the user.")
    }
  }

  destination <- fs::path_abs(destination)

  if (!fs::dir_exists(destination)) {
    cli::cli_abort("The specified destination folder does not exist. ({destination}) ")
  }

  type <- "example"

  source_folder <- switch(type,
    "example" = exampleDirectory("TestProject")
  )

  if (!overwrite && (any(stringr::str_detect("ProjectConfiguration.*xlsx$", fs::dir_ls(destination))) || fs::dir_exists(file.path(destination, "Configurations")))) {
    cli::cli_abort("The destination folder seems to already contain an esqlabsR project and would be overwriten by this function.")
    cli::cli_inform("If you want to overwrite the existing project, use the argument `overwrite = TRUE`")
  }

  res <- file.copy(list.files(source_folder, full.names = TRUE),
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
