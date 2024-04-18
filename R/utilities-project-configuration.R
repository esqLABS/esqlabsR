#' Create a default `ProjectConfiguration`
#'
#' @inheritParams readExcel
#'
#' @details Create a `ProjectConfiguration` based on the `"ProjectConfiguration.xlsx"`
#' located in the "Code" folder.
#'
#' @return Object of type `ProjectConfiguration`
#' @export
createDefaultProjectConfiguration <- function(path = file.path("ProjectConfiguration.xlsx")) {
  lifecycle::deprecate_soft(what = "createDefaultProjectConfiguration()",
                            with = "createProjectConfiguration()",
                            when = "5.1.4")
  return(createProjectConfiguration(path))
}


#' #' Create a `ProjectConfiguration`
#'
#' @param path path to the `ProjectConfiguration.xlsx` file. default to the `ProjectConfiguration.xlsx` file located in the working directory.
#'
#' @details Create a `ProjectConfiguration` based on the `"ProjectConfiguration.xlsx"`
#'
#' @return Object of type `ProjectConfiguration`
#' @export
#'
#' @examples
createProjectConfiguration <- function(path = file.path("ProjectConfiguration.xlsx")) {
  projectConfiguration <- ProjectConfiguration$new(projectConfigurationFilePath = path)
  return(projectConfiguration)
}



#' Initialize esqlabsR Project Folder
#'
#' @description
#'
#' Creates the default project folder structure with excels file templates in
#' the working directory.
#'
#' @param destination A string defining the path where to initialize the project.
#' default to current working directory.
#' @inheritParams fs::dir_copy
#' @export
init_project <- function(destination = ".", overwrite = FALSE) {
  destination <- fs::path_abs(destination)

  type <- "example"

  source_folder <- switch(type,
    "example" = example_directory("TestProject")
  )

  for (dir in fs::dir_ls(source_folder, type = "directory")) {
    fs::dir_copy(dir,
      new_path = destination,
      overwrite = overwrite
    )
  }

  for (file in fs::dir_ls(source_folder, type = "file")) {
    fs::file_copy(file,
      new_path = destination,
      overwrite = overwrite
    )
  }
}



#' Get the path to example ProjectConfiguration.xlsx
#'
#' @return a string representing the path to the ProjectConfiguration.xlsx file
#' used as example
#' @export
#'
#' @examples
#' example_ProjectConfiguration()
example_ProjectConfiguration <- function() {
  # for now it targets TestProject as it is both an example and a test project
  file.path(example_directory("TestProject"), "projectConfiguration.xlsx")
}
