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
#' @param type A string defining the type of initialization. Currently, only `"example"`
#' is supported and will create an example project.
#' @export
init_project <- function(type = "example") {

  rlang::arg_match(type, c("example"))

  source_folder <- switch(type,
                          "example" = example_directory("TestProject"))

  for (dir in fs::dir_ls(source_folder, type = "directory")) {
    fs::dir_copy(dir,
                 new_path = getwd(),
                 overwrite = FALSE)
  }

  for (file in fs::dir_ls(source_folder, type = "file")) {
    fs::file_copy(file,
                  new_path = getwd(),
                  overwrite = FALSE)
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
example_ProjectConfiguration <- function(){
  # for now it targets TestProject as it is both an example and a test project
  file.path(example_directory("TestProject"), "projectConfiguration.xlsx")
}

#' Get the path to tests' ProjectConfiguration.xlsx
#'
#' @return a string representing the path to the ProjectConfiguration.xlsx file
#' used as test.
#' @keywords internal
test_ProjectConfiguration <- function(){
  # for now it targets TestProject as it is both an example and a test project
  file.path(example_directory("TestProject"), "projectConfiguration.xlsx")
}

