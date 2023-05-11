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
