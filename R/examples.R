#' Get path to esqlabsR examples
#'
#' esqlabsR comes bundled with some example Project in its `inst/extdata`
#' directory. This function make them easy to access.
#'
#' @param name Name of example project. If `NULL`, the example names will be listed.
#' @keywords internal
exampleDirectory <- function(name = NULL) {
  if (is.null(name)) {
    dir(system.file("extdata", "examples", package = "esqlabsR"))
  } else {
    system.file("extdata", "examples", name, package = "esqlabsR", mustWork = TRUE)
  }
}

#' Get the path to example ProjectConfiguration.xlsx
#'
#' @return a string representing the path to the ProjectConfiguration.xlsx file
#' used as example
#' @export
#'
#' @examples
#' exampleProjectConfigurationFile()
exampleProjectConfigurationFile <- function() {
  # for now it targets TestProject as it is both an example and a test project
  file.path(exampleDirectory("TestProject"), "ProjectConfiguration.xlsx")
}

#' Get an example ProjectConfiguration object
#'
#' @return a projectConfiguration object with the content of the example.
#' @export
#'
#' @examples
#' exampleProjectConfiguration()
exampleProjectConfiguration <- function(){
  createProjectConfiguration(exampleProjectConfigurationFile())
}


getExampleProject <- function() {
  .exampleProject <- NULL

  function() {
    if (is.null(.exampleProject)) {
      .exampleProject <- Project$new(projectConfiguration = exampleProjectConfiguration())
    }

    return(.exampleProject)
  }
}


#' Get the example Project object
#'
#' @return a Project object with the content of the example.
#' @export
#' @examples
#' project <- exampleProject()
exampleProject <- getExampleProject()
