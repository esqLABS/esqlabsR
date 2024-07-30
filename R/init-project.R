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
    "example" = exampleDirectory("TestProject")
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

