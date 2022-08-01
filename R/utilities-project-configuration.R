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
  # Read data from excel
  data <- readExcel(path = path)
  # Create an empty project configuration object and population with data from
  # excel
  projectConfiguration <- ProjectConfiguration$new()

  for (idx in seq_along(data$Property)) {
    property <- data$Property[[idx]]
    value <- data$Value[[idx]]
    projectConfiguration[[property]] <- value
  }

  return(projectConfiguration)
}
