#' Create a default `ProjectConfiguration`
#'
#' @details Create a `ProjectConfiguration` based on the "ProjectConfiguration.xlsx"
#' located in the "Code" folder.
#'
#' @return Object of type `ProjectConfiguration`
#' @export
createDefaultProjectConfiguration <- function() {
  # Read data from excel
  data <- readExcel(path = file.path("ProjectConfiguration.xlsx"))
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
