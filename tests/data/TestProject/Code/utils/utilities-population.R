#' Title
#'
#' @param population Object of type \code{Population}
#'
#' @return
#' @export
#'
#' @examples
extendPopulationByGender <- function(population) {
  # Path to the parameter in the simulation that defines the gender
  genderPath <- "Organism|Gender"
  genderValues <- lapply(population$getCovariateValues("Gender"), function(x) {
    GenderInt[[x]]
  })
  genderValues <- unlist(genderValues)

  population$setParameterValues(parameterOrPath = genderPath, values = genderValues)
}
