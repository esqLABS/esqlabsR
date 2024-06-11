#' @title Individual
#' @description A Class representing an individual through its biological characteristics and specific simulation parameters.
Individual <- R6::R6Class(
  classname = "Individual",
  public = list(
    #' @description Creates a new instance of Configuration
    #' @param project A Project in which the individual is defined.
    #' @param individualCharacteristicsData A data frame containing the individual characteristics.
    #' @param individualParameters A list of simulation parameters for the individual.
    initialize = function(project,
                          individualCharacteristicsData,
                          individualParameters) {
      private$.project <- project
      private$.characteristics <- list(
        id = individualCharacteristicsData$IndividualId,
        specy = individualCharacteristicsData$Species,
        population = individualCharacteristicsData$Population,
        gender = individualCharacteristicsData$Gender,
        weight = individualCharacteristicsData$`Weight [kg]`,
        height = individualCharacteristicsData$`Height [cm]`,
        age = individualCharacteristicsData$`Age [year(s)]`,
        protein = individualCharacteristicsData$Protein,
        ontogeny = individualCharacteristicsData$Ontogeny
      )
      private$.parameters <- individualParameters
    },
    #' @description Prints the individual characteristics and parameters.
    print = function() {
      cli_li("Individual ID: {self$characteristics$id}")
      individual <- cli_ul()
      cli_li("Characteristics:")
      characteristics <- cli_ul()
      cli_li("Specy: {self$characteristics$specy}")
      cli_li("Population: {self$characteristics$population}")
      cli_li("Gender: {self$characteristics$gender}")
      cli_li("Weight: {self$characteristics$weight}")
      cli_li("Height: {self$characteristics$height}")
      cli_li("Age: {self$characteristics$age}")
      cli_li("Protein: {self$characteristics$protein}")
      cli_li("Ontogeny: {self$characteristics$ontogeny}")
      cli_end(characteristics)
      cli_li("Parameters:")
      parameters <- cli_ul()
      purrr::imap(self$parameters, ~ cli_li(.y))
      cli_end(parameters)
      cli_end(individual)
    },
    #' @description Converts the individual object to a data frame.
    toDataFrame = function() {
      return(
        list(
          characteristics =
            tibble::tibble(
              IndividualId = self$characteristics$id,
              Species = self$characteristics$specy,
              Population = self$characteristics$population,
              Gender = self$characteristics$gender,
              `Weight [kg]` = self$characteristics$weight,
              `Height [cm]` = self$characteristics$height,
              `Age [year(s)]` = self$characteristics$age,
              Protein = self$characteristics$protein,
              Ontogeny = self$characteristics$ontogeny
            ),
          parameters = purrr::map_dfr(private$.parameters, ~ .x$toDataFrame())
        )
      )
    }
  ),
  private = list(
    .project = NULL,
    .individualData = NULL,
    .characteristics = NULL,
    .parameters = NULL,
    .individualParameters = NULL,
    .individualObject = NULL,
    .ontogenies = NULL,
    .getOntogenies = function() {
      if (is.null(private$.ontogeny)) {
        private$.ontogenies <- createOntogenies(data.frame(
          protein = self$characteristics$protein,
          ontogeny = self$characteristics$ontogeny
        ))
      }
      return(private$.ontogenies)
    }
  ),
  active = list(
    #' @field characteristics Return the characteristics of the individual.
    characteristics = function(value) {
      if (!missing(value)) {
        private$.characteristics <- modifyList(private$.characteristics, value)
        private$.individualObject <- NULL
      }
      return(private$.characteristics)
    },
    #' @field parameters Return the individual parameters to apply to the simulation.
    parameters = function(value) {
      if (!missing(value)) {
        private$.parameters <- value
        private$.individualObject <- NULL
      }
      return(private$.parameters)
    },
    #' @field individualObject Return the individual object as created by `ospsuite::createIndividual`.
    individualObject = function() {
      if (is.null(private$.individualObject)) {
        individualCharacteristics <-
          ospsuite::createIndividualCharacteristics(
            species = self$characteristics$specy,
            population = self$characteristics$population,
            gender = self$characteristics$gender,
            weight = self$characteristics$weight,
            height = self$characteristics$height,
            age = self$characteristics$age,
            moleculeOntogenies = private$.getOntogenies()
          )
        individualCharacteristics <- ospsuite::createIndividual(individualCharacteristics)
        individualParameters <- flattenParameterObjects(purrr::map(self$parameters, ~ .x$parameterObject))

        private$.individualObject <- list(
          characteristics = individualCharacteristics,
          parameters = individualParameters
        )
      }
      return(private$.individualObject)
    }
  )
)






# Legacy code -------------------------------------------------------------

#' Create a parameter set describing an individual and write it to the Excel file
#'
#' @param individualCharacteristics An `IndividualCharacteristics` object
#'   describing the individual. See `createIndividualCharacterstics` for more
#'   information.
#' @param outputXLSPath Path to the Excel file the parameter set will be written to
#'
#' @seealso createIndividualCharacteristics crateIndividual
#'
#' @examples
#' \dontrun{
#' simulation <- loadSimulation(pathToPKML)
#' humanIndividualCharacteristics <- createIndividualCharacteristics(
#'   species = Species$Human, population = HumanPopulation$European_ICRP_2002,
#'   gender = Gender$Male, weight = 70
#' )
#' writeIndividualToXLS(humanIndividualCharacteristics, pathToExcelFile)
#' }
#'
#' @export
writeIndividualToXLS <- function(individualCharacteristics, outputXLSPath) {
  validateIsString(outputXLSPath)

  individual <- createIndividual(individualCharacteristics)

  columnNames <- c("Container Path", "Parameter Name", "Value", "Units")

  containerPaths <- vector("character", length(individual$distributedParameters$paths))
  paramNames <- vector("character", length(individual$distributedParameters$paths))
  values <- vector("numeric", length(individual$distributedParameters$paths))
  units <- vector("character", length(individual$distributedParameters$paths))

  for (i in seq_along(individual$distributedParameters$paths)) {
    fullPathParts <- strsplit(individual$distributedParameters$paths[[i]], split = "|", fixed = TRUE)[[1]]
    containerPath <- paste(fullPathParts[seq_along(fullPathParts) - 1], collapse = "|")
    paramName <- fullPathParts[[length(fullPathParts)]]

    containerPaths[i] <- containerPath
    paramNames[i] <- paramName
    values[i] <- individual$distributedParameters$values[[i]]
    units[i] <- individual$distributedParameters$units[[i]]
  }

  output <- data.frame(
    unlist(containerPaths, use.names = FALSE),
    unlist(paramNames, use.names = FALSE),
    unlist(as.numeric(values), use.names = FALSE),
    unlist(units, use.names = FALSE)
  )
  colnames(output) <- columnNames

  writeExcel(data = output, path = outputXLSPath)
}
