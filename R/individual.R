Individual <- R6::R6Class(
  classname = "Individual",
  public = list(
    initialize = function(project,
                          individualCharacteristicsData,
                          individualParameters) {
      private$.project <- project
      private$.characteristics <- individualCharacteristicsDataFrameToList(individualCharacteristicsData)
      private$.parameters <- individualParameters
    },
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
    toDataFrame = function() {
      return(
        list(
          characteristics = individualCharacteristicsListToDataFrame(private$.characteristics),
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
    characteristics = function(value) {
      if (!missing(value)) {
        private$.characteristics <- modifyList(private$.characteristics, value)
        private$.individualObject <- NULL
      }
      return(private$.characteristics)
    },
    parameters = function(value) {
      if (!missing(value)) {
        private$.parameters <- value
        private$.individualObject <- NULL
      }
      return(private$.parameters)
    },
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
        individualParameters <- flattenParameterObjects(purrr::map(self$parameters, ~.x$parameterObject))

        private$.individualObject <- list(characteristics = individualCharacteristics,
                                          parameters = individualParameters)
      }
      return(private$.individualObject)
    }
  )
)

individualCharacteristicsListToDataFrame <- function(list) {
  return(
    tibble::tibble(
      IndividualId = list$id,
      Species = list$specy,
      Population = list$population,
      Gender = list$gender,
      `Weight [kg]` = list$weight,
      `Height [cm]` = list$height,
      `Age [year(s)]` = list$age,
      Protein = list$protein,
      Ontogeny = list$ontogeny
    )
  )
}

individualCharacteristicsDataFrameToList <- function(dataframe) {
  return(
    list(
      id = dataframe$IndividualId,
      specy = dataframe$Species,
      population = dataframe$Population,
      gender = dataframe$Gender,
      weight = dataframe$`Weight [kg]`,
      height = dataframe$`Height [cm]`,
      age = dataframe$`Age [year(s)]`,
      protein = dataframe$Protein,
      ontogeny = dataframe$Ontogeny
    )
  )
}

checkIndividualsFileStructure <- function(filePath, data) {
  columnNames <- c(
    "IndividualId",
    "Species",
    "Population",
    "Gender",
    "Weight [kg]",
    "Height [cm]",
    "Age [year(s)]",
    "Protein",
    "Ontogeny"
  )

  if (!all(columnNames %in% names(data))) {
    stop(messages$errorWrongXLSStructure(filePath, expectedColNames = columnNames))
  }
}
