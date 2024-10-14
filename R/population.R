#' @title Population
#' @description A class representing a population of individuals.
Population <- R6::R6Class(
  classname = "Population",
  public = list(
    #' @description Creates a new instance of Configuration
    #' @param project A Project in which the configurations are defined.
    #' @param id The population identifier.
    #' @param populationCharacteristicsData A data frame containing the population characteristics.
    #' @param userDefinedVariabilityData A data frame containing the user-defined variability.
    #' @param CSVFile A string representing the path to the CSV file containing the population data. If not NULL (default), the population is loaded from the CSV file instead of being created from the populationCharacteristicsData.
    initialize = function(project = project,
                          id,
                          populationCharacteristicsData = NULL,
                          userDefinedVariabilityData = NULL,
                          CSVFile = NULL) {
      private$.project <- project
      private$.id <- id
      private$.populationCharacteristicsData <- populationCharacteristicsData
      private$.userDefinedVariabilityData <- userDefinedVariabilityData
      private$.CSVFile <- CSVFile
    },
    #' @description Prints the population characteristics.
    print = function() {

      cli_li("Population ID: {self$id}")
      invisible(self)
    }
  ),
  active = list(
    #' @field id The population identifier
    id = function() {
      return(private$.id)
    },
    #' @field populationObject Return the population object created from ospsuite::createPopulation.
    populationObject = function() {
      if (is.null(private$.population)) {
        if (!is.null(private$.CSVFile)) {
          private$.population <- loadPopulation(private$.CSVFile)
        } else {
          populationCharacteristicsArguments <-
            as.list(private$.populationCharacteristicsData) %>%
            purrr::keep(~ !is.na(.x)) %>%
            purrr::keep_at(formalArgs(ospsuite::createPopulationCharacteristics))

          populationCharacteristicsArguments[["moleculeOntogenies"]] <-
            createOntogenies(
              data.frame(
                Protein = private$.populationCharacteristicsData$Protein,
                Ontogeny = private$.populationCharacteristicsData$Ontogeny
              )
            )

          populationCharacteristics <- do.call(ospsuite::createPopulationCharacteristics, populationCharacteristicsArguments)

          private$.population <- ospsuite::createPopulation(populationCharacteristics)$population
        }
      }
      return(private$.population)
    }
  ),
  private = list(
    .project = NULL,
    .id = NULL,
    .populationCharacteristicsData = NULL,
    .userDefinedVariabilityData = NULL,
    .CSVFile = NULL,
    .population = NULL
  )
)




# Legacy code -------------------------------------------------------------

# The functions below are defined but not directly used in the workflow.

#' Possible gender entries as integer values
#'
#' @export
GenderInt <- enum(list(
  MALE = 1,
  FEMALE = 2,
  UNKNOWN = 3
))


#' Add user defined variability on parameters to a population.
#'
#' @param population Object of type `Population`
#' @param parameterPaths Vector of parameter path for which the variability is to be added.
#' @param meanValues Vector of mean values of the parameters. Must have the same
#'   length as `parameterPaths`. The type of mean (arithmetic, geometric)
#'   depends on the selected `distribution`. The values must be in the base
#'   units of the parameters.
#' @param sdValues Vector of standard deviation values of the parameters. Must
#'   have the same length as `parameterPaths`. The type of standard deviation
#'   depends on the selected `distribution`.
#' @param distributions Type of distribution from which the random values will
#'   be sampled. Must have the same length as `parameterPaths`.
#' A list of supported distributions is defined in `Distributions`. Default is `"Normal"`.
#' @export
extendPopulationByUserDefinedParams <- function(population, # nolint: object_length_linter.
                                                parameterPaths,
                                                meanValues,
                                                sdValues,
                                                distributions = Distributions$Normal) {
  validateIsOfType(population, "Population")
  validateIsString(parameterPaths)
  validateIsNumeric(c(meanValues, sdValues))
  distributions <- distributions %||% rep(Distributions$Normal, length(parameterPaths))
  validateIsSameLength(parameterPaths, meanValues, sdValues, distributions)


  # Iterate through all parameters and sample a parameter values vector
  for (i in seq_along(parameterPaths)) {
    path <- parameterPaths[[i]]
    mean <- meanValues[[i]]
    sd <- sdValues[[i]]

    # Sample values
    vals <- sampleRandomValue(
      distribution = distributions[[i]],
      mean = mean,
      sd = sd,
      n = population$count
    )

    population$setParameterValues(parameterOrPath = path, values = vals)
  }
}


#' Add user defined variability on parameters to a population from an excel file.
#'
#' @param population Object of type `Population`
#' @param XLSpath Path to the excel file that stores the information of
#'   parameters. The file must have the columns "Container Path",
#'   "Parameter Name", "Mean", "SD", "Units", and "Distribution". Mean and SD
#'   values must be in the base units of the parameters.
#' @param sheet Name or the index of the sheet in the excel file.
#' If `NULL`, the first sheet in the file is used.
#'
#' @details The method reads the information from the specified excel sheet(s)
#'   and calls `extendPopulationByUserDefinedParams`
#'
#' @import readxl
#' @export
extendPopulationFromXLS <- function(population, XLSpath, sheet = NULL) {
  validateIsOfType(population, "Population")
  validateIsString(XLSpath)
  validateIsString(sheet, nullAllowed = TRUE)
  if (is.null(sheet)) {
    sheet <- 1
  }

  columnNames <- c(
    "Container Path", "Parameter Name", "Mean",
    "SD", "Distribution"
  )

  columnTypes <- c("text", "text", "numeric", "numeric", "text")

  tryCatch({
    data <- readExcel(path = XLSpath, sheet = sheet, col_types = columnTypes)
  }, error = function(e) {
    cli::cli_abort(message = messages$errorWrongXLSStructure(filePath = XLSpath, expectedColNames = columnNames), call = rlang::caller_env(4))
  })

  if (!all(columnNames %in% names(data))) {
    cli::cli_abort(message = messages$errorWrongXLSStructure(filePath = XLSpath, expectedColNames = columnNames))
  }

  if(nrow(data) == 0){
    cli::cli_abort(message = c("x" = "The specified excel sheet does not contain any rows with data.",
                               "*" = "Please check the excel sheet name and content and try again."))
  }

  complete_data <-
    data %>%
    dplyr::filter(!dplyr::if_any(dplyr::everything(), ~is.na(.)))


  if(nrow(complete_data) < nrow(data)){
    cli::cli_warn(message = c("x" = "The specified excel sheet contains uncomplete row(s)",
                               "i" = "Using only complete rows to define population parameters"))
  }

  if(nrow(complete_data) == 0){
    cli::cli_abort(message = c("x" = "The specified excel sheet does not contain any complete row",
                               "*" = "Please fill all the columns and try again."))
  }


  extendPopulationByUserDefinedParams(
    population = population,
    parameterPaths = paste(complete_data$`Container Path`, complete_data$`Parameter Name`),
    meanValues = complete_data$Mean,
    sdValues = complete_data$SD,
    distributions = complete_data$Distribution
  )
}

#' Supported distributions for sampling
#' @export
Distributions <- enum(list(
  "Normal",
  "LogNormal"
))

#' Sample a random value from a distribution
#'
#' @param distribution The type of the distribution the random variable is to be
#'   sampled from. See `Distributions` for the list of supported entries.
#' @param mean Mean value of the random variable
#' @param sd Standard deviation of the random variable
#' @param n Size of the sample
#'
#' @return Numerical vector of size n with randomly sampled values
#' @export
sampleRandomValue <- function(distribution, mean, sd, n) {
  if (!enumHasKey(distribution, Distributions)) {
    stop(messages$errorDistributionNotSupported(distribution))
  }

  if (distribution == Distributions$Normal) {
    return(rnorm(n, mean, sd))
  }

  if (distribution == Distributions$LogNormal) {
    location <- log(mean^2 / sqrt(sd^2 + mean^2))
    shape <- sqrt(log(1 + (sd^2 / mean^2)))
    vals <- rlnorm(n = n, meanlog = location, sdlog = shape)
    return(vals)
  }
  return(NULL)
}
