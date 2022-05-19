#' Read an excel file containing information about population and create a
#' `PopulationCharacteristics` object
#'
#' @param XLSpath Path to the excel file
#' @param populationName Name of the population, as defined in the "PopulationName"
#' column
#' @param sheet Name or the index of the sheet in the excel file.
#' If `NULL`, the first sheet in the file is used.
#'
#' @return A `PopulationCharacteristics` object based on the information
#' in the excel file.
#' @import readxl
#' @export
readPopulationCharacteristicsFromXLS <- function(XLSpath, populationName, sheet = NULL) {
  columnNames <- c(
    "PopulationName", "species", "population", "numberOfIndividuals", "proportionOfFemales", "weightMin", "weightMax",
    "weightUnit", "heightMin", "heightMax", "heightUnit", "ageMin", "ageMax", "BMIMin", "BMIMax", "BMIUnit"
  )

  validateIsString(c(XLSpath, populationName))
  validateIsString(sheet, nullAllowed = TRUE)

  if (is.null(sheet)) {
    sheet <- 1
  }
  data <- readExcel(path = XLSpath, sheet = sheet)

  if (!all(columnNames %in% names(data))) {
    stop(messages$errorWrongXLSStructure(filePath = XLSpath, expectedColNames = columnNames))
  }
  # Find the row with the given population name
  rowIdx <- which(data$PopulationName == populationName)
  if (length(rowIdx) == 0) {
    stop(messages$errorWrongPopulationName(populationName))
  }

  # Parse the information about population.
  # We only want to use arguments that have a value in the xls-file
  arguments <- list()
  # Starting to iterate by 2 as the first entry is "PopulationName" and
  # is not an argument
  for (i in 2:length(data[rowIdx, ])) {
    value <- data[[rowIdx, i]]
    if (is.na(value)) {
      next
    }
    name <- names(data[rowIdx, ][i])
    arguments[[name]] <- value
  }

  # Using do.call to call the method with arguments in a list
  populationCharacterstics <- do.call(createPopulationCharacteristics, arguments)

  return(populationCharacterstics)
}

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
extendPopulationByUserDefinedParams <- function(population,
                                                parameterPaths,
                                                meanValues,
                                                sdValues,
                                                distributions = Distributions$Normal) {
  validateIsOfType(population, "Population")
  validateIsString(parameterPaths)
  validateIsNumeric(meanValues, sdValues)
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
#'   parameters. The file must have the columns "Container.Path",
#'   "Parameter.Name", "Mean", "SD", "Units", and "Distribution". Mean and SD
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

  data <- readExcel(path = XLSpath, sheet = sheet)
  if (!all(columnNames %in% names(data))) {
    stop(messages$errorWrongXLSStructure(filePath = XLSpath, expectedColNames = columnNames))
  }

  paramPaths <- c(dim(data)[[1]])
  meanVals <- c(dim(data)[[1]])
  sdVals <- c(dim(data)[[1]])
  distributions <- c(dim(data)[[1]])

  for (i in seq_along(data$Container.Path)) {
    paramPath <- paste(data[["Container.Path"]][[i]], data[["Parameter.Name"]][[i]], sep = "|")
    paramPaths[[i]] <- paramPath
    meanVals[[i]] <- as.numeric(data[["Mean"]][[i]])
    sdVals[[i]] <- as.numeric(data[["SD"]][[i]])
    distributions[[i]] <- data[["Distribution"]][[i]]
  }

  extendPopulationByUserDefinedParams(
    population = population, parameterPaths = paramPaths,
    meanValues = meanVals, sdValues = sdVals,
    distributions = distributions
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
