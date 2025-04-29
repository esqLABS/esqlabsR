#' Read an excel file containing information about population and create a
#' `PopulationCharacteristics` object
#'
#' @param XLSpath Path to the excel file
#' @param populationName Name of the population, as defined in the "PopulationName"
#' column
#' @param sheet Name or the index of the sheet in the excel file.
#' If `NULL`, the first sheet in the file is used.
#'
#' @returns A `PopulationCharacteristics` object based on the information
#' in the excel file.
#' @import readxl
#' @export
readPopulationCharacteristicsFromXLS <- function(XLSpath, populationName, sheet = NULL) {
  columnNames <- c(
    "PopulationName", "species", "population", "numberOfIndividuals", "proportionOfFemales", "weightMin", "weightMax",
    "weightUnit", "heightMin", "heightMax", "heightUnit", "ageMin", "ageMax", "BMIMin", "BMIMax", "BMIUnit", "Protein Ontogenies"
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
    # Skip column, if no value defined
    if (is.na(value)) {
      next
    }
    columnName <- names(data[rowIdx, ][i])
    # skip the column 'Protein Ontogenis' it will be processed separately
    if (columnName == "Protein Ontogenies") {
      next
    }
    arguments[[columnName]] <- value
  }

  # Create ontogenies for the proteins
  arguments[["moleculeOntogenies"]] <- .readOntongeniesFromXLS(data[rowIdx, ])

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

  tryCatch(
    {
      data <- readExcel(path = XLSpath, sheet = sheet, col_types = columnTypes)
    },
    error = function(e) {
      cli::cli_abort(message = messages$errorWrongXLSStructure(filePath = XLSpath, expectedColNames = columnNames), call = rlang::caller_env(4))
    }
  )

  if (!all(columnNames %in% names(data))) {
    cli::cli_abort(message = messages$errorWrongXLSStructure(filePath = XLSpath, expectedColNames = columnNames))
  }

  if (nrow(data) == 0) {
    cli::cli_abort(message = c(
      "x" = "The specified excel sheet does not contain any rows with data.",
      "*" = "Please check the excel sheet name and content and try again."
    ))
  }

  complete_data <-
    data %>%
    dplyr::filter(!dplyr::if_any(dplyr::everything(), ~ is.na(.)))


  if (nrow(complete_data) < nrow(data)) {
    cli::cli_warn(message = c(
      "x" = "The specified excel sheet contains uncomplete row(s)",
      "i" = "Using only complete rows to define population parameters"
    ))
  }

  if (nrow(complete_data) == 0) {
    cli::cli_abort(message = c(
      "x" = "The specified excel sheet does not contain any complete row",
      "*" = "Please fill all the columns and try again."
    ))
  }
  extendPopulationByUserDefinedParams(
    population = population,
    parameterPaths = paste(complete_data$`Container Path`, complete_data$`Parameter Name`, sep = "|"),
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
#' @returns Numerical vector of size n with randomly sampled values
#' @export
sampleRandomValue <- function(distribution, mean, sd, n) {
  if (!enumHasKey(distribution, Distributions)) {
    stop(messages$errorDistributionNotSupported(distribution))
  }

  if (distribution == Distributions$Normal) {
    return(stats::rnorm(n, mean, sd))
  }

  if (distribution == Distributions$LogNormal) {
    location <- log(mean^2 / sqrt(sd^2 + mean^2))
    shape <- sqrt(log(1 + (sd^2 / mean^2)))
    vals <- stats::rlnorm(n = n, meanlog = location, sdlog = shape)
    return(vals)
  }
  return(NULL)
}

#' Read ontogeny mapping from excel
#'
#' @param data Data from from excel file with the column 'Protein Ontogenies'
#'
#' @keywords internal
#' @returns A list of `MoleculeOntogeny` objects
.readOntongeniesFromXLS <- function(data) {
  proteinOntogenyMappings <- data[["Protein Ontogenies"]]
  # Return 'NULL' if no ontogenies are specified.
  if (is.na(proteinOntogenyMappings)) {
    return(NULL)
  }

  # calling 'as.character' as sometimes empty cells in Excel are not recognized as
  # chr NA, but some other NA, and strsplit fails.
  proteinOntogenyMappings <- as.character(proteinOntogenyMappings)
  # Proteins/ontogenies mappings are separated by a ','
  proteinOntogenyMappings <- unlist(strsplit(x = proteinOntogenyMappings, split = ",", fixed = TRUE))
  # Remove whitespaces
  proteinOntogenyMappings <- trimws(proteinOntogenyMappings)

  moleculeOntogenies <- vector("list", length(proteinOntogenyMappings))
  for (i in seq_along(proteinOntogenyMappings)) {
    ontogeny <- proteinOntogenyMappings[[i]]
    # Split by ':' to separate protein and ontogeny
    ontogenyMapping <- unlist(strsplit(x = ontogeny, split = ":", fixed = TRUE))
    # Throw an error when the structure is not correct
    if (length(ontogenyMapping) != 2) {
      stop(messages$errorWrongOntogenyStructure(ontogeny))
    }

    protein <- ontogenyMapping[[1]]
    ontogeny <- ontogenyMapping[[2]]
    validateEnumValue(value = ontogeny, enum = ospsuite::StandardOntogeny)
    moleculeOntogenies[[i]] <- ospsuite::MoleculeOntogeny$new(
      molecule = protein,
      ontogeny = ospsuite::StandardOntogeny[[ontogeny]]
    )
  }

  return(moleculeOntogenies)
}
