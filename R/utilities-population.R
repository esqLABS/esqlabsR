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
#' @param parameterPaths Vector of parameter path for which the variability is
#'   to be added.
#' @param meanValues Vector of mean values of the parameters. Must have the same
#'   length as `parameterPaths`. The type of mean (arithmetic, geometric)
#'   depends on the selected `distribution`. The values must be in the base
#'   units of the parameters.
#' @param sdValues Vector of standard deviation values of the parameters. Must
#'   have the same length as `parameterPaths`. The type of standard deviation
#'   depends on the selected `distribution`.
#' @param distributions Type of distribution from which the random values will
#'   be sampled. Must have the same length as `parameterPaths`. A list of
#'   supported distributions is defined in `Distributions`. Default is
#'   `"Normal"`.
#' @export
extendPopulationByUserDefinedParams <- function(
  population, # nolint: object_length_linter.
  parameterPaths,
  meanValues,
  sdValues,
  distributions = Distributions$Normal
) {
  validateIsOfType(population, "Population")
  validateIsString(parameterPaths)
  validateIsNumeric(c(meanValues, sdValues))
  distributions <- distributions %||%
    rep(Distributions$Normal, length(parameterPaths))
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


#' Add user defined variability on parameters to a population from an excel
#' file.
#'
#' @param population Object of type `Population`
#' @param XLSpath Path to the excel file that stores the information of
#'   parameters. The file must have the columns "Container Path", "Parameter
#'   Name", "Mean", "SD", "Units", and "Distribution". Mean and SD values must
#'   be in the base units of the parameters.
#' @param sheet Name or the index of the sheet in the excel file. If `NULL`, the
#'   first sheet in the file is used.
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
    "Container Path",
    "Parameter Name",
    "Mean",
    "SD",
    "Distribution"
  )

  columnTypes <- c("text", "text", "numeric", "numeric", "text")

  tryCatch(
    {
      data <- readExcel(path = XLSpath, sheet = sheet, col_types = columnTypes)
    },
    error = function(e) {
      stop(
        message = messages$errorWrongXLSStructure(
          filePath = XLSpath,
          expectedColNames = columnNames
        )
      )
    }
  )

  if (!all(columnNames %in% names(data))) {
    stop(
      messages$errorWrongXLSStructure(
        filePath = XLSpath,
        expectedColNames = columnNames
      )
    )
  }

  if (nrow(data) == 0) {
    stop(
      messages$excelNoDataRows()
    )
  }

  complete_data <-
    data |>
    dplyr::filter(!dplyr::if_any(dplyr::everything(), ~ is.na(.)))

  if (nrow(complete_data) < nrow(data)) {
    warning(messages$excelUncompleteRows())
  }

  if (nrow(complete_data) == 0) {
    stop(
      messages$excelNoCompleteRows()
    )
  }
  extendPopulationByUserDefinedParams(
    population = population,
    parameterPaths = paste(
      complete_data$`Container Path`,
      complete_data$`Parameter Name`,
      sep = "|"
    ),
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

#' Add a population to a Project
#'
#' @param project A `Project` object.
#' @param populationId Character scalar, unique ID.
#' @param species Character scalar.
#' @param numberOfIndividuals Integer, positive.
#' @param ... Optional named fields. Accepted: `proportionOfFemales`,
#'   `weightMin`, `weightMax`, `heightMin`, `heightMax`, `ageMin`, `ageMax`,
#'   `BMIMin`, `BMIMax`, `gender`, `weightUnit`, `heightUnit`, `ageUnit`,
#'   `BMIUnit`, `population`, `diseaseState`. Numeric range fields are
#'   coerced via `as.double()`.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family population
addPopulation <- function(
  project,
  populationId,
  species,
  numberOfIndividuals,
  ...
) {
  validateIsOfType(project, "Project")
  errors <- character()

  if (
    !is.character(populationId) ||
      length(populationId) != 1 ||
      is.na(populationId) ||
      nchar(populationId) == 0
  ) {
    errors <- c(errors, "populationId must be a non-empty string")
  } else if (populationId %in% names(project$populations)) {
    errors <- c(
      errors,
      paste0("population '", populationId, "' already exists")
    )
  }

  if (
    !is.character(species) ||
      length(species) != 1 ||
      is.na(species) ||
      nchar(species) == 0
  ) {
    errors <- c(errors, "species must be a non-empty string")
  }

  if (
    !is.numeric(numberOfIndividuals) ||
      length(numberOfIndividuals) != 1 ||
      is.na(numberOfIndividuals) ||
      numberOfIndividuals <= 0
  ) {
    errors <- c(errors, "numberOfIndividuals must be a positive number")
  }

  dots <- list(...)
  numericFields <- c(
    "proportionOfFemales",
    "weightMin",
    "weightMax",
    "heightMin",
    "heightMax",
    "ageMin",
    "ageMax",
    "BMIMin",
    "BMIMax"
  )
  stringFields <- c(
    "gender",
    "weightUnit",
    "heightUnit",
    "ageUnit",
    "BMIUnit",
    "population",
    "diseaseState"
  )
  allowed <- c(numericFields, stringFields)
  unknown <- setdiff(names(dots), allowed)
  if (length(unknown) > 0) {
    errors <- c(
      errors,
      paste0(
        "unknown fields: ",
        paste(unknown, collapse = ", "),
        ". Allowed: ",
        paste(allowed, collapse = ", ")
      )
    )
  }

  if (length(errors) > 0) {
    stop(paste0(
      "Cannot add population '",
      populationId,
      "':\n- ",
      paste(errors, collapse = "\n- ")
    ))
  }

  entry <- list(
    species = species,
    numberOfIndividuals = as.double(numberOfIndividuals)
  )
  for (field in numericFields) {
    if (!is.null(dots[[field]])) entry[[field]] <- as.double(dots[[field]])
  }
  for (field in stringFields) {
    if (!is.null(dots[[field]])) entry[[field]] <- dots[[field]]
  }

  project$populations[[populationId]] <- entry
  project$.markModified()
  invisible(project)
}

#' Remove a population from a Project
#' @param project A `Project` object.
#' @param populationId Character scalar.
#' @returns The `project` object, invisibly.
#' @export
#' @family population
removePopulation <- function(project, populationId) {
  validateIsOfType(project, "Project")
  if (
    !is.character(populationId) ||
      length(populationId) != 1 ||
      is.na(populationId) ||
      nchar(populationId) == 0
  ) {
    stop("populationId must be a non-empty string")
  }
  if (!(populationId %in% names(project$populations))) {
    cli::cli_warn("population {.val {populationId}} not found; no-op.")
    return(invisible(project))
  }
  .warnIfReferenced(project, "population", populationId)
  project$populations[[populationId]] <- NULL
  project$.markModified()
  invisible(project)
}
