# Parse ----
#
# Parse the `populations` JSON array into a named list keyed by
# `populationId`. Numeric fields are coerced via `as.double`. Each entry
# is stamped with `class = c("Population", "list")` to enable S3 dispatch.
#
# @keywords internal
# @noRd
.parsePopulations <- function(populationsData) {
  if (is.null(populationsData) || length(populationsData) == 0L) {
    return(list())
  }
  numericFields <- c(
    "numberOfIndividuals",
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
  result <- list()
  for (entry in populationsData) {
    id <- .keyedTreeRecordId(entry, "populationId", "population")
    .assertNoEmptyObjectFields(entry, "population")
    popData <- list()
    for (field in names(entry)) {
      if (field == "populationId") {
        next
      }
      val <- entry[[field]]
      if (is.null(val)) {
        next
      }
      if (field %in% numericFields) {
        val <- as.double(val)
      }
      popData[[field]] <- val
    }
    class(popData) <- c("Population", "list")
    result[[id]] <- popData
  }
  result
}

# Section validation adapter ----
#
# Registered in `.validationAdapters` (R/validation.R) and called by
# `.runProjectValidation()`.

#' @keywords internal
#' @noRd
.populationsValidatorAdapter <- function(project) {
  .validatePopulations(project$populations)
}

#' Validate the `populations` section of a Project
#'
#' Checks `species` is set and warns on out-of-range
#' `proportionOfFemales` or inverted Min/Max ranges (age, weight,
#' height, BMI).
#'
#' @param populations Named list from `project$populations`.
#' @return validationResult.
#' @keywords internal
#' @noRd
.validatePopulations <- function(populations) {
  result <- validationResult$new()

  if (is.null(populations) || length(populations) == 0) {
    result$add_warning("Data", "No populations defined")
    return(result)
  }

  for (id in names(populations)) {
    pop <- populations[[id]]

    result <- .check_required_fields(
      pop,
      c("species"),
      paste0("population '", id, "'"),
      result
    )

    if (!is.null(pop$proportionOfFemales)) {
      pof <- as.numeric(pop$proportionOfFemales)
      if (!is.na(pof) && (pof < 0 || pof > 100)) {
        result$add_warning(
          "Data Range",
          paste0(
            "proportionOfFemales in population '",
            id,
            "' should be between 0 and 100"
          )
        )
      }
    }

    rangePairs <- list(
      c("ageMin", "ageMax"),
      c("weightMin", "weightMax"),
      c("heightMin", "heightMax"),
      c("BMIMin", "BMIMax")
    )
    for (pair in rangePairs) {
      lo <- pop[[pair[1]]]
      hi <- pop[[pair[2]]]
      if (
        !is.null(lo) &&
          !is.null(hi) &&
          !is.na(lo) &&
          !is.na(hi) &&
          lo > hi
      ) {
        result$add_warning(
          "Data Range",
          paste0(pair[1], " > ", pair[2], " in population '", id, "'")
        )
      }
    }
  }

  result
}

# Print ----

#' @exportS3Method
#' @noRd
print.Population <- function(x, ...) {
  ospsuite.utils::ospPrintClass(x)
  ospsuite.utils::ospPrintItems(
    list(
      "Species" = x$species %||% "",
      "Number of Individuals" = x$numberOfIndividuals %||% "",
      "Proportion of Females" = x$proportionOfFemales %||% "",
      "Age Range" = .formatRange(x$ageMin, x$ageMax),
      "Weight Range" = .formatRange(x$weightMin, x$weightMax),
      "Height Range" = .formatRange(x$heightMin, x$heightMax)
    ),
    print_empty = TRUE
  )
  invisible(x)
}

# Format a "min - max" range for a population print, or "" when neither bound
# is set.
#
# @keywords internal
# @noRd
.formatRange <- function(lo, hi) {
  if (is.null(lo) && is.null(hi)) {
    return("")
  }
  paste0(format(lo %||% NA), " - ", format(hi %||% NA))
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
#'   and calls `extendPopulationByUserDefinedParams`.
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
      cli::cli_abort(
        messages$errorWrongXLSStructure(
          filePath = XLSpath,
          expectedColNames = columnNames
        )
      )
    }
  )

  if (!all(columnNames %in% names(data))) {
    cli::cli_abort(
      messages$errorWrongXLSStructure(
        filePath = XLSpath,
        expectedColNames = columnNames
      )
    )
  }

  if (nrow(data) == 0) {
    cli::cli_abort(messages$excelNoDataRows())
  }

  complete_data <- data |>
    dplyr::filter(!dplyr::if_any(dplyr::everything(), ~ is.na(.)))

  if (nrow(complete_data) < nrow(data)) {
    cli::cli_warn(messages$excelUncompleteRows())
  }

  if (nrow(complete_data) == 0) {
    cli::cli_abort(messages$excelNoCompleteRows())
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
    cli::cli_abort(messages$errorDistributionNotSupported(distribution))
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

# Public CRUD: populations ----

#' Add one or more populations to a Project
#'
#' Add populations to `project$populations`, vectorizing over a vector of ids
#' (see the recycling rule under Details). `species`, `numberOfIndividuals`,
#' and the optional `...` fields are all scalar-per-entity (recycle/align).
#'
#' @inherit vectorizedAuthoring details
#'
#' @param project A `Project` object.
#' @param id Character vector of unique ids (the number of populations to add).
#'   Each is canonicalized to a safe, lowercase id (a warning names the result
#'   if it changed).
#' @param species Character scalar (recycled) or the same length as `id`.
#' @param numberOfIndividuals Positive number, scalar (recycled) or the same
#'   length as `id`.
#' @param ... Optional named fields. Accepted: `proportionOfFemales`,
#'   `weightMin`, `weightMax`, `heightMin`, `heightMax`, `ageMin`,
#'   `ageMax`, `BMIMin`, `BMIMax`, `gender`, `weightUnit`, `heightUnit`,
#'   `ageUnit`, `BMIUnit`, `population`, `diseaseState`. Numeric range
#'   fields are coerced via `as.double()`.
#' @returns The `project` object, invisibly.
#' @export
#' @family population
addPopulation <- function(
  project,
  id,
  species,
  numberOfIndividuals,
  ...
) {
  validateIsOfType(project, "Project")
  .assertIdVector(id)
  id <- .canonicalizeId(id)
  n <- length(id)

  perEntity <- .alignAuthoringArgs(
    id,
    scalarFields = c(
      list(species = species, numberOfIndividuals = numberOfIndividuals),
      list(...)
    )
  )

  .assertNoDuplicateIds(id, "population")
  clash <- intersect(id, names(project$populations))
  if (length(clash) > 0L) {
    cli::cli_abort("population {.val {clash}} already exists")
  }
  call <- rlang::current_env()
  entries <- lapply(seq_len(n), function(i) {
    .buildPopulationEntry(id[[i]], perEntity[[i]], call = call)
  })

  populations <- project$.getSection("populations") %||% list()
  for (i in seq_len(n)) {
    populations[[id[[i]]]] <- entries[[i]]
  }
  project$.setSection("populations", populations)
  invisible(project)
}

.populationNumericFields <- c(
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

.populationStringFields <- c(
  "gender",
  "weightUnit",
  "heightUnit",
  "ageUnit",
  "BMIUnit",
  "population",
  "diseaseState"
)

# Build one classed `Population` entry from its id and per-entity field list,
# validating the same way the scalar path always has (`species` non-empty,
# `numberOfIndividuals` positive). Aborts naming the population on a problem.
#
# @keywords internal
# @noRd
.buildPopulationEntry <- function(id, fields, call = rlang::caller_env()) {
  errors <- character()
  species <- fields$species
  if (
    !is.character(species) ||
      length(species) != 1L ||
      is.na(species) ||
      nchar(species) == 0
  ) {
    errors <- c(errors, "species must be a non-empty string")
  }

  numberOfIndividuals <- fields$numberOfIndividuals
  if (
    !is.numeric(numberOfIndividuals) ||
      length(numberOfIndividuals) != 1L ||
      is.na(numberOfIndividuals) ||
      numberOfIndividuals <= 0 ||
      numberOfIndividuals != round(numberOfIndividuals)
  ) {
    errors <- c(errors, "numberOfIndividuals must be a positive whole number")
  }

  allowed <- c(
    "species",
    "numberOfIndividuals",
    .populationNumericFields,
    .populationStringFields
  )
  unknown <- setdiff(names(fields), allowed)
  if (length(unknown) > 0L) {
    errors <- c(
      errors,
      paste0(
        "unknown fields: ",
        paste(unknown, collapse = ", "),
        ". Allowed: ",
        paste(
          setdiff(allowed, c("species", "numberOfIndividuals")),
          collapse = ", "
        )
      )
    )
  }

  if (length(errors) > 0L) {
    cli::cli_abort(
      c(
        "Cannot add population {.val {id}}:",
        stats::setNames(errors, rep("x", length(errors)))
      ),
      call = call
    )
  }

  entry <- list(
    species = species,
    numberOfIndividuals = as.double(numberOfIndividuals)
  )
  for (field in .populationNumericFields) {
    if (!is.null(fields[[field]])) entry[[field]] <- as.double(fields[[field]])
  }
  for (field in .populationStringFields) {
    if (!is.null(fields[[field]])) entry[[field]] <- fields[[field]]
  }
  class(entry) <- c("Population", "list")
  entry
}

#' Remove one or more populations from a Project
#'
#' Drop the populations with matching ids in one write-through. Warns (and
#' skips) any id not present, and warns when a removed population is still
#' referenced.
#'
#' @param project A `Project` object.
#' @param id Character vector of population ids to remove. Each is
#'   canonicalized the same way [addPopulation()] canonicalizes it.
#' @returns The `project` object, invisibly.
#' @export
#' @family population
removePopulation <- function(project, id) {
  validateIsOfType(project, "Project")
  .assertIdVector(id)
  id <- .canonicalizeId(id)

  missingIds <- setdiff(id, names(project$populations))
  if (length(missingIds) > 0L) {
    cli::cli_warn("population {.val {missingIds}} not found; no-op.")
  }
  toRemove <- intersect(id, names(project$populations))
  if (length(toRemove) == 0L) {
    return(invisible(project))
  }
  for (one in toRemove) {
    .warnIfReferenced(project, "population", one)
  }
  populations <- project$.getSection("populations")
  populations[toRemove] <- NULL
  project$.setSection("populations", populations)
  invisible(project)
}

#' Modify fields of an existing population
#'
#' @description Changes one or more fields of the population identified by
#'   `id` and persists the change immediately to the population definition
#'   (write-through). The `project$populations` accessor is read-only, so this
#'   is the way to revise an existing population in place.
#'
#'   Only the arguments you pass via `...` are changed; every other field
#'   keeps its current value (partial update). Validation matches
#'   [addPopulation()]: the numeric range fields are coerced via
#'   `as.double()` and `numberOfIndividuals` (if supplied) must be a
#'   positive number. The required `species` field, if supplied, must be a
#'   non-empty string.
#'
#' @inherit vectorizedAuthoring details
#'
#' @param project A `Project` object.
#' @param id Character vector. Ids of the populations to modify. Each is
#'   canonicalized the same way [addPopulation()] canonicalizes it, and must
#'   already exist in `project$populations`.
#' @param ... Named fields to change. Accepted: `species`,
#'   `numberOfIndividuals`, `proportionOfFemales`, `weightMin`,
#'   `weightMax`, `heightMin`, `heightMax`, `ageMin`, `ageMax`, `BMIMin`,
#'   `BMIMax`, `gender`, `weightUnit`, `heightUnit`, `ageUnit`, `BMIUnit`,
#'   `population`, `diseaseState`. Scalar-per-entity fields recycle/align
#'   across `id`. Numeric fields are coerced via `as.double()`. Unknown
#'   fields trigger an error.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family population
setPopulation <- function(project, id, ...) {
  validateIsOfType(project, "Project")
  .assertIdVector(id)
  id <- .canonicalizeId(id)
  n <- length(id)
  missingIds <- setdiff(id, names(project$populations))
  if (length(missingIds) > 0L) {
    cli::cli_abort(c(
      "Cannot modify population {.val {missingIds}}: it does not exist.",
      "i" = "Use {.fn addPopulation} to create it first."
    ))
  }

  dots <- list(...)
  perEntity <- .alignAuthoringArgs(id, scalarFields = dots)
  suppliedNames <- names(dots)

  call <- rlang::current_env()
  entries <- lapply(seq_len(n), function(i) {
    .setOnePopulation(
      project,
      id[[i]],
      perEntity[[i]][suppliedNames],
      call = call
    )
  })

  populations <- project$.getSection("populations")
  for (i in seq_len(n)) {
    populations[[id[[i]]]] <- entries[[i]]
  }
  project$.setSection("populations", populations)
  invisible(project)
}

# Apply a partial-update field set to one existing population, returning the
# updated classed entry. Validates only the supplied fields. Aborts naming the
# population on a problem.
#
# @keywords internal
# @noRd
.setOnePopulation <- function(project, id, fields, call = rlang::caller_env()) {
  numericFields <- c("numberOfIndividuals", .populationNumericFields)
  stringFields <- c("species", .populationStringFields)
  allowed <- c(numericFields, stringFields)
  unknown <- setdiff(names(fields), allowed)
  if (length(unknown) > 0L) {
    cli::cli_abort(
      c(
        "Cannot modify population {.val {id}}:",
        "x" = "unknown fields: {.val {unknown}}. Allowed: {.val {allowed}}."
      ),
      call = call
    )
  }

  if ("species" %in% names(fields)) {
    species <- fields$species
    if (
      !is.character(species) ||
        length(species) != 1L ||
        is.na(species) ||
        nchar(species) == 0
    ) {
      cli::cli_abort("{.arg species} must be a non-empty string", call = call)
    }
  }
  if ("numberOfIndividuals" %in% names(fields)) {
    count <- fields$numberOfIndividuals
    if (
      !is.numeric(count) ||
        length(count) != 1L ||
        is.na(count) ||
        count <= 0
    ) {
      cli::cli_abort(
        "{.arg numberOfIndividuals} must be a positive number",
        call = call
      )
    }
  }

  entry <- project$populations[[id]]
  for (field in names(fields)) {
    if (field %in% numericFields) {
      entry[[field]] <- .coerceNumericField(fields[[field]])
    } else {
      entry[[field]] <- fields[[field]]
    }
  }
  class(entry) <- c("Population", "list")
  entry
}
