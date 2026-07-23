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
    # A `type` field marks a non-demographics source (a `programmatic` runtime
    # sentinel or a `csv` file reference); those carry no species/age/weight, so
    # stamp them a source class that prints their source instead of empty
    # demographics. An entry with no `type` is the demographics spec, unchanged.
    popData <- if (is.null(popData$type)) {
      class(popData) <- c("Population", "list")
      popData
    } else {
      .asPopulationSource(popData)
    }
    result[[id]] <- popData
  }
  result
}

# Stamp a non-demographics population entry (a `programmatic` sentinel or a
# `csv` file reference) with `c("PopulationSource", "Population", "list")`.
# `"Population"` stays in the chain so `inherits(x, "Population")` still holds,
# while the leading `"PopulationSource"` selects the source-aware print method.
#
# @keywords internal
# @noRd
.asPopulationSource <- function(entry) {
  class(entry) <- c("PopulationSource", "Population", "list")
  entry
}

# Section validation adapter ----
#
# Registered in `.validationAdapters` (R/validation.R) and called by
# `.runProjectValidation()`.

#' @keywords internal
#' @noRd
.populationsValidatorAdapter <- function(project) {
  .validatePopulations(project$definitions$populations)
}

# The optional `type` a population entry may carry. Absent means a demographics
# spec (the default, unchanged shape). `programmatic` is a runtime sentinel for
# an injected `ospsuite::Population`; `csv` references a population table file.
#
# @keywords internal
# @noRd
.populationSourceTypes <- c("programmatic", "csv")

# Required fields for a population entry, keyed by its `type`. A demographics
# spec (no `type`) needs `species`; a `csv` source needs `file`; a
# `programmatic` sentinel needs nothing (its object lives in the runtime store).
#
# @keywords internal
# @noRd
.populationEntryRequiredFields <- function(type) {
  switch(
    type %||% "spec",
    "spec" = "species",
    "csv" = "file",
    character(0)
  )
}

#' Validate the `populations` section of a Project
#'
#' Per-entry checks: an optional `type` (`programmatic` / `csv`) must be
#' recognized; the type's required fields are present (`species` for a
#' demographics spec, `file` for a `csv` source); warns on out-of-range
#' `proportionOfFemales` or inverted Min/Max ranges (age, weight, height,
#' BMI) for demographics specs.
#'
#' @param populations Named list from `populations` definitions.
#' @return validationResult.
#' @keywords internal
#' @noRd
.validatePopulations <- function(populations) {
  result <- validationResult$new()

  if (is.null(populations) || length(populations) == 0) {
    result$addWarning("Data", "No populations defined")
    return(result)
  }

  for (id in names(populations)) {
    pop <- populations[[id]]

    if (!is.null(pop$type) && !pop$type %in% .populationSourceTypes) {
      result$addCriticalError(
        "Data",
        paste0(
          "population '",
          id,
          "' has invalid type '",
          pop$type,
          "'. Must be one of: ",
          paste(.populationSourceTypes, collapse = ", "),
          " (or omitted for a demographics spec)."
        )
      )
      next
    }

    result <- .checkRequiredFields(
      pop,
      .populationEntryRequiredFields(pop$type),
      paste0("population '", id, "'"),
      result
    )

    if (!is.null(pop$proportionOfFemales)) {
      pof <- as.numeric(pop$proportionOfFemales)
      if (!is.na(pof) && (pof < 0 || pof > 100)) {
        result$addWarning(
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
        result$addWarning(
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

#' @exportS3Method
#' @noRd
print.PopulationSource <- function(x, ...) {
  ospsuite.utils::ospPrintClass(x)
  ospsuite.utils::ospPrintItems(
    list(
      "Type" = x$type %||% "",
      "File" = x$file %||%
        (if (identical(x$type, "programmatic")) {
          "resolved from the runtime store at run time"
        } else {
          ""
        })
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
        messages$wrongXLSStructure(
          filePath = XLSpath,
          expectedColNames = columnNames
        )
      )
    }
  )

  if (!all(columnNames %in% names(data))) {
    cli::cli_abort(
      messages$wrongXLSStructure(
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
    cli::cli_abort(messages$distributionNotSupported(distribution))
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
#' @description Add populations to `populations` definitions. Two forms:
#'
#' - A **demographics spec**: pass `species` and `numberOfIndividuals` (plus
#'   optional `...` fields). This vectorizes over a vector of ids (see the
#'   recycling rule under Details); `species`, `numberOfIndividuals`, and the
#'   `...` fields are scalar-per-definition (recycle/align).
#' - An **injected object**: pass a single id and an [ospsuite::Population]
#'   object in the `species` position. The object is stored in the R session
#'   and the scenario runs against it directly (a mutated or programmatically
#'   built population). `numberOfIndividuals` and `...` fields are not accepted
#'   for this form. It survives to disk only after [saveProject()], which
#'   freezes the sampled population to a `<id>.csv` file under the populations
#'   folder; rerun the code that built it to reproduce it in a new session.
#'
#' @inherit vectorizedAuthoring details
#'
#' @param project A `Project` object.
#' @param id Character vector of unique ids (the number of populations to add).
#'   Each is canonicalized to a safe, lowercase id (a warning names the result
#'   if it changed). For an injected [ospsuite::Population] object the id must
#'   be a single value.
#' @param species Character scalar (recycled) or the same length as `id`; or an
#'   [ospsuite::Population] object to inject (single id only).
#' @param numberOfIndividuals Positive number, scalar (recycled) or the same
#'   length as `id`. Omit when injecting a `Population` object.
#' @param ... Optional named fields. Accepted: `proportionOfFemales`,
#'   `weightMin`, `weightMax`, `heightMin`, `heightMax`, `ageMin`,
#'   `ageMax`, `BMIMin`, `BMIMax`, `gender`, `weightUnit`, `heightUnit`,
#'   `ageUnit`, `BMIUnit`, `population`, `diseaseState`, and `overwrite`.
#'   Numeric range fields are coerced via `as.double()`. `overwrite` is a
#'   logical scalar (default `FALSE`): an id that already exists aborts unless
#'   `overwrite = TRUE`, which replaces it (last-write-wins).
#' @returns The `project` object, invisibly.
#' @export
#' @family population
addPopulation <- function(
  project,
  id,
  species,
  numberOfIndividuals = NULL,
  ...
) {
  validateIsOfType(project, "Project")
  project$addPopulation(id, species, numberOfIndividuals, ...)
}

# Implementation behind `project$addPopulation()` / `addPopulation()`.
#
# @keywords internal
# @noRd
.addPopulation_impl <- function(
  self,
  private,
  id,
  species,
  numberOfIndividuals = NULL,
  ...,
  .call
) {
  rlang::local_error_call(.call)
  .assertIdVector(id)
  id <- .canonicalizeId(id)
  n <- length(id)

  dots <- list(...)
  # `overwrite` arrives through `...`; pull it out (validated) before aligning
  # so it is not mistaken for a per-definition field.
  overwrite <- .validateOverwriteFlag(dots[["overwrite"]])
  dots[["overwrite"]] <- NULL

  # Injected-object form: an `ospsuite::Population` (a DotNetWrapper) in the
  # `species` position. A demographics-spec entry is a plain list also classed
  # "Population", so dispatch on DotNetWrapper, not on class "Population".
  if (inherits(species, "DotNetWrapper")) {
    return(.addProgrammaticPopulation_impl(
      self,
      private,
      id,
      population = species,
      numberOfIndividuals = numberOfIndividuals,
      dots = dots,
      overwrite = overwrite,
      .call = .call
    ))
  }
  if (is.null(numberOfIndividuals)) {
    cli::cli_abort(
      "{.arg numberOfIndividuals} is required when adding a demographics-spec \
       population."
    )
  }
  perDefinition <- .alignAuthoringArgs(
    id,
    scalarFields = c(
      list(species = species, numberOfIndividuals = numberOfIndividuals),
      dots
    )
  )

  .assertNoOverwriteClash(
    id,
    names(self$definitions$populations),
    "population",
    overwrite
  )
  call <- .call
  entries <- lapply(seq_len(n), function(i) {
    .buildPopulationEntry(id[[i]], perDefinition[[i]], call = call)
  })

  populations <- private$.getSection("populations") %||% list()
  for (i in seq_len(n)) {
    populations[[id[[i]]]] <- entries[[i]]
  }
  private$.setSection("populations", populations)
  invisible(self)
}

# Add a session-injected `ospsuite::Population`, keyed by a single id. Writes a
# `{type: "programmatic"}` sentinel into the `populations` section and stores
# the live object in the runtime store; the two are updated together (in-memory
# only). Aborts on stray demographics fields, which have no meaning for an
# already-built population object.
#
# @keywords internal
# @noRd
.addProgrammaticPopulation_impl <- function(
  self,
  private,
  id,
  population,
  numberOfIndividuals,
  dots,
  overwrite,
  .call
) {
  rlang::local_error_call(.call)
  if (length(id) != 1L) {
    cli::cli_abort(
      "A programmatic population is added one id at a time; {.arg id} must be \
       a single value."
    )
  }
  stray <- names(dots)
  if (!is.null(numberOfIndividuals) || length(stray) > 0L) {
    cli::cli_abort(c(
      "Extra fields are not accepted when injecting a {.cls Population} object.",
      "i" = "An injected population already carries its own individuals; drop \
             {.arg numberOfIndividuals}{if (length(stray)) ' and the other \
             fields' else ''}."
    ))
  }

  .assertNoOverwriteClash(
    id,
    names(self$definitions$populations),
    "population",
    overwrite
  )

  sentinel <- .asPopulationSource(list(type = "programmatic"))
  populations <- private$.getSection("populations") %||% list()
  populations[[id]] <- sentinel
  private$.setSection("populations", populations)
  private$.programmaticPopulations[[id]] <- population
  cli::cli_inform(messages$populationProgrammaticAdded(
    id,
    hasPopulationsFolder = !is.null(self$paths$populationsFolder)
  ))
  invisible(self)
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

# Build one classed `Population` entry from its id and per-definition field list,
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

  # The numeric range fields are stored as doubles. Coerce a numeric-like
  # value and reject only a value that does not coerce to a single finite
  # number (e.g. "heavy" -> NA) rather than silently storing NA. This matches
  # the set path (`.setOnePopulation()`).
  for (field in .populationNumericFields) {
    value <- fields[[field]]
    if (!is.null(value)) {
      coerced <- suppressWarnings(as.double(value))
      if (length(value) != 1L || is.na(coerced) || !is.finite(coerced)) {
        errors <- c(errors, paste0(field, " must be a single finite number"))
      }
    }
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
  project$removePopulation(id)
}

# Implementation behind `project$removePopulation()` / `removePopulation()`.
#
# @keywords internal
# @noRd
.removePopulation_impl <- function(self, private, id, .call) {
  rlang::local_error_call(.call)
  .assertIdVector(id)
  id <- .canonicalizeId(id)

  missingIds <- setdiff(id, names(self$definitions$populations))
  if (length(missingIds) > 0L) {
    cli::cli_warn("population {.val {missingIds}} not found; no-op.")
  }
  toRemove <- intersect(id, names(self$definitions$populations))
  if (length(toRemove) == 0L) {
    return(invisible(self))
  }
  for (one in toRemove) {
    .warnIfReferenced(self, "population", one)
  }
  populations <- private$.getSection("populations")
  populations[toRemove] <- NULL
  private$.setSection("populations", populations)
  # Drop any injected object under a removed id (a no-op for a non-programmatic
  # id), so the runtime store never outlives its section sentinel.
  for (one in toRemove) {
    private$.programmaticPopulations[[one]] <- NULL
  }
  invisible(self)
}

#' Modify fields of an existing population
#'
#' @description Changes one or more fields of the population identified by
#'   `id` and persists the change immediately to the population definition
#'   (write-through). The `populations` definitions accessor is read-only, so this
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
#'   already exist in `populations` definitions.
#' @param ... Named fields to change. Accepted: `species`,
#'   `numberOfIndividuals`, `proportionOfFemales`, `weightMin`,
#'   `weightMax`, `heightMin`, `heightMax`, `ageMin`, `ageMax`, `BMIMin`,
#'   `BMIMax`, `gender`, `weightUnit`, `heightUnit`, `ageUnit`, `BMIUnit`,
#'   `population`, `diseaseState`. Scalar-per-definition fields recycle/align
#'   across `id`. Numeric fields are coerced via `as.double()`. Unknown
#'   fields trigger an error.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family population
setPopulation <- function(project, id, ...) {
  validateIsOfType(project, "Project")
  project$setPopulation(id, ...)
}

# Implementation behind `project$setPopulation()` / `setPopulation()`.
#
# @keywords internal
# @noRd
.setPopulation_impl <- function(self, private, id, ..., .call) {
  rlang::local_error_call(.call)
  .assertIdVector(id)
  id <- .canonicalizeId(id)
  n <- length(id)
  missingIds <- setdiff(id, names(self$definitions$populations))
  if (length(missingIds) > 0L) {
    cli::cli_abort(c(
      "Cannot modify population {.val {missingIds}}: it does not exist.",
      "i" = "Use {.fn addPopulation} to create it first."
    ))
  }

  dots <- list(...)
  perDefinition <- .alignAuthoringArgs(id, scalarFields = dots)
  suppliedNames <- names(dots)

  call <- .call
  entries <- lapply(seq_len(n), function(i) {
    .setOnePopulation(
      self,
      id[[i]],
      perDefinition[[i]][suppliedNames],
      call = call
    )
  })

  populations <- private$.getSection("populations")
  for (i in seq_len(n)) {
    populations[[id[[i]]]] <- entries[[i]]
  }
  private$.setSection("populations", populations)
  invisible(self)
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
        count <= 0 ||
        count != round(count)
    ) {
      cli::cli_abort(
        "{.arg numberOfIndividuals} must be a positive whole number",
        call = call
      )
    }
  }
  # The numeric range fields are stored as doubles. Coerce a numeric-like
  # value (including a character such as "75" from Excel) and reject only a
  # value that does not coerce to a single finite number (e.g. "heavy" -> NA)
  # rather than silently storing NA. A NULL is allowed: it clears the field
  # via `.coerceNumericField()` below.
  for (field in .populationNumericFields) {
    if (field %in% names(fields)) {
      value <- fields[[field]]
      if (!is.null(value)) {
        coerced <- suppressWarnings(as.double(value))
        if (length(value) != 1L || is.na(coerced) || !is.finite(coerced)) {
          cli::cli_abort(
            "{field} must be a single finite number",
            call = call
          )
        }
      }
    }
  }

  entry <- project$definitions$populations[[id]]
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

# Persist every session-injected `Population` to a CSV file next to the project,
# so it survives a reload. Called from the save path before the tree is written:
# for each `programmatic` entry whose object is in the runtime store, write
# `<populationsFolder>/<id>.csv` and rewrite the section entry to a `csv` source
# pointing at that file. A programmatic sentinel with no object in the store (an
# orphan read from disk) is left untouched.
#
# Returns the character vector of persisted ids; the caller drops these from the
# runtime store only after the whole save commits, so an abort partway through
# the tree write leaves the injected population recoverable (worst case is an
# orphan CSV file, never lost data). Mirrors `.persistProgrammaticObservedData`.
#
# @keywords internal
# @noRd
.persistProgrammaticPopulations <- function(self, private) {
  populations <- private$.getSection("populations")
  toPersist <- names(which(vapply(
    populations,
    function(e) {
      identical(e$type, "programmatic")
    },
    logical(1)
  )))
  # Keep only sentinels that actually have a backing object in the runtime store.
  toPersist <- Filter(
    function(id) !is.null(private$.programmaticPopulations[[id]]),
    toPersist
  )
  if (length(toPersist) == 0) {
    return(character(0))
  }

  populationsFolder <- self$paths$populationsFolder
  if (is.null(populationsFolder)) {
    cli::cli_abort(messages$populationPersistNoPopulationsFolder(toPersist[[
      1
    ]]))
  }

  # Rewrite the section in memory BEFORE writing any CSV, so a save that aborts
  # (an unsafe id, or a file basename that collides with an existing population
  # file) never overwrites a file: every write below lands on a known-safe path.
  fileNames <- stats::setNames(paste0(toPersist, ".csv"), toPersist)
  for (id in toPersist) {
    populations[[id]] <- .asPopulationSource(list(
      type = "csv",
      file = fileNames[[id]]
    ))
  }
  # Same checks the serializer runs, up front: each id a safe filename segment,
  # and no `<id>.csv` colliding with an existing population file's basename.
  lapply(names(populations), .validateDefinitionTreeKey, "population")
  existingFiles <- vapply(
    populations,
    function(e) if (is.null(e$file)) NA_character_ else basename(e$file),
    character(1)
  )
  collisions <- intersect(
    fileNames,
    existingFiles[setdiff(names(populations), toPersist)]
  )
  if (length(collisions) > 0) {
    cli::cli_abort(messages$populationPersistIdCollision(collisions))
  }

  if (!dir.exists(populationsFolder)) {
    dir.create(populationsFolder, recursive = TRUE, showWarnings = FALSE)
  }
  for (id in toPersist) {
    filePath <- .resolveProjectPath(fileNames[[id]], populationsFolder, "file")
    ospsuite::exportPopulationToCSV(
      population = private$.programmaticPopulations[[id]],
      filePath = filePath
    )
  }

  private$.setSection("populations", populations)
  toPersist
}
