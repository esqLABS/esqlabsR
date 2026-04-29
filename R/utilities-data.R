# Numeric parsing helpers ----

#' Convert string to numeric
#'
#' @param string A string or a list of strings to be converted to numeric values
#' @param lloqMode How to treat entries below LLOQ, i.e., of a form "<2":
#'   `LLOQ/2` (default): return the number divided by 2, `LLOQ`: return the
#'   numerical value, `ZERO`: return 0, `ignore`: return `NA`
#' @param uloqMode How to treat entries above ULOQ, i.e., of a form ">2":
#'   `ULOQ`: return the numerical value, `ignore`: return `NA`
#'
#' @details Tries to convert each string to a numeric with `as.numeric()`. If
#'   any conversion fails and returns `NA`, the value is tested for being a
#'   LLOQ- or a ULOQ value, i.e., of a form "<2" or ">2", respectively. If this
#'   is a case, the returned value is defined by the parameters `lloqMode` and
#'   `uloqMode`. In any other case where the string cannot be converted to a
#'   numeric, `NA` is returned.
#' @returns A numeric value or a list of numeric values
#' @export
stringToNum <- function(
  string,
  lloqMode = LLOQMode$`LLOQ/2`,
  uloqMode = ULOQMode$ULOQ
) {
  # Input validations
  validateEnumValue(lloqMode, LLOQMode)
  validateEnumValue(uloqMode, ULOQMode)
  # Remove all whitespaces
  string <- gsub(" ", "", string, fixed = TRUE)
  # Attempt to convert all passed values to numeric
  numVals <- suppressWarnings(as.numeric(string))

  # If any values could not be interpreted and were coerced to NA, decide what
  # to do (e.g. LLOQ and ULOQ treatment)
  naVals <- is.na(numVals)
  if (any(naVals)) {
    for (idx in which(naVals)) {
      # If the value in the original string is NA, skip
      if (is.na(string[[idx]])) {
        next
      }
      # CHECK FOR LLOQ
      if (substring(string[[idx]], first = 1, last = 1) == "<") {
        # Transform the value that follow the "<" character
        value <- suppressWarnings(as.numeric(substring(
          string[[idx]],
          first = 2
        )))
        # If value is NA (could not convert to numeric), continue, as the output
        # should be NA
        if (is.na(value)) {
          next
        }
        switch(
          lloqMode,
          "LLOQ/2" = {
            numVals[[idx]] <- value / 2
          },
          "LLOQ" = numVals[[idx]] <- value,
          # set all data points with lloq to 0
          "ZERO" = numVals[[idx]] <- 0,
          # remove data points with lloq
          "ignore" = numVals[[idx]] <- NA
        )
      }

      # CHECK FOR ULOQ
      if (substring(string[[idx]], first = 1, last = 1) == ">") {
        # Transform the value that follow the "<" character
        value <- suppressWarnings(as.numeric(substring(
          string[[idx]],
          first = 2
        )))
        # If value is NA (could not convert to numeric), continue, as the output
        # should be NA
        if (is.na(value)) {
          next
        }
        switch(
          uloqMode,
          "ULOQ" = numVals[[idx]] <- value,
          # remove data points with lloq
          "ignore" = numVals[[idx]] <- NA
        )
      }
    }
  }
  return(numVals)
}

# DataSet aggregation ----

#' Calculate mean and standard deviation for the yValues of the given `DataSet`
#' objects
#'
#' @param dataSets list of `DataSet` objects
#' @param method method for calculating the mean and standard deviation - either
#'   `arithmetic` (default) or `geometric`
#' @param lloqMode how to treat data points below LLOQ if LLOQ is given -
#'   `LLOQ/2` (default): use as given (since `DataSet` stores values below LLOQ
#'   as `LLOQ/2`), `LLOQ`: set value to LLOQ value, `ZERO`: set value to 0,
#'   `ignore`: do not use data points for mean calculation
#' @param outputXunit xUnit of output data set, if `NULL` (default) xUnit of the
#'   first data set will be used
#' @param outputYunit yUnit of output data set, if `NULL` (default) yUnit of the
#'   first data set will be used
#' @param outputMolWeight molWeight of output data set in `g/mol` - obligatory
#'   when initial data sets have differing molWeight values
#' @details Calculates mean and standard deviation of the yValues of the given
#'   `DataSet` objects per xValue. The meta data of the returned `DataSet`
#'   consists of all meta data that are equal in all initial data sets. Its LLOQ
#'   is the mean LLOQ value of all data sets which have an LLOQ set, e.g. if
#'   dataSet1 has LLOQ 1, dataSet2 has LLOQ 3 and dataSet3 has no LLOQ, then 2
#'   is used for the returned `DataSet`. The LLOQ of the returned `DataSet` is
#'   the arithmetic mean of LLOQ values of all `DataSet`s
#' @returns A single `DataSet` object
#' @export
calculateMeanDataSet <- function(
  dataSets,
  method = "arithmetic",
  lloqMode = LLOQMode$`LLOQ/2`,
  outputXunit = NULL,
  outputYunit = NULL,
  outputMolWeight = NULL
) {
  validateIsOfType(dataSets, "DataSet")
  if (!any(c("arithmetic", "geometric") == method)) {
    stop(messages$errorInvalidMeanMethod())
  }
  validateEnumValue(lloqMode, LLOQMode)

  meanDataSet <- ospsuite::DataSet$new(name = "Mean")
  df <- ospsuite::dataSetToDataFrame(dataSets)
  # If df is empty, return an empty mean data set
  if (isEmpty(df)) {
    meanDataSet$addMetaData(name = "Subject ID", value = "mean")
    return(meanDataSet)
  }

  molWeights <- unique(df$molWeight)
  # molWeight is specified by user - use as specified
  if (!is.null(outputMolWeight)) {
    meanDataSet$molWeight <- outputMolWeight
  } else if (length(molWeights) > 1) {
    # molWeight is not specified by user and molWeights of data sets differ -
    # error
    stop(messages$errorOutputMolWeightNeeded())
  } else if (!is.na(molWeights)) {
    # molWeight is not specified by user and all molWeights are equal and not NULL -
    # take this value
    meanDataSet$molWeight <- molWeights
  }

  # set units and dimensions to those of first data set if not specified otherwise
  # outputXunit and outputYunit
  if (is.null(outputXunit)) {
    meanDataSet$xDimension <- df$xDimension[1]
    meanDataSet$xUnit <- df$xUnit[1]
  } else {
    meanDataSet$xDimension <- ospsuite::getDimensionForUnit(outputXunit)
    meanDataSet$xUnit <- outputXunit
  }

  if (is.null(outputYunit)) {
    meanDataSet$yDimension <- df$yDimension[1]
    meanDataSet$yUnit <- df$yUnit[1]
  } else {
    meanDataSet$yDimension <- ospsuite::getDimensionForUnit(outputYunit)
    meanDataSet$yUnit <- outputYunit
  }

  # adjust yValues as specified by lloqMode argument
  ind <- !is.na(df$lloq) & df$yValues < df$lloq
  switch(
    lloqMode,
    # nothing to do for LLOQ/2
    "LLOQ/2" = {
    },
    # set all data points with lloq that are smaller than it to value of lloq
    "LLOQ" = df[ind, "yValues"] <- df[ind, "lloq"],
    # set all data points with lloq to 0
    "ZERO" = df[ind, "yValues"] <- 0,
    # remove data points with lloq
    "ignore" = df <- df[!ind, ]
  )

  # meanDataSet$LLOQ = arithmetic mean lloq of all data sets with lloq
  lloqMean <- suppressWarnings(mean(
    unlist(
      sapply(c(dataSets), function(x) {
        x$LLOQ
      }),
      use.names = FALSE
    ),
    na.rm = TRUE
  ))
  if (!is.na(lloqMean)) {
    meanDataSet$LLOQ <- lloqMean
  }

  # convert xValues to same unit
  df$xValues <- mapply(
    function(vals, unit) {
      ospsuite::toUnit(
        quantityOrDimension = meanDataSet$xDimension,
        targetUnit = meanDataSet$xUnit,
        values = vals,
        sourceUnit = unit
      )
    },
    df$xValues,
    df$xUnit
  )

  # convert yValues to same unit
  df$yValues <- mapply(
    function(vals, unit, mw) {
      ospsuite::toUnit(
        quantityOrDimension = meanDataSet$yDimension,
        targetUnit = meanDataSet$yUnit,
        values = vals,
        sourceUnit = unit,
        molWeight = mw,
        molWeightUnit = "g/mol"
      )
    },
    df$yValues,
    df$yUnit,
    df$molWeight
  )

  # calculate means and standard deviations according to chosen method
  switch(
    method,
    arithmetic = {
      yMeans <- tapply(df[["yValues"]], df[["xValues"]], mean)
      yError <- tapply(df[["yValues"]], df[["xValues"]], stats::sd)
      meanDataSet$setValues(
        xValues = as.numeric(names(yMeans)),
        yValues = yMeans,
        yErrorValues = yError
      )
      meanDataSet$yErrorType <- ospsuite::DataErrorType$ArithmeticStdDev
    },
    geometric = {
      yMeans <- tapply(df[["yValues"]], df[["xValues"]], geomean)
      yError <- tapply(df[["yValues"]], df[["xValues"]], geosd)
      meanDataSet$setValues(
        xValues = as.numeric(names(yMeans)),
        yValues = yMeans,
        yErrorValues = yError
      )
      meanDataSet$yErrorType <- ospsuite::DataErrorType$GeometricStdDev
    }
  )

  # add all meta that are equal in every data set
  metaDataNames <- Reduce(
    intersect,
    lapply(c(dataSets), function(x) {
      names(x$metaData)
    })
  )
  for (name in metaDataNames) {
    value <- Reduce(
      intersect,
      sapply(
        c(dataSets),
        function(x) {
          x$metaData[[name]]
        },
        simplify = FALSE
      )
    )
    if (length(value) != 0) {
      meanDataSet$addMetaData(name = name, value = value)
    }
  }

  meanDataSet$addMetaData(name = "Subject ID", value = "mean")

  return(meanDataSet)
}

# LLOQ / ULOQ enums ----

#' Possible entries for the `lloqMode` argument of `calculateMeans()`
#' @export
LLOQMode <- enum(list("LLOQ/2", "LLOQ", "ZERO", "ignore"))

#' Possible modes to treat values above the upper limit of quantification.
#' @export
ULOQMode <- enum(list("ULOQ", "ignore"))

# Observed data loading and CRUD ----

.resolveDataPath <- function(entryValue, projectDefault, dataFolder) {
  if (!is.null(entryValue)) {
    file.path(dataFolder, entryValue)
  } else {
    projectDefault
  }
}

#' Load observed data declared in a Project
#'
#' Reads the `observedData` declarations from a `Project` object and returns
#' the corresponding `DataSet` objects. Supports multiple source types:
#' - **excel**: loaded via the configured data importer
#' - **pkml**: loaded directly from PKML files
#' - **script**: R scripts are sourced and must return DataSet or list of DataSet objects
#' - **programmatic**: DataSets added via `project$addObservedData()`
#'
#' @param project A `Project` object (see [loadProject()]).
#' @return A named list of `ospsuite::DataSet` objects. Empty list if no
#'   observed data is declared.
#' @examples
#' \dontrun{
#' project <- loadProject(exampleProjectPath())
#' dataSets <- loadObservedData(project)
#' }
#' @export
loadObservedData <- function(project) {
  validateIsOfType(project, "Project")

  if (is.null(project$observedData)) {
    return(list())
  }

  allDataSets <- list()
  for (entry in project$observedData) {
    dataSets <- switch(
      entry$type,
      "excel" = {
        filePath <- file.path(project$dataFolder, entry$file)
        importerPath <- file.path(
          project$dataFolder,
          entry$importerConfiguration
        )
        importerConfig <- ospsuite::loadDataImporterConfiguration(
          configurationFilePath = importerPath
        )
        importerConfig$sheets <- unlist(entry$sheets)
        ospsuite::loadDataSetsFromExcel(
          xlsFilePath = filePath,
          importerConfigurationOrPath = importerConfig,
          importAllSheets = FALSE
        )
      },
      "pkml" = {
        filePath <- file.path(project$dataFolder, entry$file)
        ds <- ospsuite::loadDataSetFromPKML(filePath = filePath)
        stats::setNames(list(ds), ds$name)
      },
      "script" = {
        filePath <- file.path(project$dataFolder, entry$file)
        if (!file.exists(filePath)) {
          stop(messages$scriptFileNotFound(filePath))
        }
        cli::cli_inform(c(
          "i" = "Sourcing observed-data script: {.path {filePath}}"
        ))
        result <- source(filePath, local = TRUE)$value
        if (inherits(result, "DataSet")) {
          stats::setNames(list(result), result$name)
        } else if (
          is.list(result) && all(sapply(result, inherits, "DataSet"))
        ) {
          result
        } else {
          stop(messages$scriptWrongReturnType(filePath, class(result)[[1]]))
        }
      },
      "programmatic" = {
        NULL
      }
    )
    allDataSets <- c(allDataSets, dataSets)
  }

  # Add programmatic DataSets (keyed by dataSet$name)
  allDataSets <- c(allDataSets, project$.getProgrammaticDataSets())

  # Cache the names
  project$.cacheObservedDataNames(names(allDataSets))

  allDataSets
}

#' Get names of all observed data in a Project
#'
#' Returns the names of all DataSets that would be returned by
#' `loadObservedData()`. On first call, this loads the data to discover names;
#' subsequent calls return cached names unless the cache is invalidated.
#'
#' @param project A `Project` object (see [loadProject()]).
#' @return A character vector of DataSet names.
#' @examples
#' \dontrun{
#' project <- loadProject(exampleProjectPath())
#' getObservedDataNames(project)
#' }
#' @export
getObservedDataNames <- function(project) {
  validateIsOfType(project, "Project")

  cached <- project$.getObservedDataNamesCache()
  if (!is.null(cached)) {
    return(cached)
  }

  # Load to populate cache
  loadObservedData(project)
  project$.getObservedDataNamesCache()
}

#' Add observed data to a Project
#'
#' @description Add an observedData entry. Accepts either a `DataSet`
#' (creates a `type="programmatic"` entry keyed by `dataSet$name`) or a
#' configuration list with `type` field ("excel", "pkml", or "script")
#' plus source-specific fields.
#'
#' @param project A `Project` object.
#' @param entry Either a `DataSet` object or a configuration list.
#' @returns The `project` object, invisibly.
#' @export
#' @family observedData
addObservedData <- function(project, entry) {
  validateIsOfType(project, "Project")

  if (inherits(entry, "DataSet")) {
    name <- entry$name
    existingNames <- getObservedDataNames(project)
    if (name %in% existingNames) {
      stop(messages$observedDataNameExists(name))
    }
    project$.addProgrammaticDataSet(name, entry)
    project$.appendObservedDataNameCache(name)
    newEntry <- list(type = "programmatic", name = name)
    project$observedData <- c(project$observedData, list(newEntry))
    project$.markModified()
    cli::cli_inform(c(
      "i" = paste0(
        "For reproducibility, consider declaring this DataSet via a script ",
        "in your Project.json using the observedData field with ",
        "type = \"script\" and file = \"scripts/your_script.R\"."
      )
    ))
  } else if (is.list(entry)) {
    if (is.null(entry$type)) {
      stop(messages$observedDataConfigMissingType())
    }
    validTypes <- c("excel", "pkml", "script")
    if (!(entry$type %in% validTypes)) {
      stop(messages$observedDataInvalidType(entry$type, validTypes))
    }
    project$.invalidateObservedDataNamesCache()
    project$observedData <- c(project$observedData, list(entry))
    project$.markModified()
  } else {
    stop(messages$observedDataInvalidEntry())
  }
  invisible(project)
}

#' Remove observed data from a Project
#'
#' @description Removes by DataSet name (for `type="programmatic"` entries)
#' or by `file` basename (for `type="excel"/"pkml"/"script"` entries).
#'
#' @param project A `Project` object.
#' @param name DataSet name or config entry file basename.
#' @returns The `project` object, invisibly.
#' @export
#' @family observedData
removeObservedData <- function(project, name) {
  validateIsOfType(project, "Project")
  if (
    !is.character(name) ||
      length(name) != 1 ||
      is.na(name) ||
      nchar(name) == 0
  ) {
    stop("name must be a non-empty string")
  }

  progDS <- project$.getProgrammaticDataSets()
  if (name %in% names(progDS)) {
    project$.removeProgrammaticDataSet(name)
    # Match by the name stamped on the sentinel; falls back to the first
    # programmatic entry for older configurations whose sentinels predate
    # the `name` field.
    matchIdx <- which(vapply(
      project$observedData,
      function(e) identical(e$type, "programmatic") && identical(e$name, name),
      logical(1)
    ))
    if (length(matchIdx) == 0) {
      matchIdx <- which(vapply(
        project$observedData,
        function(e) identical(e$type, "programmatic"),
        logical(1)
      ))
    }
    if (length(matchIdx) > 0) {
      project$observedData <- project$observedData[-matchIdx[[1]]]
    }
    project$.invalidateObservedDataNamesCache()
    project$.markModified()
    return(invisible(project))
  }

  matchIdx <- which(vapply(
    project$observedData,
    function(e) {
      !is.null(e$file) && identical(basename(e$file), name)
    },
    logical(1)
  ))

  if (length(matchIdx) == 0) {
    cli::cli_warn(messages$observedDataNotFound(name))
    return(invisible(project))
  }

  project$observedData <- project$observedData[-matchIdx[[1]]]
  project$.invalidateObservedDataNamesCache()
  project$.markModified()
  invisible(project)
}
