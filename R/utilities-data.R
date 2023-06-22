#' Convert string to numeric
#'
#' @param string A string or a list of strings to be converted to numeric values
#' @param lloqMode How to treat entries below LLOQ, i.e., of a form "<2":
#'  `LLOQ/2` (default): return the number divided by 2,
#'  `LLOQ`: return the numerical value,
#'  `ZERO`: return 0,
#'  `ignore`: return `NA`
#' @param uloqMode How to treat entries above ULOQ, i.e., of a form ">2":
#'  `ULOQ`: return the numerical value,
#'  `ignore`: return `NA`
#'
#' @details Tries to convert each string to a numeric with `as.numeric()`.
#' If any conversion fails and returns `NA`, the value is tested for being a LLOQ-
#' or a ULOQ value, i.e., of a form "<2" or ">2", respectively. If this is a case,
#' the returned value is defined by the parameters `lloqMode` and `uloqMode`.
#' In any other case where the string cannot be converted to a numeric, `NA` is returned.
#' @return A numeric value or a list of numeric values
#' @export
stringToNum <- function(string, lloqMode = LLOQMode$`LLOQ/2`, uloqMode = ULOQMode$ULOQ) {
  # Input validations
  validateEnumValue(lloqMode, LLOQMode)
  validateEnumValue(uloqMode, ULOQMode)
  # Remove all whitespaces
  string <- gsub(" ", "", string, fixed = TRUE)
  # Attempt to convert all passed values to numeric
  numVals <- as.numeric(string)

  # If any values could not be interpreted and were coerced to NA, decide what to do (e.g. LLOQ and ULOQ treatment)
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
        value <- as.numeric(substring(string[[idx]], first = 2))
        # If value is NA (could not convert to numeric), continue, as the output
        # should be NA
        if (is.na(value)) {
          next
        }
        switch(lloqMode,
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
        value <- as.numeric(substring(string[[idx]], first = 2))
        # If value is NA (could not convert to numeric), continue, as the output
        # should be NA
        if (is.na(value)) {
          next
        }
        switch(uloqMode,
          "ULOQ" = numVals[[idx]] <- value,
          # remove data points with lloq
          "ignore" = numVals[[idx]] <- NA
        )
      }
    }
  }
  return(numVals)
}

#' Calculate mean and standard deviation for the yValues of the given `DataSet` objects
#'
#' @param dataSets list of `DataSet` objects
#' @param method method for calculating the mean and standard deviation - either
#'  `arithmetic` (default) or `geometric`
#' @param lloqMode how to treat data points below LLOQ if LLOQ is given - `LLOQ/2` (default):
#'  use as given (since `DataSet` stores values below LLOQ as `LLOQ/2`),
#'  `LLOQ`: set value to LLOQ value, `ZERO`: set value to 0, `ignore`:
#'  do not use data points for mean calculation
#' @param outputXunit xUnit of output data set, if `NULL` (default) xUnit of the first data set
#'  will be used
#' @param outputYunit yUnit of output data set, if `NULL` (default) yUnit of the first data set
#'  will be used
#' @param outputMolWeight molWeight of output data set in `g/mol` - obligatory when initial
#'  data sets have differing molWeight values
#' @details Calculates mean and standard deviation of the yValues of the given `DataSet`
#'  objects per xValue. The meta data of the returned `DataSet` consists of all meta
#'  data that are equal in all initial data sets. Its LLOQ is the mean LLOQ value of all
#'  data sets which have an LLOQ set, e.g. if dataSet1 has LLOQ 1, dataSet2 has LLOQ 3
#'  and dataSet3 has no LLOQ, then 2 is used for the returned `DataSet`.
#'  The LLOQ of the returned `DataSet` is the arithmetic mean of LLOQ values of
#'  all `DataSet`s
#' @return A single `DataSet` object
#' @export
calculateMeanDataSet <- function(dataSets, method = "arithmetic", lloqMode = LLOQMode$`LLOQ/2`,
                                 outputXunit = NULL, outputYunit = NULL, outputMolWeight = NULL) {
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
  switch(lloqMode,
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
  lloqMean <- suppressWarnings(mean(unlist(sapply(c(dataSets), function(x) {
    x$LLOQ
  }), use.names = FALSE), na.rm = TRUE))
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
    df$xValues, df$xUnit
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
    df$yValues, df$yUnit, df$molWeight
  )

  # calculate means and standard deviations according to chosen method
  switch(method,
    arithmetic = {
      yMeans <- tapply(df[["yValues"]], df[["xValues"]], mean)
      yError <- tapply(df[["yValues"]], df[["xValues"]], sd)
      meanDataSet$setValues(xValues = as.numeric(names(yMeans)), yValues = yMeans, yErrorValues = yError)
      meanDataSet$yErrorType <- ospsuite::DataErrorType$ArithmeticStdDev
    },
    geometric = {
      yMeans <- tapply(df[["yValues"]], df[["xValues"]], geomean)
      yError <- tapply(df[["yValues"]], df[["xValues"]], geosd)
      meanDataSet$setValues(xValues = as.numeric(names(yMeans)), yValues = yMeans, yErrorValues = yError)
      meanDataSet$yErrorType <- ospsuite::DataErrorType$GeometricStdDev
    }
  )

  # add all meta that are equal in every data set
  metaDataNames <- Reduce(intersect, lapply(c(dataSets), function(x) {
    names(x$metaData)
  }))
  for (name in metaDataNames) {
    value <- Reduce(intersect, sapply(c(dataSets), function(x) {
      x$metaData[[name]]
    }, simplify = FALSE))
    if (length(value) != 0) {
      meanDataSet$addMetaData(name = name, value = value)
    }
  }

  meanDataSet$addMetaData(name = "Subject ID", value = "mean")

  return(meanDataSet)
}

#' Possible entries for the `lloqMode` argument of `calculateMeans()`
#' @export
LLOQMode <- enum(list("LLOQ/2", "LLOQ", "ZERO", "ignore"))

#' Possible modes to treat values above the upper limit of quantification.
#' @export
ULOQMode <- enum(list("ULOQ", "ignore"))

#' Load data from excel
#'
#' @description Loads data sets from excel. The excel file containing the data
#' must be located in the folder `projectConfiguration$dataFolder`
#' and be named `projectConfiguration$dataFile`.
#' Importer configuration file must be located in the same folder and named
#' `projectConfiguration$dataImporterConfigurationFile`.
#'
#' @param projectConfiguration Object of class `ProjectConfiguration` containing
#' the necessary information.
#' @param sheets String or a list of strings defining which sheets to load.
#' If `NULL` (default), all sheets within the file are loaded.
#'
#' @return
#' A named list of `DataSet` objects, with names being the names of the data sets.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create default project configuration
#' projectConfiguration <- createDefaultProjectConfiguration()
#' dataSets <- loadObservedData(projectConfiguration)
#' }
loadObservedData <- function(projectConfiguration, sheets = NULL) {
  importerConfiguration <- ospsuite::loadDataImporterConfiguration(
    configurationFilePath = projectConfiguration$dataImporterConfigurationFile
  )
  validateIsString(sheets, nullAllowed = TRUE)
  # If sheets have been specified, import only those. Otherwise try to import all
  # sheets
  importAllSheets <- TRUE
  if (!is.null(sheets)) {
    importerConfiguration$sheets <- sheets
    importAllSheets <- FALSE
  }

  dataSets <- ospsuite::loadDataSetsFromExcel(
    xlsFilePath = projectConfiguration$dataFile,
    importerConfigurationOrPath = importerConfiguration,
    importAllSheets = importAllSheets
  )

  return(dataSets)
}

#' Load data from pkml
#'
#' @description Loads data sets that are exported as pkml. The files must be
#' located in the folder `projectConfiguration$dataFolder`, subfolder `pkml`.
#' and be named `projectConfiguration$dataFile`.
#'
#' @param projectConfiguration Object of class `ProjectConfiguration` containing
#' the necessary information.
#' @param obsDataNames String or a list of strings defining data sets to load
#' If `NULL` (default), all data sets located in the folder are loaded. Must not
#' contain the ".pkml" file extension.
#'
#' @return
#' A named list of `DataSet` objects, with names being the names of the data sets.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create default project configuration
#' projectConfiguration <- createDefaultProjectConfiguration()
#' dataSets <- loadObservedData(projectConfiguration)
#' }
loadObservedDataFromPKML <- function(projectConfiguration, obsDataNames = NULL) {
  ospsuite.utils::validateIsString(obsDataNames, nullAllowed = TRUE)
  # If data sets have been specified, import only those. Otherwise try to import all
  # files
  if (!is.null(obsDataNames)){
    allFiles <- paste0(obsDataNames, ".pkml")
  } else {
    allFiles <- list.files(path = file.path(projectConfiguration$dataFolder,
                                            "pkml"), pattern = "*.pkml")
  }

  dataSets <- lapply(allFiles, function(fileName){
    ospsuite::loadDataSetFromPKML(filePath = file.path(projectConfiguration$dataFolder,
                                                       "pkml",
                                                       fileName)
    )
  })
  names(dataSets) <- lapply(dataSets, function(x) {
    x$name
  })

  return(dataSets)
}
