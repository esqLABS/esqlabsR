#' Read time-values data from excel file
#'
#' @param dataConfiguration An object of `DataConfiguration`
#' @details The methods reads time-values data from the properly defined excel
#'   sheet and creates `XYData` objects according to the groupings. Each sheet
#'   in `DataConfiguration$sheets` is split according to columns listed in
#'   `DataConfiguration$columnsToSplitBy`. The output structure is a nested list
#'   with levels corresponding to the groupings.
#'
#' @return A (nested) list of `XYData` objects
#' @import ospsuite
#' @export
readOSPSTimeValues <- function(dataConfiguration) {
  validateIsString(c(dataConfiguration$dataFolder, dataConfiguration$dataFile, dataConfiguration$sheets))
  filePath <- file.path(dataConfiguration$dataFolder, dataConfiguration$dataFile)
  validateFileExists(filePath)

  observedData <- list()
  for (sheet in dataConfiguration$dataSheets) {
    data <- readExcel(path = filePath, sheet = sheet)
    allFactors <- list()
    groupings <- c()
    # Split the data by a column only if it contains non-NA values
    for (columnName in dataConfiguration$columnsToSplitBy) {
      if (length(data[[columnName]]) > 0 && all(!is.na(data[[columnName]]))) {
        groupings <- c(groupings, columnName)
        allFactors <- append(allFactors, list(data[[columnName]]))
      }
    }
    data <- split(data, allFactors, drop = TRUE)

    # Create an XYData object for each group
    for (groupIdx in seq_along(data)) {
      group <- data[[groupIdx]]
      groupName <- names(data)[[groupIdx]]
      xVals <- group[[dataConfiguration$XValuesColumn]]
      yVals <- group[[dataConfiguration$YValuesColumn]]
      yErrorVals <- group[[dataConfiguration$YErrorColumn]]

      # Parse Dimensions and Units
      xName <- colnames(group)[[dataConfiguration$XValuesColumn]]
      yName <- colnames(group)[[dataConfiguration$YValuesColumn]]
      yErrorName <- colnames(group)[[dataConfiguration$YErrorColumn]]


      # Get name of the dimension before the unit
      xDim <- strsplit(xName, "\\ ?\\[")[[1]][[1]]
      # The unit is the second entry
      xUnit <- strsplit(xName, "\\ ?\\[")[[1]][[2]]
      # Remove the trailing ']'
      xUnit <- gsub(pattern = "]", replacement = "", xUnit, fixed = TRUE)

      yDim <- strsplit(yName, "\\ ?\\[")[[1]][[1]]
      yUnit <- strsplit(yName, "\\ ?\\[")[[1]][[2]]
      yUnit <- gsub(pattern = "]", replacement = "", yUnit, fixed = TRUE)

      yErrorUnit <- strsplit(yErrorName, "\\ ?\\[")[[1]][[2]]
      yErrorUnit <- gsub(pattern = "]", replacement = "", yErrorUnit, fixed = TRUE)

      timeValues <- XYData$new(stringToNum(xVals), stringToNum(yVals), label = paste(sheet, groupName, sep = "."), yError = stringToNum(yErrorVals))
      timeValues$xDimension <- xDim
      timeValues$xUnit <- xUnit
      timeValues$yDimension <- yDim
      timeValues$yUnit <- yUnit
      timeValues$yErrorUnit <- yErrorUnit
      timeValues$dataType <- XYDataTypes$Observed
      timeValues$setMetaData(name = "StudyId", value = group$`Study Id`[[1]])
      timeValues$setMetaData(name = "PatientId", value = group$`Study Id`[[1]])
      timeValues$setMetaData(name = "Organ", value = group$Organ[[1]])
      timeValues$setMetaData(name = "Compartment", value = group$Compartment[[1]])
      timeValues$setMetaData(name = "Species", value = group$Species[[1]])
      timeValues$setMetaData(name = "Gender", value = group$Gender[[1]])
      timeValues$setMetaData(name = "Molecule", value = group$Molecule[[1]])

      # If a molecule is specified, retrieve its molecular weight
      moleculeName <- timeValues$getAllMetaData()$Molecule
      if (!is.null(moleculeName) && !is.na(moleculeName)) {
        compoundProperties <- readExcel(
          path = file.path(dataConfiguration$dataFolder, dataConfiguration$compoundPropertiesFile), sheet = timeValues$getAllMetaData()$Molecule
        )
        mwIdx <- which(compoundProperties$`Parameter, [AdditionalParameter]` == "MW")
        mw <- compoundProperties$`Value [1,1]`[[mwIdx]]
        unit <- compoundProperties$`Unit [1,1]`[[mwIdx]]
        timeValues$MW <- as.numeric(mw)
      }

      timeValues$setMetaData(name = "GroupId", value = group$`Group Id`[[1]])

      # Some ugly piece of code to create a tree-like structure.
      # Don't even want to comment
      levelString <- unlist(lapply(groupings, function(x) {
        group[[x]][[1]]
      }), use.names = FALSE)
      levelString <- paste0("'", levelString, "'", collapse = "$")
      evalString <- paste0("observedData[[sheet]]$", levelString, " <- timeValues")
      eval(parse(text = evalString))
    }
  }
  return(observedData)
}

#' Convert string to numeric
#'
#' @param string A string or a list of strings to be converted to numeric values
#' @details Tries to convert each string to a numeric with `as.numeric`
#' If any conversion fails and returns an NA, the value is tested for being a LLOQ-value,
#' i.e., of a form "<2". If this is a case, the value is substituted by 0.
#' In any other case where the string cannot be converted to a numeric, an NA is returned.
#' @return A numeric value or a list of numeric values
#' @export
stringToNum <- function(string) {
  # Attempt to convert all passed values to numeric
  numVals <- as.numeric(string)

  # If any values could not be interpreted and were coerced to NA, decide what to do (e.g. LLOQ treatment)
  naVals <- is.na(numVals)
  if (any(naVals)) {
    for (idx in which(naVals)) {
      if (is.na(string[[idx]])) {
        next
      }
      # CHECK FOR LLOQ
      if (substring(string[[idx]], first = 1, last = 1) == "<") {
        numVals[[idx]] <- 0
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
  if (!method %in% c("arithmetic", "geometric")) {
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
      return
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
  })), na.rm = TRUE))
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

#' Load data from excel
#'
#' @description Loads data sets from excel. The excel file containing the data
#' must be located in the folder `scenarioConfiguration$projectConfiguration$dataFolder`
#' and be named `scenarioConfiguration$projectConfiguration$dataFile`.
#' Importer configuration file must be located in the same folder and named
#' `scenarioConfiguration$projectConfiguration$dataImporterConfigurationFile`.
#'
#' @param scenarioConfiguration Object of class `ScenarioConfiguration` containing
#' the necessary information.
#' @param sheets String or a list of strings defining which sheets to load.
#' If `NULL` (default), all sheets within the file are loaded.
#'
#' @return
#' A list of `DataSet` objects
#' @export
#'
#' @examples
#' \dontrun{
#' # Create default project and scenario configurations
#' projectConfiguration <- createDefaultProjectConfiguration()
#' scenarioConfiguration <- ScenarioConfiguration$new(projectConfiguration)
#' dataSets <- loadObservedData(scenarioConfiguration)
#' }
loadObservedData <- function(scenarioConfiguration, sheets = NULL) {
  importerConfiguration <- ospsuite::loadDataImporterConfiguration(
    configurationFilePath = file.path(
      scenarioConfiguration$projectConfiguration$dataFolder,
      scenarioConfiguration$projectConfiguration$dataImporterConfigurationFile
    )
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
    xlsFilePath = file.path(
      scenarioConfiguration$projectConfiguration$dataFolder,
      scenarioConfiguration$projectConfiguration$dataFile
    ),
    importerConfigurationOrPath = importerConfiguration,
    importAllSheets = importAllSheets
  )

  return(dataSets)
}
