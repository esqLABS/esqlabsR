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
#' @import ospsuite vctrs
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
      timeValues$setMetaData(name = "dataType", value = XYDataTypes$Observed)

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
#' @param treatLLOQ how to treat data points below LLOQ if LLOQ is given - `LLOQ/2` (default):
#'  use as given, `LLOQ`: set value to LLOQ value, `ZERO`: set value to 0, `ignore`:
#'  do not use data points for mean calculation
#' @param outputXunit xUnit of output data set, if `NULL` xUnit of the first data set
#'  will be used
#' @param outputYunit yUnit of output data set, if `NULL` yUnit of the first data set
#'  will be used
#' @param outputMolWeight molWeight of output data set in `g/mol` - obligatory when initial
#'  data sets have differing molWeight values
#' @details Calculates mean and standard deviation of the yValues of the given `DataSet`
#'  objects per xValue. The meta data of the returned `DataSet` consists of all meta
#'  data that are equal in all initial data sets. Its LLOQ is the mean LLOQ value of all
#'  data sets which have an LLOQ set, e.g. if dataSet1 has LLOQ 1, dataSet2 has LLOQ 3
#'  and dataSet3 has no LLOQ, then 2 is used for the returned `DataSet`
#' @return A single `DataSet` object
#' @export
calculateMeans <- function(dataSets, method = "arithmetic", treatLLOQ = TreatLLOQ$`LLOQ/2`,
                           outputXunit = NULL, outputYunit = NULL, outputMolWeight = NULL) {
  df <- ospsuite::dataSetToDataFrame(dataSets)
  meanDataSet <- ospsuite::DataSet$new()
  meanDataSet$name <- "Mean"

  molWeights <- unique(df$molWeight)
  if (!is.null(outputMolWeight)) {
    meanDataSet$molWeight <- outputMolWeight
  } else if (length(molWeights) > 1) {
    # error when outputMolWeight is NULL, but molWeights of data sets differ
    stop(messages$errorOutputMolWeightNeeded())
  } else if (!is.na(molWeights)) {
    # outputMolWeight is NULL, but all molWeights are equal and not NULL --> take this value
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

  # adjust yValues as specified by treatLLOQ argument
  ind <- !is.na(df$lloq) & df$yValues < df$lloq
  switch(treatLLOQ,
    # nothing to do for LLOQ/2
    "LLOQ/2" = return,
    # set all data points with lloq that are smaller than it to value of lloq
    LLOQ = df[ind, "yValues"] <- df[ind, "lloq"],
    # set all data points with lloq to 0
    ZERO = df[ind, "yValues"] <- 0,
    # remove data points with lloq
    ignore = df <- df[!ind, ],
    stop(messages$errorInvalidTreatLLOQ())
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
    function(vals, unit, mw) {
      ospsuite::toUnit(
        quantityOrDimension = meanDataSet$xDimension, targetUnit = meanDataSet$xUnit,
        values = vals, sourceUnit = unit, molWeight = mw, molWeightUnit = "g/mol"
      )
    },
    df$xValues, df$xUnit, df$molWeight
  )

  # convert yValues to same unit
  df$yValues <- mapply(
    function(vals, unit, mw) {
      ospsuite::toUnit(
        quantityOrDimension = meanDataSet$yDimension, targetUnit = meanDataSet$yUnit,
        values = vals, sourceUnit = unit, molWeight = mw, molWeightUnit = "g/mol"
      )
    },
    df$yValues, df$yUnit, df$molWeight
  )

  # calculate means and standard deviations according to chosen method
  switch(method,
    arithmetic = {
      yMeans <- tapply(df[, "yValues"], df[, "xValues"], mean)
      yError <- tapply(df[, "yValues"], df[, "xValues"], sd)
      meanDataSet$setValues(xValues = as.numeric(names(yMeans)), yValues = yMeans, yErrorValues = yError)
      meanDataSet$yErrorType <- ospsuite::DataErrorType$ArithmeticStdDev
    },
    geometric = {
      yMeans <- tapply(df[, "yValues"], df[, "xValues"], geomean)
      yError <- tapply(df[, "yValues"], df[, "xValues"], geosd)
      meanDataSet$setValues(xValues = as.numeric(names(yMeans)), yValues = yMeans, yErrorValues = yError)
      meanDataSet$yErrorType <- ospsuite::DataErrorType$GeometricStdDev
    },
    stop(messages$errorInvalidMeanMethod())
  )

  # add all meta that are equal in every data set
  # we are getting the meta data names by their position in the data frame returned by
  # ospsuite::dataSetToDataFrame() - this will have to be adapted if the number of default
  # columns changes there
  metaDataNames <- names(df)[-(1:12)]
  for (name in metaDataNames) {
    value <- unique(df[[name]])
    if (length(value) == 1) {
      meanDataSet$addMetaData(name = name, value = value)
    }
  }
  meanDataSet$addMetaData(name = "Subject ID", value = "mean")

  return(meanDataSet)
}

#' Possible entries for the `treatLLOQ` argument of `calculateMeans`
#' @export
TreatLLOQ <- ospsuite::enum(list("LLOQ/2", "LLOQ", "ZERO", "ignore"))
