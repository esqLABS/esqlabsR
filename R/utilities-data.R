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
#' @details Calculates mean and standard deviation of the yValues of the given `DataSet`
#' objects per xValue. Note that data points with LLOQ are not used. If xUnit or yUnit
#' are not the same in all data sets, the unit of the first data set is used, values are
#' converted accordingly.
#' @return A `DataSet` object with yValues set to the mean of the original data sets'
#' yValues per xValue, yErrorValues set to their standard deviation, plus all meta data
#' that are equal in all data sets.
#' @export
calculateMeans <- function(dataSets) {
  df <- dataSetToDataFrame(dataSets)
  meanDataSet <- DataSet$new()

  # check if all xDimensions are the same
  xDimension <- unique(df$xDimension)
  if (length(xDimension) > 1) {
    stop(messages$errorDimensionsDoNotMatch("xDimension"))
  }
  meanDataSet$xDimension <- xDimension

  # check if all yDimensions are the same
  yDimension <- unique(df$yDimension)
  if (length(yDimension) > 1) {
    stop(messages$errorDimensionsDoNotMatch("yDimension"))
  }
  meanDataSet$yDimension <- yDimension

  # remove data points with lloq
  df <- df[is.na(df$lloq),]

  # check if xUnits match, if not: convert to all values to xUnit of first data set
  xUnit <- unique(df$xUnit)
  if (length(xUnit) > 1) {
    xUnit <- xUnit[1]
    df$xValues <- mapply(function(x, y) {
      toUnit(xDimension,targetUnit=xUnit, values = x, sourceUnit= y)
    },
    df$xValues, df$xUnit)
  }
  meanDataSet$xUnit <- xUnit

  # check if yUnits match, if not: convert all values to yUnit of first data set
  yUnit <- unique(df$yUnit)
  if (length(yUnit) > 1) {
    yUnit <- yUnit[1]
    df$yValues <- mapply(function(x, y) {
        toUnit(yDimension,targetUnit=yUnit, values = x, sourceUnit= y)
      },
      df$yValues, df$yUnit)
  }
  meanDataSet$yUnit <- yUnit

  yMeans <- tapply(df[,"yValues"], df[,"xValues"], mean)
  yError <- tapply(df[,"yValues"], df[,"xValues"], sd)

  meanDataSet$setValues(xValues = as.numeric(names(yMeans)), yValues = yMeans, yErrorValues = yError)
  meanDataSet$yErrorType <- DataErrorType$ArithmeticStdDev

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
  meanDataSet$name <- "Mean"

  return(meanDataSet)
}
