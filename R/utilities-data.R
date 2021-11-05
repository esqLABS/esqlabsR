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
  ospsuite:::validateIsString(c(dataConfiguration$dataFolder, dataConfiguration$dataFile, dataConfiguration$sheets))
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

#' Calculate mean and standard deviation for each data set of an excel sheet
#'
#' @param filePath full path of a xls/xlsx file containing at least the columns `Patient Id`,
#' `Fraction [%]` and `Error [%]`
#' @param sheet Name or number of the sheet. If `NULL` (default), the first sheet of the
#'   file is used.
#' @param columnNames vector of length 3, containing names of the columns corresponding
#' to `Time`, `Measurement` and `Error`, only necessary if they are named differently
#' in the file e.g. `Time [min]` instead of `Time`
#' @param nonGroupingCols these columns are not used for grouping data into data sets for which
#' calculations are done.
#' @param saveToOriginalFile should the calculated means be written to the original file?
#' @details Computes mean and standard deviation of `Measurement` of each subset of data
#' given by all combinations of values for the other columns (except for `nonGroupingCols`)
#' Results are written to a new file in the original folder per default, if the results shall
#' be appended to the original file and sheet, the argument `saveToOriginalFile` has to be
#' set to TRUE. Note that this may take significantly longer as every sheet from the original
#' file has to be read in first.
#'
#' @export
calculateMeans <- function(filePath, sheet = NULL, columnNames = c("Time", "Measurement", "Error"),
                           nonGroupingCols = c("Study Id", "Subject Id"), saveToOriginalFile = FALSE) {
  validateFileExists(filePath)
  validateLength(columnNames, 3)

  if (saveToOriginalFile) {
    if (is.null(sheet)) sheet <- readxl::excel_sheets(filePath)[1]
    # if the results shall be appended to the original file, all sheets have to be read in first
    # because there is no option to modify files with the writexl package
    fileContent <- sapply(readxl::excel_sheets(filePath), readExcel,
      path = filePath, USE.NAMES = TRUE, simplify = FALSE
    )
    data <- fileContent[[sheet]]
  } else {
    data <- readExcel(path = filePath, sheet = sheet)
  }

  if (!all(columnNames %in% names(data))) {
    stop(messages$errorColumnNamesNotInFile(filePath, setdiff(columnNames, names(data))))
  }
  if (nrow(data) == 0) {
    stop(messages$errorNoFileContents(filePath))
  }

  dataSets <- data[, !names(data) %in% c(columnNames[-1], nonGroupingCols)]

  if (ncol(dataSets) == 0) {
    stop(messages$errorNoGroupingColumns())
  }

  # dataframe with all combinations of column values
  dataSets <- dataSets[!duplicated(dataSets), ]
  means <- c()
  sds <- c()
  for (i in 1:nrow(dataSets)) {
    # filter data for combination of column values
    dataFiltered <- merge(data, dataSets[i, ])
    # compute mean and standard deviation for filtered data
    means <- c(means, mean(dataFiltered[[columnNames[2]]]))
    sds <- c(sds, sd(dataFiltered[[columnNames[2]]]))
  }

  # column 'Measurement'
  dataSets[[columnNames[2]]] <- means
  # column 'Error'
  dataSets[[columnNames[3]]] <- sds
  dataSets[setdiff(names(data), names(dataSets))] <- NA
  dataSets <- dataSets[names(data)]

  if (saveToOriginalFile) {
    data <- rbind(data, dataSets)
    fileContent[[sheet]] <- data
    writexl::write_xlsx(fileContent, path = filePath)
  } else {
    writexl::write_xlsx(dataSets, path = paste0(tools::file_path_sans_ext(filePath), "_mean.xlsx"))
  }
}
