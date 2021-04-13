#' Read time-values data from excel file
#'
#' @param dataConfiguration An object of \code{DataConfiguration}
#' @details The methods reads time-values data from the properly defined excel sheet and creates \code{OSPSTimeValues} objects
#' according to the groupings. Each sheet in \code{DataConfiguration$sheets} is split according to columns listed in
#' \code{DataConfiguration$columnsToSplitBy}. The output structure is a nested list with levels corresponding to the groupings.
#'
#' @return A (nested) list of \code{OSPSTimeValues} objects
#' @export
readOSPSTimeValues <- function(dataConfiguration) {
  validateIsString(c(dataConfiguration$dataFolder, dataConfiguration$dataFile, dataConfiguration$sheets))
  filePath <- file.path(dataConfiguration$dataFolder, dataConfiguration$dataFile)
  validateFileExists(filePath)

  observedData <- list()
  for (sheet in dataConfiguration$dataSheets) {
    data <- openxlsx::read.xlsx(xlsxFile = filePath, sheet = sheet)
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

    # Create an OSPSTimeValues object for each group
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


      # Get name of the dimension before the unit. Replace '.' by spaces
      xDim <- gsub(pattern = ".", replacement = " ", strsplit(xName, "\\.?\\[")[[1]][[1]], fixed = TRUE)
      # The unit is the second entry
      xUnit <- gsub(pattern = ".", replacement = " ", strsplit(xName, "\\.?\\[")[[1]][[2]], fixed = TRUE)
      # Remove the trailing ']'
      xUnit <- gsub(pattern = "]", replacement = "", xUnit, fixed = TRUE)

      yDim <- gsub(pattern = ".", replacement = " ", strsplit(yName, "\\.?\\[")[[1]][[1]], fixed = TRUE)
      yUnit <- gsub(pattern = ".", replacement = " ", strsplit(yName, "\\.?\\[")[[1]][[2]], fixed = TRUE)
      yUnit <- gsub(pattern = "]", replacement = "", yUnit, fixed = TRUE)

      yErrorUnit <- gsub(pattern = ".", replacement = " ", strsplit(yErrorName, "\\.?\\[")[[1]][[2]], fixed = TRUE)
      yErrorUnit <- gsub(pattern = "]", replacement = "", yErrorUnit, fixed = TRUE)

      timeValues <- OSPSTimeValues$new(xVals, stringToNum(yVals), label = paste(sheet, groupName, sep = "."), yError = stringToNum(yErrorVals))
      timeValues$xDimension <- xDim
      timeValues$xUnit <- xUnit
      timeValues$yDimension <- yDim
      timeValues$yUnit <- yUnit
      timeValues$yErrorUnit <- yErrorUnit
      timeValues$StudyId <- group$Study.Id[[1]]
      timeValues$PatientId <- group$PatientId[[1]]
      timeValues$Organ <- group$Organ[[1]]
      timeValues$Compartment <- group$Compartment[[1]]
      timeValues$Species <- group$Species[[1]]
      timeValues$Gender <- group$Gender[[1]]
      timeValues$Molecule <- group$Molecule[[1]]

      # If a molecule is specified, retrieve its molecular weight
      if (!is.na(timeValues$Molecule)) {
        compoundProperties <- openxlsx::read.xlsx(xlsxFile = file.path(dataConfiguration$dataFolder, dataConfiguration$compoundPropertiesFile), sheet = timeValues$Molecule)
        mwIdx <- which(compoundProperties$`Parameter,.[AdditionalParameter]` == "MW")
        mw <- compoundProperties$`Value.[1,1]`[[mwIdx]]
        unit <- compoundProperties$`Unit.[1,1]`[[mwIdx]]
        timeValues$MW <- as.numeric(mw)
      }

      timeValues$GroupId <- group$GroupId[[1]]
      timeValues$dataType <- XYDataTypes$Observed

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
#' @details Tries to convert each string to a numeric with \code{as.numeric}
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
