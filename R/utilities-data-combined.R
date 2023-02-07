#' Generate DataCombined objects as defined in excel file
#'
#' @param file Full path of excel file
#' @param sheet Name of sheet to use - if `NULL` the first sheet will be used
#' @param dataCombinedNames Names of the DataCombined objects that will be created.
#' If `NULL` (default), all DataCombined objects specified in the excel sheet will
#' be created. If a DataCombined object with a given name does not exist, an error is thrown.
#' @param simulatedScenarios A list of simulated scenarios as returned by `runScenarios()`
#' @param observedData A list of `DataSet` objects
#' @param stopIfNotFound If TRUE (default), the function stops if any of the
#' simulated results or observed data are not found. If FALSE a warning is printed.
#'
#' @return A list of `DataCombined` objects
#'
#' @import tidyr
#'
#' @export
createDataCombinedFromExcel <- function(file, sheet = NULL, dataCombinedNames = NULL, simulatedScenarios = NULL, observedData = NULL, stopIfNotFound = TRUE) {
  validateIsString(file)
  validateIsString(sheet, nullAllowed = TRUE)

  dfDataCombined <- readExcel(path = file, sheet = sheet %||% 1)

  if (!is.null(dataCombinedNames)) {
    dfDataCombined <- filter(dfDataCombined, DataCombinedName %in% dataCombinedNames)
  }
  dfDataCombined <- .validateDataCombinedFromExcel(dfDataCombined, simulatedScenarios, observedData, stopIfNotFound)

  # create named list of DataCombined objects
  dataCombinedList <- lapply(unique(dfDataCombined$DataCombinedName), \(name) {
    dataCombined <- DataCombined$new()
    # add data to DataCombined object
    # add simulated data
    simulated <- filter(dfDataCombined, DataCombinedName == name, dataType == "simulated")
    if (nrow(simulated) > 0) {
      for (j in 1:nrow(simulated)) {
        dataCombined$addSimulationResults(
          simulationResults = simulatedScenarios[[simulated[j, ]$scenario]]$results,
          quantitiesOrPaths = simulated[j, ]$path,
          groups = simulated[j, ]$group,
          names = simulated[j, ]$label
        )
      }
    }

    # add observed data
    observed <- filter(dfDataCombined, DataCombinedName == name, dataType == "observed")
    if (nrow(observed) > 0) {
      dataSets <- observedData[observed$dataSet]
      dataCombined$addDataSets(dataSets, names = observed$label, groups = observed$group)
    }
    return(dataCombined)
  })
  names(dataCombinedList) <- unique(dfDataCombined$DataCombinedName)

  # apply data transformations
  dfTransform <- filter(dfDataCombined, !is.na(xOffsets) | !is.na(yOffsets) | !is.na(xScaleFactors) | !is.na(yScaleFactors)) %>%
    replace_na(list(xOffsets = 0, yOffsets = 0, xScaleFactors = 1, yScaleFactors = 1))
  # Apply data transformations if specified in the excel file
  if (dim(dfTransform)[[1]] != 0) {
    apply(dfTransform, 1, \(row) {
      dataCombinedList[[row[["DataCombinedName"]]]]$setDataTransformations(
        forNames = row[["label"]], xOffsets = as.numeric(row[["xOffsets"]]), yOffsets = as.numeric(row[["yOffsets"]]),
        xScaleFactors = as.numeric(row[["xScaleFactors"]]), yScaleFactors = as.numeric(row[["yScaleFactors"]])
      )
    })
  }

  return(dataCombinedList)
}


#' Validate and process the 'DataCombined' sheet
#'
#' @param dfDataCombined Data frame created by reading the ' DataCombined' sheet
#' @param simulatedScenarios List of simulated scenarios as created by `runScenarios()`
#' @param observedData Observed data objects
#' @param stopIfNotFound if `TRUE`, throw an error if a simulated result of an
#' observed data are not found
#'
#' @return Processed `dfDataCombined`
#' @keywords internal
.validateDataCombinedFromExcel <- function(dfDataCombined, simulatedScenarios, observedData, stopIfNotFound) {
  # mandatory column label is empty - throw error
  missingLabel <- sum(is.na(dfDataCombined$label))
  if (missingLabel > 0) {
    stop(messages$missingLabel())
  }

  # mandatory column dataType is empty - throw error
  missingLabel <- sum(is.na(dfDataCombined$dataType))
  if (missingLabel > 0) {
    stop(messages$missingDataType())
  }

  # dataType == simulated, but no scenario defined - throw error
  missingLabel <- sum(is.na(dfDataCombined[dfDataCombined$dataType == "simulated", ]$scenario))
  if (missingLabel > 0) {
    stop(messages$missingScenarioName())
  }

  # dataType == simulated, but no path defined - throw error
  missingLabel <- is.na(dfDataCombined[dfDataCombined$dataType == "simulated", ]$path)
  if (sum(missingLabel) > 0) {
    stop(messages$stopNoPathProvided(dfDataCombined[missingLabel & dfDataCombined$dataType == "simulated", ]$DataCombinedName))
  }

  # dataType == observed, but no data set defined - throw error
  missingLabel <- is.na(dfDataCombined[dfDataCombined$dataType == "observed", ]$dataSet)
  if (sum(missingLabel) > 0) {
    stop(messages$stopNoDataSetProvided(dfDataCombined[missingLabel & dfDataCombined$dataType == "observed", ]$DataCombinedName))
  }

  # warnings for invalid data in plot definitions from excel
  # scenario not present in simulatedScenarios
  missingScenarios <- setdiff(setdiff(dfDataCombined$scenario, names(simulatedScenarios)), NA)
  if (length(missingScenarios) != 0) {
    if (stopIfNotFound) {
      stop(messages$stopInvalidScenarioName(missingScenarios))
    }
    warning(messages$warningInvalidScenarioName(missingScenarios))
    dfDataCombined <- dfDataCombined[!(dfDataCombined$scenario %in% missingScenarios), ]
  }
  # data set name not present in observedData
  missingDataSets <- setdiff(setdiff(dfDataCombined$dataSet, names(observedData)), NA)
  if (length(missingDataSets) != 0) {
    if (stopIfNotFound) {
      stop(messages$stopInvalidDataSetName(missingDataSets))
    }
    warning(messages$warningInvalidDataSetName(missingDataSets))
    dfDataCombined <- dfDataCombined[!(dfDataCombined$dataSet %in% missingDataSets), ]
  }

  return(dfDataCombined)
}
