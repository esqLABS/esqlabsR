messages <- ospsuite.utils::messages

messages$errorWrongXLSStructure <- function(filePath, expectedColNames, optionalMessage = NULL) {
  paste0("Loading from XLS failed, the file '", filePath, "' has wrong structure!
    The file should contain columns '", paste0(expectedColNames, collapse = ", "), "'", optionalMessage)
}

messages$errorCouldNotCompareParameters <- function(parameter1, parameter2, optionalMessage = NULL) {
  paste0("Could not compare parameters with paths '", parameter1$path, "' and '", parameter2$path, "'. ", optionalMessage)
}

messages$errorEnumPutListMultipleKeys <- function() {
  paste0("Trying to put multiple keys, but only one key is allowed!")
}

messages$errorDistributionNotSupported <- function(string) {
  paste0("The distribution '", string, "' is not supported. Supported distributions are listed in `Distributions`.")
}

messages$errorWrongPopulationName <- function(populationName) {
  paste0("Population name ", populationName, " is not specified in the population file!")
}

messages$errorWrongIndividualId <- function(individualId) {
  paste0("Individual with id ", individualId, " is not specified in the individual characteristics file!")
}

messages$warningValueWithinThresholdNotExisting <- function(value, threshold, optionalMessage = NULL) {
  paste("value `", value, "` not found in the array within the absolute threshold of  ", threshold, optionalMessage)
}

messages$errorInvalidMeanMethod <- function() {
  "Invalid value for argument `method`, supported values are `arithmetic` or `geometric`"
}

messages$errorOutputMolWeightNeeded <- function() {
  "`outputMolWeight` can not be `NULL` when data sets have different molWeights"
}

messages$wrongParametersStructure <- function(argumentName) {
  paste0("Argument '", argumentName, "' has wrong structure. Expected is a named list with three vectors
  `paths` representing full parameter paths, `values` with numerical values of the
  parameters, and `units' representing the units the values are in. All three
  vectors must have the same length")
}

messages$valueShouldNotBeNegative <- function(parameterName, value) {
  paste0(parameterName, " must be a positive numerical value, but the value is ", value)
}

messages$nrOfColorsShouldBePositive <- function(nrOfColors) {
  paste0("nrOfColors must be positive, value ", nrOfColors, " is not valid!")
}

messages$wrongSimulationType <- function() {
  "Wrong value for 'simulationType'! Accepted values are 'Individual and 'Population'"
}

messages$noPKDataToWrite <- function() {
  "`pkDataFilePath` argument is specified, but there is no PK parameters data to write to spreadsheets."
}

messages$cannotGetMoleculeFromQuantity <- function(quantityPath, optionalMessage = NULL) {
  paste0("Could not retrieve molecule name for the quantity with the path '", quantityPath, "'. ", optionalMessage)
}

messages$scenarioConfigurationNameNotFoundWhenReading <- function(scenarioName) {
  return(paste0("readScenarioDefinition: Scenario '", scenarioName, "' is not specified!"))
}

messages$warningInvalidScenarioName <- function(scenarioNames, file) {
  paste0("The following scenarios from ", file, " are not present in `simulatedScenarios`: ",
         paste(scenarioNames, collapse = ", "))
}

messages$warningInvalidDataSetName <- function(dataSetNames, file) {
  paste0("The following scenarios from ", file, " are not present in `observedData`: ",
         paste(dataSetNames, collapse = ", "))
}

messages$warningInvalidPlotID <- function(plotIDs, plotGridTitle) {
  paste0("The plots with plotIDs ", paste(plotIDs, collapse = ", "), " could not be added to plot grid `",
         plotGridTitle, "`. Please check if they are defined in sheet `plotConfiguration` and data is added in sheet `DataCombined`.")
}
