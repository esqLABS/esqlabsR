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

messages$errorFileNotFound <- function(filePath, optionalMessage = NULL) {
  paste0("File '", filePath, "' could not be found!")
}

messages$errorEsqlabsRSettingNotFound <- function(settingName) {
  paste0("No global setting with the name '", settingName, "' exists. Available global settings are:\n", paste0(names(esqlabsEnv), collapse = ", "))
}

messages$errorDistributionNotSupported <- function(string) {
  paste0("The distribution '", string, "' is not supported. Supported distributions are listed in `Distributions`.")
}

messages$errorOutputPathNotFound <- function(string) {
  paste0("The output with the path '", string, "' is not found.")
}

messages$warningLabelNotInDataMapping <- function(string) {
  paste0("No xy-series with label ", string, " exists in the DataMapping. Nothing to remove")
}

messages$errorValuesAreNotPositive <- function(values, optionalMessage = NULL) {
  paste("All values must be positive or 0, but they are not! Values are: ", paste(as.character(values), collapse = ", "), optionalMessage)
}

messages$errorWrongLength <- function(object, length, optionalMessage = NULL) {
  paste("Object `", object, "` must be of length ", length, " but it is not!")
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

messages$errorMultipleMetaDataEntries <- function(optionalMessage = NULL) {
  paste("Can only set a single meta data entry at once", optionalMessage)
}

messages$errorInvalidMeanMethod <- function() {
  paste("Invalid value for argument `method`, supported values are `arithmetic` or `geometric`")
}

messages$errorOutputMolWeightNeeded <- function() {
  paste("`outputMolWeight` can not be `NULL` when data sets have different molWeights")
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
