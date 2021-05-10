messages <- ospsuite:::messages
messages$errorWrongParamsXLSStructure <- function(filePath, optionalMessage = NULL) {
  paste0("Loading parameter values from XLS failed, the file '", filePath, "' has wrong structure!
    The file should consist of columns 'Container Path', 'Parameter Name', 'Value', and 'Units'. ", optionalMessage)
}
messages$errorWrongPopCharXLSStructure <- function(filePath, optinalMessage = NULL) {
  paste0("Loading population characteristics from XLS failed, the file ", filePath, " has wrong structure!
    The file should consist of columns'PopulationName', 'Species','Population','NrIndiv', '% female',
    'Weight_min', 'Weight_max', 'Height_min', 'Height_max','Age_min', 'Age_max', 'BMI_min', 'BMI_max'. ", optionalMessage)
}
messages$errorWrongAdditionalParams <- function(optionalMessage = NULL) {
  paste0("Wrong argument 'additionalParams'! Must be a list containing lists 'paths', 'values', and 'units' ", optionalMessage)
}
messages$errorCouldNotCompareParameters <- function(parameter1, parameter2, optionalMessage = NULL) {
  paste0("Could not compare parameters with paths '", parameter1$path, "' and '", parameter2$path, "'. ", optionalMessage)
}
messages$errorEnumPutListMultipleKeys <- function() {
  paste0("Trying to put multiple keys, but only one key is allowed!")
}
messages$erroFileNotFound <- function(filePath, optionalMessage = NULL) {
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
messages$errorOneArgumentNullButNotBoth <- function(name1, name2, optionalMessage = NULL) {
  paste0("Either both arugments ", name1, ", ", name2, " must be NULL or no, but
           only one argument is NULL.", optionalMessage)
}
messages$errorValuesAreNotPositive <- function(values, optionalMessage = NULL) {
  paste("All values must be positive or 0, but they are not! Values are: ", paste(as.character(values), collapse = ", "), optionalMessage)
}
messages$errorWrongLength <- function(object, length, optionalMessage = NULL) {
  paste("Object `", object, "` must be of length ", length, " but it is not!")
}
messages$warningValueWithinThresholdNotExisting <- function(value, threshold, optionalMessage = NULL) {
  paste("value `", value, "` not found in the array within the absolute threshold of  ", threshold, optionalMessage)
}
messages$errorWrongPopulationName <- function(populationName) {
    paste0("Population name ", populationName, " is not specified in the population file!")
  }
messages$errorWrongIndividualId <- function(individualId) {
    paste0("Individual with id ", individualId, " is not specified in the individual characteristics file!")
  }
