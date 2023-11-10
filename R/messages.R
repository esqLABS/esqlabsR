messages <- ospsuite.utils::messages

# Parameters structure####
messages$errorWrongXLSStructure <- function(filePath, expectedColNames, optionalMessage = NULL) {
  paste0(
    "Loading from XLS failed, the file '", filePath, "' has wrong structure!
    The file should contain columns '", paste0(expectedColNames, collapse = ", "), "'",
    optionalMessage
  )
}

messages$wrongParametersStructure <- function(argumentName) {
  paste0("Argument '", argumentName, "' has wrong structure. Expected is a named list with three vectors
  `paths` representing full parameter paths, `values` with numerical values of the
  parameters, and `units' representing the units the values are in. All three
  vectors must have the same length")
}

# Enum####
messages$errorEnumPutListMultipleKeys <- function() {
  paste0("Trying to put multiple keys, but only one key is allowed!")
}

# utilities-population####
messages$errorDistributionNotSupported <- function(string) {
  paste0("The distribution '", string, "' is not supported. Supported distributions are listed in `Distributions`.")
}

messages$errorWrongPopulationName <- function(populationName) {
  paste0("Population name ", populationName, " is not specified in the population file!")
}

# utilities-individual####
messages$errorWrongIndividualId <- function(individualId) {
  paste0("Individual with id ", individualId, " is not specified in the individual characteristics file!")
}

# utilities####
messages$warningValueWithinThresholdNotExisting <- function(value, threshold, optionalMessage = NULL) {
  paste0(
    "value `", value, "` not found in the array within the absolute threshold of  ", threshold,
    optionalMessage
  )
}

messages$errorWrongArguments <- function(expectedArguments) {
  paste0(
    "Wrong arguments provided for the function! Expected arguments are: ",
    expectedArguments, "."
  )
}

# utilities numerics####
messages$valueShouldNotBeNegative <- function(parameterName, value) {
  paste0(parameterName, " must be a positive numerical value, but the value is ", value)
}

# utilities-data####
messages$errorInvalidMeanMethod <- function() {
  "Invalid value for argument `method`, supported values are `arithmetic` or `geometric`"
}

messages$errorOutputMolWeightNeeded <- function() {
  "`outputMolWeight` can not be `NULL` when data sets have different molWeights"
}

# utilities-figures####
messages$nrOfColorsShouldBePositive <- function(nrOfColors) {
  paste0("nrOfColors must be positive, value ", nrOfColors, " is not valid!")
}

messages$PlotIDsMustBeUnique <- function(duplicated_plotIDs = NULL) {
  paste0("PlotID must be unique in PlotConfiguration, but the following plotIDs are duplicated: ", duplicated_plotIDs)
}

messages$PlotGridsNamesMustBeUnique <- function(duplicated_plotGridsNames = NULL) {
  paste0("PlotGrids names must be unique in PlotGridConfiguration, but the following names are duplicated: ", duplicated_plotGridsNames)
}

# scenario####
messages$wrongSimulationType <- function() {
  "Wrong value for 'simulationType'! Accepted values are 'Individual and 'Population'"
}

messages$scenarioConfigurationNameNotFoundWhenReading <- function(scenarioName) {
  return(paste0("readScenarioDefinition: Scenario '", scenarioName, "' is not specified!"))
}

messages$warningInvalidScenarioName <- function(scenarioNames) {
  paste0(
    "The following scenarios are not present in `simulatedScenarios`: ",
    paste(scenarioNames, collapse = ",\n"),
    ". Data can not be added to `DataCombined` object."
  )
}

messages$noPopulationIdForPopulationScenario <- function(scenarioName) {
  paste0("Simulation type of the scenario with scenario name '", scenarioName, "' is set to
         'Population', but the field `populationId` is not set! Every population
         simulation scenario must have a population id defined")
}

messages$stopScenarioNameNonUnique <- function(scenarioName) {
  paste0(
    "Scenario '", scenarioName, "' is defined multiple times! Make sure that each
    scenario defined in the excel file has a unique name."
  )
}

messages$stopWrongTimeIntervalString <- function(timeIntervalString) {
  paste0(
    "The time interval string '", timeIntervalString, "' is not valid! Please
    check the format of the string. Following criteria must be
    met: 1) Each time interval must contain three numbers separated by a ',', 2) all
    numbers must be positive, 3) The first number (start time) must be smaller than
    the second number (end time), 4) The third number (resolution) must
    be greater than zero. Time intervals must be separated by a ';'."
  )
}

messages$stopScenarioMissingTimeUnit <- function(scenarioName) {
  paste0("Scenario '", scenarioName, "' has simulation time defined, but no unit is specified! Please specify simulation time unit.")
}

messages$missingResultsForScenario <- function(scenarioName) {
  paste0("No simulation results could be computed for the cenario '", scenarioName, "'.")
}
# sensitivity-calculation####
messages$noPKDataToWrite <- function() {
  "`pkDataFilePath` argument is specified, but there is no PK parameters data to write to spreadsheets."
}

# utilities-quantity####
messages$cannotGetMoleculeFromQuantity <- function(quantityPath, optionalMessage = NULL) {
  paste0("Could not retrieve molecule name for the quantity with the path '", quantityPath, "'. ", optionalMessage)
}

# data sets
messages$warningInvalidDataSetName <- function(dataSetNames) {
  paste0(
    "The following data sets are not present in `observedData`: ",
    paste(dataSetNames, collapse = ",\n"),
    ". Data can not be added to `DataCombined` object."
  )
}

# Plots.xlsx####
messages$warningInvalidPlotID <- function(plotIDs, plotGridTitle) {
  paste0(
    "The plots with plotIDs ", paste(plotIDs, collapse = ",\n"), " could not be added to plot grid `",
    plotGridTitle,
    "`. Please check if they are defined in sheet `plotConfiguration` and data is added in sheet `DataCombined`."
  )
}

messages$errorInvalidPlotID <- function(plotIDs) {
  paste0(
    "The plots with plotIDs ", paste(plotIDs, collapse = ",\n"), " are used in the sheet
    'plotGrids' but are not defined in the sheet 'plotConfiguration'."
  )
}

messages$missingPlotIDs <- function() {
  "Missing values found in mandatory column 'plotIDs' of sheet 'plotGrids'. Fill in values to proceed."
}

messages$missingLabel <- function() {
  "Missing values found in mandatory column 'label' of sheet 'DataCombined'. Fill in values to proceed."
}

messages$missingPlotType <- function() {
  "Missing values found in mandatory column 'plotType' of sheet 'plotConfiguration'. Fill in values to proceed."
}

messages$missingDataType <- function() {
  "Missing values found in mandatory column 'dataType' of sheet 'DataCombined'. Fill in values to proceed."
}

messages$missingScenarioName <- function() {
  "Missing values found in mandatory column 'scenario' of sheet 'DataCombined' when 'dataType' is 'simulated'. Fill in values to proceed."
}

messages$missingDataCombinedName <- function() {
  "Missing values found in mandatory column 'DataCombinedName' of sheet 'plotConfiguration'. Fill in values to proceed."
}

messages$stopInvalidDataCombinedName <- function(dataCombinedNames) {
  paste0(
    "The following DataCombined are used in `plotConfiguration` sheet but are not present in `DataCombined` sheet: ",
    paste(dataCombinedNames, collapse = ", ")
  )
}

messages$stopNoPathProvided <- function(dataCombinedName) {
  paste0(
    "No output path is defined for the DataCombined '", paste0(dataCombinedName, collapse = ", "), "'.
    Each simulation output must have an output path specified."
  )
}

messages$stopWrongOutputPath <- function(dataCombinedName, scenarioName, path) {
  paste0(
    "Output path '", path, "' is defined in the DataCombined '", paste0(dataCombinedName, collapse = ", "), "' for scenario '", scenarioName, "' but has not been simulated. Please check that the output path is specified for this scenario."
  )
}

messages$stopNoDataSetProvided <- function(dataCombinedName) {
  paste0(
    "No data set is defined for the DataCombined '", paste0(dataCombinedName, collapse = ", \n"), "'.
    Each observed data must have a 'dataSet' specified."
  )
}

messages$stopInvalidDataSetName <- function(dataSetNames) {
  paste0(
    "The following data sets are not present in `observedData`: ",
    paste(dataSetNames, collapse = ",\n")
  )
}

messages$invalidConfigurationPropertyFromExcel <- function(propertyName, configurationType) {
  paste0("Trying to apply property '", propertyName, "' that is not supported by
         the configuration '", configurationType, "'! Check column names in the
         excel file defining plot configurations.")
}

messages$missingOutputFileName <- function() {
  paste0("Missing values found in mandatory column 'outputName' of sheet 'exportConfiguration'. No plots are exported to file for corresponding rows.")
}

messages$missingPlotGrids <- function(missingPlotGrids) {
  paste0(
    "Invalid values in column 'plotGridName' of sheet 'exportConfiguration': ",
    paste(missingPlotGrids, collapse = ",\n"),
    ". Plot grids are either not defined or empty and can not be exported to file."
  )
}

messages$invalidPlotGridNames <- function(plotGridNames) {
  paste(
    "Following plot grid names have been specified but are not present in the `plotGrids` sheet! Define these plots first: ",
    paste0(plotGridNames, collapse = ",\n")
  )
}

messages$invalidOutputPathIds <- function(outputPathIds, scenarioName) {
  paste(
    "Following output path IDs have been specified as output for scenario '", scenarioName,
    "', but are not present in the `OutputPaths` sheet! Define these outputs first: ",
    paste0(outputPathIds, collapse = ",\n")
  )
}
