messages <- ospsuite.utils::messages

# Parameters structure####
messages$errorWrongXLSStructure <- function(
  filePath,
  expectedColNames,
  optionalMessage = ""
) {
  glue::glue(
    "Loading from XLS failed, the file '{filePath}' has wrong structure!
    The file should contain columns '{glue::glue_collapse(expectedColNames, sep = \", \")}'. {optionalMessage}"
  ) |>
    as.character()
}

messages$wrongParametersStructure <- function(argumentName) {
  glue::glue(
    "Argument '{argumentName}' has wrong structure. Expected is a named list with three vectors `paths` 
    representing full parameter paths, `values` with numerical values of the parameters, 
    and `units` representing the units the values are in. All three vectors must have the same length"
  ) |>
    as.character()
}

# Enum####
messages$errorEnumPutListMultipleKeys <- function() {
  "Trying to put multiple keys, but only one key is allowed!"
}

# utilities-population####
messages$errorDistributionNotSupported <- function(string) {
  glue::glue(
    "The distribution '{string}' is not supported. Supported distributions are listed in `Distributions`."
  ) |>
    as.character()
}

messages$errorWrongPopulationName <- function(populationName) {
  glue::glue(
    "Population name {populationName} is not specified in the population file!"
  ) |>
    as.character()
}

messages$errorWrongOntogenyStructure <- function(entry) {
  glue::glue(
    "Wrong structure provided for the protein ontogeny specification. 
    Expected is a pair of <ProteinName:Ontogeny>, but the entry is: {entry}"
  ) |>
    as.character()
}

# utilities-individual####
messages$errorWrongIndividualId <- function(individualId) {
  glue::glue(
    "Individual with id {individualId} is not specified in the individual characteristics file!"
  ) |>
    as.character()
}

# utilities####
messages$fileNotFound <- function(filePath) {
  glue::glue("File not found: '{filePath}'") |>
    as.character()
}

messages$errorDuplicateScenarioNames <- function(duplicateNames) {
  glue::glue(
    "Duplicate scenario names found: '{glue::glue_collapse(duplicateNames, sep = \"', '\")}'.
    Please provide unique scenario names."
  ) |>
    as.character()
}

messages$warningValueWithinThresholdNotExisting <- function(
  value,
  threshold,
  optionalMessage = ""
) {
  glue::glue(
    "value `{value}` not found in the array within the absolute threshold of `{threshold}`. {optionalMessage}"
  ) |>
    as.character()
}

messages$errorWrongArguments <- function(expectedArguments) {
  glue::glue(
    "Wrong arguments provided for the function! Expected arguments are: {expectedArguments}."
  ) |>
    as.character()
}

# utilities numerics####
messages$valueShouldNotBeNegative <- function(parameterName, value) {
  glue::glue(
    "{parameterName} must be a positive numerical value, but the value is {value}"
  ) |>
    as.character()
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
  glue::glue("nrOfColors must be positive, value {nrOfColors} is not valid!") |>
    as.character()
}

messages$PlotIDsMustBeUnique <- function(duplicated_plotIDs = "") {
  glue::glue(
    "PlotID must be unique in PlotConfiguration, but the following plotIDs are duplicated: 
    {glue::glue_collapse(duplicated_plotIDs, sep = \", \")}"
  ) |>
    as.character()
}

messages$PlotGridsNamesMustBeUnique <- function(
  duplicated_plotGridsNames = ""
) {
  glue::glue(
    "PlotGrids names must be unique in PlotGridConfiguration, but the following names are duplicated: 
    {glue::glue_collapse(duplicated_plotGridsNames, sep = \"\n\")}"
  ) |>
    as.character()
}

messages$UnknownPlotConfiguration <- function(name) {
  glue::glue("Unknown plot configuration option: {name}") |>
    as.character()
}

# scenario####
messages$errorApplicationProtocolNotFound <- function(
  scenarioName,
  applicationProtocol
) {
  glue::glue(
    "Application protocol '{applicationProtocol}' defined in scenario '{scenarioName}' not found
    in the excel file 'ApplicationProtocols.xlsx'"
  ) |>
    as.character()
}
messages$wrongSimulationType <- function() {
  "Wrong value for 'simulationType'! Accepted values are 'Individual and 'Population'"
}

messages$scenarioConfigurationNameNotFoundWhenReading <- function(
  scenarioName
) {
  glue::glue(
    "readScenarioDefinition: Scenario '{scenarioName}' is not specified!"
  ) |>
    as.character()
}

messages$warningInvalidScenarioName <- function(scenarioNames) {
  glue::glue(
    "The following scenarios are not present in `simulatedScenarios`: 
    {glue::glue_collapse(scenarioNames, sep = \",\n\")}. Data cannot be added to `DataCombined` object."
  ) |>
    as.character()
}

messages$warningNoIndividualCharacteristics <- function(
  scenarioName,
  individualId
) {
  glue::glue(
    "Scenario {scenarioName}: No individual characteristics for individual id '{individualId}' found."
  ) |>
    as.character()
}

messages$warningNoIndividualSpecificModelParameters <- function(
  scenarioName,
  individualId
) {
  glue::glue(
    "Scenario {scenarioName}: No individual specific model parameters for individual id '{individualId}' found."
  ) |>
    as.character()
}

messages$noPopulationIdForPopulationScenario <- function(scenarioName) {
  glue::glue(
    "Simulation type of the scenario with scenario name '{scenarioName}' is set to 'Population', 
    but the field `populationId` is not set! Every population simulation scenario must have a population id defined"
  ) |>
    as.character()
}

messages$stopScenarioNameNonUnique <- function(scenarioName) {
  glue::glue(
    "Scenario '{scenarioName}' is defined multiple times! Make sure that each \\
    scenario defined in the excel file has a unique name."
  ) |>
    as.character()
}

messages$stopWrongTimeIntervalString <- function(timeIntervalString) {
  glue::glue(
    "The time interval string '{timeIntervalString}' is not valid! Please 
    check the format of the string. Following criteria must be 
    met: 1) Each time interval must contain three numbers separated by a ',', 2) all 
    numbers must be positive, 3) The first number (start time) must be smaller than 
    the second number (end time), 4) The third number (resolution) must 
    be greater than zero. Time intervals must be separated by a ';'."
  ) |>
    as.character()
}

messages$stopScenarioMissingTimeUnit <- function(scenarioName) {
  glue::glue(
    "Scenario '{scenarioName}' has simulation time defined, but no unit is specified! 
    Please specify simulation time unit."
  ) |>
    as.character()
}

messages$missingResultsForScenario <- function(scenarioName) {
  glue::glue(
    "No simulation results could be computed for the scenario '{scenarioName}'."
  ) |>
    as.character()
}

messages$missingSteadyStateTimeUnit <- function(scenarioName) {
  glue::glue(
    "Missing unit for steady-state time (column 'SteadyStateTimeUnit') for scenario '{scenarioName}'."
  ) |>
    as.character()
}
# sensitivity-calculation####
messages$noPKDataToWrite <- function() {
  "`saOutputFilePath` argument is specified, but there is no PK parameters data to write to spreadsheets."
}

# sensitivity analysis plotting
messages$noParameterFactor <- function(data, parameterFactor) {
  glue::glue(
    "'parameterFactor' values of {parameterFactor} and {1/parameterFactor}
    are not included in the sensitivity analysis results. 
    Current values: {glue::glue_collapse(sort(unique(data$ParameterFactor)), sep = ', ')}. 
    Please rerun the sensitivity analysis with the required values."
  ) |>
    as.character()
}

# utilities-quantity####
messages$cannotGetMoleculeFromQuantity <- function(
  quantityPath,
  optionalMessage = ""
) {
  glue::glue(
    "Could not retrieve molecule name for the quantity with the path '{quantityPath}'. {optionalMessage}"
  ) |>
    as.character()
}

# data sets
messages$warningInvalidDataSetName <- function(dataSetNames) {
  glue::glue(
    "The following data sets are not present in `observedData`: 
    {glue::glue_collapse(dataSetNames, sep =',\n')}. Data can not be added to `DataCombined` object."
  ) |>
    as.character()
}

# Plots.xlsx####
messages$warningInvalidPlotID <- function(plotIDs, plotGridTitle) {
  glue::glue(
    "The plots with plotIDs {glue::glue_collapse(plotIDs, sep = ',\n')} could not be added to plot grid
    `{plotGridTitle}`. Please check if they are defined in sheet `plotConfiguration` and data is added in
    sheet `DataCombined`."
  ) |>
    as.character()
}

messages$errorInvalidPlotID <- function(plotIDs) {
  glue::glue(
    "The plots with plotIDs {glue::glue_collapse(plotIDs, sep = ',\n')} are used in the sheet
    'plotGrids' but are not defined in the sheet 'plotConfiguration'."
  ) |>
    as.character()
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
  glue::glue(
    "The following DataCombined are used in `plotConfiguration` sheet but are not present in `DataCombined` sheet: 
    {glue::glue_collapse(dataCombinedNames, sep = ', ')}"
  ) |>
    as.character()
}

messages$stopNoPathProvided <- function(dataCombinedName) {
  glue::glue(
    "No output path is defined for the DataCombined '{glue::glue_collapse(dataCombinedName, sep = \", \")}'
    Each simulation output must have an output path specified."
  ) |>
    as.character()
}

messages$stopWrongOutputPath <- function(dataCombinedName, scenarioName, path) {
  glue::glue(
    "Output path '{path}' is defined in the DataCombined '{glue::glue_collapse(dataCombinedName, sep = \", \")}' 
    for scenario '{scenarioName}' but has not been simulated.
    Please check that the output path is specified for this scenario."
  ) |>
    as.character()
}

messages$stopNoDataSetProvided <- function(dataCombinedName) {
  glue::glue(
    "No data set is defined for the DataCombined '{glue::glue_collapse(dataCombinedName, sep = \", \n\")}'.
    Each observed data must have a 'dataSet' specified."
  ) |>
    as.character()
}

messages$stopInvalidDataSetName <- function(dataSetNames) {
  glue::glue(
    "The following data sets are not present in `observedData`: {glue::glue_collapse(dataSetNames, sep = ',\n')}"
  ) |>
    as.character()
}

messages$invalidConfigurationPropertyFromExcel <- function(
  propertyName,
  configurationType
) {
  glue::glue(
    "Trying to apply property '{propertyName}' that is not supported by 
    the configuration '{configurationType}'! Check column names in the 
    excel file defining plot configurations."
  ) |>
    as.character()
}

messages$missingOutputFileName <- function() {
  "Missing values found in mandatory column 'outputName' of sheet 'exportConfiguration'. No plots are exported to file for corresponding rows."
}

messages$missingPlotGrids <- function(missingPlotGrids) {
  glue::glue(
    "Invalid values in column 'plotGridName' of sheet 'exportConfiguration':
    {glue::glue_collapse(missingPlotGrids, sep = ',\n')}. Plot grids are either not defined or empty and can not be
    exported to file."
  ) |>
    as.character()
}

messages$invalidPlotGridNames <- function(plotGridNames) {
  glue::glue(
    "Following plot grid names have been specified but are not present in the `plotGrids` sheet! \\
    Define these plots first: {glue::glue_collapse(plotGridNames, sep = ',\n')}"
  ) |>
    as.character()
}

messages$invalidOutputPathIds <- function(outputPathIds, scenarioName) {
  glue::glue(
    "Following output path IDs have been specified as output for scenario '{scenarioName}', 
    but are not present in the `OutputPaths` sheet! Define these outputs first: 
    {glue::glue_collapse(outputPathIds, sep = ',\n')}"
  ) |>
    as.character()
}

messages$invalidSimulationResultNames <- function(
  simulationResultNames,
  parameterPaths
) {
  glue::glue(
    "The names of the simulationResults and parameterPaths must be the same.
    SimulationResults names:
    {glue::glue_collapse(simulationResultNames, sep = ', ')},
    ParameterPaths names:
    {glue::glue_collapse(parameterPaths, collapse = ', ')}"
  ) |>
    as.character()
}

messages$errorDataCombinedListMustBeList <- function(type) {
  glue::glue(
    "The argument 'dataCombined' must be a named list of DataCombined objects, but the
    type of passed argument is '{type}'."
  ) |>
    as.character()
}

# Sensitivity calculation####
messages$sensitivityAnalysisSimulationFailure <- function(
  parameterPath,
  parameterFactor
) {
  cat(
    glue::glue(
      "Simulation for `{parameterPath}` with variation factor `{parameterFactor}` failed!
      The results will not be included in the sensitivity calculation."
    ) |>
      as.character()
  )
}

messages$invalidCustomFunctionParameters <- function(providedParams) {
  glue::glue(
    "The user-defined function must have either 'x', 'y', or both 'x' and 'y' as parameters.
    Provided parameters are: {glue::glue_collapse(providedParams, sep = ', ')}"
  ) |>
    as.character()
}

messages$errorNotNamedList <- function(objectName, optionalMessage = "") {
  callingFunction <- ospsuite.utils:::.getCallingFunctionName()
  glue::glue(
    "{callingFunction}: argument '{objectName}' is not a named list! {optionalMessage}"
  ) |>
    as.character()
}

messages$invalidVariationRangeLength <- function() {
  "`variationRange` must be either a vector or a list equal to the length of `parameterPaths`."
}

messages$errorSensitivityCalculationNotFound <- function(path) {
  cliFormat("Sensitivity calculation not found at path {.file {path}}.")
}

messages$errorOutputDirExists <- function(outputDir) {
  cliFormat(
    "Directory {.file {outputDir}} already exists.",
    "Set {.code overwrite = TRUE} to replace it."
  )
}

messages$errorFailedToLoadSimulation <- function(path, message) {
  cliFormat(
    "Failed to load simulation from saved path {.file {path}}.",
    "Please provide the {.cls Simulation} object explicitly.",
    paste0("Error: ", message)
  )
}

messages$errorCorruptSensitivityCalculation <- function(path) {
  cliFormat(
    "Failed to load sensitivity calculation from {.file {path}}.",
    "The saved files appear to be incomplete or corrupted."
  )
}

messages$promptDeleteOutputDir <- function(outputDir) {
  cliFormat(
    "Directory {.file {outputDir}} already exists. Do you want to delete it?"
  )
}
