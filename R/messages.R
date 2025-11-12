messages <- ospsuite.utils::messages

# Project config####
messages$oldProjectConfigurationLayout <- function() {
  c(
    "!" = cliFormat(
      "The project configuration file layout used is from an older version of the package."
    ),
    "i" = cliFormat(
      "This version is still supported and will be loaded but it is recommended to update the project configuration file.
      To do so, use the {.code $save} method of the project configuration object."
    )
  )
}

messages$invalidConfigurationProperty <- function(
  property,
  path
) {
  c(
    "x" = cliFormat(
      "Property {property} is not a valid configuration property for {path}"
    )
  )
}

# Parameters structure####
messages$errorWrongXLSStructure <- function(
  filePath,
  expectedColNames,
  optionalMessage = ""
) {
  cliFormat(
    "Loading from XLS failed, the file '{filePath}' has wrong structure!
    The file should contain columns '{paste(expectedColNames, collapse = \", \")}'. {optionalMessage}"
  )
}

messages$wrongParametersStructure <- function(argumentName) {
  cliFormat(
    "Argument '{argumentName}' has wrong structure. Expected is a named list with three vectors `paths` 
    representing full parameter paths, `values` with numerical values of the parameters, 
    and `units` representing the units the values are in. All three vectors must have the same length"
  )
}

# Enum####
messages$errorEnumPutListMultipleKeys <- function() {
  cliFormat("Trying to put multiple keys, but only one key is allowed!")
}

# utilities-population####
messages$errorDistributionNotSupported <- function(string) {
  cliFormat(
    "The distribution '{string}' is not supported. Supported distributions are listed in `Distributions`."
  )
}

messages$errorWrongPopulationName <- function(populationName) {
  cliFormat(
    "Population name {populationName} is not specified in the population file!"
  )
}

messages$errorWrongOntogenyStructure <- function(entry) {
  cliFormat(
    "Wrong structure provided for the protein ontogeny specification. 
    Expected is a pair of <ProteinName:Ontogeny>, but the entry is: {entry}"
  )
}

# utilities-individual####
messages$errorWrongIndividualId <- function(individualId) {
  cliFormat(
    "Individual with id {individualId} is not specified in the individual characteristics file!"
  )
}

# utilities####
messages$fileNotFound <- function(filePath) {
  cliFormat("File not found: {.file {filePath}}")
}

messages$pathNotFound <- function(path) {
  cliFormat("The specified destination folder does not exist. ({path}) ")
}

messages$overwriteDestination <- function(path) {
  cliFormat("Overwriting existing esqlabsR project in {path} ")
}

messages$inconsistentArgumentLengths <- function(vectorLengths) {
  c(
    cliFormat("Inconsistent vector argument lengths:"),
    "x" = cliFormat(
      "All vector arguments with length > 1 must have the same length"
    ),
    "i" = cliFormat(
      "Found lengths: {paste(unique(vectorLengths), collapse = ', ')}"
    )
  )
}

messages$errorDuplicateScenarioNames <- function(duplicateNames) {
  cliFormat(
    "Duplicate scenario names found: '{paste(duplicateNames, collapse = \"', '\")}'.
    Please provide unique scenario names."
  )
}

messages$autocorrectDuplicateScenarioNames <- function(
  originalScenarioName,
  scenarioName
) {
  c(
    cliFormat(
      "Duplicate scenario names found and made unique by adding indices:"
    ),
    "i" = cliFormat(
      "Duplicated names: {.val {originalScenarioName}}, renamed to {.val {scenarioName}}"
    )
  )
}

messages$scenarioConfigurationNotNamedList <- function() {
  c(
    cliFormat("Invalid scenarioConfigurations:"),
    "x" = cliFormat("scenarioConfigurations must be a named list"),
    "i" = cliFormat("Each scenario configuration must have a unique name")
  )
}


messages$createdFileSnapshot <- function(inputFile, outputFile) {
  cliFormat(
    "Snapshot of {.file {inputFile}} created at {.file {outputFile}}"
  )
}

messages$restoredProjectConfiguration <- function(inputFile, outputFile) {
  cliFormat(
    "Project configuration from {.file {inputFile}} restored at {.file {outputFile}}"
  )
}

messages$hasUnsavedChanges <- function() {
  c(
    "!" = cliFormat(
      "The ProjectConfiguration object has been modified since loading from file."
    ),
    "i" = cliFormat(
      "The object properties don't match the original Excel file."
    ),
    ">" = cliFormat(
      "Consider running {.run projectConfig$save()} to save changes to the Excel file."
    )
  )
}

messages$invalidArgumentLength <- function(noOfOutpaths, noOfScenarios) {
  c(
    cliFormat("Invalid argument length:"),
    "x" = cliFormat(
      "outputPaths must have length 1 or same length as pkmlFilePaths"
    ),
    "i" = cliFormat(
      "outputPaths has length {noOfOutpaths}, pkmlFilePaths has length {noOfScenarios}"
    )
  )
}

messages$warningValueWithinThresholdNotExisting <- function(
  value,
  threshold,
  optionalMessage = ""
) {
  cliFormat(
    "value `{value}` not found in the array within the absolute threshold of `{threshold}`. {optionalMessage}"
  )
}

messages$errorWrongArguments <- function(expectedArguments) {
  cliFormat(
    "Wrong arguments provided for the function! Expected arguments are: {expectedArguments}."
  )
}

# utilities numerics####
messages$valueShouldNotBeNegative <- function(parameterName, value) {
  cliFormat(
    "{parameterName} must be a positive numerical value, but the value is {value}"
  )
}

# utilities-data####
messages$errorInvalidMeanMethod <- function() {
  cliFormat(
    "Invalid value for argument `method`, supported values are `arithmetic` or `geometric`"
  )
}

messages$errorOutputMolWeightNeeded <- function() {
  cliFormat(
    "`outputMolWeight` can not be `NULL` when data sets have different molWeights"
  )
}

messages$offsetUnitsNotDefined <- function(rows) {
  c(
    "x" = cliFormat(
      "Error in DataCombined {rows}: If x/yOffsets is set, then x/yOffsetsUnits must be defined as well. "
    )
  )
}

# utilities-figures####
messages$nrOfColorsShouldBePositive <- function(nrOfColors) {
  cliFormat("nrOfColors must be positive, value {nrOfColors} is not valid!")
}

messages$PlotIDsMustBeUnique <- function(duplicated_plotIDs = "") {
  cliFormat(
    "PlotID must be unique in PlotConfiguration, but the following plotIDs are duplicated: 
    {paste(duplicated_plotIDs, collapse = \", \")}"
  )
}

messages$PlotGridsNamesMustBeUnique <- function(
  duplicated_plotGridsNames = ""
) {
  cliFormat(
    "PlotGrids names must be unique in PlotGridConfiguration, but the following names are duplicated: 
    {paste(duplicated_plotGridsNames, collapse = \"\n\")}"
  )
}

messages$UnknownPlotConfiguration <- function(name) {
  cliFormat("Unknown plot configuration option: {name}")
}

# scenario####
messages$errorApplicationProtocolNotFound <- function(
  scenarioName,
  applicationProtocol
) {
  cliFormat(
    "Application protocol '{applicationProtocol}' defined in scenario '{scenarioName}' not found
    in the excel file 'ApplicationProtocols.xlsx'"
  )
}
messages$wrongSimulationType <- function() {
  cliFormat(
    "Wrong value for 'simulationType'! Accepted values are 'Individual and 'Population'"
  )
}

messages$scenarioConfigurationNameNotFoundWhenReading <- function(
  scenarioName
) {
  cliFormat(
    "readScenarioDefinition: Scenario '{scenarioName}' is not specified!"
  )
}

messages$warningInvalidScenarioName <- function(scenarioNames) {
  cliFormat(
    "The following scenarios are not present in `simulatedScenarios`: 
    {paste(scenarioNames, collapse = \",\n\")}. Data cannot be added to `DataCombined` object."
  )
}

messages$invalidArgumentLengthScenarios <- function(
  argName,
  arg,
  noOfScenarios
) {
  c(
    cliFormat("Invalid argument length:"),
    "x" = cliFormat(
      "{argName} must have length 1 or same length as pkmlFilePaths"
    ),
    "i" = cliFormat(
      "{argName} has length {length(arg)}, pkmlFilePaths has length {noOfScenarios}"
    )
  )
}

messages$warningNoIndividualCharacteristics <- function(
  scenarioName,
  individualId
) {
  cliFormat(
    "Scenario {scenarioName}: No individual characteristics for individual id '{individualId}' found."
  )
}

messages$warningNoIndividualSpecificModelParameters <- function(
  scenarioName,
  individualId
) {
  cliFormat(
    "Scenario {scenarioName}: No individual specific model parameters for individual id '{individualId}' found."
  )
}

messages$noPopulationIdForPopulationScenario <- function(scenarioName) {
  cliFormat(
    "Simulation type of the scenario with scenario name '{scenarioName}' is set to 'Population', 
    but the field `populationId` is not set! Every population simulation scenario must have a population id defined"
  )
}

messages$stopScenarioNameNonUnique <- function(scenarioName) {
  cliFormat(
    "Scenario '{scenarioName}' is defined multiple times! Make sure that each scenario defined in the excel file has a unique name."
  )
}

messages$stopWrongTimeIntervalString <- function(timeIntervalString) {
  cliFormat(
    "The time interval string '{timeIntervalString}' is not valid! Please 
    check the format of the string. Following criteria must be 
    met: 1) Each time interval must contain three numbers separated by a ',', 2) all 
    numbers must be positive, 3) The first number (start time) must be smaller than 
    the second number (end time), 4) The third number (resolution) must 
    be greater than zero. Time intervals must be separated by a ';'."
  )
}

messages$stopScenarioMissingTimeUnit <- function(scenarioName) {
  cliFormat(
    "Scenario '{scenarioName}' has simulation time defined, but no unit is specified! 
    Please specify simulation time unit."
  )
}

messages$missingResultsForScenario <- function(scenarioName) {
  cliFormat(
    "No simulation results could be computed for the scenario '{scenarioName}'."
  )
}

messages$missingSteadyStateTimeUnit <- function(scenarioName) {
  cliFormat(
    "Missing unit for steady-state time (column 'SteadyStateTimeUnit') for scenario '{scenarioName}'."
  )
}
# sensitivity-calculation####
messages$noPKDataToWrite <- function() {
  cliFormat(
    "`saOutputFilePath` argument is specified, but there is no PK parameters data to write to spreadsheets."
  )
}

# sensitivity analysis plotting
messages$noParameterFactor <- function(data, parameterFactor) {
  cliFormat(
    "'parameterFactor' values of {parameterFactor} and {1 / parameterFactor} are not included in the sensitivity analysis results. Current values: {paste(sort(unique(data$ParameterFactor)), collapse = ', ')}. Please rerun the sensitivity analysis with the required values."
  )
}

messages$errorOptionOutOfBounds <- function(parameterFactor) {
  range <- .getPlotConfigurationOptions(
    "parameterFactor"
  )$parameterFactor$valueRange
  cliFormat(
    "Value(s) out of the allowed range [{range[1]}, {range[2]}]"
  )
}

# utilities-quantity####
messages$cannotGetMoleculeFromQuantity <- function(
  quantityPath,
  optionalMessage = ""
) {
  cliFormat(
    "Could not retrieve molecule name for the quantity with the path {.file {quantityPath}}. {optionalMessage}"
  )
}

# data sets
messages$warningInvalidDataSetName <- function(dataSetNames) {
  cliFormat(
    "The following data sets are not present in `observedData`: 
    {paste(dataSetNames, collapse =',\n')}. Data can not be added to `DataCombined` object."
  )
}

# Plots.xlsx####
messages$warningInvalidPlotID <- function(plotIDs, plotGridTitle) {
  cliFormat(
    "The plots with plotIDs {paste(plotIDs, collapse = ',\n')} could not be added to plot grid
    {.code {plotGridTitle}}. Please check if they are defined in sheet `plotConfiguration` and data is added in
    sheet `DataCombined`."
  )
}

messages$errorInvalidPlotID <- function(plotIDs) {
  cliFormat(
    "The plots with plotIDs {paste(plotIDs, collapse = ',\n')} are used in the sheet
    'plotGrids' but are not defined in the sheet 'plotConfiguration'."
  )
}

messages$missingPlotIDs <- function() {
  cliFormat(
    "Missing values found in mandatory column 'plotIDs' of sheet 'plotGrids'. Fill in values to proceed."
  )
}

messages$missingLabel <- function() {
  cliFormat(
    "Missing values found in mandatory column 'label' of sheet 'DataCombined'. Fill in values to proceed."
  )
}

messages$missingPlotType <- function() {
  cliFormat(
    "Missing values found in mandatory column 'plotType' of sheet 'plotConfiguration'. Fill in values to proceed."
  )
}

messages$missingDataType <- function() {
  cliFormat(
    "Missing values found in mandatory column 'dataType' of sheet 'DataCombined'. Fill in values to proceed."
  )
}

messages$missingScenarioName <- function() {
  cliFormat(
    "Missing values found in mandatory column 'scenario' of sheet 'DataCombined' when 'dataType' is 'simulated'. Fill in values to proceed."
  )
}

messages$missingDataCombinedName <- function() {
  cliFormat(
    "Missing values found in mandatory column 'DataCombinedName' of sheet 'plotConfiguration'. Fill in values to proceed."
  )
}

messages$stopInvalidDataCombinedName <- function(dataCombinedNames) {
  cliFormat(
    "The following DataCombined are used in `plotConfiguration` sheet but are not present in `DataCombined` sheet: 
    {paste(dataCombinedNames, collapse = ', ')}"
  )
}

messages$stopNoPathProvided <- function(dataCombinedName) {
  cliFormat(
    "No output path is defined for the DataCombined '{paste(dataCombinedName, collapse = \", \")}'
    Each simulation output must have an output path specified."
  )
}

messages$stopWrongOutputPath <- function(dataCombinedName, scenarioName, path) {
  cliFormat(
    "Output path '{path}' is defined in the DataCombined '{paste(dataCombinedName, collapse = \", \")}' 
    for scenario '{scenarioName}' but has not been simulated.
    Please check that the output path is specified for this scenario."
  )
}

messages$stopNoDataSetProvided <- function(dataCombinedName) {
  cliFormat(
    "No data set is defined for the DataCombined '{paste(dataCombinedName, collapse = \", \n\")}'.
    Each observed data must have a 'dataSet' specified."
  )
}

messages$stopInvalidDataSetName <- function(dataSetNames) {
  cliFormat(
    "The following data sets are not present in `observedData`: {paste0(dataSetNames, collapse = ',\n')}"
  )
}

messages$invalidConfigurationPropertyFromExcel <- function(
  propertyName,
  configurationType
) {
  cliFormat(
    "Trying to apply property '{propertyName}' that is not supported by 
    the configuration '{configurationType}'! Check column names in the 
    excel file defining plot configurations."
  )
}

messages$missingOutputFileName <- function() {
  cliFormat(
    "Missing values found in mandatory column 'outputName' of sheet 'exportConfiguration'. No plots are exported to file for corresponding rows."
  )
}

messages$missingPlotGrids <- function(missingPlotGrids) {
  cliFormat(
    "Invalid values in column 'plotGridName' of sheet 'exportConfiguration':
    {paste0(missingPlotGrids, collapse = ',\n')}. Plot grids are either not defined or empty and can not be
    exported to file."
  )
}

messages$invalidPlotGridNames <- function(plotGridNames) {
  cliFormat(
    "Following plot grid names have been specified but are not present in the `plotGrids` sheet!
  Define these plots first: {paste(plotGridNames, collapse = ',\n')}"
  )
}

messages$invalidOutputPathIds <- function(outputPathIds, scenarioName) {
  cliFormat(
    "Following output path IDs have been specified as output for scenario '{scenarioName}', 
    but are not present in the `OutputPaths` sheet! Define these outputs first: 
    {paste(outputPathIds, collapse = ',\n')}"
  )
}

messages$invalidSimulationResultNames <- function(
  simulationResultNames,
  parameterPaths
) {
  cliFormat(
    "The names of the simulationResults and parameterPaths must be the same.
    SimulationResults names:
    {paste(simulationResultNames, collapse = ', ')},
    ParameterPaths names:
    {paste(parameterPaths, collapse = ', ')}"
  )
}

messages$errorDataCombinedListMustBeList <- function(type) {
  cliFormat(
    "The argument 'dataCombined' must be a named list of DataCombined objects, but the
    type of passed argument is {.code {type}}."
  )
}

# Sensitivity calculation####
messages$sensitivityAnalysisSimulationFailure <- function(
  parameterPath,
  parameterFactor
) {
  cat(
    cliFormat(
      "Simulation for {.code {parameterPath}} with variation factor {.code {parameterFactor}} failed!
      The results will not be included in the sensitivity calculation."
    )
  )
}

messages$invalidCustomFunctionParameters <- function(providedParams) {
  cliFormat(
    "The user-defined function must have either 'x', 'y', or both 'x' and 'y' as parameters.
    Provided parameters are: {paste(providedParams, collapse = ', ')}"
  )
}

messages$errorNotNamedList <- function(objectName, optionalMessage = "") {
  callingFunction <- ospsuite.utils:::.getCallingFunctionName()
  cliFormat(
    "{callingFunction}: argument {.code {objectName}} is not a named list! {optionalMessage}"
  )
}

messages$invalidVariationRangeLength <- function() {
  cliFormat(
    "`variationRange` must be either a vector or a list equal to the length of `parameterPaths`."
  )
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

# Excel field validation error messages ####
messages$excelFieldFormatError <- function(
  fieldName,
  value,
  plotID,
  expectedFormat
) {
  plotInfo <- if (!is.null(plotID)) paste0(" in plot {.val {plotID}}") else ""
  cliFormat(
    "Excel validation error{plotInfo}: Invalid format for {.field {fieldName}}.
    Provided: {.val {value}}
    Expected: Values separated by commas (not spaces)
    Example: '72, 80' or '72,80' (not '72 80')"
  )
}

messages$excelFieldLengthError <- function(
  fieldName,
  value,
  plotID,
  expected,
  actual
) {
  plotInfo <- if (!is.null(plotID)) paste0(" in plot {.val {plotID}}") else ""
  valuePlural <- if (actual != 1) "s" else ""
  expectedPlural <- if (expected != 1) "s" else ""
  cliFormat(
    "Excel validation error{plotInfo}: Wrong number of values for {.field {fieldName}}.
    Provided: {.val {value}} ({actual} value{valuePlural})
    Expected: {expected} comma-separated value{expectedPlural}
    Example: '72, 80'"
  )
}

messages$excelFieldTypeError <- function(
  fieldName,
  value,
  plotID,
  expectedType
) {
  plotInfo <- if (!is.null(plotID)) paste0(" in plot {.val {plotID}}") else ""
  cliFormat(
    "Excel validation error{plotInfo}: Invalid {.field {fieldName}} value.
    Provided: {.val {value}}
    Expected: {expectedType} values"
  )
}

messages$warningSensitivityPKParameterNotCalculated <- function(
  parameterPath,
  pkParameter
) {
  cliFormat(
    "SensitivityPKParameter could not be calculated for",
    "ParameterPath {.envvar {parameterPath}} and PKParameter {.envvar {pkParameter}}.",
    "Possible reason: baseline simulation failure (ParameterFactor = 1.0)."
  )
}

messages$excelNoDataRows <- function() {
  c(
    "x" = cliFormat(
      "The specified excel sheet does not contain any rows with data."
    ),
    "*" = cliFormat(
      "Please check the excel sheet name and content and try again."
    )
  )
}

messages$excelUncompleteRows <- function() {
  c(
    "x" = cliFormat("The specified excel sheet contains uncomplete row(s)"),
    "i" = cliFormat("Using only complete rows to define population parameters")
  )
}

messages$excelNoCompleteRows <- function() {
  c(
    "x" = cliFormat(
      "The specified excel sheet does not contain any complete row"
    ),
    "*" = cliFormat("Please fill all the columns and try again.")
  )
}


messages$excelSheetEmptyOrInvalid <- function() {
  c(
    cliFormat("Excel sheet name was empty or invalid:"),
    "i" = cliFormat("Using default name 'Sheet'")
  )
}

messages$excelSheetSanitized <- function(originalName) {
  c(
    cliFormat("Excel sheet name became empty after sanitization:"),
    "x" = cliFormat("Original name: '{originalName}'"),
    "i" = cliFormat("Using default name 'Sheet'")
  )
}

messages$excelSheetSanitizedInfo <- function(originalName, sanitizedName) {
  c(
    cliFormat("Excel sheet name was sanitized to comply with naming rules:"),
    "x" = cliFormat("Original name: '{originalName}'"),
    "v" = cliFormat("Sanitized name: '{sanitizedName}'"),
    "i" = cliFormat(
      "Excel sheet names must be 31 characters or less and cannot contain: / \\\\ * [ ] : ?"
    )
  )
}

messages$excelNotInSync <- function(message = "") {
  cliFormat(
    "The Excel configuration files are NOT in sync with the JSON snapshot. {message}"
  )
}

messages$excelInSync <- function() {
  cliFormat(
    "Excel configuration files are in sync with JSON snapshot."
  )
}

messages$projectConfigUnsavedChanges <- function() {
  cliFormat(
    "The ProjectConfiguration object has {.strong unsaved changes} that differ from the Excel file."
  )
}

messages$abortedByUser <- function() {
  cliFormat(
    "Aborted by user."
  )
}
