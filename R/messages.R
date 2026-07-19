messages <- ospsuite.utils::messages

# `messages$` is the central catalog for this package's user-facing error,
# warning, and message text: each entry is a builder returning a formatted
# string (or a `cli`-tagged character vector). Sites raise a catalog entry
# through the `cli` wrappers `cli::cli_abort()` (errors) and `cli::cli_warn()`
# (warnings); `cli::cli_inform()` surfaces informational text. New user-facing
# text belongs here as a catalog entry routed through those wrappers, not as a
# base `stop()`/`warning()`/`message()` on an inline literal string.
#
# One known exception: the project validation framework (`R/validation.R` and
# the per-section validators in `R/scenarios.R`, `R/individuals.R`,
# `R/populations.R`, `R/output-paths.R`, `R/plots.R`,
# `R/parameter-identification.R`) builds most of its `validationResult`
# messages inline with `paste0()` rather than through this catalog. The
# observed-data validator (`R/observed-data.R`) is routed through the catalog
# (the `validationObservedData*` entries below); the remaining validators are
# not yet migrated, so not all validation wording lives here.

# Parameters structure####
messages$errorWrongXLSStructure <- function(
  filePath,
  expectedColNames,
  optionalMessage = ""
) {
  cliFormat(
    "Loading from XLS failed, the file {.file {filePath}} has wrong structure!
    The file should contain columns {.val {paste(expectedColNames, collapse = \", \")}}. {optionalMessage}"
  )
}

messages$wrongParametersStructure <- function(argumentName) {
  cliFormat(
    "Argument {.arg {argumentName}} has wrong structure. Expected is a named list with three vectors `paths`
    representing full parameter paths, `values` with numerical values of the parameters,
    and `units` representing the units the values are in. All three vectors must have the same length"
  )
}

messages$errorMissingValuesInParameters <- function(
  filePath,
  parameterPaths
) {
  cliFormat(
    "Missing or non-numeric values in parameters file {.file {filePath}} for parameter(s): {.val {paste(parameterPaths, collapse = ', ')}}. A numeric value must be specified for all parameters."
  )
}

messages$warningDuplicateParameters <- function(
  filePath,
  parameterPaths
) {
  cliFormat(
    "Duplicate parameter path(s) in parameters file {.file {filePath}}: {.val {paste(parameterPaths, collapse = ', ')}}. Only the last value defined for each path is used."
  )
}

messages$errorMissingUnitsInInitialConditions <- function(
  filePath,
  moleculePaths
) {
  cliFormat(
    "Missing units in initial values file {.file {filePath}} for molecule(s): {.val {paste(moleculePaths, collapse = ', ')}}. Units must be specified for all molecule initial values."
  )
}

messages$errorMissingValuesInInitialConditions <- function(
  filePath,
  moleculePaths
) {
  cliFormat(
    "Missing or non-numeric values in initial values file {.file {filePath}} for molecule(s): {.val {paste(moleculePaths, collapse = ', ')}}. A numeric value must be specified for all present molecules."
  )
}

messages$errorInvalidIsPresentInInitialConditions <- function(
  filePath,
  moleculePaths
) {
  cliFormat(
    "Invalid 'Is Present' values in initial values file {.file {filePath}} for molecule(s): {.val {paste(moleculePaths, collapse = ', ')}}. 'Is Present' must be a logical value (TRUE/FALSE), numeric 1/0 (present/not present), or empty."
  )
}

messages$errorMissingPathInInitialConditions <- function(
  filePath,
  sheet,
  rows
) {
  cliFormat(
    "Missing {.field Container Path} or {.field Molecule Name} in initial values file {.file {filePath}}, sheet {.val {sheet}}, data row(s): {.val {paste(rows, collapse = ', ')}}."
  )
}

messages$warningDuplicateInitialConditions <- function(
  filePath,
  moleculePaths
) {
  cliFormat(
    "Duplicate molecule path(s) in initial values file {.file {filePath}}: {.val {paste(moleculePaths, collapse = ', ')}}. Only the last value defined for each path is used."
  )
}

# Enum####
messages$errorEnumPutListMultipleKeys <- function() {
  cliFormat("Trying to put multiple keys, but only one key is allowed!")
}

# populations ####
messages$errorDistributionNotSupported <- function(string) {
  cliFormat(
    "The distribution {.val {string}} is not supported. Supported distributions are listed in {.var Distributions}."
  )
}

messages$errorWrongOntogenyStructure <- function(entry) {
  cliFormat(
    "Wrong structure provided for the protein ontogeny specification.
    Expected is a pair of {.cls ProteinName:Ontogeny}, but the entry is: {.val {entry}}"
  )
}

# utilities####
messages$fileNotFound <- function(filePath) {
  cliFormat("File not found: {.file {filePath}}")
}

messages$invalidPathArgument <- function() {
  cliFormat("{.arg path} must be a single non-empty, non-NA string.")
}

messages$saveProjectNoTree <- function() {
  c(
    "This project is not bound to a directory, so there is nothing to save in place.",
    "i" = "Use {.fn snapshotProject} to write a portable single-file snapshot.",
    "i" = "Use {.fn initProject} then {.fn loadProject} to give the project a home on disk."
  )
}

messages$reloadProjectNoTree <- function() {
  c(
    "This project is not bound to a directory, so there is nothing to reload from.",
    "i" = "{.fn reloadProject} re-reads a project's on-disk tree; an in-memory project has none."
  )
}

messages$projectAlreadyUpToDate <- function() {
  "Project is already up to date; nothing to save."
}

messages$snapshotFileExists <- function(path) {
  c(
    "A snapshot file already exists at {.file {path}}.",
    "i" = "Pass {.code overwrite = TRUE} to replace it, or a different {.arg name}."
  )
}

messages$invalidSnapshotName <- function(stem) {
  c(
    "{.arg name} must be a single filename stem without path separators.",
    "x" = "The stem {.val {stem}} contains a path separator or is {.val .} / \\
    {.val ..}, so it could write outside {.arg dir}.",
    "i" = "Pass a single filename segment (no path separator and not {.val .} / \\
    {.val ..}), or leave {.arg name} as {.code NULL} for a timestamped default."
  )
}

messages$restoreDirNotEmpty <- function(dir) {
  c(
    "{.arg dir} is not empty ({.path {dir}}).",
    "i" = "Restore unpacks into a fresh directory. Pass \\
    {.code overwrite = TRUE} to replace the contents of {.arg dir}, or an \\
    empty or new {.arg dir}."
  )
}

messages$restoreOverwroteTree <- function(dir) {
  c(
    "Replaced the existing project in {.path {dir}} with the snapshot.",
    "!" = "Any {.cls Project} previously loaded from {.arg dir} is now stale.",
    "i" = "Rebind to the object {.fn restoreProject} returned, or \\
    {.fn reloadProject} the old handle."
  )
}

messages$failedToRemoveStaleDefinitionFiles <- function(paths) {
  n <- length(paths)
  # Interpolate eagerly here, where `n` and `paths` are in scope: the
  # `cli::cli_abort()` call site does not carry these names, so a lazily
  # interpolated glue vector would fail to evaluate `{n}` / `{paths}` there.
  cli::format_message(c(
    "Failed to remove {n} stale definition file{?s} from the definitions tree.",
    "x" = "{.file {paths}}",
    "i" = "A stale file that cannot be deleted would reappear as a definition on the next {.fn loadProject}; check the file permissions and remove it manually."
  ))
}

messages$pathNotFound <- function(path) {
  cliFormat(
    "The specified destination folder does not exist. ({.path {path}}) "
  )
}

messages$overwriteDestination <- function(path) {
  cliFormat("Overwriting existing esqlabsR project in {.path {path}} ")
}

messages$failedToClearProjectArtifacts <- function(path) {
  # Interpolate eagerly here where `path` is in scope; the `cli::cli_abort()`
  # call site passes a local whose name is not `path`.
  cli::format_message(c(
    "Failed to remove an existing project artifact before overwriting.",
    "x" = "{.path {path}}",
    "i" = "Overwriting requires removing the old project's definitions tree and container first; check the path's permissions and remove it manually."
  ))
}

messages$inconsistentArgumentLengths <- function(vectorLengths) {
  cli::format_message(c(
    "Inconsistent vector argument lengths:",
    "x" = "All vector arguments with length > 1 must have the same length",
    "i" = "Found lengths: {.val {paste(unique(vectorLengths), collapse = ', ')}}"
  ))
}

messages$autocorrectDuplicateScenarioNames <- function(
  originalScenarioName,
  scenarioName
) {
  cli::format_message(c(
    "Duplicate scenario names found and made unique by adding indices:",
    "i" = "Duplicated names: {.val {originalScenarioName}}, renamed to {.val {scenarioName}}"
  ))
}

messages$scenariosAddedToProject <- function(scenarioNames) {
  cli::format_message(c(
    "i" = "Added {length(scenarioNames)} scenario{?s}: {.val {scenarioNames}}"
  ))
}

messages$noModelFolderUsingAbsolutePath <- function(pkmlPath) {
  cli::format_message(c(
    "!" = "The project has no {.field modelFolder}; storing an absolute model \\
    file path.",
    "i" = "Set a {.field modelFolder} on the project so the scenario stores a \\
    portable relative path ({.file {pkmlPath}})."
  ))
}

messages$outputPathIdCollision <- function(id, existingPath, newPath) {
  cli::format_message(c(
    "x" = "Output path id {.val {id}} already maps to a different path.",
    "i" = "Existing: {.val {existingPath}}",
    "i" = "Requested: {.val {newPath}}"
  ))
}

messages$outputPathAliasIgnored <- function(userAlias, registeredId, path) {
  cli::format_message(c(
    "i" = "Output path alias {.val {userAlias}} ignored: \\
    path {.val {path}} is already registered as {.val {registeredId}}."
  ))
}

messages$noModelFolderForRelativeModelFile <- function(
  scenarioName,
  modelFile
) {
  cli::format_message(c(
    "x" = "Cannot resolve the model file for scenario {.val {scenarioName}}.",
    "i" = "{.field modelFile} {.val {modelFile}} is relative but the project \\
    has no {.field modelFolder} to resolve it against."
  ))
}

messages$noPopulationsFolderForCSVPopulation <- function(
  scenarioName,
  populationId
) {
  cli::format_message(c(
    "x" = "Cannot resolve the population csv for scenario {.val {scenarioName}}.",
    "i" = "{.field populationId} {.val {populationId}} is read from a csv but \\
    the project has no {.field populationsFolder} to resolve it against."
  ))
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

# The Excel axis of `projectStatus()`: with no `Project.xlsx` side-car there is
# nothing to compare the in-memory project against.
messages$syncNoExcel <- function() {
  cli::format_inline(
    "No {.file Project.xlsx} side-car to compare against; nothing to sync."
  )
}

# The tree axis of `projectStatus()`: whether in-memory edits diverge from the
# on-disk `definitions/` tree (the dirty bit).
messages$syncTreeDirty <- function() {
  cli::format_inline(
    "Unsaved changes: the project has in-memory edits not yet saved to the tree."
  )
}

messages$syncTreeClean <- function() {
  cli::format_inline(
    "No unsaved changes: memory matches the on-disk tree."
  )
}

messages$syncNoTree <- function() {
  cli::format_inline(
    "No on-disk tree: this in-memory project is not bound to a directory."
  )
}

messages$invalidArgumentLength <- function(noOfOutpaths, noOfScenarios) {
  cli::format_message(c(
    "Invalid argument length:",
    "x" = "outputPaths must have length 1 or same length as pkmlFilePaths",
    "i" = "outputPaths has length {.val {noOfOutpaths}}, pkmlFilePaths has length {.val {noOfScenarios}}"
  ))
}

messages$warningValueWithinThresholdNotExisting <- function(
  value,
  threshold,
  optionalMessage = ""
) {
  cliFormat(
    "value {.val {value}} not found in the array within the absolute threshold of {.val {threshold}}. {optionalMessage}"
  )
}

# data-utils ####
messages$errorInvalidMeanMethod <- function() {
  cliFormat(
    "Invalid value for argument {.arg method}, supported values are {.val arithmetic} or {.val geometric}"
  )
}

messages$errorOutputMolWeightNeeded <- function() {
  cliFormat(
    "{.arg outputMolWeight} can not be {.val NULL} when data sets have different molWeights"
  )
}

messages$offsetUnitsNotDefined <- function(rows) {
  cli::format_message(c(
    "x" = "Error in DataCombined {.arg {rows}}: If x/yOffsets is set, then x/yOffsetsUnits must be defined as well. "
  ))
}

# plots ####
messages$nrOfColorsShouldBePositive <- function(nrOfColors) {
  cliFormat(
    "nrOfColors must be positive, value {.val {nrOfColors}} is not valid!"
  )
}

messages$PlotIDsMustBeUnique <- function(duplicated_plotIDs = "") {
  duplicates <- paste(duplicated_plotIDs, collapse = ", ")
  cliFormat(
    "plotId must be unique in plotConfiguration, but the following plotIds are duplicated: {.val {duplicates}}"
  )
}

messages$PlotGridsNamesMustBeUnique <- function(
  duplicated_plotGridsNames = ""
) {
  cliFormat(
    "PlotGrids names must be unique in PlotGridConfiguration, but the following names are duplicated:
    {.val {paste(duplicated_plotGridsNames, collapse = \"\n\")}}"
  )
}

messages$UnknownPlotConfiguration <- function(name) {
  cliFormat("Unknown plot configuration option: {.arg {name}}")
}

# scenario####
messages$errorApplicationProtocolNotFound <- function(
  scenarioName,
  applicationProtocol
) {
  cliFormat(
    "Application protocol {.var {applicationProtocol}} defined in scenario {.var {scenarioName}} not found
    in the excel file {.file ApplicationProtocols.xlsx}"
  )
}
messages$warningInvalidScenarioName <- function(scenarioNames) {
  cliFormat(
    "The following scenarios are not present in {.arg scenarioResults}:
    {.val {paste(scenarioNames, collapse = \",\n\")}}. Data cannot be added to {.var DataCombined} object."
  )
}

messages$invalidArgumentLengthScenarios <- function(
  argName,
  arg,
  noOfScenarios
) {
  cli::format_message(c(
    "Invalid argument length:",
    "x" = "{.arg {argName}} must have length 1 or same length as pkmlFilePaths",
    "i" = "{.arg {argName}} has length {.val {length(arg)}}, pkmlFilePaths has length {.val {noOfScenarios}}"
  ))
}

messages$warningNoIndividualCharacteristics <- function(
  scenarioName,
  individualId
) {
  cliFormat(
    "Scenario {.val {scenarioName}}: No individual characteristics for individual id {.val {individualId}} found."
  )
}

messages$noPopulationIdForPopulationScenario <- function(scenarioName) {
  cliFormat(
    "Simulation type of the scenario with scenario name {.val {scenarioName}} is set to {.val Population},
    but the field {.var populationId} is not set! Every population simulation scenario must have a population id defined"
  )
}

messages$stopWrongTimeIntervalString <- function(timeIntervalString) {
  cliFormat(
    "The time interval string {.val {timeIntervalString}} is not valid! Please 
    check the format of the string. Following criteria must be 
    met: 1) Each time interval must contain three numbers separated by a ',', 2) all 
    numbers must be positive, 3) The first number (start time) must be smaller than 
    the second number (end time), 4) The third number (resolution) must 
    be greater than zero. Time intervals must be separated by a ';'."
  )
}

messages$stopScenarioMissingTimeUnit <- function(scenarioName) {
  cliFormat(
    "Scenario {.val {scenarioName}} has simulation time defined, but no unit is specified! 
    Please specify simulation time unit."
  )
}

messages$missingResultsForScenario <- function(scenarioName) {
  cliFormat(
    "No simulation results could be computed for the scenario {.val {scenarioName}}."
  )
}

messages$errorSavingScenarioResult <- function(scenarioName, conditionMessage) {
  # Escape braces in the condition message so that cli does not try to
  # re-interpret arbitrary error text as glue expressions when cli_warn()
  # processes the returned vector.
  safe_msg <- gsub(
    "}",
    "}}",
    gsub("{", "{{", conditionMessage, fixed = TRUE),
    fixed = TRUE
  )
  c(
    "x" = cli::format_inline(
      "Failed to save results for scenario {.val {scenarioName}}."
    ),
    "i" = safe_msg
  )
}

messages$scenarioResultNameCollision <- function(colliding) {
  cli::format_message(c(
    "x" = "Scenario names collide once {.val /} and {.val \\\\} are replaced with {.val _} for file names:",
    "*" = "{.val {colliding}}",
    "i" = "Rename the scenarios so their file-safe names differ before saving."
  ))
}
# sensitivity-calculation####
messages$noPKDataToWrite <- function(saOutputFilePath) {
  cliFormat(
    "{.arg saOutputFilePath} ({.path {saOutputFilePath}}) is specified, but there is no PK parameters data to write to spreadsheets."
  )
}

# sensitivity analysis plotting
messages$noParameterFactor <- function(data, parameterFactor) {
  cliFormat(
    "{.arg parameterFactor} values of {parameterFactor} and {1 / parameterFactor} are not included in the sensitivity analysis results. Current values: {.val {paste(sort(unique(data$ParameterFactor)), collapse = ', ')}}. Please rerun the sensitivity analysis with the required values."
  )
}

# quantities ####
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
    "The following data sets are not present in {.var observedData}:
    {.val {paste(dataSetNames, collapse =',\n')}}. Data can not be added to {.var DataCombined} object."
  )
}

# Plots.xlsx####
messages$warningLogScaleWithZeroLimit <- function(
  plotID,
  axisLimitsField,
  axis
) {
  cliFormat(
    "Column {.field {axisLimitsField}} in plot {.val {plotID}} contains zero, but the {.val {axis}}-axis scale is set to {.val log}.
    Logarithmic scale cannot display zero values. This may result in empty or unexpected plots."
  )
}

messages$errorInvalidPlotID <- function(plotIDs) {
  cliFormat(
    "The plots with plotIds {.val {paste(plotIDs, collapse = ',\n')}} are used in the sheet
    {.field plotGrids} but are not defined in the sheet {.var plotConfiguration}."
  )
}

messages$missingPlotIDs <- function() {
  cliFormat(
    "Missing values found in mandatory column {.val plotIds} of sheet {.field plotGrids}. Fill in values to proceed."
  )
}

messages$missingPlotGridId <- function() {
  cliFormat("Every plot grid must declare a `plotGridId`.")
}

messages$missingPlotId <- function() {
  cliFormat("Every plot must declare a `plotId`.")
}

messages$missingLabel <- function() {
  cliFormat(
    "Missing values found in mandatory column {.val label} of sheet {.var DataCombined}. Fill in values to proceed."
  )
}

messages$missingPlotType <- function() {
  cliFormat(
    "Missing values found in mandatory column {.val plotType} of sheet {.var plotConfiguration}. Fill in values to proceed."
  )
}

messages$missingDataType <- function() {
  cliFormat(
    "Missing values found in mandatory column {.val dataType} of sheet {.var DataCombined}. Fill in values to proceed."
  )
}

messages$missingScenarioName <- function() {
  cliFormat(
    "Missing values found in mandatory column {.val scenario} of sheet {.var DataCombined} when {.arg dataType} is {.val simulated}. Fill in values to proceed."
  )
}

messages$missingDataCombinedName <- function() {
  cliFormat(
    "Missing values found in mandatory column {.val dataCombinedId} of sheet {.var plotConfiguration}. Fill in values to proceed."
  )
}

messages$stopInvalidDataCombinedName <- function(dataCombinedNames) {
  cliFormat(
    "The following DataCombined are used in {.var plotConfiguration} sheet but are not present in {.var DataCombined} sheet:
    {.val {paste(dataCombinedNames, collapse = ', ')}}"
  )
}

messages$stopDataCombinedNamesNotFound <- function(dataCombinedNames) {
  cliFormat(
    "The following DataCombined names are not defined in the Excel file:
    {.val {paste(dataCombinedNames, collapse = ', ')}}"
  )
}

messages$stopNoPathProvided <- function(dataCombinedName) {
  cliFormat(
    "No output path is defined for the DataCombined {.val {paste(dataCombinedName, collapse = \", \")}}
    Each simulation output must have an output path specified."
  )
}

messages$stopWrongOutputPath <- function(dataCombinedName, scenarioName, path) {
  cliFormat(
    "Output path {.path {path}} is defined in the DataCombined {.val {paste(dataCombinedName, collapse = \", \")}}
    for scenario {.cls {scenarioName}} but has not been simulated.
    Please check that the output path is specified for this scenario."
  )
}

messages$stopScenarioRunFailed <- function(
  dataCombinedName,
  scenarioName,
  path
) {
  cliFormat(
    "The DataCombined {.val {paste(dataCombinedName, collapse = \", \")}} references the output path
    {.path {path}} of scenario {.cls {scenarioName}}, but that scenario produced no results.
    Re-run the scenario and check that it completed successfully."
  )
}

messages$stopPlotGridNamesNotFound <- function(plotGridNames) {
  cliFormat(
    "The following plot grids are not defined in the project:
    {.val {paste(plotGridNames, collapse = ', ')}}"
  )
}

messages$stopPlotIdsNotFound <- function(plotIds) {
  cliFormat(
    "The following plots are not defined in the project:
    {.val {paste(plotIds, collapse = ', ')}}"
  )
}

messages$stopNoDataSetProvided <- function(dataCombinedName) {
  cliFormat(
    "No data set is defined for the DataCombined {.val {paste(dataCombinedName, collapse = \", \n\")}}.
    Each observed data must have a {.var dataSet} specified."
  )
}

messages$stopInvalidDataSetName <- function(dataSetNames) {
  cliFormat(
    "The following data sets are not present in {.var observedData}: {.val {paste0(dataSetNames, collapse = ',\n')}}"
  )
}

messages$invalidConfigurationPropertyFromExcel <- function(
  propertyName,
  configurationType
) {
  cliFormat(
    "Trying to apply property {.arg {propertyName}} that is not supported by 
    the configuration {.var {configurationType}}! Check column names in the 
    excel file defining plot configurations."
  )
}

messages$invalidSimulationResultNames <- function(
  simulationResultNames,
  parameterPaths
) {
  cliFormat(
    "The names of the simulationResults and parameterPaths must be the same.
    SimulationResults names:
    {.val {paste(simulationResultNames, collapse = ', ')}},
    ParameterPaths names:
    {.val {paste(parameterPaths, collapse = ', ')}}"
  )
}

# Sensitivity calculation####
messages$nonStandardPKParametersNotCalculated <- function(pkParameterNames) {
  cli::format_message(c(
    "i" = "The following PK parameters are specified but were not calculated:",
    "*" = "{.val {pkParameterNames}}"
  ))
}

messages$sensitivityAnalysisSimulationFailure <- function(
  parameterPath,
  parameterFactor
) {
  cliFormat(
    "Simulation for {.var {parameterPath}} with variation factor {.val {parameterFactor}} failed!
    The results will not be included in the sensitivity calculation."
  )
}

messages$invalidCustomFunctionParameters <- function(providedParams) {
  cliFormat(
    "The user-defined function must have either {.var x}, {.var y}, or both {.var x} and {.var y} as parameters.
    Provided parameters are: {.val {paste(providedParams, collapse = ', ')}}"
  )
}

messages$errorNotNamedList <- function(objectName, optionalMessage = "") {
  callingFunction <- ospsuite.utils:::.getCallingFunctionName()
  cliFormat(
    "{.fn {callingFunction}}: argument {.arg {objectName}} is not a named list! {optionalMessage}"
  )
}

messages$invalidVariationRangeLength <- function() {
  cliFormat(
    "{.var variationRange} must be either a vector or a list equal to the length of {.var parameterPaths}."
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
  cli::format_message(c(
    "x" = "The specified excel sheet does not contain any rows with data.",
    "*" = "Please check the excel sheet name and content and try again."
  ))
}

messages$excelUncompleteRows <- function() {
  cli::format_message(c(
    "x" = "The specified excel sheet contains uncomplete row(s)",
    "i" = "Using only complete rows to define population parameters"
  ))
}

messages$excelNoCompleteRows <- function() {
  cli::format_message(c(
    "x" = "The specified excel sheet does not contain any complete row",
    "*" = "Please fill all the columns and try again."
  ))
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

messages$abortedByUser <- function() {
  cliFormat(
    "Aborted by user."
  )
}

messages$cannotPromptNonInteractive <- function() {
  cliFormat(
    "The destination already contains an esqlabsR project and cannot prompt \\
    in a non-interactive session. Pass {.code overwrite = TRUE} to overwrite it."
  )
}

messages$failedToCopyTemplate <- function(paths) {
  cliFormat(
    "Failed to copy {length(paths)} template file{?s} to the destination: \\
    {.file {paths}}."
  )
}

messages$errorPIDatasetNotFound <- function(datasetName, availableDatasets) {
  cli::format_message(c(
    "x" = "Dataset {.val {datasetName}} not found",
    "i" = "Available datasets: {.val {paste(availableDatasets, collapse = ', ')}}"
  ))
}

messages$errorPIInvalidBounds <- function(paramPath, min, start, max) {
  cliFormat(
    "Parameter {.val {paramPath}} has invalid bounds: Min={.val {min}}, Start={.val {start}}, Max={.val {max}}.
    Expected: Min <= Start <= Max"
  )
}

messages$errorPIRequiredField <- function(field, recordType, recordId) {
  cliFormat(
    "Required field {.val {field}} is missing or empty on {recordType} {.val {recordId}}."
  )
}

messages$errorPIEmptyList <- function(field, taskId) {
  cliFormat(
    "Field {.val {field}} on PITask {.val {taskId}} must contain at least one entry."
  )
}

messages$errorPIScenariosEmpty <- function(recordType, recordId) {
  cliFormat(
    "Field {.code scenarios} on {recordType} {.val {recordId}} must be a non-empty character vector."
  )
}

messages$errorPIInvalidNumericField <- function(field, recordId, value) {
  cliFormat(
    "Field {.code {field}} on PIOutputMapping {.val {recordId}} is invalid: \\
    {.val {value}}. Expected a finite numeric value."
  )
}

messages$errorPIInvalidScaling <- function(recordId, value) {
  cliFormat(
    "Field {.code scaling} on PIOutputMapping {.val {recordId}} is invalid: \\
    {.val {value}}. Expected a non-empty string."
  )
}

messages$errorPIWrongElementType <- function(
  field,
  index,
  taskId,
  expectedClass
) {
  cliFormat(
    "Element {field}[[{index}]] on PITask {.val {taskId}} must be a {expectedClass}."
  )
}

messages$errorPIOutputQuantityNotFound <- function(path, simulationName) {
  cliFormat(
    "Output quantity {.path {path}} not found in simulation {.val {simulationName}}.
    Check that the output path exists in the simulation."
  )
}

messages$errorPIParameterNotFound <- function(path, simulationName) {
  cliFormat(
    "Parameter {.path {path}} not found in simulation {.val {simulationName}}.
    Check that the parameter path is correct and exists in the simulation."
  )
}

messages$errorPIScenarioNotFound <- function(scenarioName, availableScenarios) {
  cli::format_message(c(
    "x" = "Scenario {.val {scenarioName}} referenced in PI task configuration not found",
    "i" = "Available scenarios: {.val {paste(availableScenarios, collapse = ', ')}}"
  ))
}

messages$messageBuildingPITask <- function(piTaskName) {
  cliFormat("Building PI task: {.val {piTaskName}}")
}

messages$messageRunningPITask <- function(piTaskName) {
  cliFormat("Running PI task: {.val {piTaskName}}")
}

# Observed data (Chapter 5) ####
messages$observedDataInvalidEntryType <- function(badType, validTypes) {
  cli::format_message(c(
    "x" = "Invalid {.field type} {.val {badType}} in {.code observedData} entry.",
    "i" = "Must be one of: {.val {validTypes}}."
  ))
}

messages$observedDataMissingField <- function(entryIndex, type, field) {
  cli::format_message(c(
    "x" = "{.code observedData} entry {entryIndex} (type {.val {type}}) is missing required field {.field {field}}."
  ))
}

messages$observedDataFileNotFound <- function(filePath) {
  cli::format_message(c(
    "x" = "Observed-data source file not found: {.path {filePath}}."
  ))
}

messages$observedDataScriptWrongReturnType <- function(filePath, klass) {
  cli::format_message(c(
    "x" = "Script {.path {filePath}} did not return a {.cls DataSet} or list of {.cls DataSet}.",
    "i" = "Got an object of class {.cls {klass}}."
  ))
}

messages$observedDataDataFolderNotDeclared <- function(file) {
  cliFormat(
    "{.field dataFolder} is not declared in {.code filePaths}; cannot resolve {.path {file}}."
  )
}

messages$observedDataNameCollision <- function(duplicates) {
  cli::format_message(c(
    "x" = "Duplicate observed-data set name{?s} across sources: {.val {duplicates}}.",
    "i" = "Each loaded {.cls DataSet} must have a unique name; rename the source or the data set."
  ))
}

# Observed-data messages surfaced by the project validator (`validateProject()`)
# rather than by the load/add path. These are stored verbatim as the `message`
# of a `validationResult` entry (a plain string, not a `cli`-tagged vector), so
# they interpolate the ids as plain text (single-quoted to match the rest of the
# validator's wording) instead of styling them with `cli` `{.val}` markup.
messages$validationObservedDataMissingType <- function(entryLabel) {
  cliFormat("{entryLabel} is missing required field 'type'")
}

messages$validationObservedDataInvalidType <- function(
  entryLabel,
  type,
  validTypes
) {
  cliFormat(
    "{entryLabel} has invalid type '{type}'. Must be one of: {paste(validTypes, collapse = \", \")}"
  )
}

messages$validationObservedDataMissingField <- function(
  entryLabel,
  type,
  field
) {
  cliFormat("{entryLabel} ({type}) is missing required field '{field}'")
}

messages$validationObservedDataFileNotFound <- function(entryLabel, file) {
  cliFormat("{entryLabel} references non-existent file: {file}")
}

messages$validationObservedDataImporterNotFound <- function(
  entryLabel,
  importerConfiguration
) {
  cliFormat(
    "{entryLabel} references non-existent importer config: {importerConfiguration}"
  )
}
