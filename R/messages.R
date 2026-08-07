messages <- ospsuite.utils::messages

# `messages$` is the central catalog for this package's user-facing error,
# warning, and message text: each entry is a builder returning a formatted
# string (or a `cli`-tagged character vector). Sites raise a catalog entry
# through the `cli` wrappers `cli::cli_abort()` (errors) and `cli::cli_warn()`
# (warnings); `cli::cli_inform()` surfaces informational text. New user-facing
# text belongs here as a catalog entry routed through those wrappers, not as a
# base `stop()`/`warning()`/`message()` on an inline literal string.
#
# An entry is built one of three ways, and the choice is capability, not taste.
#
#   * `cliFormat()` by default, for a single-line message. It is
#     `cli::format_inline(paste(..., sep = "\n"))`, so it formats one inline
#     string: interpolation, the inline classes (`{.file}`, `{.fn}`, `{.val}`,
#     ...), pluralization (`{?s}`, `{cli::qty()}`) and collapsing (`{.or}`) all
#     work. A long template still wraps across physical lines with a trailing
#     `\\`; `cliFormat()` leaves that continuation in the string it returns, and
#     the raising `cli_abort()` / `cli_warn()` resolves it on its own pass.
#   * `cli::format_message()` when the message carries `"i"` / `"x"` / `"!"` /
#     `"*"` bullets. This is the one thing `cliFormat()` cannot express: it drops
#     the names of a `cli` vector and glues the elements into one run-on line.
#   * The raw templated vector, returned unglued, when a value interpolated into
#     it may itself contain `{` or `}`. Both helpers above glue eagerly, and the
#     raising call then glue-parses the finished string a second time, so such a
#     value would be evaluated as an R expression. Returning the template
#     instead means the raising call interpolates exactly once, from its own
#     frame, which is why those entries name their parameters after the
#     variables bound at the raising site (`restoreDirNotEmpty()`'s `dir`,
#     `unsupportedSchemaVersion()`'s `version`).
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
messages$wrongXLSStructure <- function(
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

messages$missingValuesInParameters <- function(
  filePath,
  parameterPaths
) {
  cliFormat(
    "Missing or non-numeric values in parameters file {.file {filePath}} for parameter(s): {.val {paste(parameterPaths, collapse = ', ')}}. A numeric value must be specified for all parameters."
  )
}

messages$duplicateParameters <- function(
  filePath,
  parameterPaths
) {
  cliFormat(
    "Duplicate parameter path(s) in parameters file {.file {filePath}}: {.val {paste(parameterPaths, collapse = ', ')}}. Only the last value defined for each path is used."
  )
}

messages$missingUnitsInInitialConditions <- function(
  filePath,
  moleculePaths
) {
  cliFormat(
    "Missing units in initial values file {.file {filePath}} for molecule(s): {.val {paste(moleculePaths, collapse = ', ')}}. Units must be specified for all molecule initial values."
  )
}

messages$missingValuesInInitialConditions <- function(
  filePath,
  moleculePaths
) {
  cliFormat(
    "Missing or non-numeric values in initial values file {.file {filePath}} for molecule(s): {.val {paste(moleculePaths, collapse = ', ')}}. A numeric value must be specified for all present molecules."
  )
}

messages$invalidIsPresentInInitialConditions <- function(
  filePath,
  moleculePaths
) {
  cliFormat(
    "Invalid 'Is Present' values in initial values file {.file {filePath}} for molecule(s): {.val {paste(moleculePaths, collapse = ', ')}}. 'Is Present' must be a logical value (TRUE/FALSE), numeric 1/0 (present/not present), or empty."
  )
}

messages$missingPathInInitialConditions <- function(
  filePath,
  sheet,
  rows
) {
  cliFormat(
    "Missing {.field Container Path} or {.field Molecule Name} in initial values file {.file {filePath}}, sheet {.val {sheet}}, data row(s): {.val {paste(rows, collapse = ', ')}}."
  )
}

messages$duplicateInitialConditions <- function(
  filePath,
  moleculePaths
) {
  cliFormat(
    "Duplicate molecule path(s) in initial values file {.file {filePath}}: {.val {paste(moleculePaths, collapse = ', ')}}. Only the last value defined for each path is used."
  )
}

# Enum####
messages$enumPutListMultipleKeys <- function() {
  cliFormat("Trying to put multiple keys, but only one key is allowed!")
}

# populations ####
messages$distributionNotSupported <- function(string) {
  cliFormat(
    "The distribution {.val {string}} is not supported. Supported distributions are listed in {.var Distributions}."
  )
}

messages$wrongOntogenyStructure <- function(entry) {
  cliFormat(
    "Wrong structure provided for the protein ontogeny specification.
    Expected is a pair of {.cls ProteinName:Ontogeny}, but the entry is: {.val {entry}}"
  )
}

# Raised when an authoring call is handed something other than the accepted
# `proteinOntogenies` shape. It names the shape, because the field is aligned
# across ids like every other one, so a length-based error would report a
# mismatch against the number of ids, describing neither the value nor the fix.
messages$invalidProteinOntogenies <- function(given) {
  cli::format_message(c(
    "{.arg proteinOntogenies} must be a character vector of {.val Protein:Ontogeny} entries.",
    "x" = "It is {given}.",
    "i" = "One entry per ontogeny, e.g. {.code c(\"CYP3A4:CYP3A4\", \"CYP2D6:CYP2C8\")}, or the same pairs as one comma-joined string."
  ))
}

# Warned from the Excel import when a sheet declares protein ontogenies that
# cannot be read as protein/ontogeny pairs. The legacy two-column spelling pairs
# the two cells positionally, so an unmatched count means one of the two values
# would be dropped; saying so is the difference between a reported gap and
# ontogenies vanishing from the imported project.
messages$excelOntogeniesNotReadable <- function(
  recordType,
  recordId,
  proteins,
  ontogenies
) {
  cli::format_message(c(
    "!" = "The protein ontogenies of {recordType} {.val {recordId}} are not imported: the {.field Protein} and {.field Ontogeny} columns cannot be paired.",
    "i" = "{length(proteins)} protein{?s} against {length(ontogenies)} ontogen{?y/ies}; each protein needs exactly one ontogeny.",
    "i" = "Fix the workbook, or write the pairs into a single {.field Protein Ontogenies} cell as {.val Protein:Ontogeny,Protein:Ontogeny}."
  ))
}

# utilities####
messages$fileNotFound <- function(filePath) {
  cliFormat("File not found: {.file {filePath}}")
}

# Raised from `.assertNotLfsPointer()`. Unglued so a path containing `{` or `}`
# is interpolated exactly once, by the raising call, where the value is bound
# under `path`.
messages$gitLfsPointerFile <- function(path) {
  c(
    "x" = "{.file {path}} is a Git LFS pointer file, not the file it stands \\
    for.",
    "i" = "Fetch the real file with {.code git lfs pull}, then try again."
  )
}

# Raised from `.loadProjectTree()`. Unglued for the same reason as
# `legacySnapshotNotLoadable()` below: a hand-edited `schemaVersion` containing
# `{` or `}` would be glue-parsed a second time by the raising `cli_abort()` and
# fail with a glue error instead of this message. The value is bound under
# `version` in the raising frame.
messages$unsupportedSchemaVersion <- function(version) {
  "Unsupported schemaVersion: {.val {version}}. Expected {.val 2.0}."
}

messages$invalidPathArgument <- function() {
  cliFormat("{.arg path} must be a single non-empty, non-NA string.")
}

messages$saveProjectNoTree <- function() {
  c(
    "This project does not have a project folder on disk yet, so it cannot be saved.",
    "i" = "Use {.fn snapshotProject} to save it to a single file.",
    "i" = "Or create a project folder with {.fn initProject} and load it with {.fn loadProject}."
  )
}

messages$reloadProjectNoTree <- function() {
  c(
    "This project does not have a project folder on disk, so there is nothing to reload.",
    "i" = "{.fn reloadProject} re-reads the project files from disk; this project was not loaded from a folder."
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

# The `{name}` placeholder is the validated value, not `snapshotProject()`'s
# `name` argument (`{.arg name}` above renders that literally). It is spelled
# `name` because `.validateFilenameSegment()` raises this from a frame where the
# value is bound under that name. `given` is that helper's description of a value
# that is no name at all (and `NULL` for a well-formed name that would escape the
# directory), because quoting an empty string, an `NA` or a number as if it
# "contained a path separator" says something untrue about it. Both placeholders
# stay unglued, so the raising call interpolates each exactly once, from the frame
# that binds them.
messages$invalidSnapshotName <- function(name, given) {
  c(
    "{.arg name} must be a single filename stem without path separators.",
    "x" = if (is.null(given)) {
      "The stem {.val {name}} contains a path separator or is {.val .} / \\
      {.val ..}, so it could write outside {.arg dir}."
    } else {
      "It is {given}, not a single non-empty string."
    },
    "i" = "Pass a single filename segment (no path separator and not {.val .} / \\
    {.val ..}), or leave {.arg name} as {.code NULL} for a timestamped default."
  )
}

messages$invalidProjectFileName <- function(name, given) {
  c(
    "{.arg projectFileName} must be a single filename without path separators.",
    "x" = if (is.null(given)) {
      "The name {.val {name}} contains a path separator or is {.val .} / \\
      {.val ..}, so it could write outside {.arg outputDir}."
    } else {
      "It is {given}, not a single non-empty string."
    },
    "i" = "Pass a single filename segment, for example {.val Project.json} \\
    (the default) or {.val MyStudy}; a {.field .json} extension is appended \\
    when the name does not already end in one."
  )
}

# `definitionsFolder` names one folder directly under the project directory, and
# it drives the two paths that delete files (`.writeDefinitionTree()` owns its
# `<kind>/` subfolders, `.clearProjectArtifacts()` unlinks it recursively), so a
# separator or `..` in it would point both outside the project. Same two-argument
# contract as `invalidProjectFileName` above.
messages$invalidDefinitionsFolder <- function(name, given) {
  c(
    "{.field definitionsFolder} must be a single folder name directly under \\
    the project folder.",
    "x" = if (is.null(given)) {
      "The name {.val {name}} contains a path separator or is {.val .} / \\
      {.val ..}, so it could point outside the project folder."
    } else {
      "It is {given}, not a single non-empty string."
    },
    "i" = "Pass a plain folder name, for example {.val definitions} (the \\
    default) or {.val defs}."
  )
}

messages$restoreDirNotEmpty <- function(dir) {
  c(
    "The folder {.path {dir}} is not empty.",
    "i" = "{.fn restoreProject} needs an empty or new folder. Pass \\
    {.code overwrite = TRUE} to replace the folder's contents, or choose a \\
    different folder."
  )
}

messages$restoreOverwroteTree <- function(dir) {
  c(
    "Replaced the project in {.path {dir}} with the snapshot.",
    "!" = "Project objects loaded from this folder before the restore still \\
    contain the old project.",
    "i" = "Continue with the project returned by {.fn restoreProject}, or \\
    call {.fn reloadProject} on the old object."
  )
}

messages$importWouldOverwriteProject <- function(outputDir) {
  c(
    "A JSON project already exists in {.path {outputDir}}.",
    "x" = "Re-importing replaces it with the Excel project and deletes any \\
    definitions that exist only on the JSON side.",
    "i" = "Pass {.code overwrite = TRUE} to replace the existing JSON project \\
    with the Excel state, or import into a different {.arg outputDir}."
  )
}

messages$importCopiedAssetFolders <- function(folders) {
  cliFormat(
    "Copied {length(folders)} referenced folder{?s} into the new project: {.file {folders}}."
  )
}

messages$importUncopiedAssetFolders <- function(folders) {
  cliFormat(
    "{length(folders)} folder{?s} named by the project configuration {cli::qty(length(folders))}{?was/were} not copied: {.file {folders}}.",
    "Each is absent from the Excel project, points outside it, or already holds files in the new project (pass {.code overwrite = TRUE} to replace those).",
    "A definition pointing into one will not resolve until you place the folder in the new project."
  )
}

# Raised by `.appendParameterSets()` when a workbook's sheet reuses a
# parameter-set id already taken, whether by an earlier workbook or by another
# sheet of the same workbook, so the wording names neither. `bullets` are the
# pre-bound `old -> new` templates from `.canonicalizedIdBullets()`; the caller
# binds `sourceLabel` and `renamedCount` into the same environment and passes it
# as `.envir`, so the whole message is glue-parsed exactly once and a sheet name
# containing `{` is never evaluated.
#
# `cli::qty()` re-arms the plural quantity after each interpolation: cli binds a
# `{?}` marker to the nearest preceding substitution, so the length-1
# `{.file {sourceLabel}}` would otherwise force every later marker in its bullet
# to the singular no matter how many sets were renamed.
messages$importRenamedDuplicateParameterSets <- function(bullets) {
  c(
    "!" = "{renamedCount} parameter set{?s} in {.file {sourceLabel}} \\
    {cli::qty(renamedCount)}reuse{?s/} an id that is already taken, so \\
    {?it was/they were} renamed:",
    bullets,
    "i" = "The three former parameter-set kinds now share one \\
    {.field parameterSets} namespace, so one sheet name cannot serve two sets. \\
    References made in {.file {sourceLabel}} point at the \\
    {cli::qty(renamedCount)}renamed set{?s}; rename the sheet in Excel to \\
    choose the id yourself."
  )
}

messages$importSkippedObservedData <- function(dataFile) {
  c(
    "!" = "The configured data file {.file {dataFile}} was not found, so no \\
    observed data was imported.",
    "i" = "Any plot or parameter-identification mapping that references \\
    observed data will not resolve, and {.fn validateProject} will report it, \\
    until the data file is present."
  )
}

messages$importIncompleteObservedCurves <- function(dataCombinedIds) {
  # Unglued, like the sheet-skip warnings below: a definition id is free text.
  envir <- new.env(parent = parent.frame())
  assign("ids", dataCombinedIds, envir = envir)
  assign("n", length(dataCombinedIds), envir = envir)
  list(
    bullets = c(
      "!" = "{cli::qty(n)}{n} imported data {?combination/combinations} \\
      {?has/have} an observed curve that names no data set: {.val {ids}}.",
      "i" = "The {.field DataCombined} sheet marked the row {.val observed} but \\
      left its {.field dataSet} cell empty, so there is nothing to resolve \\
      against. The row is kept as it was authored, not dropped.",
      "i" = "{.fn validateProject} reports each one as a critical error until \\
      the cell is filled in Excel and the project imported again, or the curve \\
      is completed with {.fn addDataCombined}."
    ),
    envir = envir
  )
}

# The two boundaries are different sizes and take different remedies, so each
# gets its own message rather than one wording stretched over both: a `dataFile`
# that escapes `dataFolder` is very often still inside the project, where
# "outside the project folder" would be plainly untrue and "copy it under the
# project folder" would describe a state that already holds. Neither message
# names the offending path: it is absolute in the `dataFolder` case, so quoting
# it would put the user's account name in the output.

messages$importSkippedOutOfProjectDataFolder <- function() {
  c(
    "!" = "{.field dataFolder} points outside the project folder, so no \\
    observed data was imported.",
    "i" = "Data kept outside the project (a synced drive shared between \\
    projects) is named with a {.code ${{VAR}}} environment variable, which \\
    resolves from wherever the project is opened. Set one, or copy the data \\
    under the project folder, then import again.",
    "i" = "Any plot or parameter-identification mapping that references \\
    observed data will not resolve, and {.fn validateProject} will report it, \\
    until then."
  )
}

messages$importSkippedOutOfProjectDataFile <- function() {
  c(
    "!" = "{.field dataFile} points outside {.field dataFolder}, so no \\
    observed data was imported.",
    "i" = "The loader resolves {.field dataFile} under {.field dataFolder}. \\
    Move the file under that folder, or point {.field dataFolder} at the \\
    folder that holds it, then import again.",
    "i" = "Any plot or parameter-identification mapping that references \\
    observed data will not resolve, and {.fn validateProject} will report it, \\
    until then."
  )
}

messages$importIncompletePIOutputMappings <- function(taskId, scenarios) {
  # Unglued, like the two below: a task id and a scenario name are both free text.
  envir <- new.env(parent = parent.frame())
  assign("taskId", taskId, envir = envir)
  assign("scenarios", scenarios, envir = envir)
  assign("n", length(scenarios), envir = envir)
  list(
    bullets = c(
      "!" = "{cli::qty(n)}{n} output {?mapping/mappings} of \\
      parameter-identification task {.val {taskId}} {?has/have} no output path.",
      stats::setNames(
        sprintf(
          "Scenarios cell {.val {scenarios[[%1$d]]}}.",
          seq_along(scenarios)
        ),
        rep("x", length(scenarios))
      ),
      "i" = "This {.field PIOutputMappings} sheet has no {.field OutputPath} \\
      column, so each mapping takes its outputs from the {.field OutputPathsIds} \\
      of the scenarios it names. Give those scenarios an output path, or add an \\
      {.field OutputPath} column, then import again.",
      "i" = "The mappings are kept as they were authored, so {.fn validateProject} \\
      reports each one until then; {.fn addPIOutputMapping} with \\
      {.code overwrite = TRUE} completes one in place."
    ),
    envir = envir
  )
}

messages$importUnmergedPIParameterGroups <- function(taskId, paths) {
  # Unglued, like the entry above: a task id and a parameter path are free text.
  envir <- new.env(parent = parent.frame())
  assign("taskId", taskId, envir = envir)
  assign("paths", paths, envir = envir)
  assign("n", length(paths), envir = envir)
  list(
    bullets = c(
      "!" = "{cli::qty(n)}{n} {.field Group}{?s} of parameter-identification \\
      task {.val {taskId}} {?does/do} not agree on the bounds, so {?its/their} \\
      rows were imported as one parameter each: {.val {paths}}.",
      "i" = "The rows of one {.field Group} are one parameter estimated across \\
      the scenarios they name, so they share one {.field MinValue}, \\
      {.field MaxValue} and {.field StartValue}. Make them agree in Excel and \\
      import again to estimate them together.",
      "i" = "As imported, each row is estimated independently, which is not what \\
      the group asked for."
    ),
    envir = envir
  )
}

messages$importMissingWorkbooks <- function(files) {
  # Unglued: a workbook filename comes from the property sheet, so it is free text.
  envir <- new.env(parent = parent.frame())
  assign("files", files, envir = envir)
  assign("n", length(files), envir = envir)
  list(
    bullets = c(
      "!" = "{cli::qty(n)}{n} workbook{?s} named by the project configuration \\
      {?is/are} not in the configurations folder, so {?its/their} section{?s} \\
      {?was/were} imported as empty: {.file {files}}.",
      "i" = "{cli::qty(n)}Place the workbook{?s} beside the others and import \\
      again, or clear the property row{?s} if the section is deliberately empty."
    ),
    envir = envir
  )
}

messages$importUnconsumedWorkbooks <- function(files) {
  # Unglued: a workbook filename found on disk is free text.
  envir <- new.env(parent = parent.frame())
  assign("files", files, envir = envir)
  assign("n", length(files), envir = envir)
  list(
    bullets = c(
      "!" = "{cli::qty(n)}{n} workbook{?s} in the configurations folder \\
      {?was/were} not read: {.file {files}}.",
      "i" = "Only the workbooks the project configuration names are imported, \\
      whatever else the folder holds, so nothing in {cli::qty(n)}{?it/them} is \\
      in the project.",
      "i" = "Point the matching property row at one to import it, or leave it \\
      where it is if it is a spare copy."
    ),
    envir = envir
  )
}

messages$importEmptySections <- function(sections) {
  cliFormat(
    "{length(sections)} section{?s} imported no definitions: {.field {sections}}."
  )
}

messages$importUndeclaredWorkbooks <- function(files) {
  # Unglued: a workbook filename is free text.
  envir <- new.env(parent = parent.frame())
  assign("files", files, envir = envir)
  assign("n", length(files), envir = envir)
  list(
    bullets = c(
      "i" = "{cli::qty(n)}{n} workbook{?s} {?was/were} read under {?its/their} \\
      conventional name, which the project configuration does not name: \\
      {.file {files}}.",
      "i" = "Rename or remove {cli::qty(n)}{?it/them} to keep \\
      {cli::qty(n)}{?it/them} out of the project."
    ),
    envir = envir
  )
}

messages$importUnparametrizedIndividuals <- function(filePath, ids, sheets) {
  # Unglued, like its neighbours: an individual id and a sheet name are free text.
  envir <- new.env(parent = parent.frame())
  assign("filePath", filePath, envir = envir)
  assign("ids", ids, envir = envir)
  assign("sheets", sheets, envir = envir)
  assign("n", length(ids), envir = envir)
  list(
    bullets = c(
      "!" = "{cli::qty(n)}{n} individual{?s} in {.file {filePath}} {?is/are} \\
      imported without the parameter set {?it/they} declare{?s/}, because the \\
      sheet did not parse as a parameter sheet:",
      stats::setNames(
        sprintf(
          "Individual {.val {ids[[%1$d]]}}: sheet {.val {sheets[[%1$d]]}}.",
          seq_along(ids)
        ),
        rep("x", length(ids))
      ),
      "i" = "Nothing else reports this: the individual imports without the \\
      parametrization and the project still validates, so every scenario using \\
      it runs on the unmodified model. Fix the sheet's columns in Excel and \\
      import again."
    ),
    envir = envir
  )
}

messages$importUndefinedIndividualSheets <- function(filePath, sheets) {
  # Unglued, like its neighbours: a sheet name is free text.
  envir <- new.env(parent = parent.frame())
  assign("filePath", filePath, envir = envir)
  assign("sheets", sheets, envir = envir)
  assign("n", length(sheets), envir = envir)
  list(
    bullets = c(
      "!" = "{cli::qty(n)}{n} parameter {?sheet/sheets} of {.file {filePath}} \\
      {?names/name} an individual that {.field IndividualBiometrics} does not \\
      define, so that individual is not imported: {.val {sheets}}.",
      "i" = "An individual comes from its {.field IndividualBiometrics} row, so \\
      the sheet is imported as a parameter set that no individual uses, and the \\
      {cli::qty(n)}scenario{?s} naming the individual {?has/have} nothing to \\
      resolve to.",
      "i" = "Add a biometrics row for it in Excel and import again."
    ),
    envir = envir
  )
}

messages$importUnimportedPIObservedDataSheets <- function(sheets) {
  # Unglued: a sheet name is free text.
  envir <- new.env(parent = parent.frame())
  assign("sheets", sheets, envir = envir)
  assign("n", length(sheets), envir = envir)
  list(
    bullets = c(
      "!" = "{cli::qty(n)}{n} {.field ObservedDataSheet} {?sheet/sheets} named \\
      by the {.field PIOutputMappings} sheet {?is/are} not among the imported \\
      observed data: {.val {sheets}}.",
      "i" = "Observed data is imported from the project's data file, one \\
      definition listing that workbook's sheets, so a sheet that is not in it \\
      holds no data set the project can resolve; the mapping's \\
      {.field DataSet} will not resolve either, and {.fn validateProject} \\
      reports it.",
      "i" = "Point {.field dataFile} at the workbook that holds \\
      {cli::qty(n)}{?it/them} and import again."
    ),
    envir = envir
  )
}

messages$importSkippedPlotExportConfiguration <- function(
  filePath,
  rows,
  grids
) {
  # Unglued: a grid name is free text.
  envir <- new.env(parent = parent.frame())
  assign("filePath", filePath, envir = envir)
  assign("rows", rows, envir = envir)
  assign("grids", grids, envir = envir)
  assign("n", length(grids), envir = envir)
  list(
    bullets = c(
      "!" = "The {.field exportConfiguration} sheet of {.file {filePath}} is \\
      not imported: {cli::qty(rows)}{rows} {?row/rows} describing where a plot \\
      grid's figure is written and at what size.",
      if (length(grids) > 0L) {
        c("x" = "{cli::qty(n)}Plot grid{?s} {.val {grids}}.")
      },
      "i" = "A project has no field for a figure's filename, width or height: \\
      {.fn createPlots} returns the plot objects and the caller saves them, for \\
      example with {.fn ggplot2::ggsave}, so the settings are not imported and \\
      an export does not write them back."
    ),
    envir = envir
  )
}

messages$importDuplicatePlotIds <- function(sheet, idField, ids) {
  # Unglued, like its neighbours: a plot id is free text.
  envir <- new.env(parent = parent.frame())
  assign("sheet", sheet, envir = envir)
  assign("idField", idField, envir = envir)
  assign("ids", ids, envir = envir)
  assign("n", length(ids), envir = envir)
  list(
    bullets = c(
      "!" = "{cli::qty(n)}{n} {.field {idField}}{?s} {?is/are} used by more than \\
      one row of the {.field {sheet}} sheet: {.val {ids}}.",
      "i" = "Each id is one definition, built from the last row that carries it, \\
      so the earlier {cli::qty(n)}row{?s} {?is/are} not imported.",
      "i" = "Give every row its own id in Excel and import again to keep them all."
    ),
    envir = envir
  )
}

messages$importSkippedNonNumericRows <- function(
  filePath,
  sheets,
  rows,
  values
) {
  # Same unglued `bullets`/`envir` contract, and for the same reason, as
  # `importSkippedNonParameterSheets()` below: a sheet name and a cell's own
  # text are both free text that can contain `{`/`}`.
  envir <- new.env(parent = parent.frame())
  assign("filePath", filePath, envir = envir)
  assign("sheets", sheets, envir = envir)
  assign("rows", rows, envir = envir)
  assign("values", values, envir = envir)
  assign("n", length(rows), envir = envir)
  list(
    bullets = c(
      "!" = "{cli::qty(n)}Skipped {n} {?row/rows} in {.file {filePath}}: \\
      the {.field Value} cell is not a number.",
      # One bullet per skipped row rather than one summary line: a row has to be
      # findable in the workbook to be fixed. Each bullet indexes the vectors in
      # `envir` rather than embedding the cell's text, the same way
      # `.canonicalizedIdBullets()` keeps user text behind a variable.
      stats::setNames(
        sprintf(
          "Sheet {.val {sheets[[%1$d]]}}, row {rows[[%1$d]]}: \\
          {.val {values[[%1$d]]}}.",
          seq_along(rows)
        ),
        rep("x", length(rows))
      ),
      "i" = "A blank cell is allowed; a non-blank cell must be numeric \\
      (use {.val .} as the decimal separator)."
    ),
    envir = envir
  )
}

messages$importSkippedNonParameterSheets <- function(
  filePath,
  sheets,
  columns
) {
  # A sheet name is free text, so it can contain `{`/`}` (`Fit {old}`, `PK
  # {2019}`). Returns the templates still unglued together with an environment
  # binding their variables, the shape `.canonicalizedIdBullets()` uses and for
  # the same reason: the caller hands both to one `cli_warn()`, so each template
  # is glue-parsed exactly once and a value is only ever reached through a
  # variable, never parsed. Pre-rendering with `cli::format_inline()` instead
  # would leave the value's braces in the rendered text for the emitting
  # `cli_warn()` to evaluate.
  envir <- new.env(parent = parent.frame())
  assign("filePath", filePath, envir = envir)
  assign("sheets", sheets, envir = envir)
  assign("columns", columns, envir = envir)
  list(
    bullets = c(
      "!" = "{cli::qty(length(sheets))}Skipped {?sheet/sheets} {.val {sheets}} \\
      in {.file {filePath}}: {cli::qty(length(sheets))}not \\
      {?a parameter sheet/parameter sheets}.",
      "i" = "A parameter sheet carries the columns {.field {columns}}."
    ),
    envir = envir
  )
}

# Raised from `.loadProjectTree()`, whose schema-version check fails on every
# file `.isLegacySnapshot()` recognizes: a previous-version snapshot carries no
# `schemaVersion` at all. The `{jsonPath}` placeholder stays unglued so
# `cli_abort()` interpolates it once, in that frame, where the value is bound
# under that name (as with `restoreDirNotEmpty()`'s `dir`); pre-gluing it here
# would leave a path containing `{` or `}` to be glue-parsed a second time by
# the raising call.
messages$legacySnapshotNotLoadable <- function(jsonPath) {
  c(
    "x" = "{.file {jsonPath}} is a previous-version project snapshot, not a \\
    project of the current format.",
    "i" = "A previous-version snapshot has to be upgraded before it can be \\
    opened.",
    "i" = "Upgrade it into a new folder with \\
    {.code restoreProject(<snapshot>, dir = <newFolder>)}, which returns the \\
    upgraded project."
  )
}

# Raised from `.resolveProjectContainerPath()` when it is handed a folder
# that holds no project container. Naming the mistake is the point: a folder
# is the natural second guess after a file, and letting it through produced a
# raw JSON parse error about the folder itself. Unglued so a folder path
# containing `{` or `}` is interpolated exactly once, by the raising call,
# where `folder` is bound.
messages$noProjectContainerInFolder <- function(folder) {
  c(
    "x" = "No esqlabsR project found in the folder {.file {folder}}.",
    "i" = "A project is a JSON container file (usually {.file Project.json}) \\
    with a {.file definitions/} folder beside it.",
    "i" = "Turn a pre-6.0.0 Excel project into one with \\
    {.fn importProjectFromExcel}, or create a new project with \\
    {.fn initProject}."
  )
}

# Raised from `.resolveProjectContainerPath()` when a folder holds several
# project containers, so there is no single project to open. `folder` and
# `names` are bound in the raising frame; unglued for the same reason as
# above.
messages$multipleProjectContainersInFolder <- function(folder, names) {
  c(
    "x" = "The folder {.file {folder}} holds {length(names)} project files: \\
    {.file {names}}.",
    "i" = "Name the one to open, for example \\
    {.code loadProject(file.path(folder, \"{names[[1]]}\"))}."
  )
}

# Unglued, so `cli_abort()` renders these as real bullets rather than re-wrapping
# one pre-formatted string with the glyphs inline. `sheet` and `columns` are bound
# in `.requireExcelColumns()`, the raising frame.
messages$excelSheetMissingRequiredColumns <- function(sheet, columns) {
  c(
    "x" = "The {.val {sheet}} sheet is missing \\
    {cli::qty(columns)}{?a required column/required columns}: \\
    {.field {columns}}.",
    "i" = "Add {cli::qty(columns)}{?it/them} to the workbook, or re-export the \\
    project with {.fn exportProjectToExcel} to get a sheet with the columns \\
    this version reads."
  )
}

# Unglued for two reasons: the bullets, and `projectConfigPath` being a path the
# user chose, which may itself contain `{` or `}` and so must be interpolated
# exactly once, by the raising call. `projectConfigPath` is bound in
# `.excelToProjectJson()`, the raising frame.
#
# The first element stays unnamed, so it is the error header rather than a
# bullet: `.projectSyncStatus()` re-raises this text as the `"x"` bullet of its
# own warning, and a glyph baked in here would render doubled there.
messages$excelProjectFileUnreadable <- function(projectConfigPath) {
  c(
    "{.path {projectConfigPath}} is not a readable Excel project file.",
    "i" = "It must be a valid {.field .xlsx} workbook (the project's \\
    {.file Project.xlsx})."
  )
}

messages$legacySnapshotMalformedSheet <- function() {
  c(
    "x" = "This previous-version project snapshot is malformed and cannot be \\
    upgraded.",
    "i" = "A sheet in the snapshot is not a {.code {{column_names, rows}}} \\
    object; the file may be truncated or hand-edited."
  )
}

messages$upgradedLegacySnapshot <- function(missingFolders = character()) {
  notice <- c(
    "i" = "Detected a previous-version project snapshot and upgraded it to the \\
    current project format.",
    "!" = "Observed data does not travel in a snapshot; add it with \\
    {.fn addObservedData} if a plot or parameter identification needs it."
  )
  if (length(missingFolders) == 0L) {
    return(notice)
  }
  c(
    notice,
    "!" = "{length(missingFolders)} referenced folder{?s} \\
    {cli::qty(length(missingFolders))}{?is/are} missing from the upgraded \\
    project: {.file {missingFolders}}.",
    "i" = "A snapshot carries only the configuration workbooks, so a model, \\
    data or population folder travels with it only when it sits beside the \\
    snapshot file at that relative path.",
    "i" = "Nothing that reads from one of them will resolve until you place the \\
    folder in the project."
  )
}

messages$exportWouldOverwriteWorkbooks <- function(outputDir) {
  c(
    "Excel workbooks already exist in {.path {outputDir}}.",
    "x" = "Exporting overwrites {.file Project.xlsx} and the \\
    {.file Configurations} workbooks, discarding any hand-edits they carry.",
    "i" = "Pass {.code overwrite = TRUE} to replace the existing workbooks, or \\
    export into a different {.arg outputDir}."
  )
}

messages$exportUndeclaredObservedData <- function(ids) {
  # Unglued: an observed-data id is a user-chosen name (typically a data-file
  # basename), so it is free text.
  envir <- new.env(parent = parent.frame())
  assign("ids", ids, envir = envir)
  assign("n", length(ids), envir = envir)
  list(
    bullets = c(
      "!" = "{cli::qty(n)}The Excel project configuration can name a single \\
      experimental-data workbook, so {n} observed-data declaration{?s} \\
      {?was/were} not written: {.val {ids}}.",
      "i" = "{cli::qty(n)}Anything that reads {?it/them} resolves only in the \\
      JSON project, not in a project re-imported from these workbooks."
    ),
    envir = envir
  )
}

messages$failedToRemoveStaleDefinitionFiles <- function(paths) {
  n <- length(paths)
  # Interpolate eagerly here, where `n` and `paths` are in scope: the
  # `cli::cli_abort()` call site does not carry these names, so a lazily
  # interpolated glue vector would fail to evaluate `{n}` / `{paths}` there.
  cli::format_message(c(
    "Failed to delete {n} outdated definition file{?s} from the {.file definitions} folder.",
    "x" = "{.file {paths}}",
    "i" = "A file that cannot be deleted comes back as a definition the next time you {.fn loadProject}; check the file permissions and delete it manually."
  ))
}

messages$overwriteDestination <- function(path) {
  cliFormat("Overwriting existing esqlabsR project in {.path {path}} ")
}

messages$failedToClearProjectArtifacts <- function(path) {
  # Interpolate eagerly here where `path` is in scope; the `cli::cli_abort()`
  # call site passes a local whose name is not `path`.
  cli::format_message(c(
    "Failed to remove a file or folder of the existing project before overwriting.",
    "x" = "{.path {path}}",
    "i" = "Overwriting first removes the old project's {.file definitions} folder and {.file Project.json}; check the permissions and remove it manually."
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
  cliFormat(
    "Added {length(scenarioNames)} scenario{?s}: {.val {scenarioNames}}"
  )
}

messages$noSimulationsFolderUsingAbsolutePath <- function(pkmlPath) {
  cli::format_message(c(
    "!" = "The project has no {.field simulationsFolder}; storing an absolute \\
    model file path.",
    "i" = "Set a {.field simulationsFolder} on the project so the scenario \\
    stores a portable relative path ({.file {pkmlPath}})."
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
  cliFormat(
    "Output path alias {.val {userAlias}} ignored: \\
    path {.val {path}} is already registered as {.val {registeredId}}."
  )
}

messages$noSimulationsFolderForRelativeModelFile <- function(
  scenarioName,
  modelFile
) {
  cli::format_message(c(
    "x" = "Cannot resolve the model file for scenario {.val {scenarioName}}.",
    "i" = "{.field modelFile} {.val {modelFile}} is relative but the project \\
    has no {.field simulationsFolder} to resolve it against."
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

messages$populationCsvNotFound <- function(
  scenarioName,
  populationId,
  fileName,
  populationsFolder
) {
  cli::format_message(c(
    "x" = "Cannot read population {.val {populationId}} for scenario \\
    {.val {scenarioName}}: {.file {fileName}} is not in the populations folder.",
    "i" = "Expected it under {.path {populationsFolder}}.",
    "i" = "Place the csv file there, or point {.field populationsFolder} at the \\
    folder that holds it."
  ))
}

messages$populationIdResolvedTwoWays <- function(
  populationId,
  scenarioName,
  effectiveType
) {
  cli::format_message(c(
    "!" = "Population {.val {populationId}} resolves to more than one \\
    population in this run.",
    "i" = "Scenario {.val {scenarioName}} resolves it as {.val {effectiveType}}, \\
    another scenario in the same run resolves it differently.",
    "i" = "Each scenario gets the population it asks for. Check \\
    {.field readPopulationFromCSV} on the scenarios sharing this population."
  ))
}


messages$importedProject <- function(inputFile, outputFile) {
  cliFormat(
    "Imported {.file {inputFile}} into the JSON project {.file {outputFile}}."
  )
}

messages$restoredProjectConfiguration <- function(inputFile, outputFile) {
  cliFormat(
    "Project configuration from {.file {inputFile}} restored at {.file {outputFile}}"
  )
}

# The Excel axis of `projectStatus()`: with no Excel side-car there is nothing
# to compare the in-memory project against. The side-car is derived from the
# project file's name, so name the file that was looked for; a project with no
# folder on disk has no name to derive one from, hence the pathless variant.
messages$syncNoExcel <- function(excelPath = NULL) {
  if (is.null(excelPath)) {
    return(cli::format_inline(
      "No Excel configuration file found; nothing to compare."
    ))
  }
  cli::format_inline(
    "No Excel configuration file ({.file {fs::path_file(excelPath)}}) found \\
    next to the project; nothing to compare. Write one with \\
    {.fn exportProjectToExcel}."
  )
}

# The tree axis of `projectStatus()`: whether in-memory edits diverge from the
# on-disk `definitions/` tree (the dirty bit).
messages$syncTreeDirty <- function() {
  cli::format_inline(
    "Unsaved changes: the project has changes that are not saved to disk yet."
  )
}

messages$syncTreeClean <- function() {
  cli::format_inline(
    "No unsaved changes: the project matches the files on disk."
  )
}

messages$syncNoTree <- function() {
  cli::format_inline(
    "This project does not have a project folder on disk; there is nothing to compare."
  )
}

messages$invalidArgumentLength <- function(noOfOutpaths, noOfScenarios) {
  cli::format_message(c(
    "Invalid argument length:",
    "x" = "outputPaths must have length 1 or same length as pkmlFilePaths",
    "i" = "outputPaths has length {.val {noOfOutpaths}}, pkmlFilePaths has length {.val {noOfScenarios}}"
  ))
}

messages$valueWithinThresholdNotExisting <- function(
  value,
  threshold,
  optionalMessage = ""
) {
  cliFormat(
    "value {.val {value}} not found in the array within the absolute threshold of {.val {threshold}}. {optionalMessage}"
  )
}

# data-utils ####
messages$invalidMeanMethod <- function() {
  cliFormat(
    "Invalid value for argument {.arg method}, supported values are {.val arithmetic} or {.val geometric}"
  )
}

messages$outputMolWeightNeeded <- function() {
  cliFormat(
    "{.arg outputMolWeight} can not be {.val NULL} when data sets have different molWeights"
  )
}

messages$offsetUnitsNotDefined <- function(rows) {
  cliFormat(
    "Error in DataCombined {.arg {rows}}: If x/yOffsets is set, then x/yOffsetsUnits must be defined as well. "
  )
}

# plots ####
messages$nrOfColorsShouldBePositive <- function(nrOfColors) {
  cliFormat(
    "nrOfColors must be positive, value {.val {nrOfColors}} is not valid!"
  )
}

messages$plotIDsMustBeUnique <- function(duplicated_plotIDs = "") {
  duplicates <- paste(duplicated_plotIDs, collapse = ", ")
  cliFormat(
    "plotId must be unique in plotConfiguration, but the following plotIds are duplicated: {.val {duplicates}}"
  )
}

messages$plotGridsNamesMustBeUnique <- function(
  duplicated_plotGridsNames = ""
) {
  cliFormat(
    "PlotGrids names must be unique in PlotGridConfiguration, but the following names are duplicated:
    {.val {paste(duplicated_plotGridsNames, collapse = \"\n\")}}"
  )
}

messages$unknownPlotConfiguration <- function(name) {
  cliFormat("Unknown plot configuration option: {.arg {name}}")
}

# scenario####
messages$applicationProtocolNotFound <- function(
  scenarioName,
  applicationProtocol
) {
  cliFormat(
    "Application protocol {.var {applicationProtocol}} defined in scenario {.var {scenarioName}} not found
    in the excel file {.file ApplicationProtocols.xlsx}"
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

messages$noIndividualCharacteristics <- function(
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

messages$populationNotFoundForScenario <- function(populationId, scenarioName) {
  cliFormat(
    "Population {.val {populationId}} referenced by scenario {.val {scenarioName}} not found in project."
  )
}

messages$unknownScenarioNames <- function(unknownNames) {
  cliFormat(
    "Unknown scenario names: {.val {unknownNames}}."
  )
}

# Raised when a `Scenario` record is passed where an id goes, together with field
# arguments. The record carries every field, so an accompanying one would be
# quietly overruled by it; the message names the two ways forward instead.
messages$scenarioRecordWithFields <- function(fields) {
  cli::format_message(c(
    "A {.cls Scenario} passed as {.arg id} carries every field, so \\
    {.arg {fields}} cannot be given alongside it.",
    "i" = "Edit the record's own field and pass it back \\
    ({.code sc$modelFile <- \"other.pkml\"}), or name the scenario's id instead \\
    of the record."
  ))
}

# `setScenario()` intercepts `overwrite` rather than letting it reach the impl's
# generic unknown-field message, because the name is a habit picked up from
# `addScenario()` and the scenario carries a second argument that starts the
# same way.
messages$setScenarioNoOverwrite <- function() {
  c(
    "{.fn setScenario} has no {.arg overwrite} argument.",
    "i" = "It always updates the scenario named by {.arg id}; \\
    {.fn addScenario} is the one with {.arg overwrite}.",
    "i" = "For the steady-state model option, pass \\
    {.arg overwriteFormulasInSS} in full."
  )
}

# Raised when an authoring field reaches `.alignAuthoringArgs()` without a name.
# Every field there is looked up by name, so an unnamed one would be dropped in
# silence; the usual way to produce one is passing a `set*()` field positionally.
# The hint names no function and no field: `setScenario()`, `setIndividual()` and
# `setPopulation()` all raise this, they share no field, and the abort header
# already names the one the caller used.
messages$unnamedAuthoringFields <- function(positions) {
  # `cli::qty()` pluralizes on how many positions there are, not on the position
  # numbers themselves: a lone field at position 2 is still one field. It has to
  # sit before every plural marker, since interpolating `positions` resets the
  # quantity cli pluralizes on, which is why the sentence keeps them together.
  cli::format_message(c(
    "Every field must be named.",
    "x" = "{cli::qty(length(positions))}No name on field{?s} {positions}.",
    "i" = "Pass each field as {.code name = value}."
  ))
}

# Raised when a name reaching `setScenario()`'s field set is not a scenario
# field. Without the guard the value would be aligned as a per-definition field
# and then dropped in silence.
messages$setScenarioUnknownFields <- function(fields, settable) {
  cli::format_message(c(
    "{.fn setScenario} cannot set {.field {fields}}.",
    "i" = "The settable fields are {.field {settable}}."
  ))
}

messages$invalidSimulationTimeArgument <- function() {
  cliFormat(
    "{.arg simulationTime} must be a length-3 numeric vector \\
    {.code c(start, end, resolution)}, or the same grid as a string \\
    {.val 0, 42, 48} (several intervals separated by {.val ;}). To give a \\
    different grid per id, pass a list with one element per id."
  )
}

messages$wrongTimeIntervalString <- function(timeIntervalString) {
  cliFormat(
    "The time interval string {.val {timeIntervalString}} is not valid! Please 
    check the format of the string. Following criteria must be 
    met: 1) Each time interval must contain three numbers separated by a ',', 2) all 
    numbers must be positive, 3) The first number (start time) must be smaller than 
    the second number (end time), 4) The third number (resolution) must 
    be greater than zero. Time intervals must be separated by a ';'."
  )
}

messages$scenarioMissingTimeUnit <- function(scenarioName) {
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

messages$scenarioBuildFailed <- function(scenarioName, conditionMessage) {
  # Escape braces in the underlying error text so cli does not re-interpret it
  # as glue expressions when the returned vector is passed to cli_warn().
  safe_msg <- gsub(
    "}",
    "}}",
    gsub("{", "{{", conditionMessage, fixed = TRUE),
    fixed = TRUE
  )
  c(
    "x" = cli::format_inline(
      "Could not build scenario {.val {scenarioName}}; skipping it."
    ),
    "i" = safe_msg
  )
}

# The `stopIfFails = TRUE` counterpart of `scenarioBuildFailed()`: the underlying
# error is attached as the abort's `parent`, so cli renders it below this header
# rather than it needing to be interpolated (and brace-escaped) here.
messages$scenarioBuildFailedAbort <- function(scenarioName, canSkip) {
  c(
    cli::format_inline("Could not build scenario {.val {scenarioName}}."),
    # `buildSimulations()` has no `stopIfFails`, so only name it where it exists.
    if (canSkip) {
      c(
        "i" = "Pass {.code stopIfFails = FALSE} to skip it and build the other \\
        scenarios."
      )
    }
  )
}

# Raised at the end of a `stopIfFails = FALSE` run, listing every scenario that
# produced no results. The per-scenario warnings scroll away in a batch of
# eighty, and the returned list gives every scenario an entry whatever happened
# to it, so this is what tells the caller which ones to leave out.
messages$scenariosSkipped <- function(scenarioNames) {
  c(
    "!" = "{length(scenarioNames)} of the scenarios produced no results: \\
    {.val {scenarioNames}}.",
    "i" = "Their entries carry {.code results = NULL}; select the ones that ran \\
    with {.code Filter(\\(x) !is.null(x$results), results)}."
  )
}

messages$savingScenarioResult <- function(scenarioName, conditionMessage) {
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
messages$combineInvalidDataSetName <- function(dataSetNames) {
  cliFormat(
    "The following data sets are not present in {.var observedData}:
    {.val {paste(dataSetNames, collapse =',\n')}}. Data can not be added to {.var DataCombined} object."
  )
}

# Plots.xlsx####
messages$logScaleWithZeroLimit <- function(
  plotID,
  axisLimitsField,
  axis
) {
  cliFormat(
    "Column {.field {axisLimitsField}} in plot {.val {plotID}} contains zero, but the {.val {axis}}-axis scale is set to {.val log}.
    Logarithmic scale cannot display zero values. This may result in empty or unexpected plots."
  )
}

messages$invalidPlotID <- function(plotIDs) {
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

messages$missingPlotType <- function() {
  cliFormat(
    "Missing values found in mandatory column {.val plotType} of sheet {.var plotConfiguration}. Fill in values to proceed."
  )
}

messages$missingDataCombinedName <- function() {
  cliFormat(
    "Missing values found in mandatory column {.val dataCombinedId} of sheet {.var plotConfiguration}. Fill in values to proceed."
  )
}

messages$invalidDataCombinedName <- function(dataCombinedNames) {
  cliFormat(
    "The following DataCombined are used in {.var plotConfiguration} sheet but are not present in {.var DataCombined} sheet:
    {.val {paste(dataCombinedNames, collapse = ', ')}}"
  )
}

messages$dataCombinedNamesNotFound <- function(dataCombinedNames) {
  cliFormat(
    "The following DataCombined names are not defined in the project:
    {.val {paste(dataCombinedNames, collapse = ', ')}}"
  )
}

messages$wrongOutputPath <- function(dataCombinedName, scenarioName, path) {
  cliFormat(
    "Output path {.path {path}} is defined in the DataCombined {.val {paste(dataCombinedName, collapse = \", \")}}
    for scenario {.cls {scenarioName}} but has not been simulated.
    Please check that the output path is specified for this scenario."
  )
}

messages$scenarioRunFailed <- function(
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

messages$scenarioNotInResults <- function(
  dataCombinedName,
  scenarioName
) {
  cliFormat(
    "The DataCombined {.val {paste(dataCombinedName, collapse = \", \")}} references scenario
    {.cls {scenarioName}}, but that scenario is not present in {.arg scenarioResults}.
    Check the scenario name and that it was included in the {.fn runScenarios} call."
  )
}

messages$plotGridNamesNotFound <- function(plotGridNames) {
  cliFormat(
    "The following plot grids are not defined in the project:
    {.val {paste(plotGridNames, collapse = ', ')}}"
  )
}

messages$plotIdsNotFound <- function(plotIds) {
  cliFormat(
    "The following plots are not defined in the project:
    {.val {paste(plotIds, collapse = ', ')}}"
  )
}

messages$invalidDataSetName <- function(dataSetNames) {
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

messages$absoluteVariationZeroInitialValue <- function(parameterPaths) {
  cliFormat(
    "{.code variationType = \"absolute\"} requires a non-zero initial value for every parameter, but the following parameter(s) have an initial value of 0: {.val {paste(parameterPaths, collapse = ', ')}}. Use {.code variationType = \"relative\"} or provide parameters with non-zero initial values."
  )
}

messages$sensitivityAllRunsFailed <- function(parameterPath) {
  cliFormat(
    "All simulation runs failed for {.var {parameterPath}}.
    No PK parameters could be calculated for this parameter and it will not be included in the sensitivity calculation."
  )
}

messages$invalidCustomFunctionParameters <- function(providedParams) {
  cliFormat(
    "The user-defined function must have either {.var x}, {.var y}, or both {.var x} and {.var y} as parameters.
    Provided parameters are: {.val {paste(providedParams, collapse = ', ')}}"
  )
}

messages$notNamedList <- function(objectName, optionalMessage = "") {
  cliFormat(
    "Argument {.arg {objectName}} is not a named list! {optionalMessage}"
  )
}

messages$invalidVariationRangeLength <- function() {
  cliFormat(
    "{.var variationRange} must be either a vector or a list equal to the length of {.var parameterPaths}."
  )
}

messages$sensitivityCalculationNotFound <- function(path) {
  cliFormat("Sensitivity calculation not found at path {.file {path}}.")
}

messages$noRetainedSimulationResults <- function() {
  cliFormat(
    "The sensitivity calculation contains no simulation results to save.",
    "All simulation runs appear to have failed."
  )
}

messages$outputDirExists <- function(outputDir) {
  cliFormat(
    "Directory {.file {outputDir}} already exists.",
    "Set {.code overwrite = TRUE} to replace it."
  )
}

messages$failedToLoadSimulation <- function(path, message) {
  cliFormat(
    "Failed to load simulation from saved path {.file {path}}.",
    "Please provide the {.cls Simulation} object explicitly.",
    paste0("Error: ", message)
  )
}

messages$corruptSensitivityCalculation <- function(path) {
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

messages$sensitivityPKParameterNotCalculated <- function(
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
    "The Excel configuration files do not match the project. {message}"
  )
}

messages$excelInSync <- function() {
  cliFormat(
    "The Excel configuration files match the project."
  )
}

messages$abortedByUser <- function() {
  cliFormat(
    "Aborted by user."
  )
}

messages$cannotPromptNonInteractive <- function() {
  cliFormat(
    "The destination folder already contains an esqlabsR project. R is not \\
    running interactively, so esqlabsR cannot ask for confirmation; pass \\
    {.code overwrite = TRUE} to overwrite it."
  )
}

messages$failedToCopyTemplate <- function(paths) {
  cliFormat(
    "Failed to copy {length(paths)} template file{?s} to the destination: \\
    {.file {paths}}."
  )
}

messages$PIDatasetNotFound <- function(datasetName, availableDatasets) {
  cli::format_message(c(
    "x" = "Dataset {.val {datasetName}} not found",
    "i" = "Available datasets: {.val {paste(availableDatasets, collapse = ', ')}}"
  ))
}

messages$PIInvalidBounds <- function(paramPath, min, start, max) {
  cliFormat(
    "Parameter {.val {paramPath}} has invalid bounds: Min={.val {min}}, Start={.val {start}}, Max={.val {max}}.
    Expected: Min <= Start <= Max"
  )
}

messages$PIRequiredField <- function(field, recordType, recordId) {
  cliFormat(
    "Required field {.val {field}} is missing or empty on {recordType} {.val {recordId}}."
  )
}

messages$PIMustBeList <- function(field, taskId) {
  cliFormat(
    "Field {.val {field}} on PITask {.val {taskId}} must be a list."
  )
}

messages$PIEmptyList <- function(field, taskId) {
  cliFormat(
    "Field {.val {field}} on PITask {.val {taskId}} must contain at least one entry."
  )
}

messages$PIScenariosEmpty <- function(recordType, recordId) {
  cliFormat(
    "Field {.code scenarios} on {recordType} {.val {recordId}} must be a non-empty character vector."
  )
}

# A PITask's `scenarios`, unlike a PIParameter's or a PIOutputMapping's, may be
# empty: it is a reference list, and an empty reference list means "there are
# none", so the message describes the shape rather than demanding a value.
messages$PITaskScenariosInvalid <- function(recordId) {
  cliFormat(
    "Field {.code scenarios} on PITask {.val {recordId}} must be a character \\
    vector of scenario ids with no NA or empty entries, or empty for none."
  )
}

messages$PIInvalidNumericField <- function(field, recordId, value) {
  cliFormat(
    "Field {.code {field}} on PIOutputMapping {.val {recordId}} is invalid: \\
    {.val {value}}. Expected a finite numeric value."
  )
}

messages$PIInvalidScaling <- function(recordId, value) {
  cliFormat(
    "Field {.code scaling} on PIOutputMapping {.val {recordId}} is invalid: \\
    {.val {value}}. Expected a non-empty string."
  )
}

messages$PIWrongElementType <- function(
  field,
  index,
  taskId,
  expectedClass
) {
  cliFormat(
    "Element {field}[[{index}]] on PITask {.val {taskId}} must be a {expectedClass}."
  )
}

messages$outputPathRefNotFound <- function(value, outputPathIds) {
  hint <- .suggestSuffix(value, outputPathIds)
  cli::format_message(c(
    "x" = "outputPath {.val {value}} is neither a defined output-path id nor \\
    the model path of one.",
    "i" = "Pass an output-path id (a key in \\
    {.code project$definitions$outputPaths}) or the literal model path of a \\
    defined output path; define new ones with {.fn addOutputPath}.{hint}"
  ))
}

messages$PIOutputQuantityNotFound <- function(path, simulationName) {
  cliFormat(
    "Output quantity {.path {path}} not found in simulation {.val {simulationName}}.
    Check that the output path exists in the simulation."
  )
}

messages$PIParameterNotFound <- function(path, simulationName) {
  cliFormat(
    "Parameter {.path {path}} not found in simulation {.val {simulationName}}.
    Check that the parameter path is correct and exists in the simulation."
  )
}

# Warned when a PI parameter declares a display unit that belongs to another
# dimension. Not an abort: a legacy 5.x sheet's `Units` cell was never applied by
# the version that wrote it, so such a cell is normal in a workbook that ran, and
# the identification is still well defined without it.
messages$PIParameterUnitNotApplied <- function(id, unit, dimension) {
  cli::format_message(c(
    "!" = "The unit {.val {unit}} of PI parameter {.val {id}} is not applied: it is not a unit of the parameter's dimension {.val {dimension}}.",
    "i" = "Its {.field minValue}, {.field maxValue} and {.field startValue} are read in the parameter's own unit instead.",
    "i" = "A legacy 5.x {.field PIParameters} sheet carries a {.field Units} column that esqlabsR 5.x never applied, which is where such a unit usually comes from; set the field with {.fn PIParameter} to fit in a different unit."
  ))
}

messages$PIScenarioNotFound <- function(scenarioName, availableScenarios) {
  cli::format_message(c(
    "x" = "Scenario {.val {scenarioName}} referenced in PI task configuration not found",
    "i" = "Available scenarios: {.val {paste(availableScenarios, collapse = ', ')}}"
  ))
}

messages$PITaskNotFound <- function(task) {
  cliFormat("PI task {.val {task}} not found")
}

messages$PIMemberNotFound <- function(label, id, task) {
  cliFormat("{label} {.val {id}} not found in task {.val {task}}; no-op.")
}

messages$PITaskNowEmpty <- function(task) {
  cliFormat("PI task {.val {task}} is now empty and has been removed.")
}

messages$buildingPITask <- function(piTaskName) {
  cliFormat("Building PI task: {.val {piTaskName}}")
}

messages$runningPITask <- function(piTaskName) {
  cliFormat("Running PI task: {.val {piTaskName}}")
}

# Observed data (Chapter 5) ####
messages$observedDataInvalidEntryType <- function(badType, validTypes) {
  cli::format_message(c(
    "x" = "Invalid {.field type} {.val {badType}} in {.code observedData} entry.",
    "i" = "Must be one of: {.val {validTypes}}."
  ))
}

# Every missing field at once: the required set for each type is fixed, so
# reporting one field per call made an under-specified entry take as many calls
# to fix as it had gaps.
messages$observedDataMissingFields <- function(entryIndex, type, fields) {
  cliFormat(
    "{.code observedData} entry {entryIndex} (type {.val {type}}) is missing \\
    {cli::qty(length(fields))}required field{?s} {.field {fields}}."
  )
}

messages$observedDataFileNotFound <- function(filePath) {
  cliFormat("Observed-data source file not found: {.path {filePath}}.")
}

messages$observedDataScriptWrongReturnType <- function(filePath, klass) {
  cli::format_message(c(
    "x" = "Script {.path {filePath}} did not return a {.cls DataSet} or list of {.cls DataSet}.",
    "i" = "Got an object of class {.cls {klass}}."
  ))
}

messages$observedDataScriptSourcing <- function(filePath) {
  cliFormat("Sourcing observed-data script: {.path {filePath}}")
}

messages$observedDataScriptSecurityWarn <- function() {
  cli::format_message(c(
    "!" = "This project runs an R script to build observed data, executing arbitrary R code on your machine.",
    "i" = "Only resolve observed data from a project you trust. See {.help esqlabsR::loadObservedData} for details.",
    "i" = "This warning is shown once per session."
  ))
}

messages$observedDataProgrammaticUnresolved <- function(names) {
  cli::format_message(c(
    "!" = "{length(names)} programmatic observed-data source{?s} resolved to no data: {.val {names}}.",
    "i" = "A programmatic source holds its {.cls DataSet} only in the session that added it; it is not saved to disk.",
    "i" = "Re-add {cli::qty(names)}{?it/them} with {.fn addObservedData} in this session, or declare {?it/them} as a {.code script} or {.code pkml} source to persist across a reload."
  ))
}

messages$observedDataDataFolderNotDeclared <- function(file) {
  cliFormat(
    "{.field dataFolder} is not declared in {.code filePaths}; cannot resolve {.path {file}}."
  )
}

messages$observedDataProgrammaticAdded <- function(name, hasDataFolder = TRUE) {
  saveNote <- if (hasDataFolder) {
    "On {.fn saveProject} it is written to {.path {paste0(name, '.pkml')}} under the data folder, so it survives a reload."
  } else {
    "Declare a {.field dataFolder} in {.code filePaths} before saving: {.fn saveProject} writes it to a PKML file there so it survives a reload, and aborts if no data folder is declared."
  }
  cli::format_message(c(
    "i" = "Added programmatic observed-data source {.val {name}}. It lives in this session until you save.",
    "i" = saveNote
  ))
}

messages$observedDataPersistNoDataFolder <- function(name) {
  cli::format_message(c(
    "x" = "Cannot save the programmatic observed-data source {.val {name}}: {.field dataFolder} is not declared in {.code filePaths}.",
    "i" = "A programmatic source is written to a PKML file under {.field dataFolder} on save. Declare {.field dataFolder}, then save again."
  ))
}

messages$observedDataPersistIdCollision <- function(ids) {
  cli::format_message(c(
    "x" = "Saving a programmatic observed-data source would overwrite another source: {.file {ids}}.",
    "i" = "A programmatic source is written to {.file <name>.pkml}; this clashes with an existing source filed under the same name.",
    "i" = "Rename the {.cls DataSet} (its {.field name}) so the file names differ."
  ))
}

messages$populationProgrammaticAdded <- function(
  id,
  hasPopulationsFolder = TRUE
) {
  saveNote <- if (hasPopulationsFolder) {
    "On {.fn saveProject} it is written to {.path {paste0(id, '.csv')}} under the populations folder, so it survives a reload."
  } else {
    "Declare a {.field populationsFolder} in {.code filePaths} before saving: {.fn saveProject} writes it to a CSV file there so it survives a reload, and aborts if no populations folder is declared."
  }
  cli::format_message(c(
    "i" = "Added programmatic population {.val {id}}. It lives in this session until you save.",
    "i" = saveNote
  ))
}

messages$populationProgrammaticUnresolved <- function(id, scenarioName) {
  cli::format_message(c(
    "x" = "Population {.val {id}} referenced by scenario {.val {scenarioName}} was injected in a previous session and holds no data now.",
    "i" = "A programmatic population holds its {.cls Population} only in the session that added it.",
    "i" = "Re-add it with {.fn addPopulation} in this session, or run {.fn saveProject} once to freeze it to a CSV file that survives a reload."
  ))
}

messages$populationPersistNoPopulationsFolder <- function(id) {
  cli::format_message(c(
    "x" = "Cannot save the programmatic population {.val {id}}: {.field populationsFolder} is not declared in {.code filePaths}.",
    "i" = "A programmatic population is written to a CSV file under {.field populationsFolder} on save. Declare {.field populationsFolder}, then save again."
  ))
}

messages$populationPersistIdCollision <- function(ids) {
  cli::format_message(c(
    "x" = "Saving a programmatic population would overwrite an existing population file: {.file {ids}}.",
    "i" = "A programmatic population is written to {.file <id>.csv}; this clashes with a file already filed under the same id.",
    "i" = "Rename the population (its {.field id}) so the file names differ."
  ))
}

messages$projectPathEscapesRoot <- function(fieldName, path, root) {
  cli::format_message(c(
    "x" = "{.field {fieldName}} {.val {path}} resolves outside the project \\
    folder.",
    "i" = "It must stay under {.path {root}}. A project file cannot reference \\
    a path outside the project."
  ))
}

messages$pkmlOutsideSimulationsFolder <- function(pkmlPath, modelFile) {
  cli::format_message(c(
    "!" = "PKML {.file {pkmlPath}} is outside the project's \\
    {.field simulationsFolder}; storing an escaping relative path \\
    {.val {modelFile}}.",
    "i" = "This scenario will fail at run time because the model file \\
    resolves outside the project. Move the PKML under the simulations folder, \\
    or set a {.field simulationsFolder} that contains it."
  ))
}

messages$duplicateSimulationsFolderKey <- function() {
  cli::format_message(c(
    "!" = "{.code filePaths} carries both the legacy {.field modelFolder} and \\
    the current {.field simulationsFolder}; using {.field simulationsFolder}.",
    "i" = "Remove the legacy {.field modelFolder} key to silence this warning."
  ))
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
messages$observedDataMissingType <- function(entryLabel) {
  cliFormat("{entryLabel} is missing required field 'type'")
}

messages$observedDataInvalidType <- function(
  entryLabel,
  type,
  validTypes
) {
  cliFormat(
    "{entryLabel} has invalid type '{type}'. Must be one of: {paste(validTypes, collapse = \", \")}"
  )
}

messages$validatorObservedDataMissingField <- function(
  entryLabel,
  type,
  field
) {
  cliFormat("{entryLabel} ({type}) is missing required field '{field}'")
}

messages$validatorObservedDataFileNotFound <- function(entryLabel, file) {
  cliFormat("{entryLabel} references non-existent file: {file}")
}

messages$observedDataImporterNotFound <- function(
  entryLabel,
  importerConfiguration
) {
  cliFormat(
    "{entryLabel} references non-existent importer config: {importerConfiguration}"
  )
}

messages$observedDataPathEscapes <- function(entryLabel, path) {
  cliFormat(
    "{entryLabel} references a file outside the project folder: {path}"
  )
}

# Duplicate-collision abort shared by every `add*` authoring function: adding a
# definition whose id already exists aborts with this two-line message unless
# the caller passes `overwrite = TRUE`. `label` is the already-quoted subject
# (e.g. `"scenario {.val {clash}}"`), so callers keep their own wording for what
# collided while the overwrite hint stays identical everywhere.
messages$definitionAlreadyExists <- function(label) {
  c(
    paste0(label, " already exists."),
    "i" = "Pass {.code overwrite = TRUE} to replace it."
  )
}
