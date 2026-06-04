# Read Parameter Identification configurations from Excel

Read Parameter Identification configurations from Excel

## Usage

``` r
readPITaskConfigurationFromExcel(piTaskNames = NULL, projectConfiguration)
```

## Arguments

- piTaskNames:

  Character vector. Names of the parameter identification tasks that are
  defined in the Excel file. If `NULL` (default), all tasks specified in
  the Excel file will be read.

- projectConfiguration:

  A `ProjectConfiguration` object holding base project information.

## Value

A named list of `PITaskConfiguration` objects.

## Details

Reads PI task configuration from the Excel file defined in
`ProjectConfiguration` and creates `PITaskConfiguration` objects. If a
PI task that is specified in `piTaskNames` is not found in the Excel
file, an error is thrown.

The function expects the Excel file to have a "PIOutputMappings" sheet
with `PITaskName`, `Scenarios`, `OutputPath`, `ObservedDataSheet`,
`DataSet`, `Scaling`, `xOffset`, `yOffset`, `xFactor`, `yFactor`,
`Weight` columns. `OutputPath` accepts either a full simulation output
path or an `OutputPathId` defined in the "OutputPaths" sheet of
`Scenarios.xlsx`. It also expects a "PIParameters" sheet with
`PITaskName`, `Scenarios`, `Container Path`, `Parameter Name`, `Units`,
`MinValue`, `MaxValue`, `StartValue`, `Group` columns, an optional
"PIConfiguration" sheet with `PITaskName`, `Algorithm`, `CIMethod`,
`PrintEvaluationFeedback`, `AutoEstimateCI`, `numberOfCores`,
`checkForNegativeValues`, `ObjectiveFunctionType`,
`ResidualWeightingMethod`, `RobustMethod`, `ScaleVar`, `LinScaleCV`,
`LogScaleSD` columns, an "AlgorithmOptions" sheet with `PITaskName`,
`OptionName`, `OptionValue` columns, and a "CIOptions" sheet with
`PITaskName`, `OptionName`, `OptionValue` columns.

## Examples

``` r
if (FALSE) { # \dontrun{
projectConfiguration <- createProjectConfiguration(
  exampleProjectConfigurationPath(),
  ignoreVersionCheck = TRUE
)
piTaskConfigurations <- readPITaskConfigurationFromExcel(
  piTaskNames = "AciclovirSimple",
  projectConfiguration = projectConfiguration
)
} # }
```
