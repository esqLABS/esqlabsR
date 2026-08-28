# Parameter Identification

`esqlabsR` provides an excel-based workflow for parameter identification
(PI), following the same principles as the standard project workflow. It
builds on the
[`OSPSuite.ParameterIdentification`](http://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/)
package. PI tasks are defined in the `ParameterIdentification.xlsx` file
and executed in three steps:

1.  Read PI task configurations from Excel
2.  Create PI tasks
3.  Run PI

The PI workflow builds on top of a configured project. Each PI task
references one or more **scenarios** that define the simulation model,
its parametrization, and output paths. Scenarios must be set up in
`Scenarios.xlsx` before configuring PI tasks. See
[`vignette("design-scenarios")`](https://esqlabs.github.io/esqlabsR/dev/articles/design-scenarios.md)
for how to create scenarios.

> **Note:** Parameters defined in `ModelParameterSheets` and output
> paths defined in `OutputPathsIds` of the referenced scenario will be
> overwritten by the settings in `PIParameters` and `PIOutputMappings`,
> respectively.

## The `ParameterIdentification.xlsx` file

The `ParameterIdentification.xlsx` file is located in the
`Configurations` folder of your project. Its path is stored in the
`ProjectConfiguration` and can be accessed via
`projectConfiguration$parameterIdentificationFile`. The file contains
five sheets, of which **PIParameters** and **PIOutputMappings** are
mandatory. **PIConfiguration**, **AlgorithmOptions**, and **CIOptions**
are optional and allow customizing the optimization behavior.

### PIParameters (mandatory)

Defines the parameters to be identified. Each row maps a model parameter
to a scenario and specifies bounds for the optimization.

| Column | Description |
|:---|:---|
| `PITaskName` | Name of the PI task this row belongs to |
| `Scenarios` | Name of the scenario (must exist in `Scenarios.xlsx`) |
| `Container Path` | Container path of the parameter in the simulation |
| `Parameter Name` | Name of the parameter |
| `Units` | Unit of the parameter value (optional) |
| `MinValue` | Lower bound for optimization |
| `MaxValue` | Upper bound for optimization |
| `StartValue` | Starting value for optimization |
| `Group` | Group identifier (see [Parameter Grouping](#parameter-grouping)) |

### PIOutputMappings (mandatory)

Maps simulation outputs to observed data sets used to compute the
objective function.

| Column | Description |
|:---|:---|
| `PITaskName` | Name of the PI task |
| `Scenarios` | Name of the scenario (must exist in `Scenarios.xlsx`) |
| `OutputPath` | Simulation output path or `OutputPathId` from `Scenarios.xlsx` |
| `ObservedDataSheet` | Sheet name in the observed data Excel file |
| `DataSet` | Name of the specific data set within the sheet |
| `Scaling` | Residual scaling: `lin` (linear) or `log` (logarithmic) |
| `xOffset` | Offset applied to x-values of observed data (optional) |
| `yOffset` | Offset applied to y-values of observed data (optional) |
| `xFactor` | Scale factor applied to x-values of observed data (optional) |
| `yFactor` | Scale factor applied to y-values of observed data (optional) |
| `Weight` | Weight(s) for this output mapping (optional) |

### PIConfiguration (optional)

General settings for each PI task. If omitted, defaults are used.

| Column | Description |
|:---|:---|
| `PITaskName` | Unique name of the PI task |
| `Algorithm` | Optimization algorithm (e.g., `BOBYQA`) |
| `CIMethod` | Confidence interval method (e.g., `hessian`) |
| `PrintEvaluationFeedback` | Print progress during optimization (`TRUE`/`FALSE`) |
| `AutoEstimateCI` | Automatically estimate confidence intervals (`TRUE`/`FALSE`) |
| `numberOfCores` | Number of CPU cores for parallel simulation runs |
| `checkForNegativeValues` | Check for negative values during simulation (`TRUE`/`FALSE`) |
| `ObjectiveFunctionType` | Error model: `lsq` (default) or `m3` |
| `ResidualWeightingMethod` | Residual weighting: `none` (default), `std`, `mean`, `error` |
| `RobustMethod` | Robust estimation: `none` (default), `huber`, `bisquare` |
| `ScaleVar` | Scale variance (`TRUE`/`FALSE`, default `FALSE`) |
| `LinScaleCV` | Coefficient of variation for linear scale (default `0.2`) |
| `LogScaleSD` | Standard deviation for log scale (default `0.086`) |

`numberOfCores` and `checkForNegativeValues` correspond to the options
of
[`ospsuite::SimulationRunOptions`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/SimulationRunOptions.html).
Leave them empty to use the defaults.

For background on `ObjectiveFunctionType`, `ResidualWeightingMethod`,
and `RobustMethod`, see the [`OSPSuite.ParameterIdentification` error
calculation
vignette](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/articles/error-calculation.html).

### AlgorithmOptions (optional)

Fine-tuning of the optimization algorithm in long format. For guidance
on choosing an algorithm and understanding its options, see the
[`OSPSuite.ParameterIdentification` optimization algorithms
vignette](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/articles/optimization-algorithms.html).
For a quick lookup of available option names and their defaults, run
e.g.:

``` r

ospsuite.parameteridentification::AlgorithmOptions_BOBYQA
ospsuite.parameteridentification::AlgorithmOptions_DEoptim
```

| Column        | Description                                                |
|:--------------|:-----------------------------------------------------------|
| `PITaskName`  | Name of the PI task                                        |
| `OptionName`  | Name of the algorithm option (e.g., `maxeval`, `ftol_rel`) |
| `OptionValue` | Value of the option                                        |

### CIOptions (optional)

Options for confidence interval estimation in long format. For a quick
lookup of available option names and their defaults, run e.g.:

``` r

ospsuite.parameteridentification::CIOptions_hessian
ospsuite.parameteridentification::CIOptions_bootstrap
```

| Column        | Description                               |
|:--------------|:------------------------------------------|
| `PITaskName`  | Name of the PI task                       |
| `OptionName`  | Name of the CI option (e.g., `confLevel`) |
| `OptionValue` | Value of the option                       |

## Simple example

The example project included in
[esqlabsR](https://github.com/esqLABS/esqlabsR) contains a PI task
called `AciclovirSimple` that estimates the lipophilicity of aciclovir
using a single scenario and one observed data set.

### 1. Read PI task configurations

First, load the project configuration and read the PI task definitions
from Excel:

``` r

projectConfiguration <- createProjectConfiguration(
  path = exampleProjectConfigurationPath(),
  ignoreVersionCheck = TRUE
)

piTaskConfigurations <- readPITaskConfigurationFromExcel(
  piTaskNames = "AciclovirSimple",
  projectConfiguration = projectConfiguration
)
```

The returned object is a named list of `PITaskConfiguration` objects.
You can inspect the configuration by printing it:

``` r

piTaskConfigurations$AciclovirSimple
#> <PITaskConfiguration>
#> 
#> ── PI Task Configuration ───────────────────────────────────────────────────────
#>   • Task Name: AciclovirSimple
#>   • Scenario(s): PITestScenario
#>   • Model File(s): Aciclovir.pkml
#>   • Algorithm: BOBYQA
#>   • CI Method: hessian
#>   • Number of Parameters: 1
#>   • Number of Output Mappings: 1
```

### 2. Create PI tasks

Next, create the `ParameterIdentification` objects. This step loads the
simulations, resolves the parameter paths, and maps the observed data:

``` r

piTasks <- createPITasks(piTaskConfigurations)
```

### 3. Run PI

Finally, run the parameter identification:

``` r

piResults <- runPI(piTasks)
```

The function returns a named list. Each entry contains:

- `task`: the original `ParameterIdentification` object
- `result`: a `PIResult` object with the optimized parameter values, or
  `NULL` if the optimization failed
- `error`: error message string (only present if the optimization
  failed)

If multiple PI tasks are passed,
[`runPI()`](https://esqlabs.github.io/esqlabsR/dev/reference/runPI.md)
continues with the remaining tasks even if one fails.

## Multi-scenario example

The example project also includes `AciclovirMultiScenario`, which
demonstrates a more advanced setup: fitting parameters across two
scenarios with different dose levels (250 mg and 500 mg) and using
parameter grouping to control whether a parameter is shared across
scenarios.

### Excel configuration

The `PIParameters` sheet for this task:

| PITaskName | Scenarios | Container Path | Parameter Name | MinValue | MaxValue | StartValue | Group |
|:---|:---|:---|:---|---:|---:|---:|---:|
| AciclovirMultiScenario | PIScenario_250mg | Aciclovir | Lipophilicity | -10 | 10 | 1.0 | 1 |
| AciclovirMultiScenario | PIScenario_500mg | Aciclovir | Lipophilicity | -10 | 10 | 1.0 | 1 |
| AciclovirMultiScenario | PIScenario_250mg | Neighborhoods\|Kidney_pls_Kidney_ur\|Aciclovir\|Renal Clearances-TS-Aciclovir | TSspec | 0 | 10 | 0.5 | 2 |
| AciclovirMultiScenario | PIScenario_500mg | Neighborhoods\|Kidney_pls_Kidney_ur\|Aciclovir\|Renal Clearances-TS-Aciclovir | TSspec | 0 | 10 | 0.5 | 3 |

The `PIOutputMappings` sheet maps each scenario to its respective
observed data set:

| PITaskName | Scenarios | OutputPath | ObservedDataSheet | DataSet | Scaling | xOffset | yOffset | xFactor | yFactor | Weight |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| AciclovirMultiScenario | PIScenario_250mg | Organism\|PeripheralVenousBlood\|Aciclovir\|Plasma (Peripheral Venous Blood) | Laskin 1982.Group A | Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_2.5 mg/kg_iv\_ | log | NA | NA | NA | NA | NA |
| AciclovirMultiScenario | PIScenario_500mg | Organism\|PeripheralVenousBlood\|Aciclovir\|Plasma (Peripheral Venous Blood) | Laskin 1982.Group A | Laskin 1982.Group A_Aciclovir_1_Human_MALE_PeripheralVenousBlood_Plasma_5.0 mg/kg_iv\_ | log | NA | NA | NA | NA | NA |

### Parameter Grouping

The `Group` column in the `PIParameters` sheet controls how parameters
are linked across scenarios during optimization:

- **Same group, same parameter path**: The parameter is treated as a
  single variable across the listed scenarios. The optimizer finds one
  value that best fits all scenarios simultaneously. In the example
  above, `Lipophilicity` has `Group = 1` in both rows, meaning a single
  lipophilicity value is estimated across both dose levels.
- **Different groups, same parameter path**: The parameter is treated as
  independent in each scenario and optimized separately. `TSspec` has
  `Group = 2` for the 250 mg scenario and `Group = 3` for the 500 mg
  scenario, allowing different estimates per dose level.

Within a group, `MinValue`, `MaxValue`, and `StartValue` must be
identical across all rows.

### Running the multi-scenario task

The workflow is identical to the simple case:

``` r

piTaskConfigurations <- readPITaskConfigurationFromExcel(
  piTaskNames = "AciclovirMultiScenario",
  projectConfiguration = projectConfiguration
)

piTasks <- createPITasks(piTaskConfigurations)
piResults <- runPI(piTasks)
```

## Details

### Scenarios

PI tasks reference scenarios defined in the `Scenarios.xlsx` file. These
scenarios must exist before running PI — see
[`vignette("design-scenarios")`](https://esqlabs.github.io/esqlabsR/dev/articles/design-scenarios.md)
for how to set them up. In the example project, `PITestScenario` is a
minimal scenario using default simulation settings, while
`PIScenario_250mg` and `PIScenario_500mg` define specific application
protocols and simulation times.

The `Scenarios` column in `PIParameters` and `PIOutputMappings` accepts
a single scenario name per row. To apply a parameter across multiple
scenarios, add one row per scenario with the same `Group` value.

### Observed data

The `ObservedDataSheet` column specifies which sheet to load from the
observed data file configured in `ProjectConfiguration`. The `DataSet`
column specifies the exact data set name within the loaded sheet. To
find available data set names, use:

``` r

observedData <- loadObservedData(
  projectConfiguration = projectConfiguration,
  sheets = "Laskin 1982.Group A"
)
names(observedData)
```

### Residual scaling

The `Scaling` column in `PIOutputMappings` controls how residuals are
computed:

- `lin`: Linear residuals. Suitable when observed values span a narrow
  range.
- `log`: Logarithmic residuals. Preferred when observed values span
  several orders of magnitude, giving equal weight to low and high
  concentrations.

### Weights

The `Weight` column allows assigning relative importance to specific
output mappings. A single numeric value applies uniform weight. Multiple
values (separated by `,`) assign per-data-point weights. If left empty,
all data points are weighted equally.
