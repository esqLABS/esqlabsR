# Carry out and visualize sensitivity analysis (with OSPSuite)

Carry out and visualize sensitivity analysis (with OSPSuite)

## Usage

``` r
sensitivityCalculation(
  simulation,
  outputPaths,
  parameterPaths,
  variationRange = c(seq(0.1, 1, by = 0.1), seq(2, 10, by = 1)),
  variationType = c("relative", "absolute"),
  pkParameters = c("C_max", "t_max", "AUC_inf"),
  customOutputFunctions = NULL,
  saOutputFilePath = NULL,
  simulationRunOptions = NULL
)
```

## Arguments

- simulation:

  An object of type `Simulation`.

- outputPaths:

  Path (or a vector of paths) to the output(s) for which the sensitivity
  will be analyzed.

- parameterPaths:

  A single or a vector of the parameter path(s) to be varied. Can also
  be a named vector, where the names are user-defined labels. These
  names will be stored and used in downstream plotting functions (e.g.,
  as legend labels) if provided.

- variationRange:

  Optional numeric vector or list defining the scaling of the
  parameters. The same variation range is applied to all specified
  parameters unless a list is provided, in which case the length of the
  list must match the length of `parameterPaths`, allowing individual
  variation for each parameter. If not specified, the following vector
  will be used: c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 2, 3,
  4, 5, 6, 7, 8, 9, 10).

- variationType:

  A string specifying whether the values in `variationRange` are applied
  as `"absolute"` or `"relative"` scaling. When set to `"absolute"`, the
  values are interpreted as absolute parameter values. When set to
  `"relative"`, the values are interpreted as scaling factors relative
  to the initial parameter values. Default is `"relative"`.

- pkParameters:

  A vector of names of PK parameters for which the sensitivities will be
  calculated. For a full set of available standard PK parameters, run
  `names(ospsuite::StandardPKParameter)`. By default, the following
  parameters are considered: `"C_max"`, `"t_max"`, `"AUC_inf"`. If
  `NULL`, all available PK-parameters (including the user-defined) will
  be calculated.

- customOutputFunctions:

  A named list with custom function(s) for output calculations.
  User-defined functions should have either 'x', 'y', or both 'x' and
  'y' as parameters which correspond to x-Dimension (time) or
  y-Dimension values from simulation results. The output of the function
  is a single numerical value for each output and parameter path, which
  is then included in the returned dataframe of PK parameters.

- saOutputFilePath:

  Path to excel file in which PK-parameter data should be saved. If a
  file already exists, it will be overwritten. Default is `NULL`,
  meaning the data will not be saved to a spreadsheet.

- simulationRunOptions:

  Optional instance of a `SimulationRunOptions` used during the
  simulation run

## Value

A list containing following objects:

- `SimulationResults`

- specified output paths

- specified parameter paths

- A data frame of PK parameters

## See also

Other sensitivity-calculation:
[`sensitivitySpiderPlot()`](https://esqlabs.github.io/esqlabsR/dev/reference/sensitivitySpiderPlot.md),
[`sensitivityTimeProfiles()`](https://esqlabs.github.io/esqlabsR/dev/reference/sensitivityTimeProfiles.md),
[`sensitivityTornadoPlot()`](https://esqlabs.github.io/esqlabsR/dev/reference/sensitivityTornadoPlot.md)

## Examples

``` r
if (FALSE) { # \dontrun{
simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
simulation <- loadSimulation(simPath)
outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
parameterPaths <- c(
  "Aciclovir|Lipophilicity",
  "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
  "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR fraction"
)

# extract the results into a list of dataframes
sensitivityCalculation(
  simulation = simulation,
  outputPaths = outputPaths,
  parameterPaths = parameterPaths
)

# Calculate sensitivity for a user-defined function that computes the
# averate of the simulated y-values
customOutputFunctions <- list(
  "Average" = function(y) mean(y)
)
sensitivityCalculation(
  simulation = simulation,
  outputPaths = outputPaths,
  parameterPaths = parameterPaths,
  customOutputFunctions = customOutputFunctions
)
} # }
```
