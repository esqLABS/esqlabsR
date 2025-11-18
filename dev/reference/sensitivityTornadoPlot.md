# Tornado Plot for Sensitivity Analysis

Generates tornado plots to visualize the results of sensitivity
analysis. Each plot shows the effect of modifying parameters by a
specific scaling factor (`parameterFactor`) and its reciprocal on
specific model outputs. This visualization helps to assess the impact of
parameter changes on the results, highlighting the model's sensitivity
to these parameters.

## Usage

``` r
sensitivityTornadoPlot(
  sensitivityCalculation,
  outputPaths = NULL,
  parameterPaths = NULL,
  pkParameters = NULL,
  parameterFactor = 0.1,
  xAxisZoomRange = NULL,
  defaultPlotConfiguration = NULL
)
```

## Arguments

- sensitivityCalculation:

  The `SensitivityCalculation` object returned by
  [`sensitivityCalculation()`](https://esqlabs.github.io/esqlabsR/dev/reference/sensitivityCalculation.md).

- outputPaths, parameterPaths, pkParameters:

  A single or a vector of the output path(s), parameter path(s), and PK
  parameters to be displayed, respectively. If `NULL`, all included
  paths and parameters present in the supplied `SensitivityCalculation`
  object will be displayed in the visualization. A separate plot will be
  generated for each output path. Each plot will contain a tornado plot
  panel for each PK parameter, and the sensitivities for each parameter
  will be displayed as lines.

- parameterFactor:

  Numeric; the scaling factor used to adjust parameters during
  sensitivity analysis used in the tornado plot. Both the
  `parameterFactor` and its reciprocal (`1/parameterFactor`) must be
  included in the `variationRange` specified in the
  `sensitivityCalculation`. Default is 0.1.

- xAxisZoomRange:

  Numeric vector of length 2; specifies the x-axis limits to zoom into.
  This does not remove any data but constrains the visible plotting
  range for improved readability.

- defaultPlotConfiguration:

  An object of class `DefaultPlotConfiguration` used to customize plot
  aesthetics.

  Supported parameters include:

  - `legendPosition`: Position of the legend on the plot.

  - `legendTitle`: Title displayed for the legend.

  - `linesColor`: Color of the bar elements.

  - `subtitle`: Subtitle text for the plot.

  - `title`: Main title text for the plot.

  - `titleSize`: Font size of the plot title.

  - `xLabel`: Label text for the x-axis.

  - `yLabel`: Label text for the y-axis.

    Default values are set to provide a standardized look, but each
    parameter can be tailored to fit specific visual needs. Modifying
    these parameters will directly affect the aesthetics of the output
    plots.

## Value

A `patchwork` object containing the combined ggplot objects if a single
output path is specified, or a list of `patchwork` objects for multiple
output paths.

## See also

Other sensitivity-calculation:
[`sensitivityCalculation()`](https://esqlabs.github.io/esqlabsR/dev/reference/sensitivityCalculation.md),
[`sensitivitySpiderPlot()`](https://esqlabs.github.io/esqlabsR/dev/reference/sensitivitySpiderPlot.md),
[`sensitivityTimeProfiles()`](https://esqlabs.github.io/esqlabsR/dev/reference/sensitivityTimeProfiles.md)

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

results <- sensitivityCalculation(
  simulation = simulation,
  outputPaths = outputPaths,
  parameterPaths = parameterPaths,
  variationRange = c(seq(0.1, 1, by = 0.1), seq(2, 10, by = 1)),
)

# Print plots with default settings
sensitivityTornadoPlot(results)

# Print plots with specific parameter scaling factor
sensitivityTornadoPlot(results, parameterFactor = 0.5)

# Print plots with custom configuration settings
myPlotConfiguration <- createEsqlabsPlotConfiguration()
myPlotConfiguration$legendPosition <- "bottom"
myPlotConfiguration$subtitle <- "Custom settings"
sensitivityTornadoPlot(results, defaultPlotConfiguration = myPlotConfiguration)

# Use named parameter paths to customize axis labels
namedParameterPaths <- c(
  "Lipophilicity" = "Aciclovir|Lipophilicity",
  "Dose" = "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
  "GFR fraction" = "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR fraction"
)

resultsNamed <- sensitivityCalculation(
  simulation = simulation,
  outputPaths = outputPaths,
  parameterPaths = namedParameterPaths
)

sensitivityTornadoPlot(resultsNamed)
} # }
```
