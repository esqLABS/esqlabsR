# Time Profile plots for Sensitivity Analysis

Creates time profiles for selected outputs generated in a sensitivity
analysis. This function plots time profiles for each specified output
path, illustrating the dynamics of model outputs to parameter
variations.

## Usage

``` r
sensitivityTimeProfiles(
  sensitivityCalculation,
  outputPaths = NULL,
  parameterPaths = NULL,
  xAxisScale = NULL,
  yAxisScale = NULL,
  xUnits = NULL,
  yUnits = NULL,
  observedData = NULL,
  defaultPlotConfiguration = NULL
)
```

## Arguments

- sensitivityCalculation:

  The `SensitivityCalculation` object returned by
  [`sensitivityCalculation()`](https://esqlabs.github.io/esqlabsR/reference/sensitivityCalculation.md).

- outputPaths, parameterPaths:

  A single or a vector of the output path(s) to be plotted for parameter
  path(s) which impact is analyzed, respectively. If `NULL`, all
  included paths and parameters present in the supplied
  `SensitivityCalculation` object will be displayed in the
  visualization. A separate plot will be generated for each output path,
  and a separate curve will be generated for each parameter variation. A
  separate panel is created for each varied parameter.

- xAxisScale:

  Character string, either "log" (logarithmic scale) or "lin" (linear
  scale), to set the x-axis scale. Default is "lin".

- yAxisScale:

  Character string, either "log" or "lin", sets the y-axis scale
  similarly to `xAxisScale`. Default is "log".

- xUnits, yUnits:

  Lists of units for the x-axis and y-axis, respectively. If a list of
  length one is provided, it will be applied to all `outputPaths` if
  conversion is possible. If a list of multiple units is provided, the
  units list should correspond to the `outputPaths`, and units
  conversion will be applied accordingly. If `NULL`, default units from
  the simulation results will be used.

- observedData:

  Optional. A `DataSet` or a list of `DataSet` objects containing
  observed data. If provided, observed data will be plotted together
  with the simulated data based on `OutputPath` dimension for direct
  comparison within the visualizations. Observed data will only be added
  to plots with matching y-dimensions.

- defaultPlotConfiguration:

  An object of class `DefaultPlotConfiguration` used to customize plot
  aesthetics. Plot-specific settings provided directly to the function,
  such as `xAxisScale`, will take precedence over any modifications in
  `defaultPlotConfiguration`. If not provided, default settings are
  applied.

  Supported parameters for `defaultPlotConfiguration` include:

  - `legendPosition`: Specifies the position of the plot legend.

  - `legendTitle`: Sets the title displayed for the legend.

  - `linesAlpha`: Alpha transparency for the line elements.

  - `linesColor`: Color of the line elements.

  - `linesSize`: Thickness of the line elements.

  - `pointsShape`: Shape of the point elements for observed data.

  - `title`: Main title text for the plot.

  - `titleSize`: Font size of the plot title.

  - `xAxisScale`: Scale type for the x-axis (`"log"` or `"lin"`).

  - `xLabel`: Label text for the x-axis.

  - `yAxisScale`: Scale type for the y-axis (`"log"` or `"lin"`).

  - `yLabel`: Label text for the y-axis.

## Value

A `patchwork` object containing the combined ggplot objects if a single
output path is specified, or a list of `patchwork` objects for multiple
output paths.

## See also

Other sensitivity-calculation:
[`sensitivityCalculation()`](https://esqlabs.github.io/esqlabsR/reference/sensitivityCalculation.md),
[`sensitivitySpiderPlot()`](https://esqlabs.github.io/esqlabsR/reference/sensitivitySpiderPlot.md),
[`sensitivityTornadoPlot()`](https://esqlabs.github.io/esqlabsR/reference/sensitivityTornadoPlot.md)

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
  parameterPaths = parameterPaths
)

# Print plots with default settings
sensitivityTimeProfiles(results)

# Print plots with linear y-axis values
sensitivityTimeProfiles(results, yAxisScale = "lin")

# Print plots with custom configuration settings
myPlotConfiguration <- createEsqlabsPlotConfiguration()
myPlotConfiguration$linesColor <- c("#4D8076", "#C34A36")
myPlotConfiguration$subtitle <- "Custom settings"
sensitivityTimeProfiles(results, defaultPlotConfiguration = myPlotConfiguration)

# Use named parameter paths to customize facet labels
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

sensitivitySpiderPlot(resultsNamed)
} # }
```
