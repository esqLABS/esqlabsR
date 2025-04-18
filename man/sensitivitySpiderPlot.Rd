% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sensitivity-spider-plot.R
\name{sensitivitySpiderPlot}
\alias{sensitivitySpiderPlot}
\title{Sensitivity Spider Plot for Pharmacokinetic Parameters}
\usage{
sensitivitySpiderPlot(
  sensitivityCalculation,
  outputPaths = NULL,
  parameterPaths = NULL,
  pkParameters = NULL,
  xAxisScale = NULL,
  yAxisScale = NULL,
  xAxisType = "percent",
  yAxisType = "percent",
  yAxisFacetScales = "fixed",
  defaultPlotConfiguration = NULL
)
}
\arguments{
\item{sensitivityCalculation}{The \code{SensitivityCalculation} object returned by
\code{sensitivityCalculation()}.}

\item{outputPaths, parameterPaths, pkParameters}{A single or a vector of the
output path(s), parameter path(s), and PK parameters to be displayed,
respectively. If \code{NULL}, all included paths and parameters present in the
supplied \code{SensitivityCalculation} object will be displayed in the
visualization.
A separate plot will be generated for each output path. Each plot will
contain a spider plot panel for each PK parameter, and the sensitivities
for each parameter will be displayed as lines.}

\item{xAxisScale}{Character string, either "log" (logarithmic scale) or "lin"
(linear scale), to set the x-axis scale. Default is "log".}

\item{yAxisScale}{Character string, either "log" or "lin", sets the y-axis scale
similarly to \code{xAxisScale}. Default is "lin".}

\item{xAxisType}{Character string, either "percent" (percentage change) or
"absolute" (absolute values) for PK parameter values, for x-axis data normalization.
Default is "percent".}

\item{yAxisType}{Character string, either "percent" (percentage change) or
"absolute" (absolute values) for PK parameter values, for y-axis data normalization.
Default is "percent".}

\item{yAxisFacetScales}{Character string, either "fixed" or "free", determines
the scaling across y-axes of different facets. Default is "fixed".
If "fixed", all facetes within one plot will have the same range, which allows
for easier comparison between different PK parameters. If "free", each facet
will have its own range, which allows for better visualization of the single
PK parameters sensitivity.}

\item{defaultPlotConfiguration}{An object of class \code{DefaultPlotConfiguration}
used to customize plot aesthetics. Plot-specific settings provided directly
to the function, such as \code{xAxisScale}, will take precedence over any
modifications in \code{defaultPlotConfiguration}.

Supported parameters include:
\itemize{
\item \code{legendPosition}: Position of the legend on the plot.
\item \code{legendTitle}: Title displayed for the legend.
\item \code{linesAlpha}: Alpha transparency for line elements.
\item \code{linesColor}: Color of the line elements.
\item \code{linesSize}: Thickness of the line elements.
\item \code{pointsShape}: Shape of the point elements.
\item \code{pointsSize}: Size of the point elements.
\item \code{subtitle}: Subtitle text for the plot.
\item \code{title}: Main title text for the plot.
\item \code{titleSize}: Font size of the plot title.
\item \code{xAxisScale}: Scale type for the x-axis (\code{"log"} or \code{"lin"}).
\item \code{xLabel}: Label text for the x-axis.
\item \code{xValuesLimits}: Numeric vector specifying the limits for x-values.
\item \code{yAxisLimits}: Numeric vector specifying the limits for y-values.
\item \code{yAxisScale}: Scale type for the y-axis (\code{"log"} or \code{"lin"}).
\item \code{yAxisTicks}: Number of ticks on the y-axis.
\item \code{yLabel}: Label text for the y-axis.
\item \code{yValuesLimits}: Numeric vector specifying the limits for y-values.
}

Default values are set to provide a standardized look, but each parameter
can be tailored to fit specific visual needs. Modifying these parameters
will directly affect the aesthetics of the output plots.}
}
\value{
A \code{patchwork} object containing the combined ggplot objects if a
single output path is specified, or a list of \code{patchwork} objects for
multiple output paths.
}
\description{
Creates spider plots for sensitivity calculation. A spider plot is a
visualization technique that displays the sensitivity of a model output to
changes in model parameters. Each plot displays the sensitivity of a set of
PK parameters for a single output path to changes in model parameters. The x-axis
represents the value of model parameters (absolute or percent from default value),
and the y-axis represents the sensitivity of the output to changes in the model
parameter.
}
\examples{
\dontrun{
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
sensitivitySpiderPlot(results)

# Print plots with absolute y-axis values
sensitivitySpiderPlot(results, yAxisType = "absolute", yAxisFacetScales = "free")

# Print plots with custom configuration settings
myPlotConfiguration <- createEsqlabsPlotConfiguration()
myPlotConfiguration$pointsShape <- 22
myPlotConfiguration$subtitle <- "Custom settings"
sensitivitySpiderPlot(results, defaultPlotConfiguration = myPlotConfiguration)

# Use named parameter paths to customize legend labels
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
}

}
\seealso{
Other sensitivity-calculation: 
\code{\link{sensitivityCalculation}()},
\code{\link{sensitivityTimeProfiles}()},
\code{\link{sensitivityTornadoPlot}()}
}
\concept{sensitivity-calculation}
