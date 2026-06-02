# nocov start
esqlabsEnv$colorPalette <- .getEsqlabsColors()

.onLoad <- function(libname, pkgname) {
  Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")
  # showtext is currently installed along ospsuite package
  # and may be enabled which can lead to inconsistent exported plots
  # this chunk ensures that showtext is disabled
  if (requireNamespace("showtext", quietly = TRUE)) {
    showtext::showtext_auto(enable = FALSE)
  }
  options(
    ospsuite.plots.watermarkEnabled = FALSE,
    ggplot2.discrete.colour = NULL
    )
}

utils::globalVariables(c(
  "DataCombinedName",
  "IndividualId",
  "OutputPath",
  "OutputPathId",
  "PKMeanPercentChange",
  "PKParameter",
  "PKParameterValue",
  "PKPercentChange",
  "Parameter",
  "ParameterFactor",
  "ParameterPath",
  "ParameterValue",
  "QuantityPath",
  "SensitivityPKParameter",
  "Study Id",
  "Unit",
  "Value",
  "dataType",
  "name",
  "outputName",
  "paths",
  "plotGridName",
  "plotID",
  "scenario",
  "xOffsets",
  "xScaleFactors",
  "xValues",
  "yOffsets",
  "yScaleFactors",
  "yValues"
))
# nocov end
