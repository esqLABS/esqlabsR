# nocov start
esqlabsEnv$colorPalette <- .getEsqlabsColors()

.onLoad <- function(libname, pkgname) {
  Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")
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
