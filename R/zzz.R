# nocov start
esqlabsEnv$colorPalette <- .getEsqlabsColors()

.onLoad <- function(libname, pkgname) {
  Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")

  # Change maximal caption width in figures coming from TLF
  tlf::setDefaultMaxCharacterWidth(75)

  # tlf calls showtext::showtext_auto() on load, which rasterizes glyphs to
  # filled SVG paths using the system "sans" font. That font differs across OSes
  # (Helvetica on macOS, DejaVu/Liberation on Linux), so we disable it to get
  # portable <text> elements instead. This will become moot once the package
  # migrates to osp.plots.
  if (requireNamespace("showtext", quietly = TRUE)) {
    showtext::showtext_auto(enable = FALSE)
  }
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
