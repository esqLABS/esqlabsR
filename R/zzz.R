# nocov start
.onLoad <- function(libname, pkgname) {
  esqlabsEnv$colorPalette <- .getEsqlabsColors()

  # `ospsuite.plots` adds a watermark when a standalone plot is printed. Keep it
  # off unless the user turned it on before loading the package.
  if (is.null(getOption("ospsuite.plots.watermarkEnabled"))) {
    options(ospsuite.plots.watermarkEnabled = FALSE)
  }
}
# nocov end
