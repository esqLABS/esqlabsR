# nocov start
esqlabsEnv$colorPalette <- .getEsqlabsColors()

.onLoad <- function(libname, pkgname) {
  Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")

  # Change maximal caption width in figures coming from TLF
  tlf::setDefaultMaxCharacterWidth(75)

}
# nocov end
