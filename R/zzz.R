# nocov start
.onLoad <- function(libname, pkgname) {
  esqlabsEnv$colorPalette <- .getEsqlabsColors()

  # Change maximal caption width in figures coming from TLF
  tlf::setDefaultMaxCharacterWidth(75)
}
# nocov end
