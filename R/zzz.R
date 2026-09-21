# nocov start
.onLoad <- function(libname, pkgname) {
  esqlabsEnv$colorPalette <- .getEsqlabsColors()

  # Change maximal caption width in figures coming from TLF
  tlf::setDefaultMaxCharacterWidth(75)
}

# The package reaches every dependency as `pkg::fun()`, so this is the one
# `@importFrom` it carries. `R6::R6Class()` is only ever called at top level
# (`R/project.R`, `R/validation.R`), and `R CMD check` does not credit a
# top-level `::` call as a use of the package, so without an import directive it
# reports R6 as a declared-but-unused import. The call sites keep their explicit
# `R6::` prefix.
#
#' @importFrom R6 R6Class
NULL

# Column names that exist only inside a data mask (dplyr verbs that build or read
# them), which `R CMD check` cannot see are bound and so reports as undefined
# globals. Declared here rather than rewritten as `.data$` pronouns at the call
# sites, which are in the sensitivity code this refactor leaves alone.
utils::globalVariables(c(
  "name",
  "ParameterBaseValue",
  "PKParameterBaseValue"
))
# nocov end
