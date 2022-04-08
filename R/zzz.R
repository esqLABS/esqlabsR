.onLoad <- function(...) {
  options(warnPartialMatchDollar = TRUE, warn = -99)
  Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")
}
