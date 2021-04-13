.onLoad <- function(...) {
  options(warnPartialMatchDollar = TRUE)
  Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")
}
