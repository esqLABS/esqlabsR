library(testthat)
library(esqlabsR)

# Disable these warnings for the tests
options(warnPartialMatchDollar = FALSE)

# Respect pre-set TESTTHAT_CPUS (CI / CRAN / user). Only set a default when
# unset. Honour R CMD check's limit-cores rule and cap at 4 so we do not
# exhaust memory on large machines (each worker holds its own ospsuite/.NET
# state).
if (identical(Sys.getenv("TESTTHAT_CPUS"), "")) {
  limitCores <- !identical(Sys.getenv("_R_CHECK_LIMIT_CORES_"), "")
  detected <- tryCatch(
    parallel::detectCores(logical = FALSE),
    error = function(e) 1L
  )
  if (is.na(detected)) detected <- 1L
  numCores <- if (limitCores) min(2L, detected) else min(4L, max(1L, detected - 1L))
  Sys.setenv(TESTTHAT_CPUS = numCores)
}

test_check("esqlabsR")
