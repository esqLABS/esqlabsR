library(testthat)
library(esqlabsR)
library(ospsuite.utils)

# Disable these warnings for the tests
options(warnPartialMatchDollar = FALSE)

numCores <- parallel::detectCores()
numCores <- if (is.na(numCores)) 1L else numCores
Sys.setenv(TESTTHAT_CPUS = as.character(numCores))

test_check("esqlabsR")
