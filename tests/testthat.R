library(testthat)
library(esqlabsR)

# Set TESTTHAT_CPUS dynamically before test_check() spawns workers.
# Config/testthat/parallel in DESCRIPTION enables parallelism;
# this controls how many workers are used.
numCores <- max(1L, parallel::detectCores() - 1L, na.rm = TRUE)
Sys.setenv(TESTTHAT_CPUS = numCores)

test_check("esqlabsR")
