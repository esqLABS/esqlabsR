library(testthat)
library(esqlabsR)
library(ospsuite.utils)

# Disable these warnings for the tests
options(warnPartialMatchDollar = FALSE)

test_check("esqlabsR")
