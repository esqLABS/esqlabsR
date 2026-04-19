# Reusable DataCombined fixtures.
#
# This file is sourced from setup.R (once per parallel worker) and populates
# the `.testFixtures` environment (declared in helpers.R) with DataCombined
# objects and accessor functions shared across tests.
#
# Each DC is stored as `.testFixtures$fooDC` and exposed via an accessor that
# returns a deep clone, so tests can safely mutate what they receive without
# poisoning other tests (same pattern as OSPSuite-R PR #1859).

# DataCombined specification dataframe ---------------------------------------
# Used by validation tests in test-utilities-data-combined.R and
# test-create-plots-from-excel.R.
.testFixtures$dataCombinedSpec <- data.frame(list(
  "DataCombinedName" = c(
    "AciclovirPVB",
    "AciclovirPVB",
    "DC_missingPath",
    "DC_missingPath"
  ),
  "dataType" = c("simulated", "observed", "simulated", "observed"),
  "label" = c(
    "Aciclovir simulated",
    "Aciclovir observed",
    "Aciclovir simulated",
    "Aciclovir observed"
  ),
  "scenario" = c("TestScenario", NA, "TestScenario", NA),
  "path" = c(
    .testFixtures$aciclovirSaOutputPath,
    NA,
    .testFixtures$aciclovirSaOutputPath,
    NA
  ),
  "dataSet" = c(
    NA,
    names(.testFixtures$aciclovirObsData),
    NA,
    names(.testFixtures$aciclovirObsData)
  ),
  "group" = c(
    "Aciclovir PVB",
    "Aciclovir PVB",
    "Aciclovir PVB",
    "Aciclovir PVB"
  ),
  "xOffsets" = c(NA, NA, NA, NA),
  "xOffsetsUnits" = c(NA, NA, NA, NA),
  "yOffsets" = c(NA, NA, NA, NA),
  "yOffsetsUnits" = c(NA, NA, NA, NA),
  "xScaleFactors" = c(NA, NA, NA, NA),
  "yScaleFactors" = c(NA, NA, NA, NA)
))
# data.frame is value-type in R; tests that mutate get automatic copy-on-write.
dataCombinedSpec <- function() .testFixtures$dataCombinedSpec

# Variant without the "missing path" rows, used by test-create-plots-from-excel.
.testFixtures$dataCombinedSpecSingle <- .testFixtures$dataCombinedSpec[1:2, ]
dataCombinedSpecSingle <- function() .testFixtures$dataCombinedSpecSingle
