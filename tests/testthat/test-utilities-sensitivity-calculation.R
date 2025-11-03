# Load simulation and set paths for tests
simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
simulation <- loadSimulation(simPath)
outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
parameterPaths <- c(
  "Aciclovir|Lipophilicity",
  "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
  "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR fraction"
)
variationRange <- c(0.1, 2, 20) # 1.0 is deliberately left out for testing

set.seed(123)
results <- sensitivityCalculation(
  simulation = simulation,
  outputPaths = outputPaths,
  parameterPaths = parameterPaths,
  variationRange = variationRange
)


test_that("saveSensitivityCalculation() writes files and respects overwrite flag", {
  tempDir <- withr::local_tempdir()

  expect_no_error(
    saveSensitivityCalculation(results, outputDir = tempDir, overwrite = TRUE)
  )

  expect_length(
    list.files(tempDir),
    (length(variationRange) + 1) * length(parameterPaths) + 1
  )

  # Save again without overwrite should fail
  expect_error(
    saveSensitivityCalculation(results, outputDir = tempDir, overwrite = FALSE),
    messages$errorOutputDirExists(tempDir),
    fixed = TRUE
  )
})

test_that("loadSensitivityCalculation() restores functional results", {
  tempDir <- withr::local_tempdir()

  saveSensitivityCalculation(results, outputDir = tempDir, overwrite = TRUE)

  expect_no_error(resultLoaded <- loadSensitivityCalculation(tempDir))
  expect_s3_class(resultLoaded, "SensitivityCalculation")

  p <- sensitivityTimeProfiles(resultLoaded)
  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "sensitivityTimeProfiles reloaded results",
      fig = p
    )
  )
})

test_that("loadSensitivityCalculation() fails when simulation result file is missing", {
  tempDir <- withr::local_tempdir()

  saveSensitivityCalculation(results, outputDir = tempDir, overwrite = TRUE)

  fileToDelete <- list.files(
    tempDir,
    pattern = "^simulationResult_\\d+_\\d+\\.csv$",
    full.names = TRUE
  )[1]
  file.remove(fileToDelete)

  expect_error(
    loadSensitivityCalculation(tempDir),
    messages$errorCorruptSensitivityCalculation(tempDir),
    fixed = TRUE
  )
})

test_that("loadSensitivityCalculation() fails when simulation can't be retrieved", {
  tempDir <- withr::local_tempdir()

  saveSensitivityCalculation(results, outputDir = tempDir, overwrite = TRUE)

  metaPath <- file.path(tempDir, "sensitivityCalculation.meta")
  meta <- readRDS(metaPath)
  meta$simFilePath <- tempfile(fileext = ".pkml")
  saveRDS(meta, metaPath)

  err <- expect_error(
    loadSensitivityCalculation(tempDir),
    class = "error"
  )

  expect_true(startsWith(
    conditionMessage(err),
    messages$errorFailedToLoadSimulation(meta$simFilePath, "")
  ))
})

test_that(".computePercentChange() handles missing baseline simulation data", {
  successData <- data.frame(
    ParameterPath = "TestPath",
    PKParameter = "C_max",
    ParameterFactor = c(0.5, 1.0, 2.0),
    PKParameterValue = c(10, 20, 30),
    ParameterValue = c(5, 10, 20),
    stringsAsFactors = FALSE
  )

  failureData <- data.frame(
    ParameterPath = "TestPath",
    PKParameter = "C_max",
    ParameterFactor = c(0.5, 2.0),
    PKParameterValue = c(10, 30),
    ParameterValue = c(5, 20),
    stringsAsFactors = FALSE
  )

  resultSuccess <- .computePercentChange(successData)
  expect_warning(
    resultFailure <- .computePercentChange(failureData),
    messages$warningSensitivityPKParameterNotCalculated("TestPath", "C_max"),
    fixed = TRUE
  )

  expect_equal(colnames(resultSuccess), colnames(resultFailure))
  expect_true(all(is.na(resultFailure$PKPercentChange)))
  expect_true(all(is.na(resultFailure$SensitivityPKParameter)))
  expect_equal(nrow(resultFailure), nrow(failureData))
})
