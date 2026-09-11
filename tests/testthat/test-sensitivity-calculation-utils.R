# Paths shared by the tests below.
simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
parameterPaths <- c(
  "Aciclovir|Lipophilicity",
  "Events|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
  "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR-Aciclovir|GFR fraction"
)
variationRange <- c(0.1, 2, 20) # 1.0 is deliberately left out for testing

# `loadSimulation()` initializes a PK-Sim native session; running it at file
# source time (as `test_dir()` sources every test file up front) bleeds native
# state across files. Defer it behind a memoized accessor so the native load and
# the shared baseline `sensitivityCalculation()` happen inside a `test_that()`
# block on first use, computed once and cached for the rest of the file.
sensFixture <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      simulation <- loadSimulation(simPath)
      withr::local_seed(123)
      results <- sensitivityCalculation(
        simulation = simulation,
        outputPaths = outputPaths,
        parameterPaths = parameterPaths,
        variationRange = variationRange
      )
      cache <<- list(simulation = simulation, results = results)
    }
    cache
  }
})


test_that("saveSensitivityCalculation() writes files and respects overwrite flag", {
  results <- sensFixture()$results
  tempDir <- withr::local_tempdir()

  expect_no_error(
    saveSensitivityCalculation(results, outputDir = tempDir, overwrite = TRUE)
  )

  # One CSV per (parameter, factor), plus the metadata file and the bundled
  # simulation.pkml.
  expect_length(
    list.files(tempDir),
    (length(variationRange) + 1) * length(parameterPaths) + 2
  )

  # Save again without overwrite should fail
  expect_error(
    saveSensitivityCalculation(results, outputDir = tempDir, overwrite = FALSE),
    "already exists"
  )
})

test_that("loadSensitivityCalculation() restores functional results", {
  results <- sensFixture()$results
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

test_that("loadSensitivityCalculation() round-trips per-parameter variationRange", {
  # Per-parameter ranges yield a different set of factors per parameter, so the
  # naive `parameters * unique(factors)` count is wrong. The load must accept
  # the genuinely saved results.
  simulation <- sensFixture()$simulation
  withr::local_seed(123)
  perParamResults <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths[1:2],
    variationRange = list(c(0.5, 2), c(0.1, 0.5, 5, 10))
  )

  tempDir <- withr::local_tempdir()
  saveSensitivityCalculation(
    perParamResults,
    outputDir = tempDir,
    overwrite = TRUE
  )

  expect_no_error(resultLoaded <- loadSensitivityCalculation(tempDir))
  expect_s3_class(resultLoaded, "SensitivityCalculation")
  expect_equal(
    lengths(resultLoaded$simulationResults),
    lengths(perParamResults$simulationResults)
  )
  expect_equal(resultLoaded$pkData, perParamResults$pkData)
})

test_that("loadSensitivityCalculation() fails when simulation result file is missing", {
  results <- sensFixture()$results
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
    "Failed to load sensitivity calculation from"
  )
})

test_that("loadSensitivityCalculation() fails when simulation can't be retrieved", {
  results <- sensFixture()$results
  tempDir <- withr::local_tempdir()

  saveSensitivityCalculation(results, outputDir = tempDir, overwrite = TRUE)

  # Remove the bundled pkml so the source-path fallback is exercised, then point
  # it at a non-existent file.
  file.remove(file.path(tempDir, "simulation.pkml"))

  metaPath <- file.path(tempDir, "sensitivityCalculation.meta")
  meta <- readRDS(metaPath)
  meta$simFilePath <- tempfile(fileext = ".pkml")
  saveRDS(meta, metaPath)

  expect_error(
    loadSensitivityCalculation(tempDir),
    "Failed to load simulation from saved path"
  )
})

test_that("saveSensitivityCalculation() writes the simulation as simulation.pkml", {
  results <- sensFixture()$results
  tempDir <- withr::local_tempdir()

  saveSensitivityCalculation(results, outputDir = tempDir, overwrite = TRUE)

  expect_true(file.exists(file.path(tempDir, "simulation.pkml")))
})

test_that("loadSensitivityCalculation() uses the bundled simulation.pkml when the source path is invalid", {
  # The saved folder must be self-contained: loading has to succeed from the
  # bundled pkml even when the original source file is gone (moved/renamed or
  # the folder was shared with another machine).
  results <- sensFixture()$results
  tempDir <- withr::local_tempdir()
  saveSensitivityCalculation(results, outputDir = tempDir, overwrite = TRUE)

  metaPath <- file.path(tempDir, "sensitivityCalculation.meta")
  meta <- readRDS(metaPath)
  meta$simFilePath <- tempfile(fileext = ".pkml")
  saveRDS(meta, metaPath)

  expect_no_error(resultLoaded <- loadSensitivityCalculation(tempDir))
  expect_s3_class(resultLoaded, "SensitivityCalculation")
})

test_that("loadSensitivityCalculation() falls back to simFilePath when no bundled pkml is present", {
  # Backward compatibility with folders saved before the pkml was bundled: the
  # stored source path must still be used when simulation.pkml is absent.
  results <- sensFixture()$results
  tempDir <- withr::local_tempdir()
  saveSensitivityCalculation(results, outputDir = tempDir, overwrite = TRUE)

  # Assert removal so the test genuinely exercises the fallback rather than
  # silently loading the still-present bundled pkml.
  expect_true(file.remove(file.path(tempDir, "simulation.pkml")))

  expect_no_error(resultLoaded <- loadSensitivityCalculation(tempDir))
  expect_s3_class(resultLoaded, "SensitivityCalculation")
})

test_that("loadSensitivityCalculation() reports a clear error when the bundled pkml is corrupt", {
  # A corrupt bundled pkml must surface the same actionable error as a failing
  # source-path fallback, not an unwrapped low-level error.
  results <- sensFixture()$results
  tempDir <- withr::local_tempdir()
  saveSensitivityCalculation(results, outputDir = tempDir, overwrite = TRUE)

  bundledSimPath <- file.path(tempDir, "simulation.pkml")
  writeLines("not a valid pkml", bundledSimPath)

  expect_error(
    loadSensitivityCalculation(tempDir),
    "Failed to load simulation from saved path"
  )
})

test_that("saveSensitivityCalculation() succeeds when the first parameter's runs all failed", {
  # When every run for the first parameter fails, its bucket is dropped and
  # `simulationResults[[1]]` is empty, so the simulation must be located by
  # scanning for the first retained result rather than indexing [[1]][[1]].
  results <- sensFixture()$results
  resultsFirstFailed <- results
  resultsFirstFailed$simulationResults[[1]] <- list()

  tempDir <- withr::local_tempdir()
  expect_no_error(
    saveSensitivityCalculation(
      resultsFirstFailed,
      outputDir = tempDir,
      overwrite = TRUE
    )
  )
  expect_true(file.exists(file.path(tempDir, "simulation.pkml")))
})

test_that(".simulationResultsToPKDataFrame() handles a parameter whose runs all failed", {
  # When every run for a parameter fails, the nesting step drops all results,
  # leaving an empty named list. The extractor must warn and return a
  # well-formed empty frame rather than erroring in dplyr::rename().
  emptyBatch <- stats::setNames(list(), character(0))

  expect_warning(
    pkData <- .simulationResultsToPKDataFrame(
      emptyBatch,
      "SomeParameter",
      NULL
    ),
    "All simulation runs failed"
  )

  expect_equal(nrow(pkData), 0L)
  expect_equal(pkData, .emptyPKDataFrame())
})

test_that(".computePercentChange() handles missing baseline simulation data", {
  # `OutputPath` is always present in production: `.computePercentChange()` is
  # only reached from `.simulationResultsToPKDataFrame()`, after `QuantityPath`
  # has been renamed to `OutputPath`, and the baselines are joined on it.
  successData <- data.frame(
    OutputPath = "TestOutput",
    ParameterPath = "TestPath",
    PKParameter = "C_max",
    ParameterFactor = c(0.5, 1.0, 2.0),
    PKParameterValue = c(10, 20, 30),
    ParameterValue = c(5, 10, 20),
    stringsAsFactors = FALSE
  )

  failureData <- data.frame(
    OutputPath = "TestOutput",
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
    "SensitivityPKParameter could not be calculated"
  )

  expect_equal(colnames(resultSuccess), colnames(resultFailure))
  expect_true(all(is.na(resultFailure$PKPercentChange)))
  expect_true(all(is.na(resultFailure$SensitivityPKParameter)))
  expect_equal(nrow(resultFailure), nrow(failureData))
})

test_that(".computePercentChange() aligns baselines by OutputPath, not by row order", {
  # Two output paths with different baselines. Rows are interleaved (not ordered
  # output-major), so silent vector recycling would misalign baselines. The join
  # on OutputPath must pick each row's own baseline.
  data <- data.frame(
    OutputPath = c("OutA", "OutB", "OutA", "OutB"),
    ParameterPath = "TestPath",
    PKParameter = "C_max",
    ParameterFactor = c(1.0, 1.0, 2.0, 2.0),
    PKParameterValue = c(10, 100, 20, 400),
    ParameterValue = c(5, 5, 10, 10),
    stringsAsFactors = FALSE
  )

  result <- .computePercentChange(data)
  result <- result[result$ParameterFactor == 2.0, ]
  result <- result[order(result$OutputPath), ]

  # OutA: (20 - 10) / 10 * 100 = 100; OutB: (400 - 100) / 100 * 100 = 300
  expect_equal(result$PKPercentChange, c(100, 300))
})

test_that(".computePercentChange() names the parameter in the missing-baseline warning under group_modify", {
  # group_modify() strips the grouping columns from .x, so without the group
  # keys the warning would render with blank ParameterPath / PKParameter.
  failureData <- data.frame(
    OutputPath = "OutA",
    ParameterPath = "TestPath",
    PKParameter = "C_max",
    ParameterFactor = c(0.5, 2.0),
    PKParameterValue = c(10, 30),
    ParameterValue = c(5, 20),
    stringsAsFactors = FALSE
  )

  expect_warning(
    dplyr::group_by(failureData, ParameterPath, PKParameter) |>
      dplyr::group_modify(.f = ~ .computePercentChange(.x, .y)) |>
      dplyr::ungroup(),
    "TestPath"
  )
})

test_that(".computePercentChange() integrates correctly under group_modify with multiple OutputPaths", {
  # Reproduces the production call path: grouped by ParameterPath + PKParameter,
  # with OutputPath as a non-grouping column, so a group spans both outputs.
  data <- data.frame(
    OutputPath = c("OutA", "OutB", "OutA", "OutB"),
    ParameterPath = "TestPath",
    PKParameter = "C_max",
    ParameterFactor = c(1.0, 1.0, 2.0, 2.0),
    PKParameterValue = c(10, 100, 20, 400),
    ParameterValue = c(5, 5, 10, 10),
    stringsAsFactors = FALSE
  )

  result <- dplyr::group_by(data, ParameterPath, PKParameter) |>
    dplyr::group_modify(.f = ~ .computePercentChange(.x, .y)) |>
    dplyr::ungroup()
  result <- result[result$ParameterFactor == 2.0, ]
  result <- result[order(result$OutputPath), ]

  expect_equal(result$PKPercentChange, c(100, 300))
})

# End-to-end through sensitivityCalculation() -----------------------------

test_that("missing-baseline warning carries parameter context through sensitivityCalculation()", {
  # Exercise the warning through the real production path, not the bare helper:
  # group_modify() strips the grouping columns, so the warning would otherwise
  # render with blank ParameterPath / PKParameter names.
  localSim <- loadSimulation(simPath)
  parameterPath <- "Aciclovir|Lipophilicity"

  realRunBatches <- runSimulationBatches
  local_mocked_bindings(
    runSimulationBatches = function(simulationBatches, ...) {
      out <- realRunBatches(simulationBatches = simulationBatches, ...)
      # Drop the baseline (ParameterFactor == 1, sorted first) run so the
      # percent-change groups have no baseline row.
      out[[1]][[1]] <- NULL
      out
    }
  )

  warnings <- testthat::capture_warnings(
    sensitivityCalculation(
      localSim,
      outputPaths,
      parameterPath,
      variationRange = c(2, 5)
    )
  )

  baselineWarnings <- grep("could not be calculated", warnings, value = TRUE)
  expect_gt(length(baselineWarnings), 0)
  expect_true(all(grepl(parameterPath, baselineWarnings, fixed = TRUE)))
})

test_that("sensitivityCalculation() does not error when every run for a parameter fails", {
  localSim <- loadSimulation(simPath)
  parameterPath <- "Aciclovir|Lipophilicity"

  realRunBatches <- runSimulationBatches
  local_mocked_bindings(
    runSimulationBatches = function(simulationBatches, ...) {
      out <- realRunBatches(simulationBatches = simulationBatches, ...)
      # Every run for the (single, constant) parameter fails.
      out[[1]][] <- NULL
      out
    }
  )

  expect_no_error(
    res <- suppressWarnings(
      sensitivityCalculation(
        localSim,
        outputPaths,
        parameterPath,
        variationRange = c(2, 5)
      )
    )
  )
  expect_s3_class(res, "SensitivityCalculation")
  expect_equal(nrow(res$pkData), 0L)
})
