# The numeric-snapshot blocks below capture tabular output whose formatting
# depends on print/precision options. Each such block sets them locally with
# `withr::local_options()` (see `.localSnapshotOptions()`), so the state is
# scoped to the test rather than leaked at file scope.
.localSnapshotOptions <- function(.local_envir = parent.frame()) {
  withr::local_options(
    tibble.width = Inf,
    pillar.min_title_chars = Inf,
    pillar.sigfig = 4,
    digits = 4,
    scipen = 999,
    .local_envir = .local_envir
  )
}

# Single output path ------------------------------------------------------

# Paths shared by the single-output-path tests below.
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
# state across files. Defer it behind memoized accessors so the native load and
# the baseline `sensitivityCalculation()` happen inside a `test_that()` block on
# first use, computed once and cached for the rest of the file. The multiple
# output paths fixture additionally exposes its own paths/variationRange because
# the tests in that section reference them directly.
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

sensFixtureMultiple <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      simulation <- loadSimulation(simPath)
      outputPaths <- c(
        "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
        "Organism|Age",
        "Organism|ArterialBlood|Plasma|Aciclovir"
      )
      parameterPaths <- c(
        "Aciclovir|Lipophilicity",
        "Events|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
        "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR-Aciclovir|GFR fraction"
      )
      variationRange <- c(0.1, 5, 10)
      resultsMultiple <- sensitivityCalculation(
        simulation = simulation,
        outputPaths = outputPaths,
        parameterPaths = parameterPaths,
        variationRange = variationRange
      )
      cache <<- list(
        simulation = simulation,
        outputPaths = outputPaths,
        parameterPaths = parameterPaths,
        variationRange = variationRange,
        resultsMultiple = resultsMultiple
      )
    }
    cache
  }
})

# Validate outputPaths ----------------------------------------------------

test_that("sensitivityCalculation fails with invalid `outputPaths`", {
  simulation <- sensFixture()$simulation
  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = NULL,
      parameterPaths = parameterPaths
    ),
    'argument "outputPaths" is of type.*but expected <character>'
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = c(1, 2, 3),
      parameterPaths = parameterPaths
    ),
    'argument "outputPaths" is of type <numeric>.*but expected <character>'
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = list("pathNameA" = "pathA"),
      parameterPaths = parameterPaths
    ),
    'argument "outputPaths" is of type <list>.*but expected <character>'
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = "",
      parameterPaths = parameterPaths
    ),
    'argument "outputPaths" has empty strings'
  )

  # The validator prefixes the message with the enclosing function name, which
  # differs across run contexts (`test_that()`, `test_file()`, `test_dir()`);
  # scrub it so the snapshot only pins the validation message itself.
  scrubCaller <- \(lines) gsub("`[^`]+\\(\\)`: ", "", lines)

  expect_snapshot(
    error = TRUE,
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = c(
        "",
        "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
      ),
      parameterPaths = parameterPaths
    ),
    transform = scrubCaller
  )

  expect_snapshot(
    error = TRUE,
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = rep(
        "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
        2
      ),
      parameterPaths = parameterPaths
    ),
    transform = scrubCaller
  )
})

# Validate parameterPaths -------------------------------------------------

test_that("sensitivityCalculation fails with invalid `parameterPaths`", {
  simulation <- sensFixture()$simulation
  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      parameterPaths = NULL
    ),
    'argument "parameterPaths" is of type.*but expected <character>'
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      parameterPaths = c(1, 2, 3)
    ),
    'argument "parameterPaths" is of type <numeric>.*but expected <character>'
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      parameterPaths = ""
    ),
    'argument "parameterPaths" has empty strings'
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      parameterPaths = c(
        "Aciclovir|Lipophilicity",
        "",
        "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR-Aciclovir|GFR fraction"
      )
    ),
    'argument "parameterPaths" has empty strings'
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      parameterPaths = c(parameterPaths, parameterPaths[1])
    ),
    "duplicated values"
  )
})

# Validate pkParameters ---------------------------------------------------

test_that("sensitivityCalculation fails with invalid `pkParameters`", {
  simulation <- sensFixture()$simulation
  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      pkParameters = c(1, 2, 3),
      outputPaths = outputPaths,
      parameterPaths = parameterPaths
    ),
    'argument "pkParameters" is of type <numeric>.*but expected <character>'
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      pkParameters = "",
      outputPaths = outputPaths,
      parameterPaths = parameterPaths
    ),
    'argument "pkParameters" has empty strings'
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      pkParameters = c("", "C_max"),
      outputPaths = outputPaths,
      parameterPaths = parameterPaths
    ),
    'argument "pkParameters" has empty strings'
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      pkParameters = c("C_max", "C_max"),
      outputPaths = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      parameterPaths = parameterPaths
    ),
    "duplicated values"
  )

  expect_message(
    sensitivityCalculation(
      simulation = simulation,
      pkParameters = c("C_max", "abc", "xyz"),
      outputPaths = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      parameterPaths = parameterPaths
    ),
    "PK parameters are specified but were not calculated"
  )
})

test_that("sensitivityCalculation works with user-defined `pkParameters`", {
  simulation <- sensFixture()$simulation
  # Create a new parameter based on the standard AUC parameter
  myAUC <- addUserDefinedPKParameter(
    name = "MyAUC",
    standardPKParameter = StandardPKParameter$AUC_tEnd
  )

  results <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    variationRange = variationRange,
    pkParameters = c("C_max", "MyAUC")
  )

  expect_true(isOfType(results, "SensitivityCalculation"))
  expect_equal(unique(results$pkData$PKParameter), c("C_max", "MyAUC"))
})

# Validate variationRange -------------------------------------------------

test_that("sensitivityCalculation fails with invalid `variationRange`", {
  simulation <- sensFixture()$simulation
  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = outputPaths,
      parameterPaths = parameterPaths,
      variationRange = c("x", "y", "z")
    ),
    regexp = messages$errorWrongType(
      "variationRange",
      "character",
      "numeric/integer"
    ),
    fixed = TRUE
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = outputPaths,
      parameterPaths = parameterPaths,
      variationRange = list(c(0.1, 1, 10), c("x", "y", "z"), c(0.1, 1, 10)),
    ),
    regexp = messages$errorWrongType(
      "variationRange",
      "character",
      "numeric/integer"
    ),
    fixed = TRUE
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = outputPaths,
      parameterPaths = parameterPaths,
      variationRange = list(c(0.1, 1, 10), c(0.1, 1, 10)),
    ),
    "must be either a vector or a list equal to the length"
  )
})

# Simulation output-selection restore -------------------------------------

test_that("sensitivityCalculation restores the caller's output selections", {
  simulation <- loadSimulation(simPath)
  callerOutputs <- c(
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
    "Organism|ArterialBlood|Plasma|Aciclovir"
  )
  ospsuite::setOutputs(
    quantitiesOrPaths = callerOutputs,
    simulation = simulation
  )

  sensitivityCalculation(
    simulation = simulation,
    outputPaths = "Organism|Age",
    parameterPaths = "Aciclovir|Lipophilicity",
    variationRange = variationRange
  )

  restored <- vapply(
    simulation$outputSelections$allOutputs,
    function(sel) sel$path,
    character(1)
  )
  expect_setequal(restored, callerOutputs)
})

# Validate customOutputFunctions ------------------------------------------

test_that("sensitivityCalculation fails with invalid `customOutputFunctions`", {
  simulation <- sensFixture()$simulation
  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = outputPaths,
      parameterPaths = parameterPaths,
      variationRange = c(0.1, 2, 20),
      customOutputFunctions = "invalid"
    ),
    regexp = messages$errorWrongType(
      "customOutputFunctions",
      "character",
      "list"
    ),
    fixed = TRUE
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = outputPaths,
      parameterPaths = parameterPaths,
      variationRange = c(0.1, 2, 20),
      customOutputFunctions = list("invalid" = "function")
    ),
    regexp = messages$errorWrongType(
      "customOutputFunctions",
      "list",
      "function"
    ),
    fixed = TRUE
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = outputPaths,
      parameterPaths = parameterPaths,
      variationRange = c(0.1, 2, 20),
      customOutputFunctions = list(
        function(x) x,
        function(y) y
      )
    ),
    "`customOutputFunctions` is not a named list"
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = outputPaths,
      parameterPaths = parameterPaths,
      variationRange = c(0.1, 2, 20),
      customOutputFunctions = list(
        "funA" = function(x) x,
        function(y) y,
        "funC" = function(x) x^2
      )
    ),
    "`customOutputFunctions` is not a named list"
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = outputPaths,
      parameterPaths = parameterPaths,
      variationRange = c(0.1, 2, 20),
      customOutputFunctions = list("invalid" = function(x, y, z) {
        x / y * z
      })
    ),
    "The user-defined function must have either"
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = outputPaths,
      parameterPaths = parameterPaths,
      variationRange = c(0.1, 2, 20),
      customOutputFunctions = list("invalid" = \(x, y, z) x / y * z)
    ),
    "The user-defined function must have either"
  )
})

# Validate variationType

test_that("sensitivityCalculation fails with invalid `variationType`", {
  simulation <- sensFixture()$simulation
  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = outputPaths,
      parameterPaths = parameterPaths,
      variationType = "invalidType"
    ),
    'should be one of \\"relative\\", \\"absolute\\"'
  )
})

test_that("sensitivityCalculation errors on absolute variation with zero initial value", {
  # A parameter whose initial value is 0 has no multiplicative scaling that
  # reaches a non-zero absolute target, so the conversion to relative factors
  # must be refused rather than yielding Inf/NaN.
  localSim <- loadSimulation(simPath)
  zeroParam <- "Aciclovir|Lipophilicity"
  setParameterValuesByPath(zeroParam, 0, localSim)

  expect_snapshot(
    sensitivityCalculation(
      simulation = localSim,
      outputPaths = outputPaths,
      parameterPaths = zeroParam,
      variationRange = c(1, 2),
      variationType = "absolute"
    ),
    error = TRUE
  )
})

# Check SensitivityCalculation object -------------------------------------

test_that("sensitivityCalculation returns a valid `SensitivityCalculation` object", {
  results <- sensFixture()$results
  expect_true(isOfType(results, "SensitivityCalculation"))

  expect_equal(
    length(results$simulationResults),
    length(parameterPaths)
  )

  expect_equal(
    length(results$simulationResults[[1]]),
    length(variationRange) + 1L
  )

  expect_equal(
    length(results$parameterPaths),
    length(parameterPaths)
  )
})

# Test variationRange -----------------------------------------------------

test_that("sensitivityCalculation works with absolute values of `variationRange`", {
  fixture <- sensFixture()
  simulation <- fixture$simulation
  results <- fixture$results
  variationRangeAbs <- list(
    -0.097 * variationRange,
    0.00025 * variationRange,
    1 * variationRange
  )

  withr::local_seed(123)
  resultsAbs <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    variationRange = variationRangeAbs,
    variationType = "absolute"
  )

  expect_equal(results$pkData, resultsAbs$pkData)
})

# Check PK tidy data ------------------------------------------------------

test_that("sensitivityCalculation returns correct PK parameters dataframe", {
  results <- sensFixture()$results
  expect_equal(
    colnames(results$pkData),
    c(
      "OutputPath",
      "ParameterPath",
      "ParameterFactor",
      "ParameterValue",
      "ParameterUnit",
      "ParameterPathUserName",
      "PKParameter",
      "PKParameterValue",
      "PKPercentChange",
      "Unit",
      "SensitivityPKParameter"
    )
  )
})

test_that("sensitivityCalculation PK parameters tidy dataframe is as expected", {
  .localSnapshotOptions()
  results <- sensFixture()$results
  # base scaling should be present
  expect_equal(unique(results$pkData$ParameterFactor), c(0.1, 1, 2, 20))

  df1_pk <- summarizer(results$pkData, parameterPaths[1])
  expect_snapshot(df1_pk)

  df2_pk <- summarizer(results$pkData, parameterPaths[2])
  expect_snapshot(df2_pk)

  df3_pk <- summarizer(results$pkData, parameterPaths[3])
  expect_snapshot(df3_pk)
})

# Test customOutputFunctions ----------------------------------------------

test_that("sensitivityCalculation returns expected results with single custom function", {
  .localSnapshotOptions()
  simulation <- sensFixture()$simulation
  # list with custom function using only `y` parameter
  customFunctions <- list("minmax" = function(y) min(y[y != 0]) / max(y))
  customFunctionsLambda <- list("minmax" = \(y) min(y[y != 0]) / max(y))

  results <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    customOutputFunctions = customFunctions,
    variationRange = variationRange
  )

  resultsLambda <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    customOutputFunctions = customFunctionsLambda,
    variationRange = variationRange
  )

  expect_equal(results$pkData, resultsLambda$pkData, tolerance = 1e-3)

  customPKData <- dplyr::filter(
    results$pkData,
    PKParameter %in% names(customFunctions)
  )
  expect_snapshot(customPKData)
})

test_that("sensitivityCalculation returns expected results with multiple custom functions", {
  .localSnapshotOptions()
  simulation <- sensFixture()$simulation
  # List with multiple custom functions using `x` and `y` parameter
  customFunctions <- list(
    "minmax" = function(y) {
      max(y) / min(y[y != 0])
    },
    "max_slope" = function(x, y) {
      slopes <- diff(y) / diff(x)
      max(slopes)
    }
  )

  # Perform the sensitivity calculation
  results <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    customOutputFunctions = customFunctions,
    variationRange = variationRange
  )

  # Filter the custom PK data
  customPKData <- results$pkData |>
    dplyr::filter(PKParameter %in% names(customFunctions))

  # Expect snapshot
  expect_snapshot(customPKData)
})

# Test saving to xlsx file ------------------------------------------------

test_that("sensitivityCalculation saves PK data to xlsx file", {
  simulation <- sensFixture()$simulation
  path <- withr::local_tempfile(fileext = ".xlsx")

  withr::local_seed(123)
  results <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    variationRange = c(0.1, 2, 20),
    saOutputFilePath = path
  )

  expect_true(file.exists(path))
})

test_that("sensitivityCalculation errors if file extension is incorrect", {
  simulation <- sensFixture()$simulation
  path <- "mydata.csv"

  withr::local_seed(123)
  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = outputPaths,
      parameterPaths = parameterPaths,
      variationRange = c(0.1, 2, 20),
      saOutputFilePath = path
    ),
    "Provided file has extension 'csv', while 'xlsx' was expected instead."
  )
})

# Check PK wide data ------------------------------------------------------

pkDataWideColumns <- c(
  "OutputPath",
  "ParameterPath",
  "ParameterFactor",
  "ParameterValue",
  "ParameterUnit",
  "ParameterPathUserName",
  "C_max",
  "C_max_norm",
  "C_max_Unit",
  "C_max_norm_Unit",
  "C_max_PKPercentChange",
  "C_max_norm_PKPercentChange",
  "C_max_Sensitivity",
  "C_max_norm_Sensitivity",
  "t_max",
  "t_max_Unit",
  "t_max_PKPercentChange",
  "t_max_Sensitivity",
  "AUC_tEnd",
  "AUC_tEnd_norm",
  "AUC_tEnd_Unit",
  "AUC_tEnd_norm_Unit",
  "AUC_tEnd_PKPercentChange",
  "AUC_tEnd_norm_PKPercentChange",
  "AUC_tEnd_Sensitivity",
  "AUC_tEnd_norm_Sensitivity",
  "AUC_inf",
  "AUC_inf_norm",
  "AUC_inf_Unit",
  "AUC_inf_norm_Unit",
  "AUC_inf_PKPercentChange",
  "AUC_inf_norm_PKPercentChange",
  "AUC_inf_Sensitivity",
  "AUC_inf_norm_Sensitivity",
  "CL",
  "FractionAucLastToInf",
  "CL_Unit",
  "FractionAucLastToInf_Unit",
  "CL_PKPercentChange",
  "FractionAucLastToInf_PKPercentChange",
  "CL_Sensitivity",
  "FractionAucLastToInf_Sensitivity",
  "MRT",
  "MRT_Unit",
  "MRT_PKPercentChange",
  "MRT_Sensitivity",
  "Thalf",
  "Thalf_Unit",
  "Thalf_PKPercentChange",
  "Thalf_Sensitivity",
  "Vss",
  "Vss_Unit",
  "Vss_PKPercentChange",
  "Vss_Sensitivity",
  "Vd",
  "Vd_Unit",
  "Vd_PKPercentChange",
  "Vd_Sensitivity"
)

test_that("sensitivityCalculation converts output to wide format as expected", {
  simulation <- sensFixture()$simulation
  withr::local_seed(123)
  results2 <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    variationRange = c(0.1, 2, 20),
    pkParameters = NULL
  )
  pkDataWide <- esqlabsR:::.convertToWide(results2$pkData)

  expect_equal(dim(pkDataWide), c(12L, 58L))
  expect_equal(colnames(pkDataWide), pkDataWideColumns)
})

test_that("sensitivityCalculation converts output to wide format as expected with `customOutputFunctions`", {
  simulation <- sensFixture()$simulation
  customFunctions <- list(
    "minmax" = function(y) {
      max(y) / min(y[y != 0])
    },
    "max_slope" = function(x, y) {
      slopes <- diff(y) / diff(x)
      max(slopes)
    }
  )
  pkDataWideColumns <- c(
    pkDataWideColumns,
    "minmax",
    "minmax_Unit",
    "minmax_PKPercentChange",
    "minmax_Sensitivity",
    "max_slope",
    "max_slope_Unit",
    "max_slope_PKPercentChange",
    "max_slope_Sensitivity"
  )

  withr::local_seed(123)
  results2 <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    variationRange = c(0.1, 2, 20),
    customOutputFunctions = customFunctions,
    pkParameters = NULL
  )
  pkParameterNames <- c(
    names(ospsuite::StandardPKParameter),
    names(customFunctions)
  )
  pkDataWide <- esqlabsR:::.convertToWide(results2$pkData, pkParameterNames)

  expect_equal(dim(pkDataWide), c(12L, 66L))
  expect_equal(colnames(pkDataWide), pkDataWideColumns)
})

# Test sensitivityCalculation when simulation fails -----------------------

test_that("sensitivityCalculation handles simulation failure", {
  simulation <- sensFixture()$simulation
  expect_warning(
    expect_warning(
      resultsSimFailure <- sensitivityCalculation(
        simulation = simulation,
        outputPaths = outputPaths,
        parameterPaths = parameterPaths,
        variationRange = c(-1, 2, 10)
      ),
      "Simulation run failed"
    )
  )

  expect_true(isOfType(resultsSimFailure, "SensitivityCalculation"))

  expect_equal(
    length(resultsSimFailure$simulationResults),
    length(parameterPaths)
  )

  expect_equal(
    length(resultsSimFailure$simulationResults[[1]]),
    length(variationRange) + 1L
  )

  expect_equal(
    # path with failed simulation
    length(resultsSimFailure$simulationResults[[2]]),
    length(variationRange)
  )

  expect_equal(
    length(resultsSimFailure$parameterPaths),
    length(parameterPaths)
  )
})

# Multiple output paths ---------------------------------------------------

test_that("sensitivityCalculation extracts data for multiple output paths", {
  fixture <- sensFixtureMultiple()
  resultsMultiple <- fixture$resultsMultiple
  outputPaths <- fixture$outputPaths
  expect_identical(nrow(resultsMultiple$pkData), 108L)
  expect_equal(unique(resultsMultiple$pkData$OutputPath), outputPaths)
})

test_that("sensitivityCalculation applies absolute `variationRange` for multiple paths", {
  fixture <- sensFixtureMultiple()
  simulation <- fixture$simulation
  outputPaths <- fixture$outputPaths
  parameterPaths <- fixture$parameterPaths
  variationRange <- fixture$variationRange
  resultsMultiple <- fixture$resultsMultiple
  variationRangeAbs <- list(
    -0.097 * variationRange,
    0.00025 * variationRange,
    1 * variationRange
  )

  withr::local_seed(123)
  resultsMultipleAbs <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    variationRange = variationRangeAbs,
    variationType = "absolute"
  )

  expect_equal(resultsMultiple$pkData, resultsMultipleAbs$pkData)
})

test_that("sensitivityCalculation applies custom PK function with multiple output paths", {
  .localSnapshotOptions()
  fixture <- sensFixtureMultiple()
  simulation <- fixture$simulation
  outputPaths <- fixture$outputPaths
  parameterPaths <- fixture$parameterPaths
  # list with custom function using only `y` parameter
  customFunctions <- list("minmax" = function(y) min(y[y != 0]) / max(y))

  results_multiple <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    customOutputFunctions = customFunctions,
    variationRange = c(1, 5, 10)
  )

  customPKDataMultiple <- dplyr::filter(
    results_multiple$pkData,
    PKParameter %in% names(customFunctions)
  )
  expect_snapshot(customPKDataMultiple)
})

test_that("sensitivityCalculation saves PK data to xlsx for multiple output paths", {
  fixture <- sensFixtureMultiple()
  simulation <- fixture$simulation
  outputPaths <- fixture$outputPaths
  parameterPaths <- fixture$parameterPaths
  path <- withr::local_tempfile(fileext = ".xlsx")

  withr::local_seed(123)
  resultsMultiple <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    variationRange = c(0.1, 5, 10),
    saOutputFilePath = path
  )

  expect_true(file.exists(path))
})

test_that("sensitivityCalculation handles simulation failure for multiple output paths", {
  fixture <- sensFixtureMultiple()
  simulation <- fixture$simulation
  outputPaths <- fixture$outputPaths
  parameterPaths <- fixture$parameterPaths
  resultsMultiple <- fixture$resultsMultiple
  expect_warning(
    expect_warning(
      resultsMultipleSimFailure <- sensitivityCalculation(
        simulation = simulation,
        outputPaths = outputPaths,
        parameterPaths = parameterPaths,
        variationRange = c(-1, 2, 10)
      ),
      "Simulation run failed"
    )
  )

  expect_identical(nrow(resultsMultipleSimFailure$pkData), 99L)
  expect_equal(unique(resultsMultiple$pkData$OutputPath), outputPaths)
})
