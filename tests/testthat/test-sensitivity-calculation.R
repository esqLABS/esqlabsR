# Save old options
old_opts <- options()

options(
  tibble.width = Inf,
  pillar.min_title_chars = Inf,
  pillar.sigfig = 4,
  digits = 4,
  scipen = 999
)


# Single output path ------------------------------------------------------

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

# Validate outputPaths ----------------------------------------------------

test_that("sensitivityCalculation fails with invalid `outputPaths`", {
  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = NULL,
      parameterPaths = parameterPath
    ),
    "The argument `outputPaths` cannot be NULL"
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = c(1, 2, 3),
      parameterPaths = parameterPath
    ),
    'The argument `outputPaths` must be of type "character"'
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = "",
      parameterPaths = parameterPath
    ),
    "The argument `outputPaths` contains empty strings"
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = c("", "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"),
      parameterPaths = parameterPath
    ),
    "The argument `outputPaths` contains empty strings"
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = rep("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)", 2),
      parameterPaths = parameterPath
    ),
    "The argument `outputPaths` must contain only distinct values"
  )
})

# Validate parameterPaths -------------------------------------------------

test_that("sensitivityCalculation fails with invalid `parameterPaths`", {
  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      parameterPaths = NULL
    ),
    "The argument `parameterPaths` cannot be NULL"
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      parameterPaths = c(1, 2, 3)
    ),
    'The argument `parameterPaths` must be of type "character"'
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      parameterPaths = ""
    ),
    "The argument `parameterPaths` contains empty strings"
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      parameterPaths = c(
        "Aciclovir|Lipophilicity",
        "",
        "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR fraction"
      )
    ),
    "The argument `parameterPaths` contains empty strings"
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      parameterPaths = c(parameterPaths, parameterPaths[1])
    ),
    "The argument `parameterPaths` must contain only distinct values"
  )
})

# Validate pkParameters ---------------------------------------------------

test_that("sensitivityCalculation fails with invalid `pkParameters`", {
  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      pkParameters = c(1, 2, 3),
      outputPaths = outputPaths,
      parameterPaths = parameterPaths
    ),
    'The argument `pkParameters` must be of type "character"'
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      pkParameters = "",
      outputPaths = outputPaths,
      parameterPaths = parameterPaths
    ),
    "The argument `pkParameters` contains empty strings"
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      pkParameters = c("", "C_max"),
      outputPaths = outputPaths,
      parameterPaths = parameterPaths
    ),
    "The argument `pkParameters` contains empty strings"
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      pkParameters = c("C_max", "C_max"),
      outputPaths = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      parameterPaths = parameterPaths
    ),
    "The argument `pkParameters` must contain only distinct values"
  )

  expect_message(
    sensitivityCalculation(
      simulation = simulation,
      pkParameters = c("C_max", "abc", "xyz"),
      outputPaths = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      parameterPaths = parameterPaths
    ),
    "Following non-standard PK parameters will not be calculated:\nabc\nxyz\n",
    fixed = TRUE
  )
})

# Validate variationRange -------------------------------------------------

test_that("sensitivityCalculation fails with invalid `variationRange`", {
  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = outputPaths,
      parameterPaths = parameterPaths,
      variationRange = c("x", "y", "z")
    ),
    "argument 'variationRange' is of type 'character', but expected 'numeric, or integer'!"
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = outputPaths,
      parameterPaths = parameterPaths,
      variationRange = list(c(0.1, 1, 10), c("x", "y", "z"), c(0.1, 1, 10)),
    ),
    "argument 'variationRange' is of type 'character', but expected 'numeric, or integer'!"
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = outputPaths,
      parameterPaths = parameterPaths,
      variationRange = list(c(0.1, 1, 10), c(0.1, 1, 10)),
    ),
    "`variationRange` must be either a vector or a list equal to the length of `parameterPaths`"
  )
})

# Validate customOutputFunctions ------------------------------------------

test_that("sensitivityCalculation fails with invalid `customOutputFunctions`", {
  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = outputPaths,
      parameterPaths = parameterPaths,
      variationRange = c(0.1, 2, 20),
      customOutputFunctions = "invalid"
    ),
    "argument 'customOutputFunctions' is of type 'character', but expected 'list'!"
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = outputPaths,
      parameterPaths = parameterPaths,
      variationRange = c(0.1, 2, 20),
      customOutputFunctions = list("invalid" = "function")
    ),
    "argument 'customOutputFunctions' is of type 'list', but expected 'function'!"
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = outputPaths,
      parameterPaths = parameterPaths,
      variationRange = c(0.1, 2, 20),
      customOutputFunctions = list(
        function(x) x, function(y) y
      )
    ),
    "argument 'customOutputFunctions' is not a named list!"
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = outputPaths,
      parameterPaths = parameterPaths,
      variationRange = c(0.1, 2, 20),
      customOutputFunctions = list(
        "funA" = function(x) x, function(y) y, "funC" = function(x) x^2
      )
    ),
    "argument 'customOutputFunctions' is not a named list!"
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
    "The user-defined function must have either 'x', 'y', or both 'x' and 'y'"
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = outputPaths,
      parameterPaths = parameterPaths,
      variationRange = c(0.1, 2, 20),
      customOutputFunctions = list("invalid" = \(x, y, z) x / y * z)
    ),
    "The user-defined function must have either 'x', 'y', or both 'x' and 'y'"
  )
})

# Check SensitivityCalculation object -------------------------------------

test_that("sensitivityCalculation returns a valid `SensitivityCalculation` object", {
  expect_true(isOfType(results, "SensitivityCalculation"))

  expect_equal(
    length(results$simulationResults), length(parameterPaths)
  )

  expect_equal(
    length(results$simulationResults[[1]]), length(variationRange) + 1L
  )

  expect_equal(
    length(results$parameterPaths), length(parameterPaths)
  )
})

# Test variationRange -----------------------------------------------------

test_that("sensitivityCalculation works with absolute values of `variationRange`", {
  variationRangeAbs <- list(
    -0.097 * variationRange,
    0.00025 * variationRange,
    1 * variationRange
  )

  set.seed(123)
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
  expect_equal(
    colnames(results$pkData),
    c(
      "OutputPath", "ParameterPath", "ParameterFactor", "ParameterValue",
      "ParameterUnit", "PKParameter", "PKParameterValue", "PKPercentChange",
      "Unit", "SensitivityPKParameter"
    )
  )
})

test_that("sensitivityCalculation PK parameters tidy dataframe is as expected", {
  # base scaling should be present
  expect_equal(unique(results$pkData$ParameterFactor), c(0.1, 1, 2, 20))

  set.seed(123)
  df1_pk <- summarizer(results$pkData, parameterPaths[1])
  expect_snapshot(df1_pk)

  set.seed(123)
  df2_pk <- summarizer(results$pkData, parameterPaths[2])
  expect_snapshot(df2_pk)

  set.seed(123)
  df3_pk <- summarizer(results$pkData, parameterPaths[3])
  expect_snapshot(df3_pk)
})

# Test customOutputFunctions ----------------------------------------------

test_that("sensitivityCalculation returns expected results with single custom function", {
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

  expect_equal(results$pkData, resultsLambda$pkData)

  customPKData <- dplyr::filter(
    results$pkData,
    PKParameter %in% names(customFunctions)
  )
  expect_snapshot(customPKData)
})

test_that("sensitivityCalculation returns expected results with multiple custom functions", {
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
  customPKData <- results$pkData %>%
    dplyr::filter(PKParameter %in% names(customFunctions))

  # Expect snapshot
  expect_snapshot(customPKData)
})

# Test saving to xlsx file ------------------------------------------------

test_that("sensitivityCalculation saves PK data to xlsx file", {
  path <- "mydata.xlsx"

  set.seed(123)
  results <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    variationRange = c(0.1, 2, 20),
    saOutputFilePath = path
  )

  expect_true(file.exists(path))

  on.exit(unlink(path))
})

test_that("sensitivityCalculation errors if file extension is incorrect", {
  path <- "mydata.csv"

  set.seed(123)
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
  "OutputPath", "ParameterPath", "ParameterFactor", "ParameterValue", "ParameterUnit",
  "C_max", "C_max_norm", "C_max_Unit", "C_max_norm_Unit", "C_max_PKPercentChange",
  "C_max_norm_PKPercentChange", "C_max_Sensitivity", "C_max_norm_Sensitivity",
  "t_max", "t_max_Unit", "t_max_PKPercentChange", "t_max_Sensitivity",
  "AUC_tEnd", "AUC_tEnd_norm", "AUC_tEnd_Unit", "AUC_tEnd_norm_Unit",
  "AUC_tEnd_PKPercentChange", "AUC_tEnd_norm_PKPercentChange", "AUC_tEnd_Sensitivity",
  "AUC_tEnd_norm_Sensitivity", "AUC_inf", "AUC_inf_norm", "AUC_inf_Unit",
  "AUC_inf_norm_Unit", "AUC_inf_PKPercentChange", "AUC_inf_norm_PKPercentChange",
  "AUC_inf_Sensitivity", "AUC_inf_norm_Sensitivity", "CL", "FractionAucLastToInf",
  "CL_Unit", "FractionAucLastToInf_Unit", "CL_PKPercentChange", "FractionAucLastToInf_PKPercentChange",
  "CL_Sensitivity", "FractionAucLastToInf_Sensitivity", "MRT",
  "MRT_Unit", "MRT_PKPercentChange", "MRT_Sensitivity", "Thalf",
  "Thalf_Unit", "Thalf_PKPercentChange", "Thalf_Sensitivity", "Vss",
  "Vss_Unit", "Vss_PKPercentChange", "Vss_Sensitivity", "Vd", "Vd_Unit",
  "Vd_PKPercentChange", "Vd_Sensitivity"
)

test_that("sensitivityCalculation converts output to wide format as expected", {
  set.seed(123)
  results2 <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    variationRange = c(0.1, 2, 20),
    pkParameters = NULL
  )
  pkDataWide <- esqlabsR:::.convertToWide(results2$pkData)

  expect_equal(dim(pkDataWide), c(12L, 57L))
  expect_equal(colnames(pkDataWide), pkDataWideColumns)
})

test_that("sensitivityCalculation converts output to wide format as expected with `customOutputFunctions`", {
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
    pkDataWideColumns, "minmax", "minmax_Unit",
    "minmax_PKPercentChange", "minmax_Sensitivity",
    "max_slope", "max_slope_Unit", "max_slope_PKPercentChange",
    "max_slope_Sensitivity"
  )

  set.seed(123)
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

  expect_equal(dim(pkDataWide), c(12L, 65L))
  expect_equal(colnames(pkDataWide), pkDataWideColumns)
})

# Test sensitivityCalculation when simulation fails -----------------------

test_that("sensitivityCalculation handles simulation failure", {
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

simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
simulation <- loadSimulation(simPath)
outputPaths <- c(
  "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  "Organism|Age",
  "Organism|ArterialBlood|Plasma|Aciclovir"
)
parameterPaths <- c(
  "Aciclovir|Lipophilicity",
  "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
  "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR fraction"
)
variationRange <- c(0.1, 5, 10)

resultsMultiple <- sensitivityCalculation(
  simulation = simulation,
  outputPaths = outputPaths,
  parameterPaths = parameterPaths,
  variationRange = variationRange
)

test_that("sensitivityCalculation extracts data for multiple output paths", {
  expect_identical(nrow(resultsMultiple$pkData), 108L)
  expect_equal(unique(resultsMultiple$pkData$OutputPath), outputPaths)
})

test_that("sensitivityCalculation applies absolute `variationRange` for multiple paths", {
  variationRangeAbs <- list(
    -0.097 * variationRange,
    0.00025 * variationRange,
    1 * variationRange
  )

  set.seed(123)
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
  path <- "mydata.xlsx"

  set.seed(123)
  resultsMultiple <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    variationRange = c(0.1, 5, 10),
    saOutputFilePath = path
  )

  expect_true(file.exists(path))

  on.exit(unlink(path))
})

test_that("sensitivityCalculation handles simulation failure for multiple output paths", {
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

# Restore old options
on.exit(options(old_opts), add = TRUE)
