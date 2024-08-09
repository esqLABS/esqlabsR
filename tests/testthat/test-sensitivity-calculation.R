# save old options
old <- options()

options(
  tibble.width = Inf,
  pillar.min_title_chars = Inf,
  pillar.sigfig = 4,
  digits = 4,
  scipen = 999
)

# single output path -------------------------------------

# run time-consuming simulations just once
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

library(dplyr, warn.conflicts = FALSE)

# custom function to extract summary data
summarizer <- function(data, path) {
  data <- dplyr::filter(data, ParameterPath %in% path)

  list(
    "charColumnSummary" = dplyr::select(data, where(is.character)) %>%
      purrr::map_dfr(unique),
    "numericColumnSummary" = dplyr::select(data, where(is.numeric)) %>%
      purrr::map_df(summary, .id = "column")
  )
}

# validate `outputPaths` ------------------------

test_that("sensitivityCalculation fails early with incorrect `outputPaths` arguments", {
  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = c(1, 2, 3),
      parameterPaths = parameterPath
    ),
    "Only values of `character` type are allowed in `outputPaths` argument."
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = "",
      parameterPaths = parameterPath
    ),
    "Values in `outputPaths` argument can't be an empty string."
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = c("", "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"),
      parameterPaths = parameterPath
    ),
    "Values in `outputPaths` argument can't be an empty string."
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = rep("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)", 2),
      parameterPaths = parameterPath
    ),
    "Only distinct values are allowed in `outputPaths` argument."
  )
})

# validate `parameterPaths` ------------------------

test_that("sensitivityCalculation fails early with incorrect `parameterPaths` arguments", {
  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      parameterPaths = c(1, 2, 3)
    ),
    "Only values of `character` type are allowed in `parameterPaths` argument."
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      parameterPaths = ""
    ),
    "Values in `parameterPaths` argument can't be an empty string."
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
    "Values in `parameterPaths` argument can't be an empty string."
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      parameterPaths = c(parameterPaths, parameterPaths[1])
    ),
    "Only distinct values are allowed in `parameterPaths` argument."
  )
})

# validate `pkParameters` ------------------------

test_that("sensitivityCalculation fails early with incorrect `pkParameters` arguments", {
  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      pkParameters = c(1, 2, 3),
      outputPaths = outputPaths,
      parameterPaths = parameterPaths
    ),
    "Only values of `character` type are allowed in `pkParameters` argument."
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      pkParameters = "",
      outputPaths = outputPaths,
      parameterPaths = parameterPaths
    ),
    "Values in `pkParameters` argument can't be an empty string."
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      pkParameters = c("", "C_max"),
      outputPaths = outputPaths,
      parameterPaths = parameterPaths
    ),
    "Values in `pkParameters` argument can't be an empty string."
  )

  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      pkParameters = c("C_max", "C_max"),
      outputPaths = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      parameterPaths = parameterPaths
    ),
    "Only distinct values are allowed in `pkParameters` argument."
  )

  expect_message(
    sensitivityCalculation(
      simulation = simulation,
      pkParameters = c("C_max", "abc", "xyz"),
      outputPaths = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      parameterPaths = parameterPaths
    ),
    "Following non-standard PK parameters will not be calculated:
abc
xyz
",
    fixed = TRUE
  )
})

# validate `variationRange` ------------------------

test_that("sensitivityCalculation fails early with incorrect `variationRange` arguments", {
  expect_error(
    sensitivityCalculation(
      simulation = simulation,
      outputPaths = outputPaths,
      parameterPaths = parameterPaths,
      variationRange = c("x", "y", "z")
    ),
    "argument 'variationRange' is of type 'character', but expected 'numeric, or integer'!"
  )
})

# validate `customOutputFunctions` ------------------

test_that("sensitivityCalculation fails early with incorrect `customOutputFunctions` arguments", {
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
})

test_that("sensitivityCalculation fails with invalid `customOutputFunctions`", {
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

# check `SensitivityCalculation`  object ------------------

test_that("sensitivityCalculation returns the correct object", {
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

# check PK tidy data -----------------------

test_that("sensitivityCalculation PK parameters tidy dataframe column names and order as expected", {
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

# test `customOutputFunctions` ------------------

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
  # list with multiple custom functions using `x`and `y` parameter
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

# test `saOutputFilePath` ------------------

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

# check PK wide data -----------------------

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

# check `SensitivityCalculation` when simulation fails ----------

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

# multiple output paths -------------------------------------

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

resultsMultiple <- sensitivityCalculation(
  simulation = simulation,
  outputPaths = outputPaths,
  parameterPaths = parameterPaths,
  variationRange = c(0.1, 5, 10)
)

test_that("sensitivityCalculation extracts data correctly for multiple output paths", {
  expect_identical(nrow(resultsMultiple$pkData), 108L)
  expect_equal(unique(resultsMultiple$pkData$OutputPath), outputPaths)
})

test_that("sensitivityCalculation applies custom PK parameter function correctly with multiple output paths", {
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

test_that("sensitivityCalculation saves PK data to xlsx file for multiple output paths", {
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

test_that("sensitivityCalculation extracts data correctly for multiple output paths upon simulation failure", {
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

# restore old options
options(old)
