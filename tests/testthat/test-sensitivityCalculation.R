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
    "charColumnSummary" = select(data, where(is.character)) %>%
      purrr::map_dfr(unique),
    "numericColumnSummary" = select(data, where(is.numeric)) %>%
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

test_that("sensitivityCalculation saves PK data to xlsx file", {
  path <- "mydata.xlsx"

  set.seed(123)
  results <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    variationRange = c(0.1, 2, 20),
    pkDataFilePath = path
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
      pkDataFilePath = path
    ),
    "Provided file has extension 'csv', while 'xlsx' was expected instead."
  )
})

# checking `SensitivityCalculation`  object ------------------

test_that("sensitivityCalculation returns the correct object", {
  expect_s3_class(results, "SensitivityCalculation")

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

# checking PK tidy data ------------------

test_that("sensitivityCalculation PK parameters tidy datafram column names and order as expected", {
  expect_equal(
    names(results$pkData),
    c(
      "OutputPath", "ParameterPath", "ParameterFactor", "ParameterValue",
      "PKParameter", "PKParameterValue", "Unit", "PercentChangePK",
      "SensitivityPKParameter"
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

# checking PK wide data ------------------

set.seed(123)
results2 <- sensitivityCalculation(
  simulation = simulation,
  outputPaths = outputPaths,
  parameterPaths = parameterPaths,
  variationRange = c(0.1, 2, 20),
  pkParameters = NULL
)

pkDataWide <- esqlabsR:::.convertToWide(results2$pkData)

test_that("sensitivityCalculation PK parameters datafram dimensions are as expected", {
  expect_equal(dim(pkDataWide), c(12L, 56L))
})

test_that("sensitivityCalculation PK parameters wide datafram column names and order as expected", {
  expect_equal(
    names(pkDataWide),
    c(
      "OutputPath", "ParameterPath", "ParameterFactor", "ParameterValue",
      "C_max", "C_max_norm", "C_max_Unit", "C_max_norm_Unit", "C_max_PercentChange",
      "C_max_norm_PercentChange", "C_max_Sensitivity", "C_max_norm_Sensitivity",
      "t_max", "t_max_Unit", "t_max_PercentChange", "t_max_Sensitivity",
      "AUC_tEnd", "AUC_tEnd_norm", "AUC_tEnd_Unit", "AUC_tEnd_norm_Unit",
      "AUC_tEnd_PercentChange", "AUC_tEnd_norm_PercentChange", "AUC_tEnd_Sensitivity",
      "AUC_tEnd_norm_Sensitivity", "AUC_inf", "AUC_inf_norm", "AUC_inf_Unit",
      "AUC_inf_norm_Unit", "AUC_inf_PercentChange", "AUC_inf_norm_PercentChange",
      "AUC_inf_Sensitivity", "AUC_inf_norm_Sensitivity", "CL", "FractionAucLastToInf",
      "CL_Unit", "FractionAucLastToInf_Unit", "CL_PercentChange", "FractionAucLastToInf_PercentChange",
      "CL_Sensitivity", "FractionAucLastToInf_Sensitivity", "MRT",
      "MRT_Unit", "MRT_PercentChange", "MRT_Sensitivity", "Thalf",
      "Thalf_Unit", "Thalf_PercentChange", "Thalf_Sensitivity", "Vss",
      "Vss_Unit", "Vss_PercentChange", "Vss_Sensitivity", "Vd", "Vd_Unit",
      "Vd_PercentChange", "Vd_Sensitivity"
    )
  )
})

test_that("sensitivityCalculation time series dataframe is as expected", {
  # also extract and add time series data for testing
  results$tsData <- esqlabsR:::.simulationResultsBatchToTimeSeriesDataFrame(
    simulationResultsBatch = results$simulationResults,
    parameterPaths = results$parameterPaths,
    outputPaths = results$outputPaths
  )

  set.seed(123)
  df1_ts <- summarizer(results$tsData, parameterPaths[1])
  expect_snapshot(df1_ts)

  set.seed(123)
  df2_ts <- summarizer(results$tsData, parameterPaths[2])
  expect_snapshot(df2_ts)

  set.seed(123)
  df3_ts <- summarizer(results$tsData, parameterPaths[3])
  expect_snapshot(df3_ts)
})

# validating plotting arguments -------------------------

test_that("sensitivityCalculation plots fail with incorrect input objects", {
  expect_error(
    sensitivityTimeProfiles("x"),
    "argument 'sensitivityCalculation' is of type 'character', but expected 'SensitivityCalculation'"
  )

  expect_error(
    sensitivitySpiderPlot("x"),
    "argument 'sensitivityCalculation' is of type 'character', but expected 'SensitivityCalculation'"
  )
})

# default plots ---------------------------------------

test_that("sensitivityTimeProfiles plots are as expected", {
  set.seed(123)
  p <- suppressWarnings(sensitivityTimeProfiles(results))

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "sensitivityTimeProfiles works as expected",
    fig = suppressWarnings(p)
  )

  pb <- suppressWarnings(ggplot_build(p$`Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)`))

  expect_snapshot(pb$plot$labels)
})

test_that("sensitivitySpiderPlot plots are as expected", {
  # make sure a plot is returned
  set.seed(123)
  p <- sensitivitySpiderPlot(results)

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "sensitivitySpiderPlot works as expected",
    fig = suppressWarnings(p)
  )
})

# saving plots: default ---------------------------------------

test_that("sensitivityTimeProfiles saves plot file", {
  path <- "Profile_OutputPath1.png"

  p <- suppressWarnings(sensitivityTimeProfiles(results, savePlots = TRUE))

  expect_true(file.exists(path))

  on.exit(unlink(path))
})

test_that("sensitivitySpiderPlot saves plot file", {
  path <- "Spider_OutputPath1.png"

  p <- sensitivitySpiderPlot(results, savePlots = TRUE)

  expect_true(file.exists(path))

  on.exit(unlink(path))
})

# saving plots: folder ---------------------------------------

test_that("sensitivityTimeProfiles saves plot file to a specified folder", {
  dir.create("newFolder")
  path <- "newFolder/Profile_OutputPath1.png"

  p <- suppressWarnings(sensitivityTimeProfiles(results, outputFolder = "newFolder/", savePlots = TRUE))

  expect_true(file.exists(path))

  on.exit(unlink("newFolder", recursive = TRUE))
})

test_that("sensitivitySpiderPlot saves plot file to a specified folder", {
  dir.create("newFolder")
  path <- "newFolder/Spider_OutputPath1.png"

  p <- sensitivitySpiderPlot(results, outputFolder = "newFolder/", savePlots = TRUE)

  expect_true(file.exists(path))

  on.exit(unlink("newFolder", recursive = TRUE))
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

results_multiple <- sensitivityCalculation(
  simulation = simulation,
  outputPaths = outputPaths,
  parameterPaths = parameterPaths,
  variationRange = c(1, 5, 10)
)

test_that("sensitivityCalculation extracts data correctly for multiple output paths", {
  expect_equal(nrow(results_multiple$pkData), 81L)
  expect_equal(unique(results_multiple$pkData$OutputPath), outputPaths)
})

test_that("sensitivityCalculation saves PK data to xlsx file for multiple output paths", {
  path <- "mydata.xlsx"

  set.seed(123)
  results_multiple <- sensitivityCalculation(
    simulation = simulation,
    outputPaths = outputPaths,
    parameterPaths = parameterPaths,
    variationRange = c(1, 5, 10),
    pkDataFilePath = path
  )

  expect_true(file.exists(path))

  on.exit(unlink(path))
})

test_that("sensitivityTimeProfiles plots are as expected for multiple output paths", {
  set.seed(123)
  p_list <- suppressWarnings(sensitivityTimeProfiles(results))

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple output path profiles",
    fig = suppressWarnings(p_list)
  )
})

test_that("sensitivityTimeProfiles saves plot files for multiple output paths", {
  path1 <- "Profile_OutputPath1.png"
  path2 <- "Profile_OutputPath2.png"
  path3 <- "Profile_OutputPath3.png"

  p <- suppressWarnings(sensitivityTimeProfiles(results_multiple, savePlots = TRUE))

  expect_true(file.exists(path1))
  expect_true(file.exists(path2))
  expect_true(file.exists(path3))

  on.exit(unlink(c(path1, path2, path3)))
})

test_that("sensitivitySpiderPlot plots are as expected for multiple output paths", {
  set.seed(123)
  plots_multiple <- sensitivitySpiderPlot(results_multiple)

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple output path spiders",
    fig = suppressWarnings(plots_multiple)
  )
})

# filter data to be plotted -------------------------------------

outputPathsFilter <- "Organism|ArterialBlood|Plasma|Aciclovir"
parameterPathsFilter <- "Aciclovir|Lipophilicity"
pkParametersFilter <- c("C_max", "t_max")

test_that("sensitivitySpiderPlot plots are as expected with filters", {
  set.seed(123)
  spider_plot_filtered <- sensitivitySpiderPlot(
    results_multiple,
    outputPaths = outputPathsFilter,
    parameterPaths = parameterPathsFilter,
    pkParameters = pkParametersFilter
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "filtered spider",
    fig = suppressWarnings(spider_plot_filtered)
  )
})

test_that("sensitivityTimeProfiles plots are as expected with filters", {
  set.seed(123)
  profile_plot_filtered <- sensitivityTimeProfiles(
    results_multiple,
    outputPaths = outputPathsFilter,
    parameterPaths = parameterPathsFilter
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "filtered profile",
    fig = suppressWarnings(profile_plot_filtered)
  )
})

# restore old options
options(old)
