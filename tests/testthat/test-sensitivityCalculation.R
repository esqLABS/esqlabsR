# save old options
old <- options()

options(
  tibble.width = Inf,
  pillar.min_title_chars = Inf,
  pillar.sigfig = 4,
  scipen = 999
)

# run time-consuming simulations just once
simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
simulation <- loadSimulation(simPath)
outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
parameterPaths <- c(
  "Aciclovir|Lipophilicity",
  "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose",
  "Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Glomerular Filtration-GFR|GFR fraction"
)

set.seed(123)
results <- sensitivityCalculation(
  simulation = simulation,
  outputPaths = outputPaths,
  parameterPaths = parameterPaths,
  variationRange = c(0.1, 2, 20)
)

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
  "Only file path with `.xlsx` extension is allowed."
  )
})


test_that("sensitivityCalculation dataframes are as expected", {
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

  # also extract and add time series data for testing
  results$tsData <- esqlabsR:::.simResultsToTimeSeriesDataFrame(
    results$simulationResults,
    results$outputPaths,
    results$parameters
  )

  # base scaling should be present
  expect_equal(unique(results$pkData$ParameterFactor), c(0.1, 1, 2, 20))

  # checking time series data ------------------

  set.seed(123)
  df1_ts <- summarizer(results$tsData, parameterPaths[1])
  expect_snapshot(df1_ts)

  set.seed(123)
  df2_ts <- summarizer(results$tsData, parameterPaths[2])
  expect_snapshot(df2_ts)

  set.seed(123)
  df3_ts <- summarizer(results$tsData, parameterPaths[3])
  expect_snapshot(df3_ts)

  # checking PK parameters data ------------------

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

test_that("sensitivityTimeProfiles plots are as expected", {
  set.seed(123)
  # make sure a plot is returned
  p <- suppressWarnings(sensitivityTimeProfiles(results))

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "sensitivityTimeProfiles works as expected",
    fig = p
  )

  pb <- suppressWarnings(ggplot_build(p$`Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)`))

  expect_snapshot(pb$plot$labels)
})

test_that("sensitivityTimeProfiles saves plot file", {
  path <- "Profile_OutputPath1.png"

  p <- suppressWarnings(sensitivityTimeProfiles(results, savePlots = TRUE))

  expect_true(file.exists(path))

  on.exit(unlink(path))
})

test_that("sensitivitySpiderPlot plots are as expected", {
  # make sure a plot is returned
  set.seed(123)
  p <- sensitivitySpiderPlot(results)

  # for some reason, even if the plot looks the same, the SVG is slightly
  # different each time this is run, so testing using snapshots instead
  #
  # set.seed(123)
  # vdiffr::expect_doppelganger(
  #   title = "sensitivitySpiderPlot works as expected",
  #   fig = p
  # )

  pb <- ggplot_build(p$`Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)`)

  # non-reproducible geom data
  # expect_snapshot(pb$data)

  expect_snapshot(pb$plot$labels)
})

test_that("sensitivitySpiderPlot saves plot file", {
  path <- "Spider_OutputPath1.png"

  p <- sensitivitySpiderPlot(results, savePlots = TRUE)

  expect_true(file.exists(path))

  on.exit(unlink(path))
})

# restore old options
options(old)
